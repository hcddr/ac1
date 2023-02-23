#!/usr/bin/perl
# vp 28.01.2023, letzte Änderung

# NOCH UVST !!!!

# Version f. Programmpaket X Version 1.0, 8/89 H.Fey

$ARGV[0] ||= 'prog_x_e000_001.bin';    # testweise
$ARGV[1] ||= 'x_rom_001.bin';    # testweise
$ARGV[2] ||= '';    # testweise

die <<HELP unless @ARGV;
Aufruf: showrom.pl xrom rom1 rom2
zeigt Inhalt des erzeugten ROMs an/zerlegt ROMs
f. Programmpaket X Version 1.0, 8/89 H.Fey
HELP

open IN, "<$ARGV[0]" or die $!;
binmode IN;
print "liste $ARGV[0]...\n";

open OUT, ">$ARGV[0]_inhalt.txt";
$banksize = 0x2000;

seek IN, 0, 0;              # auf Fileanfang

sub getbyte {
	my $b;
	read IN, $b, 1 or die $!;
	#print ord($b);
	ord($b);
}

sub getnbyte {
	my $b;
	my $anz=shift;
	read IN, $b, $anz or die $!;
	$b;
}

sub getword { #lo, hi
	getbyte() + getbyte() * 256;
}


while ( !eof(IN) ) {
	
	# suche header
	while (getbyte != 0x8d) {};
	L0:
	if (getbyte == 0) { 
		# header found

		$pos = tell IN;

		$name = '';
		do {
			read IN, $b, 1  or die $!;
			$name .= $b if ord($b) != 0x0;
		}	while (ord($b) != 0x0);
		
		($filename) = $name =~ /^(.*?)\xff/;
		$filename =~ s/[\x00 ]+$//;	# Leerzeichen am Ende löschen
		$filename =~ s/[\/:*?"<>|]/_/g;	# unerlaubte Zeichen"
		$filename =~ s/[^ -\x7F]//g;	# unerlaubte Zeichen
		
		print "###################### ",$filename,  " ######################", "\n";
		
		$name =~ s/\xff/\t/g;	# Tabulatoren
		#print $name, "\n";
					
		# UP	UMLAD
		# XX   - Konfigurationsbyte Monitor
		# XX   - Konfigurationsbyte zum Starten
		# XXXX - Programmanfang im EPROM
		# XXXX - Programmende im EPROM
		# XXXX - Programmanfang im RAM
		# XX   - Konfigurationsbyte zum Umladen
		# XXXX - Sprungadresse Programmstart

		$typ = 'P';

		L1: 
		while ( ($b=getbyte) != 0xCD) { if ($b == 0x8D) { 
			print "no Bin at pos ",tell IN, "\n"; goto goto L0; } }
		$code = getnbyte(2); 
		if ($code ne "\x3F\xE2"  #UP UMLAD H.Fey
			and $code ne "\x27\xE2"	#UP REPAK H.Fey
		    ) { 
		    	if ($code eq "\xE7\xFF") { print "UP WBASIC\n"; $typ = 'B' }
		    	print "extra Code at pos ",tell IN, "\n"; goto L1; }
	
	
		
		if ($code eq "\x3F\xE2") { print "UP UMLAD\n" }
		if ($code eq "\x27\xE2") { print "UP REPAK\n" }
	
		getnbyte(2); # überlesen
		$romanf = getword;
		$romende = getword;
		$ramadr = getword;
		$page = getbyte;
		$startadr = getword;

		printf OUT "%-40s %.4X-%.4X (%.2X) => RAM: %.4X, start at %.4X \n", $name, $romanf, $romende, $page, $ramadr, $startadr;
		printf "%-40s %.4X-%.4X (%.2X) => RAM: %.4X, start at %.4X \n", $name, $romanf, $romende, $page, $ramadr, $startadr;
		
		#$rom = $page % 0x10;
		$rom = 8;
		
		if ($rom >= 8) {
			#$bank = int($page/0x10);
			if ($page == 8) {$bank = 0 }
			if ($page == 9) {$bank = 1 }
			if ($page == 0x18) {$bank = 2 }
			if ($page == 0x19) {$bank = 3 }
			if ($page == 0x28) {$bank = 4 }
			if ($page == 0x29) {$bank = 5 }
			
			$romanf += $bank*0x8000 - 0x8000;
			$romende += $bank*0x8000 - 0x8000;
			$romlength=$romende-$romanf+1;
	
			printf "Position im ROM: %.6X-%.6X (%.2X) %.2X\n", $romanf, $romende, $rom, $bank;

			if ($rom == 8) { $noerror = open ROMIN, "<$ARGV[1]" or die $! }
			else 	       { $noerror = open ROMIN, "<$ARGV[2]" } # or die $! }	
		
			if ($noerror) {
				binmode ROMIN;
				seek ROMIN, $romanf, 0;
				read ROMIN, $program, $romlength;
				close ROMIN;
				
#########################################
# bin-Datei schreiben
#########################################
				
				$binfilename = sprintf "%s_%.4X_%.4X_%.4X.bin", $filename, $ramadr, $ramadr+$romlength-1, $startadr;
				print "Speichern als: ", $binfilename,"\n";

				open ROMOUT, ">$binfilename";
				binmode ROMOUT;
				syswrite ROMOUT, $program;
				close ROMOUT;

#########################################
# Z80-Datei schreiben
#########################################

			# Z1013-Headersave-Header
			#KOPFAUFBAU: Byte 0-1 Anfangsadresse 
			#               2-3   Endadresse
			#               4-5   Startadresse
			#               6-0BH frei fuer Zusatzinformationen
			#                 0CH Typkennzeichen
			#             0DH-0FH 3 x 0D3H = Kopfkenzeichen
			#                     (An diesem D3 erkennt jedes Kopfsuchpro-
			#                      gramm einen Kopf !!)
			#             10H-1FH 16 Byte Namensblock.
			
			#Header schreiben
			             
			$typ ||= 'P';
			
			my $header = pack("
					SSS		   # aadr, eadr, sadr: word;
					A6		   # frei fuer Zusatzinformationen
					a1                 # Typkennzeichen
					a3                 # Kopfkennzeichen 3x 0xD3
					A16                # dateiname
					"
				,$ramadr, $ramadr+$romlength-1, $startadr, "AC1", $typ, "\xD3"x3, $filename);
			
			#printf "Dos Filename: %s\n", $filename;
			
			open ROMOUT, '>'. ($typ . '.' . ($filename || 'bin')) . '.z80';
			binmode ROMOUT;
			
			#Daten schreiben
			syswrite ROMOUT, $header; 
			syswrite ROMOUT, $program;
			close ROMOUT;

			} else {
				print OUT ">>> ROM fehlt, kein Speichern moeglich!\n";
				print "ROM fehlt, kein Speichern moeglich!\n";
			}
		}
	}
}	
	
