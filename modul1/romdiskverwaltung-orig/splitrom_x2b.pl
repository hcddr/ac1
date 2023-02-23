#!/usr/bin/perl
# vp 28.01.2023, letzte Änderung

$ARGV[0] ||= 'PaketX_AC1_2010.bin';    # testweise
$ARGV[1] ||= '27C4001_1_08.bin';    # testweise
$ARGV[2] ||= '27C4001_2_09.bin';    # testweise

die <<HELP unless @ARGV;
Aufruf: showrom.pl xrom rom1 rom2
zeigt Inhalt des erzeugten ROMs an/zerlegt ROMs
für Paket X V2 / ROM ab C000
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
		$filename ||= $name;
		$filename =~ s/[\x00 ]+$//;	# Leerzeichen am Ende löschen
		$filename =~ s/[\/:*?"<>|]/_/g;	# unerlaubte Zeichen"
		$filename =~ s/[^ -\x7F]//g;	# unerlaubte Zeichen
		
		print "###################### ",$filename,  " ######################", "\n";
		
		$name =~ s/\xff/\t/g;	# Tabulatoren
		#print $name, "\n";
		
		$typ = 'P';
					
		# UP	UMLAD
		# ix+0 XXXX - Programmanfang im EPROM
		# ix+2 XXXX - Programmende im EPROM
		# ix+4 XXXX - Programmanfang im RAM
		# ix+6 XXXX - Sprungadresse Programmstart
		# ix+8 XX   - Konfigurationsbyte zum Umladen
		# ix+9 XX   - Konfigurationsbyte zum Starten

		#while (getnbyte(3) ne "\xCD\x03\xE0") { }
		L1: 
		while ( ($b=getbyte) != 0xCD) { if ($b == 0x8D) { 
			print "no Bin at pos ",tell IN, "\n"; goto goto L0; } }
		$code = getnbyte(2); 
		if ($code ne "\x03\xE0"  #UP UMLAD
		    ) { 
		    	if ($code eq "\xE7\xFF") { print "UP WBASIC\n"; $typ = 'B' }
		    	print "extra Code at pos ",tell IN, "\n"; goto L1; }

		$romanf = getword;
		$romende = getword;
		$ramadr = getword;
		$startadr = getword;
		$page = getbyte;

		printf OUT "%-40s %.4X-%.4X (%.2X) => RAM: %.4X, start at %.4X \n", $name, $romanf, $romende, $page, $ramadr, $startadr;
		printf "%-40s %.4X-%.4X (%.2X) => RAM: %.4X, start at %.4X \n", $name, $romanf, $romende, $page, $ramadr, $startadr;
		
		#$rom = $page % 0x10;
		$rom = 8;
		
		if ($rom != 2 ) {
			$bank = $page-8;
	
			if ($rom == 1) {
			$romanf -= 0xE000;
			$romende -= 0xE000;
			} else {
			$romanf += $bank*0x4000 - 0xC000;
			$romende += $bank*0x4000 - 0xC000;
			}

			$romlength=$romende-$romanf+1;
	
			printf "ROM %.4X-%.4X (%.2X) %.2X\n", $romanf, $romende, $rom, $bank;

			if ($rom == 1) { $noerror = open ROMIN, "<$ARGV[0]" or die $! }
			elsif ($rom == 8) { $noerror = open ROMIN, "<$ARGV[1]" or die $! }
			else 	       { $noerror = open ROMIN, "<$ARGV[2]" } # or die $! }	
		
			unless ($noerror) {print "$noerror\n"}
		
			if ($noerror) {
			binmode ROMIN;
			seek ROMIN, $romanf, 0;
			read ROMIN, $program, $romlength;
			close ROMIN;

#########################################
# bin-Datei schreiben
#########################################

			
			$binfilename = sprintf "%s_%.4X_%.4X_%.4X.bin", $filename, $ramadr, $ramadr+$romlength-1, $startadr;
			print $binfilename,"\n";
			
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
		}			
	}
}	
}	
