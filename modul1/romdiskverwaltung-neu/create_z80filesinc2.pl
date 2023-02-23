#!/usr/local/bin/perl
# Scannen von Z1013-Z80-Dateien
# Rekursives Traversieren mit File::Find
# V.Pohlers, 2011

use File::Find;

$ARGV[0] = "." unless defined @ARGV;


open OUT, '>z80files.inc';
find(\&wanted, @ARGV);		  # Rekursiver Alg.
close OUT;

# Diese Funktion wird fuer jeden Verzeichniseintrag aufgerufen.
# Der Name des aktuellen Verzeichnisses
# befindet sich in $File::Find::dir.
# Der Name der aktuellen Datei
# in diesem Verzeichnis liegt in $_.
# Wir sind hier implizit chdir($File::Find::dir) geworden,
# so dass wir auf $_ direkt zugreifen koennen.

sub wanted {
	# my $file;
	$file = $_;
	# $File::Find::name ist "$File::Find::dir/$_"
	return unless -f;	# raus, wenn wir keine echte Datei haben

	if (/\.z80$/i) { # nur Endung .Z80
		
		print STDERR "Examining: ", $File::Find::name, "\n" if $^W;

		scan_tap($file);
	}
}


sub hex2 {
	return sprintf("%02X",shift);
}	
sub hex4 {
	return sprintf("%04X",shift);
}	


sub scan_tap {
	my $name,$typ, $aadr, $eadr, $sadr;
	my $type;


	my $file = shift;
	print $file, "\n";
	
	open IN, "<$file";
	binmode IN;

	# Header lesen
	read IN, $block, 0x20; 
	#close IN;
	
#KOPFAUFBAU: Byte 0-1 Anfangsadresse 
#               2-3   Endadresse
#               4-5   Startadresse
#               6-0BH frei fuer Zusatzinformationen
#                 0CH Typkennzeichen
#             0DH-0FH 3 x 0D3H = Kopfkenzeichen
#                     (An diesem D3 erkennt jedes Kopfsuchpro-
#                      gramm einen Kopf !!)
#             10H-1FH 16 Byte Namensblock.
             
	# Z1013-Headersave-Header
	($aadr, $eadr, $sadr, $extra ,$typ, $kennung, $name) = unpack("
		SSS		   # aadr, eadr, sadr: word;
		a6		   # frei fuer Zusatzinformationen
		a1                 # Typkennzeichen
		a3                 # Kopfkennzeichen 3x 0xD3
		a16                # dateiname
		", $block 
	);

	if ($kennung ne "\xd3\xd3\xd3") { # Kopfkennzeichen stimmt nicht
		close IN;
		return;
	}

	close IN;

	$name =~ s/[\x00 ]*$//g;
	$name =~ s#\\#\\\\/#g;		# escape-symbol, wg. AS-Assembler
	
	$file = $File::Find::name;
	#$fileext=sprintf "_%s_%s_%s.bin.zx7", hex4($aadr),hex4($eadr),hex4($sadr);
	#$file =~ s/.z80/$fileext/i;

#unkomprimiert	
#	$file =~ s/.z80/.bin/i;
	
#komprimiert	
	$file =~ s/.z80/.bin.zx7/i;

	printf OUT "	addfile	\"%s\",0%sh,0%sh,0%sh,'%s',\"%s\",\"\"\n",
		$file,hex4($aadr),hex4($eadr),hex4($sadr),$typ,$name;
}
