#!/usr/bin/perl
# vp 11.06.2003 erstellt
# 30.12.2012 

die <<HELP unless @ARGV;
Aufruf: z802bin z80-file
konvertiert Z1013-z80-Dateien ins bin Format

HELP

open IN, "<$ARGV[0]";
binmode IN;

#Kopfblock lesen

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


#$fileext=sprintf "_%04X_%04X_%04X.bin", $aadr, $eadr, $sadr;
$fileext=".bin";
$OUT = $ARGV[0];
$OUT =~ s/.z80/$fileext/i;

open OUT, ">$OUT";
binmode OUT;

print "Schreibe $OUT\n";

#Daten schreiben
do {
	$blocksize = read IN, $buffer, 1024;
	syswrite OUT, $buffer;	
} until ($blocksize==0);

close IN;
close OUT;
