#!/usr/bin/perl
# vp 11.06.2003 erstellt
# 30.12.2012 

die <<HELP unless @ARGV;
Aufruf: com2z80 bin-file [aadr] [sadr]
konvertiert bin-Dateien ins Z1013-z80-Format

HELP

$aadr = (oct '0x'.$ARGV[1]) || 0x100;
$sadr = (oct '0x'.$ARGV[2]) || $aadr;

($OUT = $ARGV[0]) =~ s/\..*/.z80/i;

open IN, "<$ARGV[0]";
binmode IN;

open OUT, ">$OUT";
binmode OUT;

print "Schreibe $OUT ";


#Kopfblock schreiben
$name=uc $ARGV[0];
$name =~ s/\.(.*)//;
$size= -s $ARGV[0];

$typ = 'C';

print "NAME=$name SIZE=$size\n";

#Headersave-Header
syswrite OUT, pack("
	sss		   # aadr, eadr, sadr: word;
	A6                 # frei
	a		   # Dateityp
	a3		   # Headersave-Kennung 3xD3h
	A16		   # Dateiname, mit Leerzeichen aufgefüllt
	", 
	$aadr, $aadr+$size-1, $sadr, 'vp--as',
	$typ, "\xD3"x3, $name
);

#Daten schreiben
do {
	$blocksize = sysread IN, $buffer, 1024;
	syswrite OUT, $buffer;	
} until ($blocksize==0);

#auf vollen Block auffüllen

print OUT "\x00"x(32-$size%32);  # letzten Block für MC mit 00 auffüllen

close IN;
close OUT;
