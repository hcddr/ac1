#!/usr/bin/perl
###############################################################################
#
#	Konvertiert WAV-Dateien für den AC1 (Monitor 3.1) ins binäre Z80-Format 
#
#	06.08.2018	by Volker Pohlers created
#
###############################################################################

use strict;
use Fcntl ":seek";

# $ARGV[0] = 'd:\\hobby3\\ac1\\wav2z80\\mon_v31_16.wav';

die <<"help" unless @ARGV;
konvertiert WAV-Dateien mit AC1-Programm in Z80-Datei
Wave-Datei: 8 Bit, 22 kHz, Mono

- Es wird nicht kontrolliert, ob die WAV-Datei den Vorgaben entspricht
- Der Dateiname aus Headersave-Header wird für z80 genutzt

help

our $bufsize = 8*2**10;	# 8k
our $pos = $bufsize;
our $buf;
our $offset;
our $abort;

open IN, $ARGV[0] or die $!;
binmode IN;

my $riff;
sysread IN,$riff,12;
my $fmt;
sysread IN,$fmt,24; 
my ($f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8) = unpack "A4VvvVVvv",$fmt; 
print << "EOF";
Format: $f1, length $f2, always $f3, channels $f4,
       sample rate $f5, bytes per second $f6,
       bytes per sample $f7, bits per sample $f8
EOF

my $sample_rate_faktor = $f5 / 22050; 

#seek IN, 54, SEEK_CUR;	# Wave-Header überspringen
seek IN, 60, SEEK_CUR;	# Wave-Header überspringen

# print "Pos ", tell (IN), "\n"; 

#########################################
# WAV lesen
#########################################

# ein Byte lesen
sub ReadWav {
	my $b;
	if ($pos >= $bufsize) {
		$pos = $pos-$bufsize;
		$bufsize = read(IN, $buf, $bufsize);
		$abort = 1 if $bufsize == 0;
		print "ABORT!\n" if $abort;
	}
	$b = ord(substr($buf, $pos++, 1));
#print "Byte ", tell (IN)-$bufsize+$pos, ' ', $b, "\n"; 
	return $b;
}

#########################################
# AC1-Format Lese-Routinen 
#########################################

#Samples
#15 = 6500
#7 = 13987
#8 = 13987

# 1 Bit = 1 Schwingung 1500Hz = 666µs lang,
# 0 entspricht ¯|_ und 1 entspricht _|¯
 
# Vorton 512 x 00-Bytes
# Syncbyte E6
# 'U' + Name 6 Byte
# 256 x 00-Bytes
# Blocks '<' + Blocklänge + max. 256 Byte Daten + Prüfsumme
# Programmende 'x' + sadr




# Flankenwechsel suchen
sub ReadBit {
#print "ReadBit ", tell (IN)-$bufsize+$pos, ' ', $lastb, "\n"; 
	my $w;
	if (ReadWav() < $offset){ do { } until ((ReadWav() >= $offset) or $abort); $w = 1 }
	else			{ do { } until ((ReadWav() <= $offset) or $abort); $w = 0 }

	$pos += 10*$sample_rate_faktor;	# 2/3 * (7+8)
	return $w;
}

my $syncbyte;
sub ReadByte {
	my $i, my $b;
	$b=0;

	for( $i=0;$i<=7;$i++) {
	    $b = $b * 2 + ReadBit();
#print "Bytes ", tell (IN)+$pos, "\n"; 
#print $w, ' ';
#printf ("bits %d %.2x\n", $i, $b);
	}
	$b ^= $syncbyte;
	
	#printf "ReadByte %.2X\n", $b;
	return $b;
}

#########################################
# Main
#########################################


# Mittelwert bestimmen (Gleichspannungsanteil)
# JKCEmu produziert Werte 0x80 und 0x5C

{
	my $min = 0xff;
	my $max = 0;
	for (1..1000) {
		my $b = ReadWav();
		$min = $b if $min > $b;
		$max = $b if $max < $b;
	}
	$offset = int(($max+$min)/2);
	print "DC-Offset $offset\n";
	# Puffer rücksetzen
	$pos = 0;
}

neues_file:

our @tapbuf = ();

# Vorton
my $b=0;
while ( !$abort ) {
        $b = $b * 2 + ReadBit();
        $b &= 0xFF;
       # printf "%.2X", $b;

        # Synchronisationsmuster
        if ($b == 0xE6) { $syncbyte = 0; last }
        if ($b == 0x19) { $syncbyte = 0xFF; last } 
}
printf "Syncbyte %.2X\n", $b;

my $filename = '';

# Hauptschleife
my $aadr = 0xFFFF;
my $badr = 0;
my $eadr = 0;
my $sadr = 0;
my $mode = '';
my $typ = 'P';

while ( !$abort ) {
	$b = ReadByte();
	#printf "rBYTE %.2X\n", $b;
	
	if (($mode eq '') and ($b != 0x55)) {
		$mode = 'N'; #Name von Basic-Programm folgt
	}	
	
	# Filename
	if (($mode eq '') and ($b == 0x55)) {
		$mode = $b;
		for (1..16) { $filename .= chr (ReadByte()) }; # max 16 (6) Zeichen
		printf "Filename: %s\n", $filename;
	# Ende
	} elsif ($b == 0x78) {
		$mode = $b;
		# 2 Byte startadr lesen
		$sadr = ReadByte()+ReadByte()*256;
		printf "startadr: %.4X\n", $sadr;			
		last; 
	# block
	} elsif ($b == 0x3c) { 
		$mode = $b;
		my $checksum = 0;
		my $anzahl = ReadByte(); # $checksum += $anzahl;
		if ($anzahl == 0) { $anzahl = 256 };
		$badr = ReadByte(); 	$checksum += $badr;
		$b = ReadByte();	$checksum += $b;
		$badr = $badr + $b*256;
		$eadr = $badr + $anzahl-1;
		if ($aadr > $badr) {$aadr = $badr}
		printf "block: %.4X - %4X\n", $badr, $eadr;			
		for (1..$anzahl) {
			$b = ReadByte(); $checksum += $b;
			#printf "%.2X", $b;
			push (@tapbuf, $b);
		}	
		$b = ReadByte();
		if (($checksum & 0xff) != $b) {
			print "Prüfsummenfehler! ",$checksum & 0xff," $b\n";
		}
	# basic
	} elsif ($b == 0xD3) { 
		$mode = $b;
		$typ = 'B';
		my $anzahl = ReadByte(); # $checksum += $anzahl;
		$b = ReadByte();	
		$anzahl = $anzahl * 256 + $b;
		printf "basic: %.4X\n", $anzahl;			
		for (1..$anzahl) {
			$b = ReadByte(); 
			last if $abort;
			#printf "%.2X", $b;
			push (@tapbuf, $b);
		}	
		#filename
		for (1..6) { $filename .= chr(shift @tapbuf) }
		printf "Filename: %s\n", $filename;
		$aadr = 0x60F7;
		$eadr = $aadr + $anzahl;
		last;
	# sonstige
	} else { 
		if ($mode != 0x55) {
			printf " BYTE %.2X\n", $b;
			push (@tapbuf, $b);
		}
	}
}


#########################################
# Z80-Datei schreiben
#########################################

#printf "x Filename: %s\n", $filename;
#printf "x abort-flag: %s\n", $abort;

if ($#tapbuf > 0) {

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
             
my $header = pack("
		SSS		   # aadr, eadr, sadr: word;
		A6		   # frei fuer Zusatzinformationen
		a1                 # Typkennzeichen
		a3                 # Kopfkennzeichen 3x 0xD3
		A16                # dateiname
		"
	,$aadr, $eadr, $sadr, "AC1", $typ, "\xD3"x3, $filename);

$filename =~ s/[\x00 ]+$//;	# Leerzeichen am Ende löschen
$filename =~ s/[\/:*?"<>|]/_/g;	# unerlaubte Zeichen
$filename =~ s/[^ -\x7F]//g;	# unerlaubte Zeichen

printf "Dos Filename: %s\n", $filename;

open OUT, '>'. ($filename || 'bin') . '.z80';
#open OUT, '>test.bin';
binmode OUT;

#Daten schreiben
syswrite OUT, $header; 
syswrite OUT, pack ("C*", @tapbuf); 
close OUT;

#########################################
# bin-Datei schreiben
#########################################

# bin-datei
$filename=sprintf"%s_%.4X_%.4X_%.4X.bin", $filename, $aadr, $eadr, $sadr;
open OUT, '>'. $filename;
binmode OUT;
#Daten schreiben
syswrite OUT, pack ("C*", @tapbuf); 
close OUT;

}

# folge noch ein File?
if (not eof(IN)) {goto neues_file};

close IN;

__END__

