#!/usr/bin/perl
###############################################################################
#
#	Konvertiert WAV-Dateien für den AC1 ins binäre Z80-Format
#
#	06.08.2018	by Volker Pohlers created
#	25.01.2023	Ergänzung für andere WAV-Formate (z.B. 16 Bit, 44KHz)
#
###############################################################################

use strict;
use Fcntl ":seek";

die <<"help" unless @ARGV;
konvertiert WAV-Dateien mit Turbo-AC1-Programm in A1T/Z80/BIN-Datei
Wave-Datei: 8/16 Bit, 22/44 kHz, Mono

- Es wird nicht kontrolliert, ob die WAV-Datei den Vorgaben entspricht
- Der Dateiname aus Headersave-Header wird für z80 genutzt
- basiert auf H.Haftmanns KCLOAD

help

our $bufsize = 8*2**10;	# 8k
our $pos = $bufsize;
our $buf;
our $offset;
our $abort;

open IN, $ARGV[0];
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

seek IN, 54, SEEK_CUR;	# Wave-Header überspringen


#########################################
# WAV lesen
#########################################

# ein Byte lesen
sub ReadB {
	my $b;

	if ($pos == $bufsize) {
		$bufsize = read(IN, $buf, $bufsize);
		$abort = 1 if $bufsize == 0;
		$pos = 0;
	}
	$b = ord(substr($buf, $pos++, 1));
	return $b;
}


# ein Flankenwechsel abwarten (1/2 Periode)
sub ReadSwing1 {
	my $w;

	$w = 1;
	if (ReadB() < $offset)	{ do { $w++ } until (ReadB() > $offset) or $abort }
	else			{ do { $w++ } until (ReadB() < $offset) or $abort }

	return $w;
}

# eine volle Periode
sub ReadSwing2 {
	my $ww;
	$ww = ReadSwing1() + ReadSwing1();
	return $ww;
}

#########################################
# Formatabhängige Lese-Routinen
#########################################

#Z1013-Format:
# Vorton: Halbschwingung  650 Hz (17 Samples) Diskriminator:
# Trennz: Vollschwingung 1400 Hz (16 Samples)
# 0-Bit:  Halbschwingung 1200 Hz ( 9 Samples)
# 1-Bit:  Vollschwingung 2400 Hz ( 9 Samples) Diskriminator: 6
#
# Wort: Bit0, Bit1, ... Bit15
#
# Block: Vorton (14..2000) - Trennz - Wort (Zieladr.) - 16 Wort -
#	Wort (Summe über die vorhergehenden 17(!) Worte)
#
# Datei: HEADERSAVE-Vorblock (bel. Zieladresse), n Datenblöcke
#
# HEADERSAVE-Vorblock (leider optional, aber sehr verbreitet):
#	WORD aadr,eadr,sadr; CHAR creator[6]; BYTE type; BYTE magic[3];
#	CHAR filename[16]


my %z1013standard = (vorton => 17, trennz => 16, bit => 9, diskriminator => 6);

my %ac1standard = (vorton => 11, trennz => 6, bit => 6, diskriminator => 4);

my %ac1standard48 = (vorton => 13, trennz => 12, bit => 11, diskriminator => 7);


sub WordIn {
	my $i, my $w, my $b;
	$b=0;

	for( $i=0;$i<=15;$i++) {
	    $b = $b >> 1;
	    $w = ReadSwing1();
	    if ($w> 4 * $sample_rate_faktor) { $b |= 0x8000 } else { ReadSwing1() }
	}
	return $b;
}

sub ReadBlock {
	(my $BlkNr, my $Buffer, my $VLen) = @_;
    	my $I,my $W,my $Sum;

  	# Schritt 1: Vorton erkennen und aufsynchronisieren
  	STEP1:
  	return (1==0) if $abort;

  	$I = $VLen;
  	do {
	     	$W = ReadSwing1();
     		if (($W<7 * $sample_rate_faktor) or ($W>16 * $sample_rate_faktor)) {
      			goto STEP1;
     		}
     		$I--;
    	} until $I==0;

	# schritt 2: 1. Trennzeichen holen
  	do {
	     	$W = ReadSwing1();
     		if (($W<4 * $sample_rate_faktor) or ($W>16 * $sample_rate_faktor)) {
      			goto STEP1;
     		}
	} until ($W<10 * $sample_rate_faktor); # ein halbes Trennzeichen

	# 2. halbes Trennzeichen muß folgen
	 $W =ReadSwing1();
	if (($W<4 * $sample_rate_faktor) or ($W>=26 * $sample_rate_faktor)) {
		goto STEP1;
	}

#print "Bytes ", tell (IN)+$pos, "\n";

	# Schritt 3: Bytes lesen
    	$$BlkNr = WordIn();
    	$Sum = $$BlkNr;
    	for ($I=0;$I<=15;$I++) {
		$W = WordIn();
     		$Sum += $W;
     		$$Buffer[$I] = $W;
    	}

    	$Sum &=0xFFFF;	# auf Word beschränken
    	$W = WordIn();
    	return ( $Sum==$W );
}

our ($name, $typ,$eadr, $aadr, $sadr);

# Header anzeigen (Block 0)
sub kcc_header{
	my $Buffer = pack("v*",@_);
	#my $name, my $typ,my $aadr,my $eadr,my $sadr,
	my $hskennz;
	my $free;

#Headersave-Header
#"NNNNNNNNZ1013 INFO      : PoL @L"


	($free,$name,$free,$typ,$eadr, $aadr) = unpack("
	A8                 # Header-Kennung 8xN
	A16		   # Dateiname, mit Leerzeichen aufgefüllt
	A2		   # ': '
	a		   # Dateityp
	ss		   # eadr, aadr
	a		   # spezial ('L')
	", $Buffer);

	$name =~ s/\x00//g;

	printf ("name %s, typ %s, aadr %.4lX, eadr %.4lX, sadr %.4lX\n",
	$name,$typ,$aadr,$eadr,$sadr);

	if ($hskennz == "\xD3\xD3\xD3") {
		print "Headersave\n";
		return $typ.'.'.$name;
	} elsif ($aadr == 0x1000) {
		print "TinyBasic\n";
		return $typ.'.'.$name;
	} else {
		return '';
	}
}

#########################################
# Main
#########################################


our $BlkNr;
our $I, our $W;
our @buf;
our @tapbuf;
#our $name;

# Mittelwert bestimmen (Gleichspannungsanteil)
# JKCEmu produziert Werte 0x80 und 0x5C

{
	my $min = 0xff;
	my $max = 0;
	for $I (1..1000) {
		my $b = ReadB();
		$min = $b if $min > $b;
		$max = $b if $max < $b;
	}
	$offset = ($max+$min)/2;
	print "DC-Offset $offset\n";
	# Puffer rücksetzen
	$pos = 0;
}

# 1. Block
print "1. block\n";
if (ReadBlock(\$BlkNr,\@buf,50)) {	# langer Vorton ist hier WICHTIG!
		printf "%.4X ",$BlkNr;
		push (@tapbuf, @buf);
		#$name =
		kcc_header(@buf);
	} else {
		print "\nERROR->",$BlkNr;
	}

# Blöcke
print "Bloecke\n";
while ( !eof(IN) ) {
	if (ReadBlock(\$BlkNr,\@buf, 8)) {
		printf "%.4X ",$BlkNr;
		push (@tapbuf, @buf);
	} else {
		print "\nERROR->",$BlkNr;
	}
}

close IN;

#########################################
# AC1-Turbo-Datei schreiben (original)
#########################################

our $filename = $typ.'.'.$name;
$filename =~ s/[\x00 ]+$//;	# Leerzeichen am Ende löschen

open OUT, '>'. ($filename || 'test') . '.a1t';
binmode OUT;
#Header schreiben
syswrite OUT, pack ("v*", @tapbuf);
close OUT;

#########################################
# Z80-Datei schreiben (f.  JKCEMU)
#########################################


splice @tapbuf, 0, 16; # orig Header entfernen


open OUT, '>'. ($filename || 'test') . '.z80';
#open OUT, '>test.bin';
binmode OUT;

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
	,$aadr, $eadr, $sadr, "AC1", $typ, "\xD3"x3, $name);

#Daten schreiben
syswrite OUT, $header;
syswrite OUT, pack ("v*", @tapbuf);
close OUT;

#########################################
# bin-Datei schreiben
#########################################

# bin-datei
$filename=sprintf"%s.%s_%.4X_%.4X_%.4X.bin", $typ, $name, $aadr, $eadr, $sadr;
open OUT, '>'. $filename;
binmode OUT;
#Daten schreiben
syswrite OUT, pack ("v*", @tapbuf);
close OUT;




__END__

