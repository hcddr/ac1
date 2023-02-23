#!/usr/bin/perl
# vp 19.02.2023 18:26:13, letzte Änderung

$ARGV[0] ||= 'packedroms.bin';    # testweise

die <<HELP unless @ARGV;
Aufruf: showrom.pl rom
zeigt Inhalt des erzeugten ROMs an
HELP

open IN, "<$ARGV[0]";
binmode IN;
print "liste $ARGV[0]...\n";

open OUT, ">rominhalt.txt";

# die Progamme sind im ROM hintereinander weg abgelegt (ROM1 und 2; freie Lücken sind natürlich möglich)
# konkrete Banknummern und absolute Adressen im ROM bzw. releativ in der Bank interessieren nicht. Diese
# Verwaltungssoftware durchsucht den ROM-Bereich und entnimmt alle notwendigen Daten aus dem Header
# 	align 40h
# 	Header
#	binary
# Header (40h lang), auf 
#	db	0D3h,0D3h,0D3h	; +00h 0..2 Kopfkennzeichen 3x D3
#	db	typ		; +03h +3 Typkennzeichen
#	db	compressed	; +04h +4 Z komprimiert, 0 nicht komprimiert
#	ds 	5		; +5..9 frei ("AC1-X")
#	dw	aadr		; +0ah +10 Anfangsadresse 
#	dw	eadr		; +0ch +12 Endadresse
#	dw	sadr		; +0eh +14 Startadresse
#	db	name		; +10h +16 name. max 32 Zeichen , mit leerzeichen aufgefüllt
#	db 	comment		; +30h +46 kommentar, max 16 Zeichen , mit leerzeichen aufgefüllt

print OUT "EPROM    Bnk+Adr_AC1 Typ Name                         aadr-eadr,sadr Kommentar\n\n";

$lfdnr = 0;

while ( !eof(IN) ) {
  $pos = tell IN;
#  printf "%.4x\n", $pos;

  
  # nächste integrale xx40-Adresse
  read IN, $block, 0x40;

  # Bank und Position in Bank berechnen
  $banksize = 0x8000;
  $bank        = int( $pos / $banksize );
  $pos_in_bank = $pos - $bank * $banksize;
  
  $port = ($bank < 16) ? $bank*16+8 : ($bank-16)*16+9;

  # Analyse
  
  # FA-Kommando
  if ( $block =~ /^\xD3\xD3\xD3/ ) {

	$lfdnr++;
	
    ( $ft_typ, $compressed, $unused, $aadr, $eadr, $sadr, $name, $comment ) =
      unpack( "
				xxx	#	db	0D3h,0D3h,0D3h	; +00h 0..2 Kopfkennzeichen 3x D3
				C	#	db	typ		; +03h +3 Typkennzeichen
				C	#	db	compressed	; +04h +4 Z komprimiert, 0 nicht komprimiert
				A5	#	ds 	5		; +5..9 frei 
				S	#	dw	aadr		; +0ah +10 Anfangsadresse 
				S	#	dw	eadr		; +0ch +12 Endadresse
				S	#	dw	sadr		; +0eh +14 Startadresse
				A32	#	db	name		; +10h +16 name. max 32 Zeichen , mit leerzeichen aufgefüllt
				A16	#	db 	comment		; +30h +46 kommentar, max 16 Zeichen , mit leerzeichen aufgefüllt
			", $block );


	printf OUT "%3d %.8X %.2X %.4X %c %c %-32s %.4X-%.4X,%.4X %-16s\n", $lfdnr, $pos, $port, $pos_in_bank + 0x8000, 
		$ft_typ, $compressed, $name, $aadr, $eadr, $sadr, $comment;
  }

}

close IN;
close OUT;
