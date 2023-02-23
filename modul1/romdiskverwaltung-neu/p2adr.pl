#!/usr/bin/perl
# vp 13.12.2009 erstellt
# 07.05.2007
# 24.05.2008 aadr ergänzt
# 14.06.2008 sadr ergänzt

die <<HELP unless @ARGV;
Aufruf: p2adr pfile
extrahiert Anfangsadresse aus p-File
HELP

open IN, "<$ARGV[0]";
binmode IN;

seek IN, 6, 0;
read IN, $block, 6; 

close IN;

($aadr,$dummy,$len) = unpack('sss',$block);

# printf "%X %X\n", $aadr,$len;

printf "%X\n", $aadr;
