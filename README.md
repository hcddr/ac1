Sources für den DDR-Computer AC1

http://hc-ddr.hucki.net/wiki/doku.php/homecomputer/ac1

**Anwender-Software für den AC1** findet man in den Unterverzeichnissen zur [ROM-Disk](modul1/romdiskverwaltung-neu)

Der Monitor des AC1 basiert auf dem Programm ZETBUG für TRS-80 Model I, beschrieben in funkschau 1980/11. Außerdem ist der Monitor des AC1 die Basis für den Systemmonitor des Z1013. Ich habe ZETBUG reassembliert, ebenso den AC1-Monitor und an die Labels des Z1013 angepasst. Damit kann man die Versionen gut vergleichen und Veränderungen verfolgen.

Das Minibasic basiert auf dem Minibasic V3.2 von Rolf-Dieter-Klein. Auch hier hab ich das AC1-Minibasic reassembliert und an das Original angepasst. Damit kann man beide Versionen gut vergleichen und Veränderungen verfolgen.

Das gleiche Spiel beim BASIC+EDITV1.1. Als Basis kam für mich nur das NASCOM-Basic V4.7 in Betracht, basierend auf dessen Quellcode entstand der Reassemblercode der AC1-Version. Später habe ich eine Bestätigung meiner Vermutung in der Doku zum Basic V3.2 gefunden: Grafik-Basic-Interpreter V.3.2 wurde unter Verwendung des Standardkern des Nascom-8K-Basic (C)1987 Microsoft entwickelt 1988 by E.Ludwig.

Mit dem Perl-Programm wav2ac1 können AC1-WAV-Dateien im Monitor-3.1-Format und Minibasic-Format in Binärdateien umgewandelt werden. Das Programm verarbeitet eine ganze Kassette auf einmal und produziert Binär- und .Z80-Dateien. Damit kann Musterkassette-001.WAV in die einzelne Programme zerlegt werden. 

2022: turbo2ac1.pl konvertiert Turbo-Tape-Programme (SCCH, E. Ludwig)
Es gibt Quellcodes der SCCH-Monitore V8, 10/88 und der angepassten Version für den AC1-2010 


23.02.2023

- basic/grafikbasic-v3-2.asm:	SCCH-Grafic-BASIC V3.2 + Patch f. Modul 1/Paket X
- minibasic/minibasic_4000.asm:	gelinkt auf Adr. 4000h, so dass auch mit einem 4K-Monitor die alten Programme direkt geladen werden können. RAM-Bereich bleibt 1800h ff !
- cpm/v15-funkamateur:	Das CP/M aus dem Funkamateur 04/89. Ein Mini-CPM mit Kassette und RAM-Disk. BDOS ist von CP/A, CCP von DR (modifiziert), BIOS nach Kramer
- monitor/ac1-2010.asm: Include-Datei
- monitor/ac1-turbo.asm: TURBO LOAD/COPY V.1 
- monitor/mon_v8-fdc-rfl.asm: AC1-MONITOR V8 E. Ludwig SCCH, Monitor Version Nov. 1987, Version AC1-2010 * FARBMONITOR * FDC/RFL (jkcemu-Version)
- monitor/mon-10122011.asm: mon_v8-fdc-rfl.asm, verändert auf Version "AC1-2010 * MONITOR * 12/2011"
- modul1/romdiskverwaltung-neu: meine Modul1-Software, incl. Softwarepaket u.a.m.

10.3.2023

- cpm/v22-funkamateur:	Das CP/M aus dem Funkamateur 10/89. Ein CPM mit Kassette und RAM-Disk für mp3/88-RAM-Floppy. BDOS ist von CP/A, CCP von DR (modifiziert), BIOS nach Kramer

07.03.2024

- cpm/hrcpm12: Das MLDOS/ZSDOS-basierte CP/M von R. Hänsel

20.3.2024

- rekonstruierte Quellen für HRDOS12/HRCPM12 incl. BDOS und BIOS
- rekonstruierte Quellen für FORMATAC u.a. Tools

