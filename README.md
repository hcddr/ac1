Sources für den DDR-Computer AC1

http://hc-ddr.hucki.net/wiki/doku.php/homecomputer/ac1

Der Monitor des AC1 basiert auf dem Programm ZETBUG für TRS-80 Model I, beschrieben in funkschau 1980/11. Außerdem ist der Monitor des AC1 die Basis für den Systemmonitor des Z1013. Ich habe ZETBUG reassembliert, ebenso den AC1-Monitor und an die Labels des Z1013 angepasst. Damit kann man die Versionen gut vergleichen und Veränderungen verfolgen.

Das Minibasic basiert auf dem Minibasic V3.2 von Rolf-Dieter-Klein. Auch hier hab ich das AC1-Minibasic reassembliert und an das Original angepasst. Damit kann man beide Versionen gut vergleichen und Veränderungen verfolgen.

Das gleiche Spiel beim BASIC+EDITV1.1. Als Basis kam für mich nur das NASCOM-Basic V4.7 in Betracht, basierend auf dessen Quellcode entstand der Reassemblercode der AC1-Version. Später habe ich eine Bestätigung meiner Vermutung in der Doku zum Basic V3.2 gefunden: Grafik-Basic-Interpreter V.3.2 wurde unter Verwendung des Standardkern des Nascom-8K-Basic (C)1987 Microsoft entwickelt 1988 by E.Ludwig.

Mit dem Perl-Programm wav2ac1 können AC1-WAV-Dateien im Monitor-3.1-Format und Minibasic-Format in Binärdateien umgewandelt werden. Das Programm verarbeitet eine ganze Kassette auf einmal und produziert Binär- und .Z80-Dateien. Damit kann Musterkassette-001.WAV in die einzelne Programme zerlegt werden. 

2022: turbo2ac1.pl konvertiert Turbo-Tape-Programme (SCCH, E. Ludwig)
Es gibt Quellcodes der SCCH-Monitore V8, 10/88 und der angepassten Version für den AC1-2010 
