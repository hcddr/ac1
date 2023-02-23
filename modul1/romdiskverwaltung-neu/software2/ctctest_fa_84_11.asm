;fa_84_11_561

	cpu	z80

ctc	equ	0

	ORG	1900H	

	LD	A, 19H		;H-Teil d. Interrupt-Verbdgstab.
	LD	I, A	
	LD	A, 28H		;INT-Vektor
	OUT	(CTC), A	
	LD	A, 10110111B	; Kanalsteuerwort
	OUT	(CTC), A	
	LD	A, 0		; Zeitkonstante 256
	OUT	(CTC), A	
	IM	2		; Interrupt Mode 2 einst.
	EI			; Interrupt erlauben
	LD	A, 20H		;ASCII-Kode f. Leerz. laden
M1:	JR	M1		; dynamischer Halt
	;
	;	Interruptroutine des CTC
	;
INT:	RST	10H		; Zeichen im Akku ausgeben
	INC	A		; nächster Zeichenkode
	CP	60H		; alle Zeichenkodes durch?
	JR	NZ, M2		; --> nein
	LD	A, 20H		;wieder mit Leerz. beginnen
M2:	EI		
	RETI		
	;
	;	Interruptverbindungstabelle
	;
	ORG	1928H
	;
	DEFW	INT	;Interruptroutine für Kanal 0
	DEFW	INT	;Interruptroutine für Kanal 1
	DEFW	INT	;Interruptroutine für Kanal 2
	DEFW	INT	;Interniptroutine für Kanal 3

	END

In Tabelle 6 ist ein kleines Testprogramm in Assemblersprache für den CTC-
Baustein angeführt. Man braucht den Maschinenkode nur mit dem M-Befehl
einzugeben und dann das Programm mit J 1900 zu starten. Wenn alles richtig
gemacht wurde und der CTC-Baustein ordnungsgemäß funktioniert, wird der Bild-
schirm Zeile für Zeile immer wieder mit dem gesamten Zeichenvorrat des U 402-in
der Reihenfolge des ASCII-Kodes beschrieben.

Ein Kanal des CTC dient hier in der Betriebsart Zeitgeber zur Zeitverzögerung.
Sonst würde die Zeichenausgabe so schnell erfolgen, daß man sie nicht ohne
weiteres verfolgen könnte. Wer ganz sicher gehen will, kann auch noch die
restlichen drei Kanäle des CTC-Bausteins testen, indem er nacheinander die
jeweilige Kanaladresse bei der Ausgabe von Steuerwort und Zeitkonstante
einsetzt. Dieses einfache Maschinenprogramm läßt sich dann mit einem Reset
abbrechen.
