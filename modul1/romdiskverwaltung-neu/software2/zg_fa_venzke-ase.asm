; File Name   :	d:\hobby3\ac1-2010\modul_1_vp\software2\zg_fa_venzke.z80
; Base Address:	0000h Range: 18E0h - 1A00h Loaded length: 0120h

		cpu	z80undoc

		include	ac1-2010.asm

		org 1900h

start:		rst	prnst		; Ausgabe String
		db 8Ch

		; Titelzeile
		ld	hl, bws(2,7)
		ld	(cupos), hl
		rst	prnst		; Ausgabe String
		db "* *    TESTPROGRAMM ZEICHENGENERATOR  AC1    * ",0AAh

		; Zeile 00 ... 0F
		ld	b, 10		; 00..09
		ld	hl, bws(4,7)	; 16F8h	; HL=Pos. im BWS
		ld	de, 3030h	; "00"
		jr	l2
l1:		ld	a, e
		cp	'F'+1
		jr	z, l4		; wenn fertig
		or	a
		ld	b, 6		; 0A..0F
		ld	e, 'A'
l2:		ld	(hl), d
		dec	hl
		ld	(hl), e
		inc	e
		dec	hl
		dec	hl
		djnz	l2
l3:		jr	l1

		; Trennzeile 64x '-'
l4:		ld	b, 64
		ld	hl, bws(5,0)
l5:		ld	a, '_'
		ld	(hl), a
		dec	hl
		djnz	l5
		
		; senkrechte Linie 26x '|'
		ld	b, 26
		ld	hl, bws(4,5)
		ld	a, '|'
l6:		ld	(hl), a
		ld	de, 64		; Länge einer Zeile
		sbc	hl, de
		djnz	l6

		; Spalten 00..F0
		ld	b, 5		; 5 Gruppen 00/10 .. 80/90
		ld	hl, bws(7,3)
l7:		ld	de, 3030h	; "00"
		jr	l9
l8:		ld	a, d
		cp	'F'+1
		jr	z, l12		; wenn fertig
		or	a
		ld	b, 3		; 3 Gruppen A0/B0 .. E0/F0
		ld	d, 'A'
l9:		ld	c, 0
l10:		ld	(hl), d
		dec	hl
		ld	(hl), e
		inc	d
		inc	hl
l11:		push	de
		ld	de, 64
		sbc	hl, de
		pop	de
		inc	c
		ld	a, c
		cp	1
		jr	z, l10
		cp	2
		jr	z, l11
		djnz	l9
		;
		ld	(cupos), hl
		jr	l8

		; Anzeige der Zeichen
l12:		ld	hl, bws(7,8)	; Startposition
		ld	b, 0		; Nr. der Gruppe
		ld	e, 0		; Zeichen 0..FF
l13:		ld	d, 0		; Nr in Gruppe
l14:		ld	c, 0		; Nr in Zeile
l15:		call	MS30		; 30 ms warten
		ld	(hl), e		; Zeichen eintragen
		dec	hl
		dec	hl
		dec	hl		; 3 Zeichen weiter
		inc	e		; nächstes Zeichen
		inc	c		; Zähler Nr in Zeile erhöhen
		inc	d		; Zähler Nr in Gruppe erhöhen
		ld	a, c
		cp	10h		; 16 Zeichen dargestellt?
		jr	nz, l15		; nein
		push	bc		; ja, nächste Zeile
		ld	b, 10h		; offs. zu nächster Zeile
l16:		dec	hl		; cursorpos. anpassen
		djnz	l16
		pop	bc
		ld	a, d		; Zähler Nr in Gruppe
		cp	32		; 32 Zeichen dargestellt?
		jr	nz, l14		; nein
		push	bc		; ja, Leerzeile ausgeben
		ld	b, 64		; dazu 64x 
l17:		dec	hl		;  cursorpos. anpassen
		djnz	l17
		pop	bc
		inc	b		; nächste Gruppe
		ld	a, b
		cp	8		; alle 8 Gruppen fertig?
		jr	nz, l13		; nein
		
		; Finale
		rst	prnst; Ausgabe String
		db "   (C) H.VENZKE 1988",8Dh
		jp	GETCO1; Sprung zur Monitoreingabeschleife

		end
