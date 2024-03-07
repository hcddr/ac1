; File Name   :	d:\hobby3\ac1-2010\monitor\komfort-monitor11\MO11A_4C.Z80
; Format      :	Binary file
; Base Address:	0000h Range: 18E0h - 1A00h Loaded length: 0120h

		cpu	z80undoc

; reass. V.Pohlers 01.02.2023

; Nachladbare Kommandodatei
; =========================

; L:	MO11A_4C.z80		V.24-LOAD (HEADERSAVE-Format)
; --------------------------------------------------------------------------
; Syntax:	L aaaa "name"   LOAD "name" nach aaaa
; 	L 		LOAD beliebiges File nach 
; 
; LADEN eines Files nach Adresse aaaa via V24. 
; Der Name kann bis zu 16 Zeichen lang sein und MUSS mit " beginnen,
; kann aber auch weggelassen werden. Dann wird jedes File geladen,
; sofern es ein HEADERSAVE-File ist.
; 
; Dabei wird die Einstellung vom V24-Kommandobyte auf fest 4.800Baud
; mit Protokoll: 8N1 ohne Handshake eingestellt.
; Auf diese Parameter ist der "Empfänger-PC" ebenfalls einzustellen.
; 
; Am Ende wird die CRC-Prüfsumme berechnet und ausgeschrieben. Damit kann
; dann auf fehlerfreies Laden kontrolliert werden.
; 
; Die Routine ist verschieblich und wird nach 1900H geladen, kann aber auch
; mit dem Aufruf # U xxxx "MO11A_4C.z80" in den Speicher ab xxxx geladen werden.


		include	ac1-2010.asm

;interne Monitor-Funktionen
crc		equ	0405h		; crc anzeigen, Kommando 'N'
q_ko20		equ	0A37h		; Start/Stop-Schaltung start
q_ko21		equ	0A31h		; Start/Stop-Schaltung stopp

;Monitor-Speicherzellen
kdov24		equ	1820h		; v24-Modus


		org 01900h - 20h

; z80-Header		
		dw anf			; "\tL\r"
		dw ende
		dw start
		db "MO11.0"
		db  50h	; P
		db 0D3h,0D3h,0D3h
		db "ext.Komm L      "

; --------------------------------------------------------------------------
; Syntax:	L aaaa "name"   LOAD "name" nach aaaa
; 	L 		LOAD beliebiges File nach 
; --------------------------------------------------------------------------

anf:		db 0,9,'L',0Dh

start:		xor	a
		ld	(18B0h), a
		ld	a, (kdov24)
		push	af
		ld	a, 43h
		ld	(kdov24), a
		call	q_ko20		; Start/Stop-Schaltung start
		call	80Fh
		call	95Bh
		ld	hl, 187Eh
		ld	b, 6
loc_191F:	call	0E4Ch
		ld	(hl), a
		inc	hl
		djnz	loc_191F
		ld	de, (187Eh)
		ld	hl, (1880h)
		xor	a
		sbc	hl, de
		inc	hl
		ld	(1866h), hl
		jp	c, 5C4h
		ld	hl, (ARG1)	; Kdo-Argument 1
		ld	a, l
		or	h
		jr	z, loc_193F
		ex	de, hl
loc_193F:	ld	(ARG1), de	; Kdo-Argument 1
		ld	hl, 1884h
		ld	b, 10
loc_1948:	call	0E4Ch
		ld	(hl), a
		inc	hl
		djnz	loc_1948
		ld	b, 3
		ld	a, 0D3h		; Kopfkennung
loc_1953:	dec	hl
		cp	(hl)
		jp	nz, 5BFh
		djnz	loc_1953
		ld	hl, 188Ah
		ld	a, (18B0h)
		or	a
		jr	z, loc_1967
		cp	(hl)
		jp	nz, 5BFh
loc_1967:	ld	de, (1800h)
		dec	de
		ld	hl, 188Eh
		push	hl
		ld	b, 10h
loc_1972:	call	0E4Ch
		ld	(hl), a
		inc	hl
		ld	(de), a
		dec	de
		djnz	loc_1972
		ld	(1800h), de
		ld	b, 3
		pop	hl
		ld	de, 186Eh
loc_1985:	ld	c, 20h
loc_1987:	ld	a, (de)
		cp	c
		jr	z, loc_1993
		cp	(hl)
		jp	nz, 5D4h
		inc	hl
		inc	de
		djnz	loc_1987
loc_1993:	ld	hl, (ARG1)	; Kdo-Argument 1
		ld	de, (1866h)
loc_199A:	call	0E4Ch
		ld	(hl), a
		inc	hl
		dec	de
		ld	a, e
		or	d
		jr	nz, loc_199A
		dec	hl
		ld	(ARG2), hl	; Kdo-Argument 2
		rst	prnst		; Ausgabe String
		db ':',0A0h
		ld	a, (188Ah)
		rst	outch		; Zeichenausgabe
		rst	prnst		; Ausgabe String
		db 0A0h	;  
		call	4CAh
		rst	30h
		rst	prnst		; Ausgabe String
		db 0A0h
		ex	de, hl
		rst	30h
		rst	prnst		; Ausgabe String
		db 0A0h
		call	q_ko21		; Start/Stop-Schaltung stopp
		pop	af
		ld	(kdov24), a
		push	de
		ld	h, b
		ld	l, c
		rst	30h
		rst	prnst		; Ausgabe String
		db 8Fh
		call	CRC		; Berechnung und Ausgabe CRC
		rst	prnst		; Ausgabe String
		db 8Dh
		pop	de
		call	0A7Bh
		ret	nz
		jp	(hl)
		ret

ende:		db 0FFh

		end
