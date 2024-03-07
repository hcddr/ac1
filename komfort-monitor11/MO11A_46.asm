; File Name   :	d:\hobby3\ac1-2010\monitor\komfort-monitor11\MO11A_46.Z80
; Format      :	Binary file
; Base Address:	0000h Range: 18E0h - 1A00h Loaded length: 0120h

		cpu	z80undoc

; reass. V.Pohlers 01.02.2023

; Nachladbare Kommandodatei
; =========================
; 
; F:	MO11A_46.z80		Suche
; -----------------------------------------------------------------------------
; Syntax:	F aaaa bbbb cc dd...	HEX
; 		F aaaa bbbb 'ABC..	ASCII
; 
; Der Speicher wird von Adresse aaaa bis bbbb nach der Bytefolge cc dd .. 
; (Länge beliebig) durchsucht. Suche nach ASCII-Zeichen ist ebenfalls möglich, 
; diese ist hinter einem Hochkomma anzugeben. Sämtliche im Speicher gefundenen 
; Zeichenketten werden ausgeschrieben (Anfangsadresse). 
; Ist die Kette sooft vorhanden, dass nicht alle Adressen auf den Schirm passen, 
; wird die Suche abgebrochen; günstig ist, vorher den Bildschirm zu löschen. 
; bbbb max. FFFE.
; Die Routine ist verschieblich und wird nach 1900H geladen, kann aber auch
; mit dem Aufruf # U xxxx "MO11A_46.z80" in den Speicher ab xxxx geladen werden.
; 

		include	ac1-2010.asm

;interne Monitor-Funktionen
para		equ	04cah		; Register mit Argumenten laden, aaaa=HL, bbbb=DE, cccc=BC

;Monitor-Speicherzellen
unk_18D0	equ	18D0h		; nur in sub_A61 genutzt (UP zu Find-Kdo)
unk_18E8	equ	18E8h		; nur in sub_A61 genutzt (UP zu Find-Kdo)


		org 1900h - 20h

; z80-Header
		dw anf
		dw ende
		dw start
		db "MO11.0"
		db  'P'
		db 0D3h,0D3h,0D3h
		db "ext.Komm F      "

; ---------------------------------------------------------------------------
; F aaaa bbbb cc dd ... Finding String Hex
; F aaaa bbbb 'ABC.... Finding String ASCII
; aus Monitor SCCH 1088, leicht angepasst
; ---------------------------------------------------------------------------

anf:		db 0,9,'F',0Dh

start:		;call	sub_A61
		exx
		ld	hl, unk_18D0
		ld	de, unk_18E8
		exx
;
		ld	a, (de)
		cp	'J'		; Jokerzeichen
		jr	z, f_ko4
		cp	27h ; '''	; ASCII
		jr	z, f_ko1
		jr	f_ko3
f_ko1:		dec	de
		ld	a, (de)
		cp	' '
		jr	z, f_ko2
		exx
		ld	(hl), a
		inc	hl
		ld	a, 1
		ld	(de), a
		inc	de
		exx
		jr	f_ko1
f_ko2:		dec	de
		ld	a, (de)
		cp	'J'		; Jokerzeichen
		jr	z, f_ko4
		cp	27h ; '''	; ASCII
		jr	z, f_ko1
		cp	' '
		jr	z, f_ko5
		call	INHEX; konv. ASCII-Hex ab (DE) abwärts nach HL
f_ko3:		ld	a, l
		exx
		ld	(hl), a
		inc	hl
		ld	a, 1
		ld	(de), a
		inc	de
		exx
		jr	f_ko2
f_ko4:		exx
		inc	hl
		xor	a
		ld	(de), a
		inc	de
		exx
		dec	de
		jr	f_ko2
f_ko5:		exx
		ld	a, 0FFh
		ld	(de), a
		exx
		call	para		; Register mit Argumenten laden
		ld	bc, 164h	; 1,'d' ??  
f_ko6:		;call	sub_A61
		;call	sub_4AD
		exx
		ld	hl, unk_18D0
		ld	de, unk_18E8
		ld	a, (de)
		and	a
		jr	z, f_ko6a
		cp	2
		ld	a, (hl)
f_ko6a:		inc	de
		inc	hl
		exx
f_ko7:		push	af
		and	a
		push	hl
		sbc	hl, de
		pop	hl
		jr	nc, f_ko8
		pop	af
		jr	z, f_ko11
		cp	(hl)
		jr	z, f_ko11
		inc	hl
		jr	f_ko7
;		
f_ko8:		pop	af
f_ko9:		;call	sub_A61
		exx
		ld	hl, unk_18D0
		ld	de, unk_18E8
		;call	sub_A6A
		ld	(hl), c
		push	hl
		xor	a
		ex	de, hl
		sbc	hl, de
		ld	b, h
		ld	c, l
		pop	hl
		dec	de
		ldir
		exx
		dec	b
		jr	nz, f_ko10
		rst	prnst		; Ausgabe String
		db " Not foun",0E4h
f_ko10:		rst	prnst		; Ausgabe String
		db  8Dh	
		ret
;		
f_ko6j:		jr	f_ko6
;
f_ko11:		push	hl
		pop	ix
		ld	a, 18h
		cp	h
		jr	nz, f_ko12
		ld	a, 0D0h
		cp	l
		jr	z, f_ko13
f_ko12:		inc	hl
		;call	sub_4AD
		exx
		ld	a, (de)
		and	a
		jr	z, f_ko12a
		cp	2
		ld	a, (hl)
f_ko12a:	inc	de
		inc	hl
		exx
		jr	z, f_ko12
		jr	nc, f_ko14
		cp	(hl)
		jr	z, f_ko12
f_ko13:		push	ix
		pop	hl
		inc	hl
		jr	f_ko6
;		
f_ko14:		push	ix
		pop	hl
		xor	a
		add	a, b
		jr	z, f_ko15
		ld	(ARG1), hl	; Kdo-Argument 1
f_ko15:		rst	prnst		; Ausgabe String
		db 0A0h
		call	OUTHL		; Ausgabe HL hexadezimal
		rst	prnst		; Ausgabe String
		db 0BAh
		ld	b, 4
f_ko16:		ld	a, (hl)
		call	OUTHEX		; Ausgabe A hexadezimal
		inc	hl
		djnz	f_ko16
		rst	prnst		; Ausgabe String
		db 20h
		db 0A0h	;  
		push	ix
		pop	hl
		inc	hl
		dec	c
		jr	nz, f_ko6j
		rst	inch		; Zeicheneingabe
		cp	'Q'
		jr	z, f_ko9
		ld	c, 64h 		; 'd' ??
		jr	f_ko6j

		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
ende:		db 0FFh

		end

; ; orig 1088:
; sub_4AD:	exx
; 		ld	a, (de)
; 		and	a
; 		jr	z, loc_4B5
; 		cp	2
; 		ld	a, (hl)
; loc_4B5:	inc	de
; 		inc	hl
; 		exx
; 		ret
; 
; ; UP zu Find-Kdo
; sub_A61:	exx
; 		ld	hl, unk_18D0
; 		ld	de, unk_18E8
; 		exx
; 		ret
; 
; ; UP zu Find-Kdo
; sub_A6A:	ld	(hl), c
; 		push	hl
; 		xor	a
; 		ex	de, hl
; 		sbc	hl, de
; 		ld	b, h
; 		ld	c, l
; 		pop	hl
; 		dec	de
; 		ldir
; 		exx
; 		ret

