; File Name   :	d:\hobby3\ac1\monitor\zetbug\zetbug_k.bin
; Format      :	Binary file
; Base Address:	0000h Range: 43B0h - 4A00h Loaded length: 0650h

;ZETBUG f. TRS-80 Model I funkschau 80/11
;reass: Volker Pohlers, Neustadt i.H., 24.08.2018
;letzte Änderung: 24.08.2018

; Makros
hi              function x, (x>>8) & 0ffh	; High-Byte
lo              function x, x & 0ffh		; Low-Byte

		cpu	z80

; Systemaufrufe
; s. http://www.trs-80.com/wordpress/zaps-patches-pokes-tips/internal/

KBDSCN		equ	002Bh 	; Scan keyboard and return with accumulator containing result. DE is used.
VDCHAR          equ	0033h 	; Displays a character at current cursor location.
CSOFF           equ	01F8h	; This routine turns off the cassette drive.
DEFCAS          equ	0212h	; A register contains a 0 or 1 which is the cassette number. This routine defines cassette number and turns on cassette. Model I only.
CSIN          	equ	0235h	; Inputs data one byte at a time from cassette after you use CSHIN. A = the data byte.
CSOUT          	equ	0264h	; Outputs data one byte at a time to cassette after you use CSHWR. A = the output byte.
CSHWR         	equ	0287h	; Turns on the cassette and writes the header.
CSHIN         	equ	0296h	; Finds the cassette header info at the beggining of cassette file.
up_458h         equ	0458h	; Video driver for Model I.
up_58Dh         equ	058Dh	; Line printer driver for Model I.
;
r401Eh		equ	401Eh	; Two byte video driver vector.
cupos		equ	4020h	; Two byte cursor position.



		org 4390h
STACK:		
DATA:		ds 3
SOIL:		ds 2		; Start Of Input Line
ARG1:		ds 2
ARG2:		ds 2
ARG3:		ds 2
RSA:				; Register Save Area
REGAFX:		ds 2
REGBCX:		ds 2
REGDEX:		ds 2
REGHLX:		ds 2
REGAF:		ds 2
REGBC:		ds 2
REGDE:		ds 2
REGHL:		ds 2
REGIX:		ds 2
REGIY:		ds 2
REGPC:		ds 2
REGSP:		ds 2
BPADR:		ds 2
BPOPC:		ds 3

;------------------------------------------------------------------------------
; UP liest ein ASCII-Zeichen von der Tastatur, Character in accu.
;------------------------------------------------------------------------------

INCH:		push	de
		push	iy
inch1:		call	KBDSCN
		or	a
		jr	z, inch1
		pop	iy
		pop	de
		ret

;------------------------------------------------------------------------------
; UP setzt ein Zeichen auf den Bildschirm und bewegt den Cursor (character in accu).
;------------------------------------------------------------------------------

OUTCH:		push	af
		push	de
		push	iy
		cp	8
		jr	z, outch1	; backspace
outch2:		call	nz, VDCHAR
		pop	iy
		pop	de
		pop	af
		ret
outch1:		ld	hl, (cupos)
		ld	a, '#'
		dec	hl
		dec	hl
		cp	(hl)
		ld	a, 8
		jr	outch2

;------------------------------------------------------------------------------
; Aufruf: CD E1 43 aa bb cc dd nn 00. 
; UP druckt den ASCII-string aa bb nn auf den Bildschirm und bewegt den Cursor.
;------------------------------------------------------------------------------

PRINT:		ex	(sp), hl
print1:		ld	a, (hl)
		inc	hl
		or	a
		jr	z, print2
		call	OUTCH
		jr	print1
print2:		ex	(sp), hl
		ret

;------------------------------------------------------------------------------
; UP liest eine Zeile vom Bildschirm (wickelt alle notwendigen INCH/OUTCH-
; Routinen ab) und errechnet den Zeilenanfang (Start of line in 4393+4394). 
; Der Zeilenanfang ist immer zwei Zeichen nach dem Promptsymbol
;------------------------------------------------------------------------------

INLINE:		push	af
		push	hl
		call	PRINT
		db " # ",0
inline1:	call	INCH
		call	OUTCH
		cp	0Dh
		jr	nz, inline1
		ld	hl, (cupos)
		ld	a, '#'
inline2:	cpd
		jr	nz, inline2
		inc	hl
		inc	hl
		inc	hl
		ld	(SOIL),	hl
		pop	hl
		pop	af
		ret

;------------------------------------------------------------------------------
; fuehrende Leerzeichen	ueberlesen
; letzen vier Zeichen als Hexzahl konvertieren
; und in DATA ablegen
;------------------------------------------------------------------------------

konvx:		ld	a, (de)
		cp	' '
		inc	de
		jr	z, konvx
		dec	de
		xor	a
		ld	hl, DATA
		ld	(hl), a
		inc	hl
		ld	(hl), a
		inc	hl
		ld	(hl), a
konvx1:		ld	a, (de)
		dec	hl
		dec	hl
		sub	30h ; '0'
		ret	m
		cp	0Ah
		jr	c, konvx2
		sub	7
		cp	0Ah
		ret	m
		cp	10h
		ret	p
konvx2:		inc	de
		inc	(hl)
		inc	hl
		rld
		inc	hl
		rld
		jr	konvx1

;------------------------------------------------------------------------------
; UP liest eine 4-digit-Hexzahl vom Schirm in das HL-Register. 
; Der Bufferpointer ist das DE-Register (wird aut. nachkorrigiert).
;------------------------------------------------------------------------------

INHEX:		push	bc
		push	af
		call	konvx
		inc	hl
		ld	b, h
		ld	c, l
		ld	l, (hl)
		inc	bc
		ld	a, (bc)
		ld	h, a
		pop	bc
		or	l
		ld	a, b
		pop	bc
		ret

;------------------------------------------------------------------------------
; UP druckt den Accu im Hexformat aus.
;------------------------------------------------------------------------------

OUTHEX:		push	af
		rra
		rra
		rra
		rra
		call	outhex1
		pop	af
outhex1:	push	af
		and	0Fh
		add	a, 30h ; '0'
		cp	3Ah ; ':'
		jr	c, outhex2
		add	a, 7
outhex2:	call	OUTCH
		pop	af
		ret

;------------------------------------------------------------------------------
; wie OUTHEX, jedoch HL-Register.
;------------------------------------------------------------------------------

OUTHL:		push	af
		ld	a, h
		call	OUTHEX
		ld	a, l
		call	OUTHEX
		pop	af
		ret

;------------------------------------------------------------------------------
; Kommandoschleife
;
; Suchen des Kommandos
; wenn ':' folgt, dann alte Parameter nehmen
; sonst 3 Parameter einlesen und in ARG1..ARG3 ablegen
; bei Kdo-Start gilt
; 	BC = Return-Adr
; 	DE zeigt auf ':' oder auf Leerzeichen hinter letzem Parameter
; 	HL = Kdo-Adr
;------------------------------------------------------------------------------

kdoerr:		call	PRINT
		db "WHAT?  ",0

kdo1:		call	INLINE
		ld	hl, kdo4+2
		ld	bc, 0FFFh
kdo2:		xor	a
		inc	bc
		cpir
		jr	nz, kdoerr
		ld	a, 9
		cp	(hl)
		jr	nz, kdo2
		inc	hl
		ld	de, (SOIL)
		ld	a, (de)
		cp	(hl)
		jr	nz, kdo2
		inc	hl
		ld	a, (hl)
		cp	0Dh
		jr	nz, kdo2
		inc	hl
		push	hl
;Parameter
		inc	de
		call	INHEX
		jr	nz, kdo3
		ld	a, (de)
		cp	':'
		jr	z, kdo4
kdo3:		ld	(ARG1),	hl
		call	INHEX
		ld	(ARG2),	hl
		call	INHEX
		ld	(ARG3),	hl
;Starten
kdo4:		pop	hl
		ld	bc, kdo1
		push	bc
		jp	(hl)

;------------------------------------------------------------------------------
; # ' ' aaaa bbbb cccc (Eingabe von Argumenten)
;------------------------------------------------------------------------------

		db 0,9,' ',0Dh

		ret

;------------------------------------------------------------------------------
; ZETBUG-Entrypoint. Wird angesprungen, wenn ZETBUG aufgerufen werden soll 
; (z. B. nach dem Laden van der Kassette)
;------------------------------------------------------------------------------

ENTRY:		ld	sp, STACK
		call	PRINT
		db 0Eh,0Dh,0Dh,"ZETBUG   Z80-MONITOR   V1.0",0Dh,0Dh,0

;------------------------------------------------------------------------------
; # I (Initialize RegisterSaveArea (RSA) and Memory-map)
;------------------------------------------------------------------------------

		db 0,9,'I',0Dh

		ld	hl, DATA
		ld	d, h
		ld	e, l
		inc	de
		ld	(hl), 0
		ld	bc, 39		; Bereich data..BPOPC+3
		ldir
		ld	a, 50h     ; 5000
		ld	(REGSP+1), a
		ld	sp, STACK
		call	PRINT
		db "CLR/RSA",0Dh,0
		jp	kdo1

;------------------------------------------------------------------------------
; # T aaaa bbbb (Tabulate memory on screen)
;------------------------------------------------------------------------------

		db 0,9,'T',0Dh

tko:		ld	hl, (ARG1)
tko1:		ld	de, (ARG2)
		push	hl
		sbc	hl, de
		pop	hl
		ret	nc
		call	OUTHL
		ld	b, 10h
tko2:		call	PRINT
		db ' ',0
tko3:		ld	a, (hl)
		call	OUTHEX
		inc	hl
		djnz	tko2
		call	PRINT
		db 0Dh,0
		jr	tko1

;------------------------------------------------------------------------------
; # M aaaa (Modify and inspect memory)
;------------------------------------------------------------------------------

		db 0,9,'M',0Dh

mem:		ld	hl, (ARG1)
mem1:		call	OUTHL
		call	PRINT
		db "  ",0
		ld	a, (hl)
		call	OUTHEX
		call	INLINE
		ld	de, (SOIL)
		dec	hl
mem2:		inc	hl
		push	hl
		call	INHEX
		jr	z, mem5
mem3:		ld	a, l
		pop	hl
		ld	(hl), a
		cp	(hl)
		jr	z, mem2
		call	PRINT
		db "ROM ERROR  ",0
mem4:		jr	mem1
;
mem5:		ld	a, (de)
		cp	' '
		jr	z, mem3
		pop	hl
		inc	hl
		ld	(ARG2),	hl
		cp	'.'
		ret	z
		ld	de, (SOIL)
		ld	a, (de)
		cp	' '
		jr	z, mem4
		dec	hl
		jr	mem4

;------------------------------------------------------------------------------
; 
;------------------------------------------------------------------------------

para:		ld	hl, (ARG1)
		ld	de, (ARG2)
		ld	bc, (ARG3)
		ret

;------------------------------------------------------------------------------
; # K aaaa bbbb cc (Verify memory)
;------------------------------------------------------------------------------

		db 0,9,'K',0Dh

		call	para
		ld	(hl), c
		push	hl
		xor	a
		ex	de, hl
		sbc	hl, de
		ld	b, h
		ld	c, l
		pop	hl
		ld	d, h
		ld	e, l
		inc	de
kko1:		ldir
		ret

;------------------------------------------------------------------------------
; # C aaaa bbbb cccc (Copy memory)
;------------------------------------------------------------------------------

		db 0,9,'C',0Dh

		call	para
		xor	a
		push	hl
		sbc	hl, de
		pop	hl
		jr	nc, kko1
		add	hl, bc
		ex	de, hl
		add	hl, bc
		ex	de, hl
		dec	hl
		dec	de
		lddr
		ret

;------------------------------------------------------------------------------
; # A aaaa bbbb c (Arithmetic)
;------------------------------------------------------------------------------

		db 0,9,'A',0Dh

		call	para
		push	hl
		push	de
		add	hl, bc
		ex	de, hl
		sbc	hl, de
		ld	a, 0
		jr	c, ako1
		cp	h
		jr	nz, ako3
		bit	7, h
		jr	nz, ako3
		jr	ako2
ako1:		cpl
		cp	h
		jr	nz, ako3
		bit	7, h
		jr	z, ako3
ako2:		call	PRINT
		db "DSPL:",0
		ld	a, l
		call	OUTHEX
ako3:		pop	de
		pop	hl
		push	hl
		push	hl
		add	hl, de
		call	PRINT
		db "   SUM:",0
		call	OUTHL
		pop	hl
		sbc	hl, de
		call	PRINT
		db "   DIF:",0
		call	OUTHL
		pop	bc
		ld	hl, 0
		ld	e, l
ako4:		ld	a, b
		or	c
		jr	z, ako5
		dec	bc
		ld	a, l
		inc	a
		daa
		ld	l, a
		ld	a, h
		adc	a, 0
		daa
		ld	h, a
		jr	nc, ako4
		inc	e
		jr	ako4
ako5:		call	PRINT
		db "   DEC:",0
		ld	a, e
		and	0Fh
		or	30h ; '0'
		call	OUTCH
		call	OUTHL
outcr:		call	PRINT
		db 0Dh,0
		ret

;------------------------------------------------------------------------------
; # V aaaa bbbb cccc (Verify memory) 
;------------------------------------------------------------------------------

		db 0,9,'V',0Dh

		call	para
vko1:		ld	a, (de)
		cp	(hl)
		jr	nz, vko3
vko2:		dec	bc
		inc	hl
		inc	de
		ld	a, b
		or	c
		ret	z
		jr	vko1
vko3:		call	anzw
		ld	a, (hl)
		call	OUTHEX
		call	out3sp
		call	out3sp
		ex	de, hl
		call	anzw
		ex	de, hl
		ld	a, (de)
		call	OUTHEX
		call	outcr
		call	INCH
		cp	0Dh
		ret	nz
		jr	vko2

;------------------------------------------------------------------------------
; # F aa bb cc dd ee ff ... nn (Find data string)
;------------------------------------------------------------------------------

		db 0,9,'F',0Dh

		ld	bc, (ARG1)
fko1:		ld	de, (SOIL)
		inc	de
		inc	de
		inc	bc
		call	INHEX
fko2:		ld	a, (bc)
		cp	l
		jr	z, fko3
		inc	bc
		ld	a, b
		or	c
		jr	z, fko5
		jr	fko2
fko3:		push	bc
fko4:		call	INHEX
		inc	bc
		ld	a, (bc)
		cp	l
		jr	z, fko4
		ld	a, (de)
		cp	20h ; ' '
		pop	bc
		jr	z, fko1
		ld	(ARG1),	bc
		jp	mem
fko5:		call	PRINT
		db 0Eh," NOT FOUND",0Dh,0
		ret

;------------------------------------------------------------------------------
; Registeranzeige
;------------------------------------------------------------------------------

anzsp:		call	PRINT
		db "SP:",0
		jr	anzw
anzpc:		call	PRINT
		db "PC:",0
		jr	anzw
anziy:		call	PRINT
		db "IY:",0
		jr	anzw
anzix:		call	PRINT
		db "IX:",0
		jr	anzw
anzhl:		call	PRINT
		db "HL:",0
		jr	anzw
anzde:		call	PRINT
		db "DE:",0
		jr	anzw
anzbc:		call	PRINT
		db "BC:",0
		jr	anzw
anzaf:		call	PRINT
		db "AF:",0
anzw:		call	OUTHL
out3sp:		call	PRINT
		db "    ",0
		ret

;------------------------------------------------------------------------------
; # R (Register display / modify)
; # R: 		Register display
; # R/XX	Register modify
;------------------------------------------------------------------------------

		db 0,9,'R',0Dh

		cp	':'
		jp	nz, rko1
		call	PRINT
		db "BP:",0
		ld	hl, (BPADR)
		call	anzw
		call	PRINT
		db "BS:",0
		ld	hl, BPOPC
		ld	a, (hl)
		call	OUTHEX
		inc	hl
		ld	a, (hl)
		call	OUTHEX
		inc	hl
		ld	a, (hl)
		call	OUTHEX
		call	outcr
		ld	hl, (REGSP)
		call	anzsp
		ld	hl, (REGPC)
		call	anzpc
		ld	hl, (REGIY)
		call	anziy
		ld	hl, (REGIX)
		call	anzix
		call	PRINT
		db 0Dh,"MAIN  ",0
		ld	hl, (REGHL)
		call	anzhl
		ld	hl, (REGDE)
		call	anzde
		ld	hl, (REGBC)
		call	anzbc
		ld	hl, (REGAF)
		call	anzaf
		call	PRINT
		db 0Dh,"EXXR  ",0
		ld	hl, (REGHLX)
		call	anzhl
		ld	hl, (REGDEX)
		call	anzde
		ld	hl, (REGBCX)
		call	anzbc
		ld	hl, (REGAFX)
		call	anzaf
		call	PRINT
		db 0Dh,"FLAGS:  ",0
		ld	a, (REGAF)
		call	flags
		call	PRINT
		db "  (",0
		ld	a, (REGAFX)
		call	flags
		call	PRINT
		db ')',0Dh,0
		ret

flags:		ld	l, a
		bit	7, l
		call	nz, PRINT
		db 'S',0
		bit	6, l
		call	nz, PRINT
		db 'Z',0
		bit	4, l
		call	nz, PRINT
		db 'H',0
		bit	2, l
		call	nz, PRINT
		db 'P',0
		bit	1, l
		call	nz, PRINT
		db 'N',0
		bit	0, l
		call	nz, PRINT
		db 'C',0
		ret
		
; Register aendern
rko1:		ld	hl, (SOIL)
		inc	hl
		inc	hl
		ld	a, (hl)
		ld	de, REGAF
		cp	'A'
		jr	z, rko2
		inc	de
		inc	de
		cp	'B'
		jr	z, rko2
		inc	de
		inc	de
		cp	'D'
		jr	z, rko2
		inc	de
		inc	de
		cp	'H'
		jr	nz, rko4
rko2:		inc	hl
		inc	hl
		ld	a, (hl)
		cp	27h ; '''
		ex	de, hl
		jr	nz, rko3
		ld	de, 8
		sbc	hl, de
rko3:		ld	e, (hl)
		inc	hl
		ld	d, (hl)
		ex	de, hl
		call	OUTHL
		call	INLINE
		ld	hl, (SOIL)
		ex	de, hl
		push	hl
		push	de
		call	INHEX
		ex	de, hl
		pop	hl
		ld	a, (hl)
		cp	' '
		pop	hl
		ret	z
		ld	(hl), d
		dec	hl
		ld	(hl), e
		ret
rko4:		inc	de
		inc	de
		inc	hl
		ld	a, (hl)
		cp	'X'
		jr	z, rko5
		inc	de
		inc	de
		cp	'Y'
		jr	z, rko5
		inc	de
		inc	de
		cp	'C'
		jr	z, rko5
		inc	de
		inc	de
		cp	'P'
		jp	nz, kdoerr
rko5:		ex	de, hl
		jr	rko3

;------------------------------------------------------------------------------
; UP rettet die CPU-Register (außer SP und PC) in die RSA.
;------------------------------------------------------------------------------

SAVE:		ld	(DATA), sp
		ld	sp, REGPC
		push	iy
		push	ix
		push	hl
		push	de
		push	bc
		push	af
		exx
		ex	af, af'
		push	hl
		push	de
		push	bc
		push	af
		jr	load1

;------------------------------------------------------------------------------
; Unterprogramm schreibt die RSA in die CPU zurück (außer SP- und PC-Register!)
;------------------------------------------------------------------------------

LOAD:		ld	(DATA), sp
		ld	sp, REGAFX
		pop	af
		pop	bc
		pop	de
		pop	hl
		exx
		ex	af, af'
		pop	af
		pop	bc
		pop	de
		pop	hl
		pop	ix
		pop	iy
load1:		ld	sp, (DATA)
		ret

;------------------------------------------------------------------------------
; Breakpointbehandlungsroutine (siehe B). 
; Kann außerdem als Rücksprungadresse von einem Benutzerprogramm verwendet 
; werden, wenn die CPU-Register in die RSA gerettet werden sollen.
;------------------------------------------------------------------------------

BREAK:		call	SAVE
		pop	hl
		ld	(REGSP), sp
		ld	sp, STACK
		call	PRINT
aBreakIn:	db "BREAK IN:",0
		dec	hl
		dec	hl
		dec	hl
		ld	(REGPC), hl
		call	anzw

;------------------------------------------------------------------------------
; Adresse, die ein Benutzerprogramm anspringen muß, wenn es zu ZETBUG 
; zurückkehren will. Keines der CPU-Register wird gerettet!
;------------------------------------------------------------------------------

RETURN:		ld	hl, (BPADR)
		ld	de, BPOPC
		ld	bc, 3
		ex	de, hl
		ldir
		ld	sp, STACK
		jp	kdo1

;------------------------------------------------------------------------------
;  #B aaaa (Breakpoint)
;------------------------------------------------------------------------------

		db 0,9,'B',0Dh

		ld	hl, (ARG1)
		ld	(BPADR), hl
		ld	de, BPOPC
		ld	bc, 3
		ldir
		ret

;------------------------------------------------------------------------------
; # E aaaa (Execute machine program)
;------------------------------------------------------------------------------

		db 0,9,'E',0Dh

ekdo:		ld	hl, (BPADR)
		ld	(hl), 0CDh ; 'Í'
		inc	hl
		ld	(hl), lo(BREAK)
		inc	hl
		ld	(hl), hi(BREAK)

;vp: hier fehlt eigentlich ein jr j_kdo
;die nachfolgende Kommandokennung wird dekodiert als 
;	nop
;	add     hl, bc
;	ld      c, d
;	dec     c
;das bringt zum Glueck das Programm nicht durcheinander

;------------------------------------------------------------------------------
; # J aaaa (Jump machine program)
;------------------------------------------------------------------------------

		db    0,9,'J',0Dh

eko1:		ld	hl, (ARG1)
eko2:		ld	(REGPC), hl
		ld	sp, (REGSP)
		push	hl
		jp	LOAD

;------------------------------------------------------------------------------
; # G (Go on / continue execution at PC)
;------------------------------------------------------------------------------

		db 0,9,'G',0Dh

		ld	hl, (REGPC)
		ld	(ARG1),	hl
		ld	de, (BPADR)
		xor	a
		sbc	hl, de
		jr	nz, ekdo
		jr	eko1

;------------------------------------------------------------------------------
;   P aaaa bbbb (Print data)
;------------------------------------------------------------------------------

		db 0,9,'P',0Dh

		ld	hl, (cupos)	; save cursor position
		push	hl
		ld	hl, up_58Dh	; Line printer driver
		ld	(r401Eh), hl	; video driver vector
		call	tko		; dump
		ld	hl, up_458h	; Video driver
		ld	(r401Eh), hl	; video driver vector
		pop	hl
		ld	(cupos), hl	; restore cursor position
		ret

;------------------------------------------------------------------------------
; # L aaaa± (Load data from cassette into memory)
;------------------------------------------------------------------------------

		db 0,9,'L',0Dh

cload:		ld	a, (de)
		ld	de, (ARG1)
		cp	'-'
		jr	nz, loa1
		ld	hl, 0
		sbc	hl, de
		ex	de, hl
		ld	(ARG1),	de
loa1:		xor	a
		call	DEFCAS
		call	CSHIN
		ld	b, 7
loa2:		call	CSIN
		djnz	loa2
		call	CSIN
loa3:		cp	3Ch ; '<'
		jr	nz, loa6
		call	CSIN
		ld	b, a
		call	CSIN
		ld	l, a
		call	CSIN
		ld	h, a
		add	a, l
		ld	c, a
		ld	de, (ARG1)
		add	hl, de
loa4:		call	CSIN
		ld	(hl), a
		add	a, c
		ld	c, a
		inc	hl
		djnz	loa4
		call	CSIN
		cp	c
		nop
		nop
		call	CSIN
		cp	78h ; 'x'
		jr	nz, loa3
loa5:		jp	CSOFF
loa6:		call	PRINT
		db "C??",0Dh,0
		jr	loa5

;------------------------------------------------------------------------------
; # D aaaa bbbb cccc name (dump data to cassette)
;------------------------------------------------------------------------------

		db 0,9,'D',0Dh

csave:		inc	de
		ld	a, (de)
		cp	' '
		jr	z, csave
		xor	a
		call	DEFCAS
		call	CSHWR
		ld	a, 55h ; 'U'
		call	CSOUT
		ld	b, 6
sav1:		ld	a, (de)
		call	CSOUT
		inc	de
		djnz	sav1
		ld	de, (ARG1)
sav2:		ld	hl, (ARG2)
		inc	hl
		sbc	hl, de
		jr	z, sav5
		ld	bc, 100h
		push	hl
		sbc	hl, bc
		pop	hl
		jr	c, sav3
		ld	l, 0
sav3:		ld	b, l
		ld	a, 3Ch ; '<'
		call	CSOUT
		ld	a, b
		call	CSOUT
		ld	a, e
		call	CSOUT
		ld	a, d
		call	CSOUT
		add	a, e
		ld	c, a
sav4:		ld	a, (de)
		add	a, c
		ld	c, a
		ld	a, (de)
		call	CSOUT
		inc	de
		djnz	sav4
		ld	a, c
		call	CSOUT
		jr	sav2
sav5:		ld	a, 78h ; 'x'
		call	CSOUT
		ld	a, (ARG3)
		call	CSOUT
		ld	a, (ARG3+1)
		call	CSOUT
		jp	CSOFF

		end	ENTRY
