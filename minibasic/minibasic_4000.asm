; File Name   :	d:\hobby3\ac1\minibasic\minibasic.bin
; Base Address:	0000h Range: 0800h - 1000h Loaded length: 0800h
; reass: V.Pohlers 20.08.2018

		cpu	z80

; Speicheraufteilung
BASIC		equ	4000h
RAM		equ	1800h

; Monitor-Einsprünge
inch		equ	0008h
outch		equ	0010h
MS30	    	equ	07EBh	; 30 ms	warten
;OUTHEX      	equ	07EEh	
;OUTHL	    	equ	07F1h	; Ausgabe HL hexadezimal
;INLINE	    	equ	07F4h	; Zeile	eingeben
;INHEX       	equ	07F7h	
TASTE       	equ	07FAh	
GETCO1      	equ	07FDh	


;**********************************************************
;  Mini  B A S I C  INTERPRETER     AC1
; basiert auf Basic-Interpreter: Funktionsweise u. Implementierung
; in 8080/Z-80-Computern / Rolf-Dieter Klein. München: Franzis, 1981
;**********************************************************

		org BASIC

		jp	START
		jp	RSTART
;
;CI:		jp	inch
CI:		jp	inchc
CO:		jp	outch
CSTS:		call	TASTE
		jr	nz, cs1
		xor	a
		ret
cs1:		ld	a, 0FFh
		and	a
		ret
;
COMP:		ld	a, h
		cp	d
		ret	nz
		ld	a, l
		cp	e
		ret
;
IGNB:		ld	a, (de)
		cp	' '
		ret	nz
		inc	de
		jr	IGNB
;
FINI:		pop	af
		call	FIN
		jp	QWHAT
;
TSTV:		call	IGNB
		sub	40h ; '@'
		ret	c
		jr	nz, TV1
		inc	de
		call	PARN
		add	hl, hl
		jr	c, QHOW
		push	de
		ex	de, hl
		call	SIZE
		call	COMP
		jp	c, ASORRY
		ld	hl, (TXTEND)
		sbc	hl, de
		pop	de
		ret
TV1:		cp	1Bh
		ccf
		ret	c
		inc	de
		ld	hl, VARBGN
		rlca
tv2:		add	a, l
		ld	l, a
		ld	a, 0
		adc	a, h
		ld	h, a
		ret
;
TSTC:		ex	(sp), hl
		call	IGNB
		cp	(hl)
TC1:		inc	hl
		jr	z, TC2
		push	bc
		ld	c, (hl)
		ld	b, 0
		add	hl, bc
		pop	bc
		dec	de
TC2:		inc	de
		inc	hl
		ex	(sp), hl
		ret
;
TSTNUM:		ld	hl, 0
		ld	b, h
		call	IGNB
TN1:		cp	'0'
		ret	c
		cp	3Ah ; '9'+1
		ret	nc
		ld	a, 0F0h
		and	h
		jr	nz, QHOW
		inc	b
		push	bc
		ld	b, h
		ld	c, l
		add	hl, hl
		add	hl, hl
		add	hl, bc
		add	hl, hl
		ld	a, (de)
		inc	de
		and	0Fh
		call	tv2
		pop	bc
		ld	a, (de)
		jr	TN1
;
QHOW:		push	de
AHOW:		ld	de, HOW
		jp	ERROR
;
;*****************************************************
;    EXPR		 UPR
;*****************************************************
;
EXPR:		call	EXPR2
		push	hl
		ld	hl, TAB8-1
		jp	EXEC
XP11:		call	XP18
		ret	c
		ld	l, a
		ret
XP12:		call	XP18
		ret	z
		ld	l, a
		ret
XP13:		call	XP18
		ret	z
		ret	c
		ld	l, a
		ret
XP14:		call	XP18
		ld	l, a
		ret	z
		ret	c
		ld	l, h
		ret
XP15:		call	XP18
		ret	nz
		ld	l, a
		ret
XP16:		call	XP18
		ret	nc
		ld	l, a
		ret
XP17:		pop	hl
		ret
XP18:		ld	a, c
		pop	hl
		pop	bc
		push	hl
		push	bc
		ld	c, a
		call	EXPR2
		ex	de, hl
		ex	(sp), hl
		call	CKHLDE
		pop	de
		ld	hl, 0
		ld	a, 1
		ret
;
EXPR2:		call	TSTC
		db '-'
		db    XP21-$-1
		ld	hl, 0
		jr	XP26
XP21:		call	TSTC
		db '+'
		db	XP22-$-1
XP22:		call	EXPR3
XP23:		call	TSTC
		db '+'
		db  XP25-$-1
		push	hl
		call	EXPR3
XP24:		ex	de, hl
		ex	(sp), hl
		ld	a, h
		xor	d
		ld	a, d
		add	hl, de
		pop	de
		jp	m, XP23
		xor	h
		jp	p, XP23
		jp	QHOW
XP25:		call	TSTC
		db '-'
		db  XP42-$-1
XP26:		push	hl
		call	EXPR3
		call	CHGSGN
		jr	XP24
;
EXPR3:		call	EXPR4
XP31:		call	TSTC
		db '*'
		db XP34-$-1
		push	hl
		call	EXPR4
		ld	b, 0
		call	CHKSGN
		ex	(sp), hl
		call	CHKSGN
		ex	de, hl
		ex	(sp), hl
		ld	a, h
		or	a
		jr	z, XP32
		ld	a, d
		or	d
		ex	de, hl
		jp	nz, AHOW
XP32:		ld	a, l
		ld	hl, 0
		or	a
		jr	z, XP35
XP33:		add	hl, de
		jp	c, AHOW
		dec	a
		jr	nz, XP33
		jr	XP35
XP34:		call	TSTC
		db '/'
		db  XP42-$-1
		push	hl
		call	EXPR4
		ld	b, 0
		call	CHKSGN
		ex	(sp), hl
		call	CHKSGN
		ex	de, hl
		ex	(sp), hl
		ex	de, hl
		ld	a, d
		or	e
		jp	z, AHOW
		push	bc
		call	DIVIDE
		ld	h, b
		ld	l, c
		pop	bc
XP35:		pop	de
		ld	a, h
		or	a
		jp	m, QHOW
		ld	a, b
		or	a
		call	m, CHGSGN
		jr	XP31
;
EXPR4:		ld	hl, TAB4-1
		jp	EXEC
XP40:		call	TSTV
		jr	c, XP41
ldhl:		ld	a, (hl)
		inc	hl
		ld	h, (hl)
		ld	l, a
		ret
XP41:		call	TSTNUM
		ld	a, b
		or	a
		ret	nz
PARN:		call	TSTC
		db '('
		db  XP43-$-1
		call	EXPR
		call	TSTC
		db ')'
		db  XP43-$-1
XP42:		ret
XP43:		jp	QWHAT
;
DIRECT:		ld	hl, TAB1-1
;
EXEC:		call	IGNB
		push	de
EX1:		ld	a, (de)
		inc	de
		cp	'.'
		jr	z, EX4
		inc	hl
		cp	(hl)
		jr	z, EX1
		or	80h
		cp	(hl)
		jr	z, EX5
		dec	de
		ld	a, 80h
		cp	(hl)
		jr	z, EX5
		dec	hl
EX2:		inc	hl
		bit	7, (hl)
		jr	z, EX2
		inc	hl
		inc	hl
		pop	de
		jr	EXEC
EX4:		inc	hl
		bit	7, (hl)
		jr	z, EX4
EX5:		inc	hl
		call	ldhl
		pop	af
		jp	(hl)
;
;************************************************
; DIVIDE CHKSGN CHGSGN CKHLDE	   UPR
;************************************************
;
DIVIDE:		push	hl
		ld	l, h
		ld	h, 0
		call	DV1
		ld	b, c
		ld	a, l
		pop	hl
		ld	h, a
DV1:		ld	c, 0FFh
DV2:		inc	c
		and	a
		sbc	hl, de
		jr	nc, DV2
		add	hl, de
		ret
;
CHKSGN:		ld	a, h
		or	a
		ret	p
;
CHGSGN:		ld	a, h
		or	l
		ret	z
		ld	a, h
		push	af
		cpl
		ld	h, a
		ld	a, l
		cpl
		ld	l, a
		inc	hl
		pop	af
		xor	h
		jp	p, QHOW
		ld	a, b
		xor	80h
		ld	b, a
		ret
;
CKHLDE:		ld	a, h
		xor	d
		jp	p, CK1
		ex	de, hl
CK1:		jp	COMP
;
;********************************************
;  SETVAL  FIN	ENDCHK	 ERROR	    UPR
;********************************************
;
SETVAL:		call	TSTV
		jr	c, QWHAT
		push	hl
		call	TSTC
		db '='
		db  SV1-$-1
		call	EXPR
		ld	b, h
		ld	c, l
		pop	hl
		ld	(hl), c
		inc	hl
		ld	(hl), b
		ret
;
FIN:		call	TSTC
		db ';'
		db    FI1-$-1
		pop	af
		jp	RUNSML
FI1:		call	TSTC
		db 0Dh
		db    FI2-$-1
		pop	af
		jp	RUNNXL
FI2:		ret
;
ENDCHK:		call	IGNB
		cp	0Dh
		ret	z
SV1:
QWHAT:		push	de
		ld	de, WHAT
ERROR:		sub	a
		call	PRTSTG
		pop	de
		ld	a, (de)
		push	af
		sub	a
		ld	(de), a
		ld	hl, (CURRNT)
		push	hl
		ld	a, (hl)
		inc	hl
		or	(hl)
		pop	de
		jp	z, RSTART
		ld	a, (hl)
		or	a
		jp	m, INPERR
		call	PRTLN
		dec	de
		pop	af
		ld	(de), a
		ld	a, '?'
		call	OUTC
		sub	a
		call	PRTSTG
		jp	RSTART
;
QSORRY:		push	de
ASORRY:		ld	de, SORRY
		jr	ERROR
;
;*****************************************
;  GETLN  FNDLN		    UPR
;*****************************************
;
GETLN:		call	OUTC
		ld	de, (BUFFER)
GL1:		call	CHKIO
		cp	8
		jr	z, GL3
		call	OUTC
		cp	0Ah
		jr	z, GL1
		or	a
		jr	z, GL1
		cp	1Bh
		jr	z, GL4
		ld	(de), a
		inc	de
		cp	0Dh
		ret	z
		call	CXBUFE
		jr	nz, GL1
GL3:		ld	a, e
		call	CXBUFA
		jr	z, GL4
		dec	de
		ld	a, 8
		call	OUTC
		jr	GL1
GL4:		call	CRLF
		ld	a, 0Bh
		jr	GETLN
;
FNDLN:		ld	a, h
		or	a
		jp	m, QHOW
		ld	de, TXTBGN
FNDLP:		push	hl
		ld	hl, (TXTUNF)
		dec	hl
		call	COMP
		pop	hl
		ret	c
		ld	a, (de)
		sub	l
		ld	b, a
		inc	de
		ld	a, (de)
		sbc	a, h
		jr	c, FL2
		dec	de
		or	b
		ret
FNDNXT:		inc	de
FL2:		inc	de
FNDSKP:		ld	a, (de)
		cp	0Dh
		jr	nz, FL2
		inc	de
		jr	FNDLP
;
;*******************************************
;  PRTSTG  QTSTG  PRTNUM  PRTLN	  UPR
;*******************************************
;
PRTSTG:		ld	b, a
PS1:		ld	a, (de)
		inc	de
		cp	b
		ret	z
		call	OUTC
		cp	0Dh
		jr	nz, PS1
		ret
;
QTSTG:		call	TSTC
		db '"'
		db  QT3-$-1
		ld	a, '"'
QT1:		call	PRTSTG
		cp	0Dh
		pop	hl
		jp	z, RUNNXL
		inc	hl
		inc	hl
		inc	hl
		jp	(hl)
QT3:		call	TSTC
		db 27h		;'
		db  QT4-$-1
		ld	a, 27h	;'
		jr	QT1
QT4:		ret
;
PRTNUM:		ld	b, 0
		call	CHKSGN
		jp	p, PN1
		ld	b, 2Dh ; '-'
		dec	c
PN1:		push	de
		ld	de, 10
		push	de
		dec	c
		push	bc
PN2:		call	DIVIDE
		ld	a, b
		or	c
		jr	z, PN3
		ex	(sp), hl
		dec	l
		push	hl
		ld	h, b
		ld	l, c
		jr	PN2
PN3:		pop	bc
PN4:		dec	c
		ld	a, c
		or	a
		jp	m, PN5
		ld	a, ' '
		call	OUTC
		jr	PN4
PN5:		ld	a, b
		or	a
		call	nz, OUTC
		ld	e, l
PN6:		ld	a, e
		cp	10
		pop	de
		ret	z
		add	a, '0'
		call	OUTC
		jr	PN6
;
PRTLN:		ld	a, (de)
		ld	l, a
		inc	de
		ld	a, (de)
		ld	h, a
		inc	de
		ld	c, 4
		call	PRTNUM
		ld	a, ' '
		call	OUTC
		sub	a
		jp	PRTSTG
;
;******************************************
;  MVUP	 MVDOWN	  UPR
;******************************************
;
MVUP:		call	COMP
		ret	z
		ld	a, (de)
		ld	(bc), a
		inc	de
		inc	bc
		jr	MVUP
;
MVDOWN:		ld	a, b
		sub	d
		jr	nz, MD1
		ld	a, c
		sub	e
		ret	z
MD1:		dec	de
		dec	hl
		ld	a, (de)
		ld	(hl), a
		jr	MVDOWN
;
;****************************
;*  OUTC  CHKIO  UPR        *
;****************************
;
CRLF:		ld	a, 0Dh
;
OUTC:		jp	CO
;
CHKIO:		call	CI
		and	7Fh
		cp	3
		ret	nz
		jr	RSTART
;
CXBUFE:		push	hl
		ld	hl, (BUFEND)
		cp	l
		pop	hl
		ret
;
CXBUFA:		push	hl
		ld	hl, (BUFFER)
		cp	l
		pop	hl
		ret
;
CONT:		call	CSTS
		ret	z
		call	CI
		cp	3
		ret	nz
		jr	RSTART
;
;******************************************************
;
;******************************************************
;
START:		ld	sp, STACK
		ld	a, 0Ch
		call	OUTC
		sub	a
		ld	de, about
		call	PRTSTG
		ld	hl, START
		ld	(RANPNT), hl
		ld	hl, TXTBGN
		ld	(TXTUNF), hl
		ld	hl, TXTE
		ld	(TXTEND), hl
		ld	hl, BUFA
		ld	(BUFFER), hl
		ld	hl, BUFE
		ld	(BUFEND), hl
;
;******************************************************
;    HAUPTPROGRAMM
; LEGT PROGRAMM IM SPEICHER AB
; DEFINIERT REGISTER
; LIEST EINE BENUTZER ZEILE UM DIESE ZU VERARBEITEN.
;******************************************************
;
RSTART:		ld	sp, STACK
		call	CRLF
		ld	de, OK
		sub	a
		call	PRTSTG
		ld	hl, ST2+1
		ld	(CURRNT), hl
ST2:		ld	hl, 0
		ld	(LOPVAR), hl
		ld	(STKGOS), hl
ST3:		ld	a, '>'
		call	GETLN
		push	de
		ld	de, (BUFFER)
		call	TSTNUM
		call	IGNB
		ld	a, h
		or	l
		pop	bc
		jp	z, DIRECT
		dec	de
		ld	a, h
		ld	(de), a
		dec	de
		ld	a, l
		ld	(de), a
		push	bc
		push	de
		ld	a, c
		sub	e
		push	af
		call	FNDLN
		push	de
		jr	nz, ST4
		push	de
		call	FNDNXT
		pop	bc
		ld	hl, (TXTUNF)
		call	MVUP
		ld	(TXTUNF), bc
ST4:		pop	bc
		ld	hl, (TXTUNF)
		pop	af
		push	hl
		cp	3
		jr	z, RSTART
		call	tv2
		ld	de, (TXTEND)
		call	COMP
		jp	nc, QSORRY
		ld	(TXTUNF), hl
		pop	de
		call	MVDOWN
		pop	de
		pop	hl
		call	MVUP
		jr	ST3
;
;**********************************************************
;	GOSUB	   UPR
;**********************************************************
;
GOSUB:		call	EXPR
		push	de
		call	FNDLN
		jp	nz, AHOW
		ld	hl, (CURRNT)
		push	hl
		ld	hl, (STKGOS)
		push	hl
		ld	hl, 0
		add	hl, sp
		ld	(STKGOS), hl
		jr	RUNTSL
;
;**********************************************
;     LIST   UPR
;**********************************************
;
LIST:		call	TSTNUM
		call	ENDCHK
		call	FNDLN
LS1:		jp	c, RSTART
		call	PRTLN
		call	CONT
		call	MS30
		call	MS30
		call	FNDLP
		jr	LS1
;
;*************************************************
;  RUN   GOTO     UPR
;*************************************************
;
GOTO:		call	EXPR
		push	de
		call	ENDCHK
		call	FNDLN
		jp	nz, AHOW
		pop	af
		jr	RUNTSL
;
RUN:		call	ENDCHK
		ld	de, TXTBGN
RUNNXL:		ld	hl, 0
		call	FNDLP
		jp	c, RSTART
RUNTSL:		ld	(CURRNT), de
		inc	de
		inc	de
RUNSML:		call	CONT
		ld	hl, TAB2-1
		jp	EXEC
;
;**********************************************
;     PRINT	   UPR
;**********************************************
;
PRINT:		ld	c, 6
		call	TSTC
		db ';'
		db    5
		call	CRLF
		jr	RUNSML
		call	TSTC
		db 0Dh
		db    5
		call	CRLF
		jr	RUNNXL
PR0:		call	TSTC
		db '#'
		db  0Eh
		call	EXPR
		ld	c, l
PR3:		call	TSTC
		db ','
		db  17h
		call	FIN
		jr	PR0
		call	QTSTG
		jp	PR8
		jr	PR3
PR8:		call	EXPR
		push	bc
		call	PRTNUM
		pop	bc
		jr	PR3
		call	CRLF
		jp	LT1
RET:		ld	hl, 0
		jr	IFFR
;
;*****************************************************
;  IF	 INPUT	  LET		UPR
;****************************************************
;
IFF:		call	EXPR
IFFR:		ld	a, h
		or	l
		jr	nz, RUNSML
		call	FNDSKP
		jr	nc, RUNTSL
		jp	RSTART
INPERR:		ld	hl, (STKINP)
		ld	sp, hl
		pop	hl
		ld	(CURRNT), hl
		pop	de
		pop	de
INPUT:		push	de
		call	QTSTG
		jp	IP2
		call	TSTV
		jr	c, IP4
		jr	IP3
IP2:		push	de
		call	TSTV
		jp	c, QWHAT
		ld	a, (de)
		ld	c, a
		sub	a
		ld	(de), a
		pop	de
		call	PRTSTG
		ld	a, c
		dec	de
		ld	(de), a
IP3:		push	de
		ex	de, hl
		ld	hl, (CURRNT)
		push	hl
		ld	hl, INPUT
		ld	(CURRNT), hl
		ld	hl, 0
		add	hl, sp
		ld	(STKINP), hl
		push	de
		ld	a, ':'
		call	GETLN
		ld	de, (BUFFER)
		call	EXPR
		call	CONT
		pop	de
		ex	de, hl
		ld	(hl), e
		inc	hl
		ld	(hl), d
		pop	hl
		ld	(CURRNT), hl
		pop	de
IP4:		pop	af
		call	TSTC
		db ','
		db  LT1-$-1
		jr	INPUT
;
DEFLT:		ld	a, (de)
		cp	0Dh
		jr	z, LT1
;
LET:		call	SETVAL
		call	TSTC
		db ','
		db    LT1-$-1
		jr	LET
LT1:		call	FINI
;
SIZE:		push	de
		ld	de, (TXTUNF)
		ld	hl, (TXTEND)
		and	a
		sbc	hl, de
		pop	de
		ret
;
;**********************************************************
;       RETURN             UPR
;**********************************************************
;
RETURN:		call	ENDCHK
		ld	hl, (STKGOS)
		ld	a, h
		or	l
		jp	z, QWHAT
		ld	sp, hl
		pop	hl
		ld	(STKGOS), hl
		pop	hl
		ld	(CURRNT), hl
		pop	de
jFINI:		jr	LT1
;
TAB:		call	PARN
A1:		ld	a, h
		or	l
		jr	z, jFINI
		dec	hl
		ld	a, ' '
		call	OUTC
		jr	A1
;
ABS:		call	PARN
		dec	de
		call	CHKSGN
		inc	de
		ret
;
PEEK:		call	PARN
		ld	l, (hl)
		ld	h, 0
		ret
FR1:		ld	hl, (TXTUNF)
		inc	hl
		ret
;
POKE:		call	EXPR
		push	hl
		call	TSTC
		inc	l
		ex	af, af'	;'
		call	EXPR
		ld	a, l
		pop	hl
		ld	(hl), a
		jr	jFINI
		jp	QWHAT
;
BYTE:		call	PARN
		jr	w1
;
WORD:		call	PARN
		ld	a, h
		call	WRIT2
w1:		ld	a, l
		call	WRIT2
		jr	jFINI
;
OUTCHAR:	call	EXPR
		ld	a, l
		call	OUTC
o1:		jr	jFINI
;
WRIT2:		push	af
		rrc	a
		rrc	a
		rrc	a
		rrc	a
		call	IST
		pop	af
IST:		and	0Fh
		add	a, 90h
		daa
		adc	a, 40h
		daa
		jp	OUTC
;
INCHAR:		call	CHKIO
		ld	h, 0
		ld	l, a
		ret
;
RND:		call	PARN
		ld	a, h
		or	a
		jp	m, QHOW
		or	l
		jp	z, QHOW
		push	de
		push	bc
		ld	a, r
		ld	d, e
		ld	e, a
		ex	de, hl
		call	DIVIDE
		pop	bc
		pop	de
		inc	hl
		ret
;
OUT:		call	PARN
		push	hl
		call	TSTC
		db '='
		db  jQWHAT-$-1
		call	EXPR
		ld	b, l
		pop	hl
		ld	h, 0C9h	 ; out (xx),a; ret
		ld	(IOBUFB), hl
		ld	a, 0D3h
		ld	(IOBUFA), a
		ld	a, b
		call	IOBUFA
		jr	o1
QUOTE:		ld	a, (de)
		inc	de
		ld	l, a
		ld	h, 0
		call	TSTC
		db 27h
		db  jQWHAT-$-1
		ret
;
HEX:		push	bc
		ld	hl, 0
		call	TSTC
		db '('
		db  jQWHAT-$-1
HNXTH:		ld	a, (de)
		cp	0Dh
		jr	z, jQWHAT
		call	CNVBN
		add	hl, hl
		add	hl, hl
		add	hl, hl
		add	hl, hl
		ld	b, 0
		ld	c, a
		add	hl, bc
		inc	de
		call	TSTC
		db ')'
		db    HN2-$-1
HN1:		jr	POPRET
HN2:		jr	HNXTH
;
jQWHAT:		jp	QWHAT
POPRET:		pop	bc
		ret
CNVBN:		cp	'0'
		jp	m, QWHAT
		cp	'9'
		jp	m, CONTC
		jr	z, CONTC
		cp	'A'
		jp	m, QWHAT
		cp	'G'
		jp	p, QWHAT
CONTC:	sub	'0'
		cp	0Ah
		ret	m
		sub	7
		ret
;
INA:		call	PARN
		ld	h, 0C9h
		ld	(IOBUFB), hl	; in a,(xx); ret
		ld	a, 0DBh
		ld	(IOBUFA), a
		call	IOBUFA
		ld	h, 0
		ld	l, a
		ret
;
END:		call	EXPR
		ex	de, hl
		ld	hl, TXTE
		ex	de, hl
		call	COMP
		jp	c, ASORRY
		ld	a, h
		or	a
		jp	m, ASORRY
		ld	a, (hl)
		cpl
		ld	(hl), a
		cp	(hl)
		jp	nz, ASORRY
		ld	(BUFEND), hl
		ld	bc, 70		; max.Zeilenlaenge?
		sbc	hl, bc
		ld	(BUFFER), hl
		dec	hl
		dec	hl
		ld	(TXTEND), hl
		jp	RSTART
;
BYE:		jp	GETCO1
;
CALL:		call	EXPR
		push	de
		ld	bc, HERE
		push	bc
		jp	(hl)
;
HERE:		pop	de
		call	FINI
;
NEW:		call	ENDCHK
		ld	hl, TXTBGN
		ld	(TXTUNF), hl
STOP:		call	ENDCHK
		jp	RSTART
;
;********************************
;* TABLES DIRECT EXEC           *
;********************************
;
TAB1:		db "RU",0CEh
		dw RUN
		db "LIS",0D4h
		dw LIST
		db "EN",0C4h
		dw END
		db "BY",0C5h
		dw BYE
		db "NE",0D7h
		dw NEW
;
TAB2:		db 'I',0C6h
		dw IFF
		db "GOT",0CFh
		dw GOTO
		db "INPU",0D4h
		dw INPUT
		db "LE",0D4h
		dw LET
		db "PRIN",0D4h
		dw PRINT
		db "GOSU",0C2h
		dw GOSUB
		db "RETUR",0CEh
		dw RETURN
		db "POK",0C5h
		dw POKE
		db "TA",0C2h
		dw TAB
		db "OUTCHA",0D2h
		dw OUTCHAR
		db "OU",0D4h
		dw OUT
		db "BYT",0C5h
		dw BYTE
		db "WOR",0C4h
		dw WORD
		db "CAL",0CCh
		dw CALL
		db "RE",0CDh
		dw RET
		db "STO",0D0h
		dw STOP
		db  80h
		dw DEFLT
;
TAB4:		db "SIZ",0C5h
		dw SIZE
		db "AB",0D3h
		dw ABS
		db "PEE",0CBh
		dw PEEK
		db "TO",0D0h
		dw FR1
		db "RN",0C4h
		dw RND
		db "INCHA",0D2h
		dw INCHAR
		db "HE",0D8h
		dw HEX
		db 0A7h	; '
		dw QUOTE
		db 'I',0CEh
		dw INA
		db  80h
		dw XP40
;
TAB8:		db  3Eh,0BDh	; >=
		dw XP11
		db 0A3h	; #
		dw XP12
		db 0BEh	; >
		dw XP13
		db 0BDh	; =
		dw XP15
		db  3Ch,0BDh	; <=
		dw XP14
		db 0BCh	; <
		dw XP16
		db  80h
		dw XP17
;
HOW:		db "HOW?",0Dh
OK:		db "READY",0Dh
WHAT:		db "WHAT?",0Dh
SORRY:		db "SORRY",0Dh
about:		db "              MINI - BASIC  AC 1   V 2.1",0Dh
; end of "ROM"

; Anpassung laut SCCH
inchc:          rst     8		; Tastatur abfragen
                push    af
inchc1:         call    TASTE
                jr      nz, inchc1	; warte auf Taste wieder losgelassen
                pop     af
                ret


; segment "RAM"
		org RAM

		DS	204
STACK:		DS	2
;
		DS 	2	;LEGT
IOBUFA:		DS 	1
IOBUFB:		DS 	2	;IOBUFB+IOBUFC
		DS 	1	;LSTROM
		DS 	1	;OCSW
CURRNT:		DS 	2
STKGOS:		DS 	2
		DS 	2	;VARNXT
STKINP:		DS 	2
LOPVAR:		DS 	2
		DS	2	;LOPINC
		DS	2       ;LOPLMT
		DS	2       ;LOPLN
		DS	2       ;LOPPT
RANPNT:		DS 	2
TXTUNF:		DS 	2
		DS 	40
VARBGN:		DS	55
TXTEND:		DS	2
BUFFER:		DS	2
BUFEND:		DS	2
TXTBGN:		DS	2
		DS 	600
TXTE:		DS	2
BUFA:		DS	64
BUFE:		DS	1
; end of "RAM"

		end
