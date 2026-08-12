; File Name   :	d:\hobby3\ac1-2010\edas\zeap20\ZEAP20.bin
; Format      :	Binary file
; Base Address:	0000h Range: 1000h - 2008h Loaded length: 1008h
; Segment type:	Pure code
; segment "ROM"

;orig. Autoren "Paul Chapman" <cha...@corams.freeserve.co.uk> und Geoffrey Roughton 
;ZEAP, einen Editor-Assembler für den (4 KB!) NASCOM 1
;(hier Version 2.0)

		cpu z80

;orig org 1000, edas*4 hat org 4000
;label an 4000 angepasst zum besseren Vergleich mit edas*4
;das muss am Ende wieder zurückgeändert werden

; NAS-SYS RST's
;RIN:	     equ	0008h	; Obtain an input character in the A register
;SCAL:	     equ 	0018h	; call the NAS-SYS routines
;PRS:	     equ	0028h	; Output the string
RDEL:	     equ	0038h	; Wait for a period of time dependent on the
				; value	in the A register.  A is set to	0.

SCAL	macro	nr		; NASSYS-Call
	rst	18h
	db	nr
	endm

; 0800	     The video RAM
; 0800	     There is a	10 byte	margin at the left of each line,
; 0800	     then a 48 byte line, then a 6 byte	margin on the right
; 0800
; 0800	     LINE NO LEFT MARGIN
; 0800	     16	     0BC0
; 0800	     1	     0800
; 0800	     2	     0840
; 0800	     3	     0880
; 0800	     4	     08C0
; 0800	     5	     0900
; 0800	     6	     0940
; 0800	     7	     0980
; 0800	     8	     09C0
; 0800	     9	     0A00
; 0800	     10	     0A40
; 0800	     11	     0A80
; 0800	     12	     0AC0
; 0800	     13	     0B00
; 0800	     14	     0B40
; 0800	     15	     0B80

; NAS-SYS	WORKSPACE
PORT0:	     	equ	0C00h		; Copy of current state of output part 0.
ARGC:	     	equ	0C0Ah		; Command letter or routine number	last processed for command execution or	input/output.
ARGH:	     	equ	0C0Bh		; Number of values in input line
ARG1:	     	equ	0C0Ch		; Frst value entered.
ARG2:	     	equ	0C0Eh		; Second value entered.
ARG3:	     	equ	0C10h		; Third value entered.
NUMV:	     	equ	0C21h		; Value returned by routine NUM.
CURSOR:	     	equ	0C29h		; Position of the cursor.
ARGX:	     	equ	0C2Bh		; Last command letter entered.
SUOUT:	     	equ	0C78h		; user specified output routine

;RAM
BUFBEG:	     	equ	0F00h	     	; edit buffer
BUFEND:	     	equ	0F02h	     	; Buffer Ende
outch:	     	equ	0F04h	     	; external output routine
byte_5507:	equ	0F07h
word_5508:	equ	0F08h
mc_ofs:	     	equ	0F0Ah	     	; offset for code generation
word_550C:	equ	0F0Ch
word_550E:	equ	0F0Eh
word_5510:	equ	0F10h
word_5512:	equ	0F12h
byte_5514:	equ	0F14h
word_5515:	equ	0F15h
word_5517:	equ	0F17h
word_5519:	equ	0F19h
word_551B:	equ	0F1Bh
unk_551D:	equ	0F1Dh
unk_552D:	equ	0F2Dh
unk_552E:	equ	0F2Eh
unk_552F:	equ	0F2Fh
unk_556F:	equ	0F6Fh
word_5575:	equ	0F75h
byte_5577:	equ	0F77h
word_5578:	equ	0F78h
unk_557A:	equ	0F7Ah
word_557C:	equ	0F7Ch
word_557E:	equ	0F7Eh
word_5580:	equ	0F80h
word_5582:	equ	0F82h
byte_5584:	equ	0F84h
word_5585:	equ	0F85h
word_5587:	equ	0F87h
word_5589:	equ	0F89h
word_55F2:	equ	0FF2h
unk_55F6:	equ	0FF6h
byte_55FE:	equ	0FFEh



		org 1000h
;		org 0D000h

		jp	cold		; cold start
		jp	warm		; warm start
;1006..1035 wird nach bufp kopiert
; phase bufp
; RAM:0F00 ?? ??	     BUFP:
; RAM:0F00
; RAM:0F02 ??	     unk_5502:
; RAM:0F03 ??
; RAM:0F04 ??	     outch:
; RAM:0F05 ??
; RAM:0F06 ??
; RAM:0F07 ??	     byte_5507:
; RAM:0F08 ?? ??	     word_5508:
; RAM:0F0A ?? ??	     mc_ofs:
; RAM:0F0C ?? ??	     word_550C:	     14EAr
; RAM:0F0E ?? ??	     word_550E:
; RAM:0F10 ?? ??	     word_5510:
; RAM:0F12 ?? ??	     word_5512:
; RAM:0F14 ??	     byte_5514:	     w
; RAM:0F15 ??	     unk_5515:
; RAM:0F17 ?? ??	     word_5517:
; RAM:0F19 ?? ??	     word_5519:
; RAM:0F1B ?? ??	     word_551B:
; RAM:0F2D ??	     unk_552D:
; RAM:0F2E ??	     unk_552E:
; RAM:0F2F ??	     unk_552F:

word_4006:	dw 2000h
		dw 5000h
		jp	nas_outch
		db    0
		db    0
		db    0
		db    0
		db    0
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		dw 0			; word_5510
		dw 0			; word_5512
		db 0			; byte_5514
		dw kdotab		; unk_5515
		dw unk_40AA             ; word_5517	Parse Tree Mnemonics
		dw unk_428F             ; word_5519	Parse Tree Register
		dw unk_42F4             ; word_551B	Tokenklassen Register,Flags
		db  91h		        ; unk_551D	Zeichenklassen Spezialzeichen
		db  ' '	        	; 551E
		db  ','	        	; 551F
		db  98h		        ; 5520
		db  ';'	        	; 5521		Kommentar
		db    0 	        ; 5522		Zeilenende
		db  82h		        ; 5523
		db  '+'	        	; 5524
		db  ')'	        	; 5525
		db  '\''          	; 5526
		db 0C0h		        ; 5527
		db  '$'	        	; 5528
		db 0A0h		        ; 5529
		db  '#'	        	; 552A
		db  '"'	        	; 552B
		db  80h		        ; 552C		sonstiges
		db    0 	        ; unk_552D	das aktuelles Zeichen
		;
		db 0A0h		        ; 552E
		db 0A0h		        ; 552F
; Ende Defaultwerte

kdotab:		db  'V'
		dw VIEW		; VDU List
		db  'U'
		dw ULIST
		db  'N'
		dw NASSYS
		db  'R'
		dw RENUM
		db  'O'
		dw OPTIONS
		db  'A'
		dw ASSEMBLE
		db  'F'
		dw FIND
		db  'X'
		dw KILL
		db  'Z'
		dw CORRECT
		db  'I'
		dw INUM
		db  'P'
		dw PUT
		db  ':'
		dw kdo_dpoint
		db  'H'
		dw HEIGHT
		db  '+'
		dw SETOPT
		db  '-'
		dw RESOPT
		db  'W'
		dw WERRORS
		db  'J'
		dw DELAY
		db  'K'
		dw PDELAY
		db  'G'
		dw GO
		db  'Y'
		dw REUSE
		db  '/'
		dw SEARCH
		db    0

aHeadline:	db "ZEAP 2.0/A498  Free=      End=      Ent"
aZzzzzz:	db "=ZZZZZZ"

unk_40A4:	db  5Ah		; Z
		db  20h		;
		db  67h		; g
		db  5Bh		; [

; Bereich unk_40A8..unk_42F4 entspricht komplett EDAS*4
;---------------------------------------

; new output table
unk_40A8:	db  75h		; UOUT user specified output routine
		db    0		; list end

; Parsebaum Mnemonics
; 1. Byte letze drei Bit Scanposition
; 	b7 special code
;	b4 prefix byte ed
;	b3 prefix byte cb
;	b2..b0 scan position
; zu scannende Token (Zeichen und Zeichenklassen)
; bis zu gesetztem Bit 8
; dann folgt 1 Byte Code
; Ende Baum mit 0

unk_40AA:	db  80h
		db    0
		; L
		db    1			; LD A,r
		db  4Ch	; L
		db  44h	; D
		db  60h	; reg A
		db  83h	; r
		db  78h	; code ld a,b
		;
		db    4	 		; LD A,n
		db 0F4h	; n
		db  3Eh	; code ld a,n
		;
		db    4			; LD A,(BC)
		db 0B1h ; (BC)
		db  0Ah	; code ld a,(bc)
		;
		db    4			; LD A,(DE)
		db 0E5h	; (DE)
		db  1Ah	; code ld a,(de)
		;
		db    4			; LD A,(NN)
		db 0F3h	; (nn)
		db  3Ah	; ld a,(nn)
		;
		db  14h ; ed,4 		; LD A,I
		db 0ECh ; reg I
		db  57h ; code ld a,i ed 57
		;
		db  14h ; ed,4 		; LD A,R
		db 0EEh	; reg R
		db  5Fh ; code ld a,r ed 5f
		;
		db    3 		; LD r,r'
		db    0	; r
		db  83h	; r'
		db  40h	; code LD B,B
		;
		db    4			; LD r,n
		db 0F4h ; n
		db    6	; code ld b,n
		;
		db    3			; LD HL,addr
		db  62h	; HL
		db 0F2h	; addr
		db  21h	; code ld hl,nn
		;
		db    4			; LD HL,(addr)
		db 0F3h	; (addr)
		db  2Ah	; code ld hl,(nn)
		;
		db    3			; LD SP,addr
		db  6Ah	; SP
		db 0F2h	; addr
		db  31h	; code ld sp,nn
		;
		db    4			; LD SP,HL
		db 0E2h	; HL
		db 0F9h	; code ld sp,hl
		;
		db  14h	; ed,4		; LD SP,(addr)
		db 0F3h	; (addr)
		db  7Bh	; code ed 7b
		;
		db    3			; LD rr,addr
		db  0Ch	; rr
		db 0F2h	; addr
		db    1	; code ld bc,nn
		;
		db  14h	; ed,4		; LD rr,(addr)
		db 0F3h	; (addr)
		db  4Bh	; code ed 4b ld bc,(nn)
		;
		db    3			; LD (addr),hl
		db  73h	; (addr)
		db 0E2h	; HL
		db  22h	; code ld (nn),hl
		;
		db  14h	; ed,4		; LD (addr),rr
		db  8Ch	; rr
		db  43h	; code ld (nn),bc ed 43
		;
		db    4			; LD (addr),A
		db 0E0h	; A
		db  32h	; code ld (nn),a
		;
		db    3			; LD (BC),A
		db  31h	; (bc)
		db 0E0h	; A
		db    2	; code ld (bc),a
		;
		db    3			; LD (DE),a
		db  65h	; (de)
		db 0E0h	; A
		db  12h	; code LD (DE),A
		;
		db  13h	; ed,3		; LDI
		db 0C9h	; I
		db 0A0h	; code ed a0
		;
		db  14h	; ed,4		; LDIR
		db 0D2h	; R
		db 0B0h	; code ed b0
		;
		db  13h	; ed,3		; LDD
		db 0C4h	; D
		db 0A8h; code ed a8
		;
		db  14h	; ed,4		; LDDR
		db 0D2h	; R
		db 0B8h; code ed b8
		;
		db  13h	; ed,3		; LD I,A
		db  6Ch	; reg I
		db 0E0h	; reg A
		db  47h	; code ed 47
		;
		db  13h	; ed,3		; LD R,A
		db  6Eh	; reg R
		db 0E0h	; reg A
		db  4Fh	; code ed 4f
		; J
		db    1			; JR ofs
		db  4Ah	; J
		db  52h	; R
		db 0F8h	; ofs
		db  18h	; code jr
		;
		db    3			; JR flg,ofs
		db    9	; flag
		db 0F8h	; ofs
		db  20h	; code jr nz
		;
		db    2			; JP addr
		db  50h	; P
		db 0F2h ; addr
		db 0C3h	; code jp
		;
		db    3			; JP flg,addr
		db    6	; flag
		db 0F2h	; addr
		db 0C2h	; code jp nz
		;
		db  43h	; ??,3		; JP (HL)
		db 0E3h	; (HL)
		db 0E9h	; code jp (hl)
		;P
		db    1			; PUSH rr
		db  50h	; P
		db  55h	; U
		db  53h	; S
		db  48h	; H
		db  8Fh	; rr
		db 0C5h	; code push bc
		;
		db    2			; POP rr
		db  4Fh	; O
		db  50h	; P
		db  8Fh ; rr
		db 0C1h	; code pop bc
		;C
		db    1			; CALL nn
		db  43h	; C
		db  41h	; A
		db  4Ch	; L
		db  4Ch	; L
		db 0F2h	; nn
		db 0CDh	; code call
		;
		db    5			; CALL flg,addr
		db    6	; flag
		db 0F2h	; addr
		db 0C4h	; code call nz
		;
		db    2			; CP r
		db  50h	; P
		db  83h ; r
		db 0B8h	; code cp b
		;
		db    3			; CP n
		db 0F4h	; n
		db 0FEh	; code cp n
		;
		db    3			; CPL
		db 0CCh	; L
		db  2Fh	; code cpl
		;
		db  13h	; ed,3		; CPI
		db 0C9h	; I + 80h
		db 0A1h	; code ed a1
		;
		db  14h	; ed,4		; CPIR
		db 0D2h	; R + 80h
		db 0B1h	; code ed b1
		;
		db  13h	; ed,3		; CPD
		db 0C4h	; D + 80h
		db 0A9h	; code ed a9
		;
		db  14h	; ed,4		; CPDR
		db 0D2h	; R + 80h
		db 0B9h	; code ed b9
		;
		db    2			; CCF
		db  43h	; C
		db 0C6h	; F + 80h
		db  3Fh	; code ccf
		; E
		db  81h	; special,1	; EQU
		db  45h	; E
		db  51h	; Q
		db 0D5h	; U + 80h
		db    0	; special code 0
		;
		db    2			; EX AF,AF
		db  58h	; X
		db  68h	; AF
		db 0E8h	; AF
		db  08h	; code ex af,af'
		;
		db    3			; EX DE,HL
		db  64h	; DE
		db 0E2h	; HL
		db 0EBh	; code ex de,hl
		db    3			; EX (SP),HL
		db  6Bh	; (SP)
		db 0E2h	; HL
		db 0E3h	; code ex (sp),hl
		db    3			; EXX
		db 0D8h	; X
		db 0D9h ; code exx
		db    2			; EI
		db 0C9h	; I
		db 0FBh	; code ei
		;
		db  82h	; special,2	; ENT
		db  4Eh	; N
		db 0D4h	; T + 80H
		db  80h	; special code 80
		; I
		db    1			; INC r
		db  49h	; I
		db  4Eh	; N
		db  43h	; C
		db  80h	; r
		db    4	; code inc b
		;
		db    4	 		; INC rr
		db  8Ch ; rr
		db    3	; code inc bc
		;
		db    3			; IN A,(n)
		db  60h	; reg A
		db 0F5h ; (n)
		db 0DBh	; code in a,(n) DB 56
		;
		db  14h	; ed,4		; IN A,(C)
		db 0E7h ; (c)
		db  78h	; code ed 78
		;
		db  13h	; ed,3		; IN r,(C)
		db    0	; r
		db 0E7h	; (c)
		db  40h	; code in b,(c)
		;
		db  13h	; ed,3		; INI
		db 0C9h	; I + 80H
		db 0A2h	; code ed a2
		;
		db  14h	; ed,4		; INIR
		db 0D2h	; R + 80H
		db 0B2h	; code ed b2
		;
		db  13h	; ed,3		; IND
		db 0C4h	; D + 80H
		db 0AAh	; code ed aa
		;
		db  14h	; ed,4		; INDR
		db 0D2h	; R + 80H
		db 0BAh	; code ed ba
		;
		db  12h	; ed,2		; IM n
		db  4Dh	; M
		db 0FEh	; 0..2 (IM-Mode)
		db  46h	; code im 0
		; D
		db  81h	; special,1	; DEFB
		db  44h	; D
		db  45h	; E
		db  46h	; F
		db 0C2h	; B
		db    4	; special code 4
		;
		db  84h	; special,4	; DEFW
		db 0D7h	; W + 80H
		db    5	; special code 5
		;
		db  84h	; special,4	; DEFM
		db 0CDh	; M + 80H
		db    3	; special code 3
		;
		db  84h	; special,4	; DEFS
		db 0D3h	; S + 80H
		db    2	; special code 2
		;
		db    3			; DEC r
		db  43h	; C
		db  80h ; r
		db  05h	; code dec B
		;
		db    4			; DEC rr
		db  8Ch ; rr
		db  0Bh	; code dec BC
		;
		db    2			; DJNZ ofs
		db  4Ah	; J
		db  4Eh	; N
		db  5Ah	; Z
		db 0F8h	; ofs
		db  10h	; code 10 xx
		;
		db    2			; DAA
		db  41h	; A
		db 0C1h	; A
		db  27h	; code daa
		;
		db    2			; DI
		db 0C9h	; I + 80H
		db 0F3h	; code di
		; S
		db  11h	; ed,1		; SBC HL,rr
		db  53h	; S
		db  42h	; B
		db  43h	; C
		db  62h	; HL
		db  8Ch	; rr
		db  42h	; code sbc hl,bc ed 42
		;
		db    4			; SBC r
		db  60h
		db  83h ; r
		db  98h	; code sbc b
		;
		db    5			; SBC A,n
		db 0F4h	; n
		db 0DEh ; code DE 56 sbc a,n
		;
		db    2			; SUB r
		db  55h	; U
		db  42h	; B
		db  83h	; r
		db  90h	; code sub b
		;
		db    4			; SUB n
		db 0F4h	; n
		db 0D6h	; code sub n
		;
		db  82h	; special,4	; SKIP
		db  4Bh	; K
		db  49h	; I
		db 0D0h	; P
		db  81h ; special code 81
		;
		db  0Ah	; cb,2		; SLA r
		db  4Ch	; L
		db  41h	; A
		db  83h ; r
		db  20h ; code sla b cb 20
		;
		db  0Ah	; cb,2		; SRA r
		db  52h	; R
		db  41h	; A
		db  83h	; r
		db  28h	; code sra b cb 28
		;
		db  0Bh	; cb,3		; SRL r
		db  4Ch	; L
		db  83h ; r
		db  38h	; code srl b
		;
		db  0Ah	; cb,2		; SET bit,r
		db  45h	; E
		db  54h	; T
		db  7Ah ; bit
		db  83h ; r
		db 0C0h	; code set 0,b
		;
		db    2			; SCF
		db  43h	; C
		db 0C6h	; F + 80H
		db  37h	; code 37
		;
		db    3			; SCAL n
		db  41h	; A
		db  4Ch	; L
		db 0F4h	; n
		db 0DFh	; code rst 18h n
		; R
		db    1			; RET
		db  52h	; R
		db  45h	; E
		db 0D4h	; T + 80h
		db 0C9h	; code ret
		;
		db    4			; RET flg
		db  86h	; flag
		db 0C0h	; code ret nc
		;
		db  14h	; ed,4		; RETI
		db 0C9h	; I + 80H
		db  4Dh	; code ed 4d
		;
		db  14h	; ed,4		; RETN
		db 0CEh	; N + 80H
		db  45h	; code ed 45
		;
		db  0Bh	; cb,3		; RES bit,r
		db  53h	; S
		db  7Ah	; bit
		db  83h	; r
		db  80h	; code res
		;
		db    2			; RCAL ofs
		db  43h	; C
		db  41h	; A
		db  4Ch	; L
		db 0F8h	; ofs
		db 0D7h	; code rst 10h nn
		;
		db  0Ah	; cb,1		; RLC r
		db  4Ch	; L
		db  43h	; C
		db  83h	; r
		db    0	; code rlc b
		;
		db    4			; RLCA
		db 0C1h	; A + 80H
		db    7	; code 07
		;
		db  0Bh	; cb,2		; RL r
		db  83h	; r
		db  10h	; code rl b
		;
		db    3			; RLA
		db 0C1h	; A + 80H
		db  17h	; code 17
		;
		db  13h	; ed,3		; RLD
		db 0C4h	; D + 80H
		db  6Fh	; code ed 6f
		;
		db  0Ah	; cb,1		; RRC r
		db  52h	; R
		db  43h	; C
		db  83h ; r
		db  08h	; code rrc b
		;
		db    4			; RRCA
		db 0C1h	; A + 80H
		db  0Fh	; code 0f
		;
		db  0Bh	; cb,2		; RR r
		db  83h	; r
		db  18h ; code rr b cb 18
		;
		db    3			; RRA
		db 0C1h	; A + 80H
		db  1Fh	; code 1f
		;
		db  13h	; ed,3		; RRD
		db 0C4h	; D + 80H
		db  67h	; code ed 67
		;
		db    2			; RST rst
		db  53h	; S
		db  54h	; T
		db 0FCh ; rst
		db 0C7h	; code rst 0
		; O
		db    1			; OR r
		db  4Fh	; O
		db  52h	; R
		db  83h	; r
		db 0B0h	; code or b
		;
		db    3			; OR n
		db 0F4h	; n
		db 0F6h	; code or n
		;
		db  83h	; special,3	; ORG
		db 0C7h	; G + 80H
		db    1 ; special code 1
		;
		db    2			; OUT (n),A
		db  55h	; U
		db  54h	; T
		db  75h	; (n)
		db 0E0h	; A
		db 0D3h	; code d3 56 out (n),A
		;
		db  14h	; ed,4		; OUT (C),r
		db  67h	; (C)
		db  80h	; r
		db  41h	; code out (c),b
		;
		db  14h	; ed,4		; OUTI
		db 0C9h	; I + 80H
		db 0A3h	; code ed a2
		;
		db  14h	; ed,4		; OUTD
		db 0C4h	; D + 80H
		db 0ABh	; code ed ab
		;
		db  12h	; ed,2		; OTIR
		db  54h	; T
		db  49h	; I
		db 0D2h	; R + 80H
		db 0B3h	; code ed b3
		;
		db  13h	; ed,3		; OTDR
		db  44h	; D
		db 0D2h	; R + 80H
		db 0BBh	; code ed bb
		; A
		db    1			; ADD A,r
		db  41h	; A
		db  44h	; D
		db  44h	; D
		db  60h	; reg A
		db  83h	; r
		db  80h	; code add a,b
		;
		db    5			; ADD A,n
		db 0F4h	; n
		db 0C6h ; code add a,n c6 n
		;
		db    4			; ADD HL,rr
		db  62h	; HL
		db  8Ch	; rr
		db    9	; code add hl,bc
		;
		db    3			; ADC A,r
		db  43h	; C
		db  60h	; reg A
		db  83h	; r
		db  88h	; code adc a,b
		;
		db    5			; ADC A,n
		db 0F4h	; n
		db 0CEh	; code add a,n ce 56
		;
		db  14h	; ed,4		; ADC HL,rr
		db  62h ; reg HL
		db  8Ch ; rr
		db  4Ah	; code adc hl,bc
		;
		db    2			; AND r
		db  4Eh	; N
		db  44h	; D
		db  83h	; r
		db 0A0h	; code and b
		;
		db    4			; AND n
		db 0F4h	; n
		db 0E6h	; code and n
		; X
		db    1			; XOR r
		db  58h	; X
		db  4Fh	; O
		db  52h	; R
		db  83h	; r
		db 0A8h	; code xor b
		;
		db    4			; XOR n
		db 0F4h	; n
		db 0EEh	; code xor n
		; B
		db    9			; BIT n,r
		db  42h	; B
		db  49h	; I
		db  54h	; T
		db  7Ah	; bit
		db  83h	; r
		db  40h	; code bit
		; N
		db  11h	; ed,1		; NEG
		db  4Eh	; N
		db  45h	; E
		db 0C7h	; G
		db  44h	; code ed 44
		;
		db    2			; NOP
		db  4Fh	; O
		db 0D0h	; P
		db    0	; code 00
		; H
		db    1			; HALT
		db  48h	; H
		db  41h	; A
		db  4Ch	; L
		db 0D4h	; T
		db  76h	; code 76
		;
		db    0			; ENDE

; Parsebaum Register
unk_428F:	db  80h
		db    0
		;
		db    1			; H
		db 0C8h	; H + 80h
		db  26h ; token
		;
		db    2			; HL
		db 0CCh	; L + 80h
		db  62h
		;
		db    1			; A
		db 0C1h	; A + 80h
		db  60h
		;
		db    2			; AF
		db 0C6h	; F + 80h
		db  68h
		;
		db    1			; D
		db 0C4h	; D + 80h
		db  22h
		;
		db    2			; DE
		db 0C5h	; E + 80h
		db  64h
		;
		db    1			; B
		db 0C2h	; B + 80h
		db  20h
		;
		db    2			; BC
		db 0C3h	; C + 80h
		db  30h
		;
		db    1			; Z
		db 0DAh	; Z + 80h
		db  2Ch
		;
		db    1			; C
		db 0C3h	; C + 80h
		db  66h
		;
		db    1			; NZ
		db  4Eh	; N
		db 0DAh	; Z + 80h
		db  2Ah
		;
		db    2			; NC
		db 0C3h	; C + 80h
		db  2Eh
		;
		db    1			; SP
		db  53h	; S
		db 0D0h	; P + 80h
		db  6Ah
		;
		db  81h	; special,1	; $
		db 0A4h	; '$' + 80h
		db    0
		;
		db    1			; E
		db 0C5h	; E + 80h
		db  24h
		;
		db    1			; L
		db 0CCh	; L + 80h
		db  28h
		;
		db    1			; I
		db 0C9h	; I + 80h
		db  6Ch
		;
		db  12h	; dd,2		; IX
		db 0D8h	; X + 80h
		db  62h
		;
		db  32h	; fd,2		; IY
		db 0D9h	; Y + 80h
		db  62h
		;
		db    1			; M
		db 0CDh	; M + 80h
		db  38h
		;
		db    1			; P
		db 0D0h	; P + 80h
		db  36h
		;
		db    2			; PE
		db 0C5h	; E + 80h
		db  34h
		;
		db    2			; PO
		db 0CFh	; O + 80h
		db  32h
		;
		db    1			; R
		db 0D2h	; R + 80h
		db  6Eh
		;
		db    0	; ENDE

; bit modifier == pos in list (down)
; r 8-Bit-Register
unk_42DC:	db  60h	; A	111
		db  63h	; (HL)	110
		db  28h	; L	101
		db  26h	; H	100
		db  24h	; E	011
		db  22h	; D	010
		db  66h	; C	001
		db  20h	; B	000

; Sprungbedingung 1
unk_42E4:	db  38h ; M	111
		db  36h ; P	110
		db  34h ; PE	101
		db  32h ; PO	100
; Sprungbedingung 2
unk_42E8:	db  66h	; C	011
		db  2Eh ; NC	010
		db  2Ch ; Z	001
		db  2Ah ; NZ	000

; dd 16-Bit-Register
unk_42EC:	db  6Ah	; SP	11
		db  62h	; HL	10
		db  64h	; DE	01
		db  30h	; BC	00

; qq  16-Bit-Register
unk_42F0:	db  68h	; AF	11
		db  62h	; HL	10
		db  64h	; DE	01
		db  30h	; BC	00

;---------------------------------------
;Tokenklassen Register,Flags
unk_42F4:	db  38h 	; Bitpos,Anzahl
		dw unk_42DC	; r 8-Bit-Register
		db  08h
		dw unk_42DC	; r' 8-Bit-Register
		db  38h	;
		dw unk_42E4	; Sprungbedingung 1+2
		db  34h	;
		dw unk_42E8	; Sprungbedingung 2
		db  44h	;
		dw unk_42EC	; dd 16-Bit-Register
		db  44h	;
		dw unk_42F0	; qq 16-Bit-Register

;---------------------------------------
; Abarbeitung nächstes Token
sub_4306:	exx
		pop	hl
		pop	de
		ex	(sp), hl	; Parse-Tree (z.B. 40AA)
		ld	b, a
		inc	d		; next pos (?)
		bit	7, (hl)		; Befehlsende?
		jr	nz, loc_4387	; ja
		; Tokenvergleich
loc_4310:	inc	hl		; nächstes Token aus Baum
		ld	a, (hl)		; holen
		and	7Fh 		; strip hi bit
		cp	70h 		; codeklasse?
		jr	c, loc_434A	; nein
		; codeklassen Token 72..7E
		ld	c, a		; Token merken
		xor	b		; 70 (?)
		rrca
		cp	8
		jr	nc, loc_4381
		cp	5
		jr	c, loc_4396	; <5, d.h. 72..78
		; 7A..7E  (5..7)
		ex	af, af'	;'
		ld	a, (iy-0Bh)	; 55F3
		or	a
		jr	nz, loc_4345
		ex	af, af'	;'
		cp	6		;
		ld	a, (iy-0Ch)	; 55F2
		jr	c, loc_433D	; wenn < 6 (7A bit)
		jr	z, loc_4340	; wenn = 6 (7C rst)
		cp	3
		dec	a
		jr	nc, loc_4345	;
		inc	a
		jr	z, loc_433D
		inc	a
loc_433D:	; bei bit-Befehlen pos auf bitpos 3 schieben
		rlca
		rlca
		rlca
loc_4340:	; bei rst wert direkt einfuegen
		ld	(iy-0Dh), a	; 55F1 byte modifier
		and	0C7h
loc_4345:	call	nz, sub_43B5
		jr	loc_4399	; -> Ende

		; einfaches Token oder register o. flag
loc_434A:	cp	20h 		; indiv Register o. Flag?
		jr	nc, loc_437E	; nein
		; register o. flag
		push	hl
		ld	hl, (word_551B)	; unk_42F4 TokenklassenTab
		add	a, l	; HL + A
		ld	l, a
		jr	nc, loc_4357
		inc	h
loc_4357:	ld	a, b
		ex	af, af'	;'
		ld	a, (hl)		; Bitpos+Anzahl
		and	0Fh		; nur Anzahl
		ld	c, a		; merken
		ld	a, (hl)
		ex	af, af'	;'
		inc	hl
		ld	b, (hl)
		inc	hl
		ld	h, (hl)
		ld	l, b		; HL=Zeiger auf Liste
		ld	b, 0		; C=Anzahl
		cpir			; suche A in Liste
		pop	hl
		ld	b, a
		jr	nz, loc_4381	; wenn Token nicht in Liste
		; in Liste gefunden
		ex	af, af'	;'
loc_436D:	sla	c		; c=Pos
		sub	10h		; verschieben auf Ziel Bitpos.
		jr	nc, loc_436D
		srl	c		; eins zurück; fertig
		ld	a, (iy-0Dh)	; byte modifier
		or	c		; modify
		ld	(iy-0Dh), a	; write back
		jr	loc_4399	; Ende

; einfaches Token
loc_437E:	cp	b		; nächstes Zeichen gleich?
		jr	z, loc_4399	; ja -> Ende
;
loc_4381:	bit	7, (hl)		; Befehlsende?
		inc	hl		; weiter im Baum
		jr	z, loc_4381	; nein, weiter suchen
		dec	hl		; hl=befehlsende
; Befehlsende erreicht
loc_4387:	inc	hl		; hl=codebyte
		inc	hl		; hl=nächster Eintrag im Baum
		ld	a, (hl)		; Anzahl (und Modifier)
		ld	e, a		; merken
		and	7		; gilt ab Pos (SollPos)
		cp	d		; vergleichen mit aktueller Pos
		jp	z, loc_4310	; Pos passt
		inc	hl		; weiter im Baum
		jr	nc, loc_4381	; wenn akt. Pos > SollPos
					; dann weitersuchen
		exx
		ret

; Tokenklasse 72..78 (1..4)
loc_4396:	ld	(iy-0Eh), a	; sichern
; Ende Abarbeitung nächstes Token
loc_4399:	ex	(sp), hl
		push	de
		push	hl
		exx
		or	a		; set flags
		ret

;---------------------------------------
;
sub_439F:	exx
		pop	hl
		pop	de
		ex	(sp), hl
		ld	a, (hl)
		cp	80h ; 'Ç'
		inc	hl
		ld	d, (hl)
		push	de
		exx
		pop	de
		ld	a, d
		ret

;---------------------------------------
;
sub_43AD:	ld	a, d
		or	7Fh
		and	e

sub_43B1:	rlca
		sbc	a, a
		sub	d
		ret	z
;
sub_43B5:	bit	1, (iy+0)
		ret	z
		set	6, (iy-0Ah)
		ret

;---------------------------------------
;
nas_outch:	SCAL 6Fh		; SRLX	Send the character in the A register directly to the serial output port
		ret

;---------------------------------------
;
sub_43C2:	ld	b, h
		ld	c, l
		ld	hl, (word_5585)
		or	a
		sbc	hl, de
		add	hl, de
		jr	c, loc_43D8
		sbc	hl, bc
		jr	nc, loc_43D4
		or	a
		sbc	hl, hl
loc_43D4:	add	hl, de
		ld	(word_5585), hl
loc_43D8:	ex	de, hl
		call	sub_4435	; Textende ermitteln
		ex	de, hl
		or	a
		sbc	hl, bc
		push	bc
		ex	(sp), hl
		pop	bc
		push	bc
		ldir
		call	sub_4442
		pop	bc
		ret

;---------------------------------------
; DE (BWS) dez ascii -> hex num HL
datoh:		ld	hl, 0
datoh1:		ld	a, (de)
		sub	30h ; '0'
		ret	c		; Ende,	wenn keine Dezimalziffer
		cp	0Ah ; .. '9'
		ret	nc		; Ende,	wenn keine Dezimalziffer
		push	de
		ld	d, h	; HL*10
		ld	e, l
		add	hl, hl		; *10
		add	hl, hl
		add	hl, de
		add	hl, hl
		ld	d, 0
		ld	e, a
		add	hl, de
		pop	de
		inc	de		; nächste Stelle (BWS)
		jr	datoh1

;---------------------------------------
; Parameter f. RENUM und INUM, Defaults 10
sub_4404:	ld	hl, (ARG1)	; erste Zeile
		ld	a, (ARGH)	; Number of values in input line
		cp	2
		ld	a, 10h 		; Defaultwert Schrittweite 10
		jr	nz, loc_4413
		ld	a, (ARG2)	; Second value entered.
loc_4413:	ld	(byte_5584), a	; Schrittweite
		or	a		; Schrittweite=0 ?
		ld	a, 6		; Error 06 - Inkrement ist 0
		jp	z, loc_4B19	; Fehler ausgeben
		ld	a, h
		or	l		; erste Zeile <> 0?
		ret	nz		; dann raus hier
; Nächste Zeilennummer berechnen (BCD)
sub_441F:	ld	a, (byte_5584)	; Schrittweite
		add	a, l		; + Zeilennummer
		daa
		ld	l, a
		ld	a, h
		adc	a, 0
		daa
		ld	h, a
		ret			; Cy=1 Nummer > 9999

;---------------------------------------
; Suche nächste Zeile
; suche ab HL+2 Zeilenendebyte 00. Folgt FF, dann Z=1
sub_442B:	inc	hl

; f. RENUM, Suche nächste Zeile
; suche ab HL+1 Zeilenendebyte 00. Folgt FF, dann Z=1
loc_442C:	inc	hl
;
loc_442D:	xor	a		; 00 Zeilenende
		ld	b, a
		ld	c, a
		cpir			; DE = Anf. neue Zeile
		dec	a		; FF Textende
		cp	(hl)
		ret

;---------------------------------------
; Textende ermitteln
; ret DE=Anfangsdresse+Textlänge
sub_4435:		push	hl
		ld	hl, (BUFBEG)	; Anfangsdresse
		ld	e, (hl)		; Textlänge
		inc	hl
		ld	d, (hl)
		dec	hl
		ex	de, hl
		add	hl, de
		ex	de, hl
		pop	hl
		ret

;---------------------------------------
;
sub_4442:	push	hl
		push	de
		ld	hl, (BUFBEG)	; Anfangsdresse
		ex	de, hl
		or	a
		sbc	hl, de
		ex	de, hl
		jr	loc_4460

;---------------------------------------
; freier Speicher am Ende
sub_444E:	push	hl
		ld	hl, (BUFBEG)	; Anfangsdresse
		inc	hl		; Textlänge übergehen
		inc	hl
		ld	e, (hl)		; freier Speicher am Ende
		inc	hl
		ld	d, (hl)
		pop	hl
		ret

;---------------------------------------
;
sub_4459:	push	hl
		push	de
		ld	hl, (BUFBEG)	; Anfangsdresse
		inc	hl
		inc	hl
loc_4460:	ld	(hl), e
		inc	hl
		ld	(hl), d
		pop	de
		pop	hl
		ret
;---------------------------------------
; Bufferende berechnen
sub_4466:	call	sub_4435	; Textende ermitteln
		ex	de, hl
		call	sub_444E	; freier Speicher am Ende
		add	hl, de
		ret

;---------------------------------------
sub_446F:	SCAL 5Fh		; MFLP	Alter the state of (turn an or off) the tape drive LED.
loc_4471:	ld	a, 0FFh
sub_4473:	or	a
		ret	z
		push	bc
		ld	b, a
loc_4477:	xor	a
		rst	RDEL		; RDEL	  Wait for a period of time dependent on the value in the A register.
		call	sub_4D08	; ggf. Abbruch bei Stop
		djnz	loc_4477
		pop	bc
		ret

;
sub_4480:	ld	a, 0Dh
		call	sub_4D19
		ld	a, 0Ah
		call	sub_4D19
		ld	a, (word_5508)	; Ausgabeverzögerung Druck
		jr	sub_4473

;---------------------------------------
;
sub_448F:	ld	a, 20h ; ' '
		ld	(hl), 0A0h ; 'á'
		dec	hl
		xor	(hl)
		and	7Fh ; ''
		jr	z, sub_448F
		push	hl
loc_449A:	xor	a
		dec	hl
		xor	(hl)
		jr	z, loc_44A8
		ld	a, 0A0h	; 'á'
		xor	(hl)
		jr	nz, loc_449A
		ld	(hl), 20h ; ' '
		jr	loc_449A
loc_44A8:	pop	hl
		ret

;---------------------------------------
; Ausgabe "ZEAP Z80 Assembler"
sub_44AA:	rst	28h		; Output the string
		db "ZEAP Z80 Assembler - ",0
locret_44C1:	ret

;---------------------------------------
;
sub_44C2:	push	bc
		dec	hl
		push	hl
		ld	de, (word_5519)	; unk_428F Parse Tree Register
		push	de
		ld	d, 0
		push	de
loc_44CD:	call	sub_464C	; nächstes Zeichen
		bit	6, c
		jr	z, loc_44E1
		call	sub_4306	; Abarbeitung nächstes Token
		jr	nc, loc_44CD
loc_44D9:	call	sub_4435	; Textende ermitteln
		ld	hl, (word_5589)
		jr	loc_44FD
;
loc_44E1:	call	sub_439F
		jr	c, loc_44D9
		bit	7, e
		jr	z, loc_4523
loc_44EA:	ld	de, (word_550C)	; MC-Adr.
		jr	loc_4523
loc_44F0:	call	sub_4555
		jr	c, loc_4502
		inc	de
		inc	de
		call	sub_4526
		jr	z, loc_4515
		pop	bc
loc_44FD:	pop	bc
		push	bc
		push	bc
		jr	loc_44F0
;
loc_4502:	pop	hl
loc_4503:	call	sub_464C
		bit	6, c
		jr	nz, loc_4503
loc_450A:	ld	de, 0
		set	3, (iy-0Ah)
		scf
		sbc	a, a
		jr	loc_4523

loc_4515:	push	ix
		ex	(sp), hl
		inc	hl
		or	a
		sbc	hl, de
		pop	hl
		ex	de, hl
		dec	hl
		ld	d, (hl)
		dec	hl
		ld	e, (hl)
		pop	hl
loc_4523:	pop	bc
		pop	bc
		ret

sub_4526:	ld	b, h
		ld	c, l
		pop	hl
		ex	(sp), hl
		push	bc
		ex	(sp), hl
loc_452C:	ex	(sp), hl
		call	sub_464C
		ex	(sp), hl
		bit	6, c
		jr	z, loc_4545
		inc	hl
		cp	(hl)
		jr	z, loc_452C
		push	af
		call	loc_464D
		pop	af
		bit	6, c
		jr	nz, loc_454E
		or	c
		jr	loc_454E
loc_4545:	call	sub_464C
		xor	a
		bit	6, c
		jr	z, loc_454E
		sub	c
loc_454E:	ex	(sp), hl
		pop	bc
		ex	(sp), hl
		push	hl
		ld	h, b
		ld	l, c
		ret
;
sub_4555:	call	loc_442D		; Suche nächste Zeile
		push	de
		call	sub_4638		; Suche nächste Zeile
		pop	de
		ret	c			; wenn Bereichsüberschreitung
		inc	hl
		call	sub_464C
		jr	nc, sub_4555
		dec	hl
		or	a
		ret

;---------------------------------------
; Ausgabe Symboltabelle
sub_4567:	call	sub_4C9C
		call	sub_44AA
		rst	28h		; Output the string
		db "Symbol Table",0
		call	sub_4C9C
		call	sub_4C9C
		ld	de, unk_40A4
loc_4584:	ld	hl, aZzzzzz	; "=ZZZZZZ"
		push	hl
		push	hl
		ld	hl, (word_5589)
loc_458C:	call	sub_4555
		jr	c, loc_45A9
		push	hl
		push	de
		call	sub_4526
		pop	bc
		pop	bc
		jr	nc, loc_458C
		pop	hl
		push	hl
		push	bc
		push	bc
		call	sub_4526
		pop	hl
		pop	bc
		jr	nc, loc_458C
		pop	af
		push	bc
		jr	loc_458C
loc_45A9:	pop	hl
		pop	bc
		or	a
		sbc	hl, bc
		add	hl, bc
		jp	z, sub_4C9C
		push	hl
		push	hl
		inc	hl
		call	sub_44C2
		pop	hl
		jr	z, loc_45DB
		push	hl
		ex	de, hl
		SCAL 66h		; TBCD3	Output the value in the HL register in ASCII, followed by a space.
		rst	28h		; Output the string
		db 8,"H ",0
loc_45C4:	pop	hl
		dec	hl
		call	sub_460C
loc_45C9:	ld	b, 7
		call	sub_4682
		ld	hl, -0BA2h	; -bws()
		add	hl, de
		ld	de, 0BA2h
		call	nc, setcu
		call	c, sub_4C9C
loc_45DB:	pop	de
		jr	loc_4584

;---------------------------------------
; suche Zeile ARG1
; ret HL = Zeilenanfang
sub_45DE:	ld	hl, (BUFBEG)	; edit buffer
		inc	hl
		inc	hl
loc_45E3:	call	sub_442B	; Suche nächste Zeile
		ret	z		; Textende erreicht
		ld	e, (hl)
		inc	hl
		ld	d, (hl)		; DE=Zeilennummer
		dec	hl
		push	hl		; Zeilenanfang
		ld	hl, unk_488F
		ld	a, (hl)
		inc	(hl)
		cp	(hl)
		nop
		dec	(hl)
		ld	hl, (ARG1)	; wird ARG1 überschritten?
		or	a
		sbc	hl, de
		pop	hl
		ccf
		ret	nc		; schon größer
		ret	z		; gefunden
		jr	loc_45E3

;---------------------------------------
;
sub_4600:	inc	hl
		or	a
		ld	a, 0A0h	; 'á'
		ld	(de), a
		ret	z
		dec	hl
		call	sub_4649
		jr	sub_4600

;---------------------------------------
; Ausgabe Zeilennummer
sub_460C:	call	sub_4638	; ret DE=Zeilennummer
		ret	c		; wenn Bereichsüberschreitung
		push	hl		; Zeilenanfang
		ex	de, hl		; HL=Zeilennummer
		SCAL 66h		; TBCD3	Output the value in the HL register in ASCII, followed by a space.
		ld	(word_5512), hl	; Zeilennummer merken
		pop	hl		; Zeilenanfang
		inc	hl
		call	sub_464C
		bit	7, a
; set cursor bws()
sub_461E:	ld	de, 0F8Bh
;---------------------------------------
; set cursor
; ret de=alte Cursorposition
setcu:		push	hl
		ld	hl, (CURSOR)	; Position of the cursor.
		ex	de, hl
		ld	(CURSOR), hl	; Position of the cursor.
		pop	hl
		ret

;---------------------------------------
;die letzten beiden BWS-Zeilen leeren
sub_462B:	ld	de, 0BFAh
		ld	b, 70h
; ab DE Bx Leerzeichen
sub_4630:	ld	a, ' '
loc_4632:	dec	de
		ld	(de), a
		djnz	loc_4632
		jr	setcu

; Zeilennummer holen
; ret DE=Nummer, Cy=1 bei Ende oder Bereichsüberschreitung
sub_4638:	ld	a, (hl)
		add	a, 1
		sbc	a, a
		ret	c
		ld	e, (hl)
		inc	hl
		ld	d, (hl)
		dec	hl
		push	hl
		ld	hl, (ARG2)	; Second value entered.
		sbc	hl, de
		pop	hl
		ret

;---------------------------------------
; Zeichen (HL) ausgeben
sub_4649:	ld	a, (hl)
		ld	(de), a		; schreiben in BWS
		inc	de		; Cursor weiter
;
sub_464C:	inc	hl

;---------------------------------------
;ret: C = Zeichenklasse
;	C0: A..Z
;	E0: 0..9
;	98: 0A0h ; Ende-Marker
;	Spezialzeichen s. unk_551D
; 	bit 6=1 bei ..
; 	bit 4=1 bei Kommentar, Zeilenende oder Ende-Marker
;	bit 0=1 bei Trenner (Leerzeichen, Komma)
loc_464D:	ld	a, (hl)
		cp	30h ; '0'
		jr	c, loc_4665	; < 30h
		cp	41h ; 'A'
		jr	c, loc_4660	; '0'..'@'
		ld	c, 0C0h
		cp	5Bh ; '['	; 'A'..'Z'
		ret	c
		ld	c, 98h ; 'ÿ'
		cp	0A0h 		; Ende-Marker
		ret	z
loc_4660:	ld	c, 0E0h
		cp	3Ah ; ':'	; '0'..'9'
		ret	c

;---------------------------------------
;
loc_4665:	push	hl
		and	7Fh
		ld	(unk_552D), a	; Zeichen auch in Tabelle eintragen
					; damit die Fkt. determiniert
		ld	hl, unk_551D	; Tabelle Zeichenklassen Spezialzeichen
loc_466E:	ld	c, (hl)		; Zeichenklasse
loc_466F:	inc	hl
		bit	7, (hl)
		jr	nz, loc_466E
		cp	(hl)		; Vgl. Zeichen
		jr	nz, loc_466F
		pop	hl
		bit	4, c		; bei Kommentar, Zeilenende oder Ende-Marker
		ret	nz
		scf			; Cy=1,wenn Bit4=0
		ret

;---------------------------------------
;
sub_467D:	bit	3, c
		ret	nz
		ld	b, 5
;
; in B = Anz. Zeichen
sub_4682:	bit	4, c		; Kommentar, Zeilenende oder Ende-Marker?
		jr	nz, loc_468D	; ja
		call	sub_4649	; Zeichen (HL) ausgeben
		djnz	sub_4682
		inc	b		; B=1
		ret
;
loc_468D:	inc	de		; cupos
		djnz	loc_468D
loc_4690:	call	loc_464D	; Zeichenklasse ermitteln
sub_4693:	bit	0, c		; Trenner (Leerzeichen, Komma) ?
		ret	z		; nein
		call	sub_464C
		jr	sub_4693

;---------------------------------------
; Ausgabe HL, wenn <> FFFF
; in DE=cupos
sub_469B:	ld	a, h
		and	l
		inc	a
		ret	z
		call	setcu
		SCAL 66h		; TBCD3	Output the value in the HL register in ASCII, followed by a space.
		ret

;---------------------------------------
;
sub_46A5:	ld	b, 0
sub_46A7:	push	hl
		ld	h, (iy-0Ah)
		ld	l, 70h ; 'p'
		ex	(sp), hl	; Wert auf Stack, HL restaurieren
		ld	de, 0
		call	loc_464D	; Zeichenklasse ermitteln
		cp	28h ; '('
		jr	nz, loc_46E6
		ex	(sp), hl
		bit	2, h
		jr	nz, loc_4735
		set	2, h
		inc	l
; nächsten Parameter bearbeiten
loc_46C0:	res	3, b
loc_46C2:	ex	(sp), hl
		call	sub_464C
		jr	c, loc_46E6
		ex	(sp), hl
		ld	(iy-0Ah), h
		bit	4, b
		jr	nz, loc_46DA
		bit	1, b
		jr	nz, loc_46E1
		ld	(word_55F2), de
		jr	loc_46E1
loc_46DA:	ld	a, e
		call	sub_43B1
		ld	(iy-9), e
loc_46E1:	ld	b, l
		pop	hl
		jp	loc_4690
;
loc_46E6:	ld	(word_5582), de
		bit	5, c		; Zeichenklasse
		jr	z, loc_4726
		cp	22h ; '"'
		jr	nz, loc_4701
		call	sub_464C
		ld	d, 0
		ld	e, a
		cp	0A0h ; 'á'
		jr	nz, loc_4723
		ld	e, 20h ; ' '
		dec	hl
		jr	loc_4723
loc_4701:	ex	de, hl
		cp	23h ; '#'
		jr	z, loc_471B
		push	de
		SCAL 64h		; NUM
					; Examine an input line	and convert a hexadecimal value	from ASCII to binary
		ld	a, (de)		; Zeichen nach Ziffern
		cp	'H'		; folgt suffix 'H'?
		jr	nz, loc_4715	; nein, dezimal
		pop	hl
		ld	hl, (NUMV)	; Value	returned by routine NUM.
		inc	de		; nächstes Zeichen (BWS)
		jr	loc_4721
loc_4715:	pop	de
		call	datoh		; DE dez ascii -> hex num HL
		jr	loc_4721
loc_471B:	inc	de
		SCAL 64h		; NUM
					; Examine an input line	and convert a hexadecimal value	from ASCII to binary
		ld	hl, (NUMV)	; Value	returned by routine NUM.
loc_4721:	ex	de, hl		; DE=num. Parameter
		dec	hl		; vorheriges Zeichen (BWS)
loc_4723:	ex	(sp), hl
		jr	loc_477E
loc_4726:	bit	6, c
		jr	z, loc_479E
		call	sub_44C2
		dec	hl
		ex	(sp), hl
		jr	nz, loc_476F
		bit	1, b
		ld	a, 24h  	; Error 24 - zu viele Register
loc_4735:	jr	nz, loc_47AC	; Fehler ausgeben
		bit	2, b
		jr	nz, loc_47AC	; Fehler ausgeben
		set	1, b
		ld	a, d
		cp	62h ; 'b'
		jr	nz, loc_4763
		ld	a, e
		and	30h ; '0'
		push	af
		or	b
		ld	b, a
		rrca
		rrca
		rrca
		rrca
		and	l
		and	1
		or	h
		ld	h, a
		pop	af
		bit	7, h
		jr	nz, loc_475C
		set	7, h
		or	h
		ld	h, a
		jr	loc_4763

loc_475C:	xor	h
		and	30h ; '0'
		ld	a, 25h		; Error 25 - unzulässige Operandenkombination
		jr	nz, loc_47AC	; Fehler ausgeben
loc_4763:	ld	a, l
		and	1
		or	d
		ld	l, a
		ld	de, (word_5582)
loc_476C:	jp	loc_46C0

loc_476F:	jr	nc, loc_477E
		ld	a, (iy-0Ah)
		and	8
		or	h
		ld	h, a
		bit	0, b
		ld	a, 41h 		; Error 41 - illegale Vorwärtsreferenz
		jr	nz, loc_47AC	; Fehler ausgeben
loc_477E:	push	hl
		bit	3, b
		ld	hl, (word_5582)
		jr	nz, loc_4789
		add	hl, de
		jr	loc_478C

loc_4789:	or	a
		sbc	hl, de
loc_478C:	ex	de, hl
		pop	hl
		bit	1, b
		jr	z, loc_476C
		bit	4, b
		ld	a, 27h  	; Error 27 - illegaler Operand
		jr	z, loc_47AC	; Fehler ausgeben
		bit	0, l
		jr	z, loc_47AC	; Fehler ausgeben
		jr	loc_476C

loc_479E:	set	3, b
		ex	(sp), hl
		cp	2Dh ; '-'
		jp	z, loc_46C2
		bit	1, c
		jr	nz, loc_476C
		ld	a, 26h 		; Error 26 - illegales Zeichen
; Fehler ausgeben (fwd)
loc_47AC:	jp	loc_4B19	; Fehler ausgeben

;---------------------------------------
; Ausgabe MC
sub_47AF:	exx
		ld	hl, (word_550C)
		bit	1, (iy+0)
		jp	z, loc_484C
		bit	3, (iy+1)
		jr	z, loc_47F2
		ld	bc, (word_557C)
		ld	de, (word_557E)
loc_47C8:	bit	3, (iy+0)
		jr	z, loc_47D1
		djnz	loc_47E8
		inc	b
loc_47D1:	bit	3, (iy+1)
		call	sub_4857
		ld	b, c
		ld	de, 0F2Fh
		ld	hl, (word_550C)
		ld	(word_5580), hl
		set	3, (iy+0)
		jr	loc_47C8
loc_47E8:	ld	(de), a
		inc	de
		ld	(word_557C), bc
		ld	(word_557E), de

;---------------------------------------
; UP zu sub_47AF
loc_47F2:	ld	hl, (word_550C)	; MC-Adr.
		push	hl
		bit	1, (iy+1)	; Assembleroptionen
					; "MC in Speicher laden"
		jr	z, loc_4827	; nein
		ld	de, (mc_ofs)	; offset for code generation
		add	hl, de
		bit	6, (iy+0)
		jr	nz, loc_4827
		ld	bc, 0F00h	; Bereich F00..1000
		ld	de, 1000h
		call	sub_48DE	; testen
		ld	bc, 1000h	; Bereich 1000..2000
		ld	de, 2000h
		call	sub_48DE	; testen
		ld	bc, (BUFBEG)	; Anfangsdresse
		push	hl
		call	sub_4466	; Bufferende
		ex	de, hl
		pop	hl
		call	sub_48DE	; testen
		ld	(hl), a		; Code in Speicher schreiben
loc_4827:	bit	0, (iy+0)
		jr	nz, loc_484B
		ld	hl, (CURSOR)	; Position of the cursor.
		ld	de, 0F469h
		add	hl, de
		jr	nc, loc_4849
		push	af
		call	sub_4C9C
		ld	de, 0BD0h
		ld	b, 46h ; 'F'
		call	sub_4630
		ld	de, 0B8Fh
		call	setcu
		pop	af
loc_4849:	SCAL 68h		; SPACE	Output a space.
loc_484B:	pop	hl
; Byte eintragen
loc_484C:	inc	hl
		ld	(word_550C), hl	; MC-Adr.
		exx
		ret
sub_4852:	ld	bc, (word_557C)
		cp	a
sub_4857:	push	bc
		push	af
		ld	a, c
		sub	b
		jp	z, loc_48DB
		ld	hl, sub_4D1E
		ld	(SUOUT), hl
		ld	hl, (word_5580)
		bit	6, (iy+1)
		jr	nz, loc_4890
		ld	c, 0
		SCAL 66h		; TBCD3	Output the value in the HL register in ASCII, followed by a space.
		ld	hl, unk_552F
		ld	b, 8
loc_4876:	ld	a, (hl)
		SCAL 67h		; TBCD2	Output the value in the A register in ASCII.
		inc	hl
		rst	28h		; Output the string
		db ' ',0
loc_487D:	djnz	loc_4876
		ld	a, c
		SCAL 68h		; B2HEX	Output the value in the A register in ASCII.
		rst	28h		; Output the string
		db 0Dh,0
		pop	af
		push	af
		jr	nz, loc_48D5
		rst	28h		; Output the string
		db '.',0Dh,0
		jr	loc_48D5
;
unk_488F:	db    0
;
loc_4890:	ld	b, a
		xor	a
		call	sub_4D1E
		rst	28h		; Output the string
		db 0FFh,0FFh,0FFh,0FFh,0
		ld	a, l
		call	sub_4D1E
		ld	a, h
		call	sub_4D1E
		ld	a, b
		call	sub_4D1E
		pop	af
		push	af
		ld	a, 0FFh
		jr	nz, loc_48AE
		xor	a
loc_48AE:	ld	c, a
		call	sub_4D1E
		ld	a, l
		add	a, h
		add	a, b
		add	a, c
		call	sub_4D1E
		ld	hl, unk_552F
		ld	c, 0
loc_48BE:	ld	a, (hl)
		inc	hl
		push	af
		add	a, c
		ld	c, a
		pop	af
		call	sub_4D1E
		djnz	loc_48BE
		ld	a, c
		call	sub_4D1E
		ld	b, 0Ah
loc_48CF:	xor	a
		call	sub_4D1E
		djnz	loc_48CF
loc_48D5:	ld	hl, loc_4CF4
		ld	(SUOUT), hl
loc_48DB:	pop	af
		pop	bc
		ret

;---------------------------------------
; Bereichstest HL between BC and DE
sub_48DE:	push	hl
		or	a
		sbc	hl, de
		pop	hl
		ret	nc
		push	hl
		or	a
		sbc	hl, bc
		pop	hl
		ret	c
		set	6, (iy+0)
		ld	a, 60h ; '`'
		jp	loc_4B19

;---------------------------------------
; Kdo Y
REUSE:		ld	hl, (BUFBEG)	; Anfangsdresse
		inc	hl
		inc	hl		; Länge
		inc	hl
		inc	hl		; Offs
		inc	hl		; Trenner
		ld	a, (hl)		; 1. Zeilennummer bzw. FF
		cp	0FFh		; bei leerem Puffer
		ld	a, 5		; Error 05 - Reaktivieren des Puffers nicht möglich
		jp	nz, sub_4B1D	; Fehler ausgeben, wenn nicht FF
		ld	hl, (word_5575)	; Parameter ARG2 Anfangsadresse
		ld	(BUFBEG), hl	; Anfangsdresse
		ld	de, unk_556F	; Buffersicherung
		ld	bc, 6		; Länge
		ex	de, hl
		ldir			; rückschreiben
		ret

;---------------------------------------
; The "N" editor command returns control to NAS-SYS ("N" is a mnemonic
; for NAS-SYS).
NASSYS:		SCAL 77h		; NNOM	Set the output table back to normal
		SCAL 5Bh		; MRET	end a program and return control to NAS-SYS.

;---------------------------------------
; Kdo + each option specified is turned ON
SETOPT:		ld	a, l
		or	(iy+1)		; Assembleroptionen
		jr	loc_4925

;---------------------------------------
; Kdo - each option specified is turned OFF
RESOPT:		ld	a, l
		cpl
		and	(iy+1)		; Assembleroptionen
		jr	loc_4925

;---------------------------------------
; The "O" editor command allows various options to be set which define
; the output required from the assembler ("O" is a mnemonic for
; Options).
OPTIONS:	ld	a, l
loc_4925:	ld	(iy+1),	a	; Assembleroptionen
		ld	(byte_5507), a	; Assembleroptionen
		ret

;---------------------------------------
; The "H" editor command sets the page size for page mode operation
HEIGHT:		ld	a, e
		jr	nz, loc_4931
		ld	a, 15
loc_4931:	ld	(byte_5514), a
		ret

;---------------------------------------
;The "G" editor command ("G" is a mnemonic for Go) causes control to be
;passed to the object program produced in the last assembly,
GO:		ld	hl, (mc_ofs)	; offset for code generation
		ld	a, h
		or	l
		jr	nz, loc_494C	; bei offs kein Go
		bit	1, (iy+1)	; Assembleroptionen
		jr	z, loc_494C	; no code, no Go
		ld	hl, (word_550E)	; ENT-Adresse
		inc	hl
		ld	a, h
		or	l
		jr	z, loc_494C	; no ENT Adr.
		dec	hl
		jp	(hl)
loc_494C:		ld	a, 4
		jp	loc_4B19

;---------------------------------------
; The "J" editor command sets the delay at the end of each line of
; output to the VDU, and therefore controls the display speed.
DELAY:		ld	a, e
		ld	(word_5508+1), a	; Ausgabeverzögerung BWS
		ret

;---------------------------------------
; The "K" editor command is identical to the "J" command, except that it
; controls the delay for output to the UART
PDELAY:		ld	a, e
		ld	(word_5508), a	; Ausgabeverzögerung
		ret
;???
		ld	(word_5508), hl	; Ausgabeverzögerung
		ret

;---------------------------------------
; The "P" editor command allows object code generated by the assembler
; under the MEMORY option to be placed at a physical address different
; from the logical address of the assembly,
PUT:		ld	(mc_ofs), hl	; offset for code generation
		ret

;---------------------------------------
; store all or part of it on cassette
; tape. This is archieved by the "U" editor command ("U" is a mnemonic
; for UART List).
ULIST:		set	2, (iy+0)
		call	sub_446F
		call	sub_4480
		call	loc_4471

;---------------------------------------
; examine the contents of part or all of the Edit Buffer
; using the "V" editor command. ("V" is a mnemonic for VDU List).
VIEW:		call	sub_45DE	; suche Zeile ARG1 (a)
loc_4973:	call	sub_4C93	; Anzeige Zeile
		jr	loc_4973

;---------------------------------------
; The "R" editor command ("R" is a mnemonic for Resequence) allows the
; entire source program to be renumbered.
RENUM:		call	sub_4404	; Parameter f. RENUM und INUM, Defaults 10
		ex	de, hl		; DE = nächste neue Zeilennummer
loc_497C:	ld	hl, (BUFBEG)	; Anfangsdresse
		inc	hl
		inc	hl		; size übergehen
		inc	hl
loc_4982:	call	loc_442C	; Suche nächste Zeile
		ret	z		; Textende
		ld	(hl), e		; neue Nummer eintragen
		inc	hl
		ld	(hl), d
		ex	de, hl
		call	sub_441F	; Nächste Zeilennummer berechnen (BCD)
		ex	de, hl
		jr	nc, loc_4982	; solange Zeilnnummer < 9999
		;
		ld	a, 1		; Error 01 - Zeilennummerüberlauf
		ld	(byte_5584), a	; und Schrittweite auf 1 rücksetzen
		call	sub_4B1D	; Fehler ausgeben
		ld	de, 1		; Zeilennummer auf 1 rücksetzen
		jr	loc_497C	; weitermachen

;---------------------------------------
; The "/" editor command may be used to search for a string from a
; specified line in the edit buffer.
SEARCH:		call	sub_45DE
		jr	loc_49CD

;---------------------------------------
; The "F" editor command ("F" is a mnemonic for Find) enables the user
; to find the first and thereafter subsequent occurrences of any string
; which will fit on one line in the source program.
FIND:		ld	hl, (CURSOR)	; Position of the cursor.
		ld	de, -3Fh
		add	hl, de
		ld	a, (hl)
		ex	de, hl
		ld	hl, (word_5585)
		cp	20h ; ' '
		jr	z, loc_49CD
		ld	hl, 41h	; 'A'
		add	hl, de
		ld	bc, 43h	; 'C'
		cpdr
		ex	de, hl
		ld	de, unk_552E
		ldir
		dec	de
		ld	a, 0A0h	; 'á'
		ld	(de), a
		ld	hl, (BUFBEG)	; edit buffer
		inc	hl
		inc	hl
		inc	hl
		inc	hl
		inc	hl
loc_49CD:	ld	(word_5585), hl
		ld	a, (hl)
		inc	a
		ret	z
		inc	hl
		inc	hl
loc_49D5:	push	hl
		ld	de, unk_552F
loc_49D9:	ld	a, (de)
		inc	de
		cp	0A0h ; 'á'
		jr	z, loc_49EB
		cp	(hl)
		inc	hl
		jr	z, loc_49D9
		pop	hl
		ld	a, (hl)
		inc	hl
		or	a
		jr	nz, loc_49D5
		jr	loc_49CD
loc_49EB:	pop	hl
		call	loc_442D
		ex	de, hl
		ld	hl, (word_5585)
		ld	(word_5585), de
		jp	sub_4C93

;---------------------------------------
; The "Z" editor command merely presents a source line for editing using
; the NAS-SYS cursor control facilities.
CORRECT:	jr	nz, loc_4A02	; Z=keine Param.
		ld	hl, (word_5510)	; letzte Zeilennummer
		ld	(ARG1),	hl
loc_4A02:	call	sub_45DE	; suche Zeile ARG1
		ld	a, 3		; Error 03 - nicht existierende Zeile
		jp	nc, loc_4B19	; Fehler ausgeben
		call	sub_4C93	; Anzeige Zeile
		xor	a
		ld	(byte_55FE), a
		rst	28h		; Output the string
		db 13h,12h,12h,12h,12h,12h,0
		SCAL 63h		; INLIN	Obtain an input line
					; The DE register is set to the address of the
					; start of the line where the cursor was when the
					; line was entered.
		jp	loc_4A3C

;---------------------------------------
; Deleting a block of source code is made easier by the "X" editor
; command ("X" is a mnemonic for eXpunge).
KILL:		cp	2		; in A=Anz. Parameter
		jp	nz, loc_4B17	; error 99 illegal command
		call	sub_45DE	; suche Zeile ARG1 (a)
		push	hl
		dec	hl
		call	sub_444E
loc_4A2B:	call	sub_4555
		jr	c, loc_4A35
		dec	de
		dec	de
		inc	hl
		jr	loc_4A2B
loc_4A35:	call	sub_4459
		pop	de
		jp	sub_43C2

;---------------------------------------
; Eingabe Quelltext
loc_4A3C:	ld	a, (de)	; in DE= xxx bws(xx,0)
		cp	' '
		jp	z, loc_4B17	; error 99 illegal command
		push	de
		call	datoh		; DE dez ascii -> hex num HL
		add	a, 10h		; Zeichen hinter Zeilennummer-30h+10h
					; = 20? also Leerzeichen?
		jp	nz, loc_4B17	; nein -> error 99 illegal command
		ex	de, hl
		pop	de
		or	a
		sbc	hl, de
		ld	bc, -5
		add	hl, bc		; Zeilennummer mehr als 4 Zeichen?
		jp	c, loc_4B17	; dann error 99 illegal command
		SCAL 64h		; NUM	Examine an input line and convert a hexadecimal value The resulting
					;  value is placed in NUMV (0C21-0C22)
					; The HL and A registers are modified.
					;
		ld	hl, (NUMV)	; Value returned by routine NUM.
		ld	a, h		; Zeilennummer 0000?
		or	l
		jp	z, loc_4B17	; dann error 99 illegal command
		ld	(ARG1),	hl	; Zeilennummer merken
		ld	(word_5510), hl	; Zeilennummer merken
		ld	hl, 2Bh	; '+'
		add	hl, de
loc_4A6B:	ld	a, (hl)
		or	a
		jr	z, loc_4A74
		ld	(hl), 20h ; ' '
		inc	hl
		jr	loc_4A6B
loc_4A74:	dec	hl
		call	sub_448F
		push	hl
		or	a
		sbc	hl, de
		push	hl
		inc	hl
		jr	c, loc_4A82
		inc	hl
		inc	hl
loc_4A82:	push	hl
		push	af
		push	hl
		ex	de, hl
		call	sub_464C
		call	sub_444E
		jr	nc, loc_4A90
		inc	de
		inc	de
loc_4A90:	push	de
		call	sub_45DE	; suche Zeile ARG1
		pop	de
		jr	nc, loc_4AA2
		push	hl
		inc	hl
		call	sub_464C
		jr	nc, loc_4AA0
		dec	de
		dec	de
loc_4AA0:	pop	hl
		scf
loc_4AA2:	push	de
		ld	d, h
		ld	e, l
		call	c, sub_442B	; Suche nächste Zeile
		pop	bc
		ex	(sp), hl
		add	hl, bc
		add	hl, de
		ex	(sp), hl
		ex	de, hl
		ex	(sp), hl
		sbc	hl, de
		push	de
		call	sub_4435	; Textende ermitteln
		add	hl, de
		ex	de, hl
		ld	hl, (BUFEND)	; Buffer Ende
		xor	a		; Cy=0, A=0
		sbc	hl, de
		jp	c, loc_4B19	; Fehler ausgeben
					; Error 00 - Speicherüberlauf
		ld	d, b
		ld	e, c
		call	sub_4459
		pop	hl
		pop	de
		call	sub_43C2
		pop	af
		jr	c, loc_4AF4
		pop	hl
		push	hl
		add	hl, de
		ex	de, hl
		call	sub_4442
		inc	bc
		lddr
		xor	a
		ld	(de), a
		dec	de
		pop	hl
		pop	bc
		ex	(sp), hl
		lddr
		ld	hl, 0C22h
		ldd
		ldd
		pop	bc
		ld	hl, (word_5585)
		or	a
		sbc	hl, de
		add	hl, de
		jr	c, loc_4AF4
		add	hl, bc
		ld	(word_5585), hl
loc_4AF4:					; sub_4D0F-221j
		bit	4, (iy+0)
		jr	z, loc_4B08
		ld	hl, (ARG1)
		call	sub_441F	; Nächste Zeilennummer berechnen (BCD)
		ld	a, 2		; Error 02 - Überlauf im I-Mode
		jp	c, loc_4B19	; Fehler ausgeben
		ld	(word_5587), hl
loc_4B08:		jp	loc_4BCC

;---------------------------------------
; Kdo I manual entry of
; blocks of source code, namely the "I" editor command ("I" is a
; mnemonic for Auto Input)
INUM:		call	sub_4404
		ld	(word_5587), hl
		set	4, (iy+0)
		jr	loc_4B08

; Error 99
loc_4B17:	ld	a, 99h 		; Error 99 - illegales Kommando

; Fehler ausgeben
loc_4B19:	ld	hl, (word_5578); Adr. Ret-Funktion nach Fehler
		push	hl		; auf Stack
sub_4B1D:	ld	de, 0B8Fh
		call	setcu
		ld	e, a		; Fehlernummer merken
		rst	28h		; Output the string
		db "Error ",0
loc_4B2C:	ld	a, e		; Fehlernummer
		SCAL 68h		; B2HEX	Output the value in the A register in ASCII
		set	7, (iy+0)
		jp	sub_4C9C

;---------------------------------------
; cold start
cold:		ld	hl, (BUFBEG)
		ld	(word_5575), hl
		ld	de, unk_556F
		ld	bc, 6
		ldir
		ld	hl, word_4006
		ld	de, BUFBEG	; edit buffer
		ld	bc, 30h	; '0'
		ldir
		ld	a, (ARGH)	; Number of values in input line
		cp	2
		jr	c, loc_4B64
		jr	z, loc_4B5E
		ld	hl, (ARG3)	; Third	value entered.
		ld	(BUFBEG), hl	; edit buffer
loc_4B5E:	ld	hl, (ARG2)	; Second value entered.
		ld	(BUFEND), hl	; Buffer Ende
loc_4B64:	ld	hl, (BUFBEG)	; edit buffer
		ld	(hl), 6
		inc	hl
		xor	a
		ld	(hl), a
		inc	hl
		ld	(hl), a
		inc	hl
		ld	(hl), a
		inc	hl
		ld	(hl), a
		inc	hl
		dec	a
		ld	(hl), a
		ld	(word_5585), hl

;---------------------------------------
; warm start
warm:		ld	sp, byte_55FE
		ld	de, 0B8Ah
		call	setcu
		rst	28h
		db "Copyright (c) Sigma Software Unit",0Dh,0
loc_4BA5:	xor	a
		call	sub_4FB5
		ld	(PORT0), a	; Copy of current state	of output part 0.
		ld	a, (byte_5514)
		ld	(byte_5577), a
loc_4BB2:	ld	iy, byte_55FE
		ld	sp, iy
		ld	hl, unk_40A8	; output routine
		SCAL 71h		; Set HL to the address of the new output table
					; then call this routine. It changes the address for
					; you, and returns with the previous address in HL.
		ld	hl, loc_4CF4
		ld	(SUOUT), hl
		ld	hl, loc_4BA5
		ld	(unk_557A), hl
		ld	(word_5578), hl
loc_4BCC:	ld	a, (byte_5507)
		call	sub_4FC2
		ld	sp, iy
		xor	a
		ld	h, a
		ld	l, a
		ld	(ARG1),	hl
		dec	hl
		ld	(ARG2),	hl	; Second value entered.
		ld	hl, aHeadline	; "ZEAP 2.0/A498  Free=	 End=	   Ent"
		ld	de, 0BCAh
		ld	bc, 28h	; '('
		ldir
loc_4BE9:	ld	hl, loc_4C88
		ld	(SUOUT), hl
		call	sub_4466
		ld	de, 0BDEh
		call	sub_469B
		ld	hl, (word_550C)
		ld	de, 0BE8h
		call	sub_469B
		ld	hl, (word_550E)
		ld	de, 0BF2h
		call	sub_469B
		ld	hl, loc_4CF4
		ld	(SUOUT), hl
		ld	de, 0B8Ah
		call	setcu
		bit	4, (iy+0)
		jr	z, loc_4C21
		ld	hl, (word_5587)
		SCAL 66h		; TBCD3	Output the value in the HL register in ASCII, followed by a space.
loc_4C21:	SCAL 63h		; INLIN	Obtain an input line
		push	de
		ld	a, (de)
		ld	bc, ARGX	; Last command letter entered.
		cp	20h ; ' '
		jr	nz, loc_4C32
		ld	a, (bc)
		cp	46h ; 'F'
		jp	nz, loc_4BA5
loc_4C32:		ld	(bc), a
		ld	bc, ARGC	; Command letter or routine number last	processed for command execution	or input/output.
		inc	de
		inc	bc
		xor	a
		ld	(bc), a
loc_4C3A:	SCAL 64h		;  NUM	Examine an input line and convert a hexadecimal value
					;	from ASCII to binary. Set DE to point to the start of
					;	the line. Leading blanks are ignored. The value is
					;	ended by a blank or null (0). DE is returned pointing
					;	to the next position. If the value is invalid (not
					;	0-9, A-F, or >FFFFH), then the Carry flag is set, and
					;	DE points to the invalid character. The resulting
					;	value is placed in NUMV (0C21-0C22)
		ld	a, (hl)
		or	a
		jr	z, loc_4C51
		inc	bc
		inc	hl
		ld	a, (hl)
		ld	(bc), a
		inc	bc
		inc	hl
		ld	a, (hl)
		ld	(bc), a
		ld	hl, ARGH	; Number of values in input line
		inc	(hl)
		ld	a, (hl)
		cp	3
		jr	nz, loc_4C3A
loc_4C51:	ld	bc, (ARGX)	; Last command letter entered.
		ld	hl, (word_5515)
		pop	de
loc_4C59:	ld	a, (hl)
		or	a
		jp	z, loc_4A3C
		inc	hl
		cp	c
		jr	z, loc_4C66
		inc	hl
		inc	hl
		jr	loc_4C59
loc_4C66:	ld	de, 0B8Ah
		call	setcu
		ld	e, (hl)
		inc	hl
		ld	d, (hl)
		ld	hl, loc_4BA5
		push	hl
		push	de
		ld	hl, (CURSOR)	; Position of the cursor.
		ld	de, -3Fh
		add	hl, de
		ex	de, hl
		call	datoh		; DE dez ascii -> hex num HL
		ex	de, hl
		ld	hl, (ARG1)
		ld	a, (ARGH)	; Number of values in input line
		or	a
		ret

loc_4C88:	push	hl
		ld	hl, (CURSOR)	; Position of the cursor.
		ld	(hl), a
		inc	hl
		ld	(CURSOR), hl	; Position of the cursor.
		pop	hl
		ret

sub_4C93:	call	sub_460C
		jp	nz, loc_4BA5
		call	sub_4600
sub_4C9C:		bit	2, (iy+0)
		jr	z, loc_4CC8
		ex	de, hl
		ld	hl, 0BBAh
		bit	1, (iy+0)
		jr	z, loc_4CAF
		ld	hl, 0BCFh
loc_4CAF:	call	sub_448F
		ex	de, hl
		call	sub_461E
		ld	de, 0B8Ah
		ld	a, (de)
loc_4CBA:	call	sub_4D19
		inc	de
		ld	a, (de)
		or	a
		jp	p, loc_4CBA
		call	sub_4480
		jr	loc_4CE7
loc_4CC8:	ld	a, (iy+0)
		and	3
		cp	1
		ld	a, (word_5508+1)
		call	nz, sub_4473
		ld	a, (byte_5577)
		sub	1
		jr	c, loc_4CE7
		jr	nz, loc_4CE1
		ld	a, (byte_5514)
loc_4CE1:	ld	(byte_5577), a
		call	z, sub_4D0F
loc_4CE7:	ld	de, (word_5512)
		ld	(word_5510), de
		call	sub_4D08
		ld	a, 0Dh

loc_4CF4:	push	af
		push	hl
		push	de
		push	bc
		cp	1Fh
		jr	z, loc_4CFE
		SCAL 65h		; CRT
					; Display on Nascom screen
loc_4CFE:	pop	bc
		pop	de
		pop	hl
		cp	1Bh
		jp	z, loc_4BA5
		pop	af
		ret
sub_4D08:	SCAL 62h		; IN
					; Scan for an input character.
		ret	nc
		cp	1Bh
		jr	z, loc_4D13
;
sub_4D0F:	rst	8		; RIN
					; Obtain an input character in the A register
		cp	1Bh
		ret	nz
loc_4D13:	ld	hl, (unk_557A)
		push	hl
		jr	loc_4CE7

sub_4D19:	call	outch
		jr	sub_4D08

sub_4D1E:	call	nas_outch
		jr	sub_4D08

;---------------------------------------
; "W" The "W" editor command is identical in operation to the "A" editor
; command, except that only those lines containing errors are output in
; the assembly listing.
WERRORS:	ld	a, (iy+1)
		and	24h ; '$'
		or	11h
		ld	(iy+1),	a

;---------------------------------------
; The assembler is entered from the editor by using the editor command
; "A" ("A" is a mnemonic for Assemble).
ASSEMBLE:	ld	hl, loc_4F80
		ld	(word_5578), hl	; Adr. Ret-Funktion nach Fehler
		ld	hl, 0FFFFh
		ld	(word_550E), hl	; ENT-Adresse
		pop	hl
		pop	hl
		ld	l, 1
		push	hl
		call	sub_4466	; Bufferende
		ld	(word_550C), hl	; als Start für MC-Adr.
		push	hl
		call	sub_4DB5	; Assemblieren (Pass 1)
		pop	hl
		ld	(word_550C), hl	; MC-Adr. rücksetzen auf Bufferende
					; für Pass 2
		pop	hl
		push	hl
		bit	7, l		; ohne Fehler (?)
		jr	z, loc_4D56	; dann Pass 2
		bit	4, h
		jr	z, loc_4DB2	; zurück zur Kommandoschleife
; Assemblieren Pass 2
loc_4D56:	bit	3, h
		call	nz, sub_446F
		pop	hl
		ld	a, h
		and	5
		or	2
		ld	l, a
		push	hl
		call	sub_4C9C
		bit	0, l
		jr	nz, loc_4D83
		call	sub_44AA	; Ausgabe "ZEAP Z80 Assembler"
		rst	28h		; Output the string
		db "Source Listing",0
		call	sub_4C9C
		call	sub_4C9C
loc_4D83:	ld	hl, 909h
		bit	6, (iy+1)
		jr	z, loc_4D8F
		ld	hl, 4141h
loc_4D8F:	ld	(word_557C), hl
		push	hl
		call	sub_4DB5	; Assemblieren (Pass 2)
					; Listing ausgeben
		bit	7, (iy+1)	; Assembleroptionen Symboltabelle ?
		call	nz, sub_4567	; Ausgabe Symboltabelle
		ld	a, 61h 		; Error 61 - Bereichsüberschreitung
		bit	6, (iy+0)
		call	nz, sub_4B1D	; Fehler ausgeben
		ld	a, 50h ; 'P'
		bit	7, (iy+0)
		call	nz, sub_4B1D
		call	sub_4852
;
loc_4DB2:	jp	loc_4BA5	; zurück zur Kommandoschleife

; Assemblieren? und Listing ausgeben
sub_4DB5:	call	sub_4435	; Textende ermitteln
		push	de		; DE=Textende
		pop	ix		; IX=Textende
		call	sub_45DE	; suche Zeile ARG1
		dec	hl		; HL=Zeilenanfang-1
		ld	(word_5589), hl	; merken
		inc	hl		; HL=Zeilenanfang

; LOOP: nächste Zeile assemblieren
loc_4DC3:	call	sub_462B	; die letzten beiden BWS-Zeilen leeren
		ld	de, 0B98h
		call	setcu
		call	sub_460C	; Ausgabe Zeilennummer
					; und Ermittlung Zeichenklasse
					; ret A=Zeichen, C=Zeichenklasse
		ret	nz		; Programmende erreicht
		push	af
		jr	nc, loc_4DDE
		push	hl
		ld	hl, (word_550C)	; MC-Adr.
		ld	(ix+0),	l
		ld	(ix+1),	h
		pop	hl
loc_4DDE:	cp	';'		; Kommentarzeile?
		jr	z, loc_4DEA	; ja
		ld	b, 7
		call	sub_4682
		call	z, sub_467D	; Ausgabe Mnemonik
loc_4DEA:	push	af
		call	sub_4600	; Ausgabe Text (Befehlsparameter etc.)
		pop	af
		pop	de
		push	hl
		push	de
		ex	af, af'	;'
		pop	af
		push	af
		ld	hl, 0		; drei Param. auf Stack init.
		push	hl
		push	hl
		push	hl
		ex	af, af'	;'
		ld	a, 10h		; Error 10 - illegale Befehlszeile
		jp	nz, loc_4B19	; Fehler ausgeben
		ex	af, af'	;'
		jr	nc, loc_4E0F
		ld	hl, 0B9Dh
		call	sub_44C2
		ld	a, 31h 		; Error 31 - Marke doppelt definiert
		jp	nc, loc_4B19	; Fehler ausgeben
loc_4E0F:	cp	';'		; Kommentar ?
		jp	z, loc_4F68
		ld	de, 0B8Ah
		ld	hl, (word_550C)	; MC-Adr.
		call	sub_469B	; Ausgabe HL
		; Zeile ist enpackt auf dem BWS
		; jetzt parsen und assemblieren
		ld	hl, (word_5517)	; unk_40AA Parse Tree Mnemonics
		push	hl		; auf Stack
		ld	d, 0
		push	de
		ld	hl, 0BA3h
		jr	loc_4E31
		;
loc_4E29:	call	sub_4306	; Abarbeitung nächstes Token
loc_4E2C:	ld	a, 20h		; Error 20 - unbekannte Mnemonik
loc_4E2E:	jp	c, loc_4B19	; Fehler ausgeben
		;
loc_4E31:	call	sub_464C	; nächstes Zeichen
		jr	c, loc_4E29	; solange Textzeichen
		;
		call	sub_4693	; Mnemonik parsen
		bit	7, (iy-12h)
		jp	z, loc_4EFA
		call	sub_439F
		jr	c, loc_4E2C
		push	af
		cp	80h ; 'Ç'
		bit	3, c
		ld	a, 40h ; '@'
		jp	nc, loc_4EDB
		jr	nz, loc_4E2E
		pop	af
		push	af
		set	2, (iy-0Ah)
		ld	bc, 0BD0h	; Buffer f. Code
		cp	3
		jr	z, loc_4E9E
		jr	nc, loc_4EB5
		ld	b, 5
		call	sub_46A7
		ccf
loc_4E66:	ld	a, 40h		; Error 40 - Pseudo-Befehl-Fehler
		jp	nc, loc_4B19	; Fehler ausgeben
		ld	h, d		; Adresse übernehmen
		ld	l, e
		pop	af
		cp	1
		jr	z, loc_4E79	; (bei org ?)
		jr	c, loc_4E80
		ld	bc, (word_550C)	; MC-Adr.
		add	hl, bc
loc_4E79:	ld	(word_550C), hl	; setze MC-Adr.
		res	3, (iy+0)
loc_4E80:		bit	0, (iy-8)
		jr	z, loc_4E90
		cp	2
		jr	z, loc_4E90
		ld	(ix+0),	l
		ld	(ix+1),	h
loc_4E90:	ex	de, hl
		cp	1
		ld	de, 0B8Ah
		call	z, setcu
		SCAL 	66h		; TBCD3
					; Output the value in the HL register in ASCII,
					; followed by a space. Also add H and L into the C
					; register. The A register is modified.
loc_4E9B:	jp	loc_4F68

loc_4E9E:	ld	d, (hl)
loc_4E9F:	push	bc
		call	sub_464C
		pop	bc
		cp	d
		jr	z, loc_4ECB
		cp	0A0h  		; Zeilenende?
		jr	z, loc_4ECB
		ld	(bc), a
		inc	bc
		jr	loc_4E9F

loc_4EAF:	call	sub_43AD
loc_4EB2:	pop	af
		jr	nc, loc_4ECB
loc_4EB5:	push	bc
		ld	b, 4
		call	sub_46A7
		pop	bc
		push	af
		ld	a, e
		ld	(bc), a
		inc	bc
		bit	0, (iy-0Fh)
		jr	z, loc_4EAF
		ld	a, d
		ld	(bc), a
		inc	bc
		jr	loc_4EB2
; Befehlszeilenende erreicht
; MC wegschreiben/anzeigen
loc_4ECB:	ld	hl, 0BD0h	; Buffer f. Code
loc_4ECE:	or	a		; Cy=0
		sbc	hl, bc		; Test HL=BC
		add	hl, bc
		jr	nc, loc_4E9B
		ld	a, (hl)		; gen. Code-Byte
		inc	hl		; nächstes Zeichen
		call	sub_47AF	; Ausgabe Byte
		jr	loc_4ECE
loc_4EDB:	jr	z, loc_4E66
		pop	af
		cp	81h		; special code SKIP
		jr	nc, loc_4EEA	; bei SKIP
		ld	hl, (word_550C)	; aktuelle MC-Adr.
		ld	(word_550E), hl	; ENT-Adresse
		jr	loc_4E9B	; -> loc_4F82
		;
loc_4EEA:	call	sub_462B	; die letzten beiden BWS-Zeilen leeren
		jr	loc_4E9B	; -> loc_4F82

;---------------------------------------
loc_4EEF:	call	sub_46A5	; Parameter bearbeiten
		ld	a, b
		call	sub_4306	; Abarbeitung nächstes Token
loc_4EF6:	ld	a, 21h ; '!'
		jr	c, loc_4F4C
; Einstieg
loc_4EFA:	bit	3, c
		jr	z, loc_4EEF
		call	sub_439F
		jr	c, loc_4EF6
		pop	bc
		pop	hl
		bit	2, c
		jr	z, loc_4F19
		dec	hl
		dec	hl
		bit	5, (iy+1)	; Assembleroptionen rel.Jp.Abs
		jr	nz, loc_4F19
		; relative Sprungdistanzen absolut
		push	de
		ld	de, (word_550C)	; aktuelle MC-Adr.
		sbc	hl, de
		pop	de
loc_4F19:	ex	de, hl
		ld	a, e
		bit	2, c
		call	nz, sub_43B1
		bit	1, c
		call	nz, sub_43AD
		ld	a, c
		or	a
		ld	a, e
		ex	af, af'	;'
		ld	a, d
		bit	0, c
		ex	de, hl
		pop	hl
		push	hl
		scf
		push	af
		ccf
		push	af
		ex	af, af'	;'
		push	af
		ld	a, d
		or	b
		inc	c
		push	af
		ld	a, 0CBh	; '-'
		bit	3, e
		jr	z, loc_4F40
		push	af
loc_4F40:	bit	4, l
		jr	z, loc_4F5E
		ld	a, h
		bit	6, e
		jr	z, loc_4F51
		or	a
loc_4F4A:	ld	a, 22h 		; Error 22 - Indexregister-Fehler
loc_4F4C:	jp	nz, loc_4B19	; Fehler ausgeben
		jr	loc_4F56
;
loc_4F51:	pop	bc
		bit	0, l
		push	af
		push	bc
loc_4F56:	ld	a, 0DDh		; IX ...
		or	l
		push	af
		bit	4, e
		jr	nz, loc_4F4A
;
loc_4F5E:	ld	a, 0EDh	; Präfix ED
		bit	4, e
loc_4F62:	call	nz, sub_47AF	; Ausgabe Hex-Code
		pop	af
		jr	nc, loc_4F62
loc_4F68:	bit	0, (iy+0)
		call	z, sub_4C9C	; neue Zeile
		bit	3, (iy-0Ah)
		jr	nz, loc_4F84
		bit	6, (iy-0Ah)
		ld	a, 23h 		; Error 23 - Wert bzw. Distanz zu groß
		call	nz, sub_4B1D	; Fehler ausgeben
		jr	loc_4F89
;
;---------------------------------------
; hier gehts nach einem Fehler beim Assemblieren weiter
loc_4F80:	bit	3, (iy-0Ah)
loc_4F84:	ld	a, 30h ; '0'
		call	nz, sub_4B1D	; Fehler ausgeben
loc_4F89:	ld	sp, unk_55F6	; reset Stack
					; (Param vom Stack nehmen)
		pop	af
		jr	nc, loc_4F93
		inc	ix
		inc	ix
loc_4F93:	pop	hl
		call	sub_4D08	; ggf. Abbruch bei Stop
		jp	loc_4DC3	; nächste Zeile assemblieren

		db  13h
		db    0
		db    0
		db    0
		db    0
		db    0
		db    0
		db    0
		db    0
		db    0
		db    0
		db    0
		db    0
		db    0
		db    0
		db    0
		db    0
		db    0
		db    0
		db    0
		db    0
		db    0
		db    0
		db    0
		db    0
		db    0
		db    0

sub_4FB5:	ld	(byte_55FE), a
		ld	hl, 0BBAh
		ld	b, 10h
loc_4FBD:	ld	(hl), a
		inc	hl
		djnz	loc_4FBD
		ret

sub_4FC2:	ld	(0FFFh), a
		ld	a, (byte_5514)
		ld	(byte_5577), a
		ret

;---------------------------------------
;Kdo :
kdo_dpoint:	rst	28h		; Output the string
		db "Command?",0Dh,0
		SCAL 63h		; INLIN	Obtain an input line.
		ld	bc, ARGX	; Last command letter entered.
		ld	a, (de)
		cp	' '
		ret	z
		cp	'A'
		jr	c, loc_4FF1
		cp	'Z'+1
		jr	nc, loc_4FF1
		ld	(bc), a
		ld	(ARGC),	a	; Command letter or routine number last	processed for command execution	or input/output.
		inc	de
		SCAL 79h		; RLIN	Examine an input line and convert up to ten hexadecimal values separated by spaces from ASCII to binary.
		jr	nc, loc_4FF4
loc_4FF1:	SCAL 6Bh		; ERRM	Output the message "Error" followed by a CR.
		ret

;
loc_4FF4:	SCAL 60h		; ARGS	Load the contents of ARGI into HL, ARG2 into DE and ARG3 into BC. ARGI, 2 and 3 are the first three values entered after a NAS-SYS command.
		SCAL 5Ch		; SCALJ	CALL the routine number at address ARGC
		ret

		db    0
		db    0
		db    0
		db    0
		db    0
		db    0
		db    0

; end of "ROM"
		end
