; File Name   :	d:\hobby3\ac1-2010\edas\zeap20\ZEAP20.bin
; Format      :	Binary file
; Base Address:	0000h Range: 1000h - 2008h Loaded length: 1008h
; Segment type:	Pure code
; segment "ROM"

		cpu z80


; NAS-SYS:
SCAL:	     equ 	0018h		; call the NAS-SYS routines
PRS:	     equ	0028h		; Output the string
RDEL:	     equ	0038h		; Wait for a period of time dependent on the
					; value	in the A register.  A is set to	0.

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
byte_F07:	equ	0F07h
word_F08:	equ	0F08h
mc_ofs:	     	equ	0F0Ah	     	; offset for code generation
word_F0C:	equ	0F0Ch
word_F0E:	equ	0F0Eh
word_F10:	equ	0F10h
word_F12:	equ	0F12h
byte_F14:	equ	0F14h
word_F15:	equ	0F15h
word_F17:	equ	0F17h
word_F19:	equ	0F19h
word_F1B:	equ	0F1Bh
unk_F2D:	equ	0F2Dh
unk_F2E:	equ	0F2Eh
unk_F2F:	equ	0F2Fh
unk_F6F:	equ	0F6Fh
word_F75:	equ	0F75h
byte_F77:	equ	0F77h
word_F78:	equ	0F78h
unk_F7A:	equ	0F7Ah
word_F7C:	equ	0F7Ch
word_F7E:	equ	0F7Eh
word_F80:	equ	0F80h
word_F82:	equ	0F82h
byte_F84:	equ	0F84h
word_F85:	equ	0F85h
word_F87:	equ	0F87h
word_F89:	equ	0F89h
word_FF2:	equ	0FF2h
unk_FF6:	equ	0FF6h
byte_FFE:	equ	0FFEh



		org 1000h
		jp	cold		; cold start
		jp	warm		; warm start
;1006..1035 wird nach bufp kopiert
; phase bufp
; RAM:0F00 ?? ??	     BUFP:
; RAM:0F00
; RAM:0F02 ??	     unk_F02:
; RAM:0F03 ??
; RAM:0F04 ??	     outch:
; RAM:0F05 ??
; RAM:0F06 ??
; RAM:0F07 ??	     byte_F07:
; RAM:0F08 ?? ??	     word_F08:
; RAM:0F0A ?? ??	     mc_ofs:
; RAM:0F0C ?? ??	     word_F0C:	     14EAr
; RAM:0F0E ?? ??	     word_F0E:
; RAM:0F10 ?? ??	     word_F10:
; RAM:0F12 ?? ??	     word_F12:
; RAM:0F14 ??	     byte_F14:	     w
; RAM:0F15 ??	     unk_F15:
; RAM:0F17 ?? ??	     word_F17:
; RAM:0F19 ?? ??	     word_F19:
; RAM:0F1B ?? ??	     word_F1B:
; RAM:0F2D ??	     unk_F2D:
; RAM:0F2E ??	     unk_F2E:
; RAM:0F2F ??	     unk_F2F:

word_1006:	dw 2000h
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
		dw 0			; word_F10
		dw 0			; word_F12
		db 0			; byte_F14
		dw kdotab		; unk_F15
		dw unk_10AA		; word_F17
		dw unk_128F		; word_F19
		dw unk_12F4		; word_F1B
		db  91h	; æ
		db  20h
		db  2Ch	; ,
		db  98h	; ÿ
		db  3Bh	; ;
		db    0
		db  82h	; é
		db  2Bh	; +
		db  29h	; )
		db  27h	; '
		db 0C0h	; +
		db  24h	; $
		db 0A0h	; á
		db  23h	; #
		db  22h	; "
		db  80h	; Ç
		db    0			; unk_F2D
		db 0A0h	; á		; unk_F2E
		db 0A0h	; á		; unk_F2F
;Kommandos
kdotab:		db  'V'
		dw kdo_v		; VDU List
		db  'U'
		dw kdo_u
		db  'N'
		dw kdo_n
		db  'R'
		dw kdo_r
		db  'O'
		dw kdo_o
		db  'A'
		dw kdo_a
		db  'F'
		dw kdo_f
		db  'X'
		dw kdo_x
		db  'Z'
		dw kdo_z
		db  'I'
		dw kdo_i
		db  'P'
		dw kdo_p
		db  ':'
		dw kdo_dpoint
		db  'H'
		dw kdo_h
		db  '+'
		dw kdo_plus
		db  '-'
		dw kdo_minus
		db  'W'
		dw kdo_w
		db  'J'
		dw kdo_j
		db  'K'
		dw kdo_k
		db  'G'
		dw kdo_g
		db  'Y'
		dw kdo_y
		db  '/'
		dw kdo_slash
		db    0

aZeap2_0A498Fre:db "ZEAP 2.0/A498  Free=      End=      Ent"
aZzzzzz:	db "=ZZZZZZ"

unk_10A4:	db  5Ah
		db  20h
		db  67h
		db  5Bh
unk_10A8:	db  75h
		db    0
unk_10AA:	db  80h
		db    0
		db    1
		db  4Ch
		db  44h
		db  60h
		db  83h
		db  78h
		db    4
		db 0F4h
		db  3Eh
		db    4
		db 0B1h
		db  0Ah
		db    4
		db 0E5h
		db  1Ah
		db    4
		db 0F3h
		db  3Ah
		db  14h
		db 0ECh
		db  57h
		db  14h
		db 0EEh
		db  5Fh
		db    3
		db    0
		db  83h
		db  40h
		db    4
		db 0F4h
		db    6
		db    3
		db  62h
		db 0F2h
		db  21h
		db    4
		db 0F3h
		db  2Ah
		db    3
		db  6Ah
		db 0F2h
		db  31h
		db    4
		db 0E2h
		db 0F9h
		db  14h
		db 0F3h
		db  7Bh
		db    3
		db  0Ch
		db 0F2h
		db    1
		db  14h
		db 0F3h
		db  4Bh
		db    3
		db  73h
		db 0E2h
		db  22h
		db  14h
		db  8Ch
		db  43h
		db    4
		db 0E0h
		db  32h
		db    3
		db  31h
		db 0E0h
		db    2
		db    3
		db  65h
		db 0E0h
		db  12h
		db  13h
		db 0C9h
		db 0A0h
		db  14h
		db 0D2h
		db 0B0h
		db  13h
		db 0C4h
		db 0A8h
		db  14h
		db 0D2h
		db 0B8h
		db  13h
		db  6Ch
		db 0E0h
		db  47h
		db  13h
		db  6Eh
		db 0E0h
		db  4Fh
		db    1
		db  4Ah
		db  52h
		db 0F8h
		db  18h
		db    3
		db    9
		db 0F8h
		db  20h
		db    2
		db  50h
		db 0F2h
		db 0C3h
		db    3
		db    6
		db 0F2h
		db 0C2h
		db  43h
		db 0E3h
		db 0E9h
		db    1
		db  50h
		db  55h
		db  53h
		db  48h
		db  8Fh
		db 0C5h
		db    2
		db  4Fh
		db  50h
		db  8Fh
		db 0C1h
		db    1
		db  43h
		db  41h
		db  4Ch
		db  4Ch
		db 0F2h
		db 0CDh
		db    5
		db    6
		db 0F2h
		db 0C4h
		db    2
		db  50h
		db  83h
		db 0B8h
		db    3
		db 0F4h
		db 0FEh
		db    3
		db 0CCh
		db  2Fh
		db  13h
		db 0C9h
		db 0A1h
		db  14h
		db 0D2h
		db 0B1h
		db  13h
		db 0C4h
		db 0A9h
		db  14h
		db 0D2h
		db 0B9h
		db    2
		db  43h
		db 0C6h
		db  3Fh
		db  81h
		db  45h
		db  51h
		db 0D5h
		db    0
		db    2
		db  58h
		db  68h
		db 0E8h
		db    8
		db    3
		db  64h
		db 0E2h
		db 0EBh
		db    3
		db  6Bh
		db 0E2h
		db 0E3h
		db    3
		db 0D8h
		db 0D9h
		db    2
		db 0C9h
		db 0FBh
		db  82h
		db  4Eh
		db 0D4h
		db  80h
		db    1
		db  49h
		db  4Eh
		db  43h
		db  80h
		db    4
		db    4
		db  8Ch
		db    3
		db    3
		db  60h
		db 0F5h
		db 0DBh
		db  14h
		db 0E7h
		db  78h
		db  13h
		db    0
		db 0E7h
		db  40h
		db  13h
		db 0C9h
		db 0A2h
		db  14h
		db 0D2h
		db 0B2h
		db  13h
		db 0C4h
		db 0AAh
		db  14h
		db 0D2h
		db 0BAh
		db  12h
		db  4Dh
		db 0FEh
		db  46h
		db  81h
		db  44h
		db  45h
		db  46h
		db 0C2h
		db    4
		db  84h
		db 0D7h
		db    5
		db  84h
		db 0CDh
		db    3
		db  84h
		db 0D3h
		db    2
		db    3
		db  43h
		db  80h
		db    5
		db    4
		db  8Ch
		db  0Bh
		db    2
		db  4Ah
		db  4Eh
		db  5Ah
		db 0F8h
		db  10h
		db    2
		db  41h
		db 0C1h
		db  27h
		db    2
		db 0C9h
		db 0F3h
		db  11h
		db  53h
		db  42h
		db  43h
		db  62h
		db  8Ch
		db  42h
		db    4
		db  60h
		db  83h
		db  98h
		db    5
		db 0F4h
		db 0DEh
		db    2
		db  55h
		db  42h
		db  83h
		db  90h
		db    4
		db 0F4h
		db 0D6h
		db  82h
		db  4Bh
		db  49h
		db 0D0h
		db  81h
		db  0Ah
		db  4Ch
		db  41h
		db  83h
		db  20h
		db  0Ah
		db  52h
		db  41h
		db  83h
		db  28h
		db  0Bh
		db  4Ch
		db  83h
		db  38h
		db  0Ah
		db  45h
		db  54h
		db  7Ah
		db  83h
		db 0C0h
		db    2
		db  43h
		db 0C6h
		db  37h
		db    3
		db  41h
		db  4Ch
		db 0F4h
		db 0DFh
		db    1
		db  52h
		db  45h
		db 0D4h
		db 0C9h
		db    4
		db  86h
		db 0C0h
		db  14h
		db 0C9h
		db  4Dh
		db  14h
		db 0CEh
		db  45h
		db  0Bh
		db  53h
		db  7Ah
		db  83h
		db  80h
		db    2
		db  43h
		db  41h
		db  4Ch
		db 0F8h
		db 0D7h
		db  0Ah
		db  4Ch
		db  43h
		db  83h
		db    0
		db    4
		db 0C1h
		db    7
		db  0Bh
		db  83h
		db  10h
		db    3
		db 0C1h
		db  17h
		db  13h
		db 0C4h
		db  6Fh
		db  0Ah
		db  52h
		db  43h
		db  83h
		db    8
		db    4
		db 0C1h
		db  0Fh
		db  0Bh
		db  83h
		db  18h
		db    3
		db 0C1h
		db  1Fh
		db  13h
		db 0C4h
		db  67h
		db    2
		db  53h
		db  54h
		db 0FCh
		db 0C7h
		db    1
		db  4Fh
		db  52h
		db  83h
		db 0B0h
		db    3
		db 0F4h
		db 0F6h
		db  83h
		db 0C7h
		db    1
		db    2
		db  55h
		db  54h
		db  75h
		db 0E0h
		db 0D3h
		db  14h
		db  67h
		db  80h
		db  41h
		db  14h
		db 0C9h
		db 0A3h
		db  14h
		db 0C4h
		db 0ABh
		db  12h
		db  54h
		db  49h
		db 0D2h
		db 0B3h
		db  13h
		db  44h
		db 0D2h
		db 0BBh
		db    1
		db  41h
		db  44h
		db  44h
		db  60h
		db  83h
		db  80h
		db    5
		db 0F4h
		db 0C6h
		db    4
		db  62h
		db  8Ch
		db    9
		db    3
		db  43h
		db  60h
		db  83h
		db  88h
		db    5
		db 0F4h
		db 0CEh
		db  14h
		db  62h
		db  8Ch
		db  4Ah
		db    2
		db  4Eh
		db  44h
		db  83h
		db 0A0h
		db    4
		db 0F4h
		db 0E6h
		db    1
		db  58h
		db  4Fh
		db  52h
		db  83h
		db 0A8h
		db    4
		db 0F4h
		db 0EEh
		db    9
		db  42h
		db  49h
		db  54h
		db  7Ah
		db  83h
		db  40h
		db  11h
		db  4Eh
		db  45h
		db 0C7h
		db  44h
		db    2
		db  4Fh
		db 0D0h
		db    0
		db    1
		db  48h
		db  41h
		db  4Ch
		db 0D4h
		db  76h
		db    0
unk_128F:	db  80h
		db    0
		db    1
		db 0C8h
		db  26h
		db    2
		db 0CCh
		db  62h
		db    1
		db 0C1h
		db  60h
		db    2
		db 0C6h
		db  68h
		db    1
		db 0C4h
		db  22h
		db    2
		db 0C5h
		db  64h
		db    1
		db 0C2h
		db  20h
		db    2
		db 0C3h
		db  30h
		db    1
		db 0DAh
		db  2Ch
		db    1
		db 0C3h
		db  66h
		db    1
		db  4Eh
		db 0DAh
		db  2Ah
		db    2
		db 0C3h
		db  2Eh
		db    1
		db  53h
		db 0D0h
		db  6Ah
		db  81h
		db 0A4h
		db    0
		db    1
		db 0C5h
		db  24h
		db    1
		db 0CCh
		db  28h
		db    1
		db 0C9h
		db  6Ch
		db  12h
		db 0D8h
		db  62h
		db  32h
		db 0D9h
		db  62h
		db    1
		db 0CDh
		db  38h
		db    1
		db 0D0h
		db  36h
		db    2
		db 0C5h
		db  34h
		db    2
		db 0CFh
		db  32h
		db    1
		db 0D2h
		db  6Eh
		db    0
unk_12DC:	db  60h
		db  63h
		db  28h
		db  26h
		db  24h
		db  22h
		db  66h
		db  20h
unk_12E4:	db  38h
		db  36h
		db  34h
		db  32h
unk_12E8:       db  66h 
                db  2Eh 
                db  2Ch 
                db  2Ah 
unk_12EC:       db  6Ah 
                db  62h 
                db  64h 
                db  30h 
unk_12F0:       db  68h 
                db  62h 
                db  64h 
                db  30h 

unk_12F4:       db  38h 
                dw unk_12DC
                db    8
                dw unk_12DC
                db  38h ; 8
                dw unk_12E4
                db  34h ; 4
                dw unk_12E8
                db  44h ; D
                dw unk_12EC
                db  44h ; D
                dw unk_12F0
sub_1306:					; sub_1DB5:loc_1E29p ...
		exx
		pop	hl
		pop	de
		ex	(sp), hl
		ld	b, a
		inc	d
		bit	7, (hl)
		jr	nz, loc_1387
loc_1310:		inc	hl
		ld	a, (hl)
		and	7Fh ; ''
		cp	70h ; 'p'
		jr	c, loc_134A
		ld	c, a
		xor	b
		rrca
		cp	8
		jr	nc, loc_1381
		cp	5
		jr	c, loc_1396
		ex	af, af'	;'
		ld	a, (iy-0Bh)
		or	a
		jr	nz, loc_1345
		ex	af, af'	;'
		cp	6
		ld	a, (iy-0Ch)
		jr	c, loc_133D
		jr	z, loc_1340
		cp	3
		dec	a
		jr	nc, loc_1345
		inc	a
		jr	z, loc_133D
		inc	a
loc_133D:		rlca
		rlca
		rlca
loc_1340:		ld	(iy-0Dh), a
		and	0C7h ; 'Ã'
loc_1345:		call	nz, sub_13B5
		jr	loc_1399
loc_134A:		cp	20h ; ' '
		jr	nc, loc_137E
		push	hl
		ld	hl, (word_F1B)
		add	a, l
		ld	l, a
		jr	nc, loc_1357
		inc	h
loc_1357:		ld	a, b
		ex	af, af'	;'
		ld	a, (hl)
		and	0Fh
		ld	c, a
		ld	a, (hl)
		ex	af, af'	;'
		inc	hl
		ld	b, (hl)
		inc	hl
		ld	h, (hl)
		ld	l, b
		ld	b, 0
		cpir
		pop	hl
		ld	b, a
		jr	nz, loc_1381
		ex	af, af'	;'
loc_136D:		sla	c
		sub	10h
		jr	nc, loc_136D
		srl	c
		ld	a, (iy-0Dh)
		or	c
		ld	(iy-0Dh), a
		jr	loc_1399
loc_137E:		cp	b
		jr	z, loc_1399
loc_1381:		bit	7, (hl)
		inc	hl
		jr	z, loc_1381
		dec	hl
loc_1387:		inc	hl
		inc	hl
		ld	a, (hl)
		ld	e, a
		and	7
		cp	d
		jp	z, loc_1310
		inc	hl
		jr	nc, loc_1381
		exx
		ret
loc_1396:		ld	(iy-0Eh), a
loc_1399:		ex	(sp), hl
		push	de
		push	hl
		exx
		or	a
		ret
sub_139F:					; sub_1DB5+8Bp	...
		exx
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
sub_13AD:					; sub_1DB5+16Dp
		ld	a, d
		or	7Fh ; ''
		and	e
sub_13B1:					; sub_1DB5+168p
		rlca
		sbc	a, a
		sub	d
		ret	z
sub_13B5:		bit	1, (iy+0)
		ret	z
		set	6, (iy-0Ah)
		ret

;
nas_outch:		rst	18h		; call the NAS-SYS routines
		db 6Fh			; SRLX
					; Send the character in	the A register directly	to the serial output port
		ret
sub_13C2:		ld	b, h
		ld	c, l
		ld	hl, (word_F85)
		or	a
		sbc	hl, de
		add	hl, de
		jr	c, loc_13D8
		sbc	hl, bc
		jr	nc, loc_13D4
		or	a
		sbc	hl, hl
loc_13D4:		add	hl, de
		ld	(word_F85), hl
loc_13D8:		ex	de, hl
		call	sub_1435
		ex	de, hl
		or	a
		sbc	hl, bc
		push	bc
		ex	(sp), hl
		pop	bc
		push	bc
		ldir
		call	sub_1442
		pop	bc
		ret

; DE dez ascii -> hex num HL
datoh:					; sub_1D0F-2CCp ...
		ld	hl, 0
datoh1:		ld	a, (de)
		sub	30h ; '0'
		ret	c		; Ende,	wenn keine Dezimalziffer
		cp	0Ah
		ret	nc		; Ende,	wenn keine Dezimalziffer
		push	de
		ld	d, h
		ld	e, l
		add	hl, hl		; *10
		add	hl, hl
		add	hl, de
		add	hl, hl
		ld	d, 0
		ld	e, a
		add	hl, de
		pop	de
		inc	de
		jr	datoh1

;
sub_1404:		ld	hl, (ARG1)
		ld	a, (ARGH)	; Number of values in input line
		cp	2
		ld	a, 10h
		jr	nz, loc_1413
		ld	a, (ARG2)	; Second value entered.
loc_1413:		ld	(byte_F84), a
		or	a
		ld	a, 6
		jp	z, loc_1B19
		ld	a, h
		or	l
		ret	nz
sub_141F:		ld	a, (byte_F84)
		add	a, l
		daa
		ld	l, a
		ld	a, h
		adc	a, 0
		daa
		ld	h, a
		ret
sub_142B:					; sub_1D0F-26Ap
		inc	hl
loc_142C:		inc	hl
loc_142D:		xor	a
		ld	b, a
		ld	c, a
		cpir
		dec	a
		cp	(hl)
		ret
sub_1435:		push	hl
		ld	hl, (BUFBEG)	; edit buffer
		ld	e, (hl)
		inc	hl
		ld	d, (hl)
		dec	hl
		ex	de, hl
		add	hl, de
		ex	de, hl
		pop	hl
		ret
sub_1442:					; sub_1D0F-23Ep
		push	hl
		push	de
		ld	hl, (BUFBEG)	; edit buffer
		ex	de, hl
		or	a
		sbc	hl, de
		ex	de, hl
		jr	loc_1460
sub_144E:		push	hl
		ld	hl, (BUFBEG)	; edit buffer
		inc	hl
		inc	hl
		ld	e, (hl)
		inc	hl
		ld	d, (hl)
		pop	hl
		ret
sub_1459:					; sub_1D0F-24Dp
		push	hl
		push	de
		ld	hl, (BUFBEG)	; edit buffer
		inc	hl
		inc	hl
loc_1460:		ld	(hl), e
		inc	hl
		ld	(hl), d
		pop	de
		pop	hl
		ret
sub_1466:					; sub_1D0F-120p ...
		call	sub_1435
		ex	de, hl
		call	sub_144E
		add	hl, de
		ret
sub_146F:		rst	18h		; call the NAS-SYS routines
		db 5Fh			; MFLP	      Alter the	state of (turn an or off) the tape drive LED.
					;			     Register A	is modified.
loc_1471:		ld	a, 0FFh
sub_1473:		or	a
		ret	z
		push	bc
		ld	b, a
loc_1477:		xor	a
		rst	RDEL		; RDEL	  Wait for a period of time dependent on the
					;				     value in the A register.  A is set	to 0.
		call	sub_1D08
		djnz	loc_1477
		pop	bc
		ret
sub_1480:		ld	a, 0Dh
		call	sub_1D19
		ld	a, 0Ah
		call	sub_1D19
		ld	a, (word_F08)
		jr	sub_1473
sub_148F:		ld	a, 20h ; ' '
		ld	(hl), 0A0h ; 'á'
		dec	hl
		xor	(hl)
		and	7Fh ; ''
		jr	z, sub_148F
		push	hl
loc_149A:		xor	a
		dec	hl
		xor	(hl)
		jr	z, loc_14A8
		ld	a, 0A0h	; 'á'
		xor	(hl)
		jr	nz, loc_149A
		ld	(hl), 20h ; ' '
		jr	loc_149A
loc_14A8:		pop	hl
		ret

;
sub_14AA:	rst	28h		; Output the string
		db "ZEAP Z80 Assembler - ",0
locret_14C1:		ret

;
sub_14C2:		push	bc
		dec	hl
		push	hl
		ld	de, (word_F19)
		push	de
		ld	d, 0
		push	de
loc_14CD:		call	sub_164C
		bit	6, c
		jr	z, loc_14E1
		call	sub_1306
		jr	nc, loc_14CD
loc_14D9:		call	sub_1435
		ld	hl, (word_F89)
		jr	loc_14FD
loc_14E1:		call	sub_139F
		jr	c, loc_14D9
		bit	7, e
		jr	z, loc_1523
loc_14EA:		ld	de, (word_F0C)
		jr	loc_1523
loc_14F0:		call	sub_1555
		jr	c, loc_1502
		inc	de
		inc	de
		call	sub_1526
		jr	z, loc_1515
		pop	bc
loc_14FD:		pop	bc
		push	bc
		push	bc
		jr	loc_14F0
loc_1502:		pop	hl
loc_1503:		call	sub_164C
		bit	6, c
		jr	nz, loc_1503
loc_150A:		ld	de, 0
		set	3, (iy-0Ah)
		scf
		sbc	a, a
		jr	loc_1523
loc_1515:		push	ix
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
loc_1523:		pop	bc
		pop	bc
		ret
sub_1526:		ld	b, h
		ld	c, l
		pop	hl
		ex	(sp), hl
		push	bc
		ex	(sp), hl
loc_152C:		ex	(sp), hl
		call	sub_164C
		ex	(sp), hl
		bit	6, c
		jr	z, loc_1545
		inc	hl
		cp	(hl)
		jr	z, loc_152C
		push	af
		call	loc_164D
		pop	af
		bit	6, c
		jr	nz, loc_154E
		or	c
		jr	loc_154E
loc_1545:		call	sub_164C
		xor	a
		bit	6, c
		jr	z, loc_154E
		sub	c
loc_154E:		ex	(sp), hl
		pop	bc
		ex	(sp), hl
		push	hl
		ld	h, b
		ld	l, c
		ret
sub_1555:					; sub_1555+Dj ...
		call	loc_142D
		push	de
		call	sub_1638
		pop	de
		ret	c
		inc	hl
		call	sub_164C
		jr	nc, sub_1555
		dec	hl
		or	a
		ret
sub_1567:		call	sub_1C9C
		call	sub_14AA
		rst	28h		; Output the string
		db "Symbol Table",0
		call	sub_1C9C
		call	sub_1C9C
		ld	de, unk_10A4
loc_1584:		ld	hl, aZzzzzz	; "=ZZZZZZ"
		push	hl
		push	hl
		ld	hl, (word_F89)
loc_158C:		call	sub_1555
		jr	c, loc_15A9
		push	hl
		push	de
		call	sub_1526
		pop	bc
		pop	bc
		jr	nc, loc_158C
		pop	hl
		push	hl
		push	bc
		push	bc
		call	sub_1526
		pop	hl
		pop	bc
		jr	nc, loc_158C
		pop	af
		push	bc
		jr	loc_158C
loc_15A9:		pop	hl
		pop	bc
		or	a
		sbc	hl, bc
		add	hl, bc
		jp	z, sub_1C9C
		push	hl
		push	hl
		inc	hl
		call	sub_14C2
		pop	hl
		jr	z, loc_15DB
		push	hl
		ex	de, hl
		rst	18h		; call the NAS-SYS routines
		db 66h			; TBCD3
					; Output the value in the HL register in ASCII,	followed by a space. Also add H	and L into the C register. The A register is modified.
		rst	28h		; Output the string
		db 8,"H ",0
loc_15C4:		pop	hl
		dec	hl
		call	sub_160C
loc_15C9:		ld	b, 7
		call	sub_1682
		ld	hl, -0BA2h	; -unk_ba2
		add	hl, de
		ld	de, 0BA2h
		call	nc, sub_1621
		call	c, sub_1C9C
loc_15DB:		pop	de
		jr	loc_1584
sub_15DE:		ld	hl, (BUFBEG)	; edit buffer
		inc	hl
		inc	hl
loc_15E3:		call	sub_142B
		ret	z
		ld	e, (hl)
		inc	hl
		ld	d, (hl)
		dec	hl
		push	hl
		ld	hl, unk_188F
		ld	a, (hl)
		inc	(hl)
		cp	(hl)
		nop
		dec	(hl)
		ld	hl, (ARG1)
		or	a
		sbc	hl, de
		pop	hl
		ccf
		ret	nc
		ret	z
		jr	loc_15E3
sub_1600:		inc	hl
		or	a
		ld	a, 0A0h	; 'á'
		ld	(de), a
		ret	z
		dec	hl
		call	sub_1649
		jr	sub_1600
sub_160C:		call	sub_1638
		ret	c
		push	hl
		ex	de, hl
		rst	18h		; call the NAS-SYS routines
		ld	h, (hl)
		ld	(word_F12), hl
		pop	hl
		inc	hl
		call	sub_164C
		bit	7, a
sub_161E:		ld	de, 0F8Bh
sub_1621:		push	hl
		ld	hl, (CURSOR)	; Position of the cursor.
		ex	de, hl
		ld	(CURSOR), hl	; Position of the cursor.
		pop	hl
		ret
sub_162B:					; sub_1DB5:loc_1EEAp
		ld	de, 0BFAh
		ld	b, 70h ; 'p'
sub_1630:		ld	a, 20h ; ' '
loc_1632:		dec	de
		ld	(de), a
		djnz	loc_1632
		jr	sub_1621
sub_1638:		ld	a, (hl)
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
sub_1649:		ld	a, (hl)
		ld	(de), a
		inc	de
sub_164C:					; sub_14AA:loc_1503p ...
		inc	hl
loc_164D:					; sub_1682:loc_1690p ...
		ld	a, (hl)
		cp	30h ; '0'
		jr	c, loc_1665
		cp	41h ; 'A'
		jr	c, loc_1660
		ld	c, 0C0h	; '+'
		cp	5Bh ; '['
		ret	c
		ld	c, 98h ; 'ÿ'
		cp	0A0h ; 'á'
		ret	z
loc_1660:		ld	c, 0E0h	; 'Ó'
		cp	3Ah ; ':'
		ret	c
loc_1665:		push	hl
		and	7Fh ; ''
		ld	(unk_F2D), a
		ld	hl, 0F1Dh
loc_166E:		ld	c, (hl)
loc_166F:		inc	hl
		bit	7, (hl)
		jr	nz, loc_166E
		cp	(hl)
		jr	nz, loc_166F
		pop	hl
		bit	4, c
		ret	nz
		scf
		ret
sub_167D:		bit	3, c
		ret	nz
		ld	b, 5
sub_1682:		bit	4, c
		jr	nz, loc_168D
		call	sub_1649
		djnz	sub_1682
		inc	b
		ret
loc_168D:		inc	de
		djnz	loc_168D
loc_1690:		call	loc_164D
sub_1693:		bit	0, c
		ret	z
		call	sub_164C
		jr	sub_1693
sub_169B:					; sub_1D0F-111p ...
		ld	a, h
		and	l
		inc	a
		ret	z
		call	sub_1621
		rst	18h
		db 66h			; TBCD3
					; Output the value in the HL register in ASCII,	followed by a space. Also add H	and L into the C register. The A register is modified.
		ret
sub_16A5:		ld	b, 0
sub_16A7:					; sub_1DB5+103p
		push	hl
		ld	h, (iy-0Ah)
		ld	l, 70h ; 'p'
		ex	(sp), hl
		ld	de, 0
		call	loc_164D
		cp	28h ; '('
		jr	nz, loc_16E6
		ex	(sp), hl
		bit	2, h
		jr	nz, loc_1735
		set	2, h
		inc	l
loc_16C0:		res	3, b
loc_16C2:		ex	(sp), hl
		call	sub_164C
		jr	c, loc_16E6
		ex	(sp), hl
		ld	(iy-0Ah), h
		bit	4, b
		jr	nz, loc_16DA
		bit	1, b
		jr	nz, loc_16E1
		ld	(word_FF2), de
		jr	loc_16E1
loc_16DA:		ld	a, e
		call	sub_13B1
		ld	(iy-9), e
loc_16E1:		ld	b, l
		pop	hl
		jp	loc_1690
loc_16E6:		ld	(word_F82), de
		bit	5, c
		jr	z, loc_1726
		cp	22h ; '"'
		jr	nz, loc_1701
		call	sub_164C
		ld	d, 0
		ld	e, a
		cp	0A0h ; 'á'
		jr	nz, loc_1723
		ld	e, 20h ; ' '
		dec	hl
		jr	loc_1723
loc_1701:		ex	de, hl
		cp	23h ; '#'
		jr	z, loc_171B
		push	de
		rst	18h		; call the NAS-SYS routines
		db 64h			; NUM
					; Examine an input line	and convert a hexadecimal value	from ASCII to binary
		ld	a, (de)
		cp	48h ; 'H'
		jr	nz, loc_1715
		pop	hl
		ld	hl, (NUMV)	; Value	returned by routine NUM.
		inc	de
		jr	loc_1721
loc_1715:		pop	de
		call	datoh		; DE dez ascii -> hex num HL
		jr	loc_1721
loc_171B:		inc	de
		rst	18h		; call the NAS-SYS routines
		db 64h			; NUM
					; Examine an input line	and convert a hexadecimal value	from ASCII to binary
		ld	hl, (NUMV)	; Value	returned by routine NUM.
loc_1721:		ex	de, hl
		dec	hl
loc_1723:		ex	(sp), hl
		jr	loc_177E
loc_1726:		bit	6, c
		jr	z, loc_179E
		call	sub_14C2
		dec	hl
		ex	(sp), hl
		jr	nz, loc_176F
		bit	1, b
		ld	a, 24h ; '$'
loc_1735:		jr	nz, loc_17AC
		bit	2, b
		jr	nz, loc_17AC
		set	1, b
		ld	a, d
		cp	62h ; 'b'
		jr	nz, loc_1763
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
		jr	nz, loc_175C
		set	7, h
		or	h
		ld	h, a
		jr	loc_1763
loc_175C:		xor	h
		and	30h ; '0'
		ld	a, 25h ; '%'
		jr	nz, loc_17AC
loc_1763:		ld	a, l
		and	1
		or	d
		ld	l, a
		ld	de, (word_F82)
loc_176C:		jp	loc_16C0
loc_176F:		jr	nc, loc_177E
		ld	a, (iy-0Ah)
		and	8
		or	h
		ld	h, a
		bit	0, b
		ld	a, 41h ; 'A'
		jr	nz, loc_17AC
loc_177E:					; sub_16A7:loc_176Fj
		push	hl
		bit	3, b
		ld	hl, (word_F82)
		jr	nz, loc_1789
		add	hl, de
		jr	loc_178C
loc_1789:		or	a
		sbc	hl, de
loc_178C:		ex	de, hl
		pop	hl
		bit	1, b
		jr	z, loc_176C
		bit	4, b
		ld	a, 27h ; '''
		jr	z, loc_17AC
		bit	0, l
		jr	z, loc_17AC
		jr	loc_176C
loc_179E:		set	3, b
		ex	(sp), hl
		cp	2Dh ; '-'
		jp	z, loc_16C2
		bit	1, c
		jr	nz, loc_176C
		ld	a, 26h ; '&'
loc_17AC:					; sub_16A7+92j	...
		jp	loc_1B19
sub_17AF:					; sub_1DB5:loc_1F62p
		exx
		ld	hl, (word_F0C)
		bit	1, (iy+0)
		jp	z, loc_184C
		bit	3, (iy+1)
		jr	z, loc_17F2
		ld	bc, (word_F7C)
		ld	de, (word_F7E)
loc_17C8:		bit	3, (iy+0)
		jr	z, loc_17D1
		djnz	loc_17E8
		inc	b
loc_17D1:		bit	3, (iy+1)
		call	sub_1857
		ld	b, c
		ld	de, 0F2Fh
		ld	hl, (word_F0C)
		ld	(word_F80), hl
		set	3, (iy+0)
		jr	loc_17C8
loc_17E8:		ld	(de), a
		inc	de
		ld	(word_F7C), bc
		ld	(word_F7E), de
loc_17F2:		ld	hl, (word_F0C)
		push	hl
		bit	1, (iy+1)
		jr	z, loc_1827
		ld	de, (mc_ofs)	; offset for code generation
		add	hl, de
		bit	6, (iy+0)
		jr	nz, loc_1827
		ld	bc, 0F00h
		ld	de, 1000h
		call	sub_18DE
		ld	bc, 1000h
		ld	de, 2000h
		call	sub_18DE
		ld	bc, (BUFBEG)	; edit buffer
		push	hl
		call	sub_1466
		ex	de, hl
		pop	hl
		call	sub_18DE
		ld	(hl), a
loc_1827:		bit	0, (iy+0)
		jr	nz, loc_184B
		ld	hl, (CURSOR)	; Position of the cursor.
		ld	de, 0F469h
		add	hl, de
		jr	nc, loc_1849
		push	af
		call	sub_1C9C
		ld	de, 0BD0h
		ld	b, 46h ; 'F'
		call	sub_1630
		ld	de, 0B8Fh
		call	sub_1621
		pop	af
loc_1849:		rst	18h		; call the NAS-SYS routines
		db 68h			; SPACE	     Output a space. The A register is set to a	space.
loc_184B:		pop	hl
loc_184C:		inc	hl
		ld	(word_F0C), hl
		exx
		ret
sub_1852:		ld	bc, (word_F7C)
		cp	a
sub_1857:		push	bc
		push	af
		ld	a, c
		sub	b
		jp	z, loc_18DB
		ld	hl, sub_1D1E
		ld	(SUOUT), hl
		ld	hl, (word_F80)
		bit	6, (iy+1)
		jr	nz, loc_1890
		ld	c, 0
		rst	18h		; call the NAS-SYS routines
		db 66h			; TBCD3	     Output the	value in the HL	register in ASCII, followed
					;			     by	a space. Also add H and	L into the C register.The
					;			     A register	is modified.
		ld	hl, unk_F2F
		ld	b, 8
loc_1876:		ld	a, (hl)
		rst	18h		; call the NAS-SYS routines
		db 67h			; TBCD2	     Output the	value in the A register	in ASCII. Also
					;			     add A into	the C register.	The A register is modified.
		inc	hl
		rst	28h		; Output the string
		db ' ',0
loc_187D:		djnz	loc_1876
		ld	a, c
		rst	18h		; call the NAS-SYS routines
		db 68h			; B2HEX	     Output the	value in the A register	in ASCII. The A
					;			     register is modified.
		rst	28h		; Output the string
		db 0Dh,0
		pop	af
		push	af
		jr	nz, loc_18D5
		rst	28h		; Output the string
		db '.',0Dh,0
		jr	loc_18D5
unk_188F:	db    0
loc_1890:		ld	b, a
		xor	a
		call	sub_1D1E
		rst	28h		; Output the string
		db 0FFh,0FFh,0FFh,0FFh,0
		ld	a, l
		call	sub_1D1E
		ld	a, h
		call	sub_1D1E
		ld	a, b
		call	sub_1D1E
		pop	af
		push	af
		ld	a, 0FFh
		jr	nz, loc_18AE
		xor	a
loc_18AE:		ld	c, a
		call	sub_1D1E
		ld	a, l
		add	a, h
		add	a, b
		add	a, c
		call	sub_1D1E
		ld	hl, unk_F2F
		ld	c, 0
loc_18BE:		ld	a, (hl)
		inc	hl
		push	af
		add	a, c
		ld	c, a
		pop	af
		call	sub_1D1E
		djnz	loc_18BE
		ld	a, c
		call	sub_1D1E
		ld	b, 0Ah
loc_18CF:		xor	a
		call	sub_1D1E
		djnz	loc_18CF
loc_18D5:		ld	hl, loc_1CF4
		ld	(SUOUT), hl
loc_18DB:		pop	af
		pop	bc
		ret
sub_18DE:		push	hl
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
		jp	loc_1B19
kdo_y:		ld	hl, (BUFBEG)	; edit buffer
		inc	hl
		inc	hl
		inc	hl
		inc	hl
		inc	hl
		ld	a, (hl)
		cp	0FFh
		ld	a, 5
		jp	nz, sub_1B1D
		ld	hl, (word_F75)
		ld	(BUFBEG), hl	; edit buffer
		ld	de, unk_F6F
		ld	bc, 6
		ex	de, hl
		ldir
		ret
; The "N" editor command returns control to NAS-SYS ("N" is a mnemonic
; NAS-SYS).
kdo_n:		rst	18h		; call the NAS-SYS routines
		db 77h
		rst	18h		; call the NAS-SYS routines
		db 5Bh			; MRET	      end a program and	return control to NAS-SYS.

; each option specified is turned ON
kdo_plus:		ld	a, l
		or	(iy+1)
		jr	loc_1925

; each option specified is turned OFF
kdo_minus:		ld	a, l
		cpl
		and	(iy+1)
		jr	loc_1925

; The "O" editor command allows various options to be set which define
; the output required from the assembler ("O" is a mnemonic for
; Options).
kdo_o:		ld	a, l
loc_1925:		ld	(iy+1),	a
		ld	(byte_F07), a
		ret

; The "H" editor command sets the page size for page mode operation
kdo_h:		ld	a, e
		jr	nz, loc_1931
		ld	a, 0Fh
loc_1931:		ld	(byte_F14), a
		ret

;The "G" editor command ("G" is a mnemonic for Go) causes control to be
;passed to the object program produced in the last assembly,
kdo_g:		ld	hl, (mc_ofs)	; offset for code generation
		ld	a, h
		or	l
		jr	nz, loc_194C
		bit	1, (iy+1)
		jr	z, loc_194C
		ld	hl, (word_F0E)
		inc	hl
		ld	a, h
		or	l
		jr	z, loc_194C
		dec	hl
		jp	(hl)
loc_194C:		ld	a, 4
		jp	loc_1B19

; The "J" editor command sets the delay at the end of each line of
; output to the VDU, and therefore controls the display speed.
kdo_j:		ld	a, e
		ld	(word_F08+1), a
		ret

; The "K" editor command is identical to the "J" command, except that it
; controls the delay for output to the UART
kdo_k:		ld	a, e
		ld	(word_F08), a
		ret
		ld	(word_F08), hl
		ret

; The "P" editor command allows object code generated by the assembler
; under the MEMORY option to be placed at a physical address different
; from the logical address of the assembly,
kdo_p:		ld	(mc_ofs), hl	; offset for code generation
		ret

; store all or part of it on cassette
; tape. This is archieved by the "U" editor command ("U" is a mnemonic
; for UART List).
kdo_u:		set	2, (iy+0)
		call	sub_146F
		call	sub_1480
		call	loc_1471

; examine the contents of part or all of the Edit Buffer
; using the "V" editor command. ("V" is a mnemonic for VDU List).
kdo_v:		call	sub_15DE	; VDU List
loc_1973:		call	sub_1C93
		jr	loc_1973

; The "R" editor command ("R" is a mnemonic for Resequence) allows the
; entire source program to be renumbered.
kdo_r:		call	sub_1404
		ex	de, hl
loc_197C:		ld	hl, (BUFBEG)	; edit buffer
		inc	hl
		inc	hl
		inc	hl
loc_1982:		call	loc_142C
		ret	z
		ld	(hl), e
		inc	hl
		ld	(hl), d
		ex	de, hl
		call	sub_141F
		ex	de, hl
		jr	nc, loc_1982
		ld	a, 1
		ld	(byte_F84), a
		call	sub_1B1D
		ld	de, 1
		jr	loc_197C

; The "/" editor command may be used to search for a string from a
; specified line in the edit buffer.
kdo_slash:		call	sub_15DE
		jr	loc_19CD

; The "F" editor command ("F" is a mnemonic for Find) enables the user
; to find the first and thereafter subsequent occurrences of any string
; which will fit on one line in the source program.
kdo_f:		ld	hl, (CURSOR)	; Position of the cursor.
		ld	de, -3Fh
		add	hl, de
		ld	a, (hl)
		ex	de, hl
		ld	hl, (word_F85)
		cp	20h ; ' '
		jr	z, loc_19CD
		ld	hl, 41h	; 'A'
		add	hl, de
		ld	bc, 43h	; 'C'
		cpdr
		ex	de, hl
		ld	de, unk_F2E
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
loc_19CD:		ld	(word_F85), hl
		ld	a, (hl)
		inc	a
		ret	z
		inc	hl
		inc	hl
loc_19D5:		push	hl
		ld	de, unk_F2F
loc_19D9:		ld	a, (de)
		inc	de
		cp	0A0h ; 'á'
		jr	z, loc_19EB
		cp	(hl)
		inc	hl
		jr	z, loc_19D9
		pop	hl
		ld	a, (hl)
		inc	hl
		or	a
		jr	nz, loc_19D5
		jr	loc_19CD
loc_19EB:		pop	hl
		call	loc_142D
		ex	de, hl
		ld	hl, (word_F85)
		ld	(word_F85), de
		jp	sub_1C93
; The "Z" editor command merely presents a source line for editing using
; the NAS-SYS cursor control facilities.
kdo_z:		jr	nz, loc_1A02
		ld	hl, (word_F10)
		ld	(ARG1),	hl
loc_1A02:		call	sub_15DE
		ld	a, 3
		jp	nc, loc_1B19
		call	sub_1C93
		xor	a
		ld	(byte_FFE), a
		rst	28h		; Output the string
		db 13h,12h,12h,12h,12h,12h,0
		rst	18h		; call the NAS-SYS routines
		db 63h			; INLIN       Obtain an input line
					; The DE register is set to the address of the
					; start of the line where the cursor was when the
					; line was entered.
		jp	loc_1A3C

; Deleting a block of source code is made easier by the "X" editor
; command ("X" is a mnemonic for eXpunge).
kdo_x:		cp	2
		jp	nz, loc_1B17
		call	sub_15DE
		push	hl
		dec	hl
		call	sub_144E
loc_1A2B:		call	sub_1555
		jr	c, loc_1A35
		dec	de
		dec	de
		inc	hl
		jr	loc_1A2B
loc_1A35:		call	sub_1459
		pop	de
		jp	sub_13C2
loc_1A3C:		ld	a, (de)
		cp	20h ; ' '
		jp	z, loc_1B17
		push	de
		call	datoh		; DE dez ascii -> hex num HL
		add	a, 10h
		jp	nz, loc_1B17
		ex	de, hl
		pop	de
		or	a
		sbc	hl, de
		ld	bc, -5
		add	hl, bc
		jp	c, loc_1B17
		rst	18h
		db 64h			; NUM     Examine an input line and convert a hexadecimal value The resulting
					;  value is placed in NUMV (0C21-0C22)
					; The HL and A registers are modified.
					;
		ld	hl, (NUMV)	; Value returned by routine NUM.
		ld	a, h
		or	l
		jp	z, loc_1B17
		ld	(ARG1),	hl
		ld	(word_F10), hl
		ld	hl, 2Bh	; '+'
		add	hl, de
loc_1A6B:		ld	a, (hl)
		or	a
		jr	z, loc_1A74
		ld	(hl), 20h ; ' '
		inc	hl
		jr	loc_1A6B
loc_1A74:		dec	hl
		call	sub_148F
		push	hl
		or	a
		sbc	hl, de
		push	hl
		inc	hl
		jr	c, loc_1A82
		inc	hl
		inc	hl
loc_1A82:		push	hl
		push	af
		push	hl
		ex	de, hl
		call	sub_164C
		call	sub_144E
		jr	nc, loc_1A90
		inc	de
		inc	de
loc_1A90:		push	de
		call	sub_15DE
		pop	de
		jr	nc, loc_1AA2
		push	hl
		inc	hl
		call	sub_164C
		jr	nc, loc_1AA0
		dec	de
		dec	de
loc_1AA0:		pop	hl
		scf
loc_1AA2:		push	de
		ld	d, h
		ld	e, l
		call	c, sub_142B
		pop	bc
		ex	(sp), hl
		add	hl, bc
		add	hl, de
		ex	(sp), hl
		ex	de, hl
		ex	(sp), hl
		sbc	hl, de
		push	de
		call	sub_1435
		add	hl, de
		ex	de, hl
		ld	hl, (BUFEND)	; Buffer Ende
		xor	a
		sbc	hl, de
		jp	c, loc_1B19
		ld	d, b
		ld	e, c
		call	sub_1459
		pop	hl
		pop	de
		call	sub_13C2
		pop	af
		jr	c, loc_1AF4
		pop	hl
		push	hl
		add	hl, de
		ex	de, hl
		call	sub_1442
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
		ld	hl, (word_F85)
		or	a
		sbc	hl, de
		add	hl, de
		jr	c, loc_1AF4
		add	hl, bc
		ld	(word_F85), hl
loc_1AF4:					; sub_1D0F-221j
		bit	4, (iy+0)
		jr	z, loc_1B08
		ld	hl, (ARG1)
		call	sub_141F
		ld	a, 2
		jp	c, loc_1B19
		ld	(word_F87), hl
loc_1B08:		jp	loc_1BCC

; manual entry of
; blocks of source code, namely the "I" editor command ("I" is a
; mnemonic for Auto Input)
kdo_i:		call	sub_1404
		ld	(word_F87), hl
		set	4, (iy+0)
		jr	loc_1B08
loc_1B17:		ld	a, 99h ; 'Ö'
loc_1B19:					; sub_16A7:loc_17ACj ...
		ld	hl, (word_F78)
		push	hl
sub_1B1D:		ld	de, 0B8Fh
		call	sub_1621
		ld	e, a
		rst	28h		; Output the string
		db "Error ",0
loc_1B2C:		ld	a, e
		rst	18h		; call the NAS-SYS routines
		db 68h			; B2HEX	     Output the	value in the A register	in ASCII
		set	7, (iy+0)
		jp	sub_1C9C

;---------------------------------------
cold:		ld	hl, (BUFBEG)	; cold start
		ld	(word_F75), hl
		ld	de, unk_F6F
		ld	bc, 6
		ldir
		ld	hl, word_1006
		ld	de, BUFBEG	; edit buffer
		ld	bc, 30h	; '0'
		ldir
		ld	a, (ARGH)	; Number of values in input line
		cp	2
		jr	c, loc_1B64
		jr	z, loc_1B5E
		ld	hl, (ARG3)	; Third	value entered.
		ld	(BUFBEG), hl	; edit buffer
loc_1B5E:		ld	hl, (ARG2)	; Second value entered.
		ld	(BUFEND), hl	; Buffer Ende
loc_1B64:		ld	hl, (BUFBEG)	; edit buffer
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
		ld	(word_F85), hl

;---------------------------------------
warm:		ld	sp, byte_FFE	; warm start
		ld	de, 0B8Ah
		call	sub_1621
		rst	28h
		db "Copyright (c) Sigma Software Unit",0Dh,0
loc_1BA5:		xor	a
		call	sub_1FB5
		ld	(PORT0), a	; Copy of current state	of output part 0.
		ld	a, (byte_F14)
		ld	(byte_F77), a
loc_1BB2:		ld	iy, byte_FFE
		ld	sp, iy
		ld	hl, unk_10A8
		rst	18h
		db 71h
		ld	hl, loc_1CF4
		ld	(SUOUT), hl
		ld	hl, loc_1BA5
		ld	(unk_F7A), hl
		ld	(word_F78), hl
loc_1BCC:		ld	a, (byte_F07)
		call	sub_1FC2
		ld	sp, iy
		xor	a
		ld	h, a
		ld	l, a
		ld	(ARG1),	hl
		dec	hl
		ld	(ARG2),	hl	; Second value entered.
		ld	hl, aZeap2_0A498Fre ; "ZEAP 2.0/A498  Free=	 End=	   Ent"
		ld	de, 0BCAh
		ld	bc, 28h	; '('
		ldir
loc_1BE9:		ld	hl, loc_1C88
		ld	(SUOUT), hl
		call	sub_1466
		ld	de, 0BDEh
		call	sub_169B
		ld	hl, (word_F0C)
		ld	de, 0BE8h
		call	sub_169B
		ld	hl, (word_F0E)
		ld	de, 0BF2h
		call	sub_169B
		ld	hl, loc_1CF4
		ld	(SUOUT), hl
		ld	de, 0B8Ah
		call	sub_1621
		bit	4, (iy+0)
		jr	z, loc_1C21
		ld	hl, (word_F87)
		rst	18h
		db 66h			; TBCD3	     Output the	value in the HL	register in ASCII, followed
					;			     by	a space.
loc_1C21:		rst	18h
		db 63h			; INLIN	      Obtain an	input line
		push	de
		ld	a, (de)
		ld	bc, ARGX	; Last command letter entered.
		cp	20h ; ' '
		jr	nz, loc_1C32
		ld	a, (bc)
		cp	46h ; 'F'
		jp	nz, loc_1BA5
loc_1C32:		ld	(bc), a
		ld	bc, ARGC	; Command letter or routine number last	processed for command execution	or input/output.
		inc	de
		inc	bc
		xor	a
		ld	(bc), a
loc_1C3A:		rst	18h
		db 64h			;  NUM	     Examine an	input line and convert a hexadecimal value
					;			     from ASCII	to binary. Set DE to point to the start	of
					;			     the line. Leading blanks are ignored. The value is
					;			     ended by a	blank or null (0). DE is returned pointing
					;			     to	the next position. If the value	is invalid (not
					;			     0-9, A-F, or >FFFFH), then	the Carry flag is set, and
					;			     DE	points to the invalid character. The resulting
					;			     value is placed in	NUMV (0C21-0C22)
		ld	a, (hl)
		or	a
		jr	z, loc_1C51
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
		jr	nz, loc_1C3A
loc_1C51:		ld	bc, (ARGX)	; Last command letter entered.
		ld	hl, (word_F15)
		pop	de
loc_1C59:		ld	a, (hl)
		or	a
		jp	z, loc_1A3C
		inc	hl
		cp	c
		jr	z, loc_1C66
		inc	hl
		inc	hl
		jr	loc_1C59
loc_1C66:		ld	de, 0B8Ah
		call	sub_1621
		ld	e, (hl)
		inc	hl
		ld	d, (hl)
		ld	hl, loc_1BA5
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
loc_1C88:		push	hl
		ld	hl, (CURSOR)	; Position of the cursor.
		ld	(hl), a
		inc	hl
		ld	(CURSOR), hl	; Position of the cursor.
		pop	hl
		ret
sub_1C93:		call	sub_160C
		jp	nz, loc_1BA5
		call	sub_1600
sub_1C9C:		bit	2, (iy+0)
		jr	z, loc_1CC8
		ex	de, hl
		ld	hl, 0BBAh
		bit	1, (iy+0)
		jr	z, loc_1CAF
		ld	hl, 0BCFh
loc_1CAF:		call	sub_148F
		ex	de, hl
		call	sub_161E
		ld	de, 0B8Ah
		ld	a, (de)
loc_1CBA:		call	sub_1D19
		inc	de
		ld	a, (de)
		or	a
		jp	p, loc_1CBA
		call	sub_1480
		jr	loc_1CE7
loc_1CC8:		ld	a, (iy+0)
		and	3
		cp	1
		ld	a, (word_F08+1)
		call	nz, sub_1473
		ld	a, (byte_F77)
		sub	1
		jr	c, loc_1CE7
		jr	nz, loc_1CE1
		ld	a, (byte_F14)
loc_1CE1:		ld	(byte_F77), a
		call	z, sub_1D0F
loc_1CE7:		ld	de, (word_F12)
		ld	(word_F10), de
		call	sub_1D08
		ld	a, 0Dh
loc_1CF4:					; sub_1D0F-152o ...
		push	af
		push	hl
		push	de
		push	bc
		cp	1Fh
		jr	z, loc_1CFE
		rst	18h		; call the NAS-SYS routines
		ld	h, l
loc_1CFE:		pop	bc
		pop	de
		pop	hl
		cp	1Bh
		jp	z, loc_1BA5
		pop	af
		ret
sub_1D08:		rst	18h
		ld	h, d
		ret	nc
		cp	1Bh
		jr	z, loc_1D13
;
sub_1D0F:	rst	8
		cp	1Bh
		ret	nz
loc_1D13:		ld	hl, (unk_F7A)
		push	hl
		jr	loc_1CE7
sub_1D19:		call	outch
		jr	sub_1D08
sub_1D1E:		call	nas_outch
		jr	sub_1D08

; "W" The "W" editor command is identical in operation to the "A" editor
; command, except that only those lines containing errors are output in
; the assembly listing.
kdo_w:		ld	a, (iy+1)
		and	24h ; '$'
		or	11h
		ld	(iy+1),	a

; The assembler is entered from the editor by using the editor command
; "A" ("A" is a mnemonic for Assemble).
kdo_a:		ld	hl, loc_1F80
		ld	(word_F78), hl
		ld	hl, 0FFFFh
		ld	(word_F0E), hl
		pop	hl
		pop	hl
		ld	l, 1
		push	hl
		call	sub_1466
		ld	(word_F0C), hl
		push	hl
		call	sub_1DB5
		pop	hl
		ld	(word_F0C), hl
		pop	hl
		push	hl
		bit	7, l
		jr	z, loc_1D56
		bit	4, h
		jr	z, loc_1DB2
loc_1D56:		bit	3, h
		call	nz, sub_146F
		pop	hl
		ld	a, h
		and	5
		or	2
		ld	l, a
		push	hl
		call	sub_1C9C
		bit	0, l
		jr	nz, loc_1D83
		call	sub_14AA
		rst	28h		; Output the string
		db "Source Listing",0
		call	sub_1C9C
		call	sub_1C9C
loc_1D83:		ld	hl, 909h
		bit	6, (iy+1)
		jr	z, loc_1D8F
		ld	hl, 4141h
loc_1D8F:		ld	(word_F7C), hl
		push	hl
		call	sub_1DB5
		bit	7, (iy+1)
		call	nz, sub_1567
		ld	a, 61h ; 'a'
		bit	6, (iy+0)
		call	nz, sub_1B1D
		ld	a, 50h ; 'P'
		bit	7, (iy+0)
		call	nz, sub_1B1D
		call	sub_1852
loc_1DB2:		jp	loc_1BA5
sub_1DB5:		call	sub_1435
		push	de
		pop	ix
		call	sub_15DE
		dec	hl
		ld	(word_F89), hl
		inc	hl
loc_1DC3:		call	sub_162B
		ld	de, 0B98h
		call	sub_1621
		call	sub_160C
		ret	nz
		push	af
		jr	nc, loc_1DDE
		push	hl
		ld	hl, (word_F0C)
		ld	(ix+0),	l
		ld	(ix+1),	h
		pop	hl
loc_1DDE:		cp	3Bh ; ';'
		jr	z, loc_1DEA
		ld	b, 7
		call	sub_1682
		call	z, sub_167D
loc_1DEA:		push	af
		call	sub_1600
		pop	af
		pop	de
		push	hl
		push	de
		ex	af, af'	;'
		pop	af
		push	af
		ld	hl, 0
		push	hl
		push	hl
		push	hl
		ex	af, af'	;'
		ld	a, 10h
		jp	nz, loc_1B19
		ex	af, af'	;'
		jr	nc, loc_1E0F
		ld	hl, 0B9Dh
		call	sub_14C2
		ld	a, 31h ; '1'
		jp	nc, loc_1B19
loc_1E0F:		cp	3Bh ; ';'
		jp	z, loc_1F68
		ld	de, 0B8Ah
		ld	hl, (word_F0C)
		call	sub_169B
		ld	hl, (word_F17)
		push	hl
		ld	d, 0
		push	de
		ld	hl, 0BA3h
		jr	loc_1E31
loc_1E29:		call	sub_1306
loc_1E2C:		ld	a, 20h ; ' '
loc_1E2E:		jp	c, loc_1B19
loc_1E31:		call	sub_164C
		jr	c, loc_1E29
		call	sub_1693
		bit	7, (iy-12h)
		jp	z, loc_1EFA
		call	sub_139F
		jr	c, loc_1E2C
		push	af
		cp	80h ; 'Ç'
		bit	3, c
		ld	a, 40h ; '@'
		jp	nc, loc_1EDB
		jr	nz, loc_1E2E
		pop	af
		push	af
		set	2, (iy-0Ah)
		ld	bc, 0BD0h
		cp	3
		jr	z, loc_1E9E
		jr	nc, loc_1EB5
		ld	b, 5
		call	sub_16A7
		ccf
loc_1E66:		ld	a, 40h ; '@'
		jp	nc, loc_1B19
		ld	h, d
		ld	l, e
		pop	af
		cp	1
		jr	z, loc_1E79
		jr	c, loc_1E80
		ld	bc, (word_F0C)
		add	hl, bc
loc_1E79:		ld	(word_F0C), hl
		res	3, (iy+0)
loc_1E80:		bit	0, (iy-8)
		jr	z, loc_1E90
		cp	2
		jr	z, loc_1E90
		ld	(ix+0),	l
		ld	(ix+1),	h
loc_1E90:		ex	de, hl
		cp	1
		ld	de, 0B8Ah
		call	z, sub_1621
		rst	18h		; call the NAS-SYS routines
		DB 	66h		; TBCD3
					; Output the value in the HL register in ASCII,	followed by a space. Also add H	and L into the C register. The A register is modified.
loc_1E9B:	jp	loc_1F68

loc_1E9E:		ld	d, (hl)
loc_1E9F:		push	bc
		call	sub_164C
		pop	bc
		cp	d
		jr	z, loc_1ECB
		cp	0A0h ; 'á'
		jr	z, loc_1ECB
		ld	(bc), a
		inc	bc
		jr	loc_1E9F
loc_1EAF:		call	sub_13AD
loc_1EB2:		pop	af
		jr	nc, loc_1ECB
loc_1EB5:		push	bc
		ld	b, 4
		call	sub_16A7
		pop	bc
		push	af
		ld	a, e
		ld	(bc), a
		inc	bc
		bit	0, (iy-0Fh)
		jr	z, loc_1EAF
		ld	a, d
		ld	(bc), a
		inc	bc
		jr	loc_1EB2
loc_1ECB:		ld	hl, 0BD0h
loc_1ECE:		or	a
		sbc	hl, bc
		add	hl, bc
		jr	nc, loc_1E9B
		ld	a, (hl)
		inc	hl
		call	sub_17AF
		jr	loc_1ECE
loc_1EDB:		jr	z, loc_1E66
		pop	af
		cp	81h ; 'ü'
		jr	nc, loc_1EEA
		ld	hl, (word_F0C)
		ld	(word_F0E), hl
		jr	loc_1E9B
loc_1EEA:		call	sub_162B
		jr	loc_1E9B
loc_1EEF:		call	sub_16A5
		ld	a, b
		call	sub_1306
loc_1EF6:		ld	a, 21h ; '!'
		jr	c, loc_1F4C
loc_1EFA:		bit	3, c
		jr	z, loc_1EEF
		call	sub_139F
		jr	c, loc_1EF6
		pop	bc
		pop	hl
		bit	2, c
		jr	z, loc_1F19
		dec	hl
		dec	hl
		bit	5, (iy+1)
		jr	nz, loc_1F19
		push	de
		ld	de, (word_F0C)
		sbc	hl, de
		pop	de
loc_1F19:					; sub_1DB5+15Aj
		ex	de, hl
		ld	a, e
		bit	2, c
		call	nz, sub_13B1
		bit	1, c
		call	nz, sub_13AD
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
		jr	z, loc_1F40
		push	af
loc_1F40:		bit	4, l
		jr	z, loc_1F5E
		ld	a, h
		bit	6, e
		jr	z, loc_1F51
		or	a
loc_1F4A:		ld	a, 22h ; '"'
loc_1F4C:		jp	nz, loc_1B19
		jr	loc_1F56
loc_1F51:		pop	bc
		bit	0, l
		push	af
		push	bc
loc_1F56:		ld	a, 0DDh	; '¦'
		or	l
		push	af
		bit	4, e
		jr	nz, loc_1F4A
loc_1F5E:		ld	a, 0EDh	; 'Ý'
		bit	4, e
loc_1F62:		call	nz, sub_17AF
		pop	af
		jr	nc, loc_1F62
loc_1F68:					; sub_1DB5:loc_1E9Bj
		bit	0, (iy+0)
		call	z, sub_1C9C
		bit	3, (iy-0Ah)
		jr	nz, loc_1F84
		bit	6, (iy-0Ah)
		ld	a, 23h ; '#'
		call	nz, sub_1B1D
		jr	loc_1F89
loc_1F80:		bit	3, (iy-0Ah)
loc_1F84:		ld	a, 30h ; '0'
		call	nz, sub_1B1D
loc_1F89:		ld	sp, unk_FF6
		pop	af
		jr	nc, loc_1F93
		inc	ix
		inc	ix
loc_1F93:		pop	hl
		call	sub_1D08
		jp	loc_1DC3
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
sub_1FB5:		ld	(byte_FFE), a
		ld	hl, 0BBAh
		ld	b, 10h
loc_1FBD:		ld	(hl), a
		inc	hl
		djnz	loc_1FBD
		ret
sub_1FC2:		ld	(0FFFh), a
		ld	a, (byte_F14)
		ld	(byte_F77), a
		ret
kdo_dpoint:		rst	28h		; Output the string
		db "Command?",0Dh,0
		rst	18h		; call the NAS-SYS routines
		db 63h			; INLIN
					; Obtain an input line.
		ld	bc, ARGX	; Last command letter entered.
		ld	a, (de)
		cp	20h ; ' '
		ret	z
		cp	41h ; 'A'
		jr	c, loc_1FF1
		cp	5Bh ; '['
		jr	nc, loc_1FF1
		ld	(bc), a
		ld	(ARGC),	a	; Command letter or routine number last	processed for command execution	or input/output.
		inc	de
		rst	18h		; call the NAS-SYS routines
		db 79h			; RLIN
					; Examine an input line	and convert up to ten hexadecimal values separated by spaces from ASCII	to binary.
		jr	nc, loc_1FF4
loc_1FF1:		rst	18h		; call the NAS-SYS routines
		db 6Bh			; ERRM
					; Output the message "Error" followed by a CR.
		ret
loc_1FF4:		rst	18h		; call the NAS-SYS routines
		db 60h			; ARGS
					; Load the contents of ARGI into HL, ARG2 into DE and ARG3 into	BC. ARGI, 2 and	3 are the first	three values entered after a NAS-SYS command.
		rst	18h		; call the NAS-SYS routines
		db 5Ch			; SCALJ
					; CALL the routine number at address ARGC
		ret
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
; end of "ROM"
		end
