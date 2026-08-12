; reass. VPohlers 2026 / last 30.03.2026 

; VERSION c:\MyUser\hobby3\ac1-2010\edas\picoedas\picoEDAS4.z80
; R. Weidlich Anpassung an MONX + kleine Erweiterungen

		cpu	z80

		;include	ac1-acc.asm
		include	ac1-monx.asm
;System
; ARG1:
; ARG2:
; ARG3:
; GETCO1:
; MS30:
; OUTHEX:
; OUTHL:
; TASTE:
; cupos:
; data:

; EDAS arbeitet viel mit fixen Adressen im BWS-Speicher!
; Beachte, dass beim AC1 der BWS-Speicher rückwärts zu nutzen ist.


unk_1802:	equ	1802h		; Adr. Sprungverteiler Monitor


		org 4000h		; picoEDAS4
		;org C000h		; picoEDASC

loc_4000:	jp	COLD
		jp	WARM
; Defaultwerte
; Bereich wird nach 5500h kopiert (30h Bytes)
off_4006:	dw txtbuffer		; BUFBEG	Editierpufferanfang
		dw 0BFFFh		; BUFEND	Editierpufferende
		;;jp	MS30		; unk_5504	JP Druckerausgabe (Zeichen in A)
		jp 081Ch
;unk_400D-unk_543F unveränderlicher Programmbereich (CHKSUM)
unk_400D:	db    0              	; byte_5507	Assembleroptionen
		db    0                 ; byte_5508	Ausgabeverzögerung Druck
		db    0			; byte_5509	Ausgabeverzögerung BWS
		dw    0                 ; mc_ofs	offset for code generation
		dw 0FFFFh               ; word_550C	MC-Adr.
		dw 0FFFFh               ; word_550E	ENT-Adresse
		dw    0                 ; word_5510	Zeilennummer
		dw    0                 ; word_5512	ausgegebene Zeilennummer
		db  25                  ; byte_5514	Seitengröße auf Bildschirm
		dw kdotab               ; unk_5515
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

kdotab:		db 'V'
		dw VIEW
		db 'M'
		dw MARK
		db 'Q'
		dw QUIT
		db 'R'
		dw RENUM
		db 'O'
		dw OPTIONS
		db 'A'
		dw ASSEMBLE
		db 'F'
		dw FIND
		db 'X'
		dw KILL
		db 'Z'
		dw CORRECT
		db 'I'
		dw INUM
		db 'P'
		dw PUT
;;		db 'Q'
;;		dw loc_4B2E			; error 99 illegal command
		db 'H'
		dw HEIGHT
		db '+'
		dw SETOPT
		db '-'
		dw RESOPT
		db 'W'
		dw WERRORS
		db 'J'
		dw DELAY
		db 'K'
		dw PDELAY
		db 'G'
		dw GO
		db 'Y'
		dw REUSE
		db '/'
		dw SEARCH
		db 'N'
		dw NNUM
		db 'C'
		dw MOVE
		db 'T'
		dw TRANS
		db 'S'
		dw SEC
		db 'E'
		dw SWAP
		;
		db  '?'
		dw E_HELP
		db  'l'
		dw E_LOAD
		db  's'
		dw E_SAVE
		db  'B'
		dw E_MCSAV
		db  'D'
		dw E_BUFEND
		db    0
;

		;dbr "FREE:      END:      SEC:"
		db ':TPO     :CES     :DNE          :IERF'
unk_409D:	equ	$-1
		dbr	"ZZZZZZZ"
unk_40A4:	equ	$-1
;
		db  20h
		db 0FFh
		db 0FFh

; Bereich unk_40A8..unk_42F4 entspricht unverändert ZEAP20
;---------------------------------------
;
;;unk_40A8:	db  75h
;;		db    0

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
		or	7Fh ; ''
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
;orig nas_outch (3 Byte)
;;		jp	loc_4B2E	; error 99 illegal command

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
sub_43EB:	ld	hl, 0
loc_43EE:	ld	a, (de)
		sub	30h ; '0'
		ret	c		; Ende,	wenn keine Dezimalziffer
		cp	0Ah ; .. '9'
		ret	nc		; Ende,	wenn keine Dezimalziffer
		push	de
		ld	d, h	; HL*10
		ld	e, l
		add	hl, hl
		add	hl, hl
		add	hl, de
		add	hl, hl
		ld	d, 0
		ld	e, a
		add	hl, de
		pop	de
		dec	de		; nächste Stelle (BWS), deshalb AC1 dec
		jr	loc_43EE

;---------------------------------------
; Parameter f. RENUM und INUM, Defaults 10
sub_4404:	ld	hl, (ARG1)	; erste Zeile
		ld	a, (byte_559F)	; Anzahl Kdo.Argumente
		cp	2
		ld	a, 10h		; Defaultwert Schrittweite 10
		jr	nz, loc_4413
		ld	a, (ARG2)
loc_4413:	ld	(byte_5584), a	; Schrittweite
		or	a		; Schrittweite=0 ?
		ld	a, 6		; Error 06 - Inkrement ist 0
		jp	z, loc_4B30	; Fehler ausgeben
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
sub_4435:	push	hl
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
; Ende orig ZEAP20
;---------------------------------------

;;		db    0
;;		db    0
;;		db  3Eh	; >
;;		db 0FFh

sub_4473:	or	a
		ret	z
		push	bc
		ld	b, a
loc_4477:	xor	a
		rst	20h		; MS30
		call	sub_4D22	; ggf. Abbruch bei Stop
		djnz	loc_4477
		pop	bc
		ret

sub_4480:	ld	a, 0Dh
		call	sub_4D34
		ld	a, (byte_5508)	; Ausgabeverzögerung Druck
		jr	sub_4473
;;		db    0
;;		db    0

;---------------------------------------
;;loc_448C:	jp	QUIT

;---------------------------------------
; Prüfsumme über Programm
; Schutz vor versehenlichen Überschreiben
chksum:		ld	hl, unk_400D
		ld	de, unk_543F
		xor	a
chksum1:	xor	(hl)
		inc	hl
		sbc	hl, de
		add	hl, de
		jr	c, chksum1
		xor	(hl)
		ret	z
;;		ld	a, 90h 		; checksum error
;;		call	error		; Ausgabe "Error "
		;
		rst     18h
		db 'EDAS defekt!',8Dh
		;
;;		jr	loc_448C
		jp	QUIT
;---------------------------------------
;
sub_44A6:	ld	a, 20h ; ' '
		ld	(hl), 0A0h ; 'á'
		inc	hl
		xor	(hl)
		and	7Fh ; ''
		jr	z, sub_44A6
		push	hl
loc_44B1:	xor	a
		inc	hl
		xor	(hl)
		jr	z, loc_44BF
		ld	a, 0A0h	; 'á'
		xor	(hl)
		jr	nz, loc_44B1
		ld	(hl), 20h ; ' '
		jr	loc_44B1
loc_44BF:	pop	hl
		ret

;---------------------------------------
; Ausgabe "AC1 U880 Assembler"
loc_44C1:	rst	18h		; ed_prnst
		db "AC1 U880 Assembler -",0A0h
;;		nop
		ret

;---------------------------------------
;
sub_44D9:	push	bc
		inc	hl
loc_44DB:	push	hl
		ld	de, (word_5519)	; unk_428F Parse Tree Register
		push	de
		ld	d, 0
		push	de
loc_44E4:	call	sub_48BE	; nächstes Zeichen
		bit	6, c
		jr	z, loc_44F8
		call	sub_4306	; Abarbeitung nächstes Token
		jr	nc, loc_44E4
loc_44F0:	call	sub_4435	; Textende ermitteln
		ld	hl, (word_5589)
		jr	loc_4514
;
loc_44F8:	call	sub_439F
		jr	c, loc_44F0
		bit	7, e
		jr	z, loc_453A
loc_4501:	ld	de, (word_550C)	; MC-Adr.
		jr	loc_453A
loc_4507:	call	sub_456C
		jr	c, loc_4519
		inc	de
		inc	de
		call	sub_453D
		jr	z, loc_452C
		pop	bc
loc_4514:	pop	bc
		push	bc
		push	bc
		jr	loc_4507
;
loc_4519:	pop	hl
loc_451A:	call	sub_48BE
		bit	6, c
		jr	nz, loc_451A
		ld	de, 0
		set	3, (iy-0Ah)	; 55F4 ??
		scf
		sbc	a, a
		jr	loc_453A
;
loc_452C:	push	ix
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
loc_453A:					; sub_48B1-3ACj ...
		pop	bc
		pop	bc
		ret

sub_453D:	ld	b, h
		ld	c, l
		pop	hl
		ex	(sp), hl
		push	bc
		ex	(sp), hl
loc_4543:	ex	(sp), hl
		call	sub_48BE
		ex	(sp), hl
		bit	6, c
		jr	z, loc_455C
		inc	hl
		cp	(hl)
		jr	z, loc_4543
		push	af
		call	loc_4664	; Zeichenklasse ermitteln
		pop	af
		bit	6, c
		jr	nz, loc_4565
		or	c
		jr	loc_4565
loc_455C:	call	sub_48BE
		xor	a
		bit	6, c
		jr	z, loc_4565
		sub	c
loc_4565:	ex	(sp), hl
		pop	bc
		ex	(sp), hl
		push	hl
		ld	h, b
		ld	l, c
		ret
sub_456C:					; sub_456C+Dj ...
		call	loc_442D		; Suche nächste Zeile
		push	de
		call	sub_464F		; Zeilennummer holen
		pop	de
		ret	c			; wenn Bereichsüberschreitung
		inc	hl
		call	sub_48BE
		jr	nc, sub_456C
		dec	hl
		or	a
		ret

;---------------------------------------
; Ausgabe Symboltabelle
loc_457E:	call	sub_4CB6
		call	loc_44C1	; Ausgabe "AC1 U880 Assembler"
		rst	18h		; ed_prnst
;;		db "Symbol Tabl",0E5h
		db "Symboltabell",0E5h
;;		nop
		call	sub_4CB6
		call	sub_4CB6
		ld	de, unk_40A4	; Text "ZZZZZZ"
loc_459B:	ld	hl, unk_409D	; Text "FREE: END: SEC:"
		push	hl
		push	hl
		ld	hl, (word_5589)
loc_45A3:	call	sub_456C
		jr	c, loc_45C0
		push	hl
		push	de
		call	sub_453D
		pop	bc
		pop	bc
		jr	nc, loc_45A3
		pop	hl
		push	hl
		push	bc
		push	bc
		call	sub_453D
		pop	hl
		pop	bc
		jr	nc, loc_45A3
		pop	af
		push	bc
		jr	loc_45A3
loc_45C0:		pop	hl
		pop	bc
		or	a
		sbc	hl, bc
		add	hl, bc
		jp	z, sub_4CB6
		push	hl
		push	hl
		inc	hl
		call	sub_48B1
		pop	hl
		jr	z, loc_45F2
		push	hl
		ex	de, hl
		rst	28h		; ed_outhls
;;		nop
		rst	18h		; ed_prnst
		db 8,'H',0A0h
;;		nop
		pop	hl
		dec	hl
		call	sub_4623	; Ausgabe Zeilennummer
loc_45E0:		ld	b, 7
		call	sub_4699
		ld	hl, -1027h
		add	hl, de
		ld	de, 1027h
		call	c, setcu	; set cursor
		call	nc, sub_4CB6
loc_45F2:	pop	de
		jr	loc_459B


;---------------------------------------
; suche Zeile ARG1
; ret HL = Zeilenanfang
sub_45F5:	ld	hl, (BUFBEG)	; Anfangsdresse
loc_45F8:	inc	hl
		inc	hl
loc_45FA:	call	sub_442B	; Suche nächste Zeile
		ret	z		; Textende erreicht
		ld	e, (hl)
		inc	hl
		ld	d, (hl)		; DE=Zeilennummer
		dec	hl
		push	hl		; Zeilenanfang
;;		nop
;;		nop
;;		nop
;;		nop
;;		nop
;;		nop
;;		nop
;;		nop
		ld	hl, (ARG1)	; wird ARG1 überschritten?
		or	a
		sbc	hl, de
		pop	hl
		ccf
		ret	nc		; schon größer
		ret	z		; gefunden
		jr	loc_45FA

;---------------------------------------
; Patch zu loc_4664 Zeichenklasse ermitteln
; komprimierte Leerzeichen
loc_4617:	or	a		; 00?
		jr	z, loc_467C
		cp	' '		; komprimierte Leerzeichen?
		jr	nc, loc_467C	; nein
		ld	a, ' '		; ja, als Leerzeichen auswerten
		jr	loc_467C
;;		db    0

;---------------------------------------
; Ausgabe Zeilennummer
sub_4623:	call	sub_464F	; ret DE=Zeilennummer
		ret	c		; wenn Bereichsüberschreitung
		push	hl		; Zeilenanfang
		ex	de, hl		; HL=Zeilennummer
		rst	28h		; ed_outhls Ausgabe HL
;;		nop
		ld	(word_5512), hl	; Zeilennummer merken
		pop	hl		; Zeilenanfang
		inc	hl
		call	sub_48BE
		bit	7, a
; set cursor bws(31,0)
sub_4635:	ld	de, bws(31,0)
;---------------------------------------
; set cursor
; ret de=alte Cursorposition
setcu:		push	hl
		ld	hl, (cupos)
		ex	de, hl
		ld	(cupos), hl
		pop	hl
		ret

;---------------------------------------
;die letzten beiden BWS-Zeilen leeren
sub_4642:	ld	de, 1000h
		ld	b, 7Fh
; ab DE Bx Leerzeichen
sub_4647:	ld	a, ' '
loc_4649:	inc	de
		ld	(de), a
		djnz	loc_4649
		jr	setcu		; set cursor

; Zeilennummer holen
; ret DE=Nummer, Cy=1 bei Ende oder Bereichsüberschreitung
sub_464F:	ld	a, (hl)		; Test auf FF
		add	a, 1		; wenn A=FF war, dann Cy=1
		sbc	a, a		;
		ret	c		; bei FF
		ld	e, (hl)
		inc	hl
		ld	d, (hl)		; DE=Zeilennummer
		dec	hl
		push	hl		; HL=Zeilenanfang
		ld	hl, (ARG2)	; FFFF
		sbc	hl, de
		pop	hl
		ret

;---------------------------------------
; Zeichen (HL) ausgeben
sub_4660:	ld	a, (hl)
; Zeichen (A) ausgeben
sub_4661:	ld	(de), a		; schreiben in BWS
		dec	de		; Cursor weiter
;
sub_4663:	inc	hl
;---------------------------------------
;ret: C = Zeichenklasse
;	C0: A..Z
;	E0: 0..9
;	98: 0A0h ; Ende-Marker
;	Spezialzeichen s. unk_551D
; 	bit 6=1 bei ..
; 	bit 4=1 bei Kommentar, Zeilenende oder Ende-Marker
;	bit 0=1 bei Trenner (Leerzeichen, Komma)
loc_4664:	ld	a, (hl)
		cp	30h ; '0'
		jr	c, loc_4617	; < 30h
		cp	41h ; 'A'
		jr	c, loc_4677	; '0'..'@'
		ld	c, 0C0h
		cp	5Bh ; '['	; 'A'..'Z'
		ret	c
		ld	c, 98h
		cp	0A0h 		; Ende-Marker
		ret	z
loc_4677:	ld	c, 0E0h
		cp	3Ah ; ':'	; '0'..'9'
		ret	c

;---------------------------------------
;
loc_467C:	push	hl
		and	7Fh
		ld	(unk_552D), a	; Zeichen auch in Tabelle eintragen
					; damit die Fkt. determiniert
		ld	hl, unk_551D	; Tabelle Zeichenklassen Spezialzeichen
loc_4685:	ld	c, (hl)		; Zeichenklasse
loc_4686:	inc	hl
		bit	7, (hl)
		jr	nz, loc_4685
		cp	(hl)		; Vgl. Zeichen
		jr	nz, loc_4686
		pop	hl
		bit	4, c		; bei Kommentar, Zeilenende oder Ende-Marker
		ret	nz
		scf			; Cy=1,wenn Bit4=0
		ret

;---------------------------------------
;
sub_4694:	bit	3, c
		ret	nz
		ld	b, 5
;
; in B = Anz. Zeichen
sub_4699:	bit	4, c		; Kommentar, Zeilenende oder Ende-Marker?
		jr	nz, loc_46A4	; ja
		call	sub_4660	; Zeichen (HL) ausgeben
		djnz	sub_4699
		inc	b		; B=1
		ret
loc_46A4:	dec	de		; cupos
		djnz	loc_46A4
		call	loc_4664	; Zeichenklasse ermitteln
loc_46AA:	bit	0, c		; Trenner (Leerzeichen, Komma) ?
		ret	z		; nein
		call	sub_48BE
		jr	loc_46AA

;---------------------------------------
; Ausgabe HL, wenn <> FFFF
; in DE=cupos
sub_46B2:	ld	a, h
		and	l
		inc	a
		ret	z
		call	setcu		; set cursor
		rst	28h		; ed_outhls
;;		nop
		ret

;---------------------------------------
;in HL Ende erster Parameter (in BWS)
sub_46BC:	ld	b, 0
sub_46BE:	push	hl
		ld	h, (iy-0Ah)
		ld	l, 70h ; 'p'
		ex	(sp), hl	; Wert auf Stack, HL restaurieren
		ld	de, 0
		call	loc_4664	; Zeichenklasse ermitteln
		cp	28h ; '('
		jr	nz, loc_46FD
		ex	(sp), hl
		bit	2, h
		jr	nz, loc_474C
		set	2, h
		inc	l
; nächsten Parameter bearbeiten
loc_46D7:	res	3, b
loc_46D9:	ex	(sp), hl
		call	sub_48BE
		jr	c, loc_46FD
		ex	(sp), hl
		ld	(iy-0Ah), h
		bit	4, b
		jr	nz, loc_46F1
		bit	1, b
		jr	nz, loc_46F8
		ld	(word_55F2), de
		jr	loc_46F8
loc_46F1:	ld	a, e
		call	sub_43B1
		ld	(iy-9), e
loc_46F8:	ld	b, l
		pop	hl
		jp	loc_48DE
;
loc_46FD:	ld	(word_5582), de
		bit	5, c		; Zeichenklasse
		jr	z, loc_473D
		cp	22h ; '"'
		jr	nz, loc_4718
		call	sub_48BE
		ld	d, 0
		ld	e, a
		cp	0A0h ; 'á'
		jr	nz, loc_473A
		ld	e, 20h ; ' '
		inc	hl
		jr	loc_473A
loc_4718:	ex	de, hl
		cp	23h ; '#'
		jr	z, loc_4732
		push	de
		rst	30h		; ed_konvx
;;		nop
		ld	a, (de)		; Zeichen nach Ziffern
		cp	'H'		; folgt suffix 'H'?
		jr	nz, loc_472C	; nein, dezimal
		pop	hl
		ld	hl, (data+1)	; Wert
		dec	de		; nächstes Zeichen (BWS)
		jr	loc_4738
loc_472C:	pop	de
		call	sub_43EB	; Ascii Dez -> Hex
		jr	loc_4738
loc_4732:	dec	de
		rst	30h		; ed_konvx
;;		nop
		ld	hl, (data+1)
loc_4738:	ex	de, hl		; DE=num. Parameter
		inc	hl		; vorheriges Zeichen (BWS)
loc_473A:	ex	(sp), hl
		jr	loc_4795
loc_473D:	bit	6, c
		jr	z, loc_47B5
		call	sub_44D9
		inc	hl
		ex	(sp), hl
		jr	nz, loc_4786
		bit	1, b
		ld	a, 24h 		; Error 24 - zu viele Register
loc_474C:	jr	nz, loc_47C3	; Fehler ausgeben
		bit	2, b
		jr	nz, loc_47C3	; Fehler ausgeben
		set	1, b
		ld	a, d
		cp	62h ; 'b'
		jr	nz, loc_477A
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
		jr	nz, loc_4773
		set	7, h
		or	h
		ld	h, a
		jr	loc_477A
loc_4773:	xor	h
		and	30h ; '0'
		ld	a, 25h 		; Error 25 - unzulässige Operandenkombination
		jr	nz, loc_47C3	; Fehler ausgeben
loc_477A:	ld	a, l
		and	1
		or	d
		ld	l, a
		ld	de, (word_5582)
loc_4783:	jp	loc_46D7
;
loc_4786:	jr	nc, loc_4795
		ld	a, (iy-0Ah)
		and	8
		or	h
		ld	h, a
		bit	0, b
		ld	a, 41h 		; Error 41 - illegale Vorwärtsreferenz
		jr	nz, loc_47C3	; Fehler ausgeben
loc_4795:
		push	hl
		bit	3, b
		ld	hl, (word_5582)
		jr	nz, loc_47A0
		add	hl, de
		jr	loc_47A3
loc_47A0:	or	a
		sbc	hl, de
loc_47A3:	ex	de, hl
		pop	hl
		bit	1, b
		jr	z, loc_4783
		bit	4, b
		ld	a, 27h 		; Error 27 - illegaler Operand
		jr	z, loc_47C3	; Fehler ausgeben
		bit	0, l
		jr	z, loc_47C3	; Fehler ausgeben
		jr	loc_4783
loc_47B5:	set	3, b
		ex	(sp), hl
		cp	2Dh ; '-'
		jp	z, loc_46D9
		bit	1, c
		jr	nz, loc_4783
		ld	a, 26h 		; Error 26 - illegales Zeichen
; Fehler ausgeben (fwd)
loc_47C3:	jp	loc_4B30	; Fehler ausgeben

;---------------------------------------
; Ausgabe MC
sub_47C6:	exx
		ld	hl, (word_550C)	; MC-Adr.
		bit	1, (iy+0)	;
		jp	z, loc_4864	; Byte eintragen
		bit	3, (iy+1)	; Assembleroptionen
		jr	loc_4809	; ..weiter

;---------------------------------------
; rst30
; fuehrende Leerzeichen ueberlesen,
; letzen vier Zeichen als Hexzahl konvertieren
; und in data ablegen
; entspricht Routine im Monitor
; ret HL = data, data = Anz. Stellen; data+1=word
konvx:		ld	a, (de)
		cp	' '
		dec	de
		jr	z, konvx
		inc	de
		xor	a
konvx1:		ld	hl, data	; Puffer, im Monitor
		ld	(hl), a
		inc	hl
		ld	(hl), a
		inc	hl
		ld	(hl), a
konvx2:		ld	a, (de)
		dec	hl
		dec	hl
		sub	30h ; '0'
		ret	m
		cp	0Ah
		jr	c, konvx3
		sub	7
		cp	0Ah
		ret	m
		cp	10h
		ret	p
konvx3:		dec	de
		inc	(hl)
		inc	hl
		rld
		inc	hl
		rld
		jr	konvx2

;---------------------------------------
; UP zu SWAP
loc_4803:	call	sub_5169
		jp	loc_4BE6

;---------------------------------------
; UP zu sub_47C6
loc_4809:	ld	hl, (word_550C)	; MC-Adr.
		push	hl
		bit	1, (iy+1)	; Assembleroptionen
					; "MC in Speicher laden"
		jr	z, loc_483E	; nein
		ld	de, (mc_ofs)	; offset for code generation
		add	hl, de
		bit	6, (iy+0)
		jr	nz, loc_483E
		;;ld	bc, RAMA	; Bereich loc_5400..unk_5600
		LD	BC, word_54B0
		ld	de, unk_5600
		call	sub_48F5	; testen
		ld	bc, loc_4000	; Bereich loc_4000..loc_53FC+3
		;;ld	de, loc_53FC+3
		ld	de, unk_6000
		call	sub_48F5	; testen
		ld	bc, (BUFBEG)	; Anfangsdresse
		push	hl
		call	sub_4466	; Bufferende
		ex	de, hl
		pop	hl
		call	sub_48F5	; testen
		ld	(hl), a		; Code in Speicher schreiben
loc_483E:	bit	0, (iy+0)
		jr	nz, loc_4863
		ld	hl, (cupos)
		jp	loc_48C8
;;		db    0


;
loc_484B:	jr	nc, loc_4860
		push	af
		call	sub_4CB6
		ld	de, bws(31,6)	; 1039h
		ld	b, 46h ; 'F'
		call	sub_4647
		ld	de, bws(30,5)	; 107Ah
		call	setcu		; set cursor
		pop	af
loc_4860:	call	OUTHEX
loc_4863:	pop	hl
; Byte eintragen
loc_4864:	inc	hl
		ld	(word_550C), hl	; MC-Adr.
		exx
		ret

; Fehlerzähler rücksetzen
sub_486A:	ld	hl, errcnt		; Fehlerzähler
		xor	a
		ld	(hl), a
		inc	hl
		ld	(hl), a
		ld	hl, 909h		; Defaultwert f. word_557C
		ret

; Fehlerzähler um 1 erhöhen (BCD)
sub_4875:	ld	hl, (errcnt)		; Fehlerzähler
		or	a
		ld	a, l
		inc	a
		daa
		jr	nz, loc_4880
		xor	a
		inc	h
loc_4880:	ld	l, a
		ld	(errcnt), hl		; Fehlerzähler
		ret

;---------------------------------------
; Ausgabe "Error "
error:		rst	18h		; ed_prnst
		db "Fehler",0A0h
		ret

;---------------------------------------
; Ausgabe Anz. Fehler
loc_488D:	ld	hl, (errcnt)	; Fehlerzähler
		push	de
		call	sub_4CB6
		ld	de, bws(30,8)
		call	setcu		; set cursor
		pop	de
		call	sub_48B6	; Ausgabe "******"
		rst	28h		; ed_outhls
					; Ausgabe Fehlerzähler
		call	error		; Ausgabe "Error "
		dec	hl
		ld	a, l
		or	h		; Fehlerzähler = 1?
		jr	z, loc_48AB	; ja
;;		rst	18h		; ed_prnst
;;		db 8,'s',0A0h		; sonst Plural "Errors "
loc_48AB:	call	sub_48B6	; Ausgabe "******"
		jp	sub_4CB6

;---------------------------------------
; Patch zu
sub_48B1:	push	bc
		dec	hl
		jp	loc_44DB

;---------------------------------------
; Ausgabe "******"
sub_48B6:	rst	18h		; ed_prnst
		db "*****",0A0h
		ret

;---------------------------------------
; nächstes Zeichen
sub_48BE:	; test auf BWS Ende
		ld	a, 17h		; BWS
		sub	h		; HL=cupos
		jp	c, sub_4663	; wenn BWS unterschritten
					; dann ein Zeichen zurück
					; und Zeichenklasse ermitteln
sub_48C4:	dec	hl		; sonst ein Zeichen vor
		jp	loc_4664	; und Zeichenklasse ermitteln

;---------------------------------------
;
loc_48C8:	ld	de, 1074h	; bws(30,11)
		or	a
		sbc	hl, de
		jp	loc_484B
;
sub_48D1:	push	af
loc_48D2:	ld	a, (hl)
		ld	(de), a
		inc	hl
		dec	de
		dec	c
		jr	nz, loc_48D2
		pop	af
		ld	hl, data+2
		ret

;
loc_48DE:	call	loc_4664	; Zeichenklasse ermitteln
sub_48E1:	bit	0, c		; Trenner (Leerzeichen, Komma) ?
		ret	z		; nein
		call	sub_48BE
		jr	sub_48E1

;---------------------------------------
prnst0:		ex	(sp), hl
prnst1:		ld	a, (hl)
		inc	hl
		rst	10h		; ed_outch0
		bit	7, a
		jr	z, prnst1
		ex	(sp), hl
		ret

;---------------------------------------
;;		db    0
;;		db 0FFh

;---------------------------------------
; Bereichstest HL between BC and DE
sub_48F5:	push	hl
		or	a
		sbc	hl, de
		pop	hl
		ret	nc
		push	hl
		or	a
		sbc	hl, bc
		pop	hl
		ret	c
		; Fehlerfall
		set	6, (iy+0)
		ld	a, 60h 		; Error 60 - memory object code overwriting
		jp	loc_4B30	; Fehler ausgeben

;---------------------------------------
; Reaktivieren eines vorhandenen Quelltextes
; Y - Reaktivieren des Editierpuffers. Hiermit kann ein im Speicher vorhandener
; Quelltext nach einem Kaltstart von EDAS*4 wieder aktiviert werden. Das ist
; notwendig, wenn eine Quelle von Kassette geladen wurde oder wenn nach einem
; eventuellen Systemabsturz die Quelle noch erhalten geblieben ist. Ein
; Reaktivieren ist jedoch nur so lange möglich, wie noch keine Quellzeile
; eingegeben wurde. Außerdem muß natürlich der Anfang des vereinbarten
; Editierpuffers mit dem Anfang des Quelltextes übereinstimmen.

REUSE:		ld	hl, (BUFBEG)	; Anfangsdresse
		inc	hl
		inc	hl		; Länge
		inc	hl
		inc	hl		; Offs
		inc	hl		; Trenner
		ld	a, (hl)		; 1. Zeilennummer bzw. FF
		cp	0FFh		; bei leerem Puffer
		ld	a, 5		; Error 05 - Reaktivieren des Puffers nicht möglich
		jp	nz, sub_4B34	; Fehler ausgeben, wenn nicht FF
		ld	hl, (p_BUFBEG)	; Parameter ARG2 Anfangsadresse
		ld	(BUFBEG), hl	; Anfangsdresse
		ld	de, unk_556F	; Buffersicherung
		ld	bc, 6		; Länge
		ex	de, hl
		ldir			; rückschreiben
		ret
;---------------------------------------
; Assembleroptionen setzen/rücksetzen
; +x - Ergänze Option (nur bitweise).

SETOPT:		ld	a, l
		or	(iy+1)		; Assembleroptionen
		jr	loc_4938

;---------------------------------------
; Assembleroptionen setzen/rücksetzen
; -x - Streiche Option (nur bitweise).

RESOPT:		ld	a, l
		cpl
		and	(iy+1)		; Assembleroptionen
		jr	loc_4938

;---------------------------------------
; Assembleroptionen setzen/rücksetzen
; O x - Setze Assembler-Option mit Maske (1 Byte).
; Bit 0 +01 - Unterdrücke Assemblerlisting; fehlerhafte Zeilen werden trotzdem
; ausgegeben.
; Bit 1 +02 - Maschinenkode in Speicher laden; nur wenn dieses Bit gesetzt ist,
; wird der Maschinenkode im zugehörenden Speicher abgelegt.
; Bit 2 +04 - Assemblerlisting zur Druckerschnittstelle; das Assemblerlisting wird
; zeichenweise der Druckerschnittstelle übergeben.
; Bit 4 + 10 - Erzwinge 2. Lauf bei Fehlern im 1. Lauf; im Normalfall wird bei
; Fehlern im Lauf 1 an dessen Ende der Assembler abgebrochen.
; Bit 5 +20 - Relative Sprungdistanzen sind absolut angegeben; hiermit kann z.B.
; der Sprungangabe bei SYPS K 1520 entsprochen werden (z.B. JR marke-$).
; Bit 7 + 80 - Ausgabe einer Symboltabelle; im Anschluß an den zweiten
; Assemblerlauf wird eine alphabetisch geordnete Liste aller verwendeten Symbole
; erzeugt, die auch deren Werte und die Zeilen, in denen sie definiert wurden,
; enthält.

OPTIONS:	ld	a, l
loc_4938:	;;and	0B7h
		and	0F7h
		ld	(iy+1),	a	; Assembleroptionen
		ld	(byte_5507), a	; Assembleroptionen
		ret
;;		nop
;;		nop

;---------------------------------------
; Seitengröße auf Bildschirm einstellen
; H h - Setzen der Seitengröße auf h (hex.).
; H - Setzen der Seitengröße auf 25 Zeilen.
; H 0 - Setzen auf kontinuierliche Ausgabe.
; Nach der Ausgabe von h Zeilen auf dem Bildschirm wird die Ausgabe unterbrochen
; und nach dem Drücken einer Taste um jeweils eine weitere Seitengröße
; fortgesetzt. Nach dem Start ist die Seitengröße auf 25 Zeilen festgelegt.

HEIGHT:		ld	a, l		; in HL=1.Parameter Hex
		jr	nz, loc_4948
		ld	a, 25
loc_4948:	ld	(byte_5514), a
		ret

;---------------------------------------
; Ansprung des assemblierten Programms
; G - direkter Ansprung des assemblierten Programms zur Adresse, die mit ENT
; markiert wurde. Voraussetzung zur Ausführung dieses Kommandos sind ein
; Assemblerlauf mit gesetztem Bit 1 und eine ENT-Anweisung im Quelltext. Vor
; Ausführung dieses Kommandos empfiehlt es sich, die aktuelle Quelle auf Kassette
; zu sichern, da man nie sicher ist, ob sich nicht doch ein Fehler im Programm
; befindet.

GO:		ld	hl, (mc_ofs)	; offset for code generation
		ld	a, h
		or	l
		jr	nz, loc_4963	; bei offs kein Go
		bit	1, (iy+1)	; Assembleroptionen
		jr	z, loc_4963	; no code, no Go
		ld	hl, (word_550E)	; ENT-Adresse
		inc	hl
		ld	a, h
		or	l
		jr	z, loc_4963	; no ENT Adr.
;;		jr	loc_497A

		call    loc_497A
		ld      hl, 103Fh
		ld      (cupos), hl
		rst     18h
		db '<Taste...'
		db 0BEh
		rst     8
		jp      WARM

loc_4963:	ld	a, 4		; Error 04 -- illegal GO
		jp	loc_4B30	; Fehler ausgeben

;---------------------------------------
; Ausgabeverzögerung setzen
; J h - Setzen einer Schirmausgabeverzögerung (h 1..FF).
; J 0 - Rücksetzen der Verzögerung.

DELAY:		ld	a, l		; in HL=1.Parameter Hex
		ld	(byte_5509), a	; Ausgabeverzögerung BWS
		ret

;---------------------------------------
; K h - Wie J, aber nur bei Druckerausgabe.
; Mit dem "J"- bzw. "K"-Kommando läßt sich die Geschwindigkeit der Bildschirm bzw.
; Druckerausgabe steuern, in dem entsprechend dem Argument h nach jedem cr
; gewartet wird. Nach dem Start sind beide Verzögerungen ausgeschaltet.

PDELAY:		ld	a, l		; in HL=1.Parameter Hex
		ld	(byte_5508), a	; Ausgabeverzögerung Druck
		ret

;???
;;		ld	(byte_5508), hl	; Ausgabeverzögerung Druck
;;		ret

;---------------------------------------
; Maschinenkode um Hexwert verschoben im Speicher ablegen
; P hhhh - Der vom Assembler erzeugte Maschinenkode wird auf den vorgesehenen
; Speicherplatz + hhhh (Offset-Hex) geladen.
; Dieses Kommando muß vor dem entsprechenden Assemblerlauf gegeben werden. Hiermit
; kann man auch Programme assemblieren, die sonst reservierte Speicherbereiche von
; EDAS*4 zerstören würden. Mit dem "T"-Befehl des Monitors sind sie dann auf den
; Speicherplatz zu bringen, auf dem sie laut Listing lauffähig sind.

PUT:		ld	(mc_ofs), hl	; in HL=1.Parameter Hex
					; offset for code generation
		ret

;
loc_497A:	push	hl
		call	sv_restore	; orig.	Sprungverteiler	wiederherstellen
		pop	hl
		dec	hl
		jp	(hl)

;;		db    0
;;		db    0
;;		db    0
;;		db    0
;;		db    0
;;		db    0

;---------------------------------------
; Anzeige des Quelltextes auf dem Schirm
; V - Anzeige des gesamten Quelltextes.
; V a - Anzeige ab Zeile a bis zum Ende.
; V a e - Anzeige ab Zeile a bis Zeile e.
; Hiermit kann man den Quelltext ganz oder teilweise auf dem Schirm ausgeben.

VIEW:		call	sub_45F5	; suche Zeile ARG1 (a)
loc_498A:	call	sub_4CAD	; Anzeige Zeile
		jr	loc_498A

;---------------------------------------
; Neunumerierung der gesamten Quelle
; R - 1. Zeile 10 Schrittweite 10.
; R a - Zeile a Schrittweite 10.
; R a s - Zeile a Schrittweite S.
; Zur Verbesserung der Form bzw. zum Platzschaffen für Einfügungen kann hiermit
; die gesamte Quelle mit neuen Zeilennummern versehen werden.

RENUM:		call	sub_4404	; Parameter f. RENUM und INUM, Defaults 10
		ex	de, hl		; DE = nächste neue Zeilennummer
loc_4993:	ld	hl, (BUFBEG)	; Anfangsdresse
		inc	hl
		inc	hl		; size übergehen
		inc	hl
loc_4999:	call	loc_442C	; Suche nächste Zeile
		ret	z		; Textende
		ld	(hl), e		; neue Nummer eintragen
		inc	hl
		ld	(hl), d
		ex	de, hl
		call	sub_441F	; Nächste Zeilennummer berechnen (BCD)
		ex	de, hl
		jr	nc, loc_4999	; solange Zeilnnummer < 9999
		;
		ld	a, 1		; Error 01 - Zeilennummerüberlauf
		ld	(byte_5584), a	; und Schrittweite auf 1 rücksetzen
		call	sub_4B34	; Fehler ausgeben
		ld	de, 1		; Zeilennummer auf 1 rücksetzen
		jr	loc_4993	; weitermachen

;---------------------------------------
; Zeichenkettensuche
; /STRING/ - Eingabe der zu suchenden Zeichenkette.

SEARCH:		ld	hl, (cupos)	; =  103F bws(31,0) Anfang letzte Zeile
		ld	de, 80h	; 2 Zeilen (2x COLS)
		add	hl, de		; = 10BF bws(29,0) Suchzeichenkette
		ld	a, (hl)		; '/'
		ex	de, hl
		ld	hl, 0FFBFh	; -41H, -65
		add	hl, de		; = bws(30,1)
		ld	bc, 43h		; 63
		cpir			; suche Ende-Marker '/'
		ex	de, hl
		ld	de, unk_556E	; kopieren in Buffer Suchzeichenkette
		lddr			; incl. der Trennzeichen '/'
					; Ablage revers!
		inc	de
		ld	a, 0A0h		; ' '+80h
		ld	(de), a		; Endekennzeichen ersetzen durch 0A0h
		ret

;;		db    0

;---------------------------------------
; F a - Listen der Zeilen, in denen die zuvor in Schrägstriche gestellte und
; eingegebene Zeichenkette enthalten ist.
; Damit lassen sich z.B. bestimmte Befehle, Marken und ähnliches im gesamten
; Quelltext suchen.

FIND:		call	sub_45F5	; suche Zeile ARG1 a
loc_49D5:	ld	(word_5585), hl	; Zeile merken
		ld	a, (hl)
		inc	a		; Textende FF?
		ret	z		; dann raus hier
		inc	hl		; Zeilennummer
		inc	hl		; übergehen
loc_49DD:	push	hl		; Position merken
		ld	de, unk_556E-1	; Buffer Suchzeichenkette
loc_49E1:	ld	a, (de)
		dec	de		; nächstes Zeichen
		cp	0A0h		; Textende?
		jr	z, loc_49F3	; ja, Suche erfolgreich
		cp	(hl)
		inc	hl		; nächstes Zeichen
		jr	z, loc_49E1	; solange matcht
		;nächste Zeile erreicht?
		pop	hl		; Position
		ld	a, (hl)		;
		inc	hl		;
		or	a		; 00?
		jr	nz, loc_49DD	; nein
		jr	loc_49D5	; ja Zeilendende erreicht
		;Suchpufferende erreicht -> gefunden
loc_49F3:	pop	hl		;
		call	loc_442D	; Suche Zeilenende
		ex	de, hl		; DE=Anfang nächste Zeile
		ld	hl, (word_5585)	; HL=Anfang aktuelle Zeile
		ld	(word_5585), de	; nächste Zeile zur Suche merken
		call	sub_4CAD	; Anzeige Zeile
		jr	loc_49D5	; und weitersuchen

;;		db    0
;;		db    0
;;		db    0
;;		db    0
;;		db    0
;;		db    0
;;		db    0
;;		db    0
;;		db    0
;;		db    0
;;		db    0
;;		db    0
;;		db    0

;---------------------------------------
; Ändern einer Quellzeile
; Z - Korrigiere in zuletzt eingegebener Zeile.
; Z x - Korrigiere in Zeile x.

CORRECT:	jr	nz, loc_4A19	; Z=keine Param.
		ld	hl, (word_5510)	; letzte Zeilennummer
		ld	(ARG1),	hl
loc_4A19:	call	sub_45F5	; suche Zeile ARG1
		ld	a, 3		; Error 03 - nicht existierende Zeile
		jp	nc, loc_4B30	; Fehler ausgeben
		call	sub_4CAD	; Anzeige Zeile
		xor	a
		ld	(byte_55FE), a
		rst	18h		; ed_prnst
		db 0Bh,9,9,9,9,89h
		call	sub_4FC0	; Eingabe Textzeile
		jp	loc_4A53	; Auswerten als Quelltext

;---------------------------------------
; Löschen von Quelltextzeilen
; X a e - Streichen der Zeilen ab a bis e einschließlich.
; Mit diesem Kommando können eingegebene Quelltextzeilen wieder gelöscht werden.
; Dieses Kommando wird nur dann ausgeführt, wenn beide Argumente angegeben sind,
; sonst erfolgt eine Fehlermeldung.

KILL:		cp	2		; in A=Anz. Parameter
		jp	nz, loc_4B2E	; error 99 illegal command
sub_4A3A:	call	sub_45F5	; suche Zeile ARG1 (a)
		push	hl
		dec	hl
		call	sub_444E
loc_4A42:	call	sub_456C
		jr	c, loc_4A4C
		dec	de
		dec	de
		inc	hl
		jr	loc_4A42
loc_4A4C:	call	sub_4459
		pop	de
		jp	sub_43C2

;---------------------------------------
; Eingabe Quelltext
loc_4A53:	ld	a, (de)		; in DE=10BF bws(29,0)
		cp	' '
		jp	z, loc_4B2E	; error 99 illegal command
		push	de
		call	sub_43EB	; Ascii Dez -> Hex
					; hier Übergehen der Marke
		add	a, 10h		; Zeichen hinter Zeilennummer-30h+10h
					; = 20? also Leerzeichen?
		jp	nz, loc_4B2E	; nein -> error 99 illegal command
		pop	hl
		push	hl
		or	a
		sbc	hl, de
		ld	bc, 0FFFBh	; -5
		add	hl, bc		; Zeilennummer mehr als 4 Zeichen?
		pop	de
		jp	c, loc_4B2E	; dann error 99 illegal command
		rst	30h		; ed_konvx
		ld	hl, (data+1)
		ld	a, h		; Zeilennummer 0000?
		or	l
		jp	z, loc_4B2E	; dann error 99 illegal command
		ld	(ARG1),	hl	; Zeilennummer merken
		ld	(word_5510), hl	; Zeilennummer merken
		;
		ld	hl, 1080h	; bws(29,63)
		ld	(hl), 0		; markieren
		inc	hl		; bws(29,62)
;;		nop
;;		nop
;;		nop
;;		nop
;;		nop
		call	sub_5180
loc_4A8C:	push	hl
		or	a
		push	de
		ex	de, hl
		sbc	hl, de
		pop	de
		push	hl
		inc	hl
		jr	c, loc_4A99
		inc	hl
		inc	hl
loc_4A99:	push	hl
		push	af
		push	hl
		ex	de, hl
		call	sub_48C4
		call	sub_444E
		jr	nc, loc_4AA7
		inc	de
		inc	de
loc_4AA7:	push	de
		call	sub_45F5	; suche Zeile ARG1
		pop	de
		jr	nc, loc_4AB9
		push	hl
		inc	hl
		call	sub_4663
		jr	nc, loc_4AB7
		dec	de
		dec	de
loc_4AB7:	pop	hl
		scf
loc_4AB9:	push	de
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
		ld	hl, (BUFEND)
		xor	a		; Cy=0, A=0
		sbc	hl, de
		jp	c, loc_4B30	; Fehler ausgeben
					; Error 00 - Speicherüberlauf
		ld	d, b
		ld	e, c
		call	sub_4459
		pop	hl
		pop	de
		call	sub_43C2
		pop	af
		jr	c, loc_4B0B
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
		call	sub_48D1
;;		nop
;;		nop
		ldd
		ldd
		pop	bc
		ld	hl, (word_5585)
		or	a
		sbc	hl, de
		add	hl, de
		jr	c, loc_4B0B
		add	hl, bc
		ld	(word_5585), hl
loc_4B0B:					; sub_4D26-221j
		bit	4, (iy+0)
		jr	z, loc_4B1F
		ld	hl, (ARG1)
		call	sub_441F	; Nächste Zeilennummer berechnen (BCD)
		ld	a, 2		; Error 02 - Überlauf im I-Mode
		jp	c, loc_4B30	; Fehler ausgeben
		ld	(word_5587), hl
loc_4B1F:	jp	loc_53C4

;---------------------------------------
; Automatische Zeilennummerierung
; I - ab Zeile 10 Schrittweite 10.
; I a - ab Zeile a Schrittweite 10.
; I a s - ab Zeile a Schrittweite s.
; Wie in BASIC-Interpretern der Auto-Befehl, gestattet das "I"-Kommando eine
; Eingabe mit automatischer Zeilennummernerzeugung. Der Kursor befindet sich bei
; der Zeileneröffnung immer auf der ersten Markenposition. Beendet wird dieses
; Kommando, in dem die letzte Zeilennummer wieder gelöscht wird, z.B. mit CTRL-R.

INUM:		call	sub_4404	; Parameter f. RENUM und INUM, Defaults 10
		ld	(word_5587), hl
		set	4, (iy+0)
		jr	loc_4B1F

; Error 99
loc_4B2E:	ld	a, 99h 		; Error 99 - illegales Kommando

; Fehler ausgeben
loc_4B30:	ld	hl, (word_5578)	; Adr. Ret-Funktion nach Fehler
		push	hl		; auf Stack
sub_4B34:	ld	de, bws(30,0)
		call	setcu		; set cursor
		ld	e, a		; Fehlernummer merken
		call	sub_4875	; Fehlerzähler erhöhen
		call	error		; Ausgabe "Error "
;;		nop
		ld	a, e		; Fehlernummer
		call	OUTHEX		; ausgeben
		set	7, (iy+0)
		jp	sub_4CB6

;---------------------------------------
; Warmstart
; Restart mit alten Parametern
WARM:		call	sv_init		; Sprungverteiler init
		jp	warm1

;---------------------------------------
; Kaltstart
; ARG2=AAAA ARG3=EEEE, Default 6000-BFFF
COLD:		call    loc_5F9B
		call	sv_init		; Sprungverteiler init
		ld	hl, 0FFFFh
		ld	(word_54B8), hl	; Adresse Anfang Sekundärquelle (SEC)
;;		jp	cold1

;---------------------------------------
; Kaltstart
cold1:		ld	hl, (ARG2)	; BUFBEG
		; Sicherung f. REUSE
		ld	(p_BUFBEG), hl	; Sicherung BUFBEG f. REUSE
		ld	de, unk_556F	; Kopf sichern f. REUSE
		ld	bc, 6
		ldir
		; Defaultwerte setzen
		ld	hl, off_4006	; defaultwerte kopieren
		ld	de, BUFBEG	; Anfangsdresse
		ld	bc, 30h
		ldir
		; ARG2 BUFBEG
		ld	hl, (ARG2)	; BUFBEG=0 ?
		ld	a, h
		or	l
		jr	z, loc_4B70	; nein -> Wert merken
		ld	(BUFBEG), hl	; Anfangsdresse
		; ARG3 BUFEND
loc_4B70:	ld	hl, (ARG3)	; BUFEND
		ld	a, h		; BUFEND=0 ?
		or	l
		jr	z, loc_4B7A	; nein -> Wert merken
		ld	(BUFEND), hl
		; Textspeicher init
loc_4B7A:	ld	hl, (BUFBEG)	; Anfangsdresse
;;		nop
		ld	(hl), 6		; Länge 0006 Bytes
		inc	hl
		xor	a
		ld	(hl), a
		inc	hl
		ld	(hl), a		; Overhead 0000 Bytes
		inc	hl
		ld	(hl), a
		inc	hl
		ld	(hl), a		; 00
		inc	hl
		dec	a		; FF = leer
		ld	(hl), a		; Textende setzen
		ld	(word_5585), hl
		;
		ld      hl, aNamenlos   ; "NAMENLOS "
		ld      de, unk_FF00
		ld      bc, 10h
		ldir
		ld      hl, aNamenlos   ; "NAMENLOS "
		ld      de, unk_FF10
		ld      bc, 10h
		ldir
		xor     a
		ld      (unk_FF32), a

;---------------------------------------
; Warmstart
warm1:		ld	sp, byte_55FE
		ld	de, bws(30,0)
		call	setcu		; set cursor
		rst	18h		; ed_prnst
;;		db 0Ch," AC1 - EDITOR/ASSEMBLER",8Dh
		db	8Ch
		call    sub_55AC
		;;ld	hl, 17BFh
		ld	hl, 177Fh
		ld	b, 40h 		; Länge Bildzeile
loc_4BB7:	ld	(hl), '-'	; unterstreichen
		dec	hl
		djnz	loc_4BB7

;---------------------------------------
; main: Kommandoschleife

; Ret.Adr. nach Kdo-Ausführung
loc_4BBC:	xor	a
		ld	(byte_55FE), a
		rst	18h		; ed_prnst
		db 8Dh
;;		nop
		ld	a, (byte_5514)	; Seitengröße auf Bildschirm
		ld	(unk_5577), a
		ld	iy, byte_55FE
		ld	sp, iy
;;		ld	hl, unk_40A8	; ZEAS: new output table
;;		nop			; ZEAS rst 18h
;;		nop			; ZEAS db 71h  NOM Set new output table
		ld	hl, loc_4D0E	; Ausgabe Enter+ggf. Abbruch bei Strg-C
		ld	(unk_559B), hl
		ld	hl, loc_4BBC	; Adr. Kommandoschleife
		ld	(unk_557A), hl
		ld	(word_5578), hl	; Adr. Ret-Funktion nach Fehler
		call	chksum		; Test EDAS-Integrität
;
loc_4BE6:	ld	a, (byte_5507)	; Assembleroptionen
		ld	(unk_55FF), a	; Assembleroptionen (IY+1)
		ld	sp, iy		; setze Stack
		xor	a
		ld	h, a
		ld	l, a
		ld	(ARG1),	hl	; Defaultwert 0000
		dec	hl
		ld	(ARG2),	hl	; Defaultwert FFFF
		ld	hl, unk_409D	; Text "FREE: END: SEC:"
		;;ld	de, bws(0,26)
		;;ld	bc, 19h
		ld	de, 17A9h
		ld	bc, 25h
		lddr			; anzeigen
		ld	hl, loc_4CA2	; Ausgabe in Speicher (ARG3)
		ld	(unk_559B), hl
		;
		call	sub_4466	; Bufferende ermitteln
		;;ld	de, bws(0,31)
		ld      de, 17A4h
		call	sub_46B2	; Anzeige FREE
		;
		rst     18h
		db 8,0ADh
		ld      hl, (BUFEND)
		ld      de, 179Fh
		call	sub_46B2	; Anzeige BUFEND
		;
		ld	hl, (word_550C)	; MC-Adr.
		;;ld	de, bws(0,41)
		ld      de, 1796h
		call	sub_46B2	; Anzeige END
		;
		ld	hl, (word_54B8)	; Adresse Anfang Sekundärquelle (SEC)
		;;ld	de, bws(0,51)
		ld      de, 178Dh
		call	sub_46B2	; Anzeige SEC
		;
		ld      de, 1784h
		call    setcu
		ld      a, (iy+1)
		call    7EEh
		;
		ld	hl, loc_4D0E	; Ausgabe Enter+ggf. Abbruch bei Strg-C
		ld	(unk_559B), hl
		ld	de, bws(30,0)
		call	setcu		; set cursor
		bit	4, (iy+0)
		jr	z, loc_4C3A
		ld	hl, (word_5587)
		rst	28h		; ed_outhls
		;
loc_4C3A:	call	sub_4FC0	; Eingabe Textzeile
		push	de		; Beginn Kdo-Zeile auf Stack
		ld	a, (de)		; 1. Zeichen == Kommando
		ld	bc, unk_559E	; Kdo-Buchstabe
		cp	' '		; Leeres Kdo?
		jr	nz, loc_4C4C	; nein - Kopieren
		ld	a, (bc)		; ja - war letztes Kommando FIND?
		cp	'F'		;
		jp	nz, loc_4BBC	; nein, dann zurück zur Kommandoschleife
loc_4C4C:	ld	(bc), a		; Kdo
		dec	de
		inc	bc		; BC=byte_559F ; Anzahl Kdo.Argumente
		xor	a		; Anz. Param mit 00 init
		ld	(bc), a
; Verarbeitung Parameter
		ld	bc, data+2
loc_4C54:	rst	30h		; ed_konvx
;;		nop
		ld	a, (hl)		; data=Anz. Stellen
		or	a		; 0 ?
		jr	z, loc_4C6B	; dann Ende Param.
		inc	bc		; hinter data folgen ARG1..3
		inc	hl
		ld	a, (hl)		; lo-Byte Param
		ld	(bc), a
		inc	bc
		inc	hl
		ld	a, (hl)		; hi-Byte Param
		ld	(bc), a
		ld	hl, byte_559F	; Anzahl Kdo.Argumente
		inc	(hl)		; erhoehen
		ld	a, (hl)
		cp	3		; max 3 Argumente
		jr	nz, loc_4C54	; weitere Parameter
; suche Kdo
loc_4C6B:	ld	bc, (unk_559E)	; C=Kdo-Buchstabe
					; B=Anz. Param
		ld	hl, (unk_5515)	; kdotab
		pop	de		; Adr. Textzeile von Stack
loc_4C73:	ld	a, (hl)
		or	a		; Ende kdotab erreicht (00)?
		jp	z, loc_4A53	; dann vllt. Eingabe Quelltext
		inc	hl
		cp	c		; vgl. Kdo
		jr	z, loc_4C80	; gefunden
		inc	hl		; sonst Adr. überspringen
		inc	hl
		jr	loc_4C73	; weitersuchen
; Kdo gefunden
loc_4C80:	ld	de, bws(31,0)	; 103F
		call	setcu		; set cursor
		ld	e, (hl)
		inc	hl
		ld	d, (hl)		; de=Adr. Kdo.
		ld	hl, loc_4BBC	; Ret.Adr. Kommandoschleife
		push	hl		; auf Stack
		push	de		; Kdo.Adr auf Stack
		;
		ld	hl, (cupos)	; 103F bws(31,0)
		ld	de, 3Fh
		add	hl, de		; 107E bws(30,1) ???
		ex	de, hl
		call	sub_43EB	; Ascii Dez -> Hex
		; Kdo. Register setzen und starten
		ex	de, hl		; DE=BCD
		ld	hl, (ARG1)	; HL=1.Parameter Hex
		ld	a, (byte_559F)	; A=Anzahl Kdo.Argumente
		or	a		; Z=keine Param.
		ret			; Start Kdo.

;---------------------------------------
; Ausgabe in Speicher (ARG3)
loc_4CA2:	push	hl
		ld	hl, (ARG3)
		ld	(hl), a
		inc	hl
		ld	(ARG3),	hl
		pop	hl
		ret

;---------------------------------------
; Anzeige Zeile
sub_4CAD:	call	sub_4623	; Ausgabe Zeilennummer
		jp	nz, loc_4BBC	; bei Fehler zurück zur Kommandoschleife
		call	sub_51BD	; Ausgabe Zeileninhalt
;
sub_4CB6:	bit	2, (iy+0)
		jr	z, loc_4CE2
		jp	loc_53CF
;;		db    0

;---------------------------------------
; UP zu loc_53CF
loc_4CC0:	bit	1, (iy+0)
		jr	z, loc_4CC9
		ld	hl, 1040h	; bws(30,63)
loc_4CC9:	call	sub_44A6
		ex	de, hl
		call	sub_4635 	; set cursor bws(31,0)
		ld	de, bws(30,0)
		ld	a, (de)
loc_4CD4:	call	sub_4D34
		dec	de
		ld	a, (de)
		or	a
		jp	p, loc_4CD4
		call	sub_4480
		jr	loc_4D01

;---------------------------------------
;
loc_4CE2:	ld	a, (iy+0)
		and	3
		cp	1
		ld	a, (byte_5509)	; Ausgabeverzögerung BWS
		call	nz, sub_4473
		ld	a, (unk_5577)
		sub	1
		jr	c, loc_4D01
		jr	nz, loc_4CFB
		ld	a, (byte_5514)	; Seitengröße auf Bildschirm
loc_4CFB:	ld	(unk_5577), a
		call	z, sub_4D26
loc_4D01:	ld	de, (word_5512)	; ausgegebene Zeilennummer
		ld	(word_5510), de	; Zeilennummer
		call	sub_4D22	; ggf. Abbruch bei Stop
		ld	a, 0Dh

; Ausgabe Enter+ggf. Abbruch bei Strg-C
loc_4D0E:	push	af
		rst	18h		; ed_prnst
		db 8Dh
;;		nop
		call	tastec		; TASTE?
;;		nop
;;		nop
;;		nop
;;		nop
;;		nop
;;		nop
		cp	83h		; STOP?
		jp	z, loc_4BBC	; ja, zurück zur Kommandoschleife
		pop	af
		ret

;---------------------------------------
; ggf. Abbruch bei Stop
sub_4D22:	call	tastec		; TASTE?
		ret	nc		; keine Taste gedrückt
;
sub_4D26:	rst	8		; ed_inch0
		cp	' '
		jr	z, sub_4D26	; Leerzeichen übergehen
		cp	3		; STOP gedrückt?
		scf
		ret	nz		; nein
		; bei Stop Zeilenumbruch ausgeben
		rst	18h		; ed_prnst
		db 8Dh
		jp	loc_4BBC	; zurück zur Kommandoschleife


;---------------------------------------
;
sub_4D34:	and	7Fh
		call	unk_5504	; Druckerausgabe
		jr	sub_4D22	; ggf. Abbruch bei Stop
;;		db    0
;;		db    0

;---------------------------------------
; Quelltext assemblieren, nur Fehler ausgeben
; W (a e) - Argumente analog dem "A"-Kommando.
; Zur Fehlerkorrektur ist es übersichtlicher, wenn nur die fehlerhaften Zeilen
; aufgelistet werden, die man dann leicht mit dem "Z"-Kommando berichtigen kann.

WERRORS:	ld	a, (iy+1)	; Assembleroptionen
		and	24h ; '$'
		or	11h
		ld	(iy+1),	a	; Assembleroptionen

; Assemblieren
loc_4D47:	ld      hl, 0FFFFh
		ld      (unk_54C0), hl
		;
		ld	hl, loc_4F9A
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
		call	sub_4DCF	; Assemblieren (Pass 1)
		pop	hl
		ld	(word_550C), hl	; MC-Adr. rücksetzen auf Bufferende
					; für Pass 2
		pop	hl
		push	hl
		bit	7, l		; ohne Fehler (?)
		jr	z, loc_4D70	; dann Pass 2
		bit	4, h
		jr	z, loc_4DCC	; zurück zur Kommandoschleife
; Assemblieren Pass 2
loc_4D70:
;;		nop
;;		nop
;;		nop
;;		nop
;;		nop
		pop	hl
		ld	a, h
		and	5
		or	2
		ld	l, a
		push	hl
		call	sub_4CB6
		bit	0, l
		jr	nz, loc_4D9D
		call	loc_44C1	; Ausgabe "AC1 U880 Assembler"
		rst	18h		; ed_prnst
;;		db "Source Listing",8Dh
		db "Quelltext",8Dh
		call	sub_4CB6
		call	sub_4CB6
loc_4D9D:	call	sub_486A	; Fehlerzähler rücksetzen
;;		bit	6, (iy+1)	; Assembleroptionen
;;		jr	z, loc_4DA9
;;		ld	hl, 4141h
loc_4DA9:	ld	(word_557C), hl
		push	hl
		call	sub_4DCF	; Assemblieren (Pass 2)
					; Listing ausgeben
		bit	7, (iy+1)	; Assembleroptionen Symboltabelle ?
		call	nz, loc_457E	; Ausgabe Symboltabelle
		call	sub_4CB6
		ld	a, 61h 		; Error 61 - Bereichsüberschreitung
		bit	6, (iy+0)
		call	nz, sub_4B34	; Fehler ausgeben
		call	loc_488D	; Ausgabe Anz. Fehler
;;		nop
;;		nop
;;		nop
;;		nop
;;		nop
;;		nop
;
loc_4DCC:	jp	loc_4BBC	; zurück zur Kommandoschleife

; Assemblieren? und Listing ausgeben
sub_4DCF:	call	sub_4435	; Textende ermitteln
		push	de		; DE=Textende
		pop	ix		; IX=Textende
		call	sub_45F5	; suche Zeile ARG1
		dec	hl		; HL=Zeilenanfang-1
		ld	(word_5589), hl	; merken
		inc	hl		; HL=Zeilenanfang

; LOOP: nächste Zeile assemblieren
loc_4DDD:	call	sub_4642	; die letzten beiden BWS-Zeilen leeren
		ld	de, bws(30,14)
		call	setcu		; set cursor
		call	sub_4623	; Ausgabe Zeilennummer
					; und Ermittlung Zeichenklasse
					; ret A=Zeichen, C=Zeichenklasse
		ret	nz		; Programmende erreicht
		push	af
		jr	nc, loc_4DF8
		push	hl
		ld	hl, (word_550C)	; MC-Adr.
		ld	(ix+0),	l
		ld	(ix+1),	h
		pop	hl
loc_4DF8:	cp	';'		; Kommentarzeile?
		jr	z, loc_4E04	; ja
		;
		ld	b, 7
		call	sub_4699
		call	z, sub_4694	; Ausgabe Mnemonik
loc_4E04:	push	af
		call	sub_51BD	; Ausgabe Text (Befehlsparameter etc.)
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
		jp	nz, loc_4B30	; Fehler ausgeben
		ex	af, af'	;'
		jr	nc, loc_4E29
		ld	hl, bws(30,19)
		call	sub_44D9
		ld	a, 31h 		; Error 31 - Marke doppelt definiert
		jp	nc, loc_4B30	; Fehler ausgeben
loc_4E29:	cp	';'		; Kommentar ?
		jp	z, loc_4F82
		ld	de, bws(30,0)
		ld	hl, (word_550C)	; MC-Adr.
		call	sub_46B2	; Ausgabe HL
		; Zeile ist enpackt auf dem BWS
		; jetzt parsen und assemblieren
		ld	hl, (word_5517)	; unk_40AA Parse Tree Mnemonics
		push	hl		; auf Stack
		ld	d, 0
		push	de
		ld	hl, bws(30,25)
		jr	loc_4E4B
		;
loc_4E43:	call	sub_4306	; Abarbeitung nächstes Token
loc_4E46:	ld	a, 20h 		; Error 20 - unbekannte Mnemonik
loc_4E48:	jp	c, loc_4B30	; Fehler ausgeben
		;
loc_4E4B:	call	sub_48BE	; nächstes Zeichen
		jr	c, loc_4E43	; solange Textzeichen
		;
		call	sub_48E1	; Mnemonik parsen
		bit	7, (iy-12h)
		jp	z, loc_4F14
		call	sub_439F
		jr	c, loc_4E46
		push	af
		cp	80h ; 'Ç'
		bit	3, c
		ld	a, 40h ; '@'
		jp	nc, loc_4EF5
		jr	nz, loc_4E48
		pop	af
		push	af
		set	2, (iy-0Ah)
		ld	bc, unk_54FF	; Buffer f. Code
		cp	3
		jr	z, loc_4EB8
		jr	nc, loc_4ECF
		ld	b, 5
		call	sub_46BE
		ccf
loc_4E80:	ld	a, 40h 		; Error 40 - Pseudo-Befehl-Fehler
		jp	nc, loc_4B30	; Fehler ausgeben
;;		ld	h, d		; Adresse übernehmen
;;		ld	l, e
		;
		ld      hl, (unk_54C0)
		inc     hl
		ld      a, h
		or      l
		ld      h, d
		ld      l, e
		jr      nz, loc_4E8E
		ld      (unk_54C0), hl
loc_4E8E:       ;
		pop	af
		cp	1
		jr	z, loc_4E93	; (bei org ?)
		jr	c, loc_4E9A
		ld	bc, (word_550C)	; MC-Adr.
		add	hl, bc
loc_4E93:	ld	(word_550C), hl	; setze MC-Adr.
		res	3, (iy+0)
loc_4E9A:	bit	0, (iy-8)
		jr	z, loc_4EAA
		cp	2
		jr	z, loc_4EAA
		ld	(ix+0),	l
		ld	(ix+1),	h
loc_4EAA:	ex	de, hl
		cp	1
		ld	de, bws(30,0)
		call	z, setcu	; set cursor
		rst	28h		; ed_outhls Ausgabe MC-Adresse
;;		nop
loc_4EB5:	jp	loc_4F82
;Zeichenkette
loc_4EB8:	ld	d, (hl)
loc_4EB9:	push	bc
		call	sub_48BE
		pop	bc
		cp	d
		jr	z, loc_4EE5
		cp	0A0h 		; Zeilenende?
		jr	z, loc_4EE5
		ld	(bc), a
		dec	bc
		jr	loc_4EB9
loc_4EC9:	call	sub_43AD
loc_4ECC:	pop	af
		jr	nc, loc_4EE5
loc_4ECF:	push	bc
		ld	b, 4
		call	sub_46BE
		pop	bc
		push	af
		ld	a, e
		ld	(bc), a
		dec	bc
		bit	0, (iy-0Fh)
		jr	z, loc_4EC9
		ld	a, d
		ld	(bc), a
		dec	bc
		jr	loc_4ECC
; Befehlszeilenende erreicht
; MC wegschreiben/anzeigen
loc_4EE5:	ld	hl, unk_54FF	; Buffer f. Code
loc_4EE8:	or	a		; Cy=0
		sbc	hl, bc		; Test HL=BC
		add	hl, bc
		jr	z, loc_4EB5	; bis Code-Ende
		ld	a, (hl)		; gen. Code-Byte
		dec	hl		; nächstes Zeichen
		call	sub_47C6	; Ausgabe Byte
		jr	loc_4EE8
loc_4EF5:	;;jr	z, loc_4E80
		jp	z, loc_4E80
		pop	af
		cp	81h		; special code SKIP
		jr	nc, loc_4F04	; bei SKIP
		; else 'ENT'
		ld	hl, (word_550C)	; aktuelle MC-Adr.
		ld	(word_550E), hl	; ENT-Adresse
		jr	loc_4EB5	; -> loc_4F82
		;
loc_4F04:	call	sub_4642	; die letzten beiden BWS-Zeilen leeren
		jr	loc_4EB5	; -> loc_4F82

;---------------------------------------
;
loc_4F09:	call	sub_46BC	; Parameter bearbeiten
		ld	a, b
		call	sub_4306	; Abarbeitung nächstes Token
loc_4F10:	ld	a, 21h ; '!'
		jr	c, loc_4F66
; Einstieg
loc_4F14:	bit	3, c
		jr	z, loc_4F09
		call	sub_439F
		jr	c, loc_4F10
		pop	bc
		pop	hl
		bit	2, c
		jr	z, loc_4F33
		dec	hl
		dec	hl
		bit	5, (iy+1)	; Assembleroptionen rel.Jp.Abs
		jr	nz, loc_4F33
		; relative Sprungdistanzen absolut
		push	de
		ld	de, (word_550C)	; aktuelle MC-Adr.
		sbc	hl, de
		pop	de
;
loc_4F33:	ex	de, hl
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
		ld	a, 0CBh
		bit	3, e
		jr	z, loc_4F5A
		push	af
loc_4F5A:	bit	4, l
		jr	z, loc_4F78
		ld	a, h
		bit	6, e
		jr	z, loc_4F6B
		or	a
loc_4F64:	ld	a, 22h 		; Error 22 - Indexregister-Fehler
loc_4F66:	jp	nz, loc_4B30	; Fehler ausgeben
		jr	loc_4F70
;
loc_4F6B:	pop	bc
		bit	0, l
		push	af
		push	bc
loc_4F70:	ld	a, 0DDh	; IX ...
		or	l
		push	af
		bit	4, e
		jr	nz, loc_4F64
;
loc_4F78:	ld	a, 0EDh	; Präfix ED
		bit	4, e
loc_4F7C:	call	nz, sub_47C6	; Ausgabe Hex-Code
		pop	af
		jr	nc, loc_4F7C
;
loc_4F82:	bit	0, (iy+0)	;
		call	z, sub_4CB6	; neue Zeile
		bit	3, (iy-0Ah)	;
		jr	nz, loc_4F9E
		bit	6, (iy-0Ah)	;
		ld	a, 23h 		; Error 23 - Wert bzw. Distanz zu groß
		call	nz, sub_4B34	; Fehler ausgeben
		jr	loc_4FA3

;---------------------------------------
; hier gehts nach einem Fehler beim Assemblieren weiter
loc_4F9A:	bit	3, (iy-0Ah)
;
loc_4F9E:	ld	a, 30h 		; Error 30 - Marke nicht gefunden
		call	nz, sub_4B34	; Fehler ausgeben
;
loc_4FA3:	ld	sp, unk_55F6	; reset Stack
					; (Param vom Stack nehmen)
		pop	af
		jr	nc, loc_4FAD
		inc	ix
		inc	ix
loc_4FAD:	pop	hl
		call	sub_4D22	; ggf. Abbruch bei Stop
		jp	loc_4DDD	; nächste Zeile assemblieren

;---------------------------------------
; TASTE, Ret Cy bei Taste
tastec:		push	bc
		call	TASTE		; AC1: testet den Tastaturstatus
		jr	z, loc_4FBD
		scf
		pop	bc
		ret
loc_4FBD:	and	a	; Cy=0
		pop	bc
		ret


;---------------------------------------
; Eingabe Textzeile
; in cupos=107F bws(30,0)
; ret DE=Text
sub_4FC0:	push	hl
loc_4FC1:	rst	8		; ed_inch0
		cp	0Dh		; Enter?
		jr	z, loc_4FE2
		cp	0Ch		; CLS
		;;jr	z, loc_4FC1
		jr	z, loc_4FE9
		call	sub_5417
		rst	10h		; ed_outch0
		ld	de, (cupos)
		ld	a, e
		cp	80h 		; lo( bws(29,63) )
					; Zeilenanfang unterschritten?
		jr	nc, loc_4FDD	; ja, nicht weiter zurück
		cp	4Fh 		; lo( bws(30,48) )
					; max Anzahl Zeichen erreicht?
		jr	c, loc_4FDD	; ja, nicht weiter vor
		jr	loc_4FC1
loc_4FDD:	ld	(cupos), hl
		jr	loc_4FC1
;
loc_4FE9:       ld      b, 19h
                ld      a, 0Dh
loc_4FED:	rst     10h
                djnz    loc_4FED
;
loc_4FE2:	ld	de, bws(29,0)
		rst	18h		; ed_prnst
		db 8Dh
		pop	hl
		ret

;---------------------------------------
;rst18 outch
; eigene Routinen für Bildschirm

outch0:		push	hl
		push	de
		push	bc
		push	af
		and	7Fh ; ''
		ld	hl, (cupos)
		cp	0Dh		; CR
		jp	z, oc_cr
		cp	0Ch		; ^L
		jr	z, oc_cls
		cp	12h
		jr	z, oc_bakdel	; ^R
		;
		cp      7Fh
		jr      z, oc_bakdel
		;
		cp	13h
		jr	z, oc_del	; ^S
		;
		cp      4
		jr      z, oc_del
		;
		cp	8
		jr	z, oc_left
		cp	9
		jr	z, oc_right
		cp	0Bh
		jp	z, oc_up	; Kursor nach oben
		cp	17h
		;;jr	z, oc_tab	; ^W
		jp	z, oc_tab	; ^W
		cp	5
		jr	z, oc_ins	; ^E
		cp	0Ah
		;;jr	z, oc_down	; Kursor nach unten
		jp	z, oc_down	; Kursor nach unten
		cp	14h		; ^T
		;;jr	z, oc_firstc
		jp	z, oc_firstc
		;
		cp      0Fh
		jr      z, loc_5045
		;
		cp	' '
		jp	c, oc_end	; sonstige Steuerzeichen uebergehen
		jp	outx		; Zeichen direkt ausgeben
;
loc_5045:       ld      b, 8
loc_5047:       dec     hl
                djnz    loc_5047
                jp      oc_end

; CTRL - L cls
oc_cls:		ld	hl, BWS
		ld	(hl), ' '
		ld	de, BWS+1
		ld	bc, 7FFh	; BWS-Size
		ldir
		jp	oc_end
; CTRL - S Streichen
oc_del:		ld	d, h
		ld	a, l
		and	0C0h
		ld	e, a
		ld	a, l
		sub	e
		ld	e, l
		ld	c, a
		ld	b, 0
		dec	hl
		lddr
		ld	a, ' '
		ld	(de), a
		jr	oc_end1
; CTRL - H <==
oc_left:	inc	hl
		jr	oc_end
; CTRL - I ==>
oc_right:	dec	hl
		jr	oc_end
; CTRL - R Rückwärtslöschen
oc_bakdel:	inc	hl
		ld	(hl), ' '
		jr	oc_end
; CTRL - E Einfügen
oc_ins:		push	hl
		ld	a, l
		and	0C0h
		ld	l, a
		ex	de, hl
		xor	a
		pop	hl
		sbc	hl, de
		push	hl
		pop	bc
		push	de
		pop	hl
		inc	hl
		ldir
		ld	a, ' '
		ld	(de), a
		jr	oc_end1
; Tabulator-Tabelle
ttab:		db -7
		db -4
		db -5
		db -5
		db -5
		db -5
		db    0
sub_5073:	ex	de, hl
		ld	hl, bws(30,0)
		ld	a, ' '
loc_5079:	dec	hl
		cp	(hl)
		jr	nz, loc_5079
		ret
; CTRL - W spezieller Tabulator
oc_tab:		call	sub_5073
		ld	bc, ttab-1
oc_tab1:	inc	bc
		ld	a, (bc)
		or	a
		jr	z, oc_end
		add	a, l
		ld	l, a
		sbc	hl, de
		add	hl, de
		jr	nc, oc_tab1
		jr	oc_end
; CTRL - T Kursor hinter Zeilennummer
oc_firstc:	call	sub_5073
		jr	oc_end
;
oc_down:	ld	de, -COLS	; Zeilenlaenge
oc_down1:	add	hl, de
		jr	oc_end
;
oc_up:		ld	de, COLS	; Zeilenlaenge
		jr	oc_down1
;
oc_end:		ld	(cupos), hl
oc_end1:	pop	af
		pop	bc
		pop	de
		pop	hl
		ret
;
outx:		ld	(hl), a
		dec	hl
		ex	de, hl
		ld	hl, 0FFFh
		and	a
		sbc	hl, de
		ex	de, hl
		jr	c, oc_end
;
oc_cr:		call	sub_515E
		ld	(cupos), de
		ex	de, hl
		inc	hl
oc_cr1:		dec	l
		ld	(hl), ' '
		jr	nz, oc_cr1
		jr	oc_end1

;---------------------------------------
;rst08 inch
; eigene Routinen für Bildschirm. Die Eingabe ist mit einer Auto-
; Repeat-Funktion versehen, d.h., nach längerem Drücken wird eine Taste
; automatisch wiederholt.

inch0:		push	hl
		push	bc
		ld	hl, (cupos)	; Cursorposition
inch1:		ld	c, (hl)		; Zeichen merken
		res	7, c
		ld	(hl), '_'	; Cursorzeichen
inch2:		ld	b, 0
inch3:		push	bc
		ld	b, 0
inch4:		djnz	inch4		; kurz warten
		pop	bc
		call	TASTE		; Taste gedrückt?
		jr	nz, inch5	; ja
		djnz	inch3		; sonst weiter versuchen
		; blinken
		ld	a, '_'		; Cursorzeichen
		cp	(hl)
		jr	nz, inch1
		ld	(hl), c		; Zeichen wiederherstellen
		jr	inch2
;
inch5:		ld	(hl), c		; Zeichen wiederherstellen
		push	af
inch6:		call	TASTE
		ld	hl, unk_55B4	; Repeat-Timer
		jr	nz, inch8
		;;ld	a, 10h		; Startwert 16
		ld	a, 20h
inch7:		ld	(hl), a
		pop	af
		; Bit 7 rücksetzen (Kennung Taste gedrückt)
		and	7Fh
		pop	bc
		pop	hl
		ret
		; repeat
inch8:		ld	a, (hl)
		dec	a
		ld	(hl), a
		jr	nz, inch6
		inc	a
		inc	a
		inc	a
		jr	inch7		; Folgezeitwert 3



;---------------------------------------
; orig.	Sprungverteiler	wiederherstellen
sv_restore:	ld	hl, orig_sv	; Merker für orig. Sprungverteiler
					; 12h Byte
		ld	de, unk_1802	; Adr. Sprungverteiler Monitor
		ld	bc, 12h
		ldir
		ret

;---------------------------------------
; Q - Rücksprung zum Monitor.
; Dieses Kommando übergibt die Steuerung wieder dem "AC1"-Monitor, zuvor wurden
; die Argumentzeilen Arg1 bis Arg3 so geladen, daß der Quelltext einfach mit S:
; name abgespeichert werden kann. EDAS*4 ist nun jederzeit wieder über einen
; Warmstart zu aktivieren. Alle vor dem Verlassen eingestellten Parameter bleiben
; erhalten, natürlich vorausgesetzt, es wurde nichts durch eine
; Speichermanipulation zerstört. Soweit das EDAS*4 selbst betrifft, merkt er das
; durch seine Prüfsumme und kehrt sofort zum Monitor zurück. Dann muß man EDAS*4
; neu von der Kassette laden. Wenn man Glück hat und der Quelltext ist erhalten
; geblieben, so läßt er sich mit "Y" reaktivieren.

QUIT:		call	sv_restore	; orig.	Sprungverteiler	wiederherstellen
		ld	hl, (BUFBEG)	; Anfangsdresse
		ld	(ARG1),	hl
		ld	(ARG3),	hl
		call	sub_4466	; Bufferende
		ld	(ARG2),	hl	; merken
		jp	GETCO1

;---------------------------------------
; Sprungverteiler init
sv_init:	ld	hl, unk_1802	; orig Sprungverteiler f. RST-Aufrufe und NMI
					; nach 55A0 sichern
		push	hl
		ld	de, orig_sv	; Merker für orig. Sprungverteiler
					; 12h Byte
		ld	bc, 12h
		push	bc
		ldir
		ld	hl, edas_sv	; neuer	Sprungverteiler
		pop	bc
		pop	de
		ldir
		ret

;---------------------------------------
; neuer Sprungverteiler, wird nach 1802 kopiert
edas_sv:	jp	inch0		; rst08 lokal
		jp	outch0		; rst10	lokal
		jp	prnst0		; rst18 lokal
		jp	MS30		; rst20 orig. Monitor
		jp	outhls		; rst28	lokal
		jp	konvx		; rst30 lokal

;---------------------------------------
; scrollen, Zeile 0 und 1 bleiben stehen
sub_515E:	;;ld	hl, bws(3,0)
		;;ld	de, bws(2,0)
		;;ld	bc, 740h	; 29 Zeilen
		ld      hl, 16FFh
		ld      de, 173Fh
		ld      bc, 700h
		lddr
; UP zu loc_4803 zu SWAP
sub_5169:	ld	b, 0C0h		; 3 Zeilen
		ld	hl, 1000h
loc_516E:	ld	a, (hl)
		cp	0A0h		; suche Marker
					; (Leerzeichen + Bit7)
		jr	nz, loc_5175
		res	7, (hl)		; gefunden->rücksetzen
loc_5175:	inc	hl
		djnz	loc_516E
		ret

;---------------------------------------
; rst28
; Ausgabe HL + Space
outhls:		call	OUTHL
		ld	a, ' '
		rst	10h		; ed_outch0
		ret

;---------------------------------------
; UP zu loc_4A53, Eingabe Quelltext
sub_5180:	call	sub_44A6
		or	a
		sbc	hl, de
		add	hl, de
		ret	nc
		;;ld	bc, unk_54C0
		ld	bc, unk_54C0+2
		dec	de
loc_518C:	or	a
		sbc	hl, de
		add	hl, de
		jr	z, loc_51B4
		ld	a, (hl)
		cp	20h ; ' '
		jr	z, loc_519C
		inc	hl
loc_5198:	ld	(bc), a
		inc	bc
		jr	loc_518C
loc_519C:	xor	a
		ex	af, af'	;'
loc_519E:	ex	af, af'	;'
		inc	a
		inc	hl
		or	a
		sbc	hl, de
		add	hl, de
		jr	z, loc_5198
		cp	1Fh
		jr	nc, loc_5198
		ex	af, af'	;'
		ld	a, 20h ; ' '
		cp	(hl)
		jr	z, loc_519E
		ex	af, af'	;'
		jr	loc_5198
loc_51B4:	ld	e, c
		ld	d, b
		ld	a, (hl)
		ld	(bc), a
		inc	de
		;;ld	hl, unk_54C0
		ld	hl, unk_54C0+2
		ret

;---------------------------------------
; Ausgabe Text mit Dekomprimieren
; in A Zeichen, HL Text, DE=Cursorpos.
;
sub_51BD:	inc	hl		; Zeiger auf nächstes Zeichen
		or	a		; Zeilenende?
		ld	a, 0A0h		; Ende-Zeichen
		ld	(de), a		; in Bildschirm setzen
		ret	z		; bei Ende
		;
		dec	hl		; aktuelles Zeichen
		ld	a, (hl)		; holen
		cp	' '
		jr	c, loc_51CE	; komprimierte Leerzeichen
		call	sub_4661	; Zeichen ausgeben
		jr	sub_51BD	; nächstes Zeichen
;dekomprimieren
loc_51CE:	ld	b, a		; Anzahl
		ld	a, ' '		; Leerzeichen
loc_51D1:	ld	(de), a		; ausgeben
		dec	de
		djnz	loc_51D1
		inc	hl		; nächstes Zeichen
		jr	sub_51BD	; weiter ausgeben


;---------------------------------------
; Anzeige des Quelltextes ab Marke
;
; M Marke - Anzeige des Quelltexte, ab Marke bis Ende.
;
; Dieses Kommando dient ebenfalls zur Anzeige des Quelltextes, wobei hier ab der
; angegebenen Marke begonnen wird.

MARK:		ld	hl, bws(29,0)	; Kdo Zeile
loc_51DB:	dec	hl		; Zeichen hinter Kdo.Buchstaben
		ld	a, (hl)
		cp	' '		; Leerzeichen
		jr	z, loc_51DB	; übergehen
		; Markenname kopieren
		ld	de, unk_5530	; 6 Zeichen Marke
		ld	b, 6		; max. 6 Zeichen
loc_51E6:	ld	(de), a		; Zeichen kopieren
		inc	de
		dec	hl
		ld	a, (hl)
		cp	' '		; Leerzeichen
		jr	z, loc_51F0	; dann Markenname zu Ende
		djnz	loc_51E6	; sonst nächstes Zeichen kopieren
		;
loc_51F0:	ld	a, b
		ld	(de), a
		ld	hl, (BUFBEG)	; Anfangsdresse
		ld	c, (hl)
		inc	hl
		ld	b, (hl)		; BC=TextLänge
		inc	hl
		inc	hl
		inc	hl
loc_51FB:	xor	a		; 00=Zeilenende
		cpir			; suche Zeilenende
		jp	nz, loc_4BBC	; bei Fehler zurück zur Kommandoschleife
		ld	e, (hl)
		inc	hl
		dec	bc
		ld	d, (hl)
		ld	(ARG1),	de
		inc	hl
		dec	bc
		ld	de, unk_5530
loc_520E:	ld	a, (de)
		cp	(hl)
		jr	nz, loc_51FB
		cp	20h ; ' '
		jr	c, loc_521B
		inc	hl
		dec	bc
		inc	de
		jr	loc_520E
loc_521B:	call	sub_45F5	; suche Zeile ARG1
loc_521E:	call	sub_4CAD	; Anzeige Zeile
		jr	loc_521E

;---------------------------------------
; Neunumerierung ab Zeile bis Quellende
;
; N a s - ab Zeile a mit einer Lücke von s Zeilen zur vorhergehenden a Zeile.
;
; Dieses Kommando ist immer dann nützlich, wenn man noch nachträglich etwas in die
; Quelle einfügen muß und dafür Platz benötigt.

NNUM:		ld	hl, (BUFBEG)	; Anfangsdresse
		cp	2		; in A = Anz. Param.
		jp	nz, loc_4B2E	; error 99 illegal command
		inc	hl
		inc	hl
		call	sub_442B	; Suche nächste Zeile
		ret	z		; Textende erreicht
loc_5231:	ld	e, (hl)
		inc	hl
		ld	d, (hl)		; DE=Zeilennummer
		call	loc_442D	; suche nächste Zeile
		jr	nz, loc_5231	; wiederholen bis Textende erreicht
		ld	hl, (ARG2)	; Schrittweite s
		call	sub_5257	; letzte Zeilennr + s > 9999?
		ld	a, 1		; Error 01 - Zeilennummerüberlauf
		jp	c, loc_4B30	; Fehler ausgeben
		call	sub_45F5	; suche Zeile ARG1
loc_5247:	call	sub_5253
		ld	(hl), e
		inc	hl
		ld	(hl), d
		call	sub_442B	; Suche nächste Zeile
		ret	z		; Textende erreicht
		jr	loc_5247
; hole Zeilennummer + ARG2
sub_5253:	ld	e, (hl)
		inc	hl
		ld	d, (hl)
		dec	hl
; BCD Addition HL + ARG2
sub_5257:	push	hl
		ld	hl, (ARG2)
		or	a
		ld	a, l
		add	a, e
		daa
		ld	e, a
		ld	a, h
		adc	a, d
		daa
		ld	d, a
		pop	hl
		ret

;---------------------------------------
; Teile einer EDAS-Quelle verschieben
;
; C a e x - Verschiebe von Zeile a bis Zeile e nach Zeile x und folgende.
;
; Hiermit kann man Teilstücke des Quelltextes nachträglich an eine andere Stelle
; in der Quelle bringen. Die Zeilen von a bis e werden ab der Zeile x mit der
; Schrittweite 1 neu plaziert.
;
MOVE:		cp	3
		jp	nz, loc_4B2E
		set	3, (iy+1)	; Assembleroptionen
		ld	hl, ARG1
		ld	de, word_54B0
		ld	bc, 6
		ldir
		ld	de, (word_54B4)
		ld	hl, (word_54B2)
		or	a
		sbc	hl, de
		jr	c, loc_5290
		ld	hl, (word_54B0)
		ex	de, hl
		sbc	hl, de
		ld	a, 8
		jr	nc, loc_52FC
loc_5290:	ld	hl, (BUFBEG)	; Anfangsdresse
		call	sub_5303
		ld	(ARG1),	hl
		ld	(ARG2),	hl
		push	de
		call	sub_4A3A
		pop	de
		ld	hl, (word_54B4)
		ld	(data+1), hl
		ld	(ARG1),	hl
		ld	(word_5510), hl	; Zeilennummer
		ld	hl, loc_52B9
		ld	(word_54B6), hl
		;;ld	hl, unk_54C0
		ld	hl, unk_54C0+2
		jp	loc_4A8C
loc_52B9:		ld	sp, iy
		ld	hl, (BUFBEG)	; Anfangsdresse
		call	sub_52CA
		jr	nz, loc_5290
loc_52C3:		res	3, (iy+1)	; Assembleroptionen
		jp	loc_4BE6
sub_52CA:	; FUNCTION CHUNK AT 52C3 SIZE 00000007 BYTES
		push	hl
		ld	hl, (word_54B0)
		inc	hl
		ld	(ARG1),	hl
		pop	hl
		call	loc_45F8
		cp	(hl)
		ret	z
		push	af
		ld	(word_54B0), de
		ld	hl, (word_54B2)
		or	a
		sbc	hl, de
		jr	c, loc_52C3
		ld	hl, (word_54B4)
		ld	a, l
		inc	a
		daa
		ld	l, a
		jr	nc, loc_52F7
		or	a
		ld	a, h
		inc	a
		daa
		ld	h, a
		ld	a, 1
		jr	c, loc_52FC
loc_52F7:		ld	(word_54B4), hl
		pop	af
		ret
;
loc_52FC:	res	3, (iy+1)	; Assembleroptionen
		jp	loc_4B30	; Fehler ausgeben

;
sub_5303:	push	hl
		ld	hl, (word_54B4)
		ld	(ARG1),	hl
		push	hl
		call	sub_45F5	; suche Zeile ARG1
		cp	(hl)
		pop	hl
		jr	z, loc_5319
		or	a
		sbc	hl, de
		ld	a, 7
		jr	nc, loc_52FC
loc_5319:		ld	hl, (word_54B0)
		ld	(ARG1),	hl
		pop	hl
		call	loc_45F8
		push	de
		push	hl
		call	sub_442B	; Suche nächste Zeile
		dec	hl
		dec	hl
		pop	bc
		inc	bc
		;ld	de, unk_54C0
		ld	de, unk_54C0+2
loc_532F:		ld	a, (hl)
		ld	(de), a
		or	a
		sbc	hl, bc
		add	hl, bc
		jr	z, loc_533B
		dec	hl
		inc	de
		jr	loc_532F
loc_533B:		pop	hl
		ret
sub_533D:		ld	hl, (word_54B8)	; Adresse Anfang Sekundärquelle (SEC)
		ld	a, h
		and	l
		inc	a
		ld	a, 9
		jp	z, loc_52FC
		ret

;---------------------------------------
; Einfügen einer Zweit-Quelle
;
; T a e x - Plaziere die Zeilen a bis e der Zweitquelle ab Zeile x in der
; aktuellen Quelle.
;
; Dieses Kommando ermöglicht es, die gesamte Zweitquelle oder Teile davon in die
; aktuelle Quelle einzufügen. Bei längeren Quelltexten kann das ohne weiteres
; längere Zeit beanspruchen. Ist das Kommando ausgeführt, so erscheint wieder der
; blinkende Kursor.

TRANS:		cp	3
		jp	nz, loc_4B2E
		set	3, (iy+1)	; Assembleroptionen
		ld	hl, ARG1
		ld	de, word_54B0
		ld	bc, 6
		ldir
		call	sub_533D
loc_5360:		ld	hl, (word_54B8)	; Adresse Anfang Sekundärquelle (SEC)
		call	sub_5303
		ld	hl, (word_54B4)
		ld	(data+1), hl
		ld	(ARG1),	hl
		ld	(word_5510), hl	; Zeilennummer
		ld	hl, loc_537E
		ld	(word_54B6), hl
		;ld	hl, unk_54C0
		ld	hl, unk_54C0+2
		jp	loc_4A8C
loc_537E:		ld	sp, iy
		ld	hl, (word_54B8)	; Adresse Anfang Sekundärquelle (SEC)
		call	sub_52CA
		jr	nz, loc_5360
		res	3, (iy+1)	; Assembleroptionen
		jp	loc_4BE6

;---------------------------------------
; Vereinbaren einer Zweit-Quelle (SEC)
;
; S a e - Puffervereinbarung für die Zweitquelle von aaaa bis eeee (hexadezimal).
;
; Vor dieser Vereinbarung muß sich ab aaaa bereits eine Quelle oder mindestens ein
; Quellkopf befinden. Hinter SEC: erscheint aa, die Anfangsadresse des
; Zweitquellenpuffers. Man muß darauf achten, daß sich der Zweitquellenpuffer
; nicht mit dem aktuellen Quellpuffer oder mit EDAS*4 selbst überschneidet.
; Hierbei darf eeee auch größer sein als das Ende einer von Kassette eingelesenen
; Zweitquelle, will man diese dann noch erweitern, so ist es auch erforderlich.
;

SEC:		cp	2
		jp	nz, loc_4B2E
		ld	de, (ARG2)		; eeee
		xor	a			; Cy=0; A=0
		sbc	hl, de
		add	hl, de
		jp	nc, loc_4B30		; Fehler ausgeben
						; Error 00 - Speicherüberlauf
		ld	(word_54B8), hl		; Adresse Anfang Sekundärquelle (SEC)
		ld	(word_54BA), de		; Adresse Ende Sekundärquelle (SEC)
		ret

;---------------------------------------
; Editieren in der Zweit-Quelle
;
; E - Austausch aktuelle Quelle gegen Zweit-Quelle
;
; Hiermit wird EDAS*4 der Pufferbereich der Zweitquelle zugewiesen, so daß nun in
; dieser gearbeitet werden kann. Ein weiteres "E" macht diese Zuweisung dann
; wieder rückgängig. Die Angaben hinter FREE: und SEC: geben dabei immer Auskunft,
; in welcher Quelle gerade gearbeitet wird. Hinter FREE: steht immer der erste
; freie Speicherplatz in der aktuellen Quelle, hinter SEC: wird der Anfang der
; zuletzt eingestellten Zweitquelle angegeben.
;
SWAP:		call	sub_533D
		ld	de, (BUFBEG)		; Anfangsdresse
		ld	(BUFBEG), hl		; Anfangsdresse
		ld	(word_54B8), de		; Adresse Anfang Sekundärquelle (SEC)
		ld	hl, (word_54BA)	; Adresse Ende Sekundärquelle (SEC)
		ld	de, (BUFEND)
		ld	(BUFEND),	hl
		ld	(word_54BA), de	; Adresse Ende Sekundärquelle (SEC)
;;		ret
		;
		call    sub_5480
		jp      sub_55AC
		;
loc_53C4:	bit	3, (iy+1)	; Assembleroptionen
		jp	z, loc_4803
		ld	hl, (word_54B6)
		jp	(hl)


;---------------------------------------
;
loc_53CF:	push	hl
		ld	hl, (word_5512)	; ausgegebene Zeilennummer
		ld	de, (word_54BC)
		or	a
		sbc	hl, de
		add	hl, de
		jr	c, loc_53E5
		ld	de, (word_54BE)
		or	a
		ex	de, hl
		sbc	hl, de
loc_53E5:	pop	hl
		jp	c, loc_4CE2
		ex	de, hl
		ld	hl, bws(31,0)
		jp	loc_4CC0

;---------------------------------------
; Quelltext assemblieren
;
; A - Assembliere den gesamten Quelltext.
;
; A a - Assembliere von Zeile a bis Ende.
;
; A a e - Assembliere von Zeile a bis Zeile e.
;
; Entsprechend den Optionsbits und den Argumenten wird der Assemblerlauf
; durchgeführt. Ist das Druckbit gesetzt und nur eine teilweise Assemblierung
; angewiesen, so wird die gesamte Quelle assembliert, aber nur der angegebene Teil
; als Liste ausgegeben, so daß man bei längeren Quellen auch nur Auszüge drucken
; kann.
;
ASSEMBLE:	call    sub_577F
		ld      (unk_FF31), a
		;
		ld	hl, (ARG1)	; Zeilennummer a
		ld	(word_54BC), hl
		ld	hl, (ARG2)	; Zeilennummer e
		ld	(word_54BE), hl
loc_53FC:	bit	2, (iy+1)	; Assembleroption Drucken?
		jr	z, loc_5414	; nein
		ld	hl, 0		; sonst alle Zeilen von 0000
		ld	(ARG1),	hl
		dec	hl		; bis 0FFFF
		ld	(ARG2),	hl
		call	sub_5440	; Druckerinitialisierung
		ld	a, CR
		call	unk_5504	; Druckerausgabe
loc_5414:	jp	loc_4D47

;
sub_5417:	ld	hl, (cupos)
		ld	c, a
		ld	de, bws(30,0)
loc_541E:		ld	a, (de)
		cp	3Bh ; ';'
		ld	a, c
		jr	z, loc_542C
		or	a
		sbc	hl, de
		add	hl, de
		dec	de
		jr	nz, loc_541E
		ret
loc_542C:		cp	41h ; 'A'
		ret	c
		cp	5Bh ; '['
		jr	nc, loc_5436
		set	5, a
		ret
loc_5436:		cp	61h ; 'a'
		ret	c
		cp	7Bh ; '{'
		ret	nc
		res	5, a
		ret
;---------------------------------------
; die Erweiterungen von picoEDAS

sub_545D:	call	sub_4466	; Bufferende berechnen
		ld	de, (BUFBEG)
		or	a
		sbc	hl, de
		ld	a, 6
		cp	l
		ret
;
sub_546B:	ld	hl, 10BEh
		call	819h		; GETTX0 EingabeTxt ab HL vom BWS
		ret
;
sub_5472:	ld	b, 10h
		call	sub_557B
loc_5477:	ld	a, (hl)
		cp	20h ; ' '
		ret	z
		rst	10h
		inc	hl
		djnz	loc_5477
		ret
;
sub_5480:	ld	b, 10h
		ld	hl, unk_FF00
		ld	de, unk_FF10
loc_5488:	ld	a, (hl)
		push	af
		ld	a, (de)
		ld	(hl), a
		pop	af
		ld	(de), a
		inc	hl
		inc	de
		djnz	loc_5488
		ret

;---------------------------------------
; - alphabetisch sortiert
; l+aufsteigend nach Datum sortiert
; l-absteigend nach Datum sortiert
; l DATEINAME Quelltext laden: DATEINAME.E

E_LOAD:		call	sub_546B
		ld	a, h
		or	l
		jr	z, loc_54B3
		call	sub_557B
		ld	a, '+'
		cp	(hl)
		jr	z, loc_54A9
		ld	a, '-'
		cp	(hl)
		jr	z, loc_54AE
		jr	loc_54C2
loc_54A9:	ld	hl, 1
		jr	loc_54B3
loc_54AE:	ld	hl, 2
		jr	loc_54B3
loc_54B3:	ld	(ARG1), hl
		call	83Ah		; FGETLS Anforderung Dateiliste
loc_54B9:	call	84Ch		; RDBYTE Byte v. ZCOM abholen
		jp	z, loc_555F
		rst	10h
		jr	loc_54B9
loc_54C2:	call	sub_545D
		jr	z, loc_54F5
		rst	18h
		db "Aktuelle Quelle ueberschreiben? (J)",0A0h
		rst	8
		or	20h
		cp	'j'
		jr	nz, loc_555F
		rst	18h
		db 8Dh
loc_54F5:	call	sub_557B
		dec	hl
		ld	(hl), 45h ; 'E'
		inc	hl
		ld	de, unk_FF20
		ld	bc, 10h
		ldir
		call	sub_5568
		rst	18h
		db "Laden Quelle:",0A0h
		call	sub_5472
		call	82Bh		;FOPENR	ext.Datei open	read
		call	834h		;FGETRE	ResCode FileOP	Z/0000=Ok
		jp	nz, loc_5581
		bit	6, (iy+1)
		jr	nz, loc_552E
		ld	hl, (BUFBEG)
		ld	(ARG1), hl
loc_552E:	call	831h		;FBLKR	Speicherblock lesen
		call	834h		;FGETRE	ResCode FileOP	Z/0000=Ok
		jp	nz, loc_5581
		ld	hl, unk_FF20
		ld	de, unk_FF00
		ld	bc, 10h
		ldir
		call	sub_55AC
		ld	a, 1
		ld	(unk_FF32), a
		call	sub_5562
		bit	6, (iy+1)
		jr	z, loc_555F
		ld	hl, (ARG1)
		ld	(BUFBEG), hl
		ld	hl, (ARG3)
		ld	(BUFEND), hl
loc_555F:	rst	18h
		db 8Dh
		ret
;
sub_5562:	rst	18h
		db 0Dh,"ok",0AEh
		ret
;
sub_5568:	call	sub_557B
		ld	b, 0Eh
loc_556D:	ld	a, (hl)
		cp	20h ; ' '
		jr	z, loc_5575
		inc	hl
		djnz	loc_556D
loc_5575:	ld	(hl), 2Eh ; '.'
		inc	hl
		ld	(hl), 45h ; 'E'
		ret
;
sub_557B:	ld	hl, 9
		jp	813h		; FGETSY Re:Zeiger auf SysRAM
;
loc_5581:	ld	a, h
		add	a, l
		cp	3
		jr	nz, loc_559E
		rst	18h
		db " -> nicht gefunden!",8Dh
		jr	loc_555F
loc_559E:	rst	18h
		db 0Dh,"SD-",80h
		call	error
		call	7F1h
		jr	loc_555F
;
sub_55AC:	ld	hl, (cupos)
		push	hl
		ld	hl, 17BFh
		ld	(cupos), hl
		rst	18h
		db 0BCh
		ld	b, 10h
		ld	hl, unk_FF00
loc_55BD:	ld	a, (hl)
		cp	20h ; ' '
		jr	z, loc_55C6
		rst	10h
		inc	hl
		djnz	loc_55BD
loc_55C6:	rst	18h
		db 0BEh
		ld	a, b
		or	a
		jr	z, loc_55D0
loc_55CC:	rst	18h
		db 0A0h
		djnz	loc_55CC
loc_55D0:	pop	hl
		ld	(cupos), hl
		ret

;---------------------------------------
; s Quelltext unter dem aktuellen Namen sichern
; s DATEINAME Quelltext sichern als ...

E_SAVE:		call	sub_545D
		jr	nz, loc_55EA
		rst	18h
		db "Keine Quelle!",8Dh
		ret
;
loc_55EA:	call	sub_565D
		call	sub_5772
		ld	(ARG1), hl
		ld	(ARG2), de
		ld	hl, (BUFEND)
		ld	(ARG3), hl
		call	sub_557B
		dec	hl
		ld	(hl), 45h ; 'E'
		inc	hl
		ld	a, (byte_5FFC)
		or	a
		ld	de, unk_FF00
		jr	z, loc_5610
		ld	de, unk_FF20
loc_5610:	ld	bc, 10h
		ldir
		call	sub_5568
		rst	18h
		db "Sichern Quelle:",0A0h
		call	sub_5472
		call	822h		; FOPENW ext. Datei open write
		call	828h		; FBLKW	 Speicheblock in ext.schr
		call	834h		; FGETRE ResCode FileOP	Z/0000=Ok
		jp	nz, loc_5581
		call	sub_5562
		ld	a, (unk_FF32)
		or	a
		jr	nz, loc_5657
		ld	a, 1
		ld	(unk_FF32), a
		ld	a, (byte_5FFC)
		or	a
		jr	z, loc_5657
		ld	hl, unk_FF20
		ld	de, unk_FF00
		ld	bc, 10h
		ldir
loc_5657:	call	sub_55AC
		jp	loc_555F
;
sub_565D:	call	sub_546B
		ld	a, h
		or	l
		jr	nz, loc_568B
		ld	a, (unk_FF32)
		or	a
		jr	nz, loc_567E
		rst	18h
		db "Dateiname fehlt!",8Dh
		pop	hl
		ret
;
loc_567E:	call	sub_557B
		ld	de, unk_FF00
		ex	de, hl
		ld	bc, 10h
		ldir
		ret
;
loc_568B:	ld	a, (unk_FF32)
		or	a
		ret	z
		ld	a, 2
		ld	(unk_FF32), a
		ret

;---------------------------------------
; B Maschinencode unter dem Namen der Quelldatei sichern
; B DATEINAME Maschinencode sichern als ...

E_MCSAV:	bit	1, (iy+1)
		jp	z, loc_4963
		call	sub_565D
		ld	hl, (mc_ofs)
		ld	a, h
		or	l
		jr	z, loc_56C9
		rst     18h
		db "MC mit Offset nicht zu sichern!",8Dh
		ret
;
loc_56C9:	ld	hl, (unk_54C0)
		ld	(ARG1), hl
		ld	hl, (word_550C)
		dec	hl
		ld	(ARG2), hl
		ld	hl, (word_550E)
		ld	(ARG3), hl
		inc	hl
		ld	a, h
		or	l
		jp	z, loc_4963
		call	sub_557B
		dec	hl
		ld	(hl), 4Dh ; 'M'
		call	sub_577F
		ld	b, a
		ld	a, (unk_FF31)
		cp	b
		jr	z, loc_571D
		rst	18h
		db "Quelle veraendert seit letztem ASM-Lauf!",8Dh
		ret
;
loc_571D:	rst	18h
		db "Sichern MC:",0A0h
		call	sub_5472
		call	822h		; FOPENW ext. Datei open write
		call	828h		; FBLKW	 Speicheblock in ext.schr
		call	834h		; FGETRE ResCode FileOP	Z/0000=Ok
		jp	nz, loc_5581
		call	sub_5562
		jp	loc_555F

;---------------------------------------
; D xxxx setzt Pufferobergrenze neu auf xxxx

E_BUFEND:	cp	0
		jp	z, loc_4B2E	; Error 99
		push	hl
		call	sub_4466	; Bufferende berechnen
		inc	hl
		ex	de, hl
		pop	hl
		or	a
		sbc	hl, de
		add	hl, de
		jr	c, loc_576D
		ld	de, loc_4000
		sbc	hl, de
		add	hl, de
		jr	c, loc_5769
		ld	de, unk_6000
		sbc	hl, de
		add	hl, de
		jr	c, loc_576D
		ld	de, word_54B0
		sbc	hl, de
		add	hl, de
		jr	nc, loc_576D
loc_5769:	ld	(BUFEND), hl
		ret
;
loc_576D:	ld	a, 8
		jp	loc_4B30	; Fehler ausgeben
;
sub_5772:	ld	hl, (BUFBEG)
		push	hl
		ld	e, (hl)
		inc	hl
		ld	d, (hl)
		pop	hl
		ex	de, hl
		add	hl, de
		dec	hl
		ex	de, hl
		ret
;
sub_577F:	call	sub_5772
		xor	a
loc_5783:	xor	(hl)
		inc	hl
		sbc	hl, de
		add	hl, de
		jr	c, loc_5783
		ret

;---------------------------------------
; ? Hilfe: Kommandoübersicht und Versionsanzeige
; ?F, ?O, ?S, ?T, ?X Fehlercodes, Optionen, Syntaxbesonderheiten, Tastaturbefehle, SD-Kommandos
;
E_HELP:		ld	hl, 10BEh
		ld	a, (hl)
		cp	'F'
		jp	z, loc_5958	; FEHLER-CODES
		cp	'S'
		jp	z, loc_5C27	; SYNTAXTIPPS
		cp	'T'
		jr	z, loc_57AA	; TASTATUR
		cp	'X'
		jp	z, loc_5D8D	; SD-FUNKTIONEN
		cp	'O'
		jp	z, loc_585A	; OPTIONEN
		jp	loc_5E3D
;
loc_57AA:	rst	18h
		db "<TASTATUR>",0Dh
		db "^H  Kursor <-",0Dh
		db "^I  Kursor ->",0Dh
		db "^E  Einfuegen (auch ",27h,"Einfg",27h,")",0Dh
		db "^S  Streichen (auch ",27h,"Entf",27h," und ^D)",0Dh
		db "^R  Backspace (auch <===)",0Dh
		db "^T  Kursor hinter Zeilennummer",0Dh
		db "^W  Tabulator",8Dh
		ret

loc_585A:	rst	18h
		db "<OPTIONEN>",0Dh
		db "+01  Unterdruecke Assemblerliste",0Dh
		db "+02  Maschinencode zum Speicher",0Dh
		db "+04  Drucke Assemblerliste",0Dh
		db "+10  Aktiviere 2.Lauf bei Fehlern im 1.Lauf",0Dh
		db "+20  relat. Sprungdistanz absolut (Marke-$)",0Dh
		db "+40  Lade Quelle an Headeradresse",0Dh
		db "+80  Ausgabe Symboltabelle",8Dh
		ret

loc_5958:	rst	18h
		db "<FEHLER-CODES>",0Dh
		db "00 Editierpuffer voll",0Dh
		db "01 Zeilennummer >9999",0Dh
		db "02 I-Kommando >9999",0Dh
		db "03 Zeile existiert nicht",0Dh
		db "04 G und B unzulaess. (Quelle o. ENT fehlt o. nicht assembl.)",0Dh
		db "05 Reaktivieren unmoeglich",0Dh
		db "06 Schrittweite 0",0Dh
		db "07 Bereich Zeilennr. zu klein",0Dh
		db "08 unzulaess. Argument ",0Dh
		db "09 Zweitquelle nicht aktiv",0Dh
		db "10 unzulaess. Befehlszeile",0Dh
		db "20 unbekannte Mnemonik",0Dh
		db "21 Operand passt nicht zu Code",0Dh
		db "22 IX/IY-Befehl nicht moeglich",0Dh
		db "23 Operand/Distanz zu gross",0Dh
		db "24 zu viele Register",0Dh
		db "25 unzulaess. Operandenkombination",0Dh
		db "26 unzulaess. Zeichen",0Dh
		db "27 unzulaess. Operand",0Dh
		db "28 Klammerfehler",0Dh
		db "30 Marke nicht gefunden",0Dh
		db "31 Marke mehrfach definiert",0Dh
		db "40 falscher Pseudobefehl",0Dh
		db "41 unzulaess. Vorwaertsreferenz",0Dh
		db "60/61 MC wuerde EDAS ueberschreiben",0Dh
		db "99 unzulaess. Kommando",8Dh
		ret

loc_5C27:	rst	18h
		db "<SYNTAXTIPPS>",0Dh
		db "Pseudobefehle:",0Dh
		db "ORG  Adresszuweisung nachfolg. Code",0Dh
		db "ENT  Startadr. G- und B-Kommando",0Dh
		db "SKIP erzeuge Leerzeile in Listing",0Dh
		db "$    Adresszaehlersymbol",0Dh
		db 0Dh
		db "Marken:",0Dh
		db "  max. 6 Zeichen, keine Registernamen",0Dh
		db "  kein ",27h,":",27h," (M1=M1:)",0Dh
		db 0Dh
		db "Ausdruecke:",0Dh
		db "  Hex-Angaben:  z.B. 1900H oder #1900",0Dh
		db "  Einzelzeichen: nur 1x \" voran:",0Dh
		db "        z.B.  LD  A,\"* ",0Dh
		db "        nicht LD  A,\"*\"",8Dh
		ret

loc_5D8D:	rst	18h
		db "<SD-FUNKTIONEN>",0Dh
		db "Format:  Headersave",0Dh
		db 0Dh
		db "l name   Quelle laden   nur l => DIR",0Dh
		db "s (name) Quelle sichern (als...)",0Dh
		db "  .E  wird automatisch ergaenzt!",0Dh
		db "B (name) MC sichern     (als...)",0Dh
		db 8Dh
		ret

loc_5E3D:	rst	18h
		db "<Kommandos>",0Dh
		db "A,C,E,F,G,H,I,J,K,M,N,O,P,Q,R,S,T,V,W,X,Y,Z,+,-,/",0Dh
		db "-> Siehe Originalbeschreibung!",0Dh
		db 0Dh
		db "l   Laden   Quelle/DIR",0Dh
		db "s   Sichern Quelle",0Dh
		db "B   Sichern MC",0Dh
		db "D   Pufferobergrenze setzen",0Dh
		db 0Dh
		db "<Hilfeseiten>",0Dh
		db "?F  Fehler-Codes",0Dh
		db "?O  Optionen",0Dh
		db "?S  Syntaxtipps",0Dh
		db "?T  Tasten",0Dh
		db "?X  SD-Arbeit",0Dh
		db 0Dh
		db 0Dh
		db "picoEDAS*4 V.2.0",0Dh	; picoEDAS4
		;db "picoEDAS*C V.2.0",0Dh	; picoEDASC
		db "pico-",80h
		call	837h
		rst	18h
		db 0Dh
		db 0Dh
		db "AC1-Anpassung ",27h,"2023: DL7UFH, DL7VMD, DG3RW",8Dh
		ret
;
aNamenlos:	db "NAMENLOS "
;
loc_5F9B:	ld	hl, (aPicoac1N)
		ld	de, (0FFEh)
		ld	a, h
		cp	d
		jr	nz, loc_5FAB
		ld	a, l
		cp	e
		jr	nz, loc_5FAB
		ret
;
loc_5FAB:	rst	18h
		db "Kein "
aPicoac1N:	db "picoAC1!",8Dh
		jp	7FDh
;
		db    0
		db    0

;---------------------------------------
unk_543F:	;; db  49h				; chksum
		db  48h		; picoEDAS4
		;db	0BFh	; picoEDAS4

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

;----------------------------------------
		; org 5FE9h

		db 0,9,'e',0Dh
		ld      hl, (ARG2)
		ld      (ARG3), hl
		ld      hl, (ARG1)
		ld      (ARG2), hl
		jp      loc_4000
;
byte_5FFC:      db 0FFh

;---------------------------------------
; Druckerinitialisierung
sub_5440:	jp	MS30

; E N D E

unk_6000:	equ $		; Prg.Ende

;------------------------------------------------------------------------------
; RAM

;;		org	54B0h
;RAMA		equ 5400h
RAMA		equ 0FD00h		; picoEDAS

word_54B0:      equ RAMA+0B0h		; C: Anfangszeile
word_54B2:      equ RAMA+0B2h		; C: Endezeile
word_54B4:      equ RAMA+0B4h		; C: Zielzeile
word_54B6:      equ RAMA+0B6h
word_54B8:      equ RAMA+0B8h		; Adresse Anfang Sekundärquelle (SEC)
word_54BA:      equ RAMA+0BAh		; Adresse Ende Sekundärquelle (SEC)
word_54BC:      equ RAMA+0BCh		; Adresse Assemblieren Anfang
word_54BE:      equ RAMA+0BEh		; Adresse Assemblieren Ende
unk_54C0:       equ RAMA+0C0h		; Beginn Eingabepuffer Zeile 61 Bytes
unk_54FF:       equ RAMA+0FFh		; Buffer f. Code (down)

;
BUFBEG:		equ RAMA+100h		; Anfangsdresse Textpuffer
BUFEND:		equ RAMA+102h		; Endadresse Textpuffer
unk_5504:       equ RAMA+104h		; Druckerausgabe
byte_5507:      equ RAMA+107h		; Assembleroptionen
byte_5508:      equ RAMA+108h		; Ausgabeverzögerung Druck
byte_5509:	equ RAMA+109h		; Ausgabeverzögerung BWS
mc_ofs:      	equ RAMA+10Ah		; offset for code generation
word_550C:      equ RAMA+10Ch		; letzte Adresse MC+1 (END:)
word_550E:      equ RAMA+10Eh		; ENTry-Adresse
word_5510:      equ RAMA+110h		; aktuelle Zeilennummer
word_5512:      equ RAMA+112h       	; ausgegebene Zeilennummer
byte_5514:      equ RAMA+114h		; Seitengröße bei Schirmausgabe (25 Zeilen)
unk_5515:       equ RAMA+115h		; kdotab
word_5517:      equ RAMA+117h		; unk_40AA Parse Tree Mnemonics
word_5519:      equ RAMA+119h		; unk_428F Parse Tree Register
word_551B:      equ RAMA+11Bh		; unk_42F4 Tokenklassen Register,Flags
unk_551D:       equ RAMA+11Dh		; Tabelle Zeichenklassen Spezialzeichen
unk_552D:       equ RAMA+12Dh       	; sonstiges Spezialzeichen (akt. Zeichen)
unk_5530:       equ RAMA+130h		; 6 Zeichen Marke
unk_556E:       equ RAMA+16Eh		; Find Zeichenkette, Buffer bis unk_552D (41H Zeichen)
					; Textanfang ('/'), Buffer enthält den Text revers, absteigende Adr!
unk_556F:       equ RAMA+16Fh		; 6 Zeichen vom Anfang des Buffers (Kopf), f. REUSE
p_BUFBEG:	equ RAMA+175h		; Sicherung BUFBEG f. REUSE
unk_5577:       equ RAMA+177h		; Seitengröße
word_5578:      equ RAMA+178h       	; Adr. Ret-Funktion nach Fehler
unk_557A:       equ RAMA+17Ah
word_557C:      equ RAMA+17Ch
word_5582:      equ RAMA+182h
byte_5584:      equ RAMA+184h		; Schrittweite (RENUM, INUM)
word_5585:      equ RAMA+185h
word_5587:      equ RAMA+187h
word_5589:      equ RAMA+189h
unk_559B:       equ RAMA+19Bh       	; loc_4D0E  - Ausgabe Enter+ggf. Abbruch bei Strg-C
					; o. loc_4CA2 - Ausgabe in Speicher (ARG3)
unk_559E:       equ RAMA+19Eh		; Kdo-Buchstabe
byte_559F:      equ RAMA+19Fh		; Anzahl Kdo.Argumente
orig_sv:	equ RAMA+1A0h		; Merker für orig. Sprungverteiler
					; 12h Byte
errcnt:      	equ RAMA+1B2h		; Fehlerzähler
unk_55B4:       equ RAMA+1B4h		; inch0 Repeat-Timer
;iy-12h
;iy-0Fh	; 55EF
;iy-0Eh	; 55F0	bei Tokenklasse 72..78 als 01..04
;iy-0Dh	; 55F1	byte modifier register o. flags
;iy-0Ch	; 55F2
;iy-0Bh	; 55F3
;iy-0Ah	; 55F4
;iy-9	; 55F5
;iy-8	; 55F6
word_55F2:      equ RAMA+1F2h
unk_55F6:       equ RAMA+1F6h
byte_55FE:	equ RAMA+1FEh		; IY, TOP Stack
unk_55FF:       equ RAMA+1FFh		; IY+1 = Assembleroptionen
					; Bit0=nur fehlerhafte Zeilen beim Listing
					; Bit1=MC in Speicher laden
					; Bit2=drucken
					; Bit3=(intern benutzt!)
					; Bit4=erzwinge 2. Lauf
					; Bit5=relative Sprungdistanzen absolut
					; Bit6=
					; Bit7=Ausgabe Symboltabelle
;
unk_5600:       equ RAMA+200h		; Ramende ?

txtbuffer:	equ	6000h

;--picoAC1
unk_FF00	equ 0FF00h		; Namebuffer
unk_FF10        equ 0FF10h		; Namebuffer
unk_FF20        equ 0FF20h		; Namebuffer
unk_FF31        equ 0FF31h
unk_FF32        equ 0FF32h

		end

--
Token:

.0 r  		Bits 5..3 ->  38h in unk_42F4
.3 r'  A..L	Bits 2..0 ->  08h in unk_42F4
.4 n
.6 flag
.8 ofs
.9 flag n,nz,c,nc
.c rr BC,DE,HL,SP ('dd')
.f rr BD,DE,HL,AF ('qq')

20 B
22 D
24 E
26 H
28 L
2A	NZ
2C	Z
2E	NC
30	BC
31,B1	(BC)
32	PO
34	PE
36	P
38	M

40..5F	die ASCII-Zeichen (Buchstaben)

60,E0	A
62,E2	HL	+IX,IY
63	(HL)
64	DE
65,e5	(DE)
66	C
67,e7	(C)
68	AF
6A	SP
6Bh	(SP)
6c,ec	I
6e,ee	R

72,F2 addr
73,F3 (addr)
74,F4 n
75 F5 (n)
78,F8 ofs
7A bit 0..7
7C,FC rst 08..38
7E,FE n 0..2 (IM-Mode)

--

mc
header byte in Parse Tree
bit 7 (8x) 	assembler directive/special code
bit 6 (4x)	special IX/IY	nur bei jp (hl) (geht hier nur  ohne +d)!
bit 4 (1x)	ed prefix
bit 3 (x8+)	cb prefix

assembler directives/special code
EQU 	0
ORG 	1
DEFS	2
DEFM	3
DEFB	4
DEFW	5
ENT	80h
SKIP	81h

nas mc code:
RCAL ofs	-> rst 10h n
SCAL code	-> rst 18h n

