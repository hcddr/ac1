; B A S I C  -  AC 1   V 1.1  (C) Y21SO 1986

; File Name   :	D:\hobby3\ac1\basic\BASIC+EDITV1.1_4000_5FFF_4000.bin
; reassembled: V.Pohlers, 11/2018
; last change: 07.11.2018 12:23:15
; based on nascom basic (microsoft basic 4.7)

; NASCOM ROM BASIC Ver 4.7, (C) 1978 Microsoft
; Scanned from source published in 80-BUS NEWS from Vol 2, Issue 3
; (May-June 1983) to Vol 3, Issue 3 (May-June 1984)
; adapted to arnold assembler vpohlers

		cpu	z80

; GENERAL EQUATES

CTRLC   EQU    03H             ; Control "C"
CTRLG   EQU    07H             ; Control "G"
BKSP    EQU    08H             ; Back space
LF      EQU    0AH             ; Line feed
CS      EQU    0CH             ; Clear screen
CR      EQU    0DH             ; Carriage return
CTRLO   EQU    0FH             ; Control "O"
CTRLQ	EQU	11H             ; Control "Q"
CTRLR   EQU    12H             ; Control "R"
CTRLS   EQU    13H             ; Control "S"
CTRLU   EQU    15H             ; Control "U"
ESC     EQU    1BH             ; Escape
DEL     EQU    7FH             ; Delete

; BASIC WORK SPACE LOCATIONS

WRKSPC  EQU    6000H           ; <<<< BASIC Work space ** Rx buffer & Tx buffer located from 2100H **
USR     EQU    WRKSPC+3H       ; "USR (x)" jump
OTPORT  EQU    WRKSPC+6H       ; Port (p)
TRFLAG	EQU    WRKSPC+0AH
DIVSUP  EQU    WRKSPC+0BH       ; Division support routine
DIV1    EQU    WRKSPC+0CH      ; <- Values
DIV2    EQU    WRKSPC+10H      ; <-   to
DIV3    EQU    WRKSPC+14H      ; <-   be
DIV4    EQU    WRKSPC+17H      ; <-inserted
SEED    EQU    WRKSPC+19H      ; Random number seed
LSTRND  EQU    WRKSPC+3CH      ; Last random number
NULLS   EQU    WRKSPC+40H      ; Number of nulls
LWIDTH  EQU    WRKSPC+41H      ; Terminal width
COMMAN  EQU    WRKSPC+42H      ; Width for commas
CTLOFG  EQU    WRKSPC+43H      ; Control "O" flag
LINESC  EQU    WRKSPC+44H      ; Lines counter
LINESN  EQU    WRKSPC+46H      ; Lines number
RINPUT  EQU    WRKSPC+4AH      ; Input reflection
unk_604D	equ WRKSPC+4DH
unk_604E	equ WRKSPC+4EH
unk_6050	equ WRKSPC+50H
unk_6052	equ WRKSPC+52H
unk_6054	equ WRKSPC+54H
STRSPC  EQU    WRKSPC+56H      ; Bottom of string space
LINEAT  EQU    WRKSPC+58H      ; Current line number
BASTXT  EQU    WRKSPC+5AH      ; Pointer to start of program
BUFFER  EQU    WRKSPC+5DH      ; Input buffer
STACK   EQU    WRKSPC+62H      ; Initial stack
unk_60A6	equ WRKSPC+0A6H
CURPOS  EQU    WRKSPC+0A7H     ; <<<< Character position on line ** Top of TEMPSTACK **
LCRFLG  EQU    WRKSPC+0A8H     ; Locate/Create flag
TYPE    EQU    WRKSPC+0A9H     ; Data type flag
DATFLG  EQU    WRKSPC+0AAH     ; Literal statement flag
LSTRAM  EQU    WRKSPC+0ABH     ; Last available RAM
TMSTPT  EQU    WRKSPC+0ADH     ; Temporary string pointer
TMSTPL  EQU    WRKSPC+0AFH     ; Temporary string pool
TMPSTR  EQU    WRKSPC+0BBH     ; Temporary string
STRBOT  EQU    WRKSPC+0BFH     ; Bottom of string space
CUROPR  EQU    WRKSPC+0C1H     ; Current operator in EVAL
LOOPST  EQU    WRKSPC+0C3H     ; First statement of loop
DATLIN  EQU    WRKSPC+0C5H     ; Line of current DATA item
FORFLG  EQU    WRKSPC+0C7H     ; "FOR" loop flag
LSTBIN  EQU    WRKSPC+0C8H     ; Last byte entered
READFG  EQU    WRKSPC+0C9H     ; Read/Input flag
BRKLIN  EQU    WRKSPC+0CAH     ; Line of break
NXTOPR  EQU    WRKSPC+0CCH     ; Next operator in EVAL
ERRLIN  EQU    WRKSPC+0CEH     ; Line of error
CONTAD  EQU    WRKSPC+0D0H     ; Where to CONTinue
PROGND  EQU    WRKSPC+0D2H     ; End of program
VAREND  EQU    WRKSPC+0D4H     ; End of variables
ARREND  EQU    WRKSPC+0D6H     ; End of arrays
NXTDAT  EQU    WRKSPC+0D8H     ; Next data item
FNRGNM  EQU    WRKSPC+0DAH     ; Name of FN argument
FNARG   EQU    WRKSPC+0DCH     ; FN argument value
unk_60DE	equ WRKSPC+0DEH
FPREG   EQU    WRKSPC+0E0H     ; Floating point register
FPEXP   EQU    FPREG+3         ; Floating point exponent
SGNRES  EQU    WRKSPC+0E4H     ; Sign of result
PBUFF   EQU    WRKSPC+0E5H     ; Number print buffer
MULVAL  EQU    WRKSPC+0F2H     ; Multiplier
PROGST  EQU    WRKSPC+0F6H     ; Start of program text area
STLOOK  EQU    WRKSPC+15BH     ; Start of memory test

; BASIC ERROR CODE VALUES

NF      EQU    00H             ; NEXT without FOR
SN      EQU    02H             ; Syntax error
RG      EQU    04H             ; RETURN without GOSUB
OD      EQU    06H             ; Out of DATA
FC      EQU    08H             ; Function call error
OV      EQU    0AH             ; Overflow
OM      EQU    0CH             ; Out of memory
UL      EQU    0EH             ; Undefined line number
BS      EQU    10H             ; Bad subscript
DD      EQU    12H             ; Re-DIMensioned array
DZ      EQU    14H             ; Division by zero (/0)
ID      EQU    16H             ; Illegal direct
TM      EQU    18H             ; Type miss-match
OS      EQU    1AH             ; Out of string space
LS      EQU    1CH             ; String too long
ST      EQU    1EH             ; String formula too complex
CN      EQU    20H             ; Can't CONTinue
UF      EQU    22H             ; UnDEFined FN function
MO      EQU    24H             ; Missing operand

		org 4000h

COLD:   JP      STARTB          ; Jump in for cold start
WARM:   jr	BRKRET          ; Jump in for warm start
STARTB:
	ld	sp, unk_60A6	;?
	ld	hl, INITAB
	ld	de, WRKSPC
	ld	bc, 5Fh
	ldir
	ex	de, hl
	ld	sp, hl
	call	CLREG
	ld	(unk_60A6), a	;?
	ld	(PROGST), a
	call	CLS
MSIZE:  LD      HL,MEMMSG       ; Point to message
        CALL    PRS             ; Output "Memory size"
        CALL    PROMPT          ; Get input with '?'
        CALL    GETCHR          ; Get next character
        OR      A               ; Set flags
	ld	de, 0FFFFh
	jr	z, msiz1
	call	ATOH
	or	a
	jp	nz, SNERR
msiz1:	ld	hl, 5FFFh	; default memory end
MLOOP:  INC     HL              ; Next byte
	call	CPDEHL
        Jr      Z,SETTOP        ; Yes - 64K RAM
        LD      A,(HL)          ; Get contents
        LD      B,A             ; Save it
        CPL                     ; Flip all bits
        LD      (HL),A          ; Put it back
        CP      (HL)            ; RAM there if same
        LD      (HL),B          ; Restore old contents
        Jr      Z,MLOOP         ; If RAM - test next byte
	DEC     HL              ; Back one byte

SETTOP: LD      DE,STLOOK-1     ; See if enough RAM
        CALL    CPDEHL          ; Compare DE with HL
        Jr      C,MSIZE         ; Ask again if not enough RAM
        LD      DE,0-50         ; 50 Bytes string space
        LD      (LSTRAM),HL     ; Save last available RAM
        ADD     HL,DE           ; Allocate string space
        LD      (STRSPC),HL     ; Save string space
        CALL    CLRPTR          ; Clear program area
        LD      HL,(STRSPC)     ; Get end of memory
        LD      DE,-PROGST-17         ; Offset for free bytes
        ADD     HL,DE           ; Adjust HL
        CALL    PRNTHL          ; Output amount of free memory
        LD      HL,BFREE        ; " Bytes free" message
        CALL    PRS             ; Output string

WARMST: LD      SP,STACK        ; Temporary stack
BRKRET: CALL    CLREG           ; Clear registers and stack
        JP      PRNTOK          ; Go to get command line

MEMMSG:		db 0Dh,0Dh," * * *  B A S I C  -  AC 1   V 1.1  (C) Y21SO 1986  "
		db "* * *",0Dh,0Dh
		db "Memory end ",0

BFREE:		db " Bytes free ",0Dh,0Dh,0

; INITIALISATION TABLE -------------------------------------------------------

INITAB: JP      WARMST          ; Warm start jump
        JP      FCERR           ; "USR (X)" jump (Set to Error)
       		db    0
		db    0
		db    0
		db    0
		db    0
        SUB     0               ; Division support routine
        LD      L,A
        LD      A,H
        SBC     A,0
        LD      H,A
        LD      A,B
        SBC     A,0
        LD      B,A
        LD      A,0
        RET
        DB   0,0,0                   ; Random number seed table used by RND
        DB   035H,04AH,0CAH,099H     ;-2.65145E+07
        DB   039H,01CH,076H,098H     ; 1.61291E+07
        DB   022H,095H,0B3H,098H     ;-1.17691E+07
        DB   00AH,0DDH,047H,098H     ; 1.30983E+07
        DB   053H,0D1H,099H,099H     ;-2-01612E+07
        DB   00AH,01AH,09FH,098H     ;-1.04269E+07
        DB   065H,0BCH,0CDH,098H     ;-1.34831E+07
        DB   0D6H,077H,03EH,098H     ; 1.24825E+07
        DB   052H,0C7H,04FH,080H     ; Last random number
;--
        DB   1               ; POS (x) number (1)
        DB   3Fh             ; Terminal width (255 = no auto CRLF)
        DB   42              ; Width for commas (3 columns)
	db    0
        DW   12              ; Initial lines counter
        DW   12              ; Initial lines number
	db    0
	db    0
        JP      TTYLIN          ; Input reflection (set to TTY)
	db    0
	db    0
	db    0
	db    0
	db    0
	db    0
	db    0
	db    0
	db    0
        DW   STLOOK          ; Temp string space
        DW   -2              ; Current line number (cold)
        DW   PROGST+1        ; Start of program text
INITBE:

; END OF INITIALISATION TABLE ---------------------------------------------------

; RESERVED WORD LIST

WORDS:  DB   'E'+80H,"ND"
        DB   'F'+80H,"OR"
        DB   'N'+80H,"EXT"
        DB   'D'+80H,"ATA"
        DB   'I'+80H,"NPUT"
        DB   'D'+80H,"IM"
        DB   'R'+80H,"EAD"
        DB   'L'+80H,"ET"
        DB   'G'+80H,"OTO"
        DB   'R'+80H,"UN"
        DB   'I'+80H,"F"
        DB   'R'+80H,"ESTORE"
        DB   'G'+80H,"OSUB"
        DB   'R'+80H,"ETURN"
        DB   'R'+80H,"EM"
        DB   'S'+80H,"TOP"
        DB   'O'+80H,"UT"
        DB   'O'+80H,"N"
        DB   'N'+80H,"ULL"
        DB   'W'+80H,"AIT"
        DB   'D'+80H,"EF"
        DB   'P'+80H,"OKE"
        DB   'D'+80H,"OKE"
	db   'A'+80H,"UTO"
        DB   'L'+80H,"INES"
        DB   'C'+80H,"LS"
        DB   'W'+80H,"IDTH"
	db   'B'+80H,"YE"
	db   'R'+80H,"ENUMBER"
	db   'C'+80H,"ALL"
        DB   'P'+80H,"RINT"
        DB   'C'+80H,"ONT"
        DB   'L'+80H,"IST"
        DB   'C'+80H,"LEAR"
        DB   'C'+80H,"LOAD"
        DB   'C'+80H,"SAVE"
        DB   'N'+80H,"EW"

        DB   'T'+80H,"AB("
        DB   'T'+80H,"O"
        DB   'F'+80H,"N"
        DB   'S'+80H,"PC("
        DB   'T'+80H,"HEN"
        DB   'N'+80H,"OT"
        DB   'S'+80H,"TEP"

        DB   '+'+80H
        DB   '-'+80H
        DB   '*'+80H
        DB   '/'+80H
        DB   '^'+80H
        DB   'A'+80H,"ND"
        DB   'O'+80H,"R"
        DB   '>'+80H
        DB   '='+80H
        DB   '<'+80H

        DB   'S'+80H,"GN"
        DB   'I'+80H,"NT"
        DB   'A'+80H,"BS"
        DB   'U'+80H,"SR"
        DB   'F'+80H,"RE"
        DB   'I'+80H,"NP"
        DB   'P'+80H,"OS"
        DB   'S'+80H,"QR"
        DB   'R'+80H,"ND"
        db   'L'+80H,'N'
        DB   'E'+80H,"XP"
        DB   'C'+80H,"OS"
        DB   'S'+80H,"IN"
        DB   'T'+80H,"AN"
        DB   'A'+80H,"TN"
        DB   'P'+80H,"EEK"
        DB   'D'+80H,"EEK"
        DB   'P'+80H,"OINT"
        DB   'L'+80H,"EN"
        DB   'S'+80H,"TR$"
        DB   'V'+80H,"AL"
        DB   'A'+80H,"SC"
        DB   'C'+80H,"HR$"

        DB   'L'+80H,"EFT$"
        DB   'R'+80H,"IGHT$"
        DB   'M'+80H,"ID$"

	db   'S'+80H,"ET"
	db   'R'+80H,"ESET"
	db   'W'+80H,"INDOW"
	db   'S'+80H,"CREEN"
	db   'E'+80H,"DIT"
	db   'A'+80H,"SAVE"
	db   'A'+80H,"LOAD"
	db   'T'+80H,"RON"
	db   'T'+80H,"ROFF"
        DB   80H             ; End of list marker

; KEYWORD ADDRESS TABLE

WORDTB:	dw PEND
	dw FOR
	dw NEXT
	dw DATA
	dw INPUT
	dw DIM
	dw READ
	dw LET
	dw GOTO
	dw RUN
	dw IF
	dw RESTOR
	dw GOSUB
	dw RETURN
	dw REM
	dw STOP
	dw POUT
	dw ON
	dw NULL
	dw WAIT
	dw DEF
	dw POKE
	dw DOKE
	dw AUTO
	dw LINES
	dw CLS
	dw WIDTH
	dw BYE
	dw RENUM
	dw PCALL
	dw PRINT
	dw CONT
	dw LIST
	dw CLEAR
	dw CLOAD
	dw CSAVE
	dw NEW
;---
	dw PSET
	dw PRESET
	dw FCERR	; WINDOW
	dw SCREEN
	dw EDIT
	dw FCERR	; ASAVE
	dw FCERR	; ALOAD
	dw TRON
	dw TROFF

FNCTAB:	dw SGN
	dw INT
	dw ABS
	dw USR
	dw FRE
	dw INP
	dw POS
	dw SQR
	dw RND
	dw LOG
	dw EXP
	dw COS
	dw SIN
	dw TAN
	dw ATN
	dw PEEK
	dw DEEK
	dw POINT
	dw LEN
	dw STR
	dw VAL
	dw ASC
	dw CHR
	dw LEFT
	dw RIGHT
	dw MID

; RESERVED WORD TOKEN VALUES

ZEND    EQU    080H            ; END
ZFOR    EQU    081H            ; FOR
ZDATA   EQU    083H            ; DATA
ZGOTO   EQU    088H            ; GOTO
ZGOSUB  EQU    08CH            ; GOSUB
ZREM    EQU    08EH            ; REM
ZPRINT  EQU    09EH            ; PRINT
ZNEW    EQU    0A4H            ; NEW

ZTAB    EQU    0A5H            ; TAB
ZTO     EQU    0A6H            ; TO
ZFN     EQU    0A7H            ; FN
ZSPC    EQU    0A8H            ; SPC
ZTHEN   EQU    0A9H            ; THEN
ZNOT    EQU    0AAH            ; NOT
ZSTEP   EQU    0ABH            ; STEP

ZPLUS   EQU    0ACH            ; +
ZMINUS  EQU    0ADH            ; -
ZTIMES  EQU    0AEH            ; *
ZDIV    EQU    0AFH            ; /
ZOR     EQU    0B2H            ; OR
ZGTR    EQU    0B3H            ; >
ZEQUAL  EQU    0B4H            ; M
ZLTH    EQU    0B5H            ; <
ZSGN    EQU    0B6H            ; SGN
ZPOINT  EQU    0C7H            ; POINT
ZLEFT   EQU    0CDH +2         ; LEFT$

; ARITHMETIC PRECEDENCE TABLE

PRITAB: DB   79H             ; Precedence value
        DW   PADD            ; FPREG = <last> + FPREG

        DB   79H             ; Precedence value
        DW   PSUB            ; FPREG = <last> - FPREG

        DB   7CH             ; Precedence value
        DW   MULT            ; PPREG = <last> * FPREG

        DB   7CH             ; Precedence value
        DW   DIV             ; FPREG = <last> / FPREG

        DB   7FH             ; Precedence value
        DW   POWER           ; FPREG = <last> ^ FPREG

        DB   50H             ; Precedence value
        DW   PAND            ; FPREG = <last> AND FPREG

        DB   46H             ; Precedence value
        DW   POR             ; FPREG = <last> OR FPREG

; BASIC ERROR CODE LIST

ERRORS: DB   "NF"            ; NEXT without FOR
        DB   "SN"            ; Syntax error
        DB   "RG"            ; RETURN without GOSUB
        DB   "OD"            ; Out of DATA
        DB   "FC"            ; Illegal function call
        DB   "OV"            ; Overflow error
        DB   "OM"            ; Out of memory
        DB   "UL"            ; Undefined line
        DB   "BS"            ; Bad subscript
        DB   "DD"            ; Re-DIMensioned array
        DB   "/0"            ; Division by zero
        DB   "ID"            ; Illegal direct
        DB   "TM"            ; Type mis-match
        DB   "OS"            ; Out of string space
        DB   "LS"            ; String too long
        DB   "ST"            ; String formula too complex
        DB   "CN"            ; Can't CONTinue
        DB   "UF"            ; Undefined FN function
        DB   "MO"            ; Missing operand

ERRMSG: DB   " Error",0
INMSG:  DB   " in ",0
ZERBYT  EQU    $-1             ; A zero byte
OKMSG:  DB   "ok",CR,LF,0
BRKMSG: DB   "Break",0

CHKSTK: PUSH    HL              ; Save code string address
        LD      HL,(ARREND)     ; Lowest free memory
        LD      B,0             ; BC = Number of levels to test
        ADD     HL,BC           ; 2 Bytes for each level
        ADD     HL,BC
        DB   3EH             ; Skip "PUSH HL"

ENFMEM: PUSH    HL              ; Save code string address
        LD      A,0D0H ;LOW -48 ; 48 Bytes minimum RAM
        SUB     L
        LD      L,A
        LD      A,0FFH; HIGH (-48) ; 48 Bytes minimum RAM
        SBC     A,H
        Jr      C,OMERR         ; Not enough - ?OM Error
        LD      H,A
        ADD     HL,SP           ; Test if stack is overflowed
        POP     HL              ; Restore code string address
        RET     C               ; Return if enough mmory
OMERR:  LD      E,OM            ; ?OM Error
        Jr      ERROR

DATSNR: LD      HL,(DATLIN)     ; Get line of current DATA item
        LD      (LINEAT),HL     ; Save as current line
SNERR:  LD      E,SN            ; ?SN Error
        DB   01H             ; Skip "LD E,DZ"
DZERR:  LD      E,DZ            ; ?/0 Error
        DB   01H             ; Skip "LD E,NF"
NFERR:  LD      E,NF            ; ?NF Error
        DB   01H             ; Skip "LD E,DD"
DDERR:  LD      E,DD            ; ?DD Error
        DB   01H             ; Skip "LD E,UF"
UFERR:  LD      E,UF            ; ?UF Error
        DB   01H             ; Skip "LD E,OV
OVERR:  LD      E,OV            ; ?OV Error
        DB   01H             ; Skip "LD E,TM"
TMERR:  LD      E,TM            ; ?TM Error

ERROR:  CALL    CLREG           ; Clear registers and stack
        LD      (CTLOFG),A      ; Enable output (A is 0)
        CALL    STTLIN          ; Start new line
        LD      HL,ERRORS       ; Point to error codes
        LD      D,A             ; D = 0 (A is 0)
        LD      A,'?'
        CALL    OUTC            ; Output '?'
        ADD     HL,DE           ; Offset to correct error code
        LD      A,(HL)          ; First character
        CALL    OUTC            ; Output it
        CALL    GETCHR          ; Get next character
        CALL    OUTC            ; Output it
        LD      HL,ERRMSG       ; "Error" message
ERRIN:  CALL    PRS             ; Output message
        LD      HL,(LINEAT)     ; Get line of error
        LD      DE,-2           ; Cold start error if -2
        CALL    CPDEHL          ; See if cold start error
        JP      Z,STARTB        ; Cold start error - Restart
        LD      A,H             ; Was it a direct error?
        AND     L               ; Line = -1 if direct error
        INC     A
        CALL    NZ,LINEIN       ; No - output line of error
        DB   3EH             ; Skip "POP BC"
POPNOK: POP     BC              ; Drop address in input buffer

PRNTOK: XOR     A               ; Output "Ok" and get command
        LD      (CTLOFG),A      ; Enable output
        CALL    STTLIN          ; Start new line
        LD      HL,OKMSG        ; "Ok" message
        CALL    PRS             ; Output "Ok"
	scf
GETCMD: LD      HL,-1           ; Flag direct mode
        LD      (LINEAT),HL     ; Save as current line
;---
	ld	a, (unk_604D)
	or	a
	jr	z, getcmd4
	ld	hl, (unk_604E)
	push	hl
	call	PRNTHL
	pop	de
	push	de
	call	SRCHLN
	ld	a, '*'
	jr	c, getcmd1
	ld	a, ' '
getcmd1:
	call	OUTC
	call	TTYLIN
	pop	de
	jr	nc, getcmd3
getcmd2:
	xor	a
	ld	(unk_604D), a
	jr	PRNTOK
getcmd3:
	ld	hl, (unk_6050)
	add	hl, de
	jr	c, getcmd2
	push	de
	ld	de, 0FFF9h
	call	CPDEHL
	pop	de
	jr	nc, getcmd2
	ld	(unk_604E), hl
	ld	hl, BUFFER
	push	af
	jr	getcmd5
getcmd4:
	ld	a, '>'
	call	OUTC
	call	TTYLIN
;---
        Jr      C,GETCMD        ; Get line again if break
        CALL    GETCHR          ; Get first character
        INC     A               ; Test if end of line
        DEC     A               ; Without affecting Carry
        Jr      Z,GETCMD        ; Nothing entered - Get another
        PUSH    AF              ; Save Carry status
        CALL    ATOH            ; Get line number into DE
getcmd5:
        PUSH    DE              ; Save line number
        CALL    CRUNCH          ; Tokenise rest of line
        LD      B,A             ; Length of tokenised line
        POP     DE              ; Restore line number
        POP     AF              ; Restore Carry
        JP      NC,EXCUTE       ; No line number - Direct mode
        PUSH    DE              ; Save line number
        PUSH    BC              ; Save length of tokenised line
        XOR     A
        LD      (LSTBIN),A      ; Clear last byte input
        CALL    GETCHR          ; Get next character
        OR      A               ; Set flags
        PUSH    AF              ; And save them
        CALL    SRCHLN          ; Search for line number in DE
        Jr      C,LINFND        ; Jump if line found
        POP     AF              ; Get status
        PUSH    AF              ; And re-save
        JP      Z,ULERR         ; Nothing after number - Error
        OR      A               ; Clear Carry
LINFND: PUSH    BC              ; Save address of line in prog
        Jr      NC,INEWLN       ; Line not found - Insert new
        EX      DE,HL           ; Next line address in DE
        LD      HL,(PROGND)     ; End of program
SFTPRG: LD      A,(DE)          ; Shift rest of program down
        LD      (BC),A
        INC     BC              ; Next destination
        INC     DE              ; Next source
        CALL    CPDEHL          ; All done?
        Jr      NZ,SFTPRG       ; More to do
        LD      H,B             ; HL - New end of program
        LD      L,C
        LD      (PROGND),HL     ; Update end of program

INEWLN: POP     DE              ; Get address of line,
        POP     AF              ; Get status
        Jr      Z,SETPTR        ; No text - Set up pointers
        LD      HL,(PROGND)     ; Get end of program
        EX      (SP),HL         ; Get length of input line
        POP     BC              ; End of program to BC
        ADD     HL,BC           ; Find new end
        PUSH    HL              ; Save new end
        CALL    MOVUP           ; Make space for line
        POP     HL              ; Restore new end
        LD      (PROGND),HL     ; Update end of program pointer
        EX      DE,HL           ; Get line to move up in HL
        LD      (HL), 0ffh          ; Save MSB
        POP     DE              ; Get new line number
        INC     HL              ; Skip pointer
        INC     HL
        LD      (HL),E          ; Save LSB of line number
        INC     HL
        LD      (HL),D          ; Save MSB of line number
        INC     HL              ; To first byte in line
        LD      DE,BUFFER       ; Copy buffer to program
MOVBUF: LD      A,(DE)          ; Get source
        LD      (HL),A          ; Save destinations
        INC     HL              ; Next source
        INC     DE              ; Next destination
        OR      A               ; Done?
        Jr      NZ,MOVBUF       ; No - Repeat
SETPTR: CALL    RUNFST          ; Set line pointers
        INC     HL              ; To LSB of pointer
        EX      DE,HL           ; Address to DE
	ld	hl, GETCMD
	push	hl
PTRLP:  LD      H,D             ; Address to HL
        LD      L,E
        LD      A,(HL)          ; Get LSB of pointer
        INC     HL              ; To MSB of pointer
        OR      (HL)            ; Compare with MSB pointer
	ret	z       ; Get command line if end
        INC     HL              ; To LSB of line number
	ld	a, (hl)
        INC     HL              ; Skip line number
	and	(hl)
	inc	a
	ret	z       ; Get command line if end
        INC     HL              ; Point to first byte in line
        XOR     A               ; Looking for 00 byte
FNDEND: CP      (HL)            ; Found end of line?
        INC     HL              ; Move to next byte
        Jr      NZ,FNDEND       ; No - Keep looking
        EX      DE,HL           ; Next line address to HL
        LD      (HL),E          ; Save LSB of pointer
        INC     HL
        LD      (HL),D          ; Save MSB of pointer
        Jr      PTRLP           ; Do next line

MOVUP:  CALL    ENFMEM          ; See if enough memory
MOVSTR: PUSH    BC              ; Save end of source
        EX      (SP),HL         ; Swap source and dest" end
        POP     BC              ; Get end of destination
MOVLP:  CALL    CPDEHL          ; See if list moved
        LD      A,(HL)          ; Get byte
        LD      (BC),A          ; Move it
        RET     Z               ; Exit if all done
        DEC     BC              ; Next byte to move to
        DEC     HL              ; Next byte to move
        Jr      MOVLP           ; Loop until all bytes moved

SRCHLN: LD      HL,(BASTXT)     ; Start of program text
SRCHLP: LD      B,H             ; BC = Address to look at
        LD      C,L
        LD      A,(HL)          ; Get address of next line
        INC     HL
        OR      (HL)            ; End of program found?
        DEC     HL
        RET     Z               ; Yes - Line not found
        INC     HL
        INC     HL
        LD      A,(HL)          ; Get LSB of line number
        INC     HL
        LD      H,(HL)          ; Get MSB of line number
        LD      L,A
        CALL    CPDEHL          ; Compare with line in DE
        LD      H,B             ; HL = Start of this line
        LD      L,C
        LD      A,(HL)          ; Get LSB of next line address
        INC     HL
        LD      H,(HL)          ; Get MSB of next line address
        LD      L,A             ; Next line to HL
        CCF
        RET     Z               ; Lines found - Exit
        CCF
        RET     NC              ; Line not found,at line after
        Jr      SRCHLP          ; Keep looking

CRUNCH: XOR     A               ; Tokenise line @ HL to BUFFER
        LD      (DATFLG),A      ; Reset literal flag
        LD      C,2+3           ; 2 byte number and 3 nulls
        LD      DE,BUFFER       ; Start of input buffer
CRNCLP: LD      A,(HL)          ; Get byte
        CP      ' '             ; Is it a space?
        Jr      Z,MOVDIR        ; Yes - Copy direct
        LD      B,A             ; Save character
        CP      '"'             ; Is it a quote?
        JP      Z,CPYLIT        ; Yes - Copy literal string
        OR      A               ; Is it end of buffer?
        JP      Z,ENDBUF        ; Yes - End buffer
        LD      A,(DATFLG)      ; Get data type
        OR      A               ; Literal?
        LD      A,(HL)          ; Get byte to copy
        Jr      NZ,MOVDIR       ; Literal - Copy direct
        CP      '?'             ; Is it '?' short for PRINT
        LD      A,ZPRINT        ; "PRINT" token
        Jr      Z,MOVDIR        ; Yes - replace it
        LD      A,(HL)          ; Get byte again
        CP      '0'             ; Is it less than '0'
        Jr      C,FNDWRD        ; Yes - Look for reserved words
        CP      60; ";"+1           ; Is it "0123456789:;" ?
        Jr      C,MOVDIR        ; Yes - copy it direct
FNDWRD: PUSH    DE              ; Look for reserved words
        LD      DE,WORDS-1      ; Point to table
        PUSH    BC              ; Save count
        LD      BC,RETNAD       ; Where to return to
        PUSH    BC              ; Save return address
        LD      B,ZEND-1        ; First token value -1
        LD      A,(HL)          ; Get byte
        CP      'a'             ; Less than 'a' ?
        Jr      C,SEARCH        ; Yes - search for words
        CP      'z'+1           ; Greater than 'z' ?
        Jr      NC,SEARCH       ; Yes - search for words
        AND     01011111B       ; Force upper case
        LD      (HL),A          ; Replace byte
SEARCH: LD      C,(HL)          ; Search for a word
        EX      DE,HL
GETNXT: INC     HL              ; Get next reserved word
        OR      (HL)            ; Start of word?
        JP      P,GETNXT        ; No - move on
        INC     B               ; Increment token value
        LD      A, (HL)         ; Get byte from table
        AND     01111111B       ; Strip bit 7
        RET     Z               ; Return if end of list
        CP      C               ; Same character as in buffer?
        Jr      NZ,GETNXT       ; No - get next word
        EX      DE,HL
        PUSH    HL              ; Save start of word

NXTBYT: INC     DE              ; Look through rest of word
        LD      A,(DE)          ; Get byte from table
        OR      A               ; End of word ?
        JP      M,MATCH         ; Yes - Match found
        LD      C,A             ; Save it
        LD      A,B             ; Get token value
        CP      ZGOTO           ; Is it "GOTO" token ?
        Jr      NZ,NOSPC        ; No - Don't allow spaces
        CALL    GETCHR          ; Get next character
        DEC     HL              ; Cancel increment from GETCHR
NOSPC:  INC     HL              ; Next byte
        LD      A,(HL)          ; Get byte
        CP      'a'             ; Less than 'a' ?
        Jr      C,NOCHNG        ; Yes - don't change
        AND     01011111B       ; Make upper case
NOCHNG: CP      C               ; Same as in buffer ?
        Jr      Z,NXTBYT        ; Yes - keep testing
        POP     HL              ; Get back start of word
        Jr      SEARCH          ; Look at next word

MATCH:  LD      C,B             ; Word found - Save token value
        POP     AF              ; Throw away return
        EX      DE,HL
        RET                     ; Return to "RETNAD"
RETNAD: EX      DE,HL           ; Get address in string
        LD      A,C             ; Get token value
        POP     BC              ; Restore buffer length
        POP     DE              ; Get destination address
MOVDIR: INC     HL              ; Next source in buffer
        LD      (DE),A          ; Put byte in buffer
        INC     DE              ; Move up buffer
        INC     C               ; Increment length of buffer
        SUB     ':'             ; End of statement?
        Jr      Z,SETLIT        ; Jump if multi-statement line
        CP      ZDATA-3AH       ; Is it DATA statement ?
        Jr      NZ,TSTREM       ; No - see if REM
SETLIT: LD      (DATFLG),A      ; Set literal flag
TSTREM: SUB     ZREM-3AH        ; Is it REM?
        JP      NZ,CRNCLP       ; No - Leave flag
        LD      B,A             ; Copy rest of buffer
NXTCHR: LD      A,(HL)          ; Get byte
        OR      A               ; End of line ?
        Jr      Z,ENDBUF        ; Yes - Terminate buffer
        CP      B               ; End of statement ?
        Jr      Z,MOVDIR        ; Yes - Get next one
CPYLIT: INC     HL              ; Move up source string
        LD      (DE),A          ; Save in destination
        INC     C               ; Increment length
        INC     DE              ; Move up destination
        Jr      NXTCHR          ; Repeat

ENDBUF: LD      HL,BUFFER-1     ; Point to start of buffer
        LD      (DE),A          ; Mark end of buffer (A = 00)
        INC     DE
        LD      (DE),A          ; A = 00
        INC     DE
        LD      (DE),A          ; A = 00
        RET
;---
;vvv unused
		db    2
		db    8
		db  2Eh	; .
		db    0
		db    8
		db  2Eh	; .
		db    1
		db    9
		db    5
		db    5
		db    5
		db  2Bh	; +
		db 0CDh	; Õ
		db  8Eh	; é
;^^^---

loc_4584:	dec	hl
		dec	b
		call	nz, MONOUT
		jr	nz, MORINP
		jr	loc_4590

TTYLIN:		call	sub_5F3E
loc_4590:	ld	b, 1
		ld	hl, BUFFER
MORINP:		call	CLOTST
		ld	c, a
		cp	3
		call	z, PRNTCRLF
		scf
		ret	z
		cp	0Dh
		jp	z, loc_45C6
		cp	8
		jr	z, loc_4584
		cp	9
		jr	z, loc_45B1
		cp	20h ; ' '
		jr	c, MORINP
loc_45B1:	ld	a, b
		cp	49h ; 'I'
		jr	nc, MORINP
		ld	a, c
		cp	9
		jr	z, loc_45BF
		ld	(hl), a
		ld	(LSTBIN), a
loc_45BF:	inc	hl
		inc	b
		call	MONOUT
		jr	MORINP
loc_45C6:	ld	hl, unk_60A6
loc_45C9:	dec	hl
		ld	a, l
		cp	5Dh ; ']'
		jr	z, loc_45D4
		ld	a, 20h ; ' '
		cp	(hl)
		jr	z, loc_45C9
loc_45D4:	inc	hl
		jp	ENDINP
;--
;vvv unused
		push	hl
		push	de
		jr	loc_462D

		jp	1808h

		ex	(sp), hl
		ld	a, (hl)
		inc	hl
		jr	auto1

		jp	180Bh

		ld	sp, 1856h
		jr	loc_45EF

		jp	180Eh

loc_45EF:	call	1A5h
		jr	loc_45F7

		jp	1811h

loc_45F7:	jp	24Ch

		db 0FFh
		db 0FFh

		jp	1814h
		db    0
;^^^---

AUTO:		ld	de, 10
		push	de
		jr	z, auto2
		call	ATOH
		ex	de, hl
		ex	(sp), hl
		jr	z, auto3
		ex	de, hl
		call	CHKSYN
		db	','
		ld	de, (unk_6050)
auto1:		jr	z, auto2
		call	ATOH
		jp	nz, SNERR
auto2:		ex	de, hl
auto3:		ld	a, l
		or	h
		jp	z, FCERR
		ld	(unk_6050), hl
		ld	(unk_604D), a
		pop	hl
		db  22h	; "
		db  4Eh	; N
loc_462D:	ld	h, b
		pop	bc
		jp	GETCMD
;---

NEW:    RET     NZ              ; Return if any more on line
CLRPTR: LD      HL,(BASTXT)     ; Point to start of program
        XOR     A               ; Set program area to empty
        LD      (HL),A          ; Save LSB = 00
        INC     HL
        LD      (HL),A          ; Save MSB = 00
        INC     HL
        LD      (PROGND),HL     ; Set program end

RUNFST: LD      HL,(BASTXT)     ; Clear all variables
        DEC     HL

INTVAR: LD      (BRKLIN),HL     ; Initialise RUN variables
        LD      HL,(LSTRAM)     ; Get end of RAM
        LD      (STRBOT),HL     ; Clear string space
        XOR     A
        CALL    RESTOR          ; Reset DATA pointers
        LD      HL,(PROGND)     ; Get end of program
        LD      (VAREND),HL     ; Clear variables
        LD      (ARREND),HL     ; Clear arrays

CLREG:  POP     BC              ; Save return address
        LD      HL,(STRSPC)     ; Get end of working RAN
        LD      SP,HL           ; Set stack
        LD      HL,TMSTPL       ; Temporary string pool
        LD      (TMSTPT),HL     ; Reset temporary string ptr
        XOR     A               ; A = 00
        LD      L,A             ; HL = 0000
        LD      H,A
        LD      (CONTAD),HL     ; No CONTinue
        LD      (FORFLG),A      ; Clear FOR flag
        LD      (FNRGNM),HL     ; Clear FN argument
        PUSH    HL              ; HL = 0000
        PUSH    BC              ; Put back return
DOAGN:  LD      HL,(BRKLIN)     ; Get address of code to RUN
        RET                     ; Return to execution driver

CPDEHL: LD      A,H             ; Get H
        SUB     D               ; Compare with D
        RET     NZ              ; Different - Exit
        LD      A,L             ; Get L
        SUB     E               ; Compare with E
        RET                     ; Return status

CHKSYN: LD      A,(HL)          ; Check syntax of character
        EX      (SP),HL         ; Address of test byte
        CP      (HL)            ; Same as in code string?
        INC     HL              ; Return address
        EX      (SP),HL         ; Put it back
        JP      Z,GETCHR        ; Yes - Get next character
        JP      SNERR           ; Different - ?SN Error

CHKLTR: LD      A,(HL)          ; Get byte
        CP      'A'             ; < 'a' ?
        RET     C               ; Carry set if not letter
        CP      'Z'+1           ; > 'z' ?
        CCF
        RET                     ; Carry set if not letter

OUTC:   PUSH    AF              ; Save character
	push	bc
	ld	c, a
        LD      A,(CTLOFG)      ; Get control "O" flag
        OR      A               ; Is it set?
        Jr      NZ,POPAF        ; Yes - don't output
	ld	a, c
        CP      ' '             ; Is it a control code?
        Jr      C,DINPOS        ; Yes - Don't INC POS(X)
        LD      A,(LWIDTH)      ; Get line width
        LD      B,A             ; To B
        LD      A,(CURPOS)      ; Get cursor position
        INC     B               ; Width 255?
        Jr      Z,INCLEN        ; Yes - No width limit
        DEC     B               ; Restore width
        CP      B               ; At end of line?
        CALL    Z,PRNTCRLF      ; Yes - output CRLF
INCLEN: INC     A               ; Move on one character
        LD      (CURPOS),A      ; Save new position
DINPOS: ld	a, c            ; Restore character
        CALL    MONOUT          ; Send it
POPAF:  pop	bc
	POP     AF              ; Throw away status push
        RET

CLOTST: CALL    GETINP          ; Get input character
        CP      CTRLO           ; Is it control "O"?
        RET     NZ              ; No don't flip flag
        LD      A,(CTLOFG)      ; Get flag
        CPL                     ; Flip it
        LD      (CTLOFG),A      ; Put it back
        XOR     A               ; Null character
        RET

PROMPT: LD      A,'?'           ; '?'
        CALL    OUTC            ; Output character
        LD      A,' '           ; Space
        CALL    OUTC            ; Output character
        JP      RINPUT          ; Get input line

LINES:  CALL    GETNUM          ; Get a number
        CALL    DEINT           ; Get integer -32768 to 32767
        LD      (LINESC),DE     ; Set lines counter
        LD      (LINESN),DE     ; Set lines number
        RET

LIST:   CALL    ATOH            ; ASCII number to DE
        RET     NZ              ; Return if anything extra
        POP     BC              ; Rubbish - Not needed
        CALL    SRCHLN          ; Search for line number in DE
        PUSH    BC              ; Save address of line
        CALL    SETLIN          ; Set up lines counter
LISTLP: POP     HL              ; Restore address of line
        LD      C,(HL)          ; Get LSB of next line
        INC     HL
        LD      B,(HL)          ; Get MSB of next line
        INC     HL
        LD      A,B             ; BC = 0 (End of program)?
        OR      C
        Jr      Z,loc_474A        ; Yes - Go to command mode
        CALL    COUNT           ; Count lines
        CALL    TSTBRK          ; Test for break key
        PUSH    BC              ; Save address of next line
        CALL    PRNTCRLF        ; Output CRLF
        LD      E,(HL)          ; Get LSB of line number
        INC     HL
        LD      D,(HL)          ; Get MSB of line number
        INC     HL
        PUSH    HL              ; Save address of line start
        EX      DE,HL           ; Line number to HL
        CALL    PRNTHL          ; Output line number in decimal
        pop	hl
        LD      A,' '           ; Space after line number
	call	OUTC
	call	sub_4714
	jr	LISTLP

sub_4714:
	call	sub_5F3E
	ld	b, 49h ; 'I'
	jr	loc_471E
loc_471B:
	call	sub_5F4E
loc_471E:
	ld	a, (hl)
	or	a
	inc	hl
	ret	z
	jp	p, loc_471B
	sub	7Fh
	ld	c, a
	ld	de, WORDS
loc_472B:
	ld	a, (de)
	inc	de
	or	a
	jp	p, loc_472B
	dec	c
loc_4732:
	jr	nz, loc_472B
loc_4734:
	and	7Fh
	call	sub_5F4E
	ld	a, (de)
	inc	de
	or	a
loc_473C:
	jp	p, loc_4734
	jr	loc_471E

SETLIN: PUSH    HL              ; Set up LINES counter
loc_4742:
        LD      HL,(LINESN)     ; Get LINES number
        LD      (LINESC),HL     ; Save in LINES counter
        POP     HL
        RET

loc_474A:
	jp	BRKRET

;vvv unused
	ld	e, d
	ld	(LINESC), hl
	pop	de
	pop	hl
	ret	p
	call	loc_4732+1
	call	GETINP
	cp	3
	jr	z, loc_473C
	jr	loc_4742+2
;^^^---

TRON:	db  3Eh	; ld a,..
TROFF:	xor	a
	ld	(TRFLAG), a
	ret

BAKSTK: LD      HL,4            ; Look for "FOR" block with
        ADD     HL,SP           ; same index as specified
LOKFOR: LD      A,(HL)          ; Get block ID
        INC     HL              ; Point to index address
        CP      ZFOR            ; Is it a "FOR" token
        RET     NZ              ; No - exit
        LD      C,(HL)          ; BC = Address of "FOR" index
        INC     HL
        LD      B,(HL)
        INC     HL              ; Point to sign of STEP
        PUSH    HL              ; Save pointer to sign
        LD      L,C             ; HL = address of "FOR" index
        LD      H,B
        LD      A,D             ; See if an index was specified
        OR      E               ; DE = 0 if no index specified
        EX      DE,HL           ; Specified index into HL
        Jr      Z,INDFND        ; Skip if no index given
        EX      DE,HL           ; Index back into DE
        CALL    CPDEHL          ; Compare index with one given
INDFND: LD      BC,16-3         ; Offset to next block
        POP     HL              ; Restore pointer to sign
        RET     Z               ; Return if block found
        ADD     HL,BC           ; Point to next block
        Jr      LOKFOR          ; Keep on looking

FOR:    LD      A,64H           ; Flag "FOR" assignment
        LD      (FORFLG),A      ; Save "FOR" flag
        CALL    LET             ; Set up initial index
        POP     BC              ; Drop RETurn address
        PUSH    HL              ; Save code string address
        CALL    DATA            ; Get next statement address
        LD      (LOOPST),HL     ; Save it for start of loop
        LD      HL,2            ; Offset for "FOR" block
        ADD     HL,SP           ; Point to it
FORSLP: CALL    LOKFOR          ; Look for existing "FOR" block
        POP     DE              ; Get code string address
        Jr      NZ,FORFND       ; No nesting found
        ADD     HL,BC           ; Move into "FOR" block
        PUSH    DE              ; Save code string address
        DEC     HL
        LD      D,(HL)          ; Get MSB of loop statement
        DEC     HL
        LD      E,(HL)          ; Get LSB of loop statement
        INC     HL
        INC     HL
        PUSH    HL              ; Save block address
        LD      HL,(LOOPST)     ; Get address of loop statement
        CALL    CPDEHL          ; Compare the FOR loops
        POP     HL              ; Restore block address
        Jr      NZ,FORSLP       ; Different FORs - Find another
        POP     DE              ; Restore code string address
        LD      SP,HL           ; Remove all nested loops

FORFND: EX      DE,HL           ; Code string address to HL
        LD      C,8
        CALL    CHKSTK          ; Check for 8 levels of stack
        PUSH    HL              ; Save code string address
        LD      HL,(LOOPST)     ; Get first statement of loop
        EX      (SP),HL         ; Save and restore code string
        PUSH    HL              ; Re-save code string address
        LD      HL,(LINEAT)     ; Get current line number
        EX      (SP),HL         ; Save and restore code string
        CALL    TSTNUM          ; Make sure it's a number
        CALL    CHKSYN          ; Make sure "TO" is next
        DB   ZTO          ; "TO" token
        CALL    GETNUM          ; Get "TO" expression value
        PUSH    HL              ; Save code string address
        CALL    BCDEFP          ; Move "TO" value to BCDE
        POP     HL              ; Restore code string address
        PUSH    BC              ; Save "TO" value in block
        PUSH    DE
        LD      BC,8100H        ; BCDE - 1 (default STEP)
        LD      D,C             ; C=0
        LD      E,D             ; D=0
        LD      A,(HL)          ; Get next byte in code string
        CP      ZSTEP           ; See if "STEP" is stated
        LD      A,1             ; Sign of step = 1
        Jr      NZ,SAVSTP       ; No STEP given - Default to 1
        CALL    GETCHR          ; Jump over "STEP" token
        CALL    GETNUM          ; Get step value
        PUSH    HL              ; Save code string address
        CALL    BCDEFP          ; Move STEP to BCDE
        CALL    TSTSGN          ; Test sign of FPREG
        POP     HL              ; Restore code string address
SAVSTP: PUSH    BC              ; Save the STEP value in block
        PUSH    DE
        PUSH    AF              ; Save sign of STEP
        INC     SP              ; Don't save flags
        PUSH    HL              ; Save code string address
        LD      HL,(BRKLIN)     ; Get address of index variable
        EX      (SP),HL         ; Save and restore code string
PUTFID: LD      B,ZFOR          ; "FOR" block marker
        PUSH    BC              ; Save it
        INC     SP              ; Don't save C

RUNCNT: CALL    TSTBRK          ; Execution driver - Test break
        LD      (BRKLIN),HL     ; Save code address for break
        LD      A,(HL)          ; Get next byte in code string
        CP      ':'             ; Multi statement line?
        Jr      Z,EXCUTE        ; Yes - Execute it
        OR      A               ; End of line?
        JP      NZ,SNERR        ; No - Syntax error
        INC     HL              ; Point to address of next line
        LD      A,(HL)          ; Get LSB of line pointer
        INC     HL
        OR      (HL)            ; Is it zero (End of prog)?
        JP      Z,ENDPRG        ; Yes - Terminate execution
        INC     HL              ; Point to line number
        LD      E,(HL)          ; Get LSB of line number
        INC     HL
        LD      D,(HL)          ; Get MSB of line number
        EX      DE,HL           ; Line number to HL
        LD      (LINEAT),HL     ; Save as current line number
	ld	a, (TRFLAG)
	or	a
	jr	z, loc_4830
	push	de
	ld	a, '<'
	call	OUTC
	call	PRNTHL
	ld	a, '>'
	call	OUTC
	pop	de
loc_4830:
        EX      DE,HL           ; Line number back to DE
EXCUTE: CALL    GETCHR          ; Get key word
        LD      DE,RUNCNT       ; Where to RETurn to
        PUSH    DE              ; Save for RETurn
IFJMP:  RET     Z               ; Go to RUNCNT if end of STMT
ONJMP:  SUB     ZEND            ; Is it a token?
        JP      C,LET           ; No - try to assign it
        CP      ZNEW+1-ZEND     ; END to NEW ?
	jr	c, loc_484E
	sub	50h ; 'P'
	jp	c, SNERR
	cp	9
        JP      NC,SNERR        ; Not a key word - ?SN Error
	add	a, 25h
loc_484E:
        RLCA                    ; Double it
        LD      C,A             ; BC = Offset into table
        LD      B,0
        EX      DE,HL           ; Save code string address
        LD      HL,WORDTB       ; Keyword address table
        ADD     HL,BC           ; Point to routine address
        LD      C,(HL)          ; Get LSB of routine address
        INC     HL
        LD      B,(HL)          ; Get MSB of routine address
        PUSH    BC              ; Save routine address
        EX      DE,HL           ; Restore code string address

GETCHR: INC     HL              ; Point to next character
        LD      A,(HL)          ; Get next code string byte
        CP      ':'             ; Z if ':'
        RET     NC              ; NC if > "9"
        CP      ' '
        Jr      Z,GETCHR        ; Skip over spaces
        CP      '0'
        CCF                     ; NC if < '0'
        INC     A               ; Test for zero - Leave carry
        DEC     A               ; Z if Null
        RET

RESTOR: EX      DE,HL           ; Save code string address
        LD      HL,(BASTXT)     ; Point to start of program
        Jr      Z,RESTNL        ; Just RESTORE - reset pointer
        EX      DE,HL           ; Restore code string address
        CALL    ATOH            ; Get line number to DE
        PUSH    HL              ; Save code string address
        CALL    SRCHLN          ; Search for line number in DE
        LD      H,B             ; HL = Address of line
        LD      L,C
        POP     DE              ; Restore code string address
        JP      NC,ULERR        ; ?UL Error if not found
RESTNL: DEC     HL              ; Byte before DATA statement
UPDATA: LD      (NXTDAT),HL     ; Update DATA pointer
        EX      DE,HL           ; Restore code string address
        RET


TSTBRK:	in	a, (4)
	cp	83h
	ret	nz
	call	getkey		; get input status
	cp	83h
	nop


STOP:   RET     NZ              ; Exit if anything else
        DB   0F6H            ; Flag "STOP"
PEND:   RET     NZ              ; Exit if anything else
        LD      (BRKLIN),HL     ; Save point of break
        DB   21H             ; Skip "OR 11111111B"
INPBRK: OR      11111111B       ; Flag "Break" wanted
        POP     BC              ; Return not needed and more
ENDPRG: LD      HL,(LINEAT)     ; Get current line number
        PUSH    AF              ; Save STOP / END status
        LD      A,L             ; Is it direct break?
        AND     H
        INC     A               ; Line is -1 if direct break
        Jr      Z,NOLIN         ; Yes - No line number
        LD      (ERRLIN),HL     ; Save line of break
        LD      HL,(BRKLIN)     ; Get point of break
        LD      (CONTAD),HL     ; Save point to CONTinue
NOLIN:  XOR     A
        LD      (CTLOFG),A      ; Enable output
        CALL    STTLIN          ; Start a new line
        POP     AF              ; Restore STOP / END status
        LD      HL,BRKMSG       ; "Break" message
        JP      NZ,ERRIN        ; "in line" wanted?
        JP      PRNTOK          ; Go to command mode

CONT:   LD      HL,(CONTAD)     ; Get CONTinue address
        LD      A,H             ; Is it zero?
        OR      L
        LD      E,CN            ; ?CN Error
        JP      Z,ERROR         ; Yes - output "?CN Error"
        EX      DE,HL           ; Save code string address
        LD      HL,(ERRLIN)     ; Get line of last break
        LD      (LINEAT),HL     ; Set up current line number
        EX      DE,HL           ; Restore code string address
        RET                     ; CONTinue where left off

FPSINT: CALL    GETCHR          ; Get next character
POSINT: CALL    GETNUM          ; Get integer 0 to 32767
DEPINT: CALL    TSTSGN          ; Test sign of FPREG
        JP      M,FCERR         ; Negative - ?FC Error
DEINT:  LD      A,(FPEXP)       ; Get integer value to DE
        CP      80H+16          ; Exponent in range (16 bits)?
        JP      C,FPINT         ; Yes - convert it
        LD      BC,9080H        ; BCDE = -32768
        LD      DE,0000
        PUSH    HL              ; Save code string address
        CALL    CMPNUM          ; Compare FPREG with BCDE
        POP     HL              ; Restore code string address
        LD      D,C             ; MSB to D
        RET     Z               ; Return if in range
FCERR:  LD      E,FC            ; ?FC Error
        JP      ERROR           ; Output error-

ATOH:   DEC     HL              ; ASCII number to DE binary
GETLN:  LD      DE,0            ; Get number to DE
GTLNLP: CALL    GETCHR          ; Get next character
        RET     NC              ; Exit if not a digit
        PUSH    HL              ; Save code string address
        PUSH    AF              ; Save digit
        LD      HL,65529/10     ; Largest number 65529
        CALL    CPDEHL          ; Number in range?
        JP      C,SNERR         ; No - ?SN Error
        LD      H,D             ; HL = Number
        LD      L,E
        ADD     HL,DE           ; Times 2
        ADD     HL,HL           ; Times 4
        ADD     HL,DE           ; Times 5
        ADD     HL,HL           ; Times 10
        POP     AF              ; Restore digit
        SUB     '0'             ; Make it 0 to 9
        LD      E,A             ; DE = Value of digit
        LD      D,0
        ADD     HL,DE           ; Add to number
        EX      DE,HL           ; Number to DE
        POP     HL              ; Restore code string address
        Jr      GTLNLP          ; Go to next character

CLEAR:  JP      Z,INTVAR        ; Just "CLEAR" Keep parameters
        CALL    POSINT          ; Get integer 0 to 32767 to DE
        DEC     HL              ; Cancel increment
        CALL    GETCHR          ; Get next character
        PUSH    HL              ; Save code string address
        LD      HL,(LSTRAM)     ; Get end of RAM
        Jr      Z,STORED        ; No value given - Use stored
        POP     HL              ; Restore code string address
        CALL    CHKSYN          ; Check for comma
        DB      ','
        PUSH    DE              ; Save number
        CALL    POSINT          ; Get integer 0 to 32767
        DEC     HL              ; Cancel increment
        CALL    GETCHR          ; Get next character
        JP      NZ,SNERR        ; ?SN Error if more on line
        EX      (SP),HL         ; Save code string address
        EX      DE,HL           ; Number to DE
STORED: LD      A,L             ; Get LSB of new RAM top
        SUB     E               ; Subtract LSB of string space
        LD      E,A             ; Save LSB
        LD      A,H             ; Get MSB of new RAM top
        SBC     A,D             ; Subtract MSB of string space
        LD      D,A             ; Save MSB
        JP      C,OMERR         ; ?OM Error if not enough mem
        PUSH    HL              ; Save RAM top
        LD      HL,(PROGND)     ; Get program end
        LD      BC,40           ; 40 Bytes minimum working RAM
        ADD     HL,BC           ; Get lowest address
        CALL    CPDEHL          ; Enough memory?
        JP      NC,OMERR        ; No - ?OM Error
        EX      DE,HL           ; RAM top to HL
        LD      (STRSPC),HL     ; Set new string space
        POP     HL              ; End of memory to use
        LD      (LSTRAM),HL     ; Set new top of RAM
        POP     HL              ; Restore code string address
        JP      INTVAR          ; Initialise variables

RUN:    JP      Z,RUNFST        ; RUN from start if just RUN
        CALL    INTVAR          ; Initialise variables
        LD      BC,RUNCNT       ; Execution driver loop
        Jr      RUNLIN          ; RUN from line number

GOSUB:  LD      C,3             ; 3 Levels of stack needed
        CALL    CHKSTK          ; Check for 3 levels of stack
        POP     BC              ; Get return address
        PUSH    HL              ; Save code string for RETURN
        PUSH    HL              ; And for GOSUB routine
        LD      HL,(LINEAT)     ; Get current line
        EX      (SP),HL         ; Into stack - Code string out
        LD      A,ZGOSUB        ; "GOSUB" token
        PUSH    AF              ; Save token
        INC     SP              ; Don't save flags

RUNLIN: PUSH    BC              ; Save return address
GOTO:   CALL    ATOH            ; ASCII number to DE binary
        CALL    REM             ; Get end of line
        PUSH    HL              ; Save end of line
        LD      HL,(LINEAT)     ; Get current line
        CALL    CPDEHL          ; Line after current?
        POP     HL              ; Restore end of line
        INC     HL              ; Start of next line
        CALL    C,SRCHLP        ; Line is after current line
        CALL    NC,SRCHLN       ; Line is before current line
        LD      H,B             ; Set up code string address
        LD      L,C
        DEC     HL              ; Incremented after
        RET     C               ; Line found
ULERR:  LD      E,UL            ; ?UL Error
        JP      ERROR           ; Output error message

RETURN: RET     NZ              ; Return if not just RETURN
        LD      D,-1            ; Flag "GOSUB" search
        CALL    BAKSTK          ; Look "GOSUB" block
        LD      SP,HL           ; Kill all FORs in subroutine
        CP      ZGOSUB          ; Test for "GOSUB" token
        LD      E,RG            ; ?RG Error
        JP      NZ,ERROR        ; Error if no "GOSUB" found
        POP     HL              ; Get RETURN line number
        LD      (LINEAT),HL     ; Save as current
        INC     HL              ; Was it from direct statement?
        LD      A,H
        OR      L               ; Return to line
        Jr      NZ,RETLIN       ; No - Return to line
        LD      A,(LSTBIN)      ; Any INPUT in subroutine?
        OR      A               ; If so buffer is corrupted
        JP      NZ,POPNOK       ; Yes - Go to command mode
RETLIN: LD      HL,RUNCNT       ; Execution driver loop
        EX      (SP),HL         ; Into stack - Code string out
        DB      3EH             ; Skip "POP HL"
NXTDTA: POP     HL              ; Restore code string address

DATA:   DB      01H,3AH         ; ':' End of statement
REM:    LD      C,0             ; 00  End of statement
        LD      B,0
NXTSTL: LD      A,C             ; Statement and byte
        LD      C,B
        LD      B,A             ; Statement end byte
NXTSTT: LD      A,(HL)          ; Get byte
        OR      A               ; End of line?
        RET     Z               ; Yes - Exit
        CP      B               ; End of statement?
        RET     Z               ; Yes - Exit
        INC     HL              ; Next byte
        CP      '"'             ; Literal string?
        Jr      Z,NXTSTL        ; Yes - Look for another '"'
        Jr      NXTSTT          ; Keep looking

LET:    CALL    GETVAR          ; Get variable name
        CALL    CHKSYN          ; Make sure "=" follows
        DB      ZEQUAL          ; "=" token
        PUSH    DE              ; Save address of variable
        LD      A,(TYPE)        ; Get data type
        PUSH    AF              ; Save type
        CALL    EVAL            ; Evaluate expression
        POP     AF              ; Restore type
        EX      (SP),HL         ; Save code - Get var addr
        LD      (BRKLIN),HL     ; Save address of variable
        RRA                     ; Adjust type
        CALL    CHKTYP          ; Check types are the same
        Jr      Z,LETNUM        ; Numeric - Move value
LETSTR: PUSH    HL              ; Save address of string var
        LD      HL,(FPREG)      ; Pointer to string entry
        PUSH    HL              ; Save it on stack
        INC     HL              ; Skip over length
        INC     HL
        LD      E,(HL)          ; LSB of string address
        INC     HL
        LD      D,(HL)          ; MSB of string address
        LD      HL,(BASTXT)     ; Point to start of program
        CALL    CPDEHL          ; Is string before program?
        Jr      NC,CRESTR       ; Yes - Create string entry
        LD      HL,(STRSPC)     ; Point to string space
        CALL    CPDEHL          ; Is string literal in program?
        POP     DE              ; Restore address of string
        Jr      NC,MVSTPT       ; Yes - Set up pointer
        LD      HL,TMPSTR       ; Temporary string pool
        CALL    CPDEHL          ; Is string in temporary pool?
        Jr      NC,MVSTPT       ; No - Set up pointer
        DB   3EH             ; Skip "POP DE"
CRESTR: POP     DE              ; Restore address of string
        CALL    BAKTMP          ; Back to last tmp-str entry
        EX      DE,HL           ; Address of string entry
        CALL    SAVSTR          ; Save string in string area
MVSTPT: CALL    BAKTMP          ; Back to last tmp-str entry
        POP     HL              ; Get string pointer
        CALL    DETHL4          ; Move string pointer to var
        POP     HL              ; Restore code string address
        RET

LETNUM: PUSH    HL              ; Save address of variable
        CALL    FPTHL           ; Move value to variable
        POP     DE              ; Restore address of variable
        POP     HL              ; Restore code string address
        RET

ON:     CALL    GETINT          ; Get integer 0-255
        LD      A,(HL)          ; Get "GOTO" or "GOSUB" token
        LD      B,A             ; Save in B
        CP      ZGOSUB          ; "GOSUB" token?
        Jr      Z,ONGO          ; Yes - Find line number
        CALL    CHKSYN          ; Make sure it's "GOTO"
        DB   ZGOTO           ; "GOTO" token
        DEC     HL              ; Cancel increment
ONGO:   LD      C,E             ; Integer of branch value
ONGOLP: DEC     C               ; Count branches
        LD      A,B             ; Get "GOTO" or "GOSUB" token
        JP      Z,ONJMP         ; Go to that line if right one
        CALL    GETLN           ; Get line number to DE
        CP      ','             ; Another line number?
        RET     NZ              ; No - Drop through
        Jr      ONGOLP          ; Yes - loop

IF:     CALL    EVAL            ; Evaluate expression
        LD      A,(HL)          ; Get token
        CP      ZGOTO           ; "GOTO" token?
        Jr      Z,IFGO          ; Yes - Get line
        CALL    CHKSYN          ; Make sure it's "THEN"
        DB      ZTHEN           ; "THEN" token
        DEC     HL              ; Cancel increment
IFGO:   CALL    TSTNUM          ; Make sure it's numeric
        CALL    TSTSGN          ; Test state of expression
        JP      Z,REM           ; False - Drop through
        CALL    GETCHR          ; Get next character
        JP      C,GOTO          ; Number - GOTO that line
        JP      IFJMP           ; Otherwise do statement

MRPRNT: DEC     HL              ; DEC 'cos GETCHR INCs
        CALL    GETCHR          ; Get next character
        jr	PRINT
PRINT:  Jr      Z,PRNTCRLF      ; CRLF if just PRINT
PRNTLP: RET     Z               ; End of list - Exit
        CP      ZTAB            ; "TAB(" token?
        Jr      Z,DOTAB         ; Yes - Do TAB routine
        CP      ZSPC            ; "SPC(" token?
        Jr      Z,DOTAB         ; Yes - Do SPC routine
        PUSH    HL              ; Save code string address
        CP      ','             ; Comma?
        Jr      Z,DOCOM         ; Yes - Move to next zone
        CP      59 ;";"         ; Semi-colon?
        JP      Z,NEXITM        ; Do semi-colon routine
        POP     BC              ; Code string address to BC
        CALL    EVAL            ; Evaluate expression
        PUSH    HL              ; Save code string address
        LD      A,(TYPE)        ; Get variable type
        OR      A               ; Is it a string variable?
        Jr      NZ,PRNTST       ; Yes - Output string contents
        CALL    NUMASC          ; Convert number to text
        CALL    CRTST           ; Create temporary string
        LD      (HL),' '        ; Followed by a space
        LD      HL,(FPREG)      ; Get length of output
        INC     (HL)            ; Plus 1 for the space
        LD      HL,(FPREG)      ; < Not needed >
        LD      A,(LWIDTH)      ; Get width of line
        LD      B,A             ; To B
        INC     B               ; Width 255 (No limit)?
        Jr      Z,PRNTNB        ; Yes - Output number string
        INC     B               ; Adjust it
        LD      A,(CURPOS)      ; Get cursor position
        ADD     A,(HL)          ; Add length of string
        DEC     A               ; Adjust it
        CP      B               ; Will output fit on this line?
        CALL    NC,PRNTCRLF     ; No - CRLF first
PRNTNB: CALL    PRS1            ; Output string at (HL)
        XOR     A               ; Skip CALL by setting 'z' flag
PRNTST: CALL    NZ,PRS1         ; Output string at (HL)
        POP     HL              ; Restore code string address
        Jr      MRPRNT          ; See if more to PRINT

STTLIN: LD      A,(CURPOS)      ; Make sure on new line
        OR      A               ; Already at start?
        RET     Z               ; Yes - Do nothing
        Jr      PRNTCRLF        ; Start a new line

ENDINP: LD      (HL),0          ; Mark end of buffer
        LD      HL,BUFFER-1     ; Point to buffer
PRNTCRLF: LD    A,CR            ; Load a CR
        CALL    OUTC            ; Output character
DONULL: LD      A,LF            ; Load a LF
        CALL    OUTC            ; Output character
        XOR     A               ; Set to position 0
        LD      (CURPOS),A      ; Store it
        LD      A,(NULLS)       ; Get number of nulls
NULLP:  DEC     A               ; Count them
        RET     Z               ; Return if done
        PUSH    AF              ; Save count
        XOR     A               ; Load a null
        CALL    OUTC            ; Output it
        POP     AF              ; Restore count
        Jr      NULLP           ; Keep counting

DOCOM:  LD      A,(COMMAN)      ; Get comma width
        LD      B,A             ; Save in B
        LD      A,(CURPOS)      ; Get current position
        CP      B               ; Within the limit?
        CALL    NC,PRNTCRLF     ; No - output CRLF
        Jr      NC,NEXITM       ; Get next item
ZONELP: SUB     14              ; Next zone of 14 characters
        Jr      NC,ZONELP       ; Repeat if more zones
        CPL                     ; Number of spaces to output
        Jr      ASPCS           ; Output them

DOTAB:  PUSH    AF              ; Save token
        CALL    FNDNUM          ; Evaluate expression
        CALL    CHKSYN          ; Make sure ")" follows
        DB   ")"
        DEC     HL              ; Back space on to ")"
        POP     AF              ; Restore token
        SUB     ZSPC            ; Was it "SPC(" ?
        PUSH    HL              ; Save code string address
        Jr      Z,DOSPC         ; Yes - Do 'E' spaces
        LD      A,(CURPOS)      ; Get current position
DOSPC:  CPL                     ; Number of spaces to print to
        ADD     A,E             ; Total number to print
        Jr      NC,NEXITM       ; TAB < Current POS(X)
ASPCS:  INC     A               ; Output A spaces
        LD      B,A             ; Save number to print
        LD      A,' '           ; Space
SPCLP:  CALL    OUTC            ; Output character in A
        djnz	SPCLP        ; Repeat if more
NEXITM: POP     HL              ; Restore code string address
        CALL    GETCHR          ; Get next character
        JP      PRNTLP          ; More to print

WIDTH:  CALL    GETINT          ; Get integer 0-255
        LD      A,E             ; Width to A
        LD      (LWIDTH),A      ; Set width
        RET

NULL:   CALL    GETINT          ; Get integer 0-255
        RET     NZ              ; Return if bad value
	inc	a
        LD      (NULLS),A       ; Set nulls number
        RET

REDO:   DB   "?redo from start",CR,0

BADINP: LD      A,(READFG)      ; READ or INPUT?
        OR      A
        JP      NZ,DATSNR       ; READ - ?SN Error
        POP     BC              ; Throw away code string addr
        LD      HL,REDO         ; "Redo from start" message
        CALL    PRS             ; Output string
        JP      DOAGN           ; Do last INPUT again

INPUT:  CALL    IDTEST          ; Test for illegal direct
        LD      A,(HL)          ; Get character after "INPUT"
        CP      '"'             ; Is there a prompt string?
        LD      A,0             ; Clear A and leave flags
        LD      (CTLOFG),A      ; Enable output
        Jr      NZ,NOPMPT       ; No prompt - get input
        CALL    QTSTR           ; Get string terminated by '"'
        CALL    CHKSYN          ; Check for ';' after prompt
        DB   ';'
        PUSH    HL              ; Save code string address
        CALL    PRS1            ; Output prompt string
        DB   3EH             ; Skip "PUSH HL"
NOPMPT: PUSH    HL              ; Save code string address
        CALL    PROMPT          ; Get input with "? " prompt
        POP     BC              ; Restore code string address
        JP      C,INPBRK        ; Break pressed - Exit
        INC     HL              ; Next byte
        LD      A,(HL)          ; Get it
        OR      A               ; End of line?
        DEC     HL              ; Back again
        PUSH    BC              ; Re-save code string address
        JP      Z,NXTDTA        ; Yes - Find next DATA stmt
        LD      (HL),','        ; Store comma as separator
        Jr      NXTITM          ; Get next item

READ:   PUSH    HL              ; Save code string address
        LD      HL,(NXTDAT)     ; Next DATA statement
        DB   0F6H            ; Flag "READ"
NXTITM: XOR     A               ; Flag "INPUT"
        LD      (READFG),A      ; Save "READ"/"INPUT" flag
        EX      (SP),HL         ; Get code str' , Save pointer
        Jr      GTVLUS          ; Get values

NEDMOR: CALL    CHKSYN          ; Check for comma between items
        DB      ','
GTVLUS: CALL    GETVAR          ; Get variable name
        EX      (SP),HL         ; Save code str" , Get pointer
        PUSH    DE              ; Save variable address
        LD      A,(HL)          ; Get next "INPUT"/"DATA" byte
        CP      ','             ; Comma?
        Jr      Z,ANTVLU        ; Yes - Get another value
        LD      A,(READFG)      ; Is it READ?
        OR      A
        Jr      NZ,FDTLP        ; Yes - Find next DATA stmt
        LD      A,'?'           ; More INPUT needed
        CALL    OUTC            ; Output character
        CALL    PROMPT          ; Get INPUT with prompt
        POP     DE              ; Variable address
        POP     BC              ; Code string address
        JP      C,INPBRK        ; Break pressed
        INC     HL              ; Point to next DATA byte
        LD      A,(HL)          ; Get byte
        OR      A               ; Is it zero (No input) ?
        DEC     HL              ; Back space INPUT pointer
        PUSH    BC              ; Save code string address
        JP      Z,NXTDTA        ; Find end of buffer
        PUSH    DE              ; Save variable address
ANTVLU: LD      A,(TYPE)        ; Check data type
        OR      A               ; Is it numeric?
        Jr      Z,INPBIN        ; Yes - Convert to binary
        CALL    GETCHR          ; Get next character
        LD      D,A             ; Save input character
        LD      B,A             ; Again
        CP      '"'             ; Start of literal sting?
        Jr      Z,STRENT        ; Yes - Create string entry
        LD      A,(READFG)      ; "READ" or "INPUT" ?
        OR      A
        LD      D,A             ; Save 00 if "INPUT"
        Jr      Z,ITMSEP        ; "INPUT" - End with 00
        LD      D,':'           ; "DATA" - End with 00 or ':'
ITMSEP: LD      B,','           ; Item separator
        DEC     HL              ; Back space for DTSTR
STRENT: CALL    DTSTR           ; Get string terminated by D
        EX      DE,HL           ; String address to DE
        LD      HL,LTSTND       ; Where to go after LETSTR
        EX      (SP),HL         ; Save HL , get input pointer
        PUSH    DE              ; Save address of string
        JP      LETSTR          ; Assign string to variable

INPBIN: CALL    GETCHR          ; Get next character
        CALL    ASCTFP          ; Convert ASCII to FP number
        EX      (SP),HL         ; Save input ptr, Get var addr
        CALL    FPTHL           ; Move FPREG to variable
        POP     HL              ; Restore input pointer
LTSTND: DEC     HL              ; DEC 'cos GETCHR INCs
        CALL    GETCHR          ; Get next character
        Jr      Z,MORDT         ; End of line - More needed?
        CP      ','             ; Another value?
        JP      NZ,BADINP       ; No - Bad input
MORDT:  EX      (SP),HL         ; Get code string address
        DEC     HL              ; DEC 'cos GETCHR INCs
        CALL    GETCHR          ; Get next character
        Jr      NZ,NEDMOR       ; More needed - Get it
        POP     DE              ; Restore DATA pointer
        LD      A,(READFG)      ; "READ" or "INPUT" ?
        OR      A
        EX      DE,HL           ; DATA pointer to HL
        JP      NZ,UPDATA       ; Update DATA pointer if "READ"
        PUSH    DE              ; Save code string address
        OR      (HL)            ; More input given?
        LD      HL,EXTIG        ; "?Extra ignored" message
        CALL    NZ,PRS          ; Output string if extra given
        POP     HL              ; Restore code string address
        RET

EXTIG:  DB   "?extra ignored",CR,0

FDTLP:  CALL    DATA            ; Get next statement
        OR      A               ; End of line?
        Jr      NZ,FANDT        ; No - See if DATA statement
        INC     HL
        LD      A,(HL)          ; End of program?
        INC     HL
        OR      (HL)            ; 00 00 Ends program
        LD      E,OD            ; ?OD Error
        JP      Z,ERROR         ; Yes - Out of DATA
        INC     HL
        LD      E,(HL)          ; LSB of line number
        INC     HL
        LD      D,(HL)          ; MSB of line number
        EX      DE,HL
        LD      (DATLIN),HL     ; Set line of current DATA item
        EX      DE,HL
FANDT:  CALL    GETCHR          ; Get next character
        CP      ZDATA           ; "DATA" token
        Jr      NZ,FDTLP        ; No "DATA" - Keep looking
        JP      ANTVLU          ; Found - Convert input

NEXT:   LD      DE,0            ; In case no index given
NEXT1:  CALL    NZ,GETVAR       ; Get index address
        LD      (BRKLIN),HL     ; Save code string address
        CALL    BAKSTK          ; Look for "FOR" block
        JP      NZ,NFERR        ; No "FOR" - ?NF Error
        LD      SP,HL           ; Clear nested loops
        PUSH    DE              ; Save index address
        LD      A,(HL)          ; Get sign of STEP
        INC     HL
        PUSH    AF              ; Save sign of STEP
        PUSH    DE              ; Save index address
        CALL    PHLTFP          ; Move index value to FPREG
        EX      (SP),HL         ; Save address of TO value
        PUSH    HL              ; Save address of index
        CALL    ADDPHL          ; Add STEP to index value
        POP     HL              ; Restore address of index
        CALL    FPTHL           ; Move value to index variable
        POP     HL              ; Restore address of TO value
        CALL    LOADFP          ; Move TO value to BCDE
        PUSH    HL              ; Save address of line of FOR
        CALL    CMPNUM          ; Compare index with TO value
        POP     HL              ; Restore address of line num
        POP     BC              ; Address of sign of STEP
        SUB     B               ; Compare with expected sign
        CALL    LOADFP          ; BC = Loop stmt,DE = Line num
        Jr      Z,KILFOR        ; Loop finished - Terminate it
        EX      DE,HL           ; Loop statement line number
        LD      (LINEAT),HL     ; Set loop line number
        LD      L,C             ; Set code string to loop
        LD      H,B
        JP      PUTFID          ; Put back "FOR" and continue

KILFOR: LD      SP,HL           ; Remove "FOR" block
        LD      HL,(BRKLIN)     ; Code string after "NEXT"
        LD      A,(HL)          ; Get next byte in code string
        CP      ','             ; More NEXTs ?
        JP      NZ,RUNCNT       ; No - Do next statement
        CALL    GETCHR          ; Position to index name
        CALL    NEXT1           ; Re-enter NEXT routine
; < will not RETurn to here , Exit to RUNCNT or Loop >

GETNUM: CALL    EVAL            ; Get a numeric expression
TSTNUM: DB      0F6H            ; Clear carry (numeric)
TSTSTR: SCF                     ; Set carry (string)
CHKTYP: LD      A,(TYPE)        ; Check types match
        ADC     A,A             ; Expected + actual
        OR      A               ; Clear carry , set parity
        RET     PE              ; Even parity - Types match
        JP      TMERR           ; Different types - Error

;???
	call	CHKSYN
	db 0B4h
	jp	EVAL
;---

OPNPAR: CALL    CHKSYN          ; Make sure "(" follows
        DB   "("
EVAL:   DEC     HL              ; Evaluate expression & save
        LD      D,0             ; Precedence value
EVAL1:  PUSH    DE              ; Save precedence
        LD      C,1
        CALL    CHKSTK          ; Check for 1 level of stack
        CALL    OPRND           ; Get next expression value
EVAL2:  LD      (NXTOPR),HL     ; Save address of next operator
EVAL3:  LD      HL,(NXTOPR)     ; Restore address of next opr
        POP     BC              ; Precedence value and operator
        LD      A,B             ; Get precedence value
        CP      78H             ; "AND" or "OR" ?
        CALL    NC,TSTNUM       ; No - Make sure it's a number
        LD      A,(HL)          ; Get next operator / function
        LD      D,0             ; Clear Last relation
RLTLP:  SUB     ZGTR            ; ">" Token
        Jr      C,FOPRND        ; + - * / ^ AND OR - Test it
        CP      ZLTH+1-ZGTR     ; < = >
        Jr      NC,FOPRND       ; Function - Call it
        CP      ZEQUAL-ZGTR     ; "="
        RLA                     ; <- Test for legal
        XOR     D               ; <- combinations of < = >
        CP      D               ; <- by combining last token
        LD      D,A             ; <- with current one
        JP      C,SNERR         ; Error if "<<' '==" or ">>"
        LD      (CUROPR),HL     ; Save address of current token
        CALL    GETCHR          ; Get next character
        Jr      RLTLP           ; Treat the two as one

FOPRND: LD      A,D             ; < = > found ?
        OR      A
        JP      NZ,TSTRED       ; Yes - Test for reduction
        LD      A,(HL)          ; Get operator token
        LD      (CUROPR),HL     ; Save operator address
        SUB     ZPLUS           ; Operator or function?
        RET     C               ; Neither - Exit
        CP      ZOR+1-ZPLUS     ; Is it + - * / ^ AND OR ?
        RET     NC              ; No - Exit
        LD      E,A             ; Coded operator
        LD      A,(TYPE)        ; Get data type
        DEC     A               ; FF = numeric , 00 = string
        OR      E               ; Combine with coded operator
        LD      A,E             ; Get coded operator
        JP      Z,CONCAT        ; String concatenation
        RLCA                    ; Times 2
        ADD     A,E             ; Times 3
        LD      E,A             ; To DE (D is 0)
        LD      HL,PRITAB       ; Precedence table
        ADD     HL,DE           ; To the operator concerned
        LD      A,B             ; Last operator precedence
        LD      D,(HL)          ; Get evaluation precedence
        CP      D               ; Compare with eval precedence
        RET     NC              ; Exit if higher precedence
        INC     HL              ; Point to routine address
        CALL    TSTNUM          ; Make sure it's a number

STKTHS: PUSH    BC              ; Save last precedence & token
        LD      BC,EVAL3        ; Where to go on prec' break
        PUSH    BC              ; Save on stack for return
        LD      B,E             ; Save operator
        LD      C,D             ; Save precedence
        CALL    STAKFP          ; Move value to stack
        LD      E,B             ; Restore operator
        LD      D,C             ; Restore precedence
        LD      C,(HL)          ; Get LSB of routine address
        INC     HL
        LD      B,(HL)          ; Get MSB of routine address
        INC     HL
        PUSH    BC              ; Save routine address
        LD      HL,(CUROPR)     ; Address of current operator
        Jr      EVAL1           ; Loop until prec' break

OPRND:  XOR     A               ; Get operand routine
        LD      (TYPE),A        ; Set numeric expected
        CALL    GETCHR          ; Get next character
        LD      E,MO            ; ?MO Error
        JP      Z,ERROR         ; No operand - Error
        JP      C,ASCTFP        ; Number - Get value
        CALL    CHKLTR          ; See if a letter
        Jr      NC,CONVAR       ; Letter - Find variable
 	CP      ZPLUS           ; '+' Token ?
        Jr      Z,OPRND         ; Yes - Look for operand
        CP      '.'             ; '.' ?
        JP      Z,ASCTFP        ; Yes - Create FP number
        CP      ZMINUS          ; '-' Token ?
        Jr      Z,MINUS         ; Yes - Do minus
        CP      '"'             ; Literal string ?
        JP      Z,QTSTR         ; Get string terminated by '"'
        CP      ZNOT            ; "NOT" Token ?
        JP      Z,EVNOT         ; Yes - Eval NOT expression
        CP      ZFN             ; "FN" Token ?
        JP      Z,DOFN          ; Yes - Do FN routine
        SUB     ZSGN            ; Is it a function?
        Jr      NC,FNOFST       ; Yes - Evaluate function
EVLPAR: CALL    OPNPAR          ; Evaluate expression in "()"
        CALL    CHKSYN          ; Make sure ")" follows
        DB   ")"
        RET

MINUS:  LD      D,7DH           ; '-' precedence
        CALL    EVAL1           ; Evaluate until prec' break
        LD      HL,(NXTOPR)     ; Get next operator address
        PUSH    HL              ; Save next operator address
        CALL    INVSGN          ; Negate value
RETNUM: CALL    TSTNUM          ; Make sure it's a number
        POP     HL              ; Restore next operator address
        RET

CONVAR: CALL    GETVAR          ; Get variable address to DE
FRMEVL: PUSH    HL              ; Save code string address
        EX      DE,HL           ; Variable address to HL
        LD      (FPREG),HL      ; Save address of variable
        LD      A,(TYPE)        ; Get type
        OR      A               ; Numeric?
        CALL    Z,PHLTFP        ; Yes - Move contents to FPREG
        POP     HL              ; Restore code string address
        RET

FNOFST: LD      B,0             ; Get address of function
        RLCA                    ; Double function offset
        LD      C,A             ; BC = Offset in function table
        PUSH    BC              ; Save adjusted token value
        CALL    GETCHR          ; Get next character
        LD      A,C             ; Get adjusted token value
        CP      022h		; orig: 2*(ZLEFT-ZSGN)-1; Adj' LEFT$,RIGHT$ or MID$ ?
	jp	z, POINT
	cp	02Dh
        Jr      C,FNVAL         ; No - Do function
        CALL    OPNPAR          ; Evaluate expression  (X,...
        CALL    CHKSYN          ; Make sure ',' follows
        DB      ','
        CALL    TSTSTR          ; Make sure it's a string
        EX      DE,HL           ; Save code string address
        LD      HL,(FPREG)      ; Get address of string
        EX      (SP),HL         ; Save address of string
        PUSH    HL              ; Save adjusted token value
        EX      DE,HL           ; Restore code string address
        CALL    GETINT          ; Get integer 0-255
        EX      DE,HL           ; Save code string address
        EX      (SP),HL         ; Save integer,HL = adj' token
        Jr      GOFUNC          ; Jump to string function

FNVAL:  CALL    EVLPAR          ; Evaluate expression
        EX      (SP),HL         ; HL = Adjusted token value
        LD      DE,RETNUM       ; Return number from function
        PUSH    DE              ; Save on stack
GOFUNC: LD      BC,FNCTAB       ; Function routine addresses
        ADD     HL,BC           ; Point to right address
        LD      C,(HL)          ; Get LSB of address
        INC     HL              ;
        LD      H,(HL)          ; Get MSB of address
        LD      L,C             ; Address to HL
        JP      (HL)            ; Jump to function

SGNEXP: DEC     D               ; Dee to flag negative exponent
        CP      ZMINUS          ; '-' token ?
        RET     Z               ; Yes - Return
        CP      '-'             ; '-' ASCII ?
        RET     Z               ; Yes - Return
        INC     D               ; Inc to flag positive exponent
        CP      '+'             ; '+' ASCII ?
        RET     Z               ; Yes - Return
        CP      ZPLUS           ; '+' token ?
        RET     Z               ; Yes - Return
        DEC     HL              ; DEC 'cos GETCHR INCs
        RET                     ; Return "NZ"

POR:    DB      0F6H            ; Flag "OR"
PAND:   XOR     A               ; Flag "AND"
        PUSH    AF              ; Save "AND" / "OR" flag
        CALL    TSTNUM          ; Make sure it's a number
        CALL    DEINT           ; Get integer -32768 to 32767
        POP     AF              ; Restore "AND" / "OR" flag
        EX      DE,HL           ; <- Get last
        POP     BC              ; <-  value
        EX      (SP),HL         ; <-  from
        EX      DE,HL           ; <-  stack
        CALL    FPBCDE          ; Move last value to FPREG
        PUSH    AF              ; Save "AND" / "OR" flag
        CALL    DEINT           ; Get integer -32768 to 32767
        POP     AF              ; Restore "AND" / "OR" flag
        POP     BC              ; Get value
        LD      A,C             ; Get LSB
        LD      HL,ACPASS       ; Address of save AC as current
        Jr      NZ,POR1         ; Jump if OR
        AND     E               ; "AND" LSBs
        LD      C,A             ; Save LSB
        LD      A,B             ; Get MBS
        AND     D               ; "AND" MSBs
        JP      (HL)            ; Save AC as current (ACPASS)

POR1:   OR      E               ; "OR" LSBs
        LD      C,A             ; Save LSB
        LD      A,B             ; Get MSB
        OR      D               ; "OR" MSBs
        JP      (HL)            ; Save AC as current (ACPASS)

TSTRED: LD      HL,CMPLOG       ; Logical compare routine
        LD      A,(TYPE)        ; Get data type
        RRA                     ; Carry set = string
        LD      A,D             ; Get last precedence value
        RLA                     ; Times 2 plus carry
        LD      E,A             ; To E
        LD      D,64H           ; Relational precedence
        LD      A,B             ; Get current precedence
        CP      D               ; Compare with last
        RET     NC              ; Eval if last was rel' or log'
        JP      STKTHS          ; Stack this one and get next

CMPLOG: DW   CMPLG1          ; Compare two values / strings
CMPLG1: LD      A,C             ; Get data type
        OR      A
        RRA
        POP     BC              ; Get last expression to BCDE
        POP     DE
        PUSH    AF              ; Save status
        CALL    CHKTYP          ; Check that types match
        LD      HL,CMPRES       ; Result to comparison
        PUSH    HL              ; Save for RETurn
        JP      Z,CMPNUM        ; Compare values if numeric
        XOR     A               ; Compare two strings
        LD      (TYPE),A        ; Set type to numeric
        PUSH    DE              ; Save string name
        CALL    GSTRCU          ; Get current string
        LD      A,(HL)          ; Get length of string
        INC     HL
        INC     HL
        LD      C,(HL)          ; Get LSB of address
        INC     HL
        LD      B,(HL)          ; Get MSB of address
        POP     DE              ; Restore string name
        PUSH    BC              ; Save address of string
        PUSH    AF              ; Save length of string
        CALL    GSTRDE          ; Get second string
        CALL    LOADFP          ; Get address of second string
        POP     AF              ; Restore length of string 1
        LD      D,A             ; Length to D
        POP     HL              ; Restore address of string 1
CMPSTR: LD      A,E             ; Bytes of string 2 to do
        OR      D               ; Bytes of string 1 to do
        RET     Z               ; Exit if all bytes compared
        LD      A,D             ; Get bytes of string 1 to do
        SUB     1
        RET     C               ; Exit if end of string 1
        XOR     A
        CP      E               ; Bytes of string 2 to do
        INC     A
        RET     NC              ; Exit if end of string 2
        DEC     D               ; Count bytes in string 1
        DEC     E               ; Count bytes in string 2
        LD      A,(BC)          ; Byte in string 2
        CP      (HL)            ; Compare to byte in string 1
        INC     HL              ; Move up string 1
        INC     BC              ; Move up string 2
        Jr      Z,CMPSTR        ; Same - Try next bytes
        CCF                     ; Flag difference (">" or "<")
        JP      FLGDIF          ; "<" gives -1 , ">" gives +1

CMPRES: INC     A               ; Increment current value
        ADC     A,A             ; Double plus carry
        POP     BC              ; Get other value
        AND     B               ; Combine them
        ADD     A,-1            ; Carry set if different
        SBC     A,A             ; 00 - Equal , FF - Different
        JP      FLGREL          ; Set current value & continue

EVNOT:  LD      D,5AH           ; Precedence value for "NOT"
        CALL    EVAL1           ; Eval until precedence break
        CALL    TSTNUM          ; Make sure it's a number
        CALL    DEINT           ; Get integer -32768 - 32767
        LD      A,E             ; Get LSB
        CPL                     ; Invert LSB
        LD      C,A             ; Save "NOT" of LSB
        LD      A,D             ; Get MSB
        CPL                     ; Invert MSB
        CALL    ACPASS          ; Save AC as current
        POP     BC              ; Clean up stack
        JP      EVAL3           ; Continue evaluation

DIMRET: DEC     HL              ; DEC 'cos GETCHR INCs
        CALL    GETCHR          ; Get next character
        RET     Z               ; End of DIM statement
        CALL    CHKSYN          ; Make sure ',' follows
        DB      ','
DIM:    LD      BC,DIMRET       ; Return to "DIMRET"
        PUSH    BC              ; Save on stack
        DB      0F6H            ; Flag "Create" variable
GETVAR: XOR     A               ; Find variable address,to DE
        LD      (LCRFLG),A      ; Set locate / create flag
        LD      B,(HL)          ; Get First byte of name
GTFNAM: CALL    CHKLTR          ; See if a letter
        JP      C,SNERR         ; ?SN Error if not a letter
        XOR     A
        LD      C,A             ; Clear second byte of name
        LD      (TYPE),A        ; Set type to numeric
        CALL    GETCHR          ; Get next character
        Jr      C,SVNAM2        ; Numeric - Save in name
        CALL    CHKLTR          ; See if a letter
        Jr      C,CHARTY        ; Not a letter - Check type
SVNAM2: LD      C,A             ; Save second byte of name
ENDNAM: CALL    GETCHR          ; Get next character
        Jr      C,ENDNAM        ; Numeric - Get another
        CALL    CHKLTR          ; See if a letter
        Jr      NC,ENDNAM       ; Letter - Get another
CHARTY: SUB     '$'             ; String variable?
        Jr      NZ,NOTSTR       ; No - Numeric variable
        INC     A               ; A = 1 (string type)
        LD      (TYPE),A        ; Set type to string
        RRCA                    ; A = 80H , Flag for string
        ADD     A,C             ; 2nd byte of name has bit 7 on
        LD      C,A             ; Resave second byte on name
        CALL    GETCHR          ; Get next character
NOTSTR: LD      A,(FORFLG)      ; Array name needed ?
        DEC     A
        JP      Z,ARLDSV        ; Yes - Get array name
        JP      P,NSCFOR        ; No array with "FOR" or "FN"
        LD      A,(HL)          ; Get byte again
        SUB     '('             ; Subscripted variable?
        Jr      Z,SBSCPT        ; Yes - Sort out subscript

NSCFOR: XOR     A               ; Simple variable
        LD      (FORFLG),A      ; Clear "FOR" flag
        PUSH    HL              ; Save code string address
        LD      D,B             ; DE = Variable name to find
        LD      E,C
        LD      HL,(FNRGNM)     ; FN argument name
        CALL    CPDEHL          ; Is it the FN argument?
        LD      DE,FNARG        ; Point to argument value
        JP      Z,POPHRT        ; Yes - Return FN argument value
        LD      HL,(VAREND)     ; End of variables
        EX      DE,HL           ; Address of end of search
        LD      HL,(PROGND)     ; Start of variables address
FNDVAR: CALL    CPDEHL          ; End of variable list table?
        Jr      Z,CFEVAL        ; Yes - Called from EVAL?
        LD      A,C             ; Get second byte of name
        SUB     (HL)            ; Compare with name in list
        INC     HL              ; Move on to first byte
        Jr      NZ,FNTHR        ; Different - Find another
        LD      A,B             ; Get first byte of name
        SUB     (HL)            ; Compare with name in list
FNTHR:  INC     HL              ; Move on to LSB of value
        Jr      Z,RETADR        ; Found - Return address
        INC     HL              ; <- Skip
        INC     HL              ; <- over
        INC     HL              ; <- F.P.
        INC     HL              ; <- value
        Jr      FNDVAR          ; Keep looking

CFEVAL: POP     HL              ; Restore code string address
        EX      (SP),HL         ; Get return address
        PUSH    DE              ; Save address of variable
        LD      DE,FRMEVL       ; Return address in EVAL
        CALL    CPDEHL          ; Called from EVAL ?
        POP     DE              ; Restore address of variable
        Jr      Z,RETNUL        ; Yes - Return null variable
        EX      (SP),HL         ; Put back return
        PUSH    HL              ; Save code string address
        PUSH    BC              ; Save variable name
        LD      BC,6            ; 2 byte name plus 4 byte data
        LD      HL,(ARREND)     ; End of arrays
        PUSH    HL              ; Save end of arrays
        ADD     HL,BC           ; Move up 6 bytes
        POP     BC              ; Source address in BC
        PUSH    HL              ; Save new end address
        CALL    MOVUP           ; Move arrays up
        POP     HL              ; Restore new end address
        LD      (ARREND),HL     ; Set new end address
        LD      H,B             ; End of variables to HL
        LD      L,C
        LD      (VAREND),HL     ; Set new end address

ZEROLP: DEC     HL              ; Back through to zero variable
        LD      (HL),0          ; Zero byte in variable
        CALL    CPDEHL          ; Done them all?
        Jr      NZ,ZEROLP       ; No - Keep on going
        POP     DE              ; Get variable name
        LD      (HL),E          ; Store second character
        INC     HL
        LD      (HL),D          ; Store first character
        INC     HL
RETADR: EX      DE,HL           ; Address of variable in DE
        POP     HL              ; Restore code string address
        RET

RETNUL: LD      (FPEXP),A       ; Set result to zero
        LD      HL,ZERBYT       ; Also set a null string
        LD      (FPREG),HL      ; Save for EVAL
        POP     HL              ; Restore code string address
        RET

SBSCPT: PUSH    HL              ; Save code string address
        LD      HL,(LCRFLG)     ; Locate/Create and Type
        EX      (SP),HL         ; Save and get code string
        LD      D,A             ; Zero number of dimensions
SCPTLP: PUSH    DE              ; Save number of dimensions
        PUSH    BC              ; Save array name
        CALL    FPSINT          ; Get subscript (0-32767)
        POP     BC              ; Restore array name
        POP     AF              ; Get number of dimensions
        EX      DE,HL
        EX      (SP),HL         ; Save subscript value
        PUSH    HL              ; Save LCRFLG and TYPE
        EX      DE,HL
        INC     A               ; Count dimensions
        LD      D,A             ; Save in D
        LD      A,(HL)          ; Get next byte in code string
        CP      ','             ; Comma (more to come)?
        Jr      Z,SCPTLP        ; Yes - More subscripts
        CALL    CHKSYN          ; Make sure ")" follows
        DB      ")"
        LD      (NXTOPR),HL     ; Save code string address
        POP     HL              ; Get LCRFLG and TYPE
        LD      (LCRFLG),HL     ; Restore Locate/create & type
        LD      E,0             ; Flag not CSAVE* or CLOAD*
        PUSH    DE              ; Save number of dimensions (D)
        DB      11H             ; Skip "PUSH HL" and "PUSH AF'

ARLDSV: PUSH    HL              ; Save code string address
        PUSH    AF              ; A = 00 , Flags set = Z,N
        LD      HL,(VAREND)     ; Start of arrays
        DB      3EH             ; Skip "ADD HL,DE"
FNDARY: ADD     HL,DE           ; Move to next array start
        LD      de,(ARREND)     ; End of arrays
        CALL    CPDEHL          ; End of arrays found?
        Jr      Z,CREARY        ; Yes - Create array
        LD      A,(HL)          ; Get second byte of name
        CP      C               ; Compare with name given
        INC     HL              ; Move on
        Jr      NZ,NXTARY       ; Different - Find next array
        LD      A,(HL)          ; Get first byte of name
        CP      B               ; Compare with name given
NXTARY: INC     HL              ; Move on
        LD      E,(HL)          ; Get LSB of next array address
        INC     HL
        LD      D,(HL)          ; Get MSB of next array address
        INC     HL
        Jr      NZ,FNDARY       ; Not found - Keep looking
        LD      A,(LCRFLG)      ; Found Locate or Create it?
        OR      A
        JP      NZ,DDERR        ; Create - ?DD Error
        POP     AF              ; Locate - Get number of dim'ns
        LD      B,H             ; BC Points to array dim'ns
        LD      C,L
        JP      Z,POPHRT        ; Jump if array load/save
        SUB     (HL)            ; Same number of dimensions?
        Jr      Z,FINDEL        ; Yes - Find element
BSERR:  LD      E,BS            ; ?BS Error
        JP      ERROR           ; Output error

CREARY: LD      DE,4            ; 4 Bytes per entry
        POP     AF              ; Array to save or 0 dim'ns?
        JP      Z,FCERR         ; Yes - ?FC Error
        LD      (HL),C          ; Save second byte of name
        INC     HL
        LD      (HL),B          ; Save first byte of name
        INC     HL
        LD      C,A             ; Number of dimensions to C
        CALL    CHKSTK          ; Check if enough memory
        INC     HL              ; Point to number of dimensions
        INC     HL
        LD      (CUROPR),HL     ; Save address of pointer
        LD      (HL),C          ; Set number of dimensions
        INC     HL
        LD      A,(LCRFLG)      ; Locate of Create?
        RLA                     ; Carry set = Create
        LD      A,C             ; Get number of dimensions
CRARLP: LD      BC,10+1         ; Default dimension size 10
        Jr      NC,DEFSIZ       ; Locate - Set default size
        POP     BC              ; Get specified dimension size
        INC     BC              ; Include zero element
DEFSIZ: LD      (HL),C          ; Save LSB of dimension size
        INC     HL
        LD      (HL),B          ; Save MSB of dimension size
        INC     HL
        PUSH    AF              ; Save num' of dim'ns an status
        PUSH    HL              ; Save address of dim'n size
        CALL    MLDEBC          ; Multiply DE by BC to find
        EX      DE,HL           ; amount of mem needed (to DE)
        POP     HL              ; Restore address of dimension
        POP     AF              ; Restore number of dimensions
        DEC     A               ; Count them
        Jr      NZ,CRARLP       ; Do next dimension if more
        PUSH    AF              ; Save locate/create flag
        LD      B,D             ; MSB of memory needed
        LD      C,E             ; LSB of memory needed
        EX      DE,HL
        ADD     HL,DE           ; Add bytes to array start
        JP      C,OMERR         ; Too big - Error
        CALL    ENFMEM          ; See if enough memory
        LD      (ARREND),HL     ; Save new end of array

ZERARY: DEC     HL              ; Back through array data
        LD      (HL),0          ; Set array element to zero
        CALL    CPDEHL          ; All elements zeroed?
        Jr      NZ,ZERARY       ; No - Keep on going
        INC     BC              ; Number of bytes + 1
        LD      D,A             ; A=0
        LD      HL,(CUROPR)     ; Get address of array
        LD      E,(HL)          ; Number of dimensions
        EX      DE,HL           ; To HL
        ADD     HL,HL           ; Two bytes per dimension size
        ADD     HL,BC           ; Add number of bytes
        EX      DE,HL           ; Bytes needed to DE
        DEC     HL
        DEC     HL
        LD      (HL),E          ; Save LSB of bytes needed
        INC     HL
        LD      (HL),D          ; Save MSB of bytes needed
        INC     HL
        POP     AF              ; Locate / Create?
        Jr      C,ENDDIM        ; A is 0 , End if create
FINDEL: LD      B,A             ; Find array element
        LD      C,A
        LD      A,(HL)          ; Number of dimensions
        INC     HL
        DB      16H             ; Skip "POP HL"
FNDELP: POP     HL              ; Address of next dim' size
        LD      E,(HL)          ; Get LSB of dim'n size
        INC     HL
        LD      D,(HL)          ; Get MSB of dim'n size
        INC     HL
        EX      (SP),HL         ; Save address - Get index
        PUSH    AF              ; Save number of dim'ns
        CALL    CPDEHL          ; Dimension too large?
        Jr      NC,BSERR        ; Yes - ?BS Error
        PUSH    HL              ; Save index
        CALL    MLDEBC          ; Multiply previous by size
        POP     DE              ; Index supplied to DE
        ADD     HL,DE           ; Add index to pointer
        POP     AF              ; Number of dimensions
        DEC     A               ; Count them
        LD      B,H             ; MSB of pointer
        LD      C,L             ; LSB of pointer
        Jr      NZ,FNDELP       ; More - Keep going
        ADD     HL,HL           ; 4 Bytes per element
        ADD     HL,HL
        POP     BC              ; Start of array
        ADD     HL,BC           ; Point to element
        EX      DE,HL           ; Address of element to DE
ENDDIM: LD      HL,(NXTOPR)     ; Got code string address
        RET

FRE:    LD      HL,(ARREND)     ; Start of free memory
        EX      DE,HL           ; To DE
        LD      HL,0            ; End of free memory
        ADD     HL,SP           ; Current stack value
        LD      A,(TYPE)        ; Dummy argument type
        OR      A
        Jr      Z,FRENUM        ; Numeric - Free variable space
        CALL    GSTRCU          ; Current string to pool
        CALL    GARBGE          ; Garbage collection
        LD      HL,(STRSPC)     ; Bottom of string space in use
        EX      DE,HL           ; To DE
        LD      HL,(STRBOT)     ; Bottom of string space
FRENUM: LD      A,L             ; Get LSB of end
        SUB     E               ; Subtract LSB of beginning
        LD      C,A             ; Save difference if C
        LD      A,H             ; Get MSB of end
        SBC     A,D             ; Subtract MSB of beginning
ACPASS: LD      B,C             ; Return integer AC
ABPASS: LD      D,B             ; Return integer AB
        LD      E,0
        LD      HL,TYPE         ; Point to type
        LD      (HL),E          ; Set type to numeric
        LD      B,80H+16        ; 16 bit integer
        JP      RETINT          ; Return the integr

POS:    LD      A,(CURPOS)      ; Get cursor position
PASSA:  LD      B,A             ; Put A into AB
        XOR     A               ; Zero A
        JP      ABPASS          ; Return integer AB

DEF:    CALL    CHEKFN          ; Get "FN" and name
        CALL    IDTEST          ; Test for illegal direct
        LD      BC,DATA         ; To get next statement
        PUSH    BC              ; Save address for RETurn
        PUSH    DE              ; Save address of function ptr
        CALL    CHKSYN          ; Make sure "(" follows
        DB      "("
        CALL    GETVAR          ; Get argument variable name
        PUSH    HL              ; Save code string address
        EX      DE,HL           ; Argument address to HL
        DEC     HL
        LD      D,(HL)          ; Get first byte of arg name
        DEC     HL
        LD      E,(HL)          ; Get second byte of arg name
        POP     HL              ; Restore code string address
        CALL    TSTNUM          ; Make sure numeric argument
        CALL    CHKSYN          ; Make sure ")" follows
        DB      ")"
        CALL    CHKSYN          ; Make sure "=" follows
        DB      ZEQUAL          ; "=" token
        LD      B,H             ; Code string address to BC
        LD      C,L
        EX      (SP),HL         ; Save code str , Get FN ptr
        LD      (HL),C          ; Save LSB of FN code string
        INC     HL
        LD      (HL),B          ; Save MSB of FN code string
        JP      SVSTAD          ; Save address and do function

DOFN:   CALL    CHEKFN          ; Make sure FN follows
        PUSH    DE              ; Save function pointer address
        CALL    EVLPAR          ; Evaluate expression in "()"
        CALL    TSTNUM          ; Make sure numeric result
        EX      (SP),HL         ; Save code str , Get FN ptr
        LD      E,(HL)          ; Get LSB of FN code string
        INC     HL
        LD      D,(HL)          ; Get MSB of FN code string
        INC     HL
        LD      A,D             ; And function DEFined?
        OR      E
        JP      Z,UFERR         ; No - ?UF Error
        LD      A,(HL)          ; Get LSB of argument address
        INC     HL
        LD      H,(HL)          ; Get MSB of argument address
        LD      L,A             ; HL = Arg variable address
        PUSH    HL              ; Save it
        LD      HL,(FNRGNM)     ; Get old argument name
        EX      (SP),HL ;       ; Save old , Get new
        LD      (FNRGNM),HL     ; Set new argument name
        LD      HL,(FNARG+2)    ; Get LSB,NLSB of old arg value
        PUSH    HL              ; Save it
        LD      HL,(FNARG)      ; Get MSB,EXP of old arg value
        PUSH    HL              ; Save it
        LD      HL,FNARG        ; HL = Value of argument
        PUSH    DE              ; Save FN code string address
        CALL    FPTHL           ; Move FPREG to argument
        POP     HL              ; Get FN code string address
        CALL    GETNUM          ; Get value from function
        DEC     HL              ; DEC 'cos GETCHR INCs
        CALL    GETCHR          ; Get next character
        JP      NZ,SNERR        ; Bad character in FN - Error
        POP     HL              ; Get MSB,EXP of old arg
        LD      (FNARG),HL      ; Restore it
        POP     HL              ; Get LSB,NLSB of old arg
        LD      (FNARG+2),HL    ; Restore it
        POP     HL              ; Get name of old arg
        LD      (FNRGNM),HL     ; Restore it
        POP     HL              ; Restore code string address
        RET

IDTEST: PUSH    HL              ; Save code string address
        LD      HL,(LINEAT)     ; Get current line number
        INC     HL              ; -1 means direct statement
        LD      A,H
        OR      L
        POP     HL              ; Restore code string address
        RET     NZ              ; Return if in program
        LD      E,ID            ; ?ID Error
        JP      ERROR

CHEKFN: CALL    CHKSYN          ; Make sure FN follows
        DB      ZFN             ; "FN" token
        LD      A,80H
        LD      (FORFLG),A      ; Flag FN name to find
        OR      (HL)            ; FN name has bit 7 set
        LD      B,A             ; in first byte of name
        CALL    GTFNAM          ; Get FN name
        JP      TSTNUM          ; Make sure numeric function

STR:    CALL    TSTNUM          ; Make sure it's a number
        CALL    NUMASC          ; Turn number into text
STR1:   CALL    CRTST           ; Create string entry for it
        CALL    GSTRCU          ; Current string to pool
        LD      BC,TOPOOL       ; Save in string pool
        PUSH    BC              ; Save address on stack

SAVSTR: LD      A,(HL)          ; Get string length
        INC     HL
        INC     HL
        PUSH    HL              ; Save pointer to string
        CALL    TESTR           ; See if enough string space
        POP     HL              ; Restore pointer to string
        LD      C,(HL)          ; Get LSB of address
        INC     HL
        LD      B,(HL)          ; Get MSB of address
        CALL    CRTMST          ; Create string entry
        PUSH    HL              ; Save pointer to MSB of addr
        LD      L,A             ; Length of string
        CALL    TOSTRA          ; Move to string area
        POP     DE              ; Restore pointer to MSB
        RET

MKTMST: CALL    TESTR           ; See if enough string space
CRTMST: LD      HL,TMPSTR       ; Temporary string
        PUSH    HL              ; Save it
        LD      (HL),A          ; Save length of string
        INC     HL
SVSTAD: INC     HL
        LD      (HL),E          ; Save LSB of address
        INC     HL
        LD      (HL),D          ; Save MSB of address
        POP     HL              ; Restore pointer
        RET

CRTST:  DEC     HL              ; DEC - INCed after
QTSTR:  LD      B,'"'           ; Terminating quote
        LD      D,B             ; Quote to D
DTSTR:  PUSH    HL              ; Save start
        LD      C,-1            ; Set counter to -1
QTSTLP: INC     HL              ; Move on
        LD      A,(HL)          ; Get byte
        INC     C               ; Count bytes
        OR      A               ; End of line?
        Jr      Z,CRTSTE        ; Yes - Create string entry
        CP      D               ; Terminator D found?
        Jr      Z,CRTSTE        ; Yes - Create string entry
        CP      B               ; Terminator B found?
        Jr      NZ,QTSTLP       ; No - Keep looking
CRTSTE: CP      '"'             ; End with '"'?
        CALL    Z,GETCHR        ; Yes - Get next character
        EX      (SP),HL         ; Starting quote
        INC     HL              ; First byte of string
        EX      DE,HL           ; To DE
        LD      A,C             ; Get length
        CALL    CRTMST          ; Create string entry
TSTOPL: LD      DE,TMPSTR       ; Temporary string
        LD      HL,(TMSTPT)     ; Temporary string pool pointer
        LD      (FPREG),HL      ; Save address of string ptr
        LD      A,1
        LD      (TYPE),A        ; Set type to string
        CALL    DETHL4          ; Move string to pool
        CALL    CPDEHL          ; Out of string pool?
        LD      (TMSTPT),HL     ; Save new pointer
        POP     HL              ; Restore code string address
        LD      A,(HL)          ; Get next code byte
        RET     NZ              ; Return if pool OK
        LD      E,ST            ; ?ST Error
        JP      ERROR           ; String pool overflow

PRNUMS: INC     HL              ; Skip leading space
PRS:    CALL    CRTST           ; Create string entry for it
PRS1:   CALL    GSTRCU          ; Current string to pool
        CALL    LOADFP          ; Move string block to BCDE
        INC     E               ; Length + 1
PRSLP:  DEC     E               ; Count characters
        RET     Z               ; End of string
        LD      A,(BC)          ; Get byte to output
        CALL    OUTC            ; Output character in A
        CP      CR              ; Return?
        CALL    Z,DONULL        ; Yes - Do nulls
        INC     BC              ; Next byte in string
        Jr      PRSLP           ; More characters to output

TESTR:  OR      A               ; Test if enough room
        DB      0EH             ; No garbage collection done
GRBDON: POP     AF              ; Garbage collection done
        PUSH    AF              ; Save status
        LD      de,(STRSPC)     ; Bottom of string space in use
        LD      HL,(STRBOT)     ; Bottom of string area
        CPL                     ; Negate length (Top down)
        LD      C,A             ; -Length to BC
        LD      B,-1            ; BC = -ve length of string
        ADD     HL,BC           ; Add to bottom of space in use
        INC     HL              ; Plus one for 2's complement
        CALL    CPDEHL          ; Below string RAM area?
        Jr      C,TESTOS        ; Tidy up if not done else err
        LD      (STRBOT),HL     ; Save new bottom of area
        INC     HL              ; Point to first byte of string
        EX      DE,HL           ; Address to DE
  	POP     AF              ; Throw away status push
        RET

TESTOS: POP     AF              ; Garbage collect been done?
        LD      E,OS            ; ?OS Error
        JP      Z,ERROR         ; Yes - Not enough string apace
        CP      A               ; Flag garbage collect done
        PUSH    AF              ; Save status
        LD      BC,GRBDON       ; Garbage collection done
        PUSH    BC              ; Save for RETurn
GARBGE: LD      HL,(LSTRAM)     ; Get end of RAM pointer
GARBLP: LD      (STRBOT),HL     ; Reset string pointer
        LD      HL,0
        PUSH    HL              ; Flag no string found
        LD      HL,(STRSPC)     ; Get bottom of string space
        PUSH    HL              ; Save bottom of string space
        LD      HL,TMSTPL       ; Temporary string pool
GRBLP:  LD      de,(TMSTPT)     ; Temporary string pool pointer
        CALL    CPDEHL          ; Temporary string pool done?
        LD      BC,GRBLP        ; Loop until string pool done
        Jr      NZ,STPOOL       ; No - See if in string area
        LD      HL,(PROGND)     ; Start of simple variables
SMPVAR: LD      de,(VAREND)     ; End of simple variables
        CALL    CPDEHL          ; All simple strings done?
        Jr      Z,ARRLP         ; Yes - Do string arrays
        LD      A,(HL)          ; Get type of variable
        INC     HL
        INC     HL
        OR      A               ; "S" flag set if string
        CALL    STRADD          ; See if string in string area
        Jr      SMPVAR          ; Loop until simple ones done

GNXARY: POP     BC              ; Scrap address of this array
ARRLP:  LD      de,(ARREND)     ; End of string arrays
        CALL    CPDEHL          ; All string arrays done?
        Jr      Z,SCNEND        ; Yes - Move string if found
        CALL    LOADFP          ; Get array name to BCDE
        LD      A,E             ; Get type of array
        PUSH    HL              ; Save address of num of dim'ns
        ADD     HL,BC           ; Start of next array
        OR      A               ; Test type of array
        JP      P,GNXARY        ; Numeric array - Ignore it
        LD      (CUROPR),HL     ; Save address of next array
        POP     HL              ; Get address of num of dim'ns
        LD      C,(HL)          ; BC = Number of dimensions
        LD      B,0
        ADD     HL,BC           ; Two bytes per dimension size
        ADD     HL,BC
        INC     HL              ; Plus one for number of dim'ns
GRBARY: LD      de,(CUROPR)     ; Get address of next array
        CALL    CPDEHL          ; Is this array finished?
        Jr      Z,ARRLP         ; Yes - Get next one
        LD      BC,GRBARY       ; Loop until array all done

STPOOL: PUSH    BC              ; Save return address
        OR      80H             ; Flag string type
STRADD: LD      A,(HL)          ; Get string length
        INC     HL
        INC     HL
        LD      E,(HL)          ; Get LSB of string address
        INC     HL
        LD      D,(HL)          ; Get MSB of string address
        INC     HL
        RET     P               ; Not a string - Return
        OR      A               ; Set flags on string length
        RET     Z               ; Null string - Return
        LD      B,H             ; Save variable pointer
        LD      C,L
        LD      HL,(STRBOT)     ; Bottom of new area
        CALL    CPDEHL          ; String been done?
        LD      H,B             ; Restore variable pointer
        LD      L,C
        RET     C               ; String done - Ignore
        POP     HL              ; Return address
        EX      (SP),HL         ; Lowest available string area
        CALL    CPDEHL          ; String within string area?
        EX      (SP),HL         ; Lowest available string area
        PUSH    HL              ; Re-save return address
        LD      H,B             ; Restore variable pointer
        LD      L,C
        RET     NC              ; Outside string area - Ignore
        POP     BC              ; Get return , Throw 2 away
        POP     AF              ;
        POP     AF              ;
        PUSH    HL              ; Save variable pointer
        PUSH    DE              ; Save address of current
        PUSH    BC              ; Put back return address
        RET                     ; Go to it

SCNEND: POP     DE              ; Addresses of strings
        POP     HL              ;
        LD      A,L             ; HL = 0 if no more to do
        OR      H
        RET     Z               ; No more to do - Return
        DEC     HL
        LD      B,(HL)          ; MSB of address of string
        DEC     HL
        LD      C,(HL)          ; LSB of address of string
        PUSH    HL              ; Save variable address
        DEC     HL
        DEC     HL
        LD      L,(HL)          ; HL = Length of string
        LD      H,0
        ADD     HL,BC           ; Address of end of string+1
        LD      D,B             ; String address to DE
        LD      E,C
        DEC     HL              ; Last byte in string
        LD      B,H             ; Address to BC
        LD      C,L
        LD      HL,(STRBOT)     ; Current bottom of string area
        CALL    MOVSTR          ; Move string to new address
        POP     HL              ; Restore variable address
        LD      (HL),C          ; Save new LSB of address
        INC     HL
        LD      (HL),B          ; Save new MSB of address
        LD      L,C             ; Next string area+1 to HL
        LD      H,B
        DEC     HL              ; Next string area address
        JP      GARBLP          ; Look for more strings

CONCAT: PUSH    BC              ; Save prec' opr & code string
        PUSH    HL              ;
        LD      HL,(FPREG)      ; Get first string
        EX      (SP),HL         ; Save first string
        CALL    OPRND           ; Get second string
        EX      (SP),HL         ; Restore first string
        CALL    TSTSTR          ; Make sure it's a string
        LD      A,(HL)          ; Get length of second string
        PUSH    HL              ; Save first string
        LD      HL,(FPREG)      ; Get second string
        PUSH    HL              ; Save second string
        ADD     A,(HL)          ; Add length of second string
        LD      E,LS            ; ?LS Error
        JP      C,ERROR         ; String too long - Error
        CALL    MKTMST          ; Make temporary string
        POP     DE              ; Get second string to DE
        CALL    GSTRDE          ; Move to string pool if needed
        EX      (SP),HL         ; Get first string
        CALL    GSTRHL          ; Move to string pool if needed
        PUSH    HL              ; Save first string
        LD      HL,(TMPSTR+2)   ; Temporary string address
        EX      DE,HL           ; To DE
        CALL    SSTSA           ; First string to string area
        CALL    SSTSA           ; Second string to string area
        LD      HL,EVAL2        ; Return to evaluation loop
        EX      (SP),HL         ; Save return,get code string
        PUSH    HL              ; Save code string address
        JP      TSTOPL          ; To temporary string to pool

SSTSA:  POP     HL              ; Return address
        EX      (SP),HL         ; Get string block,save return
        LD      A,(HL)          ; Get length of string
        INC     HL
        INC     HL
        LD      C,(HL)          ; Get LSB of string address
        INC     HL
        LD      B,(HL)          ; Get MSB of string address
        LD      L,A             ; Length to L
TOSTRA: INC     L               ; INC - DECed after
TSALP:  DEC     L               ; Count bytes moved
        RET     Z               ; End of string - Return
        LD      A,(BC)          ; Get source
        LD      (DE),A          ; Save destination
        INC     BC              ; Next source
        INC     DE              ; Next destination
        Jr      TSALP           ; Loop until string moved

GETSTR: CALL    TSTSTR          ; Make sure it's a string
GSTRCU: LD      HL,(FPREG)      ; Get current string
GSTRHL: EX      DE,HL           ; Save DE
GSTRDE: CALL    BAKTMP          ; Was it last tmp-str?
        EX      DE,HL           ; Restore DE
        RET     NZ              ; No - Return
        PUSH    DE              ; Save string
        LD      D,B             ; String block address to DE
        LD      E,C
        DEC     DE              ; Point to length
        LD      C,(HL)          ; Get string length
        LD      HL,(STRBOT)     ; Current bottom of string area
        CALL    CPDEHL          ; Last one in string area?
        Jr      NZ,POPHL        ; No - Return
        LD      B,A             ; Clear B (A=0)
        ADD     HL,BC           ; Remove string from str' area
        LD      (STRBOT),HL     ; Save new bottom of str' area
POPHL:  POP     HL              ; Restore string
        RET

BAKTMP: LD      HL,(TMSTPT)     ; Get temporary string pool top
        DEC     HL              ; Back
        LD      B,(HL)          ; Get MSB of address
        DEC     HL              ; Back
        LD      C,(HL)          ; Get LSB of address
        DEC     HL              ; Back
        DEC     HL              ; Back
        CALL    CPDEHL          ; String last in string pool?
        RET     NZ              ; Yes - Leave it
        LD      (TMSTPT),HL     ; Save new string pool top
        RET

LEN:    LD      BC,PASSA        ; To return integer A
        PUSH    BC              ; Save address
GETLEN: CALL    GETSTR          ; Get string and its length
        XOR     A
        LD      D,A             ; Clear D
        LD      (TYPE),A        ; Set type to numeric
        LD      A,(HL)          ; Get length of string
        OR      A               ; Set status flags
        RET

ASC:    LD      BC,PASSA        ; To return integer A
        PUSH    BC              ; Save address
GTFLNM: CALL    GETLEN          ; Get length of string
        JP      Z,FCERR         ; Null string - Error
        INC     HL
        INC     HL
        LD      E,(HL)          ; Get LSB of address
        INC     HL
        LD      D,(HL)          ; Get MSB of address
        LD      A,(DE)          ; Get first byte of string
        RET

CHR:    LD      A,1             ; One character string
        CALL    MKTMST          ; Make a temporary string
        CALL    MAKINT          ; Make it integer A
        LD      HL,(TMPSTR+2)   ; Get address of string
        LD      (HL),E          ; Save character
TOPOOL: POP     BC              ; Clean up stack
        JP      TSTOPL          ; Temporary string to pool

LEFT:   CALL    LFRGNM          ; Get number and ending ")"
        XOR     A               ; Start at first byte in string
RIGHT1: EX      (SP),HL         ; Save code string,Get string
        LD      C,A             ; Starting position in string
MID1:   PUSH    HL              ; Save string block address
        LD      A,(HL)          ; Get length of string
        CP      B               ; Compare with number given
        Jr      C,ALLFOL        ; All following bytes required
        LD      A,B             ; Get new length
        DB      11H             ; Skip "LD C,0"
ALLFOL: LD      C,0             ; First byte of string
        PUSH    BC              ; Save position in string
        CALL    TESTR           ; See if enough string space
        POP     BC              ; Get position in string
        POP     HL              ; Restore string block address
        PUSH    HL              ; And re-save it
        INC     HL
        INC     HL
        LD      B,(HL)          ; Get LSB of address
        INC     HL
        LD      H,(HL)          ; Get MSB of address
        LD      L,B             ; HL = address of string
        LD      B,0             ; BC = starting address
        ADD     HL,BC           ; Point to that byte
        LD      B,H             ; BC = source string
        LD      C,L
        CALL    CRTMST          ; Create a string entry
        LD      L,A             ; Length of new string
        CALL    TOSTRA          ; Move string to string area
        POP     DE              ; Clear stack
        CALL    GSTRDE          ; Move to string pool if needed
        JP      TSTOPL          ; Temporary string to pool

RIGHT:  CALL    LFRGNM          ; Get number and ending ")"
        POP     DE              ; Get string length
        PUSH    DE              ; And re-save
        LD      A,(DE)          ; Get length
        SUB     B               ; Move back N bytes
        Jr      RIGHT1          ; Go and get sub-string

MID:    EX      DE,HL           ; Get code string address
        LD      A,(HL)          ; Get next byte ',' or ")"
        CALL    MIDNUM          ; Get number supplied
        INC     B               ; Is it character zero?
        DEC     B
        JP      Z,FCERR         ; Yes - Error
        PUSH    BC              ; Save starting position
        LD      E,255           ; All of string
        CP      ')'             ; Any length given?
        Jr      Z,RSTSTR        ; No - Rest of string
        CALL    CHKSYN          ; Make sure ',' follows
        DB      ','
        CALL    GETINT          ; Get integer 0-255
RSTSTR: CALL    CHKSYN          ; Make sure ")" follows
        DB      ")"
        POP     AF              ; Restore starting position
        EX      (SP),HL         ; Get string,8ave code string
        LD      BC,MID1         ; Continuation of MID$ routine
        PUSH    BC              ; Save for return
        DEC     A               ; Starting position-1
        CP      (HL)            ; Compare with length
        LD      B,0             ; Zero bytes length
        RET     NC              ; Null string if start past end
        LD      C,A             ; Save starting position-1
        LD      A,(HL)          ; Get length of string
        SUB     C               ; Subtract start
        CP      E               ; Enough string for it?
        LD      B,A             ; Save maximum length available
        RET     C               ; Truncate string if needed
        LD      B,E             ; Set specified length
        RET                     ; Go and create string

VAL:    CALL    GETLEN          ; Get length of string
        JP      Z,RESZER        ; Result zero
        LD      E,A             ; Save length
        INC     HL
        INC     HL
        LD      A,(HL)          ; Get LSB of address
        INC     HL
        LD      H,(HL)          ; Get MSB of address
        LD      L,A             ; HL = String address
        PUSH    HL              ; Save string address
        ADD     HL,DE
        LD      B,(HL)          ; Get end of string+1 byte
        LD      (HL),D          ; Zero it to terminate
        EX      (SP),HL         ; Save string end,get start
        PUSH    BC              ; Save end+1 byte
        LD      A,(HL)          ; Get starting byte
	CALL    ASCTFP          ; Convert ASCII string to FP
	POP     BC              ; Restore end+1 byte
        POP     HL              ; Restore end+1 address
        LD      (HL),B          ; Put back original byte
        RET

LFRGNM: EX      DE,HL           ; Code string address to HL
        CALL    CHKSYN          ; Make sure ")" follows
        DB      ")"
MIDNUM: POP     BC              ; Get return address
        POP     DE              ; Get number supplied
        PUSH    BC              ; Re-save return address
        LD      B,E             ; Number to B
        RET

INP:    CALL    MAKINT          ; Make it integer A
	ld	c, a
	in	a, (c)
        JP      PASSA           ; Return integer A

POUT:   CALL    SETIO           ; Set up port number
	ld	a, (OTPORT)
	ld	c, a
	ld	a, e
	out	(c), a
	ret

WAIT:   CALL    SETIO           ; Set up port number
        PUSH    AF              ; Save AND mask
        LD      E,0             ; Assume zero if none given
        DEC     HL              ; DEC 'cos GETCHR INCs
        CALL    GETCHR          ; Get next character
        Jr      Z,NOXOR         ; No XOR byte given
        CALL    CHKSYN          ; Make sure ',' follows
        DB      ','
        CALL    GETINT          ; Get integer 0-255 to XOR with
NOXOR:  POP     BC              ; Restore AND mask
	ld	a, (OTPORT)
	ld	c, a
loc_5372:
	in	a, (c)
	xor	e
	and	b
	jr	z, loc_5372
	ret

SETIO:  CALL    GETINT          ; Get integer 0-255
        LD      (OTPORT),A      ; Set output port
        CALL    CHKSYN          ; Make sure ',' follows
        DB      ','
        Jr      GETINT          ; Get integer 0-255 and return

FNDNUM: CALL    GETCHR          ; Get next character
GETINT: CALL    GETNUM          ; Get a number from 0 to 255
MAKINT: CALL    DEPINT          ; Make sure value 0 - 255
        LD      A,D             ; Get MSB of number
        OR      A               ; Zero?
        JP      NZ,FCERR        ; No - Error
        DEC     HL              ; DEC 'cos GETCHR INCs
        CALL    GETCHR          ; Get next character
        LD      A,E             ; Get number to A
        RET

PEEK:   CALL    DEINT           ; Get memory address
        LD      A,(DE)          ; Get byte in memory
        JP      PASSA           ; Return integer A

POKE:   CALL    GETNUM          ; Get memory address
        CALL    DEINT           ; Get integer -32768 to 3276
        PUSH    DE              ; Save memory address
        CALL    CHKSYN          ; Make sure ',' follows
        DB      ','
        CALL    GETINT          ; Get integer 0-255
        POP     DE              ; Restore memory address
        LD      (DE),A          ; Load it into memory
        RET

DEEK:   CALL    DEINT           ; Get integer -32768 to 32767
        PUSH    DE              ; Save number
        POP     HL              ; Number to HL
        LD      B,(HL)          ; Get LSB of contents
        INC     HL
        LD      A,(HL)          ; Get MSB of contents
        JP      ABPASS          ; Return integer AB

DOKE:   CALL    GETNUM          ; Get a number
        CALL    DEINT           ; Get integer -32768 to 32767
        PUSH    DE              ; Save address
        CALL    CHKSYN          ; Make sure ',' follows
        DB      ','
        CALL    GETNUM          ; Get a number
        CALL    DEINT           ; Get integer -32768 to 32767
        EX      (SP),HL         ; Save value,get address
        LD      (HL),E          ; Save LSB of value
        INC     HL
        LD      (HL),D          ; Save MSB of value
        POP     HL              ; Restore code string address
        RET

ROUND:  LD      HL,HALF         ; Add 0.5 to FPREG
ADDPHL: CALL    LOADFP          ; Load FP at (HL) to BCDE
        Jr      FPADD           ; Add BCDE to FPREG

SUBPHL: CALL    LOADFP          ; FPREG = -FPREG + number at HL
        DB      21H             ; Skip "POP BC" and "POP DE"
PSUB:   POP     BC              ; Get FP number from stack
        POP     DE
SUBCDE: CALL    INVSGN          ; Negate FPREG
FPADD:  LD      A,B             ; Get FP exponent
        OR      A               ; Is number zero?
        RET     Z               ; Yes - Nothing to add
        LD      A,(FPEXP)       ; Get FPREG exponent
        OR      A               ; Is this number zero?
        JP      Z,FPBCDE        ; Yes - Move BCDE to FPREG
        SUB     B               ; BCDE number larger?
        Jr      NC,NOSWAP       ; No - Don't swap them
        CPL                     ; Two's complement
        INC     A               ;  FP exponent
        EX      DE,HL
        CALL    STAKFP          ; Put FPREG on stack
        EX      DE,HL
        CALL    FPBCDE          ; Move BCDE to FPREG
        POP     BC              ; Restore number from stack
        POP     DE
NOSWAP: CP      24+1            ; Second number insignificant?
        RET     NC              ; Yes - First number is result
        PUSH    AF              ; Save number of bits to scale
        CALL    SIGNS           ; Set MSBs & sign of result
        LD      H,A             ; Save sign of result
        POP     AF              ; Restore scaling factor
        CALL    SCALE           ; Scale BCDE to same exponent
        OR      H               ; Result to be positive?
        LD      HL,FPREG        ; Point to FPREG
        JP      P,MINCDE        ; No - Subtract FPREG from CDE
        CALL    PLUCDE          ; Add FPREG to CDE
        JP      NC,RONDUP       ; No overflow - Round it up
        INC     HL              ; Point to exponent
        INC     (HL)            ; Increment it
        JP      Z,OVERR         ; Number overflowed - Error
        LD      L,1             ; 1 bit to shift right
        CALL    SHRT1           ; Shift result right
        JP      RONDUP          ; Round it up

MINCDE: XOR     A               ; Clear A and carry
        SUB     B               ; Negate exponent
        LD      B,A             ; Re-save exponent
        LD      A,(HL)          ; Get LSB of FPREG
        SBC     A, E            ; Subtract LSB of BCDE
        LD      E,A             ; Save LSB of BCDE
        INC     HL
        LD      A,(HL)          ; Get NMSB of FPREG
        SBC     A,D             ; Subtract NMSB of BCDE
        LD      D,A             ; Save NMSB of BCDE
        INC     HL
        LD      A,(HL)          ; Get MSB of FPREG
        SBC     A,C             ; Subtract MSB of BCDE
        LD      C,A             ; Save MSB of BCDE
CONPOS: CALL    C,COMPL         ; Overflow - Make it positive

BNORM:  LD      L,B             ; L = Exponent
        LD      H,E             ; H = LSB
        XOR     A
BNRMLP: LD      B,A             ; Save bit count
        LD      A,C             ; Get MSB
        OR      A               ; Is it zero?
        Jr      NZ,PNORM        ; No - Do it bit at a time
        LD      C,D             ; MSB = NMSB
        LD      D,H             ; NMSB= LSB
        LD      H,L             ; LSB = VLSB
        LD      L,A             ; VLSB= 0
        LD      A,B             ; Get exponent
        SUB     8               ; Count 8 bits
        CP      -24-8           ; Was number zero?
        Jr      NZ,BNRMLP       ; No - Keep normalising
RESZER: XOR     A               ; Result is zero
SAVEXP: LD      (FPEXP),A       ; Save result as zero
        RET

NORMAL: DEC     B               ; Count bits
        ADD     HL,HL           ; Shift HL left
        rl	d		; NMSB left with last bit
        rl	c		; MSB left with last bit
PNORM:  JP      P,NORMAL        ; Not done - Keep going
        LD      A,B             ; Number of bits shifted
        LD      E,H             ; Save HL in EB
        LD      B,L
        OR      A               ; Any shifting done?
        Jr      Z,RONDUP        ; No - Round it up
        LD      HL,FPEXP        ; Point to exponent
        ADD     A,(HL)          ; Add shifted bits
        LD      (HL),A          ; Re-save exponent
        Jr      NC,RESZER       ; Underflow - Result is zero
        RET     Z               ; Result is zero
RONDUP: LD      A,B             ; Get VLSB of number
RONDB:  LD      HL,FPEXP        ; Point to exponent
        OR      A               ; Any rounding?
        CALL    M,FPROND        ; Yes - Round number up
        LD      B,(HL)          ; B = Exponent
        INC     HL
        LD      A,(HL)          ; Get sign of result
        AND     10000000B       ; Only bit 7 needed
        XOR     C               ; Set correct sign
        LD      C,A             ; Save correct sign in number
        JP      FPBCDE          ; Move BCDE to FPREG

FPROND: INC     E               ; Round LSB
        RET     NZ              ; Return if ok
        INC     D               ; Round NMSB
        RET     NZ              ; Return if ok
        INC     C               ; Round MSB
        RET     NZ              ; Return if ok
        LD      C,80H           ; Set normal value
        INC     (HL)            ; Increment exponent
        RET     NZ              ; Return if ok
        JP      OVERR           ; Overflow error

PLUCDE: LD      A,(HL)          ; Get LSB of FPREG
        ADD     A,E             ; Add LSB of BCDE
        LD      E,A             ; Save LSB of BCDE
        INC     HL
        LD      A,(HL)          ; Get NMSB of FPREG
        ADC     A,D             ; Add NMSB of BCDE
        LD      D,A             ; Save NMSB of BCDE
        INC     HL
        LD      A,(HL)          ; Get MSB of FPREG
        ADC     A,C             ; Add MSB of BCDE
        LD      C,A             ; Save MSB of BCDE
        RET

COMPL:  LD      HL,SGNRES       ; Sign of result
        LD      A,(HL)          ; Get sign of result
        CPL                     ; Negate it
        LD      (HL),A          ; Put it back
        XOR     A
        LD      L,A             ; Set L to zero
        SUB     B               ; Negate exponent,set carry
        LD      B,A             ; Re-save exponent
        LD      A,L             ; Load zero
        SBC     A,E             ; Negate LSB
        LD      E,A             ; Re-save LSB
        LD      A,L             ; Load zero
        SBC     A,D             ; Negate NMSB
        LD      D,A             ; Re-save NMSB
        LD      A,L             ; Load zero
        SBC     A,C             ; Negate MSB
        LD      C,A             ; Re-save MSB
        RET

SCALE:  LD      B,0             ; Clear underflow
SCALLP: SUB     8               ; 8 bits (a whole byte)?
        Jr      C,SHRITE        ; No - Shift right A bits
        LD      B,E             ; <- Shift
        LD      E,D             ; <- right
        LD      D,C             ; <- eight
        LD      C,0             ; <- bits
        Jr      SCALLP          ; More bits to shift

SHRITE: ADD     A,8+1           ; Adjust count
        LD      L,A             ; Save bits to shift
SHRLP:  XOR     A               ; Flag for all done
        DEC     L               ; All shifting done?
        RET     Z               ; Yes - Return
        LD      A,C             ; Get MSB
SHRT1:  RRA                     ; Shift it right
        LD      C,A             ; Re-save
        rr	d		; NMSB Shift right with last bit
        rr	e		; LSB Shift right with last bit
        rr	b		; underflow Shift right with last bit
        Jr      SHRLP           ; More bits to do

UNITY:  DB       000H,000H,000H,081H    ; 1.00000

LOGTAB: DB      3                       ; Table used by LOG
        DB      0AAH,056H,019H,080H     ; 0.59898
        DB      0F1H,022H,076H,080H     ; 0.96147
        DB      045H,0AAH,038H,082H     ; 2.88539

LOG:    CALL    TSTSGN          ; Test sign of value
        OR      A
        JP      PE,FCERR        ; ?FC Error if <= zero
        LD      HL,FPEXP        ; Point to exponent
        LD      A,(HL)          ; Get exponent
        LD      BC,8035H        ; BCDE = SQR(1/2)
        LD      DE,04F3H
        SUB     B               ; Scale value to be < 1
        PUSH    AF              ; Save scale factor
        LD      (HL),B          ; Save new exponent
        PUSH    DE              ; Save SQR(1/2)
        PUSH    BC
        CALL    FPADD           ; Add SQR(1/2) to value
        POP     BC              ; Restore SQR(1/2)
        POP     DE
        INC     B               ; Make it SQR(2)
        CALL    DVBCDE          ; Divide by SQR(2)
        LD      HL,UNITY        ; Point to 1.
        CALL    SUBPHL          ; Subtract FPREG from 1
        LD      HL,LOGTAB       ; Coefficient table
        CALL    SUMSER          ; Evaluate sum of series
        LD      BC,8080H        ; BCDE = -0.5
        LD      DE,0000H
        CALL    FPADD           ; Subtract 0.5 from FPREG
        POP     AF              ; Restore scale factor
        CALL    RSCALE          ; Re-scale number
MULLN2: LD      BC,8031H        ; BCDE = Ln(2)
        LD      DE,7218H
        DB      21H             ; Skip "POP BC" and "POP DE"

MULT:   POP     BC              ; Get number from stack
        POP     DE
FPMULT: CALL    TSTSGN          ; Test sign of FPREG
        RET     Z               ; Return zero if zero
        LD      L,0             ; Flag add exponents
        CALL    ADDEXP          ; Add exponents
        LD      A,C             ; Get MSB of multiplier
        LD      (MULVAL),A      ; Save MSB of multiplier
        EX      DE,HL
        LD      (MULVAL+1),HL   ; Save rest of multiplier
        LD      BC,0            ; Partial product (BCDE) = zero
        LD      D,B
        LD      E,B
        LD      HL,BNORM        ; Address of normalise
        PUSH    HL              ; Save for return
        LD      HL,MULT8        ; Address of 8 bit multiply
        PUSH    HL              ; Save for NMSB,MSB
        PUSH    HL              ;
        LD      HL,FPREG        ; Point to number
MULT8:  LD      A,(HL)          ; Get LSB of number
        INC     HL              ; Point to NMSB
        OR      A               ; Test LSB
        Jr      Z,BYTSFT        ; Zero - shift to next byte
        PUSH    HL              ; Save address of number
        LD      L,8             ; 8 bits to multiply by
MUL8LP: RRA                     ; Shift LSB right
        LD      H,A             ; Save LSB
        LD      A,C             ; Get MSB
        Jr      NC,NOMADD       ; Bit was zero - Don't add
        PUSH    HL              ; Save LSB and count
        LD      HL,(MULVAL+1)   ; Get LSB and NMSB
        ADD     HL,DE           ; Add NMSB and LSB
        EX      DE,HL           ; Leave sum in DE
        POP     HL              ; Restore MSB and count
        LD      A,(MULVAL)      ; Get MSB of multiplier
        ADC     A,C             ; Add MSB
NOMADD: RRA                     ; Shift MSB right
        LD      C,A             ; Re-save MSB
	rr	d		; Shift NMSB right
	rr	e		; Shift LSB right
	rr	b		; Shift VLSB right
        DEC     L               ; Count bits multiplied
        LD      A,H             ; Get LSB of multiplier
        Jr      NZ,MUL8LP       ; More - Do it
POPHRT: POP     HL              ; Restore address of number
        RET

BYTSFT: LD      B,E             ; Shift partial product left
        LD      E,D
        LD      D,C
        LD      C,A
        RET

DIV10:  CALL    STAKFP          ; Save FPREG on stack
        LD      BC,8420H        ; BCDE = 10.
        LD      DE,0000H
        CALL    FPBCDE          ; Move 10 to FPREG

DIV:    POP     BC              ; Get number from stack
        POP     DE
DVBCDE: CALL    TSTSGN          ; Test sign of FPREG
        JP      Z,DZERR         ; Error if division by zero
        LD      L,-1            ; Flag subtract exponents
        CALL    ADDEXP          ; Subtract exponents
        INC     (HL)            ; Add 2 to exponent to adjust
        INC     (HL)
        DEC     HL              ; Point to MSB
        LD      A,(HL)          ; Get MSB of dividend
        LD      (DIV3),A        ; Save for subtraction
        DEC     HL
        LD      A,(HL)          ; Get NMSB of dividend
        LD      (DIV2),A        ; Save for subtraction
        DEC     HL
        LD      A,(HL)          ; Get MSB of dividend
        LD      (DIV1),A        ; Save for subtraction
        LD      B,C             ; Get MSB
        EX      DE,HL           ; NMSB,LSB to HL
        XOR     A
        LD      C,A             ; Clear MSB of quotient
        LD      D,A             ; Clear NMSB of quotient
        LD      E,A             ; Clear LSB of quotient
        LD      (DIV4),A        ; Clear overflow count
DIVLP:  PUSH    HL              ; Save divisor
        PUSH    BC
        LD      A,L             ; Get LSB of number
        CALL    DIVSUP          ; Subt' divisor from dividend
        SBC     A,0             ; Count for overflows
        CCF
        Jr      NC,RESDIV       ; Restore divisor if borrow
        LD      (DIV4),A        ; Re-save overflow count
        POP     AF              ; Scrap divisor
        POP     AF
        SCF                     ; Set carry to
        DB      0D2H            ; Skip "POP BC" and "POP HL"

RESDIV: POP     BC              ; Restore divisor
        POP     HL
        LD      A,C             ; Get MSB of quotient
        INC     A
        DEC     A
        RRA                     ; Bit 0 to bit 7
        JP      M,RONDB         ; Done - Normalise result
        RLA                     ; Restore carry
	rl	e		; Double LSB of quotient
	rl	d		; Double NMSB of quotient
	rl	c		; Double MSB of quotient
        ADD     HL,HL           ; Double NMSB,LSB of divisor
	rl	b		; Double MSB of quotient
        LD      A,(DIV4)        ; Get VLSB of quotient
        RLA                     ; Double it
        LD      (DIV4),A        ; Put it back
        LD      A,C             ; Get MSB of quotient
        OR      D               ; Merge NMSB
        OR      E               ; Merge LSB
        Jr      NZ,DIVLP        ; Not done - Keep dividing
        PUSH    HL              ; Save divisor
        LD      HL,FPEXP        ; Point to exponent
        DEC     (HL)            ; Divide by 2
        POP     HL              ; Restore divisor
        Jr      NZ,DIVLP        ; Ok - Keep going
        JP      OVERR           ; Overflow error

ADDEXP: LD      A,B             ; Get exponent of dividend
        OR      A               ; Test it
        JP      Z,OVTST3        ; Zero - Result zero
        LD      A,L             ; Get add/subtract flag
        LD      HL,FPEXP        ; Point to exponent
        XOR     (HL)            ; Add or subtract it
        ADD     A,B             ; Add the other exponent
        LD      B,A             ; Save new exponent
        RRA                     ; Test exponent for overflow
        XOR     B
        LD      A,B             ; Get exponent
        JP      P,OVTST2        ; Positive - Test for overflow
        ADD     A,80H           ; Add excess 128
        LD      (HL),A          ; Save new exponent
        JP      Z,POPHRT        ; Zero - Result zero
        CALL    SIGNS           ; Set MSBs and sign of result
        LD      (HL),A          ; Save new exponent
        DEC     HL              ; Point to MSB
        RET

OVTST1: CALL    TSTSGN          ; Test sign of FPREG
        CPL                     ; Invert sign
        POP     HL              ; Clean up stack
OVTST2: OR      A               ; Test if new exponent zero
OVTST3: POP     HL              ; Clear off return address
        JP      P,RESZER        ; Result zero
        JP      OVERR           ; Overflow error

MLSP10: CALL    BCDEFP          ; Move FPREG to BCDE
        LD      A,B             ; Get exponent
        OR      A               ; Is it zero?
        RET     Z               ; Yes - Result is zero
        ADD     A,2             ; Multiply by 4
        JP      C,OVERR         ; Overflow - ?OV Error
        LD      B,A             ; Re-save exponent
        CALL    FPADD           ; Add BCDE to FPREG (Times 5)
        LD      HL,FPEXP        ; Point to exponent
        INC     (HL)            ; Double number (Times 10)
        RET     NZ              ; Ok - Return
        JP      OVERR           ; Overflow error

TSTSGN: LD      A,(FPEXP)       ; Get sign of FPREG
        OR      A
        RET     Z               ; RETurn if number is zero
        LD      A,(FPREG+2)     ; Get MSB of FPREG
        DB      0FEH            ; Test sign
RETREL: CPL                     ; Invert sign
        RLA                     ; Sign bit to carry
FLGDIF: SBC     A,A             ; Carry to all bits of A
        RET     NZ              ; Return -1 if negative
        INC     A               ; Bump to +1
        RET                     ; Positive - Return +1

SGN:    CALL    TSTSGN          ; Test sign of FPREG
FLGREL: LD      B,80H+8         ; 8 bit integer in exponent
        LD      DE,0            ; Zero NMSB and LSB
RETINT: LD      HL,FPEXP        ; Point to exponent
        LD      C,A             ; CDE = MSB,NMSB and LSB
        LD      (HL),B          ; Save exponent
        LD      B,0             ; CDE = integer to normalise
        INC     HL              ; Point to sign of result
        LD      (HL),80H        ; Set sign of result
        RLA                     ; Carry = sign of integer
        JP      CONPOS          ; Set sign of result

ABS:    CALL    TSTSGN          ; Test sign of FPREG
        RET     P               ; Return if positive
INVSGN: LD      HL,FPREG+2      ; Point to MSB
        LD      A,(HL)          ; Get sign of mantissa
        XOR     80H             ; Invert sign of mantissa
        LD      (HL),A          ; Re-save sign of mantissa
        RET

STAKFP: EX      DE,HL           ; Save code string address
        LD      HL,(FPREG)      ; LSB,NLSB of FPREG
        EX      (SP),HL         ; Stack them,get return
        PUSH    HL              ; Re-save return
        LD      HL,(FPREG+2)    ; MSB and exponent of FPREG
        EX      (SP),HL         ; Stack them,get return
        PUSH    HL              ; Re-save return
        EX      DE,HL           ; Restore code string address
        RET

PHLTFP: CALL    LOADFP          ; Number at HL to BCDE
FPBCDE: LD      (FPREG),DE      ; Save LSB,NLSB of number
        LD      (FPREG+2),BC    ; Save MSB and exponent
	ld	d, b
	ld	e, c
        RET

BCDEFP: LD      HL,FPREG        ; Point to FPREG
LOADFP: LD      E,(HL)          ; Get LSB of number
        INC     HL
        LD      D,(HL)          ; Get NMSB of number
        INC     HL
        LD      C,(HL)          ; Get MSB of number
        INC     HL
        LD      B,(HL)          ; Get exponent of number
INCHL:  INC     HL              ; Used for conditional "INC HL"
        RET

FPTHL:  LD      DE,FPREG        ; Point to FPREG
DETHL4: LD      B,4             ; 4 bytes to move
DETHLB: LD      A,(DE)          ; Get source
        LD      (HL),A          ; Save destination
        INC     DE              ; Next source
        INC     HL              ; Next destination
        djnz	DETHLB       ; Loop if more
        RET

SIGNS:  LD      HL,FPREG+2      ; Point to MSB of FPREG
        LD      A,(HL)          ; Get MSB
        RLCA                    ; Old sign to carry
        SCF                     ; Set MSBit
        RRA                     ; Set MSBit of MSB
        LD      (HL),A          ; Save new MSB
        CCF                     ; Complement sign
        RRA                     ; Old sign to carry
        INC     HL
        INC     HL
        LD      (HL),A          ; Set sign of result
        LD      A,C             ; Get MSB
        RLCA                    ; Old sign to carry
        SCF                     ; Set MSBit
        RRA                     ; Set MSBit of MSB
        LD      C,A             ; Save MSB
        RRA
        XOR     (HL)            ; New sign of result
        RET

CMPNUM: LD      A,B             ; Get exponent of number
        OR      A
        JP      Z,TSTSGN        ; Zero - Test sign of FPREG
        LD      HL,RETREL       ; Return relation routine
        PUSH    HL              ; Save for return
        CALL    TSTSGN          ; Test sign of FPREG
        LD      A,C             ; Get MSB of number
        RET     Z               ; FPREG zero - Number's MSB
        LD      HL,FPREG+2      ; MSB of FPREG
        XOR     (HL)            ; Combine signs
        LD      A,C             ; Get MSB of number
        RET     M               ; Exit if signs different
        CALL    CMPFP           ; Compare FP numbers
        RRA                     ; Get carry to sign
        XOR     C               ; Combine with MSB of number
        RET

CMPFP:  INC     HL              ; Point to exponent
        LD      A,B             ; Get exponent
        CP      (HL)            ; Compare exponents
        RET     NZ              ; Different
        DEC     HL              ; Point to MBS
        LD      A,C             ; Get MSB
        CP      (HL)            ; Compare MSBs
        RET     NZ              ; Different
        DEC     HL              ; Point to NMSB
        LD      A,D             ; Get NMSB
        CP      (HL)            ; Compare NMSBs
        RET     NZ              ; Different
        DEC     HL              ; Point to LSB
        LD      A,E             ; Get LSB
        SUB     (HL)            ; Compare LSBs
        RET     NZ              ; Different
        POP     HL              ; Drop RETurn
        POP     HL              ; Drop another RETurn
        RET

FPINT:  LD      B,A             ; <- Move
        LD      C,A             ; <- exponent
        LD      D,A             ; <- to all
        LD      E,A             ; <- bits
        OR      A               ; Test exponent
        RET     Z               ; Zero - Return zero
        PUSH    HL              ; Save pointer to number
        CALL    BCDEFP          ; Move FPREG to BCDE
        CALL    SIGNS           ; Set MSBs & sign of result
        XOR     (HL)            ; Combine with sign of FPREG
        LD      H,A             ; Save combined signs
        CALL    M,DCBCDE        ; Negative - Decrement BCDE
        LD      A,80H+24        ; 24 bits
        SUB     B               ; Bits to shift
        CALL    SCALE           ; Shift BCDE
        LD      A,H             ; Get combined sign
        RLA                     ; Sign to carry
        CALL    C,FPROND        ; Negative - Round number up
        LD      B,0             ; Zero exponent
        CALL    C,COMPL         ; If negative make positive
        POP     HL              ; Restore pointer to number
        RET

DCBCDE: DEC     DE              ; Decrement BCDE
        LD      A,D             ; Test LSBs
        AND     E
        INC     A
        RET     NZ              ; Exit if LSBs not FFFF
        DEC     BC              ; Decrement MSBs
        RET

INT:    LD      HL,FPEXP        ; Point to exponent
        LD      A,(HL)          ; Get exponent
        CP      80H+24          ; Integer accuracy only?
        LD      A,(FPREG)       ; Get LSB
        RET     NC              ; Yes - Already integer
        LD      A,(HL)          ; Get exponent
        CALL    FPINT           ; F.P to integer
        LD      (HL),80H+24     ; Save 24 bit integer
        LD      A,E             ; Get LSB of number
        PUSH    AF              ; Save LSB
        LD      A,C             ; Get MSB of number
        RLA                     ; Sign to carry
        CALL    CONPOS          ; Set sign of result
        POP     AF              ; Restore LSB of number
        RET

MLDEBC: LD      HL,0            ; Clear partial product
        LD      A,B             ; Test multiplier
        OR      C
        RET     Z               ; Return zero if zero
        LD      A,16            ; 16 bits
MLDBLP: ADD     HL,HL           ; Shift P.P left
        JP      C,BSERR         ; ?BS Error if overflow
        EX      DE,HL
        ADD     HL,HL           ; Shift multiplier left
        EX      DE,HL
        Jr      NC,NOMLAD       ; Bit was zero - No add
        ADD     HL,BC           ; Add multiplicand
        JP      C,BSERR         ; ?BS Error if overflow
NOMLAD: DEC     A               ; Count bits
        Jr      NZ,MLDBLP       ; More
        RET

ASCTFP: CP      '-'             ; Negative?
        PUSH    AF              ; Save it and flags
        Jr      Z,CNVNUM        ; Yes - Convert number
        CP      '+'             ; Positive?
        Jr      Z,CNVNUM        ; Yes - Convert number
        DEC     HL              ; DEC 'cos GETCHR INCs
CNVNUM: CALL    RESZER          ; Set result to zero
        LD      B,A             ; Digits after point counter
        LD      D,A             ; Sign of exponent
        LD      E,A             ; Exponent of ten
        CPL
        LD      C,A             ; Before or after point flag
MANLP:  CALL    GETCHR          ; Get next character
        Jr      C,ADDIG         ; Digit - Add to number
        CP      '.'
        Jr      Z,DPOINT        ; '.' - Flag point
        CP      'E'
        Jr      NZ,CONEXP       ; Not 'E' - Scale number
        CALL    GETCHR          ; Get next character
        CALL    SGNEXP          ; Get sign of exponent

EXPLP:  CALL    GETCHR          ; Get next character
        Jr      C,EDIGIT        ; Digit - Add to exponent
        INC     D               ; Is sign negative?
        Jr      NZ,CONEXP       ; No - Scale number
        XOR     A
        SUB     E               ; Negate exponent
        LD      E,A             ; And re-save it
        INC     C               ; Flag end of number
DPOINT: INC     C               ; Flag point passed
        Jr      Z,MANLP         ; Zero - Get another digit
CONEXP: PUSH    HL              ; Save code string address
        LD      A,E             ; Get exponent
        SUB     B               ; Subtract digits after point
SCALMI: CALL    P,SCALPL        ; Positive - Multiply number
        JP      P,ENDCON        ; Positive - All done
        PUSH    AF              ; Save number of times to /10
        CALL    DIV10           ; Divide by 10
        POP     AF              ; Restore count
        INC     A               ; Count divides

ENDCON: Jr      NZ,SCALMI       ; More to do
        POP     DE              ; Restore code string address
        POP     AF              ; Restore sign of number
        CALL    Z,INVSGN        ; Negative - Negate number
        EX      DE,HL           ; Code string address to HL
        RET

SCALPL: RET     Z               ; Exit if no scaling needed
MULTEN: PUSH    AF              ; Save count
        CALL    MLSP10          ; Multiply number by 10
        POP     AF              ; Restore count
        DEC     A               ; Count multiplies
        RET

ADDIG:  PUSH    DE              ; Save sign of exponent
        LD      D,A             ; Save digit
        LD      A,B             ; Get digits after point
        ADC     A,C             ; Add one if after point
        LD      B,A             ; Re-save counter
        PUSH    BC              ; Save point flags
        PUSH    HL              ; Save code string address
        PUSH    DE              ; Save digit
        CALL    MLSP10          ; Multiply number by 10
        POP     AF              ; Restore digit
        SUB     '0'             ; Make it absolute
        CALL    RSCALE          ; Re-scale number
        POP     HL              ; Restore code string address
        POP     BC              ; Restore point flags
        POP     DE              ; Restore sign of exponent
        Jr      MANLP           ; Get another digit

RSCALE: CALL    STAKFP          ; Put number on stack
        CALL    FLGREL          ; Digit to add to FPREG
PADD:   POP     BC              ; Restore number
        POP     DE
        JP      FPADD           ; Add BCDE to FPREG and return

EDIGIT: LD      A,E             ; Get digit
        RLCA                    ; Times 2
        RLCA                    ; Times 4
        ADD     A,E             ; Times 5
        RLCA                    ; Times 10
        ADD     A,(HL)          ; Add next digit
        SUB     '0'             ; Make it absolute
        LD      E,A             ; Save new digit
        Jr      EXPLP           ; Look for another digit

LINEIN: PUSH    HL              ; Save code string address
        LD      HL,INMSG        ; Output " in "
        CALL    PRS             ; Output string at HL
        POP     HL              ; Restore code string address
PRNTHL: EX      DE,HL           ; Code string address to DE
        XOR     A
        LD      B,80H+24        ; 24 bits
        CALL    RETINT          ; Return the integer
        LD      HL,PRNUMS       ; Print number string
        PUSH    HL              ; Save for return
NUMASC: LD      HL,PBUFF        ; Convert number to ASCII
        PUSH    HL              ; Save for return
        CALL    TSTSGN          ; Test sign of FPREG
        LD      (HL),' '        ; Space at start
        JP      P,SPCFST        ; Positive - Space to start
        LD      (HL),'-'        ; '-' sign at start
SPCFST: INC     HL              ; First byte of number
        LD      (HL),'0'        ; '0' if zero
        JP      Z,JSTZER        ; Return '0' if zero
        PUSH    HL              ; Save buffer address
        CALL    M,INVSGN        ; Negate FPREG if negative
        XOR     A               ; Zero A
        PUSH    AF              ; Save it
        CALL    RNGTST          ; Test number is in range
SIXDIG: LD      BC,9143H        ; BCDE - 99999.9
        LD      DE,4FF8H
        CALL    CMPNUM          ; Compare numbers
        OR      A
        JP      PO,INRNG        ; > 99999.9 - Sort it out
        POP     AF              ; Restore count
        CALL    MULTEN          ; Multiply by ten
        PUSH    AF              ; Re-save count
        Jr      SIXDIG          ; Test it again

GTSIXD: CALL    DIV10           ; Divide by 10
        POP     AF              ; Get count
        INC     A               ; Count divides
        PUSH    AF              ; Re-save count
        CALL    RNGTST          ; Test number is in range
INRNG:  CALL    ROUND           ; Add 0.5 to FPREG
        INC     A
        CALL    FPINT           ; F.P to integer
        CALL    FPBCDE          ; Move BCDE to FPREG
        LD      BC,0306H        ; 1E+06 to 1E-03 range
        POP     AF              ; Restore count
        ADD     A,C             ; 6 digits before point
        INC     A               ; Add one
        JP      M,MAKNUM        ; Do it in 'E' form if < 1E-02
        CP      6+1+1           ; More than 999999 ?
        Jr      NC,MAKNUM       ; Yes - Do it in 'E' form
        INC     A               ; Adjust for exponent
        LD      B,A             ; Exponent of number
        LD      A,2             ; Make it zero after

MAKNUM: DEC     A               ; Adjust for digits to do
        DEC     A
        POP     HL              ; Restore buffer address
        PUSH    AF              ; Save count
        LD      DE,POWERS       ; Powers of ten
        DEC     B               ; Count digits before point
        Jr      NZ,DIGTXT       ; Not zero - Do number
        LD      (HL),'.'        ; Save point
        INC     HL              ; Move on
        LD      (HL),'0'        ; Save zero
        INC     HL              ; Move on
DIGTXT: DEC     B               ; Count digits before point
        LD      (HL),'.'        ; Save point in case
        CALL    Z,INCHL         ; Last digit - move on
        PUSH    BC              ; Save digits before point
        PUSH    HL              ; Save buffer address
        PUSH    DE              ; Save powers of ten
        CALL    BCDEFP          ; Move FPREG to BCDE
        POP     HL              ; Powers of ten table
        LD      B, '0'-1        ; ASCII '0' - 1

TRYAGN: INC     B               ; Count subtractions
        LD      A,E             ; Get LSB
        SUB     (HL)            ; Subtract LSB
        LD      E,A             ; Save LSB
        INC     HL
        LD      A,D             ; Get NMSB
        SBC     A,(HL)          ; Subtract NMSB
        LD      D,A             ; Save NMSB
        INC     HL
        LD      A,C             ; Get MSB
        SBC     A,(HL)          ; Subtract MSB
        LD      C,A             ; Save MSB
        DEC     HL              ; Point back to start
        DEC     HL
        Jr      NC,TRYAGN       ; No overflow - Try again
        CALL    PLUCDE          ; Restore number
        INC     HL              ; Start of next number
        CALL    FPBCDE          ; Move BCDE to FPREG
        EX      DE,HL           ; Save point in table
        POP     HL              ; Restore buffer address
        LD      (HL),B          ; Save digit in buffer
        INC     HL              ; And move on
        POP     BC              ; Restore digit count
        DEC     C               ; Count digits
        Jr      NZ,DIGTXT       ; More - Do them
        DEC     B               ; Any decimal part?
        Jr      Z,DOEBIT        ; No - Do 'E' bit
SUPTLZ: DEC     HL              ; Move back through buffer
        LD      A,(HL)          ; Get character
        CP      '0'             ; '0' character?
        Jr      Z,SUPTLZ        ; Yes - Look back for more
        CP      '.'             ; A decimal point?
        CALL    NZ,INCHL        ; Move back over digit

DOEBIT: POP     AF              ; Get 'E' flag
        Jr      Z,NOENED        ; No 'E' needed - End buffer
        LD      (HL),'E'        ; Put 'E' in buffer
        INC     HL              ; And move on
        LD      (HL),'+'        ; Put '+' in buffer
        JP      P,OUTEXP        ; Positive - Output exponent
        LD      (HL),'-'        ; Put '-' in buffer
        CPL                     ; Negate exponent
        INC     A
OUTEXP: LD      B,'0'-1         ; ASCII '0' - 1
EXPTEN: INC     B               ; Count subtractions
        SUB     10              ; Tens digit
        Jr      NC,EXPTEN       ; More to do
        ADD     A,'0'+10        ; Restore and make ASCII
        INC     HL              ; Move on
        LD      (HL),B          ; Save MSB of exponent
JSTZER: INC     HL              ;
        LD      (HL),A          ; Save LSB of exponent
        INC     HL
NOENED: LD      (HL),C          ; Mark end of buffer
        POP     HL              ; Restore code string address
        RET

RNGTST: LD      BC,9474H        ; BCDE = 999999.
        LD      DE,23F7H
        CALL    CMPNUM          ; Compare numbers
        OR      A
        POP     HL              ; Return address to HL
        JP      PO,GTSIXD       ; Too big - Divide by ten
        JP      (HL)            ; Otherwise return to caller

HALF:   DB      00H,00H,00H,80H ; 0.5

POWERS: DB      0A0H,086H,001H  ; 100000
        DB      010H,027H,000H  ;  10000
        DB      0E8H,003H,000H  ;   1000
        DB      064H,000H,000H  ;    100
        DB      00AH,000H,000H  ;     10
        DB      001H,000H,000H  ;      1

NEGAFT: LD  HL,INVSGN           ; Negate result
        EX      (SP),HL         ; To be done after caller
        JP      (HL)            ; Return to caller

SQR:    CALL    STAKFP          ; Put value on stack
        LD      HL,HALF         ; Set power to 1/2
        CALL    PHLTFP          ; Move 1/2 to FPREG

POWER:  POP     BC              ; Get base
        POP     DE
        CALL    TSTSGN          ; Test sign of power
        LD      A,B             ; Get exponent of base
        JP      Z,EXP           ; Make result 1 if zero
        JP      P,POWER1        ; Positive base - Ok
        OR      A               ; Zero to negative power?
        JP      Z,DZERR         ; Yes - ?/0 Error
POWER1: OR      A               ; Base zero?
        JP      Z,SAVEXP        ; Yes - Return zero
        PUSH    DE              ; Save base
        PUSH    BC
        LD      A,C             ; Get MSB of base
        OR      01111111B       ; Get sign status
        CALL    BCDEFP          ; Move power to BCDE
        JP      P,POWER2        ; Positive base - Ok
        PUSH    DE              ; Save power
        PUSH    BC
        CALL    INT             ; Get integer of power
        POP     BC              ; Restore power
        POP     DE
        PUSH    AF              ; MSB of base
        CALL    CMPNUM          ; Power an integer?
        POP     HL              ; Restore MSB of base
        LD      A,H             ; but don't affect flags
        RRA                     ; Exponent odd or even?
POWER2: POP     HL              ; Restore MSB and exponent
        LD      (FPREG+2),HL    ; Save base in FPREG
        POP     HL              ; LSBs of base
        LD      (FPREG),HL      ; Save in FPREG
        CALL    C,NEGAFT        ; Odd power - Negate result
        CALL    Z,INVSGN        ; Negative base - Negate it
        PUSH    DE              ; Save power
        PUSH    BC
        CALL    LOG             ; Get LOG of base
        POP     BC              ; Restore power
        POP     DE
        CALL    FPMULT          ; Multiply LOG by power

EXP:    CALL    STAKFP          ; Put value on stack
        LD      BC,08138H       ; BCDE = 1/Ln(2)
        LD      DE,0AA3BH
        CALL    FPMULT          ; Multiply value by 1/LN(2)
        LD      A,(FPEXP)       ; Get exponent
        CP      80H+8           ; Is it in range?
        JP      NC,OVTST1       ; No - Test for overflow
        CALL    INT             ; Get INT of FPREG
        ADD     A,80H           ; For excess 128
        ADD     A,2             ; Exponent > 126?
        JP      C,OVTST1        ; Yes - Test for overflow
        PUSH    AF              ; Save scaling factor
        LD      HL,UNITY        ; Point to 1.
        CALL    ADDPHL          ; Add 1 to FPREG
        CALL    MULLN2          ; Multiply by LN(2)
        POP     AF              ; Restore scaling factor
        POP     BC              ; Restore exponent
        POP     DE
        PUSH    AF              ; Save scaling factor
        CALL    SUBCDE          ; Subtract exponent from FPREG
        CALL    INVSGN          ; Negate result
        LD      HL,EXPTAB       ; Coefficient table
        CALL    SMSER1          ; Sum the series
        LD      DE,0            ; Zero LSBs
        POP     BC              ; Scaling factor
        LD      C,D             ; Zero MSB
        JP      FPMULT          ; Scale result to correct value

EXPTAB: DB      8                       ; Table used by EXP
        DB      040H,02EH,094H,074H     ; -1/7! (-1/5040)
        DB      070H,04FH,02EH,077H     ;  1/6! ( 1/720)
        DB      06EH,002H,088H,07AH     ; -1/5! (-1/120)
        DB      0E6H,0A0H,02AH,07CH     ;  1/4! ( 1/24)
        DB      050H,0AAH,0AAH,07EH     ; -1/3! (-1/6)
        DB      0FFH,0FFH,07FH,07FH     ;  1/2! ( 1/2)
        DB      000H,000H,080H,081H     ; -1/1! (-1/1)
        DB      000H,000H,000H,081H     ;  1/0! ( 1/1)

SUMSER: CALL    STAKFP          ; Put FPREG on stack
        LD      DE,MULT         ; Multiply by "X"
        PUSH    DE              ; To be done after
        PUSH    HL              ; Save address of table
        CALL    BCDEFP          ; Move FPREG to BCDE
        CALL    FPMULT          ; Square the value
        POP     HL              ; Restore address of table
SMSER1: CALL    STAKFP          ; Put value on stack
        LD      A,(HL)          ; Get number of coefficients
        INC     HL              ; Point to start of table
        CALL    PHLTFP          ; Move coefficient to FPREG
        DB      06H             ; Skip "POP AF"
SUMLP:  POP     AF              ; Restore count
        POP     BC              ; Restore number
        POP     DE
        DEC     A               ; Cont coefficients
        RET     Z               ; All done
        PUSH    DE              ; Save number
        PUSH    BC
        PUSH    AF              ; Save count
        PUSH    HL              ; Save address in table
        CALL    FPMULT          ; Multiply FPREG by BCDE
        POP     HL              ; Restore address in table
        CALL    LOADFP          ; Number at HL to BCDE
        PUSH    HL              ; Save address in table
        CALL    FPADD           ; Add coefficient to FPREG
        POP     HL              ; Restore address in table
        Jr      SUMLP           ; More coefficients

RND:    CALL    TSTSGN          ; Test sign of FPREG
        LD      HL,SEED+2       ; Random number seed
        JP      M,RESEED        ; Negative - Re-seed
        LD      HL,LSTRND       ; Last random number
        CALL    PHLTFP          ; Move last RND to FPREG
        LD      HL,SEED+2       ; Random number seed
        RET     Z               ; Return if RND(0)
        ADD     A,(HL)          ; Add (SEED)+2)
        AND     00000111B       ; 0 to 7
        LD      B,0
        LD      (HL),A          ; Re-save seed
        INC     HL              ; Move to coefficient table
        ADD     A,A             ; 4 bytes
        ADD     A,A             ; per entry
        LD      C,A             ; BC = Offset into table
        ADD     HL,BC           ; Point to coefficient
        CALL    LOADFP          ; Coefficient to BCDE
        CALL    FPMULT  ;       ; Multiply FPREG by coefficient
        LD      A,(SEED+1)      ; Get (SEED+1)
        INC     A               ; Add 1
        AND     00000011B       ; 0 to 3
        LD      B,0
        CP      1               ; Is it zero?
        ADC     A,B             ; Yes - Make it 1
        LD      (SEED+1),A      ; Re-save seed
        LD      HL,RNDTAB-4     ; Addition table
        ADD     A,A             ; 4 bytes
        ADD     A,A             ; per entry
        LD      C,A             ; BC = Offset into table
        ADD     HL,BC           ; Point to value
        CALL    ADDPHL          ; Add value to FPREG
RND1:   CALL    BCDEFP          ; Move FPREG to BCDE
        LD      A,E             ; Get LSB
        LD      E,C             ; LSB = MSB
        XOR     01001111B       ; Fiddle around
        LD      C,A             ; New MSB
        LD      (HL),80H        ; Set exponent
        DEC     HL              ; Point to MSB
        LD      B,(HL)          ; Get MSB
        LD      (HL),80H        ; Make value -0.5
        LD      HL,SEED         ; Random number seed
        INC     (HL)            ; Count seed
        LD      A,(HL)          ; Get seed
        SUB     171             ; Do it modulo 171
        Jr      NZ,RND2         ; Non-zero - Ok
        LD      (HL),A          ; Zero seed
        INC     C               ; Fillde about
        DEC     D               ; with the
        INC     E               ; number
RND2:   CALL    BNORM           ; Normalise number
        LD      HL,LSTRND       ; Save random number
        JP      FPTHL           ; Move FPREG to last and return

RESEED: LD      (HL),A          ; Re-seed random numbers
        DEC     HL
        LD      (HL),A
        DEC     HL
        LD      (HL),A
        Jr      RND1            ; Return RND seed

RNDTAB: DB   068H,0B1H,046H,068H     ; Table used by RND
        DB   099H,0E9H,092H,069H
        DB   010H,0D1H,075H,068H

COS:    LD      HL,HALFPI       ; Point to PI/2
        CALL    ADDPHL          ; Add it to PPREG
SIN:    CALL    STAKFP          ; Put angle on stack
        LD      BC,8349H        ; BCDE = 2 PI
        LD      DE,0FDBH
        CALL    FPBCDE          ; Move 2 PI to FPREG
        POP     BC              ; Restore angle
        POP     DE
        CALL    DVBCDE          ; Divide angle by 2 PI
        CALL    STAKFP          ; Put it on stack
        CALL    INT             ; Get INT of result
        POP     BC              ; Restore number
        POP     DE
        CALL    SUBCDE          ; Make it 0 <= value < 1
        LD      HL,QUARTR       ; Point to 0.25
        CALL    SUBPHL          ; Subtract value from 0.25
        CALL    TSTSGN          ; Test sign of value
        SCF                     ; Flag positive
        JP      P,SIN1          ; Positive - Ok
        CALL    ROUND           ; Add 0.5 to value
        CALL    TSTSGN          ; Test sign of value
        OR      A               ; Flag negative
SIN1:   PUSH    AF              ; Save sign
        CALL    P,INVSGN        ; Negate value if positive
        LD      HL,QUARTR       ; Point to 0.25
        CALL    ADDPHL          ; Add 0.25 to value
        POP     AF              ; Restore sign
        CALL    NC,INVSGN       ; Negative - Make positive
        LD      HL,SINTAB       ; Coefficient table
        JP      SUMSER          ; Evaluate sum of series

HALFPI: DB   0DBH,00FH,049H,081H     ; 1.5708 (PI/2)

QUARTR: DB   000H,000H,000H,07FH     ; 0.25

SINTAB: DB   5                       ; Table used by SIN
        DB   0BAH,0D7H,01EH,086H     ; 39.711
        DB   064H,026H,099H,087H     ;-76.575
        DB   058H,034H,023H,087H     ; 81.602
        DB   0E0H,05DH,0A5H,086H     ;-41.342
        DB   0DAH,00FH,049H,083H     ;  6.2832

TAN:    CALL    STAKFP          ; Put angle on stack
        CALL    SIN             ; Get SIN of angle
        POP     BC              ; Restore angle
        POP     HL
        CALL    STAKFP          ; Save SIN of angle
        EX      DE,HL           ; BCDE = Angle
        CALL    FPBCDE          ; Angle to FPREG
        CALL    COS             ; Get COS of angle
        JP      DIV             ; TAN = SIN / COS

ATN:    CALL    TSTSGN          ; Test sign of value
        CALL    M,NEGAFT        ; Negate result after if -ve
        CALL    M,INVSGN        ; Negate value if -ve
        LD      A,(FPEXP)       ; Get exponent
        CP      81H             ; Number less than 1?
        JP      C,ATN1          ; Yes - Get arc tangnt
        LD      BC,8100H        ; BCDE = 1
        LD      D,C
        LD      E,C
        CALL    DVBCDE          ; Get reciprocal of number
        LD      HL,SUBPHL       ; Sub angle from PI/2
        PUSH    HL              ; Save for angle > 1
ATN1:   LD      HL,ATNTAB       ; Coefficient table
        CALL    SUMSER          ; Evaluate sum of series
        LD      HL,HALFPI       ; PI/2 - angle in case > 1
        RET                     ; Number > 1 - Sub from PI/2

ATNTAB: DB   9                       ; Table used by ATN
        DB   04AH,0D7H,03BH,078H     ; 1/17
        DB   002H,06EH,084H,07BH     ;-1/15
        DB   0FEH,0C1H,02FH,07CH     ; 1/13
        DB   074H,031H,09AH,07DH     ;-1/11
        DB   084H,03DH,05AH,07DH     ; 1/9
        DB   0C8H,07FH,091H,07EH     ;-1/7
        DB   0E4H,0BBH,04CH,07EH     ; 1/5
        DB   06CH,0AAH,0AAH,07FH     ;-1/3
        DB   000H,000H,000H,081H     ; 1/1

;---
PCALL:	call	sub_5ABE
	jr	z, loc_5AB5
	call	GETNUM
	call	DEINT
loc_5AB5:
	push	hl
	ld	hl, loc_5ABC
	push	hl
	ex	de, hl
	jp	(hl)

loc_5ABC:
	pop	hl
	ret

sub_5ABE:
	cp	0AEh ; 'Æ'
	ret	nz
	ld	de, 0
loc_5AC4:
	call	GETCHR
	ret	z
	jr	c, loc_5AD5
	cp	94h ; 'î'
	jr	z, loc_5AE4
	cp	47h ; 'G'
	jp	nc, SNERR
	sub	7
loc_5AD5:
	sub	'0'
	jp	c, SNERR
	ex	de, hl
	add	hl, hl
	add	hl, hl
	add	hl, hl
	add	hl, hl
	or	l
	ld	l, a
	ex	de, hl
	jr	loc_5AC4
loc_5AE4:
	ld	a, e
	add	a, a
	add	a, a
	add	a, a
	add	a, a
	or	0Dh
	ld	d, a
	ld	e, 0EFh	; 'Ô'
	jr	loc_5AC4


RENUM:	push	hl
	ld	hl, 10
	ld	(unk_6054), hl
	ld	hl, (BASTXT)
	push	hl
	inc	hl
	inc	hl
	ld	a, (hl)
	inc	hl
	ld	h, (hl)
	ld	l, a
	ld	(unk_604E), hl
	ld	(unk_6052), hl
	ex	af, af'	;'
	ld	de, (PROGND)
	dec	de
	dec	de
loc_5B0E:
	pop	hl
	push	hl
	ld	a, (hl)
	inc	hl
	ld	h, (hl)
	ld	l, a
	call	CPDEHL
	ex	(sp), hl
	jr	nz, loc_5B0E
	pop	de
	inc	hl
	inc	hl
	ld	a, (hl)
	inc	hl
	ld	h, (hl)
	ld	l, a
	ld	(unk_6050), hl
	ld	a, 4
	ex	af, af'	;'
	ld	hl, unk_604E
	ex	(sp), hl
loc_5B2B:
	jr	z, loc_5B4E
	call	ATOH
	push	af
	ld	a, d
	or	e
loc_5B33:
	jp	z, FCERR
	pop	af
	ex	(sp), hl
	ld	(hl), e
	inc	hl
	ld	(hl), d
	inc	hl
	jr	z, loc_5B4E
	ex	af, af'	;'
	dec	a
	jr	z, loc_5B4A
	ex	af, af'	;'
	ex	(sp), hl
	call	CHKSYN
	db ','
	jr	loc_5B2B
loc_5B4A:
	ex	af, af'	;'
	jp	nz, SNERR
loc_5B4E:
	ld	hl, (unk_6050)
	ld	de, (unk_604E)
	call	CPDEHL
loc_5B58:
	jp	c, FCERR
	ld	hl, (BASTXT)
loc_5B5E:
	call	SRCHLP
	jr	c, loc_5B67
	jr	z, loc_5B33
	jr	loc_5B5E
loc_5B67:
	pop	hl
	push	bc
	ld	de, (unk_6050)
	ld	hl, 0
	ld	(unk_6050), hl
loc_5B73:
	ld	h, b
	ld	l, c
	ld	c, (hl)
	inc	hl
	ld	b, (hl)
	ld	a, b
	or	c
	jr	z, loc_5B33
	inc	hl
	ld	a, (hl)
	inc	hl
	ld	h, (hl)
	ld	l, a
	call	CPDEHL
	ld	hl, (unk_6050)
	inc	hl
	ld	(unk_6050), hl
	jr	nz, loc_5B73
	inc	hl
	inc	hl
	add	hl, hl
	inc	hl
	ld	de, (PROGND)
	add	hl, de
	jr	c, loc_5B58
	call	ENFMEM
	ld	(PROGND), hl
	xor	a
	dec	hl
	ld	(hl), a
	dec	hl
	ld	(hl), a
	dec	de
	dec	de
	ex	de, hl
	ld	(hl), e
	inc	hl
	ld	(hl), d
	inc	hl
	dec	a
	ld	(hl), a
	inc	hl
	ld	(hl), a
	inc	hl
	ex	de, hl
	ld	hl, (unk_6052)
	ld	(unk_604E), hl
loc_5BB6:
	pop	hl
	ld	c, (hl)
	inc	hl
	ld	b, (hl)
	inc	hl
	push	bc
	ld	bc, unk_604E
	ld	a, (hl)
	ld	(de), a
	ld	a, (bc)
	ld	(hl), a
	inc	hl
	inc	de
	inc	bc
	ld	a, (hl)
	ld	(de), a
	ld	a, (bc)
	ld	(hl), a
	inc	de
	ld	hl, (unk_604E)
	ld	bc, (unk_6054)
	add	hl, bc
	ld	(unk_604E), hl
	ld	hl, (unk_6050)
	dec	hl
	ld	a, h
	or	l
	ld	(unk_6050), hl
	jr	nz, loc_5BB6
	ld	(de), a
	pop	hl
	ld	hl, (BASTXT)
	push	hl
loc_5BE7:
	pop	hl
	ld	c, (hl)
	inc	hl
	ld	b, (hl)
	inc	hl
	push	bc
	ld	a, (hl)
	inc	hl
	and	(hl)
	inc	a
	jr	z, loc_5C16
loc_5BF3:
	inc	hl
loc_5BF4:
	ld	a, (hl)
	or	a
	jr	z, loc_5BE7
	cp	88h ; 'à'
	jr	z, loc_5C23
	cp	8Ch ; 'å'
	jr	z, loc_5C23
	cp	8Bh ; 'ã'
	jr	z, loc_5C08
	cp	0A9h ; '©'
	jr	nz, loc_5BF3
loc_5C08:
	inc	hl
	call	ATOH
	ld	a, e
	or	d
	call	nz, sub_5C38
	call	nz, sub_5C6A
	jr	loc_5BF4

loc_5C16:
	dec	hl
	ld	(PROGND), hl
	dec	hl
	ld	(hl), a
	dec	hl
	ld	(hl), a
	pop	hl
	pop	hl
	jp	SETPTR

loc_5C23:
	inc	hl
	call	ATOH
	ld	a, e
	or	d
	jr	z, loc_5BF4
	call	sub_5C38
	call	nz, sub_5C6A
	ld	a, (hl)
	cp	','
	jr	nz, loc_5BF4
	jr	loc_5C23

sub_5C38:
	push	hl
	push	de
	ld	de, 0FFFFh
	call	SRCHLN
	pop	de
	inc	bc
	inc	bc
	inc	bc
	inc	bc
	ld	h, b
	ld	l, c
	ld	bc, (unk_6052)
loc_5C4B:
	ld	a, (hl)
	inc	hl
	push	hl
	or	(hl)
	jr	z, loc_5C67
	ld	a, (hl)
	dec	hl
	ld	l, (hl)
	ld	h, a
	call	CPDEHL
	jr	z, loc_5C64
	ld	hl, (unk_6054)
	add	hl, bc
	ld	b, h
	ld	c, l
	pop	hl
	inc	hl
	jr	loc_5C4B

loc_5C64:
	ld	a, 0FFh
	or	a
loc_5C67:
	pop	hl
	pop	hl
	ret

sub_5C6A:
	push	bc
	ex	de, hl
	ld	hl, (PROGND)
	sbc	hl, de
	push	hl
loc_5C72:
	pop	bc
	ld	h, d
	ld	l, e
	dec	de
	ld	a, (de)
	cp	','
	jr	z, loc_5C86
	cp	':'
	jr	nc, loc_5C86
	push	bc
	push	de
	ldir
	pop	de
	jr	loc_5C72

loc_5C86:
	ex	de, hl
	pop	de
	push	hl
	push	bc
	xor	a
	ld	b, 98h ; 'ò'
	call	RETINT
	call	NUMASC
	pop	bc
	pop	de
	inc	hl
	inc	de
loc_5C97:
	ld	a, (hl)
	or	a
	jr	z, loc_5CAA
	push	bc
	push	hl
	ex	de, hl
	add	hl, bc
	ld	d, h
	ld	e, l
	dec	hl
	lddr
	pop	hl
	ldi
	pop	bc
	jr	loc_5C97

loc_5CAA:
	push	de
	ld	de, (BASTXT)
	call	PTRLP
loc_5CB2:
	inc	hl
	ld	a, (hl)
	inc	hl
	or	(hl)
	jr	nz, loc_5CB2
	ex	de, hl
	ld	(hl), e
	inc	hl
	ld	(hl), d
	inc	de
	inc	de
	ld	(PROGND), de
	pop	hl
	ld	d, h
	ld	e, l
loc_5CC5:
	ld	a, (hl)
	or	a
	inc	hl
	jr	nz, loc_5CC5
	pop	bc
	ex	(sp), hl
	push	bc
	ex	de, hl
	ret

; CSAVE Abspeichern auf Kassette
; 512 x 00-Byte Vorton
; E6 Syncbyte
; 6 Byte Name
; D3 Kennung Basic
; 2 Byte Laenge (hi, lo)
; xxx Bytes
CSAVE:	call	EVAL
	push	hl
	call	GTFLNM
	ld	b, 0
csave1:	xor	a
	call	330h		;Ausgabe 1 Byte
	xor	a
	call	330h		;Ausgabe 1 Byte
	djnz	csave1
	ld	a, 0E6h		;Sync
	call	330h
	ld	b, 6
csave2:	ld	a, (de)
	call	330h		;Ausgabe 1 Byte
	inc	de
	djnz	csave2
	ld	a, 0D3h		;ID Basic Pgm
	call	330h		;Ausgabe 1 Byte
	ld	hl, (PROGND)
	ld	de, PROGST+1
	xor	a
	sbc	hl, de
	ld	a, h
	call	330h		;Ausgabe 1 Byte
	ld	a, l
	inc	hl
csave3:	call	330h		;Ausgabe 1 Byte
	dec	hl
	ld	a, l
	or	h
	ld	a, (de)
	inc	de
	jr	nz, csave3
	pop	hl
	ret

; CLOAD Einlesen von Kassette
CLOAD:	call	EVAL
	call	GTFLNM
	xor	a
	ld	(1824h), a	;phako
cload1:	call	435h		;Lesen ein Bit
	cp	0E6h 		;Sync
	jr	z, cload2
	cp	19h		;neg?
	jr	nz, cload1
	ld	a, 0FFh		;Korr
	ld	(1824h), a	;phako
cload2:	ld	b, 6
	ld	hl, (1800h)	;cupos
cload3:	call	426h		;Lesen ein Byte
	ld	(hl), a
	dec	hl
	djnz	cload3
	ld	(1800h), hl	;cupos
	call	426h		;Lesen ein Byte
	cp	0D3h 		;ID Basic Pgm
	jr	nz, cloaderr
	call	426h		;Lesen ein Byte
	ld	b, a
	call	426h		;Lesen ein Byte
	ld	c, a		;BC = Laenge
cload4:	ld	hl, (PROGND)
	dec	hl
	dec	hl
cload5:	call	426h		;Lesen ein Byte
	ld	(hl), a
	inc	hl
	dec	bc
	ld	a, b
	or	c
	jr	nz, cload5
	ld	(PROGND), hl
	jp	SETPTR
cloaderr:
	ld	hl, BadFile	; "Bad file\r"
	call	PRS
	jp	SETPTR

BadFile:	db "Bad file",0Dh,0

; POINT  (x,y) Graphikfeld Testen
; SET (x,y)   - setzt das Feld x,y auf weiss
; RESET (x,y) - setzt das Feld x,y zurueck
POINT:	xor	a
	db    1
PSET:	ld	a, 80h
	db    1
PRESET:	ld	a, 1
	push	af
	call	CHKSYN
	db '('
	call	GETINT
	cp	128
	jp	nc, FCERR
	push	af
	call	CHKSYN
	db ','
	call	GETINT
	cp	64
	jp	nc, FCERR
	ccf
	rra
	ld	d, a
	ld	c, 0
	jr	nc, point1
	inc	c
point1:	pop	af
	add	a, a
	ld	e, a
	ld	b, 2
point2:	ld	a, d
	rra
	ld	d, a
	ld	a, e
	rra
	ld	e, a
	djnz	point2
	ld	a, c
	adc	a, a
	inc	a
	ld	b, a
	xor	a
	scf
point3:	adc	a, a
	djnz	point3
	ld	c, a
	push	hl
	ld	hl, 17FFh	; BWS-Anfang
	sbc	hl, de
	ex	de, hl
	pop	hl
	ld	a, (de)
	cp	' '
	jr	c, point4
	xor	a
point4:	ld	b, a
	pop	af
	or	a
point5:	ld	a, b
	jr	z, point8
	ld	(de), a
	jp	m, point7
	ld	a, c
	cpl
	ld	c, a
	ld	a, (de)
	and	c
point6:	ld	(de), a
	call	CHKSYN
	db ')'
	ret
point7:	or	c
	jr	point6
point8:	and	c
	sub	c
	push	af
	call	CHKSYN
	db ')'
	pop	af
	pop	de
	push	hl
	ld	de, RETNUM
	push	de
	jr	z, point10
	xor	a
	ld	b, a
point9:	jp	ABPASS
point10:
	ld	b, 1
	jr	point9

;SCREEN  Cursorpositionierung
SCREEN:	call	CHKSYN
	db '('
	call    GETINT
	cp	64
	jr	nc, screen1
	push	af
	call	CHKSYN
	db ','
	call	GETINT
	cp	32
screen1:
	jp	nc, FCERR
	pop	de
	ld	e, d
	ld	d, 0
	ld	b, 6
screen2:
	sla	a
	rl	d
	djnz	screen2
	add	a, e
	ld	e, a
	push	hl
	ld	hl, 17FFh	;BWS-Anfang
	sbc	hl, de
	ld	(1800h), hl	;cupos
	pop	hl
	call	CHKSYN
	db ')'
	ret

CLS:	ld	a, 0Ch

MONOUT:	push	af
	jp	co1
coret:	pop	af
	ret

GETINP:	jp	co6

getkey:	jp	7FAh		;get input status

BYE:	jp	7FDh		;Monitoreingabeschleife

co1:	cp	8
	scf
	jr	z, co4
	cp	9
	jr	z, co4
co2:	call	10h
	jr	coret
co3:	ld	a, 0Dh
	pop	hl
	jr	co2
co4:	push	hl
	ld	hl, (1800h)	;cupos
	inc	hl
	jr	c, co5
	dec	hl
	dec	hl
	ld	a, h
	cp	0Fh
	jr	z, co3
co5:	ld	(1800h), hl	;cupos
	pop	hl
	jr	coret

co6:	push	hl
	push	de
	push	bc
	ld	hl, (1800h)	;cupos
	ld	d, (hl)
co7:	ld	(hl), 7Fh
co8:	ld	b, 0
co9:	push	bc
	ld	b, 0
co10:	djnz	co10
	pop	bc
	call	7FAh		;TASTE
	jr	nz, co12
	push	hl
	ld	hl, 18C0h
	sla	(hl)
	ld	(hl), 10h
	jr	nc, co11
	set	7, (hl)
co11:	pop	hl
	djnz	co9
	ld	a, 7Fh
	cp	(hl)
	jr	nz, co7
	ld	(hl), d
	jr	co8
co12:	ld	(hl), d
	push	af
	call	7EBh		;MS30
	in	a, (4)
	cp	92h
	jr	nz, co13
	ld	hl, 18C0h
	sla	(hl)
	ccf
	rr	(hl)
co13:	in	a, (4)
	ld	hl, cotab
co14:	ld	c, (hl)
	cp	c
	jr	z, co13
	inc	hl
	dec	c
	jr	nz, co14
	bit	7, a
	ld	hl, 18C0h
	jr	z, co16
	call	7EBh		;MS30
	dec	(hl)
	jr	z, co15
	ld	a, (hl)
	cp	80h
	jr	nz, co13
co15:	inc	(hl)
co16:	pop	af
	and	7Fh
	ld	hl, 18C0h
	ld	e, (hl)
	bit	7, e
	jr	z, co18
	cp	'A'
	jr	c, co18
	cp	'Z'+1
	jr	nc, co17
	set	5, a
	jr	co18
co17:	cp	'a'
	jr	c, co18
	cp	'z'+1
	jr	nc, co18
	res	5, a
co18:	pop	bc
	pop	de
	pop	hl
	ret

cotab:	db  92h	; 
	db  8Dh	; ç
	db    1

EDIT:	call	ATOH
	ret	nz
	pop	bc
	call	SRCHLN
	jp	nc, ULERR
	ld	h, b
	ld	l, c
	ld	a, (hl)
	inc	hl
	or	(hl)
	inc	hl
	jp	z, ULERR
	inc	hl
	inc	hl
	push	de
	push	hl
	ex	de, hl
	call	PRNTHL
	ex	de, hl
	pop	hl
	ld	a, ' '
	call	OUTC
	call	sub_4714
	ld	a, 'J'
	sub	b
	ld	hl, BUFFER-1
	ld	c, a
	ld	b, 0
	add	hl, bc
	ld	b, c
	call	MORINP
	pop	de
	ld	hl, BUFFER
	scf
	push	af
	jp	getcmd5


COUNT:	push	hl
	push	de
	ld	hl, (LINESC)
	ld	de, 0FFFFh
	adc	hl, de
	ld	(LINESC), hl
	pop	de
	pop	hl
	ret	p
	call	SETLIN
	call	GETINP
	cp	3
	jp	z, loc_474A
	jr	COUNT

;TTYLIN
sub_5F3E:
	push	hl
	ld	b, 49h
	ld	hl, BUFFER
	push	hl
	pop	ix
loc_5F47:
	ld	(hl), ' '
	inc	hl
	djnz	loc_5F47
	pop	hl
	ret
sub_5F4E:
	dec	b
	jr	z, loc_5F59
	ld	(ix+0),	a
	inc	ix
	jp	MONOUT

loc_5F59:
	pop	de
	ret

	end
