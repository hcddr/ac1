	.Z80
.LIMAGE
.PABS
.PHEX
.TITLE "RDK  B A S I C  INTERPRETER 3K  780524  V3.3"
;**********************************************************
;  RDK  B A S I C  INTERPRETER     V3.2 780524    
;**********************************************************
ANBAS=\ " ANFANGS ADRESSE BASIC"

.LOC:		100H:		;BEI CPM
		JP	BEGINN
.LOC:		ANBAS-29:
BEGINN:
		LD	SP,STACK
		LD	A,0C9H
		LD	(HAUPTP),A
		CALL	HAUPTP
ANF:
		DEC	SP
		DEC	SP
		POP	DE
		LD	HL,HAUPTP-ANF
		ADD	HL,DE
		LD	DE,HAUPTP
		LD	BC,ENDE-BEGINN
		LDIR
		JP	START
;HAUPTPROGRAMM
HAUPTP:
		JP	START
		JP	RSTART
CI:		JP	0F003H
ECHO:		JP	0F009H
CSTS:		JP	0F012H
START:
		LD	SP,STACK
		LD	A,0FFH
		JP	INIT
TSTC:
		EX	(SP),HL
		CALL	IGNB
		CP	(HL)
		JP	TC1
CRLF:
		LD	A,0DH
OUTC:
		PUSH	BC
		PUSH	AF
		LD	A,(OCSW)
		OR	A
		JP	OC2
EXPR:
		CALL	EXPR2
		PUSH	HL
		JP	EXPR1
COMP:
		LD	A,H
		CP	D
		RET	NZ
		LD	A,L
		CP	E
		RET
IGNB:
		LD	A,(DE)
		CP	" "
		RET	NZ
		INC	DE
		JP	IGNB
FINI:
		POP	AF
		CALL	FIN
		JP	QWHAT
TSTV:
		CALL	IGNB
		SUB	040H
		RET	C
		JP	NZ,TV1
		INC	DE
		CALL	PARN
		ADD	HL,HL
		JP	C,QHOW
		PUSH	DE
		EX	DE,HL
		CALL	SIZE
		CALL	COMP
		JP	C,ASORRY
		LD	HL,(TXTEND)
		CALL	SUBDE
		POP	DE
		RET
TV1:
		CP	01BH
		CCF
		RET	C
		INC	DE
		LD	HL,VARBGN
		RLCA
		ADD	A,L
		LD	L,A
		LD	A,0
		ADC	A,H
		LD	H,A
		RET
TC1:
		INC	HL
		JP	Z,TC2
		PUSH	BC
		LD	C,(HL)
		LD	B,0
		ADD	HL,BC
		POP	BC
		DEC	DE
TC2:
		INC	DE
		INC	HL
		EX	(SP),HL
		RET
TSTNUM:
		LD	HL,0
		LD	B,H
		CALL	IGNB
TN1:
		CP	"0"
		RET	C
		CP	03AH
		RET	NC
		LD	A,0F0H
		AND	H
		JP	NZ,QHOW
		INC	B
		PUSH	BC
		LD	B,H
		LD	C,L
		ADD	HL,HL
		ADD	HL,HL
		ADD	HL,BC
		ADD	HL,HL
		LD	A,(DE)
		INC	DE
		AND	0FH
		ADD	A,L
		LD	L,A
		LD	A,0
		ADC	A,H
		LD	H,A
		POP	BC
		LD	A,(DE)
		JP	P,TN1
QHOW:
		PUSH	DE
AHOW:
		LD	DE,HOW
		JP	ERROR
HOW:
.ASCII:		/HOW?:
/:
OK:
.ASCII:		/READY:
/:
WHAT:
.ASCII:		/WHAT?:
/:
SORRY:
.ASCII:		/SORRY:
/:
;******************************************************
;    HAUPTPROGRAMM
; LEGT PROGRAMM IM SPEICHER AB
; DEFINIERT REGISTER
; LIEST EINE BENUTZER ZEILE UM DIESE ZU VERARBEITEN.
;******************************************************
;
RSTART:
		LD	SP,STACK
ST1:
		CALL	CRLF
		LD	DE,OK
		SUB	A
		CALL	PRTSTG
		LD	HL,ST2+1
		LD	(CURRNT),HL
ST2:
		LD	HL,0
		LD	(LOPVAR),HL
		LD	(STKGOS),HL
ST3:
		LD	A,">"
		CALL	GETLN
		PUSH	DE
		CALL	DBUFF
		CALL	TSTNUM
		CALL	IGNB
		LD	A,H
		OR	L
		POP	BC
		JP	Z,DIRECT
		DEC	DE
		LD	A,H
		LD	(DE),A
		DEC	DE
		LD	A,L
		LD	(DE),A
		PUSH	BC
		PUSH	DE
		LD	A,C
		SUB	E
		PUSH	AF
		CALL	FNDLN
		PUSH	DE
		JP	NZ,ST4
		PUSH	DE
		CALL	FNDNXT
		POP	BC
		LD	HL,(TXTUNF)
		CALL	MVUP
		LD	H,B
		LD	L,C
		LD	(TXTUNF),HL
ST4:
		POP	BC
		LD	HL,(TXTUNF)
		POP	AF
		PUSH	HL
		CP	3
		JP	Z,RSTART
		ADD	A,L
		LD	L,A
		LD	A,0
		ADC	A,H
		LD	H,A
		CALL	DTXTE
		CALL	COMP
		JP	NC,QSORRY
		LD	(TXTUNF),HL
		POP	DE
		CALL	MVDOWN
		POP	DE
		POP	HL
		CALL	MVUP
		JP	ST3
;
;*************************************************
;  NEW  STOP  RUN   GOTO     UPR
;*************************************************
;
NEW:
		CALL	ENDCHK
		LD	HL,TXTBGN
		LD	(TXTUNF),HL
STOP:
		CALL	ENDCHK
		JP	RSTART
RUN:
		CALL	ENDCHK
		LD	DE,TXTBGN
RUNNXL:
		LD	HL,0
		CALL	FNDLP
		JP	C,RSTART
RUNTSL:
		EX	DE,HL
		LD	(CURRNT),HL
		EX	DE,HL
		INC	DE
		INC	DE
RUNSML:
		CALL	CONT
		LD	HL,TAB2-1
		JP	EXEC
GOTO:
		CALL	EXPR
		PUSH	DE
		CALL	ENDCHK
		CALL	FNDLN
		JP	NZ,AHOW
		POP	AF
		JP	RUNTSL
;
;**********************************************
;     LIST  PRINT        UPR
;**********************************************
;
LIST:
		CALL	TSTNUM
		CALL	ENDCHK
		CALL	FNDLN
LS1:
		JP	C,RSTART
		CALL	PRTLN
		CALL	CONT
		CALL	FNDLP
		JP	LS1
PRINT:
		LD	C,6
		CALL	TSTC
.BYTE:		03BH:
.BYTE:		PR2-.-1:
		CALL	CRLF
		JP	RUNSML
PR2:
		CALL	TSTC
.BYTE:		0DH:
.BYTE:		PR0-.-1:
		CALL	CRLF
		JP	RUNNXL
PR0:
		CALL	TSTC
.BYTE:		023H:
.BYTE:		PR1-.-1:
		CALL	EXPR
		LD	C,L
		JP	PR3
PR1:
		CALL	QTSTG
		JP	PR8
PR3:
		CALL	TSTC
.BYTE:		02CH:
.BYTE:		PR6-.-1:
		CALL	FIN
		JP	PR0
PR6:
		CALL	CRLF
		CALL	FINI
PR8:
		CALL	EXPR
		PUSH	BC
		CALL	PRTNUM
		POP	BC
		JP	PR3
;**********************************************************
;       GOSUB      RETURN             UPR
;**********************************************************
;
GOSUB:
		CALL	PUSHA
		CALL	EXPR
		PUSH	DE
		CALL	FNDLN
		JP	NZ,AHOW
		LD	HL,(CURRNT)
		PUSH	HL
		LD	HL,(STKGOS)
		PUSH	HL
		LD	HL,0
		LD	(LOPVAR),HL
		ADD	HL,SP
		LD	(STKGOS),HL
		JP	RUNTSL
RETURN:
		CALL	ENDCHK
		LD	HL,(STKGOS)
		LD	A,H
		OR	L
		JP	Z,QWHAT
		LD	SP,HL
		POP	HL
		LD	(STKGOS),HL
		POP	HL
		LD	(CURRNT),HL
		POP	DE
		CALL	POPA
		CALL	FINI
;*************************************************
;     FOR    NEXT            UPR
;*************************************************
;
FOR:
		CALL	PUSHA
		CALL	SETVAL
		DEC	HL
		LD	(LOPVAR),HL
		LD	HL,TAB5-1
		JP	EXEC
FR1:
		CALL	EXPR
		LD	(LOPLMT),HL
		LD	HL,TAB6-1
		JP	EXEC
FR2:
		CALL	EXPR
		JP	FR4
FR3:
		LD	HL,1
FR4:
		LD	(LOPINC),HL
FR5:
		LD	HL,(CURRNT)
		LD	(LOPLN),HL
		EX	DE,HL
		LD	(LOPPT),HL
		LD	BC,0AH
		LD	HL,(LOPVAR)
		EX	DE,HL
		LD	H,B
		LD	L,B
		ADD	HL,SP
.BYTE:		03EH:
FR7:
		ADD	HL,BC
		LD	A,(HL)
		INC	HL
		OR	(HL)
		JP	Z,FR8
		LD	A,(HL)
		DEC	HL
		CP	D
		JP	NZ,FR7
		LD	A,(HL)
		CP	E
		JP	NZ,FR7
		EX	DE,HL
		LD	HL,0
		ADD	HL,SP
		LD	B,H
		LD	C,L
		LD	HL,0AH
		ADD	HL,DE
		CALL	MVDOWN
		LD	SP,HL
FR8:
		LD	HL,(LOPPT)
		EX	DE,HL
		CALL	FINI
NEXT:
		CALL	TSTV
		JP	C,QWHAT
		LD	(VARNXT),HL
NX0:
		PUSH	DE
		EX	DE,HL
		LD	HL,(LOPVAR)
		LD	A,H
		OR	L
		JP	Z,AWHAT
		CALL	COMP
		JP	Z,NX3
		POP	DE
		CALL	POPA
		LD	HL,(VARNXT)
		JP	NX0
NX3:
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		LD	HL,(LOPINC)
		PUSH	HL
		LD	A,H
		XOR	D
		LD	A,D
		ADD	HL,DE
		JP	M,NX4
		XOR	H
		JP	M,NX5
NX4:
		EX	DE,HL
		LD	HL,(LOPVAR)
		LD	(HL),E
		INC	HL
		LD	(HL),D
		LD	HL,(LOPLMT)
		POP	AF
		OR	A
		JP	P,NX1
		EX	DE,HL
NX1:
		CALL	CKHLDE
		POP	DE
		JP	C,NX2
		LD	HL,(LOPLN)
		LD	(CURRNT),HL
		LD	HL,(LOPPT)
		EX	DE,HL
		CALL	FINI
NX5:
		POP	HL
		POP	DE
NX2:
		CALL	POPA
		CALL	FINI
;*****************************************************
;  REM      IF    INPUT    LET           UPR
;****************************************************
REM:
		LD	HL,0
		JP	IFFR
IFF:
		CALL	EXPR
IFFR:
		LD	A,H
		OR	L
		JP	NZ,RUNSML
		CALL	FNDSKP
		JP	NC,RUNTSL
		JP	RSTART
INPERR:
		LD	HL,(STKINP)
		LD	SP,HL
		POP	HL
		LD	(CURRNT),HL
		POP	DE
		POP	DE
INPUT:
IP1:
		PUSH	DE
		CALL	QTSTG
		JP	IP2
		CALL	TSTV
		JP	C,IP4
		JP	IP3
IP2:
		PUSH	DE
		CALL	TSTV
		JP	C,QWHAT
		LD	A,(DE)
		LD	C,A
		SUB	A
		LD	(DE),A
		POP	DE
		CALL	PRTSTG
		LD	A,C
		DEC	DE
		LD	(DE),A
IP3:
		PUSH	DE
		EX	DE,HL
		LD	HL,(CURRNT)
		PUSH	HL
		LD	HL,IP1
		LD	(CURRNT),HL
		LD	HL,0
		ADD	HL,SP
		LD	(STKINP),HL
		PUSH	DE
		LD	A,03AH
		CALL	GETLN
		CALL	DBUFF
		CALL	EXPR
		CALL	CONT
		POP	DE
		EX	DE,HL
		LD	(HL),E
		INC	HL
		LD	(HL),D
		POP	HL
		LD	(CURRNT),HL
		POP	DE
IP4:
		POP	AF
		CALL	TSTC
.BYTE:		",":
.BYTE:		IP5-.-1:
		JP	IP1
IP5:
		CALL	FINI
DEFLT:
		LD	A,(DE)
		CP	0DH
		JP	Z,LT1
LET:
		CALL	SETVAL
		CALL	TSTC
.BYTE:		",":
.BYTE:		LT1-.-1:
		JP	LET
LT1:
		CALL	FINI
;*****************************************************
;    EXPR                UPR
;*****************************************************
EXPR1:
		LD	HL,TAB8-1
		JP	EXEC
XP11:
		CALL	XP18
		RET	C
		LD	L,A
		RET
XP12:
		CALL	XP18
		RET	Z
		LD	L,A
		RET
XP13:
		CALL	XP18
		RET	Z
		RET	C
		LD	L,A
		RET
XP14:
		CALL	XP18
		LD	L,A
		RET	Z
		RET	C
		LD	L,H
		RET
XP15:
		CALL	XP18
		RET	NZ
		LD	L,A
		RET
XP16:
		CALL	XP18
		RET	NC
		LD	L,A
		RET
XP17:
		POP	HL
		RET
XP18:
		LD	A,C
		POP	HL
		POP	BC
		PUSH	HL
		PUSH	BC
		LD	C,A
		CALL	EXPR2
		EX	DE,HL
		EX	(SP),HL
		CALL	CKHLDE
		POP	DE
		LD	HL,0
		LD	A,1
		RET
EXPR2:
		CALL	TSTC
.BYTE:		02DH:
.BYTE:		XP21-.-1:
		LD	HL,0
		JP	XP26
XP21:
		CALL	TSTC
.BYTE:		02BH:
.BYTE:		XP22-.-1:
XP22:
		CALL	EXPR3
XP23:
		CALL	TSTC
.BYTE:		02BH:
.BYTE:		XP25-.-1:
		PUSH	HL
		CALL	EXPR3
XP24:
		EX	DE,HL
		EX	(SP),HL
		LD	A,H
		XOR	D
		LD	A,D
		ADD	HL,DE
		POP	DE
		JP	M,XP23
		XOR	H
		JP	P,XP23
		JP	QHOW
XP25:
		CALL	TSTC
.BYTE:		02DH:
.BYTE:		XP42-.-1:
XP26:
		PUSH	HL
		CALL	EXPR3
		CALL	CHGSGN
		JP	XP24
EXPR3:
		CALL	EXPR4
XP31:
		CALL	TSTC
.BYTE:		02AH:
.BYTE:		XP34-.-1:
		PUSH	HL
		CALL	EXPR4
		LD	B,0
		CALL	CHKSGN
		EX	(SP),HL
		CALL	CHKSGN
		EX	DE,HL
		EX	(SP),HL
		LD	A,H
		OR	A
		JP	Z,XP32
		LD	A,D
		OR	D
		EX	DE,HL
		JP	NZ,AHOW
XP32:
		LD	A,L
		LD	HL,0
		OR	A
		JP	Z,XP35
XP33:
		ADD	HL,DE
		JP	C,AHOW
		DEC	A
		JP	NZ,XP33
		JP	XP35
XP34:
		CALL	TSTC
.BYTE:		02FH:
.BYTE:		XP42-.-1:
		PUSH	HL
		CALL	EXPR4
		LD	B,0
		CALL	CHKSGN
		EX	(SP),HL
		CALL	CHKSGN
		EX	DE,HL
		EX	(SP),HL
		EX	DE,HL
		LD	A,D
		OR	E
		JP	Z,AHOW
		PUSH	BC
		CALL	DIVIDE
		LD	H,B
		LD	L,C
		POP	BC
XP35:
		POP	DE
		LD	A,H
		OR	A
		JP	M,QHOW
		LD	A,B
		OR	A
		CALL	M,CHGSGN
		JP	XP31
EXPR4:
		LD	HL,TAB4-1
		JP	EXEC
XP40:
		CALL	TSTV
		JP	C,XP41
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A
		RET
XP41:
		CALL	TSTNUM
		LD	A,B
		OR	A
		RET	NZ
PARN:
		CALL	TSTC
.BYTE:		028H:
.BYTE:		XP43-.-1:
		CALL	EXPR
		CALL	TSTC
.BYTE:		029H:
.BYTE:		XP43-.-1:
XP42:
		RET
XP43:
		JP	QWHAT
RND:
		CALL	PARN
		LD	A,H
		OR	A
		JP	M,QHOW
		OR	L
		JP	Z,QHOW
		PUSH	DE
		PUSH	HL
		LD	HL,(RANPNT)
		LD	DE,LSTROM
		CALL	COMP
		JP	C,RA1
		LD	HL,START
RA1:
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		LD	(RANPNT),HL
		POP	HL
		EX	DE,HL
		PUSH	BC
		CALL	DIVIDE
		POP	BC
		POP	DE
		INC	HL
		RET
ABS:
		CALL	PARN
		DEC	DE
		CALL	CHKSGN
		INC	DE
		RET
SIZE:
		LD	HL,(TXTUNF)
		PUSH	DE
		EX	DE,HL
		LD	HL,(TXTEND)
		CALL	SUBDE
		POP	DE
		RET
;************************************************
; DIVIDE  SUBDE  CHKSGN  CHGSGN  CKHLDE    UPR
;************************************************
;
DIVIDE:
		PUSH	HL
		LD	L,H
		LD	H,0
		CALL	DV1
		LD	B,C
		LD	A,L
		POP	HL
		LD	H,A
DV1:
		LD	C,0FFH
DV2:
		INC	C
		CALL	SUBDE
		JP	NC,DV2
		ADD	HL,DE
		RET
SUBDE:
		LD	A,L
		SUB	E
		LD	L,A
		LD	A,H
		SBC	A,D
		LD	H,A
		RET
CHKSGN:
		LD	A,H
		OR	A
		RET	P
CHGSGN:
		LD	A,H
		OR	L
		RET	Z
		LD	A,H
		PUSH	AF
		CPL
		LD	H,A
		LD	A,L
		CPL
		LD	L,A
		INC	HL
		POP	AF
		XOR	H
		JP	P,QHOW
		LD	A,B
		XOR	080H
		LD	B,A
		RET
CKHLDE:
		LD	A,H
		XOR	D
		JP	P,CK1
		EX	DE,HL
CK1:
		CALL	COMP
		RET
;********************************************
;  SETVAL  FIN  ENDCHK   ERROR      UPR
;********************************************
;
SETVAL:
		CALL	TSTV
		JP	C,QWHAT
		PUSH	HL
		CALL	TSTC
.BYTE:		03DH:
.BYTE:		SV1-.-1:
		CALL	EXPR
		LD	B,H
		LD	C,L
		POP	HL
		LD	(HL),C
		INC	HL
		LD	(HL),B
		RET
SV1:
		JP	QWHAT
FIN:
		CALL	TSTC
.BYTE:		03BH:
.BYTE:		FI1-.-1:
		POP	AF
		JP	RUNSML
FI1:
		CALL	TSTC
.BYTE:		0DH:
.BYTE:		FI2-.-1:
		POP	AF
		JP	RUNNXL
FI2:
		RET
ENDCHK:
		CALL	IGNB
		CP	0DH
		RET	Z
QWHAT:
		PUSH	DE
AWHAT:
		LD	DE,WHAT
ERROR:
		SUB	A
		CALL	PRTSTG
		POP	DE
		LD	A,(DE)
		PUSH	AF
		SUB	A
		LD	(DE),A
		LD	HL,(CURRNT)
		PUSH	HL
		LD	A,(HL)
		INC	HL
		OR	(HL)
		POP	DE
		JP	Z,RSTART
		LD	A,(HL)
		OR	A
		JP	M,INPERR
		CALL	PRTLN
		DEC	DE
		POP	AF
		LD	(DE),A
		LD	A,03FH
		CALL	OUTC
		SUB	A
		CALL	PRTSTG
		JP	RSTART
QSORRY:
		PUSH	DE
ASORRY:
		LD	DE,SORRY
		JP	ERROR
;*****************************************
;  GETLN  FNDLN             UPR
;*****************************************
;
GETLN:
		CALL	OUTC
		CALL	DBUFF
GL1:
		CALL	CHKIO
		CP	1
		JP	Z,GL3
		CALL	OUTC
		CP	0AH
		JP	Z,GL1
		OR	A
		JP	Z,GL1
		CP	01BH
		JP	Z,GL4
		LD	(DE),A
		INC	DE
		CP	0DH
		RET	Z
		LD	A,E
		CALL	CXBUFE
		JP	NZ,GL1
GL3:
		LD	A,E
		CALL	CXBUFA
		JP	Z,GL4
		DEC	DE
		LD	A,8
		CALL	OUTC
		JP	GL1
GL4:
		CALL	CRLF
		LD	A,0BH
		JP	GETLN
FNDLN:
		LD	A,H
		OR	A
		JP	M,QHOW
		LD	DE,TXTBGN
FNDLP:
FL1:
		PUSH	HL
		LD	HL,(TXTUNF)
		DEC	HL
		CALL	COMP
		POP	HL
		RET	C
		LD	A,(DE)
		SUB	L
		LD	B,A
		INC	DE
		LD	A,(DE)
		SBC	A,H
		JP	C,FL2
		DEC	DE
		OR	B
		RET
FNDNXT:
		INC	DE
FL2:
		INC	DE
FNDSKP:
		LD	A,(DE)
		CP	0DH
		JP	NZ,FL2
		INC	DE
		JP	FL1
;*******************************************
;  PRTSTG  QTSTG  PRTNUM  PRTLN   UPR
;*******************************************
;
PRTSTG:
		LD	B,A
PS1:
		LD	A,(DE)
		INC	DE
		CP	B
		RET	Z
		CALL	OUTC
		CP	0DH
		JP	NZ,PS1
		RET
QTSTG:
		CALL	TSTC
.BYTE:		022H:
.BYTE:		QT3-.-1:
		LD	A,022H
QT1:
		CALL	PRTSTG
		CP	0DH
		POP	HL
		JP	Z,RUNNXL
QT2:
		INC	HL
		INC	HL
		INC	HL
		JP	(HL)
QT3:
		CALL	TSTC
.BYTE:		027H:
.BYTE:		QT4-.-1:
		LD	A,027H
		JP	QT1
QT4:
		CALL	TSTC
.BYTE:		05FH:
.BYTE:		QT5-.-1:
		LD	A,08DH
		CALL	OUTC
		CALL	OUTC
		POP	HL
		JP	QT2
QT5:
		RET
PRTNUM:
		LD	B,0
		CALL	CHKSGN
		JP	P,PN1
		LD	B,02DH
		DEC	C
PN1:
		PUSH	DE
		LD	DE,0AH
		PUSH	DE
		DEC	C
		PUSH	BC
PN2:
		CALL	DIVIDE
		LD	A,B
		OR	C
		JP	Z,PN3
		EX	(SP),HL
		DEC	L
		PUSH	HL
		LD	H,B
		LD	L,C
		JP	PN2
PN3:
		POP	BC
PN4:
		DEC	C
		LD	A,C
		OR	A
		JP	M,PN5
		LD	A,020H
		CALL	OUTC
		JP	PN4
PN5:
		LD	A,B
		OR	A
		CALL	NZ,OUTC
		LD	E,L
PN6:
		LD	A,E
		CP	0AH
		POP	DE
		RET	Z
		ADD	A,030H
		CALL	OUTC
		JP	PN6
PRTLN:
		LD	A,(DE)
		LD	L,A
		INC	DE
		LD	A,(DE)
		LD	H,A
		INC	DE
		LD	C,4
		CALL	PRTNUM
		LD	A,020H
		CALL	OUTC
		SUB	A
		CALL	PRTSTG
		RET
;******************************************
;  MVUP  MVDOWN  POPA  PUSHA     UPR
;******************************************
;
MVUP:
		CALL	COMP
		RET	Z
		LD	A,(DE)
		LD	(BC),A
		INC	DE
		INC	BC
		JP	MVUP
MVDOWN:
		LD	A,B
		SUB	D
		JP	NZ,MD1
		LD	A,C
		SUB	E
		RET	Z
MD1:
		DEC	DE
		DEC	HL
		LD	A,(DE)
		LD	(HL),A
		JP	MVDOWN
POPA:
		POP	BC
		POP	HL
		LD	(LOPVAR),HL
		LD	A,H
		OR	L
		JP	Z,PP1
		POP	HL
		LD	(LOPINC),HL
		POP	HL
		LD	(LOPLMT),HL
		POP	HL
		LD	(LOPLN),HL
		POP	HL
		LD	(LOPPT),HL
PP1:
		PUSH	BC
		RET
PUSHA:
		LD	HL,STKLMT
		CALL	CHGSGN
		POP	BC
		ADD	HL,SP
		JP	NC,QSORRY
		LD	HL,(LOPVAR)
		LD	A,H
		OR	L
		JP	Z,PU1
		LD	HL,(LOPPT)
		PUSH	HL
		LD	HL,(LOPLN)
		PUSH	HL
		LD	HL,(LOPLMT)
		PUSH	HL
		LD	HL,(LOPINC)
		PUSH	HL
		LD	HL,(LOPVAR)
PU1:
		PUSH	HL
		PUSH	BC
		RET
;****************************
;*  OUTC  CHKIO  UPR        *
;****************************
;
INIT:
		LD	(OCSW),A
		LD	D,3
PATLOP:
		CALL	CRLF
		DEC	D
		JP	NZ,PATLOP
		SUB	A
		LD	DE,MSG1
		CALL	PRTSTG
		LD	HL,START
		LD	(RANPNT),HL
		LD	HL,TXTBGN
		LD	(TXTUNF),HL
		LD	HL,TXTE
		LD	(TXTEND),HL
		LD	HL,BUFA
		LD	(BUFFER),HL
		LD	HL,BUFE
		LD	(BUFEND),HL
		JP	RSTART
OC2:
		JP	NZ,OC3
		POP	AF
		POP	BC
		RET
OC3:
		POP	AF
		PUSH	AF
		LD	C,A
LPT:
		LD	A,C
		CP	0DH
		JP	Z,LINEF
H1:
		CALL	ECHO
		POP	AF
		POP	BC
		RET
LINEF:
		LD	C,0DH
		CALL	ECHO
		LD	C,0AH
		JP	H1
CHKIO:
		CALL	CI
		AND	7FH
		CP	2
		JP	NZ,CI1
		LD	A,(OCSW)
		CPL
		LD	(OCSW),A
		JP	CHKIO
CI1:
		CP	3
		RET	NZ
		JP	RSTART
MSG1:
.ASCII:		"RDK:		PROMPT:		":
.ASCII:		"BASIC:		V3.2:		3K:
":
;********************************
;* TABLES DIRECT EXEC           *
;********************************
.OPSYN:		.WORD,DWA:;
.OPSYN:		.ASCIZ,TX:
;
;
TAB1:
TX:		"LIST":
DWA:		LIST:
TX:		"RUN":
DWA:		RUN:
TX:		"NEW":
DWA:		NEW:
TX:		"BYE":
DWA:		BYE:
TX:		"END":
DWA:		END
TAB2:
TX:		"NEXT":
DWA:		NEXT:
TX:		"LET":
DWA:		LET:
TX:		"IF":
DWA:		IFF:
TX:		"GOTO":
DWA:		GOTO:
TX:		"GOSUB":
DWA:		GOSUB:
TX:		"RETURN":
DWA:		RETURN:
TX:		"REM":
DWA:		REM:
TX:		"FOR":
DWA:		FOR:
TX:		"INPUT":
DWA:		INPUT:
TX:		"PRINT":
DWA:		PRINT:
TX:		"STOP":
DWA:		STOP:
TX:		"CALL":
DWA:		CALL
TX:		"OUTCHAR":
DWA:		OUTCHAR:
TX:		"OUT":
DWA:		OUT(),A
TX:		"O$":
DWA:		O:
TX:		"I$":
DWA:		I:
TX:		"POKE":
DWA:		POKE:
TX:		"TAB":
DWA:		TAB:
TX:		"BYTE":
DWA:		BYTE:
TX:		"WORD":
DWA:		WORD:
.BYTE:		0:
DWA:		DEFLT:
TAB4:
TX:		"RND":
DWA:		RND:
TX:		"ABS":
DWA:		ABS:
TX:		"SIZE":
DWA:		SIZE:
TX:		"PEEK":
DWA:		PEEK:
TX:		"INCHAR":
DWA:		INCHAR:
TX:		"HEX":
DWA:		HEX:
TX:		"IN":
DWA:		INA,()
TX:		"'":
DWA:		QUOTE:
TX:		"TOP":
DWA:		TOP:
TX:		"LEN":
DWA:		LENGTH:
TX:		"CSTS":
DWA:		CSTAT:
.BYTE:		0:
DWA:		XP40:
TAB5:
TX:		"TO":
DWA:		FR1:
.BYTE:		0:
DWA:		QWHAT:
TAB6:
TX:		"STEP":
DWA:		FR2:
.BYTE:		0:
DWA:		FR3:
TAB8:
TX:		">=":
DWA:		XP11:
TX:		"#":
DWA:		XP12:
TX:		">":
DWA:		XP13:
TX:		"=":
DWA:		XP15:
TX:		"<=":
DWA:		XP14:
TX:		"<":
DWA:		XP16:
.BYTE:		0:
DWA:		XP17:
;
;
;****************
;* DIRECT MODUL *
;****************
;
;
;
DIRECT:
		LD	HL,TAB1-1
EXEC:
EX0:
		CALL	IGNB
		PUSH	DE
EX1:
		LD	A,(DE)
		INC	DE
		CP	"."
		JP	Z,EX3
		INC	HL
		CP	(HL)
		JP	Z,EX1
		LD	A,0
		DEC	DE
		CP	(HL)
		JP	Z,EX5
EX2:
		INC	HL
		CP	(HL)
		JP	NZ,EX2
		INC	HL
		INC	HL
		POP	DE
		JP	EX0
EX3:
		LD	A,0
EX4:
		INC	HL
		CP	(HL)
		JP	NZ,EX4
EX5:
		INC	HL
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A
		POP	AF
		JP	(HL)
;***************
;* END EXEC    *
;***************
;
;
;
DBUFF:
		PUSH	HL
		LD	HL,(BUFFER)
		LD	D,H
		LD	E,L
		POP	HL
		RET
;
DTXTE:
		PUSH	HL
		LD	HL,(TXTEND)
		LD	D,H
		LD	E,L
		POP	HL
		RET
;
CXBUFE:
		PUSH	HL
		LD	HL,(BUFEND)
		CP	L
		POP	HL
		RET
;
CXBUFA:
		PUSH	HL
		LD	HL,(BUFFER)
		CP	L
		POP	HL
		RET
;
END:
		CALL	EXPR
		EX	DE,HL
		LD	HL,TXTE
		EX	DE,HL
		CALL	COMP
		JP	C,ASORRY
		LD	A,H
		OR	A
		JP	M,ASORRY
		LD	A,(HL)
		CPL
		LD	(HL),A
		LD	B,(HL)
		CP	B
		JP	NZ,ASORRY
		LD	(BUFEND),HL
		LD	A,L
		SUB	132
		LD	L,A
		LD	A,H
		SBC	A,0
		LD	H,A
		LD	(BUFFER),HL
		DEC	HL
		DEC	HL
		LD	(TXTEND),HL
		JP	RSTART
;
BYE:
		RST	38H
		JP	RSTART
;
;
CALL:
		CALL	EXPR
		PUSH	DE
		LD	BC,HERE
		PUSH	BC
		JP	(HL)
HERE:
		POP	DE
		CALL	FINI
;
OUT:
		CALL	PARN
		PUSH	HL
		CALL	TSTC
.BYTE:		"=":
.BYTE:		RSV0-.-1:
		CALL	EXPR
		LD	B,L
		LD	A,0D3H
		LD	(IOBUFA),A
		POP	HL
		LD	A,L
		LD	(IOBUFB),A
		LD	A,0C9H
		LD	(IOBUFC),A
		LD	A,B
		CALL	IOBUFA
		CALL	FINI
RSV0:
		JP	QWHAT
;
TAB:
		CALL	PARN
A1:
		LD	A,H
		OR	L
		CALL	Z,FINI
		DEC	HL
		LD	A,20H
		CALL	OUTC
		JP	A1
;
IN:
		CALL	PARN
		PUSH	HL
		LD	A,0DBH
		LD	(IOBUFA),A
		POP	HL
		LD	A,L
		LD	(IOBUFB),A
		LD	A,0C9H
		LD	(IOBUFC),A
		CALL	IOBUFA
		LD	H,0
		LD	L,A
		RET
;
;
O:
		CALL	EXPR
		PUSH	DE
		EX	DE,HL
		XOR	A
		CALL	PRTSTG
		POP	DE
		CALL	FINI
;
;
I:
		CALL	EXPR
		PUSH	DE
		EX	DE,HL
		LD	HL,(TXTUNF)
		EX	DE,HL
		CALL	COMP
		JP	C,ASORRY
		CALL	DBUFF
		CALL	GL1
		LD	B,H
		LD	C,L
		EX	DE,HL
		DEC	HL
		CALL	DBUFF
		PUSH	DE
		CALL	MVUP
		XOR	A
		LD	(BC),A
		POP	DE
		INC	HL
		CALL	SUBDE
		EX	DE,HL
		LD	HL,LEGT
		LD	(HL),E
		INC	HL
		LD	(HL),D
		POP	DE
		CALL	FINI
;
;
PEEK:
		CALL	PARN
		LD	L,(HL)
		LD	H,0
		RET
;
;
POKE:
		CALL	EXPR
		PUSH	DE
		EX	DE,HL
		LD	HL,(TXTUNF)
		EX	DE,HL
		CALL	COMP
		JP	C,ASORRY
		POP	DE
		PUSH	HL
		CALL	TSTC
.BYTE:		",":
.BYTE:		PK1-.-1:
		CALL	EXPR
		LD	A,L
		POP	HL
		LD	(HL),A
		CALL	FINI
PK1:
		JP	QWHAT
;
BYTE:
		CALL	PARN
		LD	A,L
		CALL	WRIT2
		CALL	FINI
;
WORD:
		CALL	PARN
		LD	A,H
		CALL	WRIT2
		LD	A,L
		CALL	WRIT2
		CALL	FINI
WRIT2:
		PUSH	AF
		RRCA
		RRCA
		RRCA
		RRCA
		CALL	IST
		POP	AF
IST:
		AND	0FH
		ADD	A,90H
		DAA
		ADC	A,40H
		DAA
		JP	OUTC
;
CSTAT:
		CALL	CSTS
		CPL
		LD	L,A
		LD	H,0
		RET
;
QUOTE:
		LD	A,(DE)
		INC	DE
		LD	L,A
		LD	H,0
		CALL	TSTC
.BYTE:		"'":
.BYTE:		ASCI-.-1:
		RET
ASCI:
		JP	QWHAT
;
TOP:
		LD	HL,(TXTUNF)
		INC	HL
		RET
;
LENGTH:
		LD	HL,(LEGT)
		DEC	HL
		RET
;
OUTCHAR:
		CALL	EXPR
		LD	A,L
		CALL	OUTC
		CALL	FINI
;
INCHAR:
		CALL	CHKIO
		LD	H,0
		LD	L,A
		RET
;
HEX:
		PUSH	BC
		LD	HL,0
		CALL	TSTC
.BYTE:		"(":
.BYTE:		HN2-.-1:
HNXTH:
		LD	A,(DE)
		CP	0DH
		JP	Z,QWHAT
		CALL	CNVBN
		ADD	HL,HL
		ADD	HL,HL
		ADD	HL,HL
		ADD	HL,HL
		LD	B,0
		LD	C,A
		ADD	HL,BC
		INC	DE
		CALL	TSTC
.BYTE:		")":
.BYTE:		HN1-.-1:
		JP	POPRET
HN1:
		JP	HNXTH
HN2:
		JP	QWHAT
POPRET:
		POP	BC
		RET
;
;
CNVBN:
		CP	30H
		JP	M,QWHAT
		CP	39H
		JP	M,CONTC
		JP	Z,CONTC
		CP	41H
		JP	M,QWHAT
		CP	47H
		JP	P,QWHAT
CONTC:
		SUB	30H
		CP	10
		RET	M
		SUB	7
		RET
;
;
;
CONT:
		CALL	CSTS
		RET	Z
		CALL	CI
		CP	3H
		RET	NZ
		JP	RSTART
;
;
ENDE:		NOP
;
RAM=\"RAM:	LOCATION:	":
.LOC:		RAM:
;
.OPSYN:		.BLKB,DS:
;
LEGT:		DS	2
IOBUFA:		DS	1
IOBUFB:		DS	1
IOBUFC:		DS	1
LSTROM:		DS	1
OCSW:		DS	1
CURRNT:		DS	2
STKGOS:		DS	2
VARNXT:		DS	2
STKINP:		DS	2
LOPVAR:		DS	2
LOPINC:		DS	2
LOPLMT:		DS	2
LOPLN:		DS	2
LOPPT:		DS	2
RANPNT:		DS	2
TXTUNF:		DS	2
		DS	10
STKLMT:		DS	2
		DS	200
STACK:		DS	2
VARBGN:		DS	55
TXTEND:		DS	2
BUFFER:		DS	2
BUFEND:		DS	2
TXTBGN:		DS	2
		DS	600
TXTE:		DS	2
BUFA:		DS	64
BUFE:		DS	1
.END:

