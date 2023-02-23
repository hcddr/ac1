		cpu	Z80
;*****************************************************************
;*****************************************************************
;**                                                             **
;**   B a s i c    D i s k   O p e r a t i n g   S y s t e m    **
;**            I n t e r f a c e   M o d u l e                   **
;**                                                             **
;*****************************************************************
;*****************************************************************
;
;	CP/A	Akademie der Wissenschaften der DDR
;		
;	rebuild from original Digital Research Code Comments
;	(Bdos Interface, Bdos, Version 2.2 Feb, 1980)
; 	for AS-Assembler and Z80 mnemonics 
;	V.Pohlers 14.02.2011
; 	Version vom AC1-CPM64_FA, vermutlich etwas älter

ON		EQU	0FFFFH
OFF		EQU	00000H
TEST		EQU	ON
;
;;		IF	TEST
;;		ORG	0CC00H+800H
;;		ELSE
;;		ORG	0800H
;;		ENDIF
;	bios value defined at end of module
;
SSIZE		EQU	32		;32 level stack
;
;	low memory locations
REBOOT		EQU	0000H		;reboot system
;;IOLOC		EQU	0003H		;i/o byte location
;;DISKA		EQU	0004H		;disk address for current disk
BDOSA		EQU	0006H		;address field of jmp BDOS
;
;
;	equates for non graphic characters
CTLC		EQU	03H		;control c
CTLE		EQU	05H		;physical eol
CTLH		EQU	08H		;backspace
CTLP		EQU	10H		;prnt toggle
CTLR		EQU	12H		;repeat line
CTLS		EQU	13H		;stop/start screen
CTLU		EQU	15H		;line delete
CTLX		EQU	18H		;=ctl-u
CTLZ		EQU	1AH		;end of file
RUBOUT		EQU	7FH		;char delete
TAB		EQU	09H		;tab char
CR		EQU	0DH		;carriage return
LF		EQU	0AH		;line feed
CTL		EQU	5EH		;up arrow
;
loc_0:		DB	2,8,0,8,5,5
;
;	enter here from the user's program with function number in c,
;	and information address in d,e
		JP	BDOSE		;past parameter block
;
;	************************************************
;	*** relative locations 0009 - 000e           ***
;	************************************************
PERERR:		DW	PERSUB		;permanent error subroutine
SELERR:		DW	SELSUB		;select error subroutine
RODERR:		DW	RODSUB		;ro disk error subroutine
ROFERR:		DW	ROFSUB		;ro file error subroutine
;
;
;	dispatch table for functions
FUNCTAB:
		DW	WBOOTF, FUNC1, FUNC2, FUNC3
		DW	PUNCHF, LISTF, FUNC6, FUNC7
		DW	FUNC8, FUNC9, FUNC10,FUNC11
DISKF		EQU	($-FUNCTAB)/2	;disk funcs
		DW	FUNC12,FUNC13,FUNC14,FUNC15
		DW	FUNC16,FUNC17,FUNC18,FUNC19
		DW	FUNC20,FUNC21,FUNC22,FUNC23
		DW	FUNC24,FUNC25,FUNC26,FUNC27
		DW	FUNC28,FUNC29,FUNC30,FUNC31
		DW	FUNC32,FUNC33,FUNC34,FUNC35
		DW	FUNC36,FUNC37,FUNC38,FUNC39;
		DW	FUNC40;
NFUNCS		EQU	($-FUNCTAB)/2
;
BDOSE:		;arrive here from user programs
		EX	DE,HL
		LD	(INFO),HL
		EX	DE,HL		;info=DE, DE=info
		LD	A,E
		LD	(LINFO),A	;linfo = low(info) - don't equ
		LD	HL,0
		LD	(ARET),HL	;return value defaults to 0000
;save user's stack pointer, set to local stack
		ADD	HL,SP
		LD	(ENTSP),HL	;entsp = stackptr
		LD	SP,LSTACK	;local stack setup
		XOR	A
		LD	(FCBDSK),A
		LD	(RESEL),A	;fcbdsk,resel=FALSE_
		push	ix
		push	iy
		LD	HL,GOBACK	;return here after all functions
		PUSH	HL		;jmp goback equivalent to ret
		LD	A,C
		CP	NFUNCS
		RET	NC		;skip if invalid #
		LD	C,E		;possible output character to C
		LD	HL,FUNCTAB
		LD	E,A
		LD	D,0		;DE=func, HL=.ciotab
		ADD	HL,DE
		ADD	HL,DE
		LD	E,(HL)
		INC	HL
		LD	D,(HL)		;DE=functab(func)
		LD	HL,(INFO)	;info in DE for later xchg
		EX	DE,HL
		JP	(HL)		;dispatched
;
;
;	error subroutines
PERSUB:		;report permanent error
		LD	HL,PERMSG
		CALL	ERRFLG		;to report the error
		CP	CTLC
		JP	Z,REBOOT	;reboot if response is ctlc
		RET			;and ignore the error
;
SELSUB:		;report select error
		LD	HL,SELMSG
		jr	WAIT_ERR	;wait console before boot
;
RODSUB:		;report write to read/only disk
		LD	HL,RODMSG
		jr	WAIT_ERR	;wait console
;
ROFSUB:		;report read/only file
		LD	HL,ROFMSG	;drop through to wait for console
;
WAIT_ERR:
;wait for response before boot
		CALL	ERRFLG
		JP	REBOOT
;
;	error messages
DSKMSG:		DB	"Bdos Err On "
DSKERR:		DB	" : $"		;filled in by errflg
permsg:		db 	"Bad Sector (^C=abort, other=ignore)$"
SELMSG:		DB	"Select$"
ROFMSG:		DB	"File "
RODMSG:		DB	"R/O$"
;
;
ERRFLG:
;report error to console, message address in HL
		PUSH	HL
		CALL	CRLF		;stack mssg address, new line
		LD	A,(CURDSK)
		ADD	A,'A'
		LD	(DSKERR),A	;current disk name
		LD	BC,DSKMSG
		CALL	PRINT		;the error message
		POP	BC
		CALL	PRINT		;error mssage tail
		jp	CONINF		; to get the input character
;(drop through to conin)
;ret
;
CONECH:
;read character with echo
		CALL	CONINF
		CALL	ECHOC
		RET	C		;echo character?
;character must be echoed before return
		PUSH	AF
		LD	C,A
		CALL	TABOUT
		POP	AF
		RET			;with character in A
;
ECHOC:
;echo character if graphic
;cr, lf, tab, or backspace
		CP	CR
		RET	Z		;carriage return?
		CP	LF
		RET	Z		;line feed?
		CP	TAB
		RET	Z		;tab?
		CP	CTLH
		RET	Z		;backspace?
		CP	' '
		RET			;carry set if not graphic
;
CONBRK:
;no active kbchar, check external break
		CALL	CONSTF
		AND	1
		RET

CONOUT:
;compute character position/write console char from C
;compcol = TRUE_ if computing column position
		LD	A,(COMPCOL)
		OR	A
		Jr	NZ,COMPOUT
;write the character, then compute the column
;write console character from C
		PUSH	BC
		CALL	CONBRK		;check for screen stop function
		POP	BC
		PUSH	BC		;recall/save character
		CALL	CONOUTF		;externally, to console
		POP	BC
		PUSH	BC		;recall/save character
;may be copying to the list device
		LD	A,(LISTCP)
		OR	A
		CALL	NZ,LISTF	;to printer, if so
		POP	BC		;recall the character
COMPOUT:
		LD	A,C		;recall the character
;and compute column position
		LD	HL,COLUMN	;A = char, HL = .column
		CP	RUBOUT
		RET	Z		;no column change if nulls
		INC	(HL)		;column = column + 1
		CP	' '
		RET	NC		;return if graphic
;not graphic, reset column position
		DEC	(HL)		;column = column - 1
		LD	A,(HL)
		OR	A
		RET	Z		;return if at zero
;not at zero, may be backspace or end line
		LD	A,C		;character back to A
		CP	CTLH
		Jr	NZ,NOTBACKSP
;backspace character
		DEC	(HL)		;column = column - 1
		RET
NOTBACKSP:
;not a backspace character, eol?
		CP	LF
		RET	NZ		;return if not
;end of line, column = 0
		LD	(HL),0		;column = 0
		RET
;
CTLOUT:
;send C character with possible preceding up-arrow
		LD	A,C
		CALL	ECHOC		;cy if not graphic (or special case)
		Jr	NC,TABOUT	;skip if graphic, tab, cr, lf, or ctlh
;send preceding up arrow
		PUSH	AF
		LD	C,CTL
		CALL	CONOUT		;up arrow
		POP	AF
		OR	40H		;becomes graphic letter
		LD	C,A		;ready to print
;(drop through to tabout)
;
TABOUT:
;expand tabs to console
		LD	A,C
		CP	TAB
		Jr	NZ,CONOUT	;direct to conout if not
;tab encountered, move to next tab position
TAB0:
		LD	C,' '
		CALL	CONOUT		;another blank
		LD	A,(COLUMN)
		AND	111B		;column mod 8 = 0 ?
		Jr	NZ,TAB0		;back for another if not
		RET
;
;
BACKUP:
;back-up one screen position
		CALL	PCTLH
		LD	C,' '
		CALL	CONOUTF
;	(drop through to pctlh)				;
PCTLH:
;send ctlh to console without affecting column count
		LD	C,CTLH
		JP	CONOUTF
;ret
;
CRLFP:
;print #, cr, lf for ctlx, ctlu, ctlr functions
;then move to strtcol (starting column)
		LD	C,'#'
		CALL	CONOUT
		CALL	CRLF
;column = 0, move to position strtcol
CRLFP0:
		LD	A,(COLUMN)
		LD	HL,STRTCOL
		CP	(HL)
		RET	NC		;stop when column reaches strtcol
		LD	C,' '
		CALL	CONOUT		;print blank
		Jr	CRLFP0
;;
;
CRLF:
;carriage return line feed sequence
		LD	C,CR
		CALL	CONOUT
		LD	C,LF
		JP	CONOUT
;ret
;
PRINT:
;print message until M(BC) = '$'
		LD	A,(BC)
		CP	'$'
		RET	Z		;stop on _
;more to print
		INC	BC
		PUSH	BC
		LD	C,A		;char to C
		CALL	TABOUT		;another character printed
		POP	BC
		Jr	PRINT
;
READ:		;read to info address (max length, current length, buffer)
		LD	A,(COLUMN)
		LD	(STRTCOL),A	;save start for ctl-x, ctl-h
		LD	HL,(INFO)
		LD	C,(HL)
		INC	HL
		PUSH	HL
		LD	B,0
;B = current buffer length,
;C = maximum buffer length,
;HL= next to fill - 1
READNX:
;read next character, BC, HL active
		PUSH	BC
		PUSH	HL		;blen, cmax, HL saved
READN0:
		CALL	CONINF		;next char in A
		AND	7FH		;mask parity bit
		POP	HL
		POP	BC		;reactivate counters
		CP	CR
		JP	Z,READEN	;end of line?
		CP	LF
		JP	Z,READEN	;also end of line
		cp	13h
		jr	z, loc_D5F9
		CP	CTLH
		jr	z, loc_D5F9
		cp	7Fh
		jr	nz, notrub
;do we have any characters to back over?
loc_D5F9:	LD	A,B
		OR	A
		Jr	Z,READNX
;characters remain in buffer, backup one
		DEC	B		;remove one character
		LD	A,(COLUMN)
		LD	(COMPCOL),A	;col > 0
;compcol > 0 marks repeat as length compute
		Jr	LINELEN		;uses same code as repeat
NOTRUB:
;not a rubout character, check end line
		CP	CTLE
		Jr	NZ,NOTE		;physical end line?
;yes, save active counters and force eol
		PUSH	BC
		PUSH	HL
		CALL	CRLF
		XOR	A
		LD	(STRTCOL),A	;start position = 00
		Jr	READN0		;for another character
NOTE:
;not end of line, list toggle?
		CP	CTLP
		Jr	NZ,NOTP		;skip if not ctlp
;list toggle - change parity
		PUSH	HL		;save next to fill - 1
		LD	HL,LISTCP	;HL=.listcp flag
		LD	A,1
		SUB	(HL)		;TRUE_-listcp
		LD	(HL),A		;listcp = not listcp
		POP	HL
		Jr	READNX		;for another char
NOTP:
;not a ctlp, line delete?
		CP	CTLX
		Jr	NZ,rdecho
		POP	HL		;discard start position
;loop while column > strtcol
BACKX:
		LD	A,(STRTCOL)
		LD	HL,COLUMN
		CP	(HL)
		Jr	NC,READ		;start again
		DEC	(HL)		;column = column - 1
		CALL	BACKUP		;one position
		Jr	BACKX
LINELEN:
;repeat line, or compute line len (ctlh)
;if compcol > 0
		PUSH	BC
		CALL	CRLFP		;save line length
		POP	BC
		POP	HL
		PUSH	HL
		PUSH	BC
;bcur, cmax active, beginning buff at HL
REP0:
		LD	A,B
		OR	A
		Jr	Z,REP1		;count len to 00
		INC	HL
		LD	C,(HL)		;next to print
		DEC	B
		PUSH	BC
		PUSH	HL		;count length down
		CALL	CTLOUT		;character echoed
		POP	HL
		POP	BC		;recall remaining count
		Jr	REP0		;for the next character
REP1:
;end of repeat, recall lengths
;original BC still remains pushed
		PUSH	HL		;save next to fill
		LD	A,(COMPCOL)
		OR	A		;>0 if computing length
		JP	Z,READN0	;for another char if so
;column position computed for ctlh
		LD	HL,COLUMN
		SUB	(HL)		;diff > 0
		LD	(COMPCOL),A	;count down below
;move back compcol-column spaces
BACKSP:
;move back one more space
		CALL	BACKUP		;one space
		LD	HL,COMPCOL
		DEC	(HL)
		Jr	NZ,BACKSP
		JP	READN0		;for next character
;not a ctlr, place into buffer
RDECHO:
		INC	HL
		LD	(HL),A		;character filled to mem
		INC	B		;blen = blen + 1
RDECH1:
;look for a random control character
		PUSH	BC
		PUSH	HL		;active values saved
		LD	C,A		;ready to print
		CALL	CTLOUT		;may be up-arrow C
		POP	HL
		POP	BC
		LD	A,(HL)		;recall char
		CP	CTLC		;set flags for reboot test
		LD	A,B		;move length to A
		Jr	NZ,NOTC		;skip if not a control c
		CP	1		;control C, must be length 1
		JP	Z,REBOOT	;reboot if blen = 1
;length not one, so skip reboot
NOTC:
;not reboot, are we at end of buffer?
		CP	C
		JP	C,READNX	;go for another if not
READEN:
;end of read operation, store blen
		POP	HL
		LD	(HL),B		;M(current len) = B
		LD	C,CR
		JP	CONOUT		;return carriage
;ret
FUNC1:
;return console character with echo
		CALL	CONECH
		Jr	STA_RET
;
FUNC2		EQU	TABOUT
;write console character with tab expansion
;
FUNC3:
;return reader character
		CALL	READERF
		Jr	STA_RET
;
;func4:	equated to punchf
;write punch character
;
;func5:	equated to listf
;write list character
;write to list device
;
FUNC6:
;direct console i/o - read if 0ffh
		LD	A,C
		INC	A
		Jr	Z,DIRINP	;0ffh => 00h, means input mode
		INC	A
		JP	Z,CONSTF	;0feH in C for status
;direct output function
		JP	CONOUTF;
DIRINP:
		CALL	CONSTF		;status check
		OR	A
		ret	z		;skip, return 00 if not ready
;character is ready, get it
		CALL	CONINF	;to A
		Jr	STA_RET
;
FUNC7:
;return io byte
		LD	A,(IOLOC)
		Jr	STA_RET
;
FUNC8:
;set i/o byte
		LD	HL,IOLOC
		LD	(HL),C
		RET			;jmp goback
;
FUNC9:
;write line until _ encountered
		EX	DE,HL		;was lhld info
		LD	C,L
		LD	B,H		;BC=string address
		JP	PRINT		;out to console
;
FUNC10		EQU	READ
;read a buffered console line
;
FUNC11:
;check console status
		CALL	CONBRK
;(drop through to sta_ret)
STA_RET:
;store the A register to aret
		LD	(ARET),A
FUNC_RET:	;
		RET			;jmp goback (pop stack for non cp/m functions)
;
SETLRET1:
;set lret = 1
		LD	A,1
		Jr	STA_RET;
;
;
;
;	data areas
;
COMPCOL:	DB	0		;TRUE_ if computing column position
STRTCOL:	DB	0		;starting column position after read
COLUMN:		DB	0		;column position
LISTCP:		DB	0		;listing toggle
ENTSP:		DS	2		;entry stack pointer
		DS	SSIZE*2		;stack size
LSTACK:
;	end of Basic I/O System
;
;*****************************************************************
;*****************************************************************
;
;	common values shared between bdosi and bdos
USRCODE:	DB	0		;current user number
CURDSK:		DB	0		;current disk number
INFO:		DS	2		;information address
ARET:		DS	2		;address value to return
LRET		EQU	ARET		;low(aret)
;
;*****************************************************************
;*****************************************************************
;**                                                             **
;**   B a s i c    D i s k   O p e r a t i n g   S y s t e m    **
;**                                                             **
;*****************************************************************
;*****************************************************************
;
DVERS		EQU	22H		;version 2.2
;	module addresses
;
;	literal constants
TRUE_		EQU	0FFH		;constant TRUE_
FALSE_		EQU	000H		;constant FALSE_
ENDDIR		EQU	0FFFFH		;end of directory
BYTE		EQU	1		;number of bytes for "byte" type
WORD		EQU	2		;number of bytes for "word" type
;
;	fixed addresses in low memory
TFCB		EQU	005CH		;default fcb location
TBUFF		EQU	0080H		;default buffer location
;
;	fixed addresses referenced in bios module are
;	pererr (0009), selerr (000c), roderr (000f)
;
;	error message handlers
;
;per_error:
;report permanent error to user
;	lxi h,pererr  jmp goerr
;
;rod_error:						;
;report read/only disk error
;	lxi h,roderr  jmp goerr				;
;
;rof_error:						;
;report read/only file error			;
;	lxi h,roferr	;jmp goerr
;
SEL_ERROR:
;report select error
		LD	HL,SELERR;
;
;
GOERR:
;HL = .errorhandler, call subroutine
		LD	E,(HL)
		INC	HL
		LD	D,(HL)		;address of routine in DE
		EX	DE,HL
		JP	(HL)		;to subroutine
;
;
;
;	local subroutines for bios interface
;
MOVE:
;move data length of length C from source DE to
;destination given by HL
		INC	C		;in case it is zero
MOVE0:
		DEC	C
		RET	Z		;more to move
		LD	A,(DE)
		LD	(HL),A		;one byte moved
		INC	DE
		INC	HL		;to next byte
		Jr	MOVE0
;
SELECTDISK:
;select the disk drive given by curdsk, and fill
;the base addresses curtrka - alloca, then fill
;the values of the disk parameter block
		LD	A,(CURDSK)
		LD	C,A		;current disk# to c
;lsb of e = 0 if not yet logged - in
		CALL	SELDSKF		;HL filled by call
;HL = 0000 if error, otherwise disk headers
		LD	A,H
		OR	L
		RET	Z		;return with 0000 in HL and z flag
;disk header block address in hl
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		INC	HL		;DE=.tran
		LD	(CDRMAXA),HL
		INC	HL
		INC	HL		;.cdrmax
		LD	(CURTRKA),HL
		INC	HL
		INC	HL		;HL=.currec
		LD	(CURRECA),HL
		INC	HL
		INC	HL		;HL=.buffa
;DE still contains .tran
		EX	DE,HL
		LD	(TRANV),HL	;.tran vector
		LD	HL,BUFFA	;DE= source for move, HL=dest
		LD	C,ADDLIST
		CALL	MOVE		;addlist filled
;now fill the disk parameter block
		LD	HL,(DPBADDR)
		EX	DE,HL		;DE is source
		LD	HL,SECTPT	;HL is destination
		LD	C,DPBLIST
		CALL	MOVE		;data filled
;now set single/double map mode
		LD	HL,(MAXALL)	;largest allocation number
		LD	A,H		;00 indicates < 255
		LD	HL,SINGLE
		LD	(HL),TRUE_	;assume a=00
		OR	A
		Jr	Z,RETSELECT
;high order of maxall not zero, use double dm
		LD	(HL),FALSE_
RETSELECT:
		LD	A,TRUE_
		OR	A
		RET			;select disk function ok
;
HOME:
;move to home position, then offset to start of dir
		CALL	HOMEF		;move to track 00, sector 00 reference
;lxi h,offset ;mov c,m ;inx h ;mov b,m ;call settrkf	;
;first directory position selected
		XOR	A		;constant zero to accumulator
		LD	HL,(CURTRKA)
		LD	(HL),A
		INC	HL
		LD	(HL),A		;curtrk=0000
		LD	HL,(CURRECA)
		LD	(HL),A
		INC	HL
		LD	(HL),A		;currec=0000
;curtrk, currec both set to 0000
		RET
;
RDBUFF:
;read buffer and check condition
		CALL	READF		;current drive, track, sector, dma
		Jr	DIOCOMP		;check for i/o errors
;
WRBUFF:
;write buffer and check condition
;write type (wrtype) is in register C
;wrtype = 0 => normal write operation
;wrtype = 1 => directory write operation
;wrtype = 2 => start of new block
		CALL	WRITEF		;current drive, track, sector, dma
DIOCOMP:	;check for disk errors
		OR	A
		RET	Z	;
		LD	HL,PERERR;
		JP	GOERR
;
SEEK_DIR:
;seek the record containing the current dir entry
		LD	HL,(DCNT)	;directory counter to HL
		LD	C,DSKSHF
		CALL	HLROTR		;value to HL
		LD	(ARECORD),HL
		LD	(DREC),HL	;ready for seek
;  jmp seek				;
;ret
;
;
SEEK:
;seek the track given by arecord (actual record)
;local equates for registers
;ARECH		EQU	B
;ARECL		EQU	C		;arecord = BC
;CRECH		EQU	D
;CRECL		EQU	E		;currec  = DE
;CTRKH		EQU	H
;CTRKL		EQU	L		;curtrk  = HL
;TCRECH		EQU	H
;TCRECL		EQU	L		;tcurrec = HL
;load the registers from memory
		LD	HL,ARECORD                     ;LD	HL,ARECORD
		LD	c,(hl)                         ;LD	ARECL,M
		INC	HL                             ;INC	HL
		LD	b,(hl)                         ;LD	ARECH,M
		LD	HL,(CURRECA)                   ;LD	HL,(CURRECA)
		LD	e,(hl)                         ;LD	CRECL,M
		INC	HL                             ;INC	HL
		LD	d,(hl)                         ;LD	CRECH,M
		LD	HL,(CURTRKA)                   ;LD	HL,(CURTRKA)
		LD	A,(HL)                         ;LD	A,(HL)
		INC	HL                             ;INC	HL
		LD	h,(hl)                         ;LD	CTRKH,M
		LD	l,A                            ;LD	CTRKL,A
;loop while arecord < currec
SEEK0:
		LD	A,c                            ;LD	A,ARECL
		SUB	e                              ;SUB	CRECL
		LD	A,b                            ;LD	A,ARECH
		SBC	A,d                            ;SBC	A,CRECH
		Jr	NC,SEEK1	;skip if arecord >= currec
;currec = currec - sectpt
		PUSH	hl                              ;PUSH	CTRKH
		LD	HL,(SECTPT)                     ;LD	HL,(SECTPT)
		LD	A,e                             ;LD	A,CRECL
		SUB	L                               ;SUB	L
		LD	e,A                             ;LD	CRECL,A
		LD	A,d                             ;LD	A,CRECH
		SBC	A,H                             ;SBC	A,H
		LD	d,A                             ;LD	CRECH,A
		POP	hl                              ;POP	CTRKH
;curtrk = curtrk - 1
		DEC	hl                              ;DEC	CTRKH
		Jr	SEEK0		;for another try
SEEK1:
;look while arecord >= (t:=currec + sectpt)
		PUSH	hl                       	;PUSH	CTRKH
		LD	HL,(SECTPT)                     ;LD	HL,(SECTPT)
		ADD	HL,de	;HL = currec+sectpt     ;ADD	HL,CRECH
		Jr	C,SEEK2	;can be > FFFFH         ;JP	C,SEEK2
		LD	A,c                             ;LD	A,ARECL
		SUB	l                               ;SUB	TCRECL
		LD	A,b                             ;LD	A,ARECH
		SBC	A,h                             ;SBC	A,TCRECH
		Jr	C,SEEK2	;skip if t > arecord    ;JP	C,SEEK2
;currec = t
		EX	DE,HL                           ;EX	DE,HL
;curtrk = curtrk + 1
		POP	hl                              ;POP	CTRKH
		INC	hl                              ;INC	CTRKH
		Jr	SEEK1	;for another try        ;JP	SEEK1
SEEK2:		POP	hl                              ;POP	CTRKH
;arrive here with updated values in each register
		PUSH	bc                              ;PUSH	ARECH
		PUSH	de                              ;PUSH	CRECH
		PUSH	hl	;to stack for later     ;PUSH	CTRKH
;stack contains (lowest) BC=arecord, DE=currec, HL=curtrk
		EX	DE,HL                           ;EX	DE,HL
		LD	HL,(OFFSET)                     ;LD	HL,(OFFSET)
		ADD	HL,DE	;HL = curtrk+offset     ;ADD	HL,DE
		LD	B,H                             ;LD	B,H
		LD	C,L                             ;LD	C,L
		CALL	SETTRKF	;track set up           ;CALL	SETTRKF
;note that BC - curtrk is difference to move in bios
		POP	DE	;recall curtrk          ;POP	DE
		LD	HL,(CURTRKA)                    ;LD	HL,(CURTRKA)
		LD	(HL),E                          ;LD	(HL),E
		INC	HL                              ;INC	HL
		LD	(HL),D	;curtrk updated         ;LD	(HL),D
;now compute sector as arecord-currec
		POP	de	;recall currec          ;POP	CRECH
		LD	HL,(CURRECA)                    ;LD	HL,(CURRECA)
		LD	(HL),e                          ;LD	(HL),CRECL
		INC	HL                              ;INC	HL
		LD	(HL),d                          ;LD	(HL),CRECH
		POP	bc	;BC=arecord, DE=currec  ;POP	ARECH
		LD	A,c                             ;LD	A,ARECL
		SUB	e                               ;SUB	CRECL
		LD	c,A                             ;LD	ARECL,A
		LD	A,b                             ;LD	A,ARECH
		SBC	A,d                             ;SBC	A,CRECH
		LD	b,A                             ;LD	ARECH,A
		LD	HL,(TRANV)                      ;LD	HL,(TRANV)
		EX	DE,HL	;BC=sector#, DE=.tran   ;EX	DE,HL
		CALL	SECTRAN	;HL = tran(sector)      ;CALL	SECTRAN
		LD	C,L                             ;LD	C,L
		LD	B,H	;BC = tran(sector)      ;LD	B,H
		JP	SETSECF	;sector selected        ;JP	SETSECF
;ret
;
;	file control block (fcb) constants
EMPTY		EQU	0E5H		;empty directory entry
LSTREC		EQU	127		;last record# in extent
RECSIZ		EQU	128		;record size
FCBLEN		EQU	32		;file control block size
DIRREC		EQU	RECSIZ/FCBLEN	;directory elts / record
DSKSHF		EQU	2		;log2(dirrec)
DSKMSK		EQU	DIRREC-1
FCBSHF		EQU	5		;log2(fcblen)
;
EXTNUM		EQU	12		;extent number field
MAXEXT		EQU	31		;largest extent number
UBYTES		EQU	13		;unfilled bytes field
MODNUM		EQU	14		;data module number
MAXMOD		EQU	15		;largest module number
FWFMSK		EQU	80H		;file write flag is high order modnum
NAMLEN		EQU	15		;name length
RECCNT		EQU	15		;record count field
DSKMAP		EQU	16		;disk map field
LSTFCB		EQU	FCBLEN-1
NXTREC		EQU	FCBLEN
RANREC		EQU	NXTREC+1	;random record field (2 bytes)
;
;	reserved file indicators
ROFILE		EQU	9		;high order of first type char
INVIS		EQU	10		;invisible file in dir command
;	equ	11	;reserved
;
;	utility functions for file access
;
DM_POSITION:
;compute disk map position for vrecord to HL
		LD	HL,BLKSHF
		LD	C,(HL)		;shift count to C
		LD	A,(VRECORD)	;current virtual record to A
DMPOS0:
		OR	A
		RRA
		DEC	C
		Jr	NZ,DMPOS0
;A = shr(vrecord,blkshf) = vrecord/2**(sect/block)
		LD	B,A		;save it for later addition
		LD	A,8
		SUB	(HL)		;8-blkshf to accumulator
		LD	C,A		;extent shift count in register c
		LD	A,(EXTVAL)	;extent value ani extmsk
DMPOS1:
;blkshf = 3,4,5,6,7, C=5,4,3,2,1
;shift is 4,3,2,1,0
		DEC	C
		Jr	Z,DMPOS2
		OR	A
		RLA
		Jr	DMPOS1
DMPOS2:
;arrive here with A = shl(ext and extmsk,7-blkshf)
		ADD	A,B		;add the previous shr(vrecord,blkshf) value
;A is one of the following values, depending upon alloc
;bks blkshf
;1k   3     v/8 + extval * 16
;2k   4     v/16+ extval * 8
;4k   5     v/32+ extval * 4
;8k   6     v/64+ extval * 2
;16k  7     v/128+extval * 1
		RET			;with dm_position in A
;
GETDM:
;return disk map value from position given by BC
		LD	HL,(INFO)	;base address of file control block
		LD	DE,DSKMAP
		ADD	HL,DE		;HL =.diskmap
		ADD	HL,BC		;index by a single byte value
		LD	A,(SINGLE)	;single byte/map entry?
		OR	A
		Jr	Z,GETDMD	;get disk map single byte
		LD	L,(HL)
		LD	H,0
		RET			;with HL=00bb
GETDMD:
		ADD	HL,BC		;HL=.fcb(dm+i*2)
;double precision value returned
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		EX	DE,HL
		RET
;
INDEX:
;compute disk block number from current fcb
		CALL	DM_POSITION	;0...15 in register A
		LD	C,A
		LD	B,0
		CALL	GETDM		;value to HL
		LD	(ARECORD),HL
		RET
;
ALLOCATED:
;called following index to see if block allocated
		LD	HL,(ARECORD)
		LD	A,L
		OR	H
		RET
;
ATRAN:
;compute actual record address, assuming index called
		LD	A,(BLKSHF)	;shift count to reg A
		LD	HL,(ARECORD)
ATRAN0:
		ADD	HL,HL
		DEC	A
		Jr	NZ,ATRAN0	;shl(arecord,blkshf)
		LD	(ARECORD1),HL	;save shifted block #
		LD	A,(BLKMSK)
		LD	C,A		;mask value to C
		LD	A,(VRECORD)
		AND	C		;masked value in A
		OR	L
		LD	L,A		;to HL
		LD	(ARECORD),HL	;arecord=HL or (vrecord and blkmsk)
		RET
;
GETEXTA:
;get current extent field address to A
		LD	HL,(INFO)
		LD	DE,EXTNUM
		ADD	HL,DE		;HL=.fcb(extnum)
		RET
;
GETFCBA:
;compute reccnt and nxtrec addresses for get/setfcb
		LD	HL,(INFO)
		LD	DE,RECCNT
		ADD	HL,DE
		EX	DE,HL		;DE=.fcb(reccnt)
		LD	HL,NXTREC-RECCNT
		ADD	HL,DE		;HL=.fcb(nxtrec)
		RET
;
GETFCB:
;set variables from currently addressed fcb
		CALL	GETFCBA		;addresses in DE, HL
		LD	A,(HL)
		LD	(VRECORD),A	;vrecord=fcb(nxtrec)
		EX	DE,HL
		LD	A,(HL)
		LD	(RCOUNT),A	;rcount=fcb(reccnt)
		CALL	GETEXTA		;HL=.fcb(extnum)
		LD	A,(EXTMSK)	;extent mask to a
		AND	(HL)		;fcb(extnum) and extmsk
		LD	(EXTVAL),A
		RET
;
SETFCB:
;place values back into current fcb
		CALL	GETFCBA		;addresses to DE, HL
		LD	A,(SEQIO)
		CP	02
		Jr	NZ,SETFCB1
		XOR	A		;check ranfill
SETFCB1:
		LD	C,A		;=1 if sequential i/o
		LD	A,(VRECORD)
		ADD	A,C
		LD	(HL),A		;fcb(nxtrec)=vrecord+seqio
		EX	DE,HL
		LD	A,(RCOUNT)
		LD	(HL),A		;fcb(reccnt)=rcount
		RET
;
HLROTR:
;hl rotate right by amount C
		INC	C		;in case zero
HLROTR0:	DEC	C
		RET	Z		;return when zero
		LD	A,H
		OR	A
		RRA
		LD	H,A		;high byte
		LD	A,L
		RRA
		LD	L,A		;low byte
		Jr	HLROTR0
;
;
COMPUTE_CS:
;compute checksum for current directory buffer
		LD	C,RECSIZ	;size of directory buffer
		LD	HL,(BUFFA)	;current directory buffer
		XOR	A		;clear checksum value
COMPUTECS0:
		ADD	A,(HL)
		INC	HL
		DEC	C		;cs=cs+buff(recsiz-C)
		Jr	NZ,COMPUTECS0
		RET	;with checksum in A
;
HLROTL:
;rotate the mask in HL by amount in C
		INC	C		;may be zero
HLROTL0:	DEC	C
		RET	Z		;return if zero
		ADD	HL,HL
		Jr	HLROTL0
;
SET_CDISK:
;set a "1" value in curdsk position of BC
		PUSH	BC		;save input parameter
		LD	A,(CURDSK)
		LD	C,A		;ready parameter for shift
		LD	HL,1		;number to shift
		CALL	HLROTL		;HL = mask to integrate
		POP	BC		;original mask
		LD	A,C
		OR	L
		LD	L,A
		LD	A,B
		OR	H
		LD	H,A		;HL = mask or rol(1,curdsk)
		RET
;
NOWRITE:
;return TRUE_ if dir checksum difference occurred
		LD	HL,(RODSK)
loc_D8DB:	LD	A,(CURDSK)
		LD	C,A
		CALL	HLROTR
		LD	A,L
		AND	1B
		RET			;non zero if nowrite
;
SET_RO:
;set current disk to read only
		LD	HL,RODSK
		LD	C,(HL)
		INC	HL
		LD	B,(HL)
		CALL	SET_CDISK	;sets bit to 1
		LD	(RODSK),HL
;high water mark in directory goes to max
		LD	HL,(DIRMAX)
		INC	HL
		EX	DE,HL		;DE = directory max
		LD	HL,(CDRMAXA)	;HL = .cdrmax
		LD	(HL),E
		INC	HL
		LD	(HL),D		;cdrmax = dirmax
		RET
;
CHECK_RODIR:
;check current directory element for read/only status
		CALL	GETDPTRA	;address of element
;
CHECK_ROFILE:
;check current buff(dptr) or fcb(0) for r/o status
		LD	DE,ROFILE
		ADD	HL,DE		;offset to ro bit
		LD	A,(HL)
		RLA
		RET	NC		;return if not set
loc_D908:	LD	HL,ROFERR
		JP	GOERR;
;	jmp rof_error ;exit to read only disk message
;
;
CHECK_WRITE:
;check for write protected disk
		CALL	NOWRITE
		RET	Z		;ok to write if not rodsk
		LD	HL,RODERR
		JP	GOERR
;	jmp rod_error ;read only disk error
;
GETDPTRA:
;compute the address of a directory element at
;positon dptr in the buffer
		LD	HL,(BUFFA)
		LD	A,(DPTR);
ADDH:
;HL = HL + A
		ADD	A,L
		LD	L,A
		RET	NC
;overflow to H
		INC	H
		RET
;
;
GETMODNUM:
;compute the address of the module number
;bring module number to accumulator
;(high order bit is fwf (file write flag)
		LD	HL,(INFO)
		LD	DE,MODNUM
		ADD	HL,DE		;HL=.fcb(modnum)
		LD	A,(HL)
		RET			;A=fcb(modnum)
;
CLRMODNUM:
;clear the module number field for user open/make
		CALL	GETMODNUM
		LD	(HL),0		;fcb(modnum)=0
		RET
;
SETFWF:
		CALL	GETMODNUM	;HL=.fcb(modnum), A=fcb(modnum)
;set fwf (file write flag) to "1"
		OR	FWFMSK
		LD	(HL),A		;fcb(modnum)=fcb(modnum) or 80h
;also returns non zero in accumulator
		RET
;
;
COMPCDR:
;return cy if cdrmax > dcnt
		LD	HL,(DCNT)
		EX	DE,HL		;DE = directory counter
		LD	HL,(CDRMAXA)	;HL=.cdrmax
		LD	A,E
		SUB	(HL)		;low(dcnt) - low(cdrmax)
		INC	HL		;HL = .cdrmax+1
		LD	A,D
		SBC	A,(HL)		;hig(dcnt) - hig(cdrmax)
;condition dcnt - cdrmax  produces cy if cdrmax>dcnt
		RET
;
SETCDR:
;if not (cdrmax > dcnt) then cdrmax = dcnt+1
		CALL	COMPCDR
		RET	C		;return if cdrmax > dcnt
;otherwise, HL = .cdrmax+1, DE = dcnt
		INC	DE
		LD	(HL),D
		DEC	HL
		LD	(HL),E
		RET
;
SUBDH:
;compute HL = DE - HL
		LD	A,E
		SUB	L
		LD	L,A
		LD	A,D
		SBC	A,H
		LD	H,A
		RET
;
NEWCHECKSUM:
		LD	C,TRUE_		;drop through to compute new checksum
CHECKSUM:
;compute current checksum record and update the
;directory element if C=TRUE_, or check for = if not
;drec < chksiz?
		LD	HL,(DREC)
		EX	DE,HL
		LD	HL,(CHKSIZ)
		CALL	SUBDH		;DE-HL
		RET	NC		;skip checksum if past checksum vector size
;drec < chksiz, so continue
		PUSH	BC		;save init flag
		CALL	COMPUTE_CS	;check sum value to A
		LD	HL,(CHECKA)	;address of check sum vector
		EX	DE,HL
		LD	HL,(DREC)	;value of drec
		ADD	HL,DE		;HL = .check(drec)
		POP	BC		;recall TRUE_=0ffh or FALSE_=00 to C
		INC	C		;0ffh produces zero flag
		Jr	Z,INITIAL_CS
;not initializing, compare
		CP	(HL)		;compute_cs=check(drec)?
		RET	Z		;no message if ok
;checksum error, are we beyond
;the end of the disk?
		CALL	COMPCDR
		RET	NC		;no message if so
		jp	SET_RO		;read/only disk set
INITIAL_CS:
;initializing the checksum
		LD	(HL),A
		RET
;
;
WRDIR:
;write the current directory entry, set checksum
		CALL	NEWCHECKSUM	;initialize entry
		CALL	SETDIR		;directory dma
		LD	C,1		;indicates a write directory operation
		CALL	WRBUFF		;write the buffer
		Jr	SETDATA		;to data dma address
;ret
;
RD_DIR:
;read a directory entry into the directory buffer
		CALL	SETDIR		;directory dma
		CALL	RDBUFF		;directory record loaded
; jmp setdata to data dma address
;ret
;
SETDATA:
;set data dma address
		LD	HL,DMAAD
		Jr	SETDMA		;to complete the call
;
SETDIR:
;set directory dma address
		LD	HL,BUFFA	;jmp setdma to complete call
;
SETDMA:
;HL=.dma address to set (i.e., buffa or dmaad)
		LD	C,(HL)
		INC	HL
		LD	B,(HL)		;parameter ready
		JP	SETDMAF
;
;
DIR_TO_USER:
;copy the directory entry to the user buffer
;after call to search or searchn by user code
		LD	HL,(BUFFA)
		EX	DE,HL		;source is directory buffer
		LD	HL,(DMAAD)	;destination is user dma address
		LD	C,RECSIZ	;copy entire record
		JP	MOVE
;ret
;
END_OF_DIR:
;return zero flag if at end of directory, non zero
;if not at end (end of dir if dcnt = 0ffffh)
		LD	HL,DCNT
		LD	A,(HL)		;may be 0ffh
		INC	HL
		CP	(HL)		;low(dcnt) = high(dcnt)?
		RET	NZ		;non zero returned if different
;high and low the same, = 0ffh?
		INC	A		;0ffh becomes 00 if so
		RET
;
SET_END_DIR:
;set dcnt to the end of the directory
		LD	HL,ENDDIR
		LD	(DCNT),HL
		RET
;
READ_DIR:
;read next directory entry, with C=TRUE_ if initializing
		LD	HL,(DIRMAX)
		EX	DE,HL		;in preparation for subtract
		LD	HL,(DCNT)
		INC	HL
		LD	(DCNT),HL	;dcnt=dcnt+1
;continue while dirmax >= dcnt (dirmax-dcnt no cy)
		CALL	SUBDH		;DE-HL
		Jr	NC,READ_DIR0
;yes, set dcnt to end of directory
		Jr	SET_END_DIR;
;		ret				;
READ_DIR0:
;not at end of directory, seek next element
;initialization flag is in C
		LD	A,(DCNT)
		AND	DSKMSK		;low(dcnt) and dskmsk
		LD	B,FCBSHF	;to multiply by fcb size
READ_DIR1:
		ADD	A,A
		DEC	B
		Jr	NZ,READ_DIR1
;A = (low(dcnt) and dskmsk) shl fcbshf
		LD	(DPTR),A	;ready for next dir operation
		OR	A
		RET	NZ		;return if not a new record
		PUSH	BC		;save initialization flag C
		CALL	SEEK_DIR	;seek proper record
		CALL	RD_DIR		;read the directory record
		POP	BC		;recall initialization flag
		JP	CHECKSUM	;checksum the directory elt
;ret
;
;
GETALLOCBIT:
;given allocation vector position BC, return with byte
;containing BC shifted so that the least significant
;bit is in the low order accumulator position.  HL is
;the address of the byte for possible replacement in
;memory upon return, and D contains the number of shifts
;required to place the returned value back into position
		LD	A,C
		AND	111B
		INC	A
		LD	E,A
		LD	D,A
;d and e both contain the number of bit positions to shift
		LD	A,C
		RRCA
		RRCA
		RRCA
		AND	11111B
		LD	C,A		;C shr 3 to C
		LD	A,B
		ADD	A,A
		ADD	A,A
		ADD	A,A
		ADD	A,A
		ADD	A,A		;B shl 5
		OR	C
		LD	C,A		;bbbccccc to C
		LD	A,B
		RRCA
		RRCA
		RRCA
		AND	11111B
		LD	B,A		;BC shr 3 to BC
		LD	HL,(ALLOCA)	;base address of allocation vector
		ADD	HL,BC
		LD	A,(HL)		;byte to A, hl = .alloc(BC shr 3)
;now move the bit to the low order position of A
ROTL:		RLCA
		DEC	E
		Jr	NZ,ROTL
		RET
;
;
SETALLOCBIT:
;BC is the bit position of ALLOC to set or reset.  The
;value of the bit is in register E.
		PUSH	DE
		CALL	GETALLOCBIT	;shifted val A, count in D
		AND	11111110B	;mask low bit to zero (may be set)
		POP	BC
		OR	C		;low bit of C is masked into A
;	jmp rotr ;to rotate back into proper position
;ret
ROTR:
;byte value from ALLOC is in register A, with shift count
;in register C (to place bit back into position), and
;target ALLOC position in registers HL, rotate and replace
		RRCA
		DEC	D
		Jr	NZ,ROTR		;back into position
		LD	(HL),A		;back to ALLOC
		RET
;
SCANDM:
;scan the disk map addressed by dptr for non-zero
;entries, the allocation vector entry corresponding
;to a non-zero entry is set to the value of C (0,1)
		CALL	GETDPTRA	;HL = buffa + dptr
;HL addresses the beginning of the directory entry
		LD	DE,DSKMAP
		ADD	HL,DE		;hl now addresses the disk map
		PUSH	BC		;save the 0/1 bit to set
		LD	C,FCBLEN-DSKMAP+1	;size of single byte disk map + 1
SCANDM0:
;loop once for each disk map entry
		POP	DE		;recall bit parity
		DEC	C
		RET	Z		;all done scanning?
;no, get next entry for scan
		PUSH	DE		;replace bit parity
		LD	A,(SINGLE)
		OR	A
		Jr	Z,SCANDM1
;single byte scan operation
		PUSH	BC		;save counter
		PUSH	HL		;save map address
		LD	C,(HL)
		LD	B,0		;BC=block#
		Jr	SCANDM2
SCANDM1:
;double byte scan operation
		DEC	C		;count for double byte
		PUSH	BC		;save counter
		LD	C,(HL)
		INC	HL
		LD	B,(HL)		;BC=block#
		PUSH	HL		;save map address
SCANDM2:
;arrive here with BC=block#, E=0/1
		LD	A,C
		OR	B		;skip if = 0000
		Jr	Z,SCANM3
		LD	HL,(MAXALL)	;check invalid index
		LD	A,L
		SUB	C
		LD	A,H
		SBC	A,B		;maxall - block#
		CALL	NC,SETALLOCBIT	;
;bit set to 0/1
SCANM3:		;
		POP	HL
		INC	HL		;to next bit position
		POP	BC		;recall counter
		Jr	SCANDM0		;for another item
;
INITIALIZE:
;initialize the current disk
;lret = FALSE_ ;set to TRUE_ if _ file exists
;compute the length of the allocation vector - 2
		LD	HL,(MAXALL)
		LD	C,3		;perform maxall/8
;number of bytes in alloc vector is (maxall/8)+1
		CALL	HLROTR
		INC	HL		;HL = maxall/8+1
		LD	B,H
		LD	C,L		;count down BC til zero
		LD	HL,(ALLOCA)	;base of allocation vector
;fill the allocation vector with zeros
INITIAL0:
		LD	(HL),0
		INC	HL		;alloc(i)=0
		DEC	BC		;count length down
		LD	A,B
		OR	C
		Jr	NZ,INITIAL0
;set the reserved space for the directory
		LD	HL,(DIRBLK)
		EX	DE,HL
		LD	HL,(ALLOCA)	;HL=.alloc()
		LD	(HL),E
		INC	HL
		LD	(HL),D		;sets reserved directory blks
;allocation vector initialized, home disk
		CALL	HOME
;cdrmax = 3 (scans at least one directory record)
		LD	HL,(CDRMAXA)
		LD	(HL),3
		INC	HL
		LD	(HL),0
;cdrmax = 0000
		CALL	SET_END_DIR	;dcnt = enddir
;read directory entries and check for allocated storage
INITIAL2:
		LD	C,TRUE_
		CALL	READ_DIR
		CALL	END_OF_DIR
		RET	Z		;return if end of directory
;not end of directory, valid entry?
		CALL	GETDPTRA	;HL = buffa + dptr
		LD	A,(HL)
		CP	EMPTY
		Jr	Z,INITIAL2	;go get another item
		inc	hl
		ld	a, (hl)
		dec	hl
		add	a, 10h
		cp	11h
		jr	c, initial2
;not empty, user code the same?
		LD	A,(USRCODE)
		CP	(HL)
		Jr	NZ,PDOLLAR
;same user code, check for '$' submit
		INC	HL
		LD	A,(HL)		;first character
		SUB	'$'		;dollar file?
		Jr	NZ,PDOLLAR
;dollar file found, mark in lret
		DEC	A
		LD	(LRET),A	;lret = 255
PDOLLAR:
;now scan the disk map for allocated blocks
		LD	C,1		;set to allocated
		CALL	SCANDM
		CALL	SETCDR		;set cdrmax to dcnt
		Jr	INITIAL2	;for another entry
;
COPY_DIRLOC:
;copy directory location to lret following
;delete, rename, ... ops
		LD	A,(DIRLOC)
		JP	STA_RET;
;	ret						;
;
COMPEXT:
;compare extent# in A with that in C, return nonzero
;if they do not match
		PUSH	BC		;save C's original value
		PUSH	AF
		LD	A,(EXTMSK)
		CPL
		LD	B,A
;B has negated form of extent mask
		LD	A,C
		AND	B
		LD	C,A		;low bits removed from C
		POP	AF
		AND	B		;low bits removed from A
		SUB	C
		AND	MAXEXT		;set flags
		POP	BC		;restore original values
		RET
;
SEARCH:
;search for directory element of length C at info
		LD	A,0FFH
		LD	(DIRLOC),A	;changed if actually found
		LD	HL,SEARCHL
		LD	(HL),C		;searchl = C
		LD	HL,(INFO)
		LD	(SEARCHA),HL	;searcha = info
		CALL	SET_END_DIR	;dcnt = enddir
		CALL	HOME		;to start at the beginning
;(drop through to searchn)			;
;
SEARCHN:
;search for the next directory element, assuming
;a previous call on search which sets searcha and
;searchl
		LD	C,FALSE_
		CALL	READ_DIR	;read next dir element
		CALL	END_OF_DIR
		Jr	Z,SEARCH_FIN	;skip to end if so
;not end of directory, scan for match
		LD	HL,(SEARCHA)
		EX	DE,HL		;DE=beginning of user fcb
		LD	A,(DE)		;first character
		CP	EMPTY		;keep scanning if empty
		Jr	Z,SEARCHNEXT
;not empty, may be end of logical directory
		PUSH	DE		;save search address
		CALL	COMPCDR		;past logical end?
		POP	DE		;recall address
		Jr	NC,SEARCH_FIN	;artificial stop
SEARCHNEXT:
		CALL	GETDPTRA	;HL = buffa+dptr
		LD	A,(SEARCHL)
		LD	C,A		;length of search to c
		LD	B,0		;b counts up, c counts down
SEARCHLOOP:
		LD	A,C
		OR	A
		Jr	Z,ENDSEARCH
		LD	A,(DE)
		CP	'?'
		Jr	Z,SEARCHOK	;? matches all
;scan next character if not ubytes
		LD	A,B
		CP	UBYTES
		Jr	Z,SEARCHOK
;not the ubytes field, extent field?
		CP	EXTNUM		;may be extent field
		LD	A,(DE)		;fcb character
		Jr	Z,SEARCHEXT	;skip to search extent
		SUB	(HL)
		AND	7FH		;mask-out flags/extent modulus
		Jr	NZ,SEARCHN	;skip if not matched
		Jr	SEARCHOK	;matched character
SEARCHEXT:
;A has fcb character
;attempt an extent # match
		PUSH	BC		;save counters
		LD	C,(HL)		;directory character to c
		CALL	COMPEXT		;compare user/dir char
		POP	BC		;recall counters
		Jr	NZ,SEARCHN	;skip if no match
SEARCHOK:
;current character matches
		INC	DE
		INC	HL
		INC	B
		DEC	C
		Jr	SEARCHLOOP
ENDSEARCH:
;entire name matches, return dir position
		LD	A,(DCNT)
		AND	DSKMSK
		LD	(LRET),A
;lret = low(dcnt) and 11b
		LD	HL,DIRLOC
		LD	A,(HL)
		RLA
		RET	NC		;dirloc=0ffh?
;yes, change it to 0 to mark as found
		XOR	A
		LD	(HL),A		;dirloc=0
		RET
SEARCH_FIN:
;end of directory, or empty name
		CALL	SET_END_DIR	;may be artifical end
		LD	A,255
		JP	STA_RET;
;
;
DELETE:
;delete the currently addressed file
		CALL	CHECK_WRITE	;write protected?
		LD	C,EXTNUM
		CALL	SEARCH		;search through file type
DELETE0:
;loop while directory matches
		CALL	END_OF_DIR
		RET	Z		;stop if end
;set each non zero disk map entry to 0
;in the allocation vector
;may be r/o file
		CALL	CHECK_RODIR	;ro disk error if found
		CALL	GETDPTRA	;HL=.buff(dptr)
		LD	(HL),EMPTY
		LD	C,0
		CALL	SCANDM		;alloc elts set to 0
		CALL	WRDIR		;write the directory
		CALL	SEARCHN		;to next element
		Jr	DELETE0		;for another record
;
GET_BLOCK:
;given allocation vector position BC, find the zero bit
;closest to this position by searching left and right.
;if found, set the bit to one and return the bit position
;in hl.  if not found (i.e., we pass 0 on the left, or
;maxall on the right), return 0000 in hl
		LD	D,B
		LD	E,C		;copy of starting position to de
LEFTTST:
		LD	A,C
		OR	B
		Jr	Z,RIGHTTST	;skip if left=0000
;left not at position zero, bit zero?
		DEC	BC
		PUSH	DE
		PUSH	BC		;left,right pushed
		CALL	GETALLOCBIT
		RRA
		Jr	NC,RETBLOCK	;return block number if zero
;bit is one, so try the right
		POP	BC
		POP	DE		;left, right restored
RIGHTTST:
		LD	HL,(MAXALL)	;value of maximum allocation#
		LD	A,E
		SUB	L
		LD	A,D
		SBC	A,H		;right=maxall?
		Jr	NC,RETBLOCK0	;return block 0000 if so
		INC	DE
		PUSH	BC
		PUSH	DE		;left, right pushed
		LD	B,D
		LD	C,E		;ready right for call
		CALL	GETALLOCBIT
		RRA
		Jr	NC,RETBLOCK	;return block number if zero
		POP	DE
		POP	BC		;restore left and right pointers
		Jr	LEFTTST		;for another attempt
RETBLOCK:
		RLA
		INC	A		;bit back into position and set to 1
;d contains the number of shifts required to reposition
		CALL	ROTR		;move bit back to position and store
		POP	HL
		POP	DE		;HL returned value, DE discarded
		RET
RETBLOCK0:
;cannot find an available bit, return 0000
		LD	A,C;
		OR	B
		Jr	NZ,LEFTTST	;also at beginning
		LD	HL,0000H
		RET
;
COPY_FCB:
;copy the entire file control block
		LD	C,0
		LD	E,FCBLEN	;start at 0, to fcblen-1
;	jmp copy_dir			;
;
COPY_DIR:
;copy fcb information starting at C for E bytes
;into the currently addressed directory entry
		PUSH	DE		;save length for later
		LD	B,0		;double index to BC
		LD	HL,(INFO)	;HL = source for data
		ADD	HL,BC
		EX	DE,HL		;DE=.fcb(C), source for copy
		CALL	GETDPTRA	;HL=.buff(dptr), destination
		POP	BC		;DE=source, HL=dest, C=length
		CALL	MOVE		;data moved
SEEK_COPY:
;enter from close to seek and copy current element
		CALL	SEEK_DIR	;to the directory element
		JP	WRDIR		;write the directory element
;ret
;
;
RENAME:
;rename the file described by the first half of
;the currently addressed file control block. the
;new name is contained in the last half of the
;currently addressed file conrol block.  the file
;name and type are changed, but the reel number
;is ignored.  the user number is identical
		CALL	CHECK_WRITE	;may be write protected
;search up to the extent field
		LD	C,EXTNUM
		CALL	SEARCH
;copy position 0
		LD	HL,(INFO)
		push	hl
		pop	ix
		ld	bc, 0C10h	;B=12 Länge fn C=DSKMAP
;extra Copy-Protection CP/A		
		bit	7, (ix+1Bh)	;T3 Bit 7 nfn (POWER SET -X)
		jr	z, loc_DBEA
		ld	a, '$'		;temp file *.$$$?
		cp	(ix+9)		;T1, Filetyp
		jr	nz, loc_DBEA
		cp	(ix+0Ah)	;T2, Filetyp
		jr	nz, loc_DBEA
		cp	(ix+0Bh)	;T3, Filetyp
		jr	nz, loc_DBEA
		ld	hl, loc_D908	;Abbruch mit FILE R/O
		push	hl
		ld	hl, unk_DC06	;empty fcb
		ld	(info),	hl
		ld	bc, 2000h	;b=32 Länge leerer FCB, C=0 (Anfang FCB)
loc_DBEA:	push	bc
		ld	a, (ix+0)	;A=fcb(0)
		ld	(ix+10h), a	;fcb(dskmap)=fcb(0)

;assume the same disk drive for new named file
RENAME0:
		CALL	END_OF_DIR
		jr	z, loc_DC04 	;stop at end of dir
;not end of directory, rename next element
		CALL	CHECK_RODIR	;may be read-only file
		pop	bc
		push	bc
		ld	e, b		;Anz. zu kopierender Bytes
		CALL	COPY_DIR
;element renamed, move to next
		CALL	SEARCHN
		Jr	RENAME0
;
loc_DC04:	pop	bc
		ret

unk_DC06:	db	32 dup(0E5h)	;empty fcb

INDICATORS:
;set file indicators for current fcb
		LD	C,EXTNUM
		CALL	SEARCH		;through file type
INDIC0:
		CALL	END_OF_DIR
		RET	Z		;stop at end of dir
;not end of directory, continue to change
		LD	C,0
		LD	E,EXTNUM	;copy name
		CALL	COPY_DIR
		CALL	SEARCHN
		Jr	INDIC0
;
OPEN:
;search for the directory entry, copy to fcb
		LD	C,NAMLEN
		CALL	SEARCH
		CALL	END_OF_DIR
		RET	Z		;return with lret=255 if end
;not end of directory, copy fcb information
OPEN_COPY:
;(referenced below to copy fcb info)
		CALL	GETEXTA
		LD	A,(HL)
		PUSH	AF
		PUSH	HL		;save extent#
		CALL	GETDPTRA
		EX	DE,HL		;DE = .buff(dptr)
		LD	HL,(INFO)	;HL=.fcb(0)
		LD	C,NXTREC	;length of move operation
		PUSH	DE		;save .buff(dptr)
		CALL	MOVE		;from .buff(dptr) to .fcb(0)
;note that entire fcb is copied, including indicators
		CALL	SETFWF		;sets file write flag
		POP	DE
		LD	HL,EXTNUM
		ADD	HL,DE		;HL=.buff(dptr+extnum)
		LD	C,(HL)		;C = directory extent number
		LD	HL,RECCNT
		ADD	HL,DE		;HL=.buff(dptr+reccnt)
		LD	B,(HL)		;B holds directory record count
		POP	HL
		POP	AF
		LD	(HL),A		;restore extent number
;HL = .user extent#, B = dir rec cnt, C = dir extent#
;if user ext < dir ext then user := 128 records
;if user ext = dir ext then user := dir records
;if user ext > dir ext then user := 0 records
		LD	A,C
		CP	(HL)
		LD	A,B		;ready dir reccnt
		Jr	Z,OPEN_RCNT	;if same, user gets dir reccnt
		LD	A,0
		Jr	C,OPEN_RCNT	;user is larger
		LD	A,128		;directory is larger
OPEN_RCNT:	;A has record count to fill
		LD	HL,(INFO)
		LD	DE,RECCNT
		ADD	HL,DE
		LD	(HL),A
		RET
;
MERGEZERO:
;HL = .fcb1(i), DE = .fcb2(i),
;if fcb1(i) = 0 then fcb1(i) := fcb2(i)
		LD	A,(HL)
		INC	HL
		OR	(HL)
		DEC	HL
		RET	NZ		;return if = 0000
		LD	A,(DE)
		LD	(HL),A
		INC	DE
		INC	HL		;low byte copied
		LD	A,(DE)
		LD	(HL),A
		DEC	DE
		DEC	HL		;back to input form
		RET
;
CLOSE:
;locate the directory element and re-write it
		XOR	A
		LD	(LRET),A
		LD	(DCNT),A
		LD	(DCNT+1),A;
		CALL	NOWRITE
		RET	NZ		;skip close if r/o disk
;check file write flag - 0 indicates written
		CALL	GETMODNUM	;fcb(modnum) in A
		AND	FWFMSK
		RET	NZ		;return if bit remains set
		LD	C,NAMLEN
		CALL	SEARCH		;locate file
		CALL	END_OF_DIR
		RET	Z		;return if not found
;merge the disk map at info with that at buff(dptr)
		LD	BC,DSKMAP
		CALL	GETDPTRA
		ADD	HL,BC
		EX	DE,HL		;DE is .buff(dptr+16)
		LD	HL,(INFO)
		ADD	HL,BC		;DE=.buff(dptr+16), HL=.fcb(16)
;;lo              function x,x&255
		LD	C,lo(FCBLEN-DSKMAP)	;length of single byte dm
MERGE0:
		LD	A,(SINGLE)
		OR	A
		Jr	Z,MERGED	;skip to double
;this is a single byte map
;if fcb(i) = 0 then fcb(i) = buff(i)
;if buff(i) = 0 then buff(i) = fcb(i)
;if fcb(i) <> buff(i) then error
		LD	A,(HL)
		OR	A
		LD	A,(DE)
		Jr	NZ,FCBNZERO
;fcb(i) = 0
		LD	(HL),A		;fcb(i) = buff(i)
FCBNZERO:
		OR	A
		Jr	NZ,BUFFNZERO
;buff(i) = 0
		LD	A,(HL)
		LD	(DE),A		;buff(i)=fcb(i)
BUFFNZERO:
		CP	(HL)
		Jr	NZ,MERGERR	;fcb(i) = buff(i)?
		Jr	DMSET		;if merge ok
MERGED:
;this is a double byte merge operation
		CALL	MERGEZERO	;buff = fcb if buff 0000
		EX	DE,HL
		CALL	MERGEZERO
		EX	DE,HL		;fcb = buff if fcb 0000
;they should be identical at this point
		LD	A,(DE)
		CP	(HL)
		Jr	NZ,MERGERR	;low same?
		INC	DE
		INC	HL		;to high byte
		LD	A,(DE)
		CP	(HL)
		Jr	NZ,MERGERR	;high same?
;merge operation ok for this pair
		DEC	C		;extra count for double byte
DMSET:
		INC	DE
		INC	HL		;to next byte position
		DEC	C
		Jr	NZ,MERGE0	;for more
;end of disk map merge, check record count
;DE = .buff(dptr)+32, HL = .fcb(32)
		LD	BC,-(FCBLEN-EXTNUM)
		ADD	HL,BC
		EX	DE,HL
		ADD	HL,BC
;DE = .fcb(extnum), HL = .buff(dptr+extnum)
		LD	A,(DE)		;current user extent number
;if fcb(ext) >= buff(fcb) then
;buff(ext) := fcb(ext), buff(rec) := fcb(rec)
		CP	(HL)
		Jr	C,ENDMERGE
;fcb extent number >= dir extent number
		LD	(HL),A		;buff(ext) = fcb(ext)
;update directory record count field
		LD	BC,RECCNT-EXTNUM
		ADD	HL,BC
		EX	DE,HL
		ADD	HL,BC
;DE=.buff(reccnt), HL=.fcb(reccnt)
		LD	A,(HL)
		LD	(DE),A		;buff(reccnt)=fcb(reccnt)
ENDMERGE:
		LD	A,TRUE_
		LD	(FCB_COPIED),A	;mark as copied
		JP	SEEK_COPY	;ok to "wrdir" here - 1.4 compat
;		ret				;
MERGERR:
;elements did not merge correctly
		LD	HL,LRET
		DEC	(HL)		;=255 non zero flag set
		RET
;
MAKE:
;create a new file by creating a directory entry
;then opening the file
		CALL	CHECK_WRITE	;may be write protected
		LD	HL,(INFO)
		PUSH	HL		;save fcb address, look for e5
		LD	HL,EFCB
		LD	(INFO),HL	;info = .empty
		LD	C,1
		CALL	SEARCH		;length 1 match on empty entry
		CALL	END_OF_DIR	;zero flag set if no space
		POP	HL		;recall info address
		LD	(INFO),HL	;in case we return here
		RET	Z		;return with error condition 255 if not found
		EX	DE,HL		;DE = info address
;clear the remainder of the fcb
		LD	HL,NAMLEN
		ADD	HL,DE		;HL=.fcb(namlen)
		LD	C,FCBLEN-NAMLEN	;number of bytes to fill
		XOR	A		;clear accumulator to 00 for fill
MAKE0:
		LD	(HL),A
		INC	HL
		DEC	C
		Jr	NZ,MAKE0
		LD	HL,UBYTES
		ADD	HL,DE		;HL = .fcb(ubytes)
		LD	(HL),A		;fcb(ubytes) = 0
		CALL	SETCDR		;may have extended the directory
;now copy entry to the directory
		CALL	COPY_FCB
;and set the file write flag to "1"
		JP	SETFWF
;ret
;
OPEN_REEL:
;close the current extent, and open the next one
;if possible.  RMF is TRUE_ if in read mode
		XOR	A
		LD	(FCB_COPIED),A	;set TRUE_ if actually copied
		CALL	CLOSE		;close current extent
;lret remains at enddir if we cannot open the next ext
		CALL	END_OF_DIR
		RET	Z		;return if end
;increment extent number
		LD	HL,(INFO)
		LD	BC,EXTNUM
		ADD	HL,BC		;HL=.fcb(extnum)
		LD	A,(HL)
		INC	A
		AND	MAXEXT
		LD	(HL),A		;fcb(extnum)=++1
		Jr	Z,OPEN_MOD	;move to next module if zero
;may be in the same extent group
		LD	B,A
		LD	A,(EXTMSK)
		AND	B
;if result is zero, then not in the same group
		LD	HL,FCB_COPIED	;TRUE_ if the fcb was copied to directory
		AND	(HL)		;produces a 00 in accumulator if not written
		Jr	Z,OPEN_REEL0	;go to next physical extent
;result is non zero, so we must be in same logical ext
		Jr	OPEN_REEL1	;to copy fcb information
OPEN_MOD:
;extent number overflow, go to next module
		LD	BC,MODNUM-EXTNUM
		ADD	HL,BC		;HL=.fcb(modnum)
		INC	(HL)		;fcb(modnum)=++1
;module number incremented, check for overflow
		LD	A,(HL)
		AND	MAXMOD		;mask high order bits
		Jr	Z,OPEN_R_ERR	;cannot overflow to zero
;otherwise, ok to continue with new module
OPEN_REEL0:
		LD	C,NAMLEN
		CALL	SEARCH		;next extent found?
		CALL	END_OF_DIR
		Jr	NZ,OPEN_REEL1
;end of file encountered
		LD	A,(RMF)
		INC	A		;0ffh becomes 00 if read
		Jr	Z,OPEN_R_ERR	;sets lret = 1
;try to extend the current file
		CALL	MAKE
;cannot be end of directory
		CALL	END_OF_DIR
		Jr	Z,OPEN_R_ERR	;with lret = 1
		Jr	OPEN_REEL2
OPEN_REEL1:
;not end of file, open
		CALL	OPEN_COPY
OPEN_REEL2:
		CALL	GETFCB		;set parameters
		XOR	A
		JP	STA_RET		;lret = 0	;
;			ret ;with lret = 0
OPEN_R_ERR:
;cannot move to next extent of this file
		CALL	SETLRET1	;lret = 1
		JP	SETFWF		;ensure that it will not be closed
;ret
;
SEQDISKREAD:
;sequential disk read operation
		LD	A,1
		LD	(SEQIO),A
;drop through to diskread
;
DISKREAD: ;(may enter from seqdiskread)
		LD	A,TRUE_
		LD	(RMF),A		;read mode flag = TRUE_ (open_reel)
;read the next record from the current fcb
		CALL	GETFCB		;sets parameters for the read
		LD	A,(VRECORD)
		LD	HL,RCOUNT
		CP	(HL)		;vrecord-rcount
;skip if rcount > vrecord
		Jr	C,RECORDOK
;not enough records in the extent
;record count must be 128 to continue
		CP	128		;vrecord = 128?
		Jr	NZ,DISKEOF	;skip if vrecord<>128
		CALL	OPEN_REEL	;go to next extent if so
		XOR	A
		LD	(VRECORD),A	;vrecord=00
;now check for open ok
		LD	A,(LRET)
		OR	A
		Jr	NZ,DISKEOF	;stop at eof
RECORDOK:
;arrive with fcb addressing a record to read
		CALL	INDEX
;error 2 if reading unwritten data
;(returns 1 to be compatible with 1.4)
		CALL	ALLOCATED	;arecord=0000?
		Jr	Z,DISKEOF
;record has been allocated, read it
		CALL	ATRAN		;arecord now a disk address
		CALL	SEEK		;to proper track,sector
		CALL	RDBUFF		;to dma address
		JP	SETFCB		;replace parameter
;		ret					;
DISKEOF:
		JP	SETLRET1	;lret = 1
;ret
;
SEQDISKWRITE:
;sequential disk write
		LD	A,1
		LD	(SEQIO),A
;drop through to diskwrite
;
DISKWRITE:	;(may enter here from seqdiskwrite above)
		LD	A,FALSE_
		LD	(RMF),A		;read mode flag
;write record to currently selected file
		CALL	CHECK_WRITE	;in case write protected
		LD	HL,(INFO)	;HL = .fcb(0)
		CALL	CHECK_ROFILE	;may be a read-only file
		CALL	GETFCB		;to set local parameters
		LD	A,(VRECORD)
		CP	LSTREC+1	;vrecord-128
;skip if vrecord > lstrec
;vrecord = 128, cannot open next extent
		JP	NC,SETLRET1	;lret=1		;
DISKWR0:
;can write the next record, so continue
		CALL	INDEX
		CALL	ALLOCATED
		LD	C,0		;marked as normal write operation for wrbuff
		Jr	NZ,DISKWR1
;not allocated
;the argument to getblock is the starting
;position for the disk search, and should be
;the last allocated block for this file, or
;the value 0 if no space has been allocated
		CALL	DM_POSITION
		LD	(DMINX),A	;save for later
		LD	BC,0000H	;may use block zero
		OR	A
		Jr	Z,NOPBLOCK	;skip if no previous block
;previous block exists at A
		LD	C,A
		DEC	BC		;previous block # in BC
		CALL	GETDM		;previous block # to HL
		LD	B,H
		LD	C,L		;BC=prev block#
NOPBLOCK:
;BC = 0000, or previous block #
		CALL	GET_BLOCK	;block # to HL
;arrive here with block# or zero
		LD	A,L
		OR	H
		Jr	NZ,BLOCKOK
;cannot find a block to allocate
		LD	A,2
		JP	STA_RET		;lret=2
BLOCKOK:
;allocated block number is in HL
		LD	(ARECORD),HL
		EX	DE,HL		;block number to DE
		LD	HL,(INFO)
		LD	BC,DSKMAP
		ADD	HL,BC		;HL=.fcb(dskmap)
		LD	A,(SINGLE)
		OR	A		;set flags for single byte dm
		LD	A,(DMINX)	;recall dm index
		Jr	Z,ALLOCWD	;skip if allocating word
;allocating a byte value
		CALL	ADDH
		LD	(HL),E		;single byte alloc
		Jr	DISKWRU		;to continue
ALLOCWD:
;allocate a word value
		LD	C,A
		LD	B,0		;double(dminx)
		ADD	HL,BC
		ADD	HL,BC		;HL=.fcb(dminx*2)
		LD	(HL),E
		INC	HL
		LD	(HL),D		;double wd
DISKWRU:
;disk write to previously unallocated block
		LD	C,2		;marked as unallocated write
DISKWR1:
;continue the write operation of no allocation error
;C = 0 if normal write, 2 if to prev unalloc block
		LD	A,(LRET)
		OR	A
		RET	NZ		;stop if non zero returned value
		PUSH	BC		;save write flag
		CALL	ATRAN		;arecord set
		LD	A,(SEQIO)
		DEC	A
		DEC	A
		Jr	NZ,DISKWR11;
		POP	BC
		PUSH	BC
		LD	A,C
		DEC	A
		DEC	A;
		Jr	NZ,DISKWR11	;old allocation
		PUSH	HL		;arecord in hl ret from atran
		LD	HL,(BUFFA)
		LD	D,A		;zero buffa & fill
FILL0:		LD	(HL),A
		INC	HL
		INC	D
		JP	P,FILL0;
		CALL	SETDIR
		LD	HL,(ARECORD1);
		LD	C,2;
FILL1:		LD	(ARECORD),HL
		PUSH	BC
		CALL	SEEK
		POP	BC;
		CALL	WRBUFF		;write fill record	;
		LD	HL,(ARECORD)
;restore last record
		LD	C,0		;change  allocate flag
		LD	A,(BLKMSK)
		LD	B,A
		AND	L
		CP	B
		INC	HL;
		Jr	NZ,FILL1	;cont until cluster is zeroed
		POP	HL
		LD	(ARECORD),HL
		CALL	SETDATA
DISKWR11:	;
		CALL	SEEK		;to proper file position
		POP	BC
		PUSH	BC		;restore/save write flag (C=2 if new block)
		CALL	WRBUFF		;written to disk
		POP	BC		;C = 2 if a new block was allocated, 0 if not
;increment record count if rcount<=vrecord
		LD	A,(VRECORD)
		LD	HL,RCOUNT
		CP	(HL)		;vrecord-rcount
		Jr	C,DISKWR2
;rcount <= vrecord
		LD	(HL),A
		INC	(HL)		;rcount = vrecord+1
		LD	C,2		;mark as record count incremented
DISKWR2:
;A has vrecord, C=2 if new block or new record#
;patch for blocking/deblocking error
		nop
		nop
		ld	hl, 0d800h	;value will not used
;orig
;		DEC	C
;		DEC	C
;		JP	NZ,NOUPDATE
		PUSH	AF		;save vrecord value
		CALL	GETMODNUM	;HL=.fcb(modnum), A=fcb(modnum)
;reset the file write flag to mark as written fcb
		AND	A,(~FWFMSK) & 0FFH	;bit reset
		LD	(HL),A		;fcb(modnum) = fcb(modnum) and 7fh
		POP	AF		;restore vrecord
NOUPDATE:
;check for end of extent, if found attempt to open
;next extent in preparation for next write
		CP	LSTREC		;vrecord=lstrec?
		Jr	NZ,DISKWR3	;skip if not
;may be random access write, if so we are done
;change next
		LD	A,(SEQIO)
		CP	1
		Jr	NZ,DISKWR3	;skip next extent open op
;update current fcb before going to next extent
		CALL	SETFCB
		CALL	OPEN_REEL	;rmf=FALSE_
;vrecord remains at lstrec causing eof if
;no more directory space is available
		LD	HL,LRET
		LD	A,(HL)
		OR	A
		Jr	NZ,NOSPACE
;space available, set vrecord=255
		DEC	A
		LD	(VRECORD),A	;goes to 00 next time
NOSPACE:
		LD	(HL),0		;lret = 00 for returned value
DISKWR3:
		JP	SETFCB		;replace parameters
;ret
;
RSEEK:
;random access seek operation, C=0ffh if read mode
;fcb is assumed to address an active file control block
;(modnum has been set to 1100_0000b if previous bad seek)
		XOR	A
		LD	(SEQIO),A	;marked as random access operation
RSEEK1:
		PUSH	BC		;save r/w flag
		LD	HL,(INFO)
		EX	DE,HL		;DE will hold base of fcb
		LD	HL,RANREC
		ADD	HL,DE		;HL=.fcb(ranrec)
		LD	A,(HL)
		AND	7FH
		PUSH	AF		;record number
		LD	A,(HL)
		RLA	;cy=lsb of extent#
		INC	HL
		LD	A,(HL)
		RLA
		AND	11111B		;A=ext#
		LD	C,A		;C holds extent number, record stacked
		LD	A,(HL)
		RRA
		RRA
		RRA
		RRA
		AND	1111B		;mod#
		LD	B,A		;B holds module#, C holds ext#
		POP	AF		;recall sought record #
;check to insure that high byte of ran rec = 00
		INC	HL
		LD	L,(HL)		;l=high byte (must be 00)
		INC	L
		DEC	L
		LD	L,6		;zero flag, l=6
;produce error 6, seek past physical eod
		Jr	NZ,SEEKERR
;otherwise, high byte = 0, A = sought record
		LD	HL,NXTREC
		ADD	HL,DE		;HL = .fcb(nxtrec)
		LD	(HL),A		;sought rec# stored away
;arrive here with B=mod#, C=ext#, DE=.fcb, rec stored
;the r/w flag is still stacked.  compare fcb values
		LD	HL,EXTNUM
		ADD	HL,DE
		LD	A,C		;A=seek ext#
		SUB	(HL)
		Jr	NZ,RANCLOSE	;tests for = extents
;extents match, check mod#
		LD	HL,MODNUM
		ADD	HL,DE
		LD	A,B		;B=seek mod#
;could be overflow at eof, producing module#
;of 90H or 10H, so compare all but fwf
		SUB	(HL)
		AND	7FH
		Jr	Z,SEEKOK	;same?
RANCLOSE:
		PUSH	BC
		PUSH	DE		;save seek mod#,ext#, .fcb
		CALL	CLOSE		;current extent closed
		POP	DE
		POP	BC		;recall parameters and fill
		LD	L,3		;cannot close error #3
		LD	A,(LRET)
		INC	A
		Jr	Z,BADSEEK
		LD	HL,EXTNUM
		ADD	HL,DE
		LD	(HL),C		;fcb(extnum)=ext#
		LD	HL,MODNUM
		ADD	HL,DE
		LD	(HL),B		;fcb(modnum)=mod#
		CALL	OPEN		;is the file present?
		LD	A,(LRET)
		INC	A
		Jr	NZ,SEEKOK	;open successful?
;cannot open the file, read mode?
		POP	BC		;r/w flag to c (=0ffh if read)
		PUSH	BC		;everyone expects this item stacked
		LD	L,4		;seek to unwritten extent #4
		INC	C		;becomes 00 if read operation
		Jr	Z,BADSEEK	;skip to error if read operation
;write operation, make new extent
		CALL	MAKE
		LD	L,5		;cannot create new extent #5
		LD	A,(LRET)
		INC	A
		Jr	Z,BADSEEK	;no dir space
;file make operation successful
SEEKOK:
		POP	BC		;discard r/w flag
		XOR	A
		JP	STA_RET		;with zero set
BADSEEK:
;fcb no longer contains a valid fcb, mark
;with 1100_000b in modnum field so that it
;appears as overflow with file write flag set
		PUSH	HL		;save error flag
		CALL	GETMODNUM	;HL = .modnum
		LD	(HL),11000000B
		POP	HL		;and drop through
SEEKERR:
		POP	BC		;discard r/w flag
		LD	A,L
		LD	(LRET),A	;lret=#, nonzero
;setfwf returns non-zero accumulator for err
		JP	SETFWF		;flag set, so subsequent close ok
;ret
;
RANDISKREAD:
;random disk read operation
		LD	C,TRUE_		;marked as read operation
		CALL	RSEEK
		CALL	Z,DISKREAD	;if seek successful
		RET
;
RANDISKWRITE:
;random disk write operation
		LD	C,FALSE_	;marked as write operation
		CALL	RSEEK
		CALL	Z,DISKWRITE	;if seek successful
		RET
;
COMPUTE_RR:
;compute random record position for getfilesize/setrandom
		EX	DE,HL
		ADD	HL,DE
;DE=.buf(dptr) or .fcb(0), HL = .f(nxtrec/reccnt)
		LD	C,(HL)
		LD	B,0		;BC = 0000 0000 ?rrr rrrr
		LD	HL,EXTNUM
		ADD	HL,DE
		LD	A,(HL)
		RRCA
		AND	80H		;A=e000 0000
		ADD	A,C
		LD	C,A
		LD	A,0
		ADC	A,B
		LD	B,A
;BC = 0000 000? errrr rrrr
		LD	A,(HL)
		RRCA
		AND	0FH
		ADD	A,B
		LD	B,A
;BC = 000? eeee errrr rrrr
		LD	HL,MODNUM
		ADD	HL,DE
		LD	A,(HL)		;A=XXX? mmmm
		ADD	A,A
		ADD	A,A
		ADD	A,A
		ADD	A,A		;cy=? A=mmmm 0000
		PUSH	AF
		ADD	A,B
		LD	B,A
;cy=?, BC = mmmm eeee errr rrrr
		PUSH	AF		;possible second carry
		POP	HL		;cy = lsb of L
		LD	A,L		;cy = lsb of A
		POP	HL		;cy = lsb of L
		OR	L		;cy/cy = lsb of A
		AND	1		;A = 0000 000? possible carry-out
		RET
;
GETFILESIZE:
;compute logical file size for current fcb
		LD	C,EXTNUM
		CALL	SEARCH
;zero the receiving ranrec field
		LD	HL,(INFO)
		LD	DE,RANREC
		ADD	HL,DE
		PUSH	HL		;save position
		LD	(HL),D
		INC	HL
		LD	(HL),D
		INC	HL
		LD	(HL),D		;=00 00 00
GETSIZE:
		CALL	END_OF_DIR
		Jr	Z,SETSIZE
;current fcb addressed by dptr
		CALL	GETDPTRA
		LD	DE,RECCNT	;ready for compute size
		CALL	COMPUTE_RR
;A=0000 000? BC = mmmm eeee errr rrrr
;compare with memory, larger?
		POP	HL
		PUSH	HL		;recall, replace .fcb(ranrec)
		LD	E,A		;save cy
		LD	A,C
		SUB	(HL)
		INC	HL		;ls byte
		LD	A,B
		SBC	A,(HL)
		INC	HL		;middle byte
		LD	A,E
		SBC	A,(HL)		;carry if .fcb(ranrec) > directory
		Jr	C,GETNEXTSIZE	;for another try
;fcb is less or equal, fill from directory
		LD	(HL),E
		DEC	HL
		LD	(HL),B
		DEC	HL
		LD	(HL),C
GETNEXTSIZE:
		CALL	SEARCHN
		Jr	GETSIZE
SETSIZE:
		POP	HL		;discard .fcb(ranrec)
		RET
;
SETRANDOM:
;set random record from the current file control block
		LD	HL,(INFO)
		LD	DE,NXTREC	;ready params for computesize
		CALL	COMPUTE_RR	;DE=info, A=cy, BC=mmmm eeee errr rrrr
		LD	HL,RANREC
		ADD	HL,DE		;HL = .fcb(ranrec)
		LD	(HL),C
		INC	HL
		LD	(HL),B
		INC	HL
		LD	(HL),A		;to ranrec
		RET
;
SELECT:
;select disk info for subsequent input or output ops
		LD	HL,(DLOG)
		LD	A,(CURDSK)
		LD	C,A
		CALL	HLROTR
		PUSH	HL
		EX	DE,HL		;save it for test below, send to seldsk
		CALL	SELECTDISK
		POP	HL		;recall dlog vector
		CALL	Z,SEL_ERROR	;returns TRUE_ if select ok
;is the disk logged in?
		LD	A,L
		RRA
		RET	C		;return if bit is set
;disk not logged in, set bit and initialize
		LD	HL,(DLOG)
		LD	C,L
		LD	B,H		;call ready
		CALL	SET_CDISK
		LD	(DLOG),HL	;dlog=set_cdisk(dlog)
		JP	INITIALIZE
;ret
;
func14x:	ld	a, 0FFh
		ld	(olddsk), a

CURSELECT:
		LD	A,(LINFO)
		LD	HL,CURDSK
		CP	(HL)
		RET	Z		;skip if linfo=curdsk
		LD	(HL),A		;curdsk=info
		Jr	SELECT
;ret
;
RESELECT:
;check current fcb to see if reselection necessary
		LD	A,TRUE_
		LD	(RESEL),A	;mark possible reselect
		LD	HL,(INFO)
		LD	A,(HL)		;drive select code
		AND	11111B		;non zero is auto drive select
		DEC	A		;drive code normalized to 0..30, or 255
		CP	30
		jp	c, loc_E034
		ld	hl, olddsk
		ld	a, (hl)
		inc	a
		jr	z, noselect
		ld	(hl), 0FFh
		dec	a
		ld	(linfo), a
		call	curselect
		jr	noselect

loc_E034:	ld	(linfo), a
		ld	a, (olddsk)
		inc	a
		jr	nz, loc_E043
;auto select function, save curdsk
		LD	A,(CURDSK)
		LD	(OLDDSK),A	;olddsk=curdsk
loc_E043:	LD	A,(HL)
		LD	(FCBDSK),A	;save drive code
		AND	11100000B
		LD	(HL),A		;preserve hi bits
		CALL	CURSELECT
NOSELECT:
;set user code
		LD	A,(USRCODE)	;0...31
		LD	HL,(INFO)
		OR	(HL)
		LD	(HL),A
		RET
;
;	individual function handlers
FUNC12:
;return version number
		LD	A,DVERS
		JP	STA_RET		;lret = dvers (high = 00)
;	ret ;jmp goback
;
FUNC13:
;reset disk system - initialize to disk 0
		ld	a, 0FFh
		ld	(olddsk), a
		LD	HL,0
		LD	(RODSK),HL
		LD	(DLOG),HL
		ld	hl, DISKO
		ld	a, (hl)
		cp	0FFh
		jr	nz, FUNC13a
		ld	a, (DISKA)	;current disk
		and	0Fh
		ld	(hl), a
FUNC13a:	LD	(CURDSK),A	;note that usrcode remains unchanged
		ld	(aret+1), a
		LD	HL,TBUFF
		LD	(DMAAD),HL	;dmaad = tbuff
		CALL	SETDATA		;to data dma address
		JP	SELECT
;ret ;jmp goback
;
FUNC14		EQU	FUNC14x;
;select disk info
;ret ;jmp goback
;
FUNC15:
;open file
		CALL	CLRMODNUM	;clear the module number
		CALL	RESELECT
		JP	OPEN
;ret ;jmp goback
;
FUNC16:
;close file
		CALL	RESELECT
		JP	CLOSE
;ret ;jmp goback
;
FUNC17:
;search for first occurrence of a file
		LD	C,0		;length assuming '?' TRUE_
		EX	DE,HL		;was lhld info
		LD	A,(HL)
		CP	'?'		;no reselect if ?
		Jr	Z,QSELECT	;skip reselect if so
;normal search
		CALL	GETEXTA
		LD	A,(HL)
		CP	'?'	;
		CALL	NZ,CLRMODNUM	;module number zeroed
		CALL	RESELECT
		LD	C,NAMLEN
QSELECT:
		CALL	SEARCH
		JP	DIR_TO_USER	;copy directory entry to user
;ret ;jmp goback
;
FUNC18:
;search for next occurrence of a file name
		LD	HL,(SEARCHA)
		LD	(INFO),HL
		CALL	RESELECT
		CALL	SEARCHN
		JP	DIR_TO_USER	;copy directory entry to user
;ret ;jmp goback
;
FUNC19:
;delete a file
		CALL	RESELECT
		CALL	DELETE
		JP	COPY_DIRLOC
;ret ;jmp goback
;
FUNC20:
;read a file
		CALL	RESELECT
		JP	SEQDISKREAD;
;jmp goback
;
FUNC21:
;write a file
		CALL	RESELECT
		JP	SEQDISKWRITE;
;jmp goback
;
FUNC22:
;make a file
		CALL	CLRMODNUM
		CALL	RESELECT
		JP	MAKE
;ret ;jmp goback
;
FUNC23:
;rename a file
		CALL	RESELECT
		CALL	RENAME
		JP	COPY_DIRLOC
;ret ;jmp goback
;
FUNC24:
;return the login vector
		LD	HL,(DLOG)
		JP	STHL_RET;
;	ret ;jmp goback
;
FUNC25:
;return selected disk number
		LD	A,(olddsk)
		inc	a
		jr	z, loc_E0FA
		dec	a
		JP	STA_RET;
;	ret ;jmp goback
loc_E0FA:	ld	a, (curdsk)
		jp	sta_ret

;
FUNC26:
;set the subsequent dma address to info
		EX	DE,HL		;was lhld info
		LD	(DMAAD),HL	;dmaad = info
		JP	SETDATA		;to data dma address
;ret ;jmp goback
;
FUNC27:
;return the login vector address
		LD	HL,(ALLOCA)
		JP	STHL_RET;
;	ret ;jmp goback
;
FUNC28		EQU	SET_RO;
;write protect current disk
;ret ;jmp goback
;
FUNC29:
;return r/o bit vector
		LD	HL,(RODSK)
		JP	STHL_RET;
;	ret ;jmp goback
;
FUNC30:
;set file indicators
		CALL	RESELECT
		CALL	INDICATORS
		JP	COPY_DIRLOC	;lret=dirloc
;ret ;jmp goback
;
FUNC31:
;return address of disk parameter block
		LD	HL,(DPBADDR)
STHL_RET:
		LD	(ARET),HL
		RET			;jmp goback
FUNC32:
;set user code
		LD	A,(LINFO)
		CP	0FFH
		Jr	NZ,SETUSRCODE
;interrogate user code instead
		LD	A,(USRCODE)
		JP	STA_RET		;lret=usrcode
;		ret ;jmp goback
SETUSRCODE:
		AND	1FH
		LD	(USRCODE),A
		RET	;jmp goback
;
FUNC33:
;random disk read operation
		CALL	RESELECT
		JP	RANDISKREAD	;to perform the disk read
;ret ;jmp goback
;
FUNC34:
;random disk write operation
		CALL	RESELECT
		JP	RANDISKWRITE	;to perform the disk write
;ret ;jmp goback
;
FUNC35:
;return file size (0-65536)
		CALL	RESELECT
		JP	GETFILESIZE
;ret ;jmp goback
;
FUNC36		EQU	SETRANDOM;
;set random record
;ret ;jmp goback
FUNC37:
;
		LD	HL,(INFO)
		LD	A,L
		CPL
		LD	E,A
		LD	A,H
		CPL
		LD	HL,(DLOG)
		AND	H
		LD	D,A
		LD	A,L
		AND	E
		LD	E,A
		LD	HL,(RODSK)
		EX	DE,HL
		LD	(DLOG),HL
		LD	A,L
		AND	E
		LD	L,A
		LD	A,H
		AND	D
		LD	H,A
		LD	(RODSK),HL
		RET
;
;
GOBACK:
;arrive here at end of processing to return to user
		LD	A,(RESEL)
		OR	A
		Jr	Z,RETMON
;reselection may have taken place
		LD	HL,(INFO)
		LD	A,(FCBDSK)
		LD	(HL),A		;fcb(0)=disk
;
;	return from the disk monitor
RETMON:		pop	iy
		pop	ix
		LD	HL,(ENTSP)
		LD	SP,HL		;user stack restored
		LD	HL,(ARET)
		LD	A,L
		LD	B,H		;BA = HL = aret
		RET
FUNC38		EQU	FUNC_RET
FUNC39		EQU	FUNC_RET
FUNC40:
;random disk write with zero fill of unallocated block
		CALL	RESELECT
		LD	A,2
		LD	(SEQIO),A
		LD	C,FALSE_
		CALL	RSEEK1
		CALL	Z,DISKWRITE	;if seek successful
		RET

		db 0FEh
		db 0FEh
		db 0FEh
		db 0FEh
		db 0FEh
		db 0FEh
		db 0FEh
		db 0FEh
		db 0FEh
		db 0FEh
		db 0FEh
		db 0FEh
		db 0FEh
		db 0FEh
		db 0FEh
		db 0FEh
		db 0FEh
		db 0FEh
		db 0FEh
		db 0FEh
		db 0FEh

;		org	F5ACh
;
;	data areas
;
;	initialized data
EFCB:		DB	EMPTY		;0e5=available dir entry
RODSK:		DW	0		;read only disk vector
DLOG:		DW	0		;logged-in disks
DMAAD:		DW	TBUFF		;initial dma address
;
;	curtrka - alloca are set upon disk select
;	(data must be adjacent, do not insert variables)
;	(address of translate vector, not used)
CDRMAXA:	DS	WORD		;pointer to cur dir max value
CURTRKA:	DS	WORD		;current track address
CURRECA:	DS	WORD		;current record address
BUFFA:		DS	WORD		;pointer to directory dma address
DPBADDR:	DS	WORD		;current disk parameter block address
CHECKA:		DS	WORD		;current checksum vector address
ALLOCA:		DS	WORD		;current allocation vector address
ADDLIST		EQU	$-BUFFA		;address list size
;
;	sectpt - offset obtained from disk parm block at dpbaddr
;	(data must be adjacent, do not insert variables)
SECTPT:		DS	WORD		;sectors per track
BLKSHF:		DS	BYTE		;block shift factor
BLKMSK:		DS	BYTE		;block mask
EXTMSK:		DS	BYTE		;extent mask
MAXALL:		DS	WORD		;maximum allocation number
DIRMAX:		DS	WORD		;largest directory number
DIRBLK:		DS	WORD		;reserved allocation bits for directory
CHKSIZ:		DS	WORD		;size of checksum vector
OFFSET:		DS	WORD		;offset tracks at beginning
DPBLIST		EQU	$-SECTPT	;size of area
;
;	local variables
TRANV:		DS	WORD		;address of translate vector
FCB_COPIED:
		DS	BYTE		;set TRUE_ if copy_fcb called
RMF:		DS	BYTE		;read mode flag for open_reel
DIRLOC:		DS	BYTE		;directory flag in rename, etc.
SEQIO:		DS	BYTE		;1 if sequential i/o
LINFO:		DS	BYTE		;low(info)
DMINX:		DS	BYTE		;local for diskwrite
SEARCHL:	DS	BYTE		;search length
SEARCHA:	DS	WORD		;search address
TINFO:		DS	WORD		;temp for info in "make"
SINGLE:		DS	BYTE		;set TRUE_ if single byte allocation map
RESEL:		DS	BYTE		;reselection flag
OLDDSK:		DS	BYTE		;disk on entry to bdos
FCBDSK:		DS	BYTE		;disk named in fcb
RCOUNT:		DS	BYTE		;record count in current fcb
EXTVAL:		DS	BYTE		;extent number and extmsk
VRECORD:	DS	WORD		;current virtual record
ARECORD:	DS	WORD		;current actual record
ARECORD1:	DS	WORD		;current actual block# * blkmsk
;
;	local variables for directory access
DPTR:		DS	BYTE		;directory pointer 0,1,2,3
DCNT:		DS	WORD		;directory counter 0,1,...,dirmax
DREC:		DS	WORD		;directory record 0,1,...,dirmax/4
DISKO:		db 0FFh			;initial disk on INIT, if <> FFh
		db 0FEh
		db 0FEh
		db 0FEh
		db 0FEh
		db 0FEh
		db 0FEh
		db 0FEh
		db 0FEh
		db 0FEh
		db 0FEh
		db 0FEh
		db 0FEh
		db 0FEh
		db 0FEh
		db 0FEh
		db 0FEh
		db 0FEh

		db	0E00h-($-loc_0) dup (0FFh)
;
;BIOS:		EQU	($ & 0FF00H)+100H	;next module
BIOS:		EQU	$
;	bios access constants
;bios		equ	$+0E00h
BOOTF		equ	BIOS+3*0	;cold boot function
WBOOTF		equ	BIOS+3*1	;warm boot function
CONSTF		equ	BIOS+3*2	;console status function
CONINF		equ	BIOS+3*3	;console input function
CONOUTF		equ	BIOS+3*4	;console output function
LISTF		equ	BIOS+3*5	;list output function
PUNCHF		equ	BIOS+3*6	;punch output function
READERF		equ	BIOS+3*7	;reader input function
HOMEF		equ	BIOS+3*8	;disk home function
SELDSKF		equ	BIOS+3*9	;select disk function
SETTRKF		equ	BIOS+3*10	;set track function
SETSECF		equ	BIOS+3*11	;set sector function
SETDMAF		equ	BIOS+3*12	;set dma function
READF		equ	BIOS+3*13	;read disk function
WRITEF		equ	BIOS+3*14	;write disk function
LISTSTF		equ	BIOS+3*15	;list status function
SECTRAN		equ	BIOS+3*16	;sector translate


;;	END
