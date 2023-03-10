		cpu	Z80

;		TITLE	'console command processor (CCP), ver 2.0'
;	assembly language version of the CP/M console command processor
;
;	version 2.2 February, 1980
;
;	Copyright (c) 1976, 1977, 1978, 1979, 1980
;	Digital Research
;	Box 579, Pacific Grove,
;	California, 93950
;
; für AS-Assembler und Z80-Menemonics V.Pohlers 08.12.2011

;FALSE		EQU	0000H
;TRUE		EQU	NOT FALSE
TESTING		EQU	FALSE		;true if debugging
;
;
;;		IF	TESTING
;;		ORG	3400H
BDOSL		EQU	$+800H		;bdos location
;;		ELSE
;;		ORG	000H
;BDOSL		EQU	$+800H		;bdos location
;;		ENDIF
TRAN		EQU	100H
TRANM		EQU	$
CCPLOC		EQU	$
;
;	********************************************************
;	*	Base of CCP contains the following code/data   *
;	*	ccp:	jmp ccpstart	(start with command)   *
;	*		jmp ccpclear    (start, clear command) *
;	*	ccp+6	127		(max command length)   *
;	*	ccp+7	comlen		(command length = 00)  *
;	*	ccp+8	' ... '		(16 blanks)	       *
;	********************************************************
;	* Normal entry is at ccp, where the command line given *
;	* at ccp+8 is executed automatically (normally a null  *
;	* command with comlen = 00).  An initializing program  *
;	* can be automatically loaded by storing the command   *
;	* at ccp+8, with the command length at ccp+7.  In this *
;	* case, the ccp executes the command before prompting  *
;	* the console for input.  Note that the command is exe-*
;	* cuted on both warm and cold starts.  When the command*
;	* line is initialized, a jump to "jmp ccpclear" dis-   *
;	* ables the automatic command execution.               *
;	********************************************************
;
		JP	CCPSTART	;start ccp with possible initial command
		JP	CCPCLEAR	;clear the command buffer
MAXLEN:		DB	127		;max buffer length
COMLEN:		DB	4		;command length (filled in by dos)
;	(command executed initially if comlen non zero)
COMBUF:
		DB	"DIR",0		;character fill
		DS	128-($-COMBUF)
		
;	total buffer length is 128 characters
COMADDR:	DW	COMBUF		;address of next to char to scan
STADDR:		DS	2		;starting address of current fillfcb request
;                               	
;;DISKA		EQU	0004H		;disk address for current disk
BDOS		EQU	0005H		;primary bdos entry point
BUFF		EQU	0080H		;default buffer
FCB		EQU	005CH		;default file control block
;                               	
RCHARF		EQU	1		;read character function
PCHARF		EQU	2		;print character function
PBUFF		EQU	9		;print buffer function
RBUFF		EQU	10		;read buffer function
BREAKF		EQU	11		;break key function
LIFTF		EQU	12		;lift head function (no operation)
INITF		EQU	13		;initialize bdos function
SELF		EQU	14		;select disk function
OPENF		EQU	15		;open file function
CLOSEF		EQU	16		;close file function
SEARF		EQU	17		;search for file function
SEARNF		EQU	18		;search for next file function
DELF		EQU	19		;delete file function
DREADF		EQU	20		;disk read function
DWRITF		EQU	21		;disk write function
MAKEF		EQU	22		;file make function
RENF		EQU	23		;rename file function
LOGF		EQU	24		;return login vector
CSELF		EQU	25		;return currently selected drive number
DMAF		EQU	26		;set dma address
USERF		EQU	32		;set user number
;                               	
;	special fcb flags       	
ROFILE		EQU	9		;read only file
SYSFILE		EQU	10		;system file flag
;                               	
;	special characters      	
CR		EQU	13		;carriage return
LF		EQU	10		;line feed
LA		EQU	5FH		;left arrow
EOFILE		EQU	1AH		;end of file
;
;	utility procedures
PRINTCHAR:
		LD	E,A
		LD	C,PCHARF
		JP	BDOS
;
PRINTBC:
;print character, but save b,c registers
		PUSH	BC
		CALL	PRINTCHAR
		POP	BC
		RET
;
CRLF:
		LD	A,CR
		CALL	PRINTBC
		LD	A,LF
		Jr	PRINTBC
;
BLANK:
		LD	A,' '
		Jr	PRINTBC
;
PRINT:		;print string starting at b,c until next 00 entry
		PUSH	BC
		CALL	CRLF
		POP	HL		;now print the string
PRIN0:		LD	A,(HL)  	
		OR	A       	
		RET	Z		;stop on 00
		INC	HL      	
		PUSH	HL		;ready for next
		CALL	PRINTCHAR
		POP	HL		;character printed
		Jr	PRIN0		;for another character
;
INITIALIZE:
		LD	C,INITF
		JP	BDOS
;
SELECT:
		LD	E,A
		LD	C,SELF
		JP	BDOS
;
BDOS_INR:
		CALL	BDOS
		LD	(DCNT),A
		INC	A
		RET
;
OPEN:		;open the file given by d,e
		LD	C,OPENF
		Jr	BDOS_INR
;
OPENC:		;open comfcb
		XOR	A
		LD	(COMREC),A	;clear next record to read
		LD	DE,COMFCB
		Jr	OPEN
;
CLOSE:		;close the file given by d,e
		LD	C,CLOSEF
		Jr	BDOS_INR
;
SEARCH:		;search for the file given by d,e
		LD	C,SEARF
		Jr	BDOS_INR
;
SEARCHN:
;search for the next occurrence of the file given by d,e
		LD	C,SEARNF
		Jr	BDOS_INR
;
SEARCHCOM:
;search for comfcb file
		LD	DE,COMFCB
		JP	SEARCH
;
DELETE:		;delete the file given by d,e
		LD	C,DELF
		JP	BDOS
;
BDOS_COND:
		CALL	BDOS
		OR	A
		RET
;
DISKREAD:
;read the next record from the file given by d,e
		LD	C,DREADF
		JP	BDOS_COND
;
DISKREADC:
;read the comfcb file
		LD	DE,COMFCB
		JP	DISKREAD
;
DISKWRITE:
;write the next record to the file given by d,e
		LD	C,DWRITF
		JP	BDOS_COND
;
MAKE:		;create the file given by d,e
		LD	C,MAKEF
		JP	BDOS_INR
;
RENAM:		;rename the file given by d,e
		LD	C,RENF
		JP	BDOS
;
GETUSER:
;return current user code in a
		LD	E,0FFH		;drop through to setuser
;
SETUSER:
		LD	C,USERF
		JP	BDOS		;sets user number
;
SAVEUSER:
;save user#/disk# before possible ^c or transient
		CALL	GETUSER		;code to a
		ADD	A,A
		ADD	A,A
		ADD	A,A
		ADD	A,A		;rot left
		LD	HL,CDISK
		OR	(HL)		;4b=user, 4b=disk
		LD	(DISKA),A	;stored away in memory for later
		RET
;
SETDISKA:
		LD	A,(CDISK)
		LD	(DISKA),A	;user/disk
		RET
;
TRANSLATE:
;translate character in register A to upper case
		CP	61H
		RET	C		;return if below lower case a
		CP	7BH
		RET	NC		;return if above lower case z
		AND	5FH
		RET			;translated to upper case
;
READCOM:
;read the next command into the command buffer
;check for submit file
		LD	A,(SUBMIT)
		OR	A
		JP	Z,NOSUB
;scanning a submit file
;change drives to open and read the file
		LD	A,(CDISK)
		OR	A
		LD	A,0
		CALL	NZ,SELECT
;have to open again in case xsub present
		LD	DE,SUBFCB
		CALL	OPEN
		JP	Z,NOSUB		;skip if no sub
		LD	A,(SUBRC)
		DEC	A		;read last record(s) first
		LD	(SUBCR),A	;current record to read
		LD	DE,SUBFCB
		CALL	DISKREAD	;end of file if last record
		JP	NZ,NOSUB
;disk read is ok, transfer to combuf
		LD	DE,COMLEN
		LD	HL,BUFF
		LD	B,128
		CALL	MOVE0
;line is transferred, close the file with a
;deleted record
		LD	HL,SUBMOD
		LD	(HL),0		;clear fwflag
		INC	HL
		DEC	(HL)		;one less record
		LD	DE,SUBFCB
		CALL	CLOSE
		JP	Z,NOSUB
;close went ok, return to original drive
		LD	A,(CDISK)
		OR	A
		CALL	NZ,SELECT
;print to the 00
		LD	HL,COMBUF
		CALL	PRIN0
		CALL	BREAK_KEY
		JP	Z,NOREAD
		CALL	DEL_SUB
		JP	CCP		;break key depressed
;
NOSUB:		;no submit file
		CALL	DEL_SUB
;translate to upper case, store zero at end
		CALL	SAVEUSER	;user # save in case control c
		LD	C,RBUFF
		LD	DE,MAXLEN
		CALL	BDOS
		CALL	SETDISKA	;no control c, so restore diska
NOREAD:		;enter here from submit file
;set the last character to zero for later scans
		LD	HL,COMLEN
		LD	B,(HL)		;length is in b
READCOM0:	INC	HL
		LD	A,B
		OR	A		;end of scan?
		JP	Z,READCOM1
		LD	A,(HL)		;get character and translate
		CALL	TRANSLATE
		LD	(HL),A
		DEC	B
		JP	READCOM0
;
READCOM1:	;end of scan, h,l address end of command
		LD	(HL),A		;store a zero
		LD	HL,COMBUF
		LD	(COMADDR),HL	;ready to scan to zero
		RET
;
BREAK_KEY:
;check for a character ready at the console
		LD	C,BREAKF
		CALL	BDOS
		OR	A
		RET	Z
		LD	C,RCHARF
		CALL	BDOS		;character cleared
		OR	A
		RET
;
CSELECT:
;get the currently selected drive number to reg-A
		LD	C,CSELF
		JP	BDOS
;
SETDMABUFF:
;set default buffer dma address
		LD	DE,BUFF	;(drop through)
;
SETDMA:
;set dma address to d,e
		LD	C,DMAF
		JP	BDOS
;
DEL_SUB:
;delete the submit file, and set submit flag to false
		LD	HL,SUBMIT
		LD	A,(HL)
		OR	A
		RET	Z		;return if no sub file
		LD	(HL),0		;submit flag is set to false
		XOR	A	
		CALL	SELECT		;on drive a to erase file
		LD	DE,SUBFCB
		CALL	DELETE
		LD	A,(CDISK)
		JP	SELECT		;back to original drive
;
SERIALIZE:
;check serialization
		LD	DE,SERIAL
		LD	HL,BDOSL
		LD	B,6		;check six bytes
SER0:		LD	A,(DE)
		CP	(HL)
		JP	NZ,BADSERIAL
		INC	DE
		INC	HL
		DEC	B
		JP	NZ,SER0
		RET	;serial number is ok
;
COMERR:
;error in command string starting at position
;'staddr' and ending with first delimiter
		CALL	CRLF		;space to next line
		LD	HL,(STADDR)	;h,l address first to print
COMERR0:	;print characters until blank or zero
		LD	A,(HL)
		CP	' '
		JP	Z,COMERR1	; not blank
		OR	A
		JP	Z,COMERR1	; not zero, so print it
		PUSH	HL
		CALL	PRINTCHAR
		POP	HL
		INC	HL
		JP	COMERR0		; for another character
COMERR1:	;print question mark,and delete sub file
		LD	A,'?'
		CALL	PRINTCHAR
		CALL	CRLF
		CALL	DEL_SUB
		JP	CCP		;restart with next command
;
; fcb scan and fill subroutine (entry is at fillfcb below)
;fill the comfcb, indexed by A (0 or 16)
;subroutines
DELIM:		;look for a delimiter
		LD	A,(DE)
		OR	A
		RET	Z		;not the last element
		CP	' '
		JP	C,COMERR	;non graphic
		RET	Z		;treat blank as delimiter
		CP	'='
		RET	Z
		CP	LA
		RET	Z		;left arrow
		CP	'.'
		RET	Z
		CP	':'
		RET	Z
		CP	';'
		RET	Z
		CP	'<'
		RET	Z
		CP	'>'
		RET	Z
		RET			;delimiter not found
;
DEBLANK:	;deblank the input line
		LD	A,(DE)
		OR	A
		RET	Z		;treat end of line as blank
		CP	' '
		RET	NZ
		INC	DE
		JP	DEBLANK
;
ADDH:		;add a to h,l
		ADD	A,L
		LD	L,A
		RET	NC
		INC	H
		RET
;
FILLFCB0:
;equivalent to fillfcb(0)
		LD	A,0
;
FILLFCB:
		LD	HL,COMFCB
		CALL	ADDH
		PUSH	HL
		PUSH	HL		;fcb rescanned at end
		XOR	A
		LD	(SDISK),A	;clear selected disk (in case A:...)
		LD	HL,(COMADDR)
		EX	DE,HL		;command address in d,e
		CALL	DEBLANK		;to first non-blank character
		EX	DE,HL
		LD	(STADDR),HL	;in case of errors
		EX	DE,HL
		POP	HL		;d,e has command, h,l has fcb address
;look for preceding file name A: B: ...
		LD	A,(DE)
		OR	A
		JP	Z,SETCUR0	;use current disk if empty command
		SBC	A,'A'-1
		LD	B,A		;disk name held in b if : follows
		INC	DE
		LD	A,(DE)
		CP	':'
		JP	Z,SETDSK	;set disk name if :
;
SETCUR		;set current disk
		DEC	DE		;back to first character of command
SETCUR0:
		LD	A,(CDISK)
		LD	(HL),A
		JP	SETNAME
;
SETDSK		;set disk to name in register b
		LD	A,B
		LD	(SDISK),A	;mark as disk selected
		LD	(HL),B
		INC	DE		;past the :
;
SETNAME		;set the file name field
		LD	B,8		;file name length (max)
SETNAM0:	CALL	DELIM
		JP	Z,PADNAME	;not a delimiter
		INC	HL
		CP	'*'
		JP	NZ,SETNAM1	;must be ?'s
		LD	(HL),'?'
		JP	SETNAM2		;to dec count
;
SETNAM1:	LD	(HL),A		;store character to fcb
		INC	DE
SETNAM2:	DEC	B		;count down length
		JP	NZ,SETNAM0
;
;end of name, truncate remainder
TRNAME:		CALL	DELIM
		JP	Z,SETTY		;set type field if delimiter
		INC	DE
		JP	TRNAME
;
PADNAME:	INC	HL
		LD	(HL),' '
		DEC	B
		JP	NZ,PADNAME
;
SETTY		;set the type field
		LD	B,3
		CP	'.'
		JP	NZ,PADTY	;skip the type field if no .
		INC	DE		;past the ., to the file type field
SETTY0		;set the field from the command buffer
		CALL	DELIM
		JP	Z,PADTY
		INC	HL
		CP	'*'
		JP	NZ,SETTY1
		LD	(HL),'?'	;since * specified
		JP	SETTY2
;
SETTY1:		;not a *, so copy to type field
		LD	(HL),A
		INC	DE
SETTY2:		;decrement count and go again
		DEC	B
		JP	NZ,SETTY0
;
;end of type field, truncate
TRTYP:		;truncate type field
		CALL	DELIM
		JP	Z,EFILL
		INC	DE
		JP	TRTYP
;
PADTY:		;pad the type field with blanks
		INC	HL
		LD	(HL),' '
		DEC	B
		JP	NZ,PADTY
;
EFILL:		;end of the filename/filetype fill, save command address
;fill the remaining fields for the fcb
		LD	B,3
EFILL0:		INC	HL
		LD	(HL),0
		DEC	B
		JP	NZ,EFILL0
		EX	DE,HL
		LD	(COMADDR),HL	;set new starting point
;
;recover the start address of the fcb and count ?'s
		POP	HL
		LD	BC,11		;b=0, c=8+3
SCNQ:		INC	HL
		LD	A,(HL)
		CP	'?'
		JP	NZ,SCNQ0
;? found, count it in b
		INC	B
SCNQ0:		DEC	C
		JP	NZ,SCNQ
;
;number of ?'s in c, move to a and return with flags set
		LD	A,B
		OR	A
		RET
;
INTVEC:
;intrinsic function names (all are four characters)
		DB	"DIR "
		DB	"ERA "
		DB	"TYPE"
		DB	"SAVE"
		DB	"REN "
		DB	"USER"
		DB	"GO  "
		DB	"LOAD"
		DB	"EXIT"
INTLEN		EQU	($-INTVEC)/4	;intrinsic function length
SERIAL:		DB	2,8,0,8,5,5
;
;
INTRINSIC:
;look for intrinsic functions (comfcb has been filled)
		LD	HL,INTVEC
		LD	C,0		;c counts intrinsics as scanned
INTRIN0:	LD	A,C
		CP	INTLEN		;done with scan?
		RET	NC
;no, more to scan
		LD	DE,COMFCB+1	;beginning of name
		LD	B,4		;length of match is in b
INTRIN1:	LD	A,(DE)
		CP	(HL)		;match?
		Jr	NZ,INTRIN2	;skip if no match
		INC	DE
		INC	HL
		DEC	B
		Jr	NZ,INTRIN1	;loop while matching
;
;complete match on name, check for blank in fcb
		LD	A,(DE)
		CP	' '
		Jr	NZ,INTRIN3	;otherwise matched
		LD	A,C
		RET			;with intrinsic number in a
;
INTRIN2:	;mismatch, move to end of intrinsic
		INC	HL
		DEC	B
		Jr	NZ,INTRIN2
;
INTRIN3:	;try next intrinsic
		INC	C		;to next intrinsic number
		JP	INTRIN0		;for another round
;
CCPCLEAR:
;clear the command buffer
		XOR	A
		LD	(COMLEN),A
;drop through to start ccp
CCPSTART:
;enter here from boot loader
		LD	SP,STACK
		PUSH	BC		;save initial disk number
;(high order 4bits=user code, low 4bits=disk#)
		LD	A,C
		RRA
		RRA
		RRA
		RRA
		AND	0FH		;user code
		LD	E,A
		CALL	SETUSER		;user code selected
;initialize for this user, get $ flag
		CALL	INITIALIZE	;0ffh in accum if $ file present
		LD	(SUBMIT),A	;submit flag set if $ file present
		POP	BC		;recall user code and disk number
		LD	A,C
		AND	0FH		;disk number in accumulator
		LD	(CDISK),A	;clears user code nibble
		CALL	SELECT		;proper disk is selected, now check sub files
;check for initial command
		LD	A,(COMLEN)
		OR	A
		JP	NZ,CCP0		;assume typed already
;
CCP:
;enter here on each command or error condition
		LD	SP,STACK
		CALL	CRLF		;print d> prompt, where d is disk name
		CALL	CSELECT		;get current disk number
		ADD	A,'A'
		CALL	PRINTCHAR
		LD	A,'>'
		CALL	PRINTCHAR
		CALL	READCOM		;command buffer filled
CCP0:		;(enter here from initialization with command full)
		LD	DE,BUFF
		CALL	SETDMA		;default dma address at buff
		CALL	CSELECT
		LD	(CDISK),A	;current disk number saved
		CALL	FILLFCB0	;command fcb filled
		CALL	NZ,COMERR	;the name cannot be an ambiguous reference
		LD	A,(SDISK)
		OR	A
		JP	NZ,USERFUNC
;check for an intrinsic function
		CALL	INTRINSIC
		LD	HL,JMPTAB	;index is in the accumulator
		LD	E,A
		LD	D,0
		ADD	HL,DE
		ADD	HL,DE		;index in d,e
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A
		JP	(HL)
;pc changes to the proper intrinsic or user function
JMPTAB:
		DW	DIRECT		;directory search
		DW	ERASE		;file erase
		DW	TYPE		;type file
		DW	SAVE		;save memory image
		DW	RENAME		;file rename
		DW	USER		;user number
		dw GO
		dw BIOS+003Bh	; load
		dw BIOS+0038h	; exit
		DW	USERFUNC	;user-defined function

BADSERIAL:
		LD	HL,76F3h	; DI OR (HALT SHL 8)
		LD	(CCPLOC),HL
		LD	HL,CCPLOC
		JP	(HL)
;
;
;utility subroutines for intrinsic handlers
READERR:
;print the read error message
		LD	BC,RDMSG
		JP	PRINT
RDMSG:		DB	"READ ERROR",0
;
NOFILE:
;print no file message
		LD	BC,NOFMSG
		JP	PRINT
NOFMSG:		DB	"NO FILE",0
;
GETNUMBER:	;read a number from the command line
		CALL	FILLFCB0	;should be number
		LD	A,(SDISK)
		OR	A
		JP	NZ,COMERR	;cannot be prefixed
;convert the byte value in comfcb to binary
		LD	HL,COMFCB+1
		LD	BC,11		;(b=0, c=11)
;value accumulated in b, c counts name length to zero
CONV0:		LD	A,(HL)
		CP	' '
		JP	Z,CONV1
;more to scan, convert char to binary and add
		INC	HL
		SUB	'0'
		CP	10
		JP	NC,COMERR	;valid?
		LD	D,A		;save value
		LD	A,B		;mult by 10
		AND	11100000B
		JP	NZ,COMERR
		LD	A,B		;recover value
		RLCA
		RLCA
		RLCA	;*8
		ADD	A,B
		JP	C,COMERR
		ADD	A,B
		JP	C,COMERR	;*8+*2 = *10
		ADD	A,D
		JP	C,COMERR	;+digit
		LD	B,A
		DEC	C
		JP	NZ,CONV0	;for another digit
		RET
CONV1:		;end of digits, check for all blanks
		LD	A,(HL)
		CP	' '
		JP	NZ,COMERR	;blanks?
		INC	HL
		DEC	C
		JP	NZ,CONV1
		LD	A,B		;recover value
		RET
;
MOVENAME:
;move 3 characters from h,l to d,e addresses
		LD	B,3
MOVE0:		LD	A,(HL)
		LD	(DE),A
		INC	HL
		INC	DE
		DEC	B
		JP	NZ,MOVE0
		RET
;
ADDHCF:		;buff + a + c to h,l followed by fetch
		LD	HL,BUFF
		ADD	A,C
		CALL	ADDH
		LD	A,(HL)
		RET
;
SETDISK:
;change disks for this command, if requested
		XOR	A
		LD	(COMFCB),A	;clear disk name from fcb
		LD	A,(SDISK)
		OR	A
		RET	Z		;no action if not specified
		DEC	A
		LD	HL,CDISK
		CP	(HL)
		RET	Z		;already selected
		JP	SELECT
;
RESETDISK:
;return to original disk after command
		LD	A,(SDISK)
		OR	A
		RET	Z		;no action if not selected
		DEC	A
		LD	HL,CDISK
		CP	(HL)
		RET	Z		;same disk
		LD	A,(CDISK)
		JP	SELECT
;
;individual intrinsics follow
DIRECT:
;directory search
		CALL	FILLFCB0	;comfcb gets file name
		CALL	SETDISK		;change disk drives if requested
		LD	HL,COMFCB+1
		LD	A,(HL)		;may be empty request
		CP	' '
		JP	NZ,DIR1		;skip fill of ??? if not blank
;set comfcb to all ??? for current disk
		LD	B,11		;length of fill ????????.???
DIR0:		LD	(HL),'?'
		INC	HL
		DEC	B
		JP	NZ,DIR0
;not a blank request, must be in comfcb
DIR1:		LD	E,0
		PUSH	DE		;E counts directory entries
		CALL	SEARCHCOM	;first one has been found
		CALL	Z,NOFILE	;not found message
DIR2:		JP	Z,ENDIR
;found, but may be system file
		LD	A,(DCNT)	;get the location of the element
		RRCA
		RRCA
		RRCA
		AND	1100000B
		LD	C,A
;c contains base index into buff for dir entry
		LD	A,SYSFILE
		CALL	ADDHCF		;value to A
		RLA
		JP	C,DIR6		;skip if system file
;c holds index into buffer
;another fcb found, new line?
		POP	DE
		LD	A,E
		INC	E
		PUSH	DE
;e=0,1,2,3,...new line if mod 4 = 0
		AND	11B
dircol:		equ $-1		
		PUSH	AF		;and save the test
		JP	NZ,DIRHDR0	;header on current line
		CALL	CRLF
		PUSH	BC
		CALL	CSELECT
		POP	BC
;current disk in A
		ADD	A,'A'
		CALL	PRINTBC
		LD	A,':'
		CALL	PRINTBC
		JP	DIRHDR1		;skip current line hdr
DIRHDR0:	CALL	BLANK		;after last one
		LD	A,':'
		CALL	PRINTBC
DIRHDR1:
		CALL	BLANK
;compute position of name in buffer
		LD	B,1		;start with first character of name
DIR3:		LD	A,B     	
		CALL	ADDHCF		;buff+a+c fetched
		AND	7FH		;mask flags
;may delete trailing blanks     	
		CP	' '     	
		JP	NZ,DIR4		;check for blank type
		POP	AF      	
		PUSH	AF		;may be 3rd item
		CP	3       	
		JP	NZ,DIRB		;place blank at end if not
		LD	A,9     	
		CALL	ADDHCF		;first char of type
		AND	7FH
		CP	' '
		JP	Z,DIR5
;not a blank in the file type field
DIRB:		LD	A,' '		;restore trailing filename chr
DIR4:                           	
		CALL	PRINTBC		;char printed
		INC	B       	
		LD	A,B     	
		CP	12      	
		JP	NC,DIR5 	
;check for break between names  	
		CP	9       	
		JP	NZ,DIR3		;for another char
;print a blank between names
		CALL	BLANK
		JP	DIR3
;
DIR5:		;end of current entry
		POP	AF		;discard the directory counter (mod 4)
DIR6:		CALL	BREAK_KEY	;check for interrupt at keyboard
		JP	NZ,ENDIR	;abort directory search
		CALL	SEARCHN
		JP	DIR2		;for another entry
ENDIR:		;end of directory scan
		POP	DE		;discard directory counter
		JP	RETCOM
;
;
ERASE:
		CALL	FILLFCB0	;cannot be all ???'s
		CP	11
		JP	NZ,ERASEFILE
;erasing all of the disk
		LD	BC,ERMSG
		CALL	PRINT
		CALL	READCOM
		LD	HL,COMLEN
		DEC	(HL)
		JP	NZ,CCP		;bad input
		INC	HL
		LD	A,(HL)
		CP	'Y'
		JP	NZ,CCP
;ok, erase the entire diskette
		INC	HL
		LD	(COMADDR),HL	;otherwise error at retcom
ERASEFILE:
		CALL	SETDISK
		LD	DE,COMFCB
		CALL	DELETE
		INC	A		;255 returned if not found
		CALL	Z,NOFILE	;no file message if so
		JP	RETCOM
;
ERMSG:		DB	"ALL (Y/N)?",0
;
TYPE:
		CALL	FILLFCB0
		JP	NZ,COMERR	;don't allow ?'s in file name
		CALL	SETDISK
		CALL	OPENC		;open the file
		JP	Z,TYPERR	;zero flag indicates not found
;file opened, read 'til eof
		CALL	CRLF
		LD	HL,BPTR
		LD	(HL),255	;read first buffer
TYPE0:		;loop on bptr
		LD	HL,BPTR
		LD	A,(HL)
		CP	128		;end buffer
		JP	C,TYPE1
		PUSH	HL		;carry if 0,1,...,127
;read another buffer full
		CALL	DISKREADC
		POP	HL		;recover address of bptr
		JP	NZ,TYPEOF	;hard end of file
		XOR	A
		LD	(HL),A		;bptr = 0
TYPE1:		;read character at bptr and print
		INC	(HL)		;bptr = bptr + 1
		LD	HL,BUFF
		CALL	ADDH		;h,l addresses char
		LD	A,(HL)
		CP	EOFILE
		JP	Z,RETCOM
		CALL	PRINTCHAR
		CALL	BREAK_KEY
		JP	NZ,RETCOM	;abort if break
		JP	TYPE0		;for another character
;
TYPEOF:		;end of file, check for errors
		DEC	A
		JP	Z,RETCOM
		CALL	READERR
TYPERR:		CALL	RESETDISK
		JP	COMERR
;
SAVE:
		CALL	GETNUMBER	; value to register a
		PUSH	AF		;save it for later
;
;should be followed by a file to save the memory image
		CALL	FILLFCB0
		JP	NZ,COMERR	;cannot be ambiguous
		CALL	SETDISK		;may be a disk change
		LD	DE,COMFCB
		PUSH	DE
		CALL	DELETE		;existing file removed
		POP	DE
		CALL	MAKE		;create a new file on disk
		JP	Z,SAVERR	;no directory space
		XOR	A
		LD	(COMREC),A	; clear next record field
		POP	AF		;#pages to write is in a, change to #sectors
		LD	L,A
		LD	H,0
		ADD	HL,HL

		LD	DE,TRAN		;h,l is sector count, d,e is load address
SAVE0:		;check for sector count zero
		LD	A,H
		OR	L
		JP	Z,SAVE1		;may be completed
		DEC	HL		;sector count = sector count - 1
		PUSH	HL		;save it for next time around
		LD	HL,128  	
		ADD	HL,DE   	
		PUSH	HL		;next dma address saved
		CALL	SETDMA		;current dma address set
		LD	DE,COMFCB
		CALL	DISKWRITE
		POP	DE
		POP	HL		;dma address, sector count
		JP	NZ,SAVERR	;may be disk full case
		JP	SAVE0		;for another sector
;
SAVE1:		;end of dump, close the file
		LD	DE,COMFCB
		CALL	CLOSE
		INC	A		; 255 becomes 00 if error
		JP	NZ,RETSAVE	;for another command
SAVERR:		;must be full or read only disk
		LD	BC,FULLMSG
		CALL	PRINT
RETSAVE:
;reset dma buffer
		CALL	SETDMABUFF
		JP	RETCOM
FULLMSG:	DB	"NO SPACE",0
;
;
RENAME:
;rename a file on a specific disk
		CALL	FILLFCB0
		JP	NZ,COMERR	;must be unambiguous
		LD	A,(SDISK)
		PUSH	AF		;save for later compare
		CALL	SETDISK		;disk selected
		CALL	SEARCHCOM	;is new name already there?
		JP	NZ,RENERR3
;file doesn't exist, move to second half of fcb
		LD	HL,COMFCB
		LD	DE,COMFCB+16
		LD	B,16
		CALL	MOVE0
;check for = or left arrow
		LD	HL,(COMADDR)
		EX	DE,HL
		CALL	DEBLANK
		CP	'='
		JP	Z,REN1	;ok if =
		CP	LA
		JP	NZ,RENERR2
REN1:		EX	DE,HL
		INC	HL
		LD	(COMADDR),HL	;past delimiter
;proper delimiter found
		CALL	FILLFCB0
		JP	NZ,RENERR2
;check for drive conflict
		POP	AF
		LD	B,A		;previous drive number
		LD	HL,SDISK
		LD	A,(HL)
		OR	A
		JP	Z,REN2
;drive name was specified.  same one?
		CP	B
		LD	(HL),B
		JP	NZ,RENERR2
REN2:		LD	(HL),B		;store the name in case drives switched
		XOR	A
		LD	(COMFCB),A
		CALL	SEARCHCOM	;is old file there?
		JP	Z,RENERR1
;
;everything is ok, rename the file
		LD	DE,COMFCB
		CALL	RENAM
		JP	RETCOM
;
RENERR1:	; no file on disk
		CALL	NOFILE
		JP	RETCOM
RENERR2:	; ambigous reference/name conflict
		CALL	RESETDISK
		JP	COMERR
RENERR3:	; file already exists
		LD	BC,RENMSG
		CALL	PRINT
		JP	RETCOM
RENMSG:		DB	"FILE EXISTS",0
;
USER:
;set user number
		CALL	GETNUMBER	; leaves the value in the accumulator
		CP	16
		JP	NC,COMERR	; must be between 0 and 15
		LD	E,A		;save for setuser call
		LD	A,(COMFCB+1)
		CP	' '
		JP	Z,COMERR
		CALL	SETUSER		;new user number set
		JP	ENDCOM

go100:		jp	100h
;
USERFUNC:
		CALL	SERIALIZE	;check serialization
;load user function and set up for execution
		LD	A,(COMFCB+1)
		CP	' '
		JP	NZ,USER0
;no file name, but may be disk switch
		LD	A,(SDISK)
		OR	A
		JP	Z,ENDCOM	;no disk name if 0
		DEC	A
		LD	(CDISK),A
		CALL	SETDISKA	;set user/disk
		CALL	SELECT
		JP	ENDCOM
USER0:		;file name is present
		LD	DE,COMFCB+9
		LD	A,(DE)
		CP	' '
		JP	NZ,COMERR	;type ' '
		PUSH	DE
		CALL	SETDISK
		POP	DE
		LD	HL,COMTYPE	;.com
		CALL	MOVENAME	;file type is set to .com
		CALL	OPENC
		JP	Z,USERER
;file opened properly, read it into memory
		LD	HL,TRAN		;transient program base
LOAD0:		PUSH	HL		;save dma address
		EX	DE,HL
		CALL	SETDMA
		LD	DE,COMFCB
		CALL	DISKREAD
		JP	NZ,LOAD1
;sector loaded, set new dma address and compare
		POP	HL
		LD	DE,128
		ADD	HL,DE
		LD	DE,TRANM	;has the load overflowed?
		LD	A,L
		SUB	E
		LD	A,H
		SBC	A,D
		JP	NC,LOADERR
		JP	LOAD0		;for another sector
;
LOAD1:		POP	HL
		DEC	A
		JP	NZ,LOADERR	;end file is 1
GO:
		CALL	RESETDISK	;back to original disk
		CALL	FILLFCB0
		LD	HL,SDISK
		PUSH	HL
		LD	A,(HL)
		LD	(COMFCB),A	;drive number set
		LD	A,16
		CALL	FILLFCB		;move entire fcb to memory
		POP	HL
		LD	A,(HL)
		LD	(COMFCB+16),A
		XOR	A
		LD	(COMREC),A	;record number set to zero
		LD	DE,FCB
		LD	HL,COMFCB
		LD	B,33
		CALL	MOVE0
;move command line to buff
		LD	HL,COMBUF
BMOVE0:		LD	A,(HL)
		OR	A
		JP	Z,BMOVE1
		CP	' '
		JP	Z,BMOVE1
		INC	HL
		JP	BMOVE0		;for another scan
;first blank position found
BMOVE1:		LD	B,0
		LD	DE,BUFF+1
;ready for the move
BMOVE2:		LD	A,(HL)
		LD	(DE),A
		OR	A
		JP	Z,BMOVE3
;more to move
		INC	B
		INC	HL
		INC	DE
		JP	BMOVE2
BMOVE3:		;b has character count
		LD	A,B
		LD	(BUFF),A
		CALL	CRLF
;now go to the loaded program
		CALL	SETDMABUFF	;default dma
		CALL	SAVEUSER	;user code saved
;low memory diska contains user code
		CALL	TRAN		;gone to the loaded program
		LD	SP,STACK	;may come back here
		CALL	SETDISKA
		CALL	SELECT
		JP	CCP
;
USERER:		;arrive here on command error
		CALL	RESETDISK
		JP	COMERR
;
LOADERR:	;cannot load the program
		LD	BC,LOADMSG
		CALL	PRINT
		JP	RETCOM
LOADMSG:	DB	"BAD LOAD",0
COMTYPE:	DB	"COM"		;for com files
;
;
RETCOM:		;reset disk before end of command check
		CALL	RESETDISK
;
ENDCOM:		;end of intrinsic command
		CALL	FILLFCB0	;to check for garbage at end of line
		LD	A,(COMFCB+1)
		SUB	' '
		LD	HL,SDISK
		OR	(HL)
;0 in accumulator if no disk selected, and blank fcb
		JP	NZ,COMERR
		JP	CCP
;
;
;
;	data areas
		DS	16		;8 level stack
STACK:
;
;	'submit' file control block
SUBMIT:		DB	0		;00 if no submit file, ff if submitting
SUBFCB:		DB	0,"$$$     "	;file name is $$$
		DB	"SUB",0,0	;file type is sub
SUBMOD:		DB	0		;module number
SUBRC:		DS	1		;record count filed
		DS	16		;disk map
SUBCR:		DS	1		;current record to read
;
;	command file control block
COMFCB:		DS	32		;fields filled in later
COMREC:		DS	1		;current record to read/write
DCNT:		DS	1		;disk directory count (used for error codes)
CDISK:		DS	1		;current disk
SDISK:		DS	1		;selected disk for current operation
;none=0, a=1, b=2 ...           	
BPTR:		DS	1		;buffer pointer

;;	END	CCPLOC
