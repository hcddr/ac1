;
; Macro zum Berechnen der Festplatten-DPB's fuer ZBIOS:
;
; CALHDD.INC			(c) ML-Soft 13.01.2003
; ----------------------------------------------------

; Aufruf:	CALHDD	name
;

CALHDD	MACRO	N
;LOCAL	X

SPT	EVAL	4*HEAD_N*SECS_N ; 128-Byte Sektoren je Spur

BLM	EVAL	BLOH_N*8

BSH	EVAL	0
X	EVAL	BLM-1
	REPT	8
	IF	X <> 0
BSH	EVAL	BSH+1
	ENDIF
X	EVAL	X >> 1
	ENDM

DSM	EVAL	(CYL_N-OFFH_N)*HEAD_N/2*SECS_N
	IF	BLOH_N = 4
DSM	EVAL	DSM >> 2
	ENDIF
	IF	BLOH_N = 8
DSM	EVAL	DSM >> 3
	ENDIF
BLOCK_N	equ	DSM
DSM	EVAL	(DSM * BLOH_N) >> 10

	IF	BLOCK_N >= 256
EXM	EVAL	(BLM-1) >> 4
	ELSE
EXM	EVAL	(BLM-1) >> 3
	ENDIF

AL01	EVAL	0FFFFH
	REPT	(DIRH_N+BLM-1)/BLM/4
AL01	EVAL	AL01 >> 1
	ENDM
AL01	EVAL	0FFFFH-AL01

; allg. Parameter:
	DW	SPT		; SPT
	DB	BSH		; BSH
	DB	BLM-1		; BLM
	DB	EXM		; EXM
	DW	BLOCK_N-1	; DSM
	DW	DIRH_N-1	; DRM
	DB	HIGH (AL01)	; AL0
	DB	LOW (AL01)	; AL1
	DW	0		; CKS
	DW	OFFH_N		; OFF
	DB	2,3		; phys. Sektorgroess: 512 Byte

; Parameter der Partition:
	DB	MAST_N		; Master/Slave-Kennung!
	DW	CYL0_N		; erste Spurnummer der Partition
	DW	CYL_N		; Spuranzahl in der Partition

; Parameter der Platte:
	DW	CYLS_N		; Spuranzahl in der Platte
	DB	HEAD_N		; Anzahl Koepfe
	DB	SECS_N		; Anzahl phys. Sektoren

	IF MOMPASS=1
	IF	MAST_N = 0A0h
	PRTVAL 10,"*  Master-Partition N:",DSM,"MByte *"
	ELSE
	PRTVAL 10,"*  Slave-Partition  N:",DSM,"MByte *"
	ENDIF
	IF OFFH_N
X	EVAL SECS_N*HEAD_N/2
	PRTVAL 10,"*      System enthaelt:",X,"KByte *"
	ENDIF
	ENDIF

	ENDM

;	end od include
