;
; Macro zum Berechnen der Disketten-DPB's f}r ZBIOS:
;
; CALDPB.INC			(c) ML-Soft 28.12.96
; --------------------------------------------------

; Aufruf:	CALDPB	phys,block,dirs,offset,unit
;
; Beispiel:	CALDPB 9,2,128,2,0	=> B:780k

; physische Formate f}r LW-Typ 1.6:

; Nr.	phys. Format	Kapazit{t
; 1	16* 256*40*1	160k
; 2	 5*1024*40*1	200k
; 3	 8* 512*40*2	320k
; 4	 9* 512*40*2	360k
; 5	16* 256*80*1	320k
; 6	 5*1024*80*1	400k
; 7	16* 256*80*2	640k
; 8	 9* 512*80*2	720k
; 9	 5*1024*80*2	800k
;

PHYS	MACRO	a,b,c,d,e
SEC	EVAL	a
FACT	EVAL	b
NTR	EVAL	c
SIDE	EVAL	d
GAP	EVAL	e
	ENDM

CALDPB	MACRO	FORMAT,BLOCK,DRM,OFFSET,UNIT
;LOCAL	X

	IF	FORMAT = 1
	PHYS	16,2,40,1,20
	ENDIF
	IF	FORMAT = 2
	PHYS	5,8,40,1,37
	ENDIF
	IF	FORMAT = 3
	PHYS	8,4,40,2,42
	ENDIF
	IF	FORMAT = 4
	PHYS	9,4,40,2,25
	ENDIF
	IF	FORMAT = 5
	PHYS	16,2,80,1,20
	ENDIF
	IF	FORMAT = 6
	PHYS	5,8,80,1,37
	ENDIF
	IF	FORMAT = 7
	PHYS	16,2,80,2,20
	ENDIF
	IF	FORMAT = 8
	PHYS	9,4,80,2,25
	ENDIF
	IF	FORMAT = 9
	PHYS	5,8,80,2,37
	ENDIF

SPT	EVAL	SEC*FACT*SIDE

BLM	EVAL	BLOCK*8

BSH	EVAL	0
X	EVAL	BLM-1
	REPT	8
	IF	X <> 0
BSH	EVAL	BSH+1
	ENDIF
X	EVAL	X >> 1
	ENDM

DSM	EVAL	(NTR-OFFSET)*SPT/BLM

	IF	DSM >= 256
EXM	EVAL	(BLM-1) >> 4
	ELSE
EXM	EVAL	(BLM-1) >> 3
	ENDIF

AL01	EVAL	0FFFFH
	REPT	(DRM+BLM-1)/BLM/4
AL01	EVAL	AL01 >> 1
	ENDM
AL01	EVAL	0FFFFH-AL01

CKS	EVAL	DRM/4
	IF	CKS > 32
CKS	EVAL	32
	ENDIF

PSH	EVAL	0
X	EVAL	FACT-1
	REPT	3
	IF	X <> 0
PSH	EVAL	PSH+1
	ENDIF
X	EVAL	X >> 1
	ENDM

TYP	EVAL	43H
	IF	NTR = 40
TYP	EVAL	53H
	ENDIF

	DW	SPT		; SPT
	DB	BSH		; BSH
	DB	BLM-1		; BLM
	DB	EXM		; EXM
	DW	DSM-1		; DSM
	DW	DRM-1		; DRM
	DB	HIGH (AL01)	; AL0
	DB	LOW (AL01)	; AL1
	DW	CKS		; CKS
	DW	OFFSET		; OFF
	DB	PSH		; PSH
	DB	FACT-1		; PHM
	DB	SEC		; EOT
	DB	GAP		; GAP
	DB	NTR		; NTR
	DB	UNIT		; PUN
	DB	TYP		; DTYP
	DB	0FFH		; FTP
	DB	225	;239	; TSS
	DB	51	;20	; HLT
	DB	0FFH		; CUR

	ENDM

;	end od include
