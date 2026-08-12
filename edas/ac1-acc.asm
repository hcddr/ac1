;------------------------------------------------------------------------------
;AC1-ACC include-Datei
;Volker Pohlers, Neustadt i.H., 09.01.2023
;------------------------------------------------------------------------------


;------------------------------------------------------------------------------
; Makros
;------------------------------------------------------------------------------

hi              function x, (x>>8) & 0ffh	; High-Byte
lo              function x, x & 0ffh		; Low-Byte
; bws(zeile 0..31, spalte 0..63) analog print_at
bws		function z, s, 17FFh-z*64-s

;reverse db via locals
dbr		MACRO	name,{NOEXPAND},{EXPREST}
qcnt		eval	0
		irpc	x,name
temp		eval	"\{qcnt}"
qrdc_{temp}	eval 	'x'
qcnt		eval	qcnt+1
		endm
		rept	qcnt
qcnt		eval	qcnt-1
temp		eval	"\{qcnt}"
		DB	qrdc_{temp}
		endm
		endm
;	dbr "TEST"

;------------------------------------------------------------------------------
; Speicheradressen
;------------------------------------------------------------------------------

ROM		equ	0000h
BASIC		equ	0800h
BWS		equ	1000h
RAM		equ	1800h
; BWS
COLS		equ	64
LINES		equ	32
; 01000H Bildschirmende (rechts unten !!)
; 017FFH Bildschirmanfang (links oben !!)

cupos:		equ	1800h		; Cursorposition (Adr im RAM)
jp_rst08:	equ	1802h		; jp	rinch
jp_rst10:	equ	1805h		; jp	routch
jp_rst18:	equ	1808h		; jp	rprnst
jp_rst20:	equ	180Bh		; jp	0FFFFh
jp_rst28:	equ	180Eh		; jp	0FFFFh
jp_rst30:	equ	1811h		; jp	0FFFFh
jp_rst38:	equ	1814h		; jp	rError
jp_nmi:		equ	1817h		; jp NMI-Routine
soil:		equ	181Ah		; Beginn Eingabezeile
syssk:		equ	1856h		; System-Stack 
data:		equ	1858h		; interner Speicher f. Monitor
ARG1:		equ	185Bh		; Kdo-Argument 1
ARG2:		equ	185Dh		; Kdo-Argument 2
ARG3:		equ	185Fh		; Kdo-Argument 3


;------------------------------------------------------------------------------
; I/O-Adressen
;------------------------------------------------------------------------------

CTC0		equ	0
CTC1		equ	1
CTC2		equ	2
CTC3		equ	3

PIODA		equ	4		; Tastatur
PIODB		equ	5		; Grafik/Ton
PIOCA		equ	6
PIOCB		equ	7

;------------------------------------------------------------------------------
; Monitor-Funktionen
;------------------------------------------------------------------------------
;
INCH		equ	0008h		; rst 8
OUTCH		equ	0010h		; rst 10
PRNST		equ	0018h		; rst 18
;
MS30	    	equ	07EBh		; 30 ms	warten
OUTHEX      	equ	07EEh		; Ausgabe A  hexadezimal
OUTHL	    	equ	07F1h		; Ausgabe HL hexadezimal
INLINE	    	equ	07F4h		; Zeile	eingeben, Adr steht in soil
INHEX       	equ	07F7h		; konv. ASCII-Hex ab (DE) abwärts nach HL
TASTE       	equ	07FAh		; testet den Tastaturstatus, ret Z=1 keine Taste, sonst A=Code
GETCO1      	equ	07FDh		; Sprung zur Monitoreingabeschleife

;------------------------------------------------------------------------------
; Zeichen
;------------------------------------------------------------------------------

; AC1
BS		equ	08h		; backspace
CLS		equ	0Ch		; Bildschirm löschen
CR		equ	0Dh		; neue Zeile
SPC		EQU	20H		; Leerzeichen
LEFT		equ 	08h		; Kursor nach links
RIGHT		equ 	09h		; Kursor nach rechts
; FA 4/84, aber nicht im Monitor genutzt:
DOWN		equ 	0Ah		; Kursor nach unten
UP		equ 	0Bh		; Kursor nach oben
BEL		equ	07h		; aktustisches Signal
RUBOUT		equ	7Fh		; original FA 4/84: 5Fh
