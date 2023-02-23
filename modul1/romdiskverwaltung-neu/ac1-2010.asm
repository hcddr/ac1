;------------------------------------------------------------------------------
;AC1-2010 include-Datei
;Volker Pohlers, Neustadt i.H., 09.01.2023
;------------------------------------------------------------------------------


;------------------------------------------------------------------------------
; Makros
;------------------------------------------------------------------------------

hi              function x, (x>>8) & 0ffh	; High-Byte
lo              function x, x & 0ffh		; Low-Byte
; bws(zeile 0..31, spalte 0..63) analog print_at
bws		function z, s, 17FFh-z*64-s

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
SYSSK:		equ	1856h		; System-Stack 
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

PIO2DA		equ	8
PIO2DB		equ	9
PIO2CA		equ	0Ah
PIO2CB		equ	0Bh

modul1		equ	14h		; Konfiguationsbyte für Modul 1

;------------------------------------------------------------------------------
; Monitor-Funktionen
;------------------------------------------------------------------------------
;

; allg. AC1
inch		equ	0008h
outch		equ	0010h
prnst		equ	0018h

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

BS		equ	08h		; backspace
CLS		equ	0Ch		; Bildschirm löschen
CR		equ	0Dh		; neue Zeile
SPC		EQU	20H		; Leerzeichen

; CURSL:	EQU	8		;CURSOR LINKS
; CURSR:	EQU	9		;CURSOR RECHTS
; CURSD:	EQU	0AH		;CURSOR RUNTER (LF)
; CURSU:	EQU	0BH		;CURSOR HOCH


; ausgabe steuerzeichen (ac1-2010)
; 0bh
; 06h
; 02h
