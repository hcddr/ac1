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
syssk:		equ	1856h		; System-Stack 
data:		equ	1858h		; interner Speicher f. Monitor
ARG1:		equ	185Bh		; Kdo-Argument 1
ARG2:		equ	185Dh		; Kdo-Argument 2
ARG3:		equ	185Fh		; Kdo-Argument 3

; erw. 2010
warmst:		equ	181Ch		; warmstart-Erkennung ("SCH")
farbbws:	equ	181Fh		; Farb-Attribut
kdov24:		equ	1820h		; V24-Konfiguration
iobyt:		equ	1821h		; I/O-Byte


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


PIO2EPROM	equ	0Fh		; EPROM auf PIO2-Karte
modul1		equ	14h		; Konfiguationsbyte für Modul 1
cpmumsch	equ	1Eh		; Bit0=1 64K RAM aktiv


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

; ac1
BS		equ	08h		; backspace
CLS		equ	0Ch		; Bildschirm löschen
CR		equ	0Dh		; neue Zeile
SPC		EQU	20H		; Leerzeichen

; ac12010
; Steuercodes Bildschirm
HOME:		equ 	01h		; Home, Kursor oben links
CLSC:		equ 	02h		; Bildschirm ab Kursorposition löschen
CLLN:		equ 	03h		; Zeile ab Kursorposition löschen
DEL:		equ 	04h		; Delete; Zeichen löschen, Zeile rückt nach links
INS:		equ 	05h		; Insert; Space einfügen, Zeile rückt nach rechts
SOL:		equ 	06h		; Kursor an den Anfang der Zeile
BELL:		equ 	07h		; BEL, akustisches Signal
LEFT:		equ 	08h		; Kursor nach links
RIGHT:		equ 	09h		; Kursor nach rechts
DOWN:		equ 	0Ah		; Kursor nach unten
UP:		equ 	0Bh		; Kursor nach oben
;CLS:		equ 	0Ch		; Bildschirm löschen
;CR:		equ 	0Dh		; CR, Kursor an Anfang nächster Zeile; Enter
SETC:		equ 	0Eh		; Kursor direkt positionieren
TAB:		equ 	0Fh		; Tabulator 8 Spalten

norm:		equ	10H 		; Einzelzeichen-normal
inv:		equ	11H 		; Einzelzeichen-Invers
printon:	equ	18H 		; Drucker ein (V 24 – Schnittstelle)
printoff:	equ	19H 		; Ein-/Ausgabe normal Drucker aus
;		equ	1AH 		; Umschaltung Zeichensatz SCCH/ACC
rubout:		equ	5FH 		; Kursor nach links und Zeichen löschen
