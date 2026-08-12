;------------------------------------------------------------------------------
;AC1-pico include-Datei
;Volker Pohlers, Neustadt i.H., 09.03.2024
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

; erw. picoAC1 ->  ORG  1880H
; Systemzellen der pico-Erw.
; nicht direkt zugreifen ! --> FGETSY OFFS in HL!

FRESCO EQU 0    ;Fehlercode ext. Save/Load
FLEN   EQU 2    ;Laenge des Speicherblocks
FPOS   EQU 4    ;Position des akt Zeichens
FRESV  EQU 6    ;Reserve fuer Erweiterung
FTYPE  EQU 8    ;Filetyp
BUFLEN EQU 9    ;max Textpuffer
TXTBUF EQU 10 	;Textpuffer (Name)
FCSUM  EQU 19h   ;Zwischenerg.CSum


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

cpmumsch	equ	1Eh		; Bit0=1 64K RAM aktiv

;------------------------------------------------------------------------------
; Monitor-Funktionen
;------------------------------------------------------------------------------
;

; allg. AC1
inch		equ	0008h
outch		equ	0010h
prnst		equ	0018h

MS30		equ	07EBh		; 30 ms	warten
OUTHEX		equ	07EEh		; Ausgabe A  hexadezimal
OUTHL		equ	07F1h		; Ausgabe HL hexadezimal
INLINE		equ	07F4h		; Zeile	eingeben, Adr steht in soil
INHEX		equ	07F7h		; konv.	ASCII-Hex ab (DE) abwärts nach HL
TASTE		equ	07FAh		; testet den Tastaturstatus, ret Z=1 keine Taste, sonst	A=Code
GETCO1		equ	07FDh		; Sprung zur Monitoreingabeschleife

; picoac1 monx

pico		equ	0FFEH		; Kennung picoAC1 ( = 'pi')
monx_version	equ	0800h		; Ausgabe Versionsstring f.Ueberschrift

; Sprungverteiler Monitorerweiterung -------

; Sprungverteiler V1.0.3
CLRBUF		EQU  0810H		;Namebuffer loeschen
FGETSY		EQU  0813H		;Re:Zeiger auf SysRAM
GETTXT		EQU  0816H		;EingabeTxt ab SOIL v.BWS
GETTX0		EQU  0819H		;EingabeTxt ab HL vom BWS
PRNTXA		EQU  081CH		;A -> Druckdatei
CRC16		EQU  081FH		;CRC-Summe Speicherbereic
FOPENW		EQU  0822H		;ext. Datei open write
FCHRW		EQU  0825H		;Byte in ext.Datei schr.
FBLKW		EQU  0828H		;Speicheblock in ext.schr
FOPENR		EQU  082BH		;ext.Datei open	read
FCHRR		EQU  082EH		;Byte aus ext.Datei lesen
FBLKR		EQU  0831H		;Speicherblock lesen
FGETRE		EQU  0834H		;ResCode FileOP	Z/0000=Ok
VERSTR		EQU  0837H		;Ausgabe Versionsstr.
FGETLS		EQU  083AH		;Anforderung Dateiliste
FGETDR		EQU  083DH		;Anforderung Verz.liste
GETIME		EQU  0840H		;Zeit ->ARG1..3
; ZCOM - low level
PICOMW		EQU  0843H		;Verbindung oeffnen
WRBYTE		EQU  0846H		;Byte in ZCOM schreiben
RDSYNC		EQU  0849H		;Antwort pIO synchronis.
RDBYTE		EQU  084CH		;Byte v. ZCOM abholen
WRHDRX		EQU  084FH		;Zeichenkette (BWS)->ZCOM

;------------------------------------------------------------------------------
; Zeichen
;------------------------------------------------------------------------------

; ac1
BS		equ	08h		; backspace
CLS		equ	0Ch		; Bildschirm löschen
CR		equ	0Dh		; neue Zeile	(co: NL, = CR+LF)
SPC		EQU	20H		; Leerzeichen

; ac1 BASIC 1.1
LEFT:		equ 	08h		; Kursor nach links
RIGHT:		equ 	09h		; Kursor nach rechts
;DOWN:		equ 	0Ah		; Kursor nach unten
;UP:		equ 	0Bh		; Kursor nach oben

; EDIT 4.1 FA 07+08/87

; -Cursorfunktionen: backspace      Ø8H (64AFH)
;                    horizontal tab.Ø9H (64B4H)
;                    line feed      ØAH (64B9H)
;                    vertical tab.  ØBH (64BEH)
;                    carriage ret.  14H (64C7H)
;
;                    carriage ret.
;                    +line feed     ØDH (64AAH)
;
;                    vertical tab.  17H (64DCH)
;                    (zum naechsten
;                     Wort  in  der
;                     darueber lie-
;                     genden Zeile)
;
; -Zeichen           einfuegen      Ø5H (64E1H)
;                    loeschen       13H (64E6H)
;
; -Zeile             einfuegen      Ø1H (64D2H)
;                    loeschen       Ø2H (64CDH;6616H)
;
; -Bild rollen       nach oben      11H (64C3H)
;                    nach unten     15H (64C8H)
;
; -Ruecksprung ins   Menu           Ø3H (64EBH;6457H)
;                                             (+BIT7)
;
; -Shiftarretierung                 12H (6445H;6453H)
;                                       (  +Bit 7   )

; CPM 1.5 FA

; Bildschirm-Steuerzeichen, SCP-kompatibel
; 00h            NOP (keine Wirkung)
; 01h            Cursor links oben (home)
; 07h            akustisches  Zeichen an  Tastatur
; 08h            Cursor zurueck
; 0ah            Linefeed (neue Zeile)
; 0ch            Bildschirm  loeschen (verzoegert zum Lesen  der
;                zuletzt ausgegebenen Bildschirmzeilen), Cursor
;                links oben
; 0dh            Carriage Return (an Zeilenanfang)
; 14h            Rest des Bildschirms loeschen
; 15h            Cursor nach rechts
; 16h            Rest der Zeile loeschen
; 18h            Zeile loeschen, Cursor an Zeiilenanfang
; 1ah            Cursor eine Zeile hoch
; 1bh            Einleitung  Cursorpositionierfolge, die naech-
;                sten beiden Bytes beinhalten Zeile und  Spalte
;                Offset 00h oder 80h
; 7fh            Delete (streichen Zeichen links vom Kursor)
