; --------------------------------------------------------------------
; Include-Datei fuer ZBIOS.MAC (USER-Optionen)
;
; =========== Version fuer zwei Festplatten (MASTER/SLAVE) ===========
;
; OPTION.INC		(c) ML-Soft 29.12.1996, 13.02.1998, 13.01.2003
; --------------------------------------------------------------------


; 1. Anzahl logischer Laufwerke:
;
; Hier kann eine Zahl zwischen 5 und 16 eingegeben werden,
; entsprechend viele Laufwerke stehen dann zur Verfuegung.
; Bei 5 sind es die Laufwerke A: bis E:, bei 6 A: bis F: usw.
; bis maximal A: bis P: bei 16 Laufwerken.

NDRIVES	EQU	7	; 7 logische Laufwerke

; --------------------------------------------------------------------

; 2. Laufwerke fuer die Festplatte reservieren:
;
; Die Werte geben an, wieviele der vorher definierten Laufwerke von
; Festplatte(n) gebildet werden. Moeglich sind insgesamt maximal 6,
; wobei das erste Laufwerk immer C: ist, und von der Masterplatte
; gebildet wird, also:
; HARD=0 fuer Systeme ohne Festplatte
; HARD=1 fuer eine einzige Partition C:
; HARD=2 fuer 2 Partitionen C: und D:
; HARD=3 fuer 3 Partitionen C:, D: und E:
; usw.

HARD	EQU	3		; drei Master-Partitionen (C:/D:/E:)
				; und drei Slave-Partitionen (F:/G:/H:)

; --------------------------------------------------------------------

; 3. IDE-Festplattenparameter:
;
; Hier sind die physischen Parameter der verwendeten Festplatte(n)
; einzutragen. Diese kann man z.B. mit dem Testprogramm GIDETEST.COM
; ermitteln. Die Werte entsprechen...

; Master
; AC1-Version: DELOCK SSD (256 MByte)ß: 984*16*32

CYLS0	EQU	984		; Anzahl Zylinder
HEAD0	EQU	16		; Anzahl Koepfe
SECS0	EQU	32		; Anzahl Sektoren

; Slave = Conner CP2121 (119 MByte)

CYLS1	EQU	928		; Anzahl Zylinder
HEAD1	EQU	8		; Anzahl Koepfe
SECS1	EQU	32		; Anzahl Sektoren

; Einige Festplattendaten (Beispiele getesteter Platten):

; Typ			Zylind.	Koepfe	Sektor.	Kapazitaet
;						Partitionierung C/D/E
; --------------------------------------------------------------------
; Seagate ST-157A	560	6	26	42MB	120/220/220
; ALPS D311C		732	8	35	100MB	150/300/282
; Caviar WDAC 2120	872	8	35	119MB	200/300/300
; Conner CP2121 (2,5")	928	8	32	119MB	180/374/374
; Conner CFS210A	685	16	38	213MB	107/214/214
; Seagate ST 3243A	1024	12	34	214MB	160/320/320
; --------------------------------------------------------------------

; 4. Partitionierung der Festplatte(n):
;
; Um die Festplatte(n) auf mehrere logische Laufwerke aufzuteilen,
; muss festgelegt werden, wie gross jede Partition werden soll.
; Dazu erhaelt jede Partition einen Teil der Gesamtzylinder einer
; Festplatte.  Es muessen nicht alle vorhandenen Zylinder benutzt
; werden, dann bleibt ein Teil der Gesamtkapazitaet der Festplatte
; ungenutzt.  Die maximale Partitionsgroesse betraegt 64 MByte!
;
; In Abhaengigkeit von den Parametern der Festplatte variiert die
; Speicherkapazitaet je Zylinder. Berechnung der Spurgroesse:
;
;	HEADS * SECS * 0,5 = Kilobyte/Zylinder
;
; ALPS D311C:		8 * 35 * 0,5 = 140K
; Conner CP2121:	8 * 32 * 0,5 = 128K
;
; Fuer Partition C: sollte eine Systemspur (mindestens 32 KByte) definiert
; werden, in der spaeter ein Bootsektor untergebracht werden kann.
;
; Fuer jedes logische Festplattenlaufwerk sind 6 Parameter anzugeben:
;	- Laufwerksbuchstabe
;	- MASTER (0) oder SLAVE (1), bei nur einem Laufwerk MASTER eingeben!
;	- erste genutzte Spur(Zylinder) dieser Partition
;	- Anzahl der genutzten Spuren(Zylinder) in dieser Partition
;	- Anzahl Systemspuren (nur bei C: erforderlich)
;	- Blockgroesse (4K oder 8K)
;	- Anzahl Verzeichniseintraege (1024, 2048 oder 4096)
;
; ACHTUNG: Blockgroesse 4K => max. 2048 Verzeichniseintraege!
;	   Blockgroesse 8K => max. 4096 Verzeichniseintraege!

; Partitionierung der Master-Festplatte:
; DeLock SSD 256MB je Track 256KB, 984 Tracks
; 1.Partition beginnt ab Spur 10, damit stehen die Spuren 0..9 
; 				  zusätzliche als Bootspuren

	PART	C,0, 10, 64,1,4,2048	; C: mit  64 Spuren ab Spur 10  (16 MB) HR
	PART	D,0,150, 64,0,8,4096	; D: mit  64 Spuren ab Spur 150 (16 MB) HR
	PART	E,0,300, 64,0,8,4096	; E: mit  64 Spuren ab Spur 300 (16 MB) HR
;	PART	E,0,300,128,0,8,4096	; E: mit  64 Spuren ab Spur 300 (32 MB) HR

; Partitionierung der Slave-Festplatte:

	PART	F,1,  0,180,1,4,2048	; F: mit 180 Spuren ab Spur 0 (22 MB)
	PART	G,1,180,374,0,8,4096	; G: mit 374 Spuren ab Spur 180 (46 MB)
	PART	H,1,554,374,0,8,4096	; H: mit 374 Spuren ab Spur 554 (46 MB)


; --------------------------------------------------------------------

; 5. Definition der Diskettenlaufwerke:
;
; Die Laufwerke A: (RAM-Floppy) und B: (Bootlaufwerk 780K) sind
; fest vorgegeben und lassen sich nicht aendern. Die restlichen
; zu erzeugenden Diskettenlaufwerke (entsprechend der Anzahl NDRIVES und
; HARD) sind im folgenden Abschnitt zu definieren. Dazu sind 5 Parameter
; fuer jedes Laufwerk anzugeben:
;	- Laufwerksbuchstabe
;	- das physische Format (1-9, siehe folgende Tabelle)
;	- die Blockgroesse (1, 2 oder 4 kByte)
;	- die Anzahl der Verzeichniseintraege (64, 128 oder 256)
;	- die Anzahl der Systemspuren
;	- die verwendete physische Einheit (0-3)
;
; Definierte Formate:	1	16* 256*40*1 => 160k
; (alle fuer Typ 1.6)	2	 5*1024*40*1 => 200k
;			3	 8* 512*40*2 => 320k
;			4	 9* 512*40*2 => 360k
;			5	16* 256*80*1 => 320k
;			6	 5*1024*80*1 => 400k
;			7	16* 256*80*2 => 640k
;			8	 9* 512*80*2 => 720k
;			9	 5*1024*80*2 => 800k

	LAUFW	C,7,2,128,2,0		; C: 624k, Lw 0 (SCP-Format)
	LAUFW	D,9,2,128,0,0		; D: 800k, Lw 0 (KC-Datenlaufwerk)
	LAUFW	E,8,2,128,0,1		; E: 720k, Lw 1 (DOS-Format)

;	LAUFW	F,9,2,128,2,1		; F: 780k, Lw 1 (KC-Standard)
;	LAUFW	G,8,4,128,1,1		; G: 708k, Lw 1 (Vortex)

;AC1
; FDC (48H) B0:5*1024 F0:16*256 G1:5*1024
	LAUFW	F,7,2,128,0,0		; C: 624k, Lw 0 (SCP-Format, ohne Systemspur)
	LAUFW	G,9,2,128,0,1		; D: 800k, Lw 1 (KC-Datenlaufwerk)
	
	LAUFW	H,9,2,128,2,2		; H: 780k, Lw 2 (KC-Standard)
	LAUFW	I,2,1, 64,3,3		; I: 185k, Lw 3 (Osborne 1 - SSDD)
	LAUFW	J,2,1, 64,0,3		; J: 200k, Lw 3 (CP/A-200)
	LAUFW	K,4,2, 64,2,3		; K: 342k, Lw 3 (Siemens PG-675)
	LAUFW	L,8,2,256,2,3		; L: 702k, Lw 3 (Siemens PG-685)
	LAUFW	M,9,2,192,0,3		; M: 800k, Lw 3 (KC mit 192 DIR-Eintr{gen)
	LAUFW	N,7,2,128,2,3		; N: 624k, Lw 3 (KC-Standard)
	LAUFW	O,9,2,128,2,3		; O: 780k, Lw 3 (KC-Standard)
	LAUFW	P,9,2,128,2,3		; P: 780k, Lw 3 (KC-Standard)

; HINWEIS: Es werden nur die Parameter der tatsaechlich benutzten Laufwerke
; (entsprechend NDRIVES und HARD) ausgewertet.  Die restlichen Definitionen
; muessen zumindest korrekte Werte enthalten, koennen aber auch geloescht
; werden.  Die angegebenen Definitionen der Laufwerke C: bis M: sind erprobt
; und koennen als Beispiele benutzt werden.

;	end of include
