;-----------------------------------------------------------------------------
; Mini-CP/M für den AC1 ohne Floppy (zum Reinschnuppern)
; mit kleiner RAM-Disk
; 02.08.2016 V.Pohlers
;
; letzte Änderung 02.08.2016
;-----------------------------------------------------------------------------

		cpu	z80
		LISTING	NOSKIPPED

;-----------------------------------------------------------------------------
; Speicheraufteilung
; RAM:
; 	E000 CCP
; 	E806 BDOS
;	F600 BIOS
; 	7000-DFFF RAM-Floppy

;-----------------------------------------------------------------------------

x_align		function x, (($+x-1)/x)*x
hi              function x, (x>>8) & 0ffh	; High-Byte
lo              function x, x & 0ffh		; Low-Byte


CCP		equ	0E000h		; base of ccp
BDOS       	equ	CCP+0800h	; base of bdos
BIOS		equ	CCP+1600h	; base of bios


IOLOC	EQU	3		;i/o definition byte.
DISKA	EQU	4		;current drive name and user number.

;-----------------------------------------------------------------------------
; CCP = orig. DR-CCP + 3 Zusatzbefehle (GO, LOAD, EXIT) + JP -> Jr
; vermutlich frühe CPA-Version
;-----------------------------------------------------------------------------
; 2 Sprünge extern in BIOS
;		dw 0F638h	; exit, BIOS+38
;		dw 0F63Bh	; load, BIOS+3B
;-----------------------------------------------------------------------------

		ORG	CCP
		LISTING	OFF
		section CCP
		public	dircol,COMLEN,RETCOM
		include	ccp22_ac1.asm
		endsection
		LISTING	NOSKIPPED
		
;-----------------------------------------------------------------------------
; BDOS = CPA
; frühere CPA-Version 24.08.86
;-----------------------------------------------------------------------------

		org	BDOS
		LISTING	OFF
		section BDOS
		include	bdos_cpa_ac1.asm
		endsection
		LISTING	NOSKIPPED

;-----------------------------------------------------------------------------
; BIOS
;-----------------------------------------------------------------------------

		org	BIOS
		section BIOS
		include	bios15_ac1.asm
		;include	bios16_ac1.asm
		endsection

		end
