;Hexdump mit Pruefbyte und CRC

	cpu	z80

	ORG	0
;*************************************
;*                                   *
;* 'd' Hexdump mit Zeilenpruefbyte   *
;*     und CRC ueber gesamten Dump   *
;*                                   *
;* 'c' CRC (SDLC) ueber den Speicher *
;*     von ARG1 bis einschl. ARG2    *
;*                                   *
;*************************************
;
;(C) Y21SO
;Frank Heyder , 17. August 1986
;
;	Vereinbahrungen zum Monitor
;	---------------------------
ARG1	EQU 185BH	;1. Argument
ARG2	EQU 185DH	;2. Argument
MSG	EQU 18H		;String ausgeben 
OUTHL 	EQU 7F1H	;HL hex ausgeben
OUTHEX 	EQU 7EEH	;Akku hex ausgeben
ASCCR 	EQU 0DH
;
;
;
	ORG 1F80H	;nach RAM-Test
;
;	dieses Programm ist relocatibel !!

	DEFB 0,9,'d',0dh	;Kennbyte 'd'
	LD	DE,(ARG1)	;Anfangsadresse
	LD	HL,(ARG2)	;Endadresse
	XOR 	A		;Akku=0 , Cy=9
	SBC	HL,DE
	INC 	HL
	EX 	DE,HL 		;Bytezahl in DE
LOOP2	JR	Z, CRCCAL	;fertig ?
	XOR	A
	LD	C,A		; loesche Pruefbyte
	CALL 	OUTHL		;Adresse ausgeben
	LD	B,16		;16 Byte/Zeile
	RST	MSG
	DEFB ' ',' '+80H	;2xSpace ausgeben 
LOOP1	LD	A, (HL)		;Byte holen
	CALL 	OUTHEX
	XOR 	C		;Pruefbyte
	LD	C,A
	RST	MSG
	DEFB ' '+80H
	DEC	DE
	LD	A,E
	OR	D
	JR	Z,ENDLN		;fertig?
	INC	HL		;naechstes Byte
	DJNZ	LOOP1
	AND	A		;loesche Z-Flag
ENDLN	PUSH	AF		;rette Z-Flag
	RST	MSG
	DEFB ' ','*',' '+80H
	LD	A,C
	CALL 	OUTHEX		;Pruefsumme ausg.
	RST 	MSG
	DEFB ' ','*',ASCCR+80H
	POP 	AF
	JR	LOOP2
CRCCAL 	RST 	MSG
	DEFB ASCCR+80H
;
;
;	CRC Berechnung nach dem SDLC-
;	Polynom	x16+x12+x5+1
;
	DEFB 0,9,'c',0dh	;Kennbyte 'c'
;
CRCENT	LD	HL,(ARG1)
	LD	DE,0FFFFh	;ruecksetzen CRC
BYTCRC 	LD	B,80H		;beginne mit Bit 7
CRCLP1 	SLA	E		;CRC schieben
	RL	D
	SBC	A,A		;Cy=1 -> A=FF
	XOR	(HL)		;Cy=0 -> A=00
	AND 	B
	JR	Z,CRC0
	;Rueckkopplung CRC-Generator
	LD	A,E
	XOR 	021h		;00100001 bei SDLC
	LD	E,A
	LD	A,D
	XOR 	010h		;00010000 bei SDLC
	LD	D,A
CRC0	SRL	B
	JR	NC,CRCLP1	;Byte fertig ?
	LD	BC,(ARG2)
	XOR	A		;Cy -> 0
	SBC 	HL,BC
	ADD 	HL,BC
	INC 	HL
	JR	NZ,BYTCRC	;fertig ?
	EX	DE,HL		;CRC nach HL
	RST 	MSG
	DEFB "CRC (SDLC) ="
	DEFB ' '+80h		;Space
	CALL 	OUTHL
	RST 	MSG
	DEFB ASCCR+80H
	RET			;--> Monitor

	END	

d 1F80 1FFF
00 09 64 0D ED 5B 5B 18 2A 5D 18 AF ED 52 23 EB
28 2B AF 4F CD F1 07 06 10 DF 20 A0 7E CD EE 07
A9 4F DF A0 1B 7B B2 28 04 23 10 F0 A7 F5 DF 20
2A A0 79 CD EE 07 DF 20 2A 8D F1 18 D3 DF 8D 00
09 63 0D 2A 5B 18 11 FF FF 06 80 CB 23 CB 12 9F
AE A0 28 08 7B EE 21 5F 7A EE 10 57 CB 38 30 EB
ED 4B 5D 18 AF ED 42 09 23 20 DE EB DF 43 52 43
20 28 53 44 4C 43 29 20 3D A0 CD F1 07 DF 8D C9
CRC (SDLC) = 6022
