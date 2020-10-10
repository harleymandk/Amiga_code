	section prg,code_c
	include "sources:skidrow/registres.s"

;*************** INDEX FONCTIONS ***************

openlibrary=-552
forbid=-132
permit=-138
allocmem=-198
freemem=-210

;*************** PARAMETRES DE L'ECRAN ***************

nbplane=5
sizex=320+16
sizey=200

startx=129				;depart horizontal du trac�
starty=45				;depart vertical du trac�

tracex=320				;largeur de l'image a tracer � ...
					;... partir de startx

displayx=320				;largeur de l'image a visualiser � ...
					;... partir de startdisplayx
displayy=sizey				;hauteur de l'image a visualiser � ...
					;... partir de starty

startdisplayx=startx			;debut horizontal visible de l'image

stopx=displayx+startdisplayx-256	;fin horizontale visible de l'image 
stopy=displayy+starty-256		;fin verticale visible de l'image

ddf_strt=(startx-17)/2
ddf_stop=ddf_strt+(tracex/2-8)

modulo_pair=(sizex-tracex)/8
modulo_impair=(sizex-tracex)/8

planesize=sizey*sizex/8

;*************** CONSTANTES ***************

startdelay=30
timer=200
scrollhauteur=200-8
scrollspeed=2
motifsizex=15
motifsizey=12
logoplane=4
logosizex=80
logosizey=200
logosize=logosizex*logosizey/8
picsizex=256
picsizey=16
picsize=picsizex*picsizey/8
memory=$10000+2
copsize=32*4+9*4+2*4+nbplane*8+4+sizey*(4+40*4)+8*(4+4+4+4)

;32*4 (couleurs)
;9*4 (parametres screen)
;2*4 (compatiblit� AGA janvier 2003)
;nbplane*8 (adresses bitplanes)
;8*(4+4+4+4) (modif $0182 pour le scroll et modif $0182 pour le logo)
;4 ($FFFFFFFE)

;*************** PROGRAMME PRINCIPAL ***************

	bsr init

;----- recopie du logo -----

	move.w #$0000,bltcon1
	move.w #%0000010111001100,bltcon0
	move.w #0,bltbmod
	move.w #(sizex-logosizex)/8,bltdmod
	movea.l #logoadr,a0

	move.l plane0adr,bltdpth
	move.l a0,bltbpth
	move.w #logosizex/16+64*logosizey,bltsize
	bsr waitblit
	lea logosize(a0),a0

	move.l plane1adr,bltdpth
	move.l a0,bltbpth
	move.w #logosizex/16+64*logosizey,bltsize
	bsr waitblit
	lea logosize(a0),a0

	move.l plane2adr,bltdpth
	move.l a0,bltbpth
	move.w #logosizex/16+64*logosizey,bltsize
	bsr waitblit
	lea logosize(a0),a0

	move.l plane3adr,bltdpth
	move.l a0,bltbpth
	move.w #logosizex/16+64*logosizey,bltsize
	bsr waitblit
	lea logosize(a0),a0

;----- initialisations -----

	bsr mt_init
	move.w #%0000010111001100,bltcon0
	lea scrollstart,a4
	move.w #startdelay,d4		;premier timer
	move.b #$FF,d5			;curseur
	lea textstart,a5
	move.l plane0adr,a1
	lea 8*sizex/8+sizex/8-2*motifsizex+1(a1),a1
	movea.l a1,a6
	moveq #8/scrollspeed-1,d2

;----- programme principal -----

boucle:
	bsr waitvbl
	bsr mt_music

;----- scroll -----

	bsr waitblit
	move.w #%0000010111001100,bltcon0
	move.w #(sizex-(motifsizex+1)*16)/8,bltdmod
	move.w #(sizex-(motifsizex+1)*16)/8,bltbmod
	move.w #scrollspeed,d0
	ror.w #4,d0
	bset #1,d0
	move.w d0,bltcon1
	movea.l plane0adr,a0
	lea (scrollhauteur+8)*sizex/8-2(a0),a0
	move.l a0,bltbpth
	move.l a0,bltdpth
	move.w #(motifsizex+1)+64*8,bltsize

;----- affichage des bobs -----

	bsr waitblit
	move.w #%0000010111001100,bltcon0
	move.w #$0000,bltcon1
	move.w #(picsizex-16)/8,bltbmod
	move.w #(sizex-16)/8,bltdmod
	movea.l plane4adr,a0
	lea (sizex/16-motifsizex-1)*2(a0),a0
	lea motifstart,a2
	moveq #motifsizex-1,d0
	move.w #(motifsizex*motifsizey)-1,d1
bobloop0:
	move.w (a2),d3
	sub.w #1,(a2)+
	bge bobsuite1
	move.w #15,-2(a2)
bobsuite1:
	add.w d3,d3
	lea bobadr,a3
	lea (a3,d3.w),a3
	bsr waitblit
	move.l a3,bltbpth
	move.l a0,bltdpth
	move.w #1+64*16,bltsize
	lea 2(a0),a0
	dbf d0,bobsuite0
	lea 15*sizex/8+(sizex/16-motifsizex)*2(a0),a0
	moveq #motifsizex-1,d0
bobsuite0:
	dbf d1,bobloop0

;----- text du scroll ------

	dbf d2,scrollsuite1
	moveq #8/scrollspeed-1,d2
	tst.b (a4)
	bne scrollsuite0
	lea scrollstart,a4
scrollsuite0:
	moveq #0,d0
	move.b (a4)+,d0
	subi.b #$20,d0
	lsl.w #3,d0
	movea.l plane0adr,a0
	lea scrollhauteur*sizex/8+sizex/8-1(a0),a0
	movea.l #font8adr,a2
	lea (a2,d0.w),a2
	rept 8
	move.b (a2)+,(a0)
	lea sizex/8(a0),a0
	endr
scrollsuite1:

;----- affichage du texte -----

	tst.w d4
	blt textsuite5
	bne textsuite0
	bsr waitblit
	move.w #%0000000111001100,bltcon0
	move.w #0000,bltcon1
	move.w #$0000,bltbdat
	move.w #sizex/8-2*motifsizex,bltdmod
	movea.l plane0adr,a0
	lea (sizex/16-motifsizex-1)*2(a0),a0
	move.l a0,bltdpth
	move.w #motifsizex+64*(sizey-8),bltsize
	jmp textsuite0
textsuite5:
	move.b #$FF,d5
	moveq #0,d0
	move.b (a5)+,d0
	cmp.b #$A,d0
	beq textsuite2
	cmp.b #$1B,d0
	beq textsuite3
	subi.b #$20,d0
	lsl.w #3,d0
	lea font8adr,a0
	lea (a0,d0.w),a0
	movea.l a6,a2
	rept 8
	move.b (a0)+,(a2)
	lea sizex/8(a2),a2
	endr
	addq.l #1,a6
	jmp textsuite1

textsuite3:
	rept 8
	move.b #$00,(a6)
	lea sizex/8(a6),a6
	endr
	move.w #timer,d4
	tst.b (a5)
	bge textsuite4
	lea textstart,a5
textsuite4:
	movea.l plane0adr,a1
	lea 8*sizex/8+sizex/8-2*motifsizex+1(a1),a1
	movea.l a1,a6
	jmp textsuite1

textsuite2:
	rept 8
	move.b #$00,(a6)
	lea sizex/8(a6),a6
	endr
	lea 8*sizex/8(a1),a1
	movea.l a1,a6
	jmp textsuite1

textsuite0:
	subq.w #1,d4
textsuite1:

;----- curseur clignotant -----

	btst #3,d4
	bne cursorsuite0
	not.b d5
cursorsuite0:
	movea.l a6,a0
	rept 8
	move.b d5,(a0)
	lea sizex/8(a0),a0
	endr

	btst #6,$bfe001
	bne boucle
	bsr mt_end
	bsr fin
	rts

;*************** INITIALISATION ***************

init:
	
;memoire pour copperlist

	move.l $4,a6
	move.l #copsize,d0
	move.l #memory,d1
	jsr allocmem(a6)
	move.l d0,copadr

;memoire pour bitplane 0

	move.l #planesize,d0
	move.l #memory,d1
	jsr allocmem(a6)
	move.l d0,plane0adr

;memoire pour bitplane 1

	move.l #planesize,d0
	move.l #memory,d1
	jsr allocmem(a6)
	move.l d0,plane1adr

;memoire pour bitplane 2

	move.l #planesize,d0
	move.l #memory,d1
	jsr allocmem(a6)
	move.l d0,plane2adr

;memoire pour bitplane 3

	move.l #planesize,d0
	move.l #memory,d1
	jsr allocmem(a6)
	move.l d0,plane3adr

;memoire pour bitplane 4

	move.l #planesize,d0
	move.l #memory,d1
	jsr allocmem(a6)
	move.l d0,plane4adr

;creation de la copperlist

	move.l copadr,a0

	movea.l #logoadr,a1
	lea logoplane*logosize(a1),a1
	move.w #$0180,d0
	rept 16
	move.w d0,(a0)+
	addq.w #2,d0
	move.w (a1)+,(a0)+
	endr

	move.l #$01A00000,(a0)+
	move.l #$01A20FFF,(a0)+
	move.l #$01A40000,(a0)+
	move.l #$01A60000,(a0)+
	move.l #$01A80000,(a0)+
	move.l #$01AA0000,(a0)+
	move.l #$01AC0000,(a0)+
	move.l #$01AE0000,(a0)+

	move.l #$01B00000,(a0)+
	move.l #$01B20000,(a0)+
	move.l #$01B40000,(a0)+
	move.l #$01B60000,(a0)+
	move.l #$01B80000,(a0)+
	move.l #$01BA0000,(a0)+
	move.l #$01BC0000,(a0)+
	move.l #$01BE0000,(a0)+

	move.w #$008E,(a0)+		;DIWSTRT
	move.w #starty*256+startdisplayx,(a0)+
	move.w #$0090,(a0)+		;DIWSTOP
	move.w #stopy*256+stopx,(a0)+
	move.w #$0100,(a0)+		;BPLCON0
	move.w #nbplane,d0
	ror.w #4,d0
	bset #9,d0
	move.w d0,(a0)+
	move.w #$0102,(a0)+		;BPLCON1
	move.w #$0000,(a0)+
	move.w #$0104,(a0)+		;BPLCON2
	move.w #$0000,(a0)+
	move.w #$0092,(a0)+		;DDFSTRT
	move.w #ddf_strt,(a0)+
	move.w #$0094,(a0)+		;DDFSTOP
	move.w #ddf_stop,(a0)+
	move.w #$0108,(a0)+		;BPL1MOD
	move.w #modulo_pair,(a0)+
	move.w #$010A,(a0)+		;BPL2MOD
	move.w #modulo_impair,(a0)+
	move.l #$01FC0000,(a0)+
	move.l #$01060000,(a0)+

	move.l plane0adr,d0
	move.w #$E0,(a0)+
	swap d0
	move.w d0,(a0)+
	move.w #$E2,(a0)+
	swap d0
	move.w d0,(a0)+

	move.l plane1adr,d0
	move.w #$E4,(a0)+
	swap d0
	move.w d0,(a0)+
	move.w #$E6,(a0)+
	swap d0
	move.w d0,(a0)+

	move.l plane2adr,d0
	move.w #$E8,(a0)+
	swap d0
	move.w d0,(a0)+
	move.w #$EA,(a0)+
	swap d0
	move.w d0,(a0)+

	move.l plane3adr,d0
	move.w #$EC,(a0)+
	swap d0
	move.w d0,(a0)+
	move.w #$EE,(a0)+
	swap d0
	move.w d0,(a0)+

	move.l plane4adr,d0
	move.w #$F0,(a0)+
	swap d0
	move.w d0,(a0)+
	move.w #$F2,(a0)+
	swap d0
	move.w d0,(a0)+

	move.w #starty,d0
	lsl.w #8,d0
	or.w #$3D+(20-motifsizex)*8,d0

	move.w #motifsizey-1,d2
coploop1:
	move.w #16-1,d1
coploop0:
	move.w d0,(a0)+
	addi.w #$0100,d0
	move.w #$FFFE,(a0)+
	lea color0start,a1
	lea 10*2(a1),a1
	rept motifsizex
	move.w #$01A0,(a0)+
	move.w (a1),(a0)+
	move.w #$01A0,(a0)+
	move.w (a1)+,(a0)+
	endr
	dbf d1,coploop0
	lea color0start,a1
	move.w (a1),d1
	rept 44
	move.w 2(a1),(a1)+
	endr
	move.w d1,(a1)
	dbf d2,coploop1

	rept 8
	move.w d0,(a0)+
	addi.w #$0100,d0
	move.w #$FFFE,(a0)+
	move.l #$01820BBF,(a0)+
	move.w d0,d1
	move.b #$01,d1
	move.w d1,(a0)+
	move.w #$FFFE,(a0)+
	move.l #$01820FFF,(a0)+
	endr
	
	move.l #$FFFFFFFE,(a0)

;modif DMA,...
	
	jsr forbid(a6)
	move.w #$03F0,dmacon		;couper tous les DMA
	move.l copadr,cop1lch		;adresse coplist
	clr.w copjmp1			;copjump1
	move.w #$87C0,dmacon		;BLTEN, COPEN, BPLEN et BLTPRI

	rts
	
;*************** FIN PROGRAMME ***************

fin:

;restaurer la copperlist du dos

	move.l $4,a6
	move.l #grname,a1
	clr.l d0
	jsr openlibrary(a6)
	move.l d0,a4
	move.w #$03FF,dmacon
	move.l 38(a4),cop1lch
	clr.w copjmp1
	move.w #$83F0,dmacon
	jsr permit(a6)

;liberation memoire copper

	move.l copadr,a1
	move.l #copsize,d0
	jsr freemem(a6)	

;liberation memoire biplane 0

	move.l plane0adr,a1
	move.l #planesize,d0
	jsr freemem(a6)	

;liberation memoire biplane 1

	move.l plane1adr,a1
	move.l #planesize,d0
	jsr freemem(a6)	

;liberation memoire biplane 2

	move.l plane2adr,a1
	move.l #planesize,d0
	jsr freemem(a6)	

;liberation memoire biplane 3

	move.l plane3adr,a1
	move.l #planesize,d0
	jsr freemem(a6)	

;liberation memoire biplane 4

	move.l plane4adr,a1
	move.l #planesize,d0
	jsr freemem(a6)	

	rts
	
;*************** WAITBLIT ***************

waitblit:
	btst #14,dmaconr
	bne waitblit
	rts
	
;*************** LEFTCLICK ***************

leftclick:
	btst #6,$BFE001
	bne leftclick
	rts

;*************** ATTENTE ***************

attente:

attente1:
	cmp.b #$39,$bfec01
	bne attente1
attente2:
	cmp.b #$37,$bfec01
	bne attente2
	rts

;*************** WAITVBL ***************

waitvbl:
	or.b #$00,vhposr
	bne waitvbl	
	rts

;*************** PROPACKER PLAYER ***************

*****************************************
* Pro-Packer v2.1 Replay-Routine.	*
* Based upon the PT1.1B-Replayer	*
* by Lars 'ZAP' Hamre/Amiga Freelancers.*
* Modified by Estrup/Static Bytes.	*
*****************************************

; A LITTLE BUG HAS BEEN FOUND AND FIXED IN THE OFFICIAL ONE.
; Revolution / ANALOG.


mt_lev6use=		1		; 0=NO, 1=YES
mt_finetuneused=	0		; 0=NO, 1=YES

mt_init	LEA	mt_data,A0
	MOVE.L	A0,mt_SongDataPtr
	LEA	250(A0),A1
	MOVE.W	#511,D0
	MOVEQ	#0,D1
mtloop	MOVE.L	D1,D2
	SUBQ.W	#1,D0
mtloop2	MOVE.B	(A1)+,D1
	CMP.W	D2,D1
	BGT.S	mtloop
	DBRA	D0,mtloop2
	ADDQ	#1,D2

	MOVE.W	D2,D3
	MULU	#128,D3
	ADD.L	#766,D3
	ADD.L	mt_SongDataPtr(PC),D3
	MOVE.L	D3,mt_LWTPtr

	LEA	mt_SampleStarts(PC),A1
	MULU	#128,D2
	ADD.L	#762,D2
	ADD.L	(A0,D2.L),D2
	ADD.L	mt_SongDataPtr(PC),D2
	ADDQ.L	#4,D2
	MOVE.L	D2,A2
	MOVEQ	#30,D0
mtloop3	MOVE.L	A2,(A1)+
	MOVEQ	#0,D1
	MOVE.W	(A0),D1
	ADD.L	D1,D1
	ADD.L	D1,A2
	LEA	8(A0),A0
	DBRA	D0,mtloop3

	OR.B	#2,$BFE001
	lea	mt_speed(PC),A4
	MOVE.B	#6,(A4)
	CLR.B	mt_counter-mt_speed(A4)
	CLR.B	mt_SongPos-mt_speed(A4)
	CLR.W	mt_PatternPos-mt_speed(A4)
mt_end	LEA	$DFF096,A0
	CLR.W	$12(A0)
	CLR.W	$22(A0)
	CLR.W	$32(A0)
	CLR.W	$42(A0)
	MOVE.W	#$F,(A0)
	RTS

mt_music
	MOVEM.L	D0-D4/D7/A0-A6,-(SP)
	LEA	0.w,A4
	ADDQ.B	#1,mt_counter
	MOVE.B	mt_counter(PC),D0
	CMP.B	mt_speed(PC),D0
	BLO.S	mt_NoNewNote
	CLR.B	mt_counter
	TST.B	mt_PattDelTime2
	BEQ.S	mt_GetNewNote
	BSR.S	mt_NoNewAllChannels
	BRA.W	mt_dskip

mt_NoNewNote
	BSR.S	mt_NoNewAllChannels
	BRA.W	mt_NoNewPosYet

mt_NoNewAllChannels
	LEA	$DFF090,A5
	LEA	mt_chan1temp-44(PC),A6
	BSR.W	mt_CheckEfx
	BSR.W	mt_CheckEfx
	BSR.W	mt_CheckEfx
	BRA.W	mt_CheckEfx

mt_GetNewNote
	MOVE.L	mt_SongDataPtr(PC),A0
	LEA	(A0),A3
	LEA	122(A0),A2	;pattpo
	LEA	762(A0),A0	;patterndata
	CLR.W	mt_DMACONtemp

	LEA	$DFF090,A5
	LEA	mt_chan1temp-44(PC),A6
	BSR.S	mt_DoVoice
	BSR.S	mt_DoVoice
	BSR.B	mt_DoVoice
	BSR.B	mt_DoVoice
	BRA.W	mt_SetDMA

mt_DoVoice
	MOVEQ	#0,D0
	MOVEQ	#0,D1
	MOVE.B	mt_SongPos(PC),D0
	LEA	128(A2),A2
	MOVE.B	(A2,D0.W),D1
	MOVE.W	mt_PatternPos(PC),D2
	LSL	#7,D1
	LSR.W	#1,D2
	ADD.W	D2,D1
	LEA	$10(A5),A5
	LEA	44(A6),A6

	TST.L	(A6)
	BNE.S	mt_plvskip
	BSR.W	mt_PerNop
mt_plvskip
	MOVE.W	(A0,D1.W),D1
	LSL.W	#2,D1
	MOVE.L	A0,-(sp)
	MOVE.L	mt_LWTPtr(PC),A0
	MOVE.L	(A0,D1.W),(A6)
	MOVE.L	(sp)+,A0
	MOVE.B	2(A6),D2
	AND.L	#$F0,D2
	LSR.B	#4,D2
	MOVE.B	(A6),D0
	AND.B	#$F0,D0
	OR.B	D0,D2
	BEQ.B	mt_SetRegs
	MOVEQ	#0,D3
	LEA	mt_SampleStarts(PC),A1
	SUBQ	#1,D2
	MOVE	D2,D4
	ADD	D2,D2
	ADD	D2,D2
	LSL	#3,D4
	MOVE.L	(A1,D2.L),4(A6)
	MOVE.W	(A3,D4.W),8(A6)
	MOVE.W	(A3,D4.W),40(A6)
	MOVE.W	2(A3,D4.W),18(A6)
	MOVE.L	4(A6),D2	; Get start
	MOVE.W	4(A3,D4.W),D3	; Get repeat
	BEQ.S	mt_NoLoop
	MOVE.W	D3,D0		; Get repeat
	ADD.W	D3,D3
	ADD.L	D3,D2		; Add repeat
	ADD.W	6(A3,D4.W),D0	; Add replen
	MOVE.W	D0,8(A6)

mt_NoLoop
	MOVE.L	D2,10(A6)
	MOVE.L	D2,36(A6)
	MOVE.W	6(A3,D4.W),14(A6)	; Save replen
	MOVE.B	19(A6),9(A5)	; Set volume
mt_SetRegs
	MOVE.W	(A6),D0
	AND.W	#$0FFF,D0
	BEQ.W	mt_CheckMoreEfx	; If no note

	IF mt_finetuneused=1 THEN
	MOVE.W	2(A6),D0
	AND.W	#$0FF0,D0
	CMP.W	#$0E50,D0
	BEQ.S	mt_DoSetFineTune
	ENDC

	MOVE.B	2(A6),D0
	AND.B	#$0F,D0
	CMP.B	#3,D0	; TonePortamento
	BEQ.S	mt_ChkTonePorta
	CMP.B	#5,D0
	BEQ.S	mt_ChkTonePorta
	CMP.B	#9,D0	; Sample Offset
	BNE.S	mt_SetPeriod
	BSR.W	mt_CheckMoreEfx
	BRA.S	mt_SetPeriod

mt_ChkTonePorta
	BSR.W	mt_SetTonePorta
	BRA.W	mt_CheckMoreEfx

mt_DoSetFineTune
	BSR.W	mt_SetFineTune

mt_SetPeriod
	MOVEM.L	D1/A1,-(SP)
	MOVE.W	(A6),D1
	AND.W	#$0FFF,D1

	IF mt_finetuneused=0 THEN
	MOVE.W	D1,16(A6)

	ELSE
mt_SetPeriod2
	LEA	mt_PeriodTable(PC),A1
	MOVEQ	#36,D7
mt_ftuloop
	CMP.W	(A1)+,D1
	BHS.S	mt_ftufound
	DBRA	D7,mt_ftuloop
mt_ftufound
	MOVEQ	#0,D1
	MOVE.B	18(A6),D1
	LSL	#3,D1
	MOVE	D1,D0
	LSL	#3,D1
	ADD	D0,D1
	MOVE.W	-2(A1,D1.W),16(A6)
	ENDC

	MOVEM.L	(SP)+,D1/A1

	MOVE.W	2(A6),D0
	AND.W	#$0FF0,D0
	CMP.W	#$0ED0,D0 ; Notedelay
	BEQ.W	mt_CheckMoreEfx

	MOVE.W	20(A6),$DFF096
	BTST	#2,30(A6)
	BNE.S	mt_vibnoc
	CLR.B	27(A6)
mt_vibnoc
	BTST	#6,30(A6)
	BNE.S	mt_trenoc
	CLR.B	29(A6)
mt_trenoc
	MOVE.L	4(A6),(A5)	; Set start
	MOVE.W	8(A6),4(A5)	; Set length
	MOVE.W	16(A6),6(A5)	; Set period
	MOVE.W	20(A6),D0
	OR.W	D0,mt_DMACONtemp
	BRA.W	mt_CheckMoreEfx
 
mt_SetDMA
	IF mt_lev6use=1 THEN
	lea	$bfd000,a3
	move.b	#$7f,$d00(a3)
	move.w	#$2000,$dff09c
	move.w	#$a000,$dff09a
	move.l	$78.w,mt_oldirq
	move.l	#mt_irq1,$78.w
	moveq	#0,d0
	move.b	d0,$e00(a3)
	move.b	#$a8,$400(a3)
	move.b	d0,$500(a3)
	move.b	#$11,$e00(a3)
	move.b	#$81,$d00(a3)
	OR.W	#$8000,mt_DMACONtemp
	BRA.w	mt_dskip

	ELSE
	OR.W	#$8000,mt_DMACONtemp
	bsr.w	mt_WaitDMA
	ENDC

	IF mt_lev6use=1 THEN
mt_irq1:tst.b	$bfdd00
	MOVE.W	mt_dmacontemp(pc),$DFF096
	move.w	#$2000,$dff09c
	move.l	#mt_irq2,$78.w
	rte

	ELSE
	MOVE.W	mt_dmacontemp(pc),$DFF096
	bsr.w	mt_WaitDMA
	ENDC

	IF mt_lev6use=1 THEN
mt_irq2:tst.b	$bfdd00
	movem.l	a5-a6,-(a7)
	ENDC

	LEA	$DFF0A0,A5
	LEA	mt_chan1temp(PC),A6
	MOVE.L	10(A6),(A5)
	MOVE.W	14(A6),4(A5)
	MOVE.L	54(A6),$10(A5)
	MOVE.W	58(A6),$14(A5)
	MOVE.L	98(A6),$20(A5)
	MOVE.W	102(A6),$24(A5)
	MOVE.L	142(A6),$30(A5)
	MOVE.W	146(A6),$34(A5)

	IF mt_lev6use=1 THEN
	move.b	#0,$bfde00
	move.b	#$7f,$bfdd00
	move.l	mt_oldirq(pc),$78.w
	move.w	#$2000,$dff09c
	movem.l	(a7)+,a5-a6
	rte
	ENDC

mt_dskip
	lea	mt_speed(PC),A4
	ADDQ.W	#4,mt_PatternPos-mt_speed(A4)
	MOVE.B	mt_PattDelTime-mt_speed(A4),D0
	BEQ.S	mt_dskc
	MOVE.B	D0,mt_PattDelTime2-mt_speed(A4)
	CLR.B	mt_PattDelTime-mt_speed(A4)
mt_dskc	TST.B	mt_PattDelTime2-mt_speed(A4)
	BEQ.S	mt_dska
	SUBQ.B	#1,mt_PattDelTime2-mt_speed(A4)
	BEQ.S	mt_dska
	SUBQ.W	#4,mt_PatternPos-mt_speed(A4)
mt_dska	TST.B	mt_PBreakFlag-mt_speed(A4)
	BEQ.S	mt_nnpysk
	SF	mt_PBreakFlag-mt_speed(A4)
	MOVEQ	#0,D0
	MOVE.B	mt_PBreakPos(PC),D0
	CLR.B	mt_PBreakPos-mt_speed(A4)
	LSL	#2,D0
	MOVE.W	D0,mt_PatternPos-mt_speed(A4)
mt_nnpysk
	CMP.W	#256,mt_PatternPos-mt_speed(A4)
	BLO.S	mt_NoNewPosYet
mt_NextPosition	
	MOVEQ	#0,D0
	MOVE.B	mt_PBreakPos(PC),D0
	LSL	#2,D0
	MOVE.W	D0,mt_PatternPos-mt_speed(A4)
	CLR.B	mt_PBreakPos-mt_speed(A4)
	CLR.B	mt_PosJumpFlag-mt_speed(A4)
	ADDQ.B	#1,mt_SongPos-mt_speed(A4)
	AND.B	#$7F,mt_SongPos-mt_speed(A4)
	MOVE.B	mt_SongPos(PC),D1
	MOVE.L	mt_SongDataPtr(PC),A0
	CMP.B	248(A0),D1
	BLO.S	mt_NoNewPosYet
	CLR.B	mt_SongPos-mt_speed(A4)
mt_NoNewPosYet	
	TST.B	mt_PosJumpFlag-mt_speed(A4)
	BNE.S	mt_NextPosition
	MOVEM.L	(SP)+,D0-D4/D7/A0-A6
	RTS

mt_CheckEfx
	LEA	$10(A5),A5
	LEA	44(A6),A6
	BSR.W	mt_UpdateFunk
	MOVE.W	2(A6),D0
	AND.W	#$0FFF,D0
	BEQ.S	mt_PerNop
	MOVE.B	2(A6),D0
	MOVEQ	#$0F,D1
	AND.L	D1,D0
	BEQ.S	mt_Arpeggio
	SUBQ	#1,D0
	BEQ.W	mt_PortaUp
	SUBQ	#1,D0
	BEQ.W	mt_PortaDown
	SUBQ	#1,D0
	BEQ.W	mt_TonePortamento
	SUBQ	#1,D0
	BEQ.W	mt_Vibrato
	SUBQ	#1,D0
	BEQ.W	mt_TonePlusVolSlide
	SUBQ	#1,D0
	BEQ.W	mt_VibratoPlusVolSlide
	SUBQ	#8,D0
	BEQ.W	mt_E_Commands
SetBack	MOVE.W	16(A6),6(A5)
	ADDQ	#7,D0
	BEQ.W	mt_Tremolo
	SUBQ	#3,D0
	BEQ.W	mt_VolumeSlide
mt_Return2
	RTS

mt_PerNop
	MOVE.W	16(A6),6(A5)
	RTS

mt_Arpeggio
	MOVEQ	#0,D0
	MOVE.B	mt_counter(PC),D0
	DIVS	#3,D0
	SWAP	D0
	TST.W	D0
	BEQ.S	mt_Arpeggio2
	SUBQ	#2,D0
	BEQ.S	mt_Arpeggio1
	MOVEQ	#0,D0
	MOVE.B	3(A6),D0
	LSR.B	#4,D0
	BRA.S	mt_Arpeggio3

mt_Arpeggio2
	MOVE.W	16(A6),6(A5)
	RTS

mt_Arpeggio1
	MOVE.B	3(A6),D0
	AND.W	#15,D0
mt_Arpeggio3
	ADD.W	D0,D0
	LEA	mt_PeriodTable(PC),A0

	IF mt_finetuneused=1 THEN
	MOVEQ	#0,D1
	MOVE.B	18(A6),D1
	LSL	#3,D1
	MOVE	D1,D2
	LSL	#3,D1
	ADD	D2,D1
	ADD.L	D1,A0
	ENDC

	MOVE.W	16(A6),D1
	MOVEQ	#36,D7
mt_arploop
	CMP.W	(A0)+,D1
	BHS.S	mt_Arpeggio4
	DBRA	D7,mt_arploop
	RTS

mt_Arpeggio4
	MOVE.W	-2(A0,D0.W),6(A5)
	RTS

mt_FinePortaUp
	TST.B	mt_counter
	BNE.S	mt_Return2
	MOVE.B	#$0F,mt_LowMask
mt_PortaUp
	MOVEQ	#0,D0
	MOVE.B	3(A6),D0
	AND.B	mt_LowMask(PC),D0
	MOVE.B	#$FF,mt_LowMask
	SUB.W	D0,16(A6)
	MOVE.W	16(A6),D0
	AND.W	#$0FFF,D0
	CMP.W	#113,D0
	BPL.S	mt_PortaUskip
	AND.W	#$F000,16(A6)
	OR.W	#113,16(A6)
mt_PortaUskip
	MOVE.W	16(A6),D0
	AND.W	#$0FFF,D0
	MOVE.W	D0,6(A5)
	RTS	
 
mt_FinePortaDown
	TST.B	mt_counter
	BNE.W	mt_Return2
	MOVE.B	#$0F,mt_LowMask
mt_PortaDown
	CLR.W	D0
	MOVE.B	3(A6),D0
	AND.B	mt_LowMask(PC),D0
	MOVE.B	#$FF,mt_LowMask
	ADD.W	D0,16(A6)
	MOVE.W	16(A6),D0
	AND.W	#$0FFF,D0
	CMP.W	#856,D0
	BMI.S	mt_PortaDskip
	AND.W	#$F000,16(A6)
	OR.W	#856,16(A6)
mt_PortaDskip
	MOVE.W	16(A6),D0
	AND.W	#$0FFF,D0
	MOVE.W	D0,6(A5)
	RTS

mt_SetTonePorta
	MOVEM.L	A0,-(SP)
	MOVE.W	(A6),D2
	AND.W	#$0FFF,D2
	LEA	mt_PeriodTable(PC),A0

	IF	mt_finetuneused=1 THEN
	MOVEQ	#0,D0
	MOVE.B	18(A6),D0
	ADD	D0,D0
	MOVE	D0,D7
	ADD	D0,D0
	ADD	D0,D0
	ADD	D0,D7
	LSL	#3,D0
	ADD	D7,D0
	ADD.L	D0,A0
	ENDC

	MOVEQ	#0,D0
mt_StpLoop
	CMP.W	(A0,D0.W),D2
	BHS.S	mt_StpFound
	ADDQ	#2,D0
	CMP.W	#37*2,D0
	BLO.S	mt_StpLoop
	MOVEQ	#35*2,D0
mt_StpFound
	BTST	#3,18(A6)
	BEQ.S	mt_StpGoss
	TST.W	D0
	BEQ.S	mt_StpGoss
	SUBQ	#2,D0
mt_StpGoss
	MOVE.W	(A0,D0.W),D2
	MOVE.L	(SP)+,A0
	MOVE.W	D2,24(A6)
	MOVE.W	16(A6),D0
	CLR.B	22(A6)
	CMP.W	D0,D2
	BEQ.S	mt_ClearTonePorta
	BGE.W	mt_Return2
	MOVE.B	#1,22(A6)
	RTS

mt_ClearTonePorta
	CLR.W	24(A6)
	RTS

mt_TonePortamento
	MOVE.B	3(A6),D0
	BEQ.S	mt_TonePortNoChange
	MOVE.B	D0,23(A6)
	CLR.B	3(A6)
mt_TonePortNoChange
	TST.W	24(A6)
	BEQ.W	mt_Return2
	MOVEQ	#0,D0
	MOVE.B	23(A6),D0
	TST.B	22(A6)
	BNE.S	mt_TonePortaUp
mt_TonePortaDown
	ADD.W	D0,16(A6)
	MOVE.W	24(A6),D0
	CMP.W	16(A6),D0
	BGT.S	mt_TonePortaSetPer
	MOVE.W	24(A6),16(A6)
	CLR.W	24(A6)
	BRA.S	mt_TonePortaSetPer

mt_TonePortaUp
	SUB.W	D0,16(A6)
	MOVE.W	24(A6),D0
	CMP.W	16(A6),D0
	BLT.S	mt_TonePortaSetPer
	MOVE.W	24(A6),16(A6)
	CLR.W	24(A6)

mt_TonePortaSetPer
	MOVE.W	16(A6),D2
	MOVE.B	31(A6),D0
	AND.B	#$0F,D0
	BEQ.S	mt_GlissSkip
	LEA	mt_PeriodTable(PC),A0

	IF mt_finetuneused=1 THEN
	MOVEQ	#0,D0
	MOVE.B	18(A6),D0
	LSL	#3,D0
	MOVE	D0,D1
	LSL	#3,D0
	ADD	D1,D0
	ADD.L	D0,A0
	ENDC

	MOVEQ	#0,D0
mt_GlissLoop
	CMP.W	(A0,D0.W),D2
	BHS.S	mt_GlissFound
	ADDQ	#2,D0
	CMP.W	#36*2,D0
	BLO.S	mt_GlissLoop
	MOVEQ	#35*2,D0
mt_GlissFound
	MOVE.W	(A0,D0.W),D2
mt_GlissSkip
	MOVE.W	D2,6(A5) ; Set period
	RTS

mt_Vibrato
	MOVE.B	3(A6),D0
	BEQ.S	mt_Vibrato2
	MOVE.B	26(A6),D2
	AND.B	#$0F,D0
	BEQ.S	mt_vibskip
	AND.B	#$F0,D2
	OR.B	D0,D2
mt_vibskip
	MOVE.B	3(A6),D0
	AND.B	#$F0,D0
	BEQ.S	mt_vibskip2
	AND.B	#$0F,D2
	OR.B	D0,D2
mt_vibskip2
	MOVE.B	D2,26(A6)
mt_Vibrato2
	MOVE.B	27(A6),D0
	LEA	mt_VibratoTable(PC),A4
	LSR.W	#2,D0
	AND.W	#$001F,D0
	MOVE.B	30(A6),D2
	AND.W	#$03,D2
	BEQ.S	mt_vib_sine
	LSL.B	#3,D0
	CMP.B	#1,D2
	BEQ.S	mt_vib_rampdown
	MOVE.B	#255,D2
	BRA.S	mt_vib_set
mt_vib_rampdown
	TST.B	27(A6)
	BPL.S	mt_vib_rampdown2
	MOVE.B	#255,D2
	SUB.B	D0,D2
	BRA.S	mt_vib_set
mt_vib_rampdown2
	MOVE.B	D0,D2
	BRA.S	mt_vib_set
mt_vib_sine
	MOVE.B	0(A4,D0.W),D2
mt_vib_set
	MOVE.B	26(A6),D0
	AND.W	#15,D0
	MULU	D0,D2
	LSR.W	#7,D2
	MOVE.W	16(A6),D0
	TST.B	27(A6)
	BMI.S	mt_VibratoNeg
	ADD.W	D2,D0
	BRA.S	mt_Vibrato3
mt_VibratoNeg
	SUB.W	D2,D0
mt_Vibrato3
	MOVE.W	D0,6(A5)
	MOVE.B	26(A6),D0
	LSR.W	#2,D0
	AND.W	#$003C,D0
	ADD.B	D0,27(A6)
	RTS

mt_TonePlusVolSlide
	BSR.W	mt_TonePortNoChange
	BRA.W	mt_VolumeSlide

mt_VibratoPlusVolSlide
	BSR.S	mt_Vibrato2
	BRA.W	mt_VolumeSlide

mt_Tremolo
	MOVE.B	3(A6),D0
	BEQ.S	mt_Tremolo2
	MOVE.B	28(A6),D2
	AND.B	#$0F,D0
	BEQ.S	mt_treskip
	AND.B	#$F0,D2
	OR.B	D0,D2
mt_treskip
	MOVE.B	3(A6),D0
	AND.B	#$F0,D0
	BEQ.S	mt_treskip2
	AND.B	#$0F,D2
	OR.B	D0,D2
mt_treskip2
	MOVE.B	D2,28(A6)
mt_Tremolo2
	MOVE.B	29(A6),D0
	LEA	mt_VibratoTable(PC),A4
	LSR.W	#2,D0
	AND.W	#$001F,D0
	MOVEQ	#0,D2
	MOVE.B	30(A6),D2
	LSR.B	#4,D2
	AND.B	#$03,D2
	BEQ.S	mt_tre_sine
	LSL.B	#3,D0
	CMP.B	#1,D2
	BEQ.S	mt_tre_rampdown
	MOVE.B	#255,D2
	BRA.S	mt_tre_set
mt_tre_rampdown
	TST.B	27(A6)
	BPL.S	mt_tre_rampdown2
	MOVE.B	#255,D2
	SUB.B	D0,D2
	BRA.S	mt_tre_set
mt_tre_rampdown2
	MOVE.B	D0,D2
	BRA.S	mt_tre_set
mt_tre_sine
	MOVE.B	0(A4,D0.W),D2
mt_tre_set
	MOVE.B	28(A6),D0
	AND.W	#15,D0
	MULU	D0,D2
	LSR.W	#6,D2
	MOVEQ	#0,D0
	MOVE.B	19(A6),D0
	TST.B	29(A6)
	BMI.S	mt_TremoloNeg
	ADD.W	D2,D0
	BRA.S	mt_Tremolo3
mt_TremoloNeg
	SUB.W	D2,D0
mt_Tremolo3
	BPL.S	mt_TremoloSkip
	CLR.W	D0
mt_TremoloSkip
	CMP.W	#$40,D0
	BLS.S	mt_TremoloOk
	MOVE.W	#$40,D0
mt_TremoloOk
	MOVE.W	D0,8(A5)
	MOVE.B	28(A6),D0
	LSR.W	#2,D0
	AND.W	#$003C,D0
	ADD.B	D0,29(A6)
	RTS

mt_SampleOffset
	MOVEQ	#0,D0
	MOVE.B	3(A6),D0
	BEQ.S	mt_sononew
	MOVE.B	D0,32(A6)
mt_sononew
	MOVE.B	32(A6),D0
	LSL.W	#7,D0
	CMP.W	8(A6),D0
	BGE.S	mt_sofskip
	SUB.W	D0,8(A6)
	ADD.W	D0,D0
	ADD.L	D0,4(A6)
	RTS
mt_sofskip
	MOVE.W	#$0001,8(A6)
	RTS

mt_VolumeSlide
	MOVEQ	#0,D0
	MOVE.B	3(A6),D0
	LSR.B	#4,D0
	TST.B	D0
	BEQ.S	mt_VolSlideDown
mt_VolSlideUp
	ADD.B	D0,19(A6)
	CMP.B	#$40,19(A6)
	BMI.S	mt_vsuskip
	MOVE.B	#$40,19(A6)
mt_vsuskip
	MOVE.B	19(A6),9(A5)
	RTS

mt_VolSlideDown
	MOVE.B	3(A6),D0
	AND.W	#$0F,D0
mt_VolSlideDown2
	SUB.B	D0,19(A6)
	BPL.S	mt_vsdskip
	CLR.B	19(A6)
mt_vsdskip
	MOVE.B	19(A6),9(A5)
	RTS

mt_PositionJump
	MOVE.B	3(A6),D0
	SUBQ	#1,D0
	MOVE.B	D0,mt_SongPos
mt_pj2	CLR.B	mt_PBreakPos
	ST 	mt_PosJumpFlag
	RTS

mt_VolumeChange
	MOVE.B	3(A6),D0
	CMP.B	#$40,D0
	BLS.S	mt_VolumeOk
	MOVEQ	#$40,D0
mt_VolumeOk
	MOVE.B	D0,19(A6)
	MOVE.B	D0,9(A5)
	RTS

mt_PatternBreak
	MOVEQ	#0,D0
	MOVE.B	3(A6),D0
	MOVE.W	D0,D2
	LSR.B	#4,D0
	ADD	D0,D0
	MOVE	D0,D1
	ADD	D0,D0
	ADD	D0,D0
	ADD	D1,D0
	AND.B	#$0F,D2
	ADD.B	D2,D0
	CMP.B	#63,D0
	BHI.S	mt_pj2
	MOVE.B	D0,mt_PBreakPos
	ST	mt_PosJumpFlag
	RTS

mt_SetSpeed
	MOVE.B	3(A6),D0
	BEQ.W	mt_Return2
	CLR.B	mt_counter
	MOVE.B	D0,mt_speed
	RTS

mt_CheckMoreEfx
	BSR.W	mt_UpdateFunk
	MOVE.B	2(A6),D0
	AND.B	#$0F,D0
	SUB.B	#9,D0
	BEQ.W	mt_SampleOffset
	SUBQ	#2,D0
	BEQ.W	mt_PositionJump
	SUBQ	#1,D0
	BEQ.B	mt_VolumeChange
	SUBQ	#1,D0
	BEQ.S	mt_PatternBreak
	SUBQ	#1,D0
	BEQ.S	mt_E_Commands
	SUBQ	#1,D0
	BEQ.S	mt_SetSpeed
	BRA.W	mt_PerNop

mt_E_Commands
	MOVE.B	3(A6),D0
	AND.W	#$F0,D0
	LSR.B	#4,D0
	BEQ.S	mt_FilterOnOff
	SUBQ	#1,D0
	BEQ.W	mt_FinePortaUp
	SUBQ	#1,D0
	BEQ.W	mt_FinePortaDown
	SUBQ	#1,D0
	BEQ.S	mt_SetGlissControl
	SUBQ	#1,D0
	BEQ.B	mt_SetVibratoControl

	IF mt_finetuneused=1 THEN
	SUBQ	#1,D0
	BEQ.B	mt_SetFineTune
	SUBQ	#1,D0

	ELSE
	SUBQ	#2,D0
	ENDC

	BEQ.B	mt_JumpLoop
	SUBQ	#1,D0
	BEQ.W	mt_SetTremoloControl
	SUBQ	#2,D0
	BEQ.W	mt_RetrigNote
	SUBQ	#1,D0
	BEQ.W	mt_VolumeFineUp
	SUBQ	#1,D0
	BEQ.W	mt_VolumeFineDown
	SUBQ	#1,D0
	BEQ.W	mt_NoteCut
	SUBQ	#1,D0
	BEQ.W	mt_NoteDelay
	SUBQ	#1,D0
	BEQ.W	mt_PatternDelay
	BRA.W	mt_FunkIt

mt_FilterOnOff
	MOVE.B	3(A6),D0
	AND.B	#1,D0
	ADD.B	D0,D0
	AND.B	#$FD,$BFE001
	OR.B	D0,$BFE001
	RTS	

mt_SetGlissControl
	MOVE.B	3(A6),D0
	AND.B	#$0F,D0
	AND.B	#$F0,31(A6)
	OR.B	D0,31(A6)
	RTS

mt_SetVibratoControl
	MOVE.B	3(A6),D0
	AND.B	#$0F,D0
	AND.B	#$F0,30(A6)
	OR.B	D0,30(A6)
	RTS

mt_SetFineTune
	MOVE.B	3(A6),D0
	AND.B	#$0F,D0
	MOVE.B	D0,18(A6)
	RTS

mt_JumpLoop
	TST.B	mt_counter
	BNE.W	mt_Return2
	MOVE.B	3(A6),D0
	AND.B	#$0F,D0
	BEQ.S	mt_SetLoop
	TST.B	34(A6)
	BEQ.S	mt_jumpcnt
	SUBQ.B	#1,34(A6)
	BEQ.W	mt_Return2
mt_jmploop 	MOVE.B	33(A6),mt_PBreakPos
	ST	mt_PBreakFlag
	RTS

mt_jumpcnt
	MOVE.B	D0,34(A6)
	BRA.S	mt_jmploop

mt_SetLoop
	MOVE.W	mt_PatternPos(PC),D0
	LSR	#2,D0
	MOVE.B	D0,33(A6)
	RTS

mt_SetTremoloControl
	MOVE.B	3(A6),D0
	AND.B	#$0F,D0
	LSL.B	#4,D0
	AND.B	#$0F,30(A6)
	OR.B	D0,30(A6)
	RTS

mt_RetrigNote
	MOVE.L	D1,-(SP)
	MOVE.B	3(A6),D0
	AND.W	#$0F,D0
	BEQ.S	mt_rtnend
	MOVEQ	#0,d1
	MOVE.B	mt_counter(PC),D1
	BNE.S	mt_rtnskp
	MOVE.W	(A6),D1
	AND.W	#$0FFF,D1
	BNE.S	mt_rtnend
	MOVEQ	#0,D1
	MOVE.B	mt_counter(PC),D1
mt_rtnskp
	DIVU	D0,D1
	SWAP	D1
	TST.W	D1
	BNE.S	mt_rtnend
mt_DoRetrig
	MOVE.W	20(A6),$DFF096	; Channel DMA off
	MOVE.L	4(A6),(A5)	; Set sampledata pointer
	MOVE.W	8(A6),4(A5)	; Set length
	BSR.W	mt_WaitDMA
	MOVE.W	20(A6),D0
	BSET	#15,D0
	MOVE.W	D0,$DFF096
	BSR.W	mt_WaitDMA
	MOVE.L	10(A6),(A5)
	MOVE.L	14(A6),4(A5)
mt_rtnend
	MOVE.L	(SP)+,D1
	RTS

mt_VolumeFineUp
	TST.B	mt_counter
	BNE.W	mt_Return2
	MOVE.B	3(A6),D0
	AND.W	#$F,D0
	BRA.W	mt_VolSlideUp

mt_VolumeFineDown
	TST.B	mt_counter
	BNE.W	mt_Return2
	MOVE.B	3(A6),D0
	AND.W	#$0F,D0
	BRA.W	mt_VolSlideDown2

mt_NoteCut
	MOVE.B	3(A6),D0
	AND.W	#$0F,D0
	CMP.B	mt_counter(PC),D0
	BNE.W	mt_Return2
	CLR.B	19(A6)
	CLR.W	8(A5)
	RTS

mt_NoteDelay
	MOVE.B	3(A6),D0
	AND.W	#$0F,D0
	CMP.B	mt_Counter(PC),D0
	BNE.W	mt_Return2
	MOVE.W	(A6),D0
	BEQ.W	mt_Return2
	MOVE.L	D1,-(SP)
	BRA.W	mt_DoRetrig

mt_PatternDelay
	TST.B	mt_counter
	BNE.W	mt_Return2
	MOVE.B	3(A6),D0
	AND.W	#$0F,D0
	TST.B	mt_PattDelTime2
	BNE.W	mt_Return2
	ADDQ.B	#1,D0
	MOVE.B	D0,mt_PattDelTime
	RTS

mt_FunkIt
	TST.B	mt_counter
	BNE.W	mt_Return2
	MOVE.B	3(A6),D0
	AND.B	#$0F,D0
	LSL.B	#4,D0
	AND.B	#$0F,31(A6)
	OR.B	D0,31(A6)
	TST.B	D0
	BEQ.W	mt_Return2
mt_UpdateFunk
	MOVEM.L	D1/A0,-(SP)
	MOVEQ	#0,D0
	MOVE.B	31(A6),D0
	LSR.B	#4,D0
	BEQ.S	mt_funkend
	LEA	mt_FunkTable(PC),A0
	MOVE.B	(A0,D0.W),D0
	ADD.B	D0,35(A6)
	BTST	#7,35(A6)
	BEQ.S	mt_funkend
	CLR.B	35(A6)

	MOVE.L	10(A6),D0
	MOVEQ	#0,D1
	MOVE.W	14(A6),D1
	ADD.L	D1,D0
	ADD.L	D1,D0
	MOVE.L	36(A6),A0
	ADDQ.L	#1,A0
	CMP.L	D0,A0
	BLO.S	mt_funkok
	MOVE.L	10(A6),A0
mt_funkok
	MOVE.L	A0,36(A6)
	NEG.B	(A0)
	SUBQ.B	#1,(A0)
mt_funkend
	MOVEM.L	(SP)+,D1/A0
	RTS

mt_WaitDMA
	MOVEQ	#3,D0
mt_WaitDMA2
	MOVE.B	$DFF006,D1
mt_WaitDMA3
	CMP.B	$DFF006,D1
	BEQ.S	mt_WaitDMA3
	DBF	D0,mt_WaitDMA2
	RTS

mt_FunkTable dc.b 0,5,6,7,8,10,11,13,16,19,22,26,32,43,64,128

mt_VibratoTable	
	dc.b   0, 24, 49, 74, 97,120,141,161
	dc.b 180,197,212,224,235,244,250,253
	dc.b 255,253,250,244,235,224,212,197
	dc.b 180,161,141,120, 97, 74, 49, 24

mt_PeriodTable
; Tuning 0, Normal
	dc.w	856,808,762,720,678,640,604,570,538,508,480,453
	dc.w	428,404,381,360,339,320,302,285,269,254,240,226
	dc.w	214,202,190,180,170,160,151,143,135,127,120,113
; Tuning 1
	dc.w	850,802,757,715,674,637,601,567,535,505,477,450
	dc.w	425,401,379,357,337,318,300,284,268,253,239,225
	dc.w	213,201,189,179,169,159,150,142,134,126,119,113
; Tuning 2
	dc.w	844,796,752,709,670,632,597,563,532,502,474,447
	dc.w	422,398,376,355,335,316,298,282,266,251,237,224
	dc.w	211,199,188,177,167,158,149,141,133,125,118,112
; Tuning 3
	dc.w	838,791,746,704,665,628,592,559,528,498,470,444
	dc.w	419,395,373,352,332,314,296,280,264,249,235,222
	dc.w	209,198,187,176,166,157,148,140,132,125,118,111
; Tuning 4
	dc.w	832,785,741,699,660,623,588,555,524,495,467,441
	dc.w	416,392,370,350,330,312,294,278,262,247,233,220
	dc.w	208,196,185,175,165,156,147,139,131,124,117,110
; Tuning 5
	dc.w	826,779,736,694,655,619,584,551,520,491,463,437
	dc.w	413,390,368,347,328,309,292,276,260,245,232,219
	dc.w	206,195,184,174,164,155,146,138,130,123,116,109
; Tuning 6
	dc.w	820,774,730,689,651,614,580,547,516,487,460,434
	dc.w	410,387,365,345,325,307,290,274,258,244,230,217
	dc.w	205,193,183,172,163,154,145,137,129,122,115,109
; Tuning 7
	dc.w	814,768,725,684,646,610,575,543,513,484,457,431
	dc.w	407,384,363,342,323,305,288,272,256,242,228,216
	dc.w	204,192,181,171,161,152,144,136,128,121,114,108
; Tuning -8
	dc.w	907,856,808,762,720,678,640,604,570,538,508,480
	dc.w	453,428,404,381,360,339,320,302,285,269,254,240
	dc.w	226,214,202,190,180,170,160,151,143,135,127,120
; Tuning -7
	dc.w	900,850,802,757,715,675,636,601,567,535,505,477
	dc.w	450,425,401,379,357,337,318,300,284,268,253,238
	dc.w	225,212,200,189,179,169,159,150,142,134,126,119
; Tuning -6
	dc.w	894,844,796,752,709,670,632,597,563,532,502,474
	dc.w	447,422,398,376,355,335,316,298,282,266,251,237
	dc.w	223,211,199,188,177,167,158,149,141,133,125,118
; Tuning -5
	dc.w	887,838,791,746,704,665,628,592,559,528,498,470
	dc.w	444,419,395,373,352,332,314,296,280,264,249,235
	dc.w	222,209,198,187,176,166,157,148,140,132,125,118
; Tuning -4
	dc.w	881,832,785,741,699,660,623,588,555,524,494,467
	dc.w	441,416,392,370,350,330,312,294,278,262,247,233
	dc.w	220,208,196,185,175,165,156,147,139,131,123,117
; Tuning -3
	dc.w	875,826,779,736,694,655,619,584,551,520,491,463
	dc.w	437,413,390,368,347,328,309,292,276,260,245,232
	dc.w	219,206,195,184,174,164,155,146,138,130,123,116
; Tuning -2
	dc.w	868,820,774,730,689,651,614,580,547,516,487,460
	dc.w	434,410,387,365,345,325,307,290,274,258,244,230
	dc.w	217,205,193,183,172,163,154,145,137,129,122,115
; Tuning -1
	dc.w	862,814,768,725,684,646,610,575,543,513,484,457
	dc.w	431,407,384,363,342,323,305,288,272,256,242,228
	dc.w	216,203,192,181,171,161,152,144,136,128,121,114

mt_chan1temp	blk.l	5
		dc.w	1
		blk.w	21
		dc.w	2
		blk.w	21
		dc.w	4
		blk.w	21
		dc.w	8
		blk.w	11

mt_SampleStarts	blk.l	31,0

mt_SongDataPtr	dc.l 0
mt_LWTPtr	dc.l 0
mt_oldirq	dc.l 0

mt_speed	dc.b 6
mt_counter	dc.b 0
mt_SongPos	dc.b 0
mt_PBreakPos	dc.b 0
mt_PosJumpFlag	dc.b 0
mt_PBreakFlag	dc.b 0
mt_LowMask	dc.b 0
mt_PattDelTime	dc.b 0
mt_PattDelTime2	dc.b 0,0
mt_PatternPos	dc.w 0
mt_DMACONtemp	dc.w 0
mt_data:	incbin	"sources:skidrow/mod.blitz-packed"

;*************** VARIABLES ***************

plane0adr:	dc.l 0
plane1adr:	dc.l 0
plane2adr:	dc.l 0
plane3adr:	dc.l 0
plane4adr:	dc.l 0
font8adr:	incbin "sources:skidrow/uniplane.fnt"
bobadr:		incbin "sources:skidrow/square.raw"
logoadr:	incbin "sources:skidrow/skidrow-alex.raw"
oldirq:		dc.l 0
copadr:		dc.l 0
grname:		dc.b 'graphics.library',0
	even

;+++++ editer le texte sous cygnus avec set right border a 28
;+++++ ne pas enlever le dc.b -1
;+++++ ne pas depasser 22 lignes par page de texte
;+++++ separer les pages de textes par le char ESC
;+++++ mettre un char ESC comme dernier char du fichier

textstart:
	incbin "sources:skidrow/skidrow.txt"
	dc.b -1
	even

;+++++ aucun return dans le scrolltxt, l'editer en une seule ligne

scrollstart:
	incbin "sources:skidrow/skidrow.scrl"
	dc.b 0
	even

color0start:
	dc.w $0FFF
	dc.w $0EFF
	dc.w $0DFF
	dc.w $0CFF
	dc.w $0BFF

	dc.w $0AFF
	dc.w $09FF
	dc.w $08FF
	dc.w $07FF
	dc.w $06FF

	dc.w $05FF
	dc.w $04FF
	dc.w $03FF
	dc.w $02FF
	dc.w $01FF

	dc.w $00FF
	dc.w $00EF
	dc.w $00DF
	dc.w $00CF
	dc.w $00BF

	dc.w $00AF
	dc.w $009F
	dc.w $008F
	dc.w $007F
	dc.w $006F

	dc.w $005F
	dc.w $004F
	dc.w $003F
	dc.w $002F
	dc.w $001F

	dc.w $000F
	dc.w $011F
	dc.w $022F
	dc.w $033F
	dc.w $044F

	dc.w $055F
	dc.w $066F
	dc.w $077F
	dc.w $088F
	dc.w $099F

	dc.w $0AAF
	dc.w $0BBF
	dc.w $0CCF
	dc.w $0DDF
	dc.w $0EEF

color1start:
	dc.w $0F00
	dc.w $0F10
	dc.w $0F20
	dc.w $0F30
	dc.w $0F40

	dc.w $0F50
	dc.w $0F60
	dc.w $0F70
	dc.w $0F80
	dc.w $0F90

	dc.w $0FA0
	dc.w $0FB0
	dc.w $0FC0
	dc.w $0FD0
	dc.w $0FE0

	dc.w $0FF0
	dc.w $0EF1
	dc.w $0DF2
	dc.w $0CF3
	dc.w $0BF4

	dc.w $0AF5
	dc.w $09F6
	dc.w $08F7
	dc.w $07F8
	dc.w $06F9

	dc.w $05FA
	dc.w $04FB
	dc.w $03FC
	dc.w $02FD
	dc.w $01FE

	dc.w $00FF
	dc.w $01EE
	dc.w $02DD
	dc.w $03CC
	dc.w $04BB

	dc.w $05AA
	dc.w $0699
	dc.w $0788
	dc.w $0877
	dc.w $0966

	dc.w $0A55
	dc.w $0B44
	dc.w $0C33
	dc.w $0D22
	dc.w $0E11

color2start:
	dc.w $000F
	dc.w $010F
	dc.w $020F
	dc.w $030F
	dc.w $040F

	dc.w $050F
	dc.w $060F
	dc.w $070F
	dc.w $080F
	dc.w $090F

	dc.w $0A0F
	dc.w $0B0F
	dc.w $0C0F
	dc.w $0D0F
	dc.w $0E0F

	dc.w $0F0F
	dc.w $0E1E
	dc.w $0D2D
	dc.w $0C3C
	dc.w $0B4B

	dc.w $0A5A
	dc.w $0969
	dc.w $0878
	dc.w $0787
	dc.w $0696

	dc.w $05A5
	dc.w $04B4
	dc.w $03C3
	dc.w $02D2
	dc.w $01E1

	dc.w $00F0
	dc.w $00E1
	dc.w $00D2
	dc.w $00C3
	dc.w $00B4

	dc.w $00A5
	dc.w $0096
	dc.w $0087
	dc.w $0078
	dc.w $0069

	dc.w $005A
	dc.w $004B
	dc.w $003C
	dc.w $002D
	dc.w $001E

motifstart:
	dc.w 0,1,2,3,4,5,6,7,8,7,6,5,4,3,2
	dc.w 1,2,3,4,5,6,7,8,7,6,5,4,3,2,1
	dc.w 2,3,4,5,6,7,8,7,6,5,4,3,2,1,0
	dc.w 3,4,5,6,7,8,7,6,5,4,3,2,1,0,1
	dc.w 4,5,6,7,8,7,6,5,4,3,2,1,0,1,2
	dc.w 5,6,7,8,7,6,5,4,3,2,1,0,1,2,3
	dc.w 6,7,8,7,6,5,4,3,2,1,0,1,2,3,4
	dc.w 7,8,7,6,5,4,3,2,1,0,1,2,3,4,5
	dc.w 8,7,6,5,4,3,2,1,0,1,2,3,4,5,6
	dc.w 7,6,5,4,3,2,1,0,1,2,3,4,5,6,7
	dc.w 6,5,4,3,2,1,0,1,2,3,4,5,6,7,8
	dc.w 5,4,3,2,1,0,1,2,3,4,5,6,7,8,7
