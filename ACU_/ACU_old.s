* _________________________________________________________________ / / /_
*                                                                  / / /
* Flashtro.com                                              ____  / / /
* cracktro source                                           \   \/ / /
* ________________________________________ powered by AMIGA _\ \ \/ /_____
*                                                             \_\_\/
* ACU cracktro by PIRANHA OF THE PREDATORS FOR ACU 25/4/89
* no music in original
* 
* + MultiPlayers
* 
* possible to use external file "msg" for the scrolltext
* 
* resourced by VrS!

;ASMONE
ORIG

* ------------------------------------- choose your module player
;PT	; sound/noise/protracker
;HIP	; hippel player
FRED	; fred player
;SID	; sidmon 1
;SID2	; sidmon 2
;SA	; sonic arranger
;FC	; future composer 1.3
;FC14	; future composer 1.4
;MA	; music arranger
;DM	; delta music
;DMU	; DigiMusic

	IFND	ASMONE
	opt	c-,ow-,o+ 
	ifd	FC
	opt	o-	; warning : no optimization for FC/FC14/SID2 players
	endc
	ifd	FC14
	opt	o-
	endc
	ifd	SID2
	opt	o-
	endc
	ENDC

	IFD	ASMONE
	incdir	"dh0:acu/"
	ENDC
	
* ----------------------------------- offset execlib
OpenLib	equ	-552
CloseLib	equ	-414	
Forbid	equ	-132
Permit	equ	-138
Supervisor	equ	-30
AllocMem	equ	-$c6
AllocAbs	equ	-$cc
FreeMem	equ	-$d2

* ----------------------------------- offset gfxlib
MrgCop		equ	-210
MakeVPort	equ	-216
LoadView	equ	-222
WaitBlit	equ	-228
WaitTOF		equ	-270
OwnBlit		equ	-456
DisownBlit	equ	-462
ActiView=$22

* ----------------------------------- offset doslib
Open	equ	-30
Close	equ	-36
Read	equ	-42
* ----------------------------------- offset customs
BLTCON0=$40		
BLTCON1=$42
BLTAFWM=$44
BLTALWM=$46
BLTCPTH=$48
BLTCPTL=$4A
BLTBPTH=$4C
BLTBPTL=$4E
BLTAPTH=$50
BLTAPTL=$52
BLTDPTH=$54
BLTDPTL=$56
BLTSIZE=$58
BLTCMOD=$60
BLTBMOD=$62
BLTAMOD=$64
BLTDMOD=$66
BLTCDAT=$70
BLTBDAT=$72
BLTADAT=$74
DMACONR=$2

* ------------------------------------ offset dmacon
D_init=$8200
D_priB=$400
D_Btpl=$100
D_Copp=$80
D_Blit=$40
D_sprt=$20
D_disk=$10
D_CON=D_init|D_copp|D_Btpl|D_Blit|D_priB

* -------------------------------------
	section acu,code

	rsreset
sDma	rs.w	1
intena	rs.w	1
intvbl	rs.l	1
intcia	rs.l	1
GfxBase	rs.l	1
OldView	rs.l	1
sVBR	rs.l	1
Aga	rs.b	2
* ------
Timer	rs.l	1
flagVbl	rs.w	1
* ------ system fader
sav4vp	rs.l	1
numcol	rs.w	1
syspal	rs.w	32
* ------- 
ptBob		rs.l	1
ptPos		rs.l	1
ptFrames	rs.l	1
delayanim	rs.w	1
shiftbltcon0	rs.w	1
valbltcon0	rs.w	1
valbltcon1	rs.w	1
ptMovX		rs.w	1
ptMovY		rs.w	1
* -------
posbkgY		rs.w	1
posbkgX		rs.w	1
scrollbkgY	rs.b	1
scrollbkgX	rs.b	1
dirmovY		rs.b	1
dirmovX		rs.b	2
* -------
ptText		rs.l	1
VARSIZE	rs.w	0

* --------------------------------------
Start:	movem.l	d0-d7/a0-a6,-(sp)
	
	lea	Vars(pc),a5	
	movea.l	a5,a6
	move.w	#Varsize-1,d7
.clear	clr.b	(a6)+
	dbf	d7,.clear

	bsr	ReadText

	movea.l	4.w,a6
	lea	GfxName(pc),a1
	moveq	#0,d0
	jsr	OpenLib(a6)
	tst.l	d0
	beq	NoGfx
	
	move.l	d0,GfxBase(a5)
	move.l	d0,a6
	bsr	FadeOutSystem
	move.l	ActiView(a6),OldView(a5)
	
	suba.l	a1,a1
	jsr	LoadView(a6)
	
	jsr	WaitTOF(a6)
	jsr	WaitTOF(a6)
	jsr	WaitBlit(a6)
	
	movea.l	4.w,a6
	jsr	Forbid(a6)
	
	bsr	GetVBR
; my init
	bsr	InitAll

	bsr	InitMusic

	bsr	InitSys
	
* Main Program ------------------------------------------------------
	lea	Vars(pc),a5
	lea	$dff000,a6

main	;bsr	waitvbl
	bsr	waitraster

	bsr	RestoreBkgBob
	bsr	MoveBkg
	bsr	MoveBob
	bsr	SaveBkgBob
	bsr	AnimBob
	bsr	DrawBob
	bsr	ScrollText

	btst	#6,$bfe001
	bne.s	main

* restore -----------------------------------------------------------	
End	bsr	StopMusic

	bsr	Restore
	
	movea.l	4.w,a6
	jsr	Permit(a6)

	lea	Vars(pc),a5
	movea.l	GfxBase(a5),a6
	movea.l	OldView(a5),a1
	jsr	LoadView(a6)
	move.l	$26(a6),$dff080
	move.l	$32(a0),$dff084
	move.w	$dff088,d0

	bsr	FadeInSystem

	movea.l	a6,a1
	movea.l	4.w,a6
	jsr	CloseLib(a6)
	
NoGfx	movem.l	(sp)+,d0-d7/a0-a6
	moveq	#0,d0
	rts
* ---------------------------------------------	
InitSys:
	lea	$dff000,a6
	lea	Vars(pc),a5
	move.w	2(a6),d0		save dma
	ori.w	#$8200,d0
	move.w	d0,sDma(a5)
	move.w	$1c(a6),d0		intena
	ori.w	#$c000,d0
	move.w	d0,intena(a5)
	
	move.w	#$7fff,d0
	move.w	d0,$96(a6)
	move.w	d0,$9a(a6)
	move.w	d0,$9c(a6)
	
	
	movea.l	sVBR(a5),a1
	move.l	$6c(a1),intvbl(a5)
	lea	interrupt(pc),a0
	move.l	a0,$6c(a1)

	lea	NewCopper,a0
	move.l	a0,$80(a6)
	move.w	#0,$88(a6)
	move.w	#D_con,$96(a6)

	move.w	#$c020,$9a(a6)
	
	rts
* ---------------------------------------------
Restore:	
	lea	Vars(pc),a5
	
	lea	$dff000,a6
	move.w	#$7fff,d0
	move.w	d0,$96(a6)
	move.w	d0,$9a(a6)
	move.w	d0,$9c(a6)
		
	movea.l	sVBR(a5),a1
	move.l	intvbl(a5),$6c(a1)
	move.w	intena(a5),$9a(a6)
	move.w	sDma(a5),$96(a6)
	
	rts
* --------------------------------------------
GetVBR:	movea.l	4.w,a6
	btst	#0,$129(a6)
	beq.s	.normal
	
	lea	SuperCode(pc),a5
	jsr	Supervisor(a6)
	
	lea	Vars(pc),a5
	move.l	d0,sVBR(a5)
.normal	rts
	CNOP	0,4
SuperCode:	
	dc.l	$4e7a0801	
	;movec Vbr,d0		; 680x0
	rte
* --------------------------------------------
GfxName	dc.b	'graphics.library',0
	even
* --------------------------------------------
interrupt:	
	movem.l	a0-a6/d0-d7,-(sp)

	lea	$dff000,a6	
	andi.w	#$20,$1e(a6)	interruption vbl
	beq.s	pasInt

	lea	Vars(pc),a5
	st	flagVbl(a5)
	
	bsr	PlayMusic

	move.w	#$0020,$dff09c	; ok ! interrupt finished	
	move.w	#$0020,$dff09c
pasInt	movem.l	(sp)+,a0-a6/d0-d7	
	rte
* --------------------------------------------
Inst_plans:		
	move.w	d0,6(a0)
	swap	d0			
	move.w 	d0,2(a0)
	swap	d0
	tst.w	d1
	beq.s	.fin
	add.l	d2,d0
	addq.l	#8,a0
	subq.w	#1,d1
	bra.s	inst_plans
.fin	rts
* --------------------------------------------
waitblitter	
	btst	#6,$dff002
.wait	btst	#6,$dff002
	bne.s	.wait
	rts
* --------------------------------------------
waitraster
	movem.l	d0,-(sp)
.loop	move.l	$dff004,d0
	and.l	#$1ff00,d0
	cmp.l	#302<<8,d0
	bne.b	.loop
	movem.l	(sp)+,d0
	rts

waitvbl	moveq	#10,d7
.loop	sf	flagVbl(a5)
.wait:	tst.b	flagVbl(a5)
	beq.s	.wait
	dbf	d7,.loop
	rts
* --------------------------------------------------------
* - Fader System
* - a5=Vars
* - a6=GfxBase
* --------------------------------------------------------
FadeOutSystem:
	movea.l	ActiView(a6),a3
	movea.l	(a3),a3		; viewport
	movea.l	4(a3),a2
	move.l	a2,sav4vp(a5)
	move.w	2(a2),d7	; number of colors
	cmp.w	#31,d7
	blt.s	.inf
	moveq	#32,d7
.inf:
	subq.w	#1,d7
	move.w	d7,numcol(a5)	; number of colors
	lea	syspal(a5),a0
	movea.l	4(a2),a1	; palette
.copy	move.w	(a1)+,(a0)+	; save system palette
	dbf	d7,.copy

	moveq	#16-1,d7
.loopfade:
	move.w	numcol(a5),d6
	movea.l	4(a2),a1
.loopcol:
	move.w	(a1),d0
	move.w	d0,d1
	andi.w	#$F00,d1
	tst.w	d1
	beq.s	.green
	subi.w	#$100,d0
.green:
	move.b	d0,d1
	andi.b	#$F0,d1
	tst.b	d1
	beq.s	.blue
	subi.b	#$10,d0
.blue:
	move.b	d0,d1
	andi.b	#$F,d1
	tst.b	d1
	beq.s	.next
	subq.b	#1,d0
.next:
	move.w	d0,(a1)+
	dbf	d6,.loopcol

	jsr	WaitTOF(a6)
	movea.l ActiView(a6),a0
	movea.l	a3,a1
	jsr	MakeVPort(a6)	; a0=view / a1=viewport
	movea.l	ActiView(a6),a1
	jsr	MrgCop(a6)	; a1=view
	movea.l	ActiView(a6),a1
	jsr	LoadView(a6)

	dbf	d7,.loopfade
	rts
* ------------------- Fade In
FadeInSystem:
	movea.l	ActiView(a6),a3
	movea.l	(a3),a3		; viewport
	movea.l	sav4vp(a5),a2

	moveq	#16-1,d7
.loopfade:
	move.w	numcol(a5),d6
	lea	syspal(a5),a0	; original palette
	movea.l	4(a2),a1
.loopcol:
	move.w	(a0)+,d0
	move.w	(a1),d2
	sub.w	d2,d0
	move.w	d0,d1
	andi.w	#$F00,d1
	tst.w	d1
	beq.s	.green
	addi.w	#$100,d2
.green:
	move.b	d0,d1
	andi.b	#$F0,d1
	tst.b	d1
	beq.s	.blue
	addi.b	#$10,d2
.blue:
	move.b	d0,d1
	andi.b	#$F,d1
	tst.b	d1
	beq.s	.next
	addq.b	#1,d2
.next:
	move.w	d2,(a1)+
	dbf	d6,.loopcol

	jsr	WaitTOF(a6)
	movea.l	ActiView(a6),a0
	movea.l	a3,a1
	jsr	MakeVPort(a6)
	movea.l	ActiView(a6),a1
	jsr	MrgCop(a6)
	movea.l	ActiView(a6),a1
	jsr	LoadView(a6)
	dbf	d7,.loopfade
	rts
* --------------------------------------------------------
InitMusic:
	lea	$dff000,a6

	ifd	HIP
	 lea	modadr,a0
	 jsr	h_initsmpl
	 moveq	#1,d0
	 jsr	h_control
	endc
	ifd	PT
	 lea	Module,a0
	 bsr	mt_init
	endc
	ifd	FRED
	 lea	Module,a0
	 moveq	#0,d0
	 jsr	(a0)
	 bset	#1,$bfe001
	endc
	ifd	SID
	 jsr	Module		; initmuzak
	 bset	#1,$bfe001
	endc
	ifd	SID2
	 bsr	INITMUZAK
	endc
	ifd	SA
	 jsr	Module		; Init Pointers
	 moveq	#0,d0		; start song 0
	 jsr	Module+12
	endc
	ifd	FC
	 lea	Module,a0
	 jsr	init
	endc
	ifd	FC14
	 bsr	fc_init
	endc
	ifd	MA
	 moveq	#0,d0
	 jsr	Module
	endc
	ifd	DM
	 moveq	#1,d0			; INIT MUSIC
	 jsr	Module			; CALL THE PLAYER
	endc
	ifd	DMU
	 moveq	#0,d0
	 jsr	muzaxon
	endc
	rts
* ---
StopMusic:
	lea	$dff000,a6

	ifd	HIP
	 jsr	h_stop
	 move.w	#0,$DFF0A8
	 move.w	#0,$DFF0B8
	 move.w	#0,$DFF0C8
	 move.w	#0,$DFF0D8
	 bclr	#1,$BFE001
	endc
	ifd	PT
	 bsr	mt_end
	endc
	ifd	FRED
	 moveq	#0,d1
	 lea	Module,a0
	 jsr	8(a0)
	 bclr	#1,$bfe001
	 move.w	#$f,$dff096
	 move.w	#0,$DFF0A8
	 move.w	#0,$DFF0B8
	 move.w	#0,$DFF0C8
	 move.w	#0,$DFF0D8
	endc
	ifd	FC
	 jsr	stop
	endc
	ifd	FC14
	 bsr	fc_stop
	endc
	ifd	SID
	 jsr	Module+$120	; stopmuzak
	 move.w	#$f,$dff096
	 move.w	#0,$DFF0A8
	 move.w	#0,$DFF0B8
	 move.w	#0,$DFF0C8
	 move.w	#0,$DFF0D8
	endc
	ifd	SID2
	 bsr	STOPMUZAK	
	endc
	ifd	SA
	 jsr	Module+16
	endc
	ifd	MA
	 move.w	#$f,$dff096
	 clr.w	$DFF0A8
	 clr.w	$DFF0B8
	 clr.w	$DFF0C8
	 clr.w	$DFF0D8
	endc
	ifd	DM
	 move.w	#$f,$dff096
	 move.w	#0,$DFF0A8
	 move.w	#0,$DFF0B8
	 move.w	#0,$DFF0C8
	 move.w	#0,$DFF0D8
	endc
	ifd	DMU
	 jsr	muzaxoff
	endc
	rts
* ---
PlayMusic:
	lea	$dff000,a6
	ifd	HIP
	 jsr	h_replay
	endc
	ifd	PT
	 bsr	mt_music
	endc
	ifd	FRED
	 lea	Module,a0
	 jsr	4(a0)
	endc
	ifd	FC
	 jsr	play
	endc
	ifd	FC14
	 bsr	fc_play
	endc
	ifd	SID
	 jsr	Module+$13e		; playmuzak
	endc
	ifd	SID2
	 bsr	PLAYMUZAK	
	endc
	ifd	SA
	 jsr	Module+24
	endc
	ifd	MA
	 jsr	Module+$C
	endc
	ifd	DM
	 moveq	#0,d0			; SET REPLAY MODE
	 jsr	Module			; CALL THE PLAYER
	endc
	ifd	DMU
	 jsr	player
	endc
	rts
* --------------------------------------------------------
* - INITIALIZATION
* --------------------------------------------------------
InitAll:
	lea	Vars(pc),a5
	lea	$dff000,a6

	move.w	#$c00,$106(a6)	; AGA fix
	move.w	#0,$1fc(a6)
	move.w	#$11,$10c(a6)
* - inst bitplans
	move.l	#logo,d0
	lea	CLlogo,a0
	moveq	#4-1,d1
	move.l	#40*100,d2
	bsr	Inst_Plans

	move.l	#BpScroll,d0
	lea	CLscroll,a0
	moveq	#0,d1
	moveq	#0,d2
	bsr	Inst_Plans
* - init var
	lea	BobFrames(pc),a0
	move.l	a0,ptFrames(a5)
	move.w	#4,delayanim(a5)

	lea	Bobs,a0
	move.l	a0,ptBob(a5)

	move.l	#$fca0000,valbltcon0(a5)

	move.w	#4*15,ptMovX(a5)
	move.w	#2*34,ptMovY(a5)
	move.b	#10,scrollbkgY(a5)
	move.b	#16,scrollbkgX(a5)
	move.w	#7,posbkgY(a5)
	move.w	#7,posbkgX(a5)

	lea	ScrollStruct(pc),a0
	move.l	#16,d0
	moveq	#23-1,d7	; 23 chars
.loop	move.w	d0,(a0)
	move.w	#0,2(a0)
	move.l	#font,4(a0)
	move.l	#BpScroll,8(a0)
	addi.l	#16,d0
	adda.l	#12,a0
	dbf	d7,.loop

	lea	TabChar(pc),a1
	moveq	#0,d1
	moveq	#4-1,d2
.loopl	lea	font,a0
	adda.l	d1,a0
	move.l	#16-1,d0
.loopch	move.l	a0,(a1)+
	adda.l	#2,a0
	dbf	d0,.loopch
	addi.l	#32*36,d1	; next line of chars
	dbf	d2,.loopl
	rts
* --------------------------------------------------------
* - read external file
* --------------------------------------------------------
ReadText:
	movea.l	4.w,a6
	lea	DosName(pc),a1
	move.l	#31,d0
	jsr	OpenLib(a6)
	movea.l	d0,a6
	beq.s	.nodos
	lea	filename(pc),a0
	move.l	a0,d1		; file
	move.l	#$3ed,d2	; access mode
	jsr	Open(a6)
	move.l	d0,d5
	beq.s	.openerror
	move.l	d0,d1		; file handle
	lea	Text(pc),a0
	move.l	a0,d2		; buffer
	move.l	#1000,d3	; len
	jsr	Read(a6)
	move.l	d0,d6
	beq.s	.readerror
	lea	Text(pc),a0
	move.b	#-1,(a0,d6.w)

.readerror
	move.l	d5,d1
	jsr	Close(a6)
.openerror
	movea.l	a6,a1
	movea.l	4.w,a6
	jsr	CloseLib(a6)
.nodos	rts
* -
filename
	dc.b ':msg',0
	even
DosName	dc.b	'dos.library',0
	even
* -----------------
* - Blit functions
* -----------------
* d0=bltsize
* d1=modulo(s)
* a0=source A/B or dest (clear)
* a1=dest/source C
* a2=source A
BlitMask:
	bsr	waitblitter

	move.l	valbltcon0(a5),$40(a6)	; copy with mask
	move.l	#$FFFF0000,$44(a6)
	move.l	d1,$60(a6)	; modulos C/B
	swap	d1
	move.l	d1,$64(a6)	; modulos A/D
	move.l	a0,$4C(a6)	; source B
	move.l	a2,$50(a6)	; source A
	move.l	a1,$48(a6)	; source C
	move.l	a1,$54(a6)	; dest D
	move.w	d0,$58(a6)
	rts
* -
BlitCopy:
	bsr	waitblitter
	move.l	#$9f00000,$40(a6)
	move.l	#-1,$44(a6)
	move.l	d1,$64(a6)	; modulos A/D
	move.l	a0,$50(a6)	; source A
	move.l	a1,$54(a6)	; dest D
	move.w	d0,$58(a6)
	rts
* -
BlitClear:
	bsr	waitblitter
	move.l	#$1000000,$40(a6)
	move.l	a0,$54(a6)
	move.w	d1,$66(a6)	; modulo D
	move.w	d0,$58(a6)
	rts
* --------------------------------------------------------
* - Anim Blitter OBject
* --------------------------------------------------------
MoveBob:
	subi.w	#4,ptMovX(a5)
	bne.s	.skipx
	move.w	#4*157,ptMovX(a5)
.skipx	move.w	ptMovX(a5),d0
	lea	TabMovX(pc),a0
	move.w	(a0,d0.w),d0

	subi.w	#2,ptMovY(a5)
	bne.s	.skipy
	move.w	#2*63,ptMovY(a5)
.skipy	move.w	ptMovY(a5),d1
	lea	TabMovY(pc),a0
	move.b	(a0,d1.w),d1

	ext.w	d1
	bsr.s	getNewPosBob
	move.w	d1,shiftbltcon0(a5)
	move.l	a1,ptPos(a5)
	rts
* -
getNewPosBob:
	lea	logo,a1
	mulu.w	#40,d1
	adda.l	d1,a1
	move.w	d0,d1
	lsr.w	#3,d0
	andi.w	#-2,d0
	adda.w	d0,a1
	andi.l	#15,d1
	lsl.b	#4,d1
	rts
* ---
AnimBob:
	subi.w	#1,delayanim(a5)
	bne.s	.exit
	move.w	#3,delayanim(a5)
	addi.l	#4,ptFrames(a5)
.loop	movea.l	ptFrames(a5),a0
	movea.l	(a0),a0
	cmpa.l	#-1,a0
	bne.s	.notyet
	lea	BobFrames(pc),a0
	move.l	a0,ptFrames(a5)
	bra.s	.loop
.notyet	move.l	a0,ptBob(a5)
.exit	rts
* ---
DrawBob:
	move.w	shiftbltcon0(a5),d1
	andi.l	#$FFF0000,valbltcon0(a5)
	or.b	d1,valbltcon0(a5)
	or.b	d1,valbltcon1(a5)
	movea.l	ptPos(a5),a1	; dest
	movea.l	ptBob(a5),a0	; source
	movea.l	a0,a2
	adda.l	#4*48*4,a2
	move.l	#4-1,d2
.blitplan
	move.l	#$22FFFE,d1	; modulo A/D et C/B: 4-6/40-6 et 40-6/4-6
	bsr	BlitMask
	adda.l	#4*48,a0	; next plan
	adda.l	#40*100,a1
	dbf	d2,.blitplan
	rts
* ---
SaveBkgBob:
	move.l	#4-1,d2
	movea.l	ptPos(a5),a0	; source screen
	movea.l	#SaveBuf,a1	; temp buffer
	move.l	#$220000,d1	; modulo A/D : 40-6/0
	move.w	#48*64+6/2,d0	; bltsize
.blitplan
	bsr	BlitCopy
	adda.l	#6*48,a1	; next plan
	adda.l	#40*100,a0
	dbf	d2,.blitplan
	rts
* -
RestoreBkgBob:
	move.l	#4-1,d2
	movea.l	#SaveBuf,a0	; source save buffer
	movea.l	ptPos(a5),a1	; dest screen
	cmpa.l	#0,a1
	beq.s	.exit
	move.l	#40-6,d1	; modulo A/D: 0/40-6
	move.w	#48*64+6/2,d0
.blitplan
	bsr	BlitCopy
	adda.l	#288,a0		; 48*6
	adda.l	#4000,a1	; 40*100
	dbf	d2,.blitplan
.exit	rts
* --------------------------------------------------------
* - Anim Background Logo
* --------------------------------------------------------
MoveBkg:
	tst.b	dirmovX(a5)
	bne.s	.right
	subi.w	#1,posbkgX(a5)		; to the left
	bne.s	.contx

	subi.b	#1,scrollbkgX(a5)
	bne.s	.maxx
	move.b	#1,dirmovX(a5)
	move.b	#19,scrollbkgX(a5)
	bra.s	.contx

.maxx	move.w	#6,posbkgX(a5)
	bra.s	.contx
.right	addi.w	#1,posbkgX(a5)		; to the right
	cmpi.w	#6,posbkgX(a5)
	bne.s	.contx

	subi.b	#1,scrollbkgX(a5)
	bne.s	.minx
	move.b	#0,dirmovX(a5)
	move.b	#24,scrollbkgX(a5)
	bra.s	.contx

.minx	move.w	#0,posbkgX(a5)
.contx	move.w	posbkgX(a5),d0
;
	tst.b	dirmovY(a5)
	bne.s	.down
	subi.w	#1,posbkgY(a5)		; up
	bne.s	.conty

	subi.b	#1,scrollbkgY(a5)
	bne.s	.maxy
	move.b	#1,dirmovY(a5)
	move.b	#29,scrollbkgY(a5)
	bra.s	.conty

.maxy	move.w	#6,posbkgY(a5)
	bra.s	.conty
.down	addi.w	#1,posbkgY(a5)		; down
	cmpi.w	#6,posbkgY(a5)
	bne.s	.conty

	subi.b	#1,scrollbkgY(a5)
	bne.s	.miny
	move.b	#0,dirmovY(a5)
	move.b	#22,scrollbkgY(a5)
	bra.s	.conty

.miny	move.w	#0,posbkgY(a5)
.conty	move.w	posbkgY(a5),d1

	bsr	BlitGrid
	rts
* --
BlitGrid:
	lea	grid+2,a1
	bsr	getPosGrid

	movea.l	a1,a0	; source
	andi.l	#$FFF0000,valbltcon0(a5)
	or.b	d1,valbltcon1(a5)
	movea.l	#logo+40*100*2+40*4+10,a1	; logo bp3
	lea	logomsk,a2	; logo mask
	move.w	#94*64+24/2,d0	; bltsize
	move.l	#$100000,d1	; modulos 40-24/0
	bsr	BlitMask

	adda.l	#24*108,a0	; next plan
	adda.l	#40*100,a1
	move.l	#$100000,d1
	bsr	BlitMask
	rts
* -
getPosGrid:
	mulu.w	#24,d1
	adda.l	d1,a1
	move.w	d0,d1
	lsr.w	#3,d0
	andi.w	#-2,d0
	adda.w	d0,a1
	andi.l	#15,d1
	lsl.b	#4,d1
	rts
* --------------------------------------------------------
* - ScrollText dycp
* --------------------------------------------------------
ScrollText:
	move.w	#23-1,d4
	lea	ScrollStruct(pc),a3
	lea	ptPosScrollY(pc),a4
.loopclr
	movea.l	8(a3),a0	; dest
	move.w	#44,d1		; modulo D
	move.w	#36*64+4/2,d0
	bsr	BlitClear
	adda.l	#12,a3
	dbf	d4,.loopclr
;
	move.w	#23-1,d4
	lea	ScrollStruct(pc),a3
.loopchar
	subi.w	#2,(a3)		; scroll (decr xpos)
	bne.s	.nonewchar
	move.w	#48*8-16,(a3)
.restart
	lea	Text(pc),a1
.search	move.w	ptText(a5),d0
	addi.w	#1,ptText(a5)
	move.b	(a1,d0.w),d1
	cmp.b	#-1,d1
	bne.s	.noend
	move.w	#0,ptText(a5)
	bra.s	.search

.noend	ext.w	d1
	subi.w	#' ',d1
	bmi.s	.restart
	cmp.w	#'@',d1
	bcc.s	.restart
	lsl.w	#2,d1
	lea	TabChar(pc),a1
	move.l	(a1,d1.w),4(a3)	; source

.nonewchar
	move.w	(a3),d0		; xpos
	subi.w	#2,(a4)
	bne.s	.skip
	move.w	#126,(a4)
.skip	move.w	(a4),d1
	lea	TabMovY(pc),a1
	move.b	(a1,d1.w),d1	; ypos
	ext.w	d1
	lea	BpScroll-2,a1
	mulu.w	#48,d1
	adda.l	d1,a1
	move.w	d0,d1
	lsr.w	#3,d0
	andi.w	#-2,d0
	adda.w	d0,a1
	andi.l	#15,d1
	lsl.b	#4,d1
	andi.l	#$FFF0000,valbltcon0(a5)
	or.b	d1,valbltcon0(a5)
	or.b	d1,valbltcon1(a5)
	move.l	a1,8(a3)	; dest
	movea.l	4(a3),a0	; source
	movea.l	4(a3),a2
	move.w	#36*64+4/2,d0
	move.l	#$2C001C,d1	; modulos A/D :	44/28 =	48-4 / 32-4
	bsr	BlitMask
	adda.l	#12,a3
	adda.l	#2,a4
	dbf	d4,.loopchar
	rts
* -------------------------------------------------------- no chip player
	ifd	PT
	 include	"ptreplay.s"
	endc
	ifd	FC14
	 include	"playerFC.s"
	endc
	ifd	SID2
	 include	"sidmon2.s"
	endc
* ---------------------------------------------------------
Vars	ds.b	VARSIZE
	even
* --- bob
B=4*48*5
BobFrames:
	dc.l	Bobs
	dc.l	Bobs+B
	dc.l	Bobs+B*2
	dc.l	Bobs+B*3
	dc.l	Bobs+B*2
	dc.l	Bobs+B
	dc.l	-1
TabMovY:
 dc.b $16,$17,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,$20,$21,$22,$23,$24
 dc.b $24,$25,$26,$27,$27,$28,$29,$29,$2a,$2a,$2a,$2b,$2b,$2b,$2b
 dc.b $2b,$2b,$2b,$2b,$2b,$2b,$2b,$2b,$2a,$2a,$2a,$29,$28,$28,$27
 dc.b $27,$26,$25,$24,$24,$23,$22,$21,$20,$1f,$1e,$1d,$1c,$1b,$1a
 dc.b $19,$18,$16,$15,$14,$13,$12,$11,$10,$0f,$0e,$0d,$0c,$0b,$0a
 dc.b $09,$08,$07,$06,$06,$05,$04,$03,$03,$02,$02,$01,$01,$01,$00
 dc.b $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$02,$02
 dc.b $03,$03,$04,$04,$05,$06,$07,$08,$08,$09,$0a,$0b,$0c,$0d,$0e
 dc.b $0f,$10,$11,$12,$13,$14,$15,$00
TabMovX:
 dc.w $008c,$008e,$0092,$0094,$0097,$009a,$009c,$00a0,$00a2,$00a5
 dc.w $00a8,$00aa,$00ad,$00b0,$00b2,$00b5,$00b8,$00ba,$00bd,$00c0
 dc.w $00c2,$00c5,$00c8,$00ca,$00cc,$00cf,$00d2,$00d4,$00d6,$00d8
 dc.w $00db,$00dd,$00e0,$00e2,$00e4,$00e6,$00e8,$00ea,$00ec,$00ee
 dc.w $00f0,$00f2,$00f4,$00f6,$00f8,$00fa,$00fb,$00fd,$00fe,$0100
 dc.w $0102,$0103,$0104,$0106,$0107,$0108,$010a,$010b,$010c,$010d
 dc.w $010e,$010f,$0110,$0111,$0112,$0112,$0114,$0114,$0114,$0115
 dc.w $0116,$0116,$0116,$0117,$0117,$0118,$0118,$0118,$0118,$0118
 dc.w $0118,$0118,$0118,$0117,$0117,$0116,$0116,$0116,$0116,$0114
 dc.w $0114,$0114,$0112,$0112,$0111,$0110,$0110,$010e,$010e,$010c
 dc.w $010b,$010a,$0108,$0108,$0106,$0104,$0103,$0102,$0100,$00fe
 dc.w $00fd,$00fc,$00fa,$00f8,$00f6,$00f4,$00f2,$00f0,$00ee,$00ec
 dc.w $00ea,$00e8,$00e6,$00e4,$00e2,$00e0,$00de,$00db,$00d8,$00d6
 dc.w $00d4,$00d2,$00cf,$00cc,$00ca,$00c8,$00c5,$00c2,$00c0,$00be
 dc.w $00ba,$00b8,$00b6,$00b2,$00b0,$00ad,$00aa,$00a8,$00a5,$00a2
 dc.w $00a0,$009c,$009a,$0097,$0094,$0092,$008f,$008c,$0089,$0086
 dc.w $0084,$0081,$007e,$007b,$0078,$0076,$0073,$0070,$006e,$006a
 dc.w $0068,$0066,$0062,$0060,$005e,$005a,$0058,$0056,$0053,$0050
 dc.w $004e,$004c,$0049,$0046,$0044,$0042,$003f,$003d,$003a,$0038
 dc.w $0036,$0034,$0032,$0030,$002e,$002c,$002a,$0028,$0026,$0024
 dc.w $0022,$0020,$001e,$001c,$001b,$0019,$0018,$0016,$0014,$0013
 dc.w $0012,$0010,$000f,$000e,$000c,$000c,$000a,$000a,$0008,$0008
 dc.w $0006,$0006,$0005,$0004,$0004,$0003,$0002,$0002,$0002,$0001
 dc.w $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
 dc.w $0000,$0000,$0001,$0002,$0002,$0002,$0003,$0004,$0004,$0004
 dc.w $0006,$0006,$0007,$0008,$0009,$000a,$000b,$000c,$000e,$000f
 dc.w $0010,$0012,$0013,$0014,$0016,$0017,$0019,$001a,$001c,$001e
 dc.w $0020,$0022,$0023,$0025,$0027,$0029,$002b,$002d,$002f,$0031
 dc.w $0034,$0036,$0038,$003a,$003c,$003e,$0041,$0044,$0046,$0048
 dc.w $004a,$004d,$0050,$0052,$0055,$0058,$005a,$005c,$0060,$0062
 dc.w $0064,$0068,$006a,$006d,$0070,$0072,$0075,$0078,$007a,$007e
 dc.w $0080,$0083,$0086,$0088,$008c,$0000

* --- dycp
ptPosScrollY:	
	dc.w	$10,$14,$18,$1C,$20,$24,$28,$2C,$30,$34,$38,$3C
	dc.w	$38,$34,$30,$2C,$28,$24,$20,$1C,$18,$14,$10,$C,8
ScrollStruct:
	ds.b	12*23
TabChar	ds.l	4*16
Text:	dc.b	'....CODED BY PIRANHA OF THE PREDATORS FOR ACU 25/4/89..........'
	dc.b	-1
	ds.b	1000
	even
* ---------------------------------------------------------- COPPERLISTS
	section	copper,data_c	
Newcopper:
	dc.w	$106,$c00,$1fc,0,$10c,$11
	dc.w	$200f,$fffe
	dc.w	$8e,$0581,$90,$40c1
	dc.w	$92,$38,$94,$d0
	dc.w	$102,0,$104,$24
	dc.w	$108,0,$10a,0
	dc.w	$100,0,$180,0

CLspr	dc.w	$120,0,$122,0
	dc.w	$124,0,$126,0
	dc.w	$128,0,$12a,0
	dc.w	$12c,0,$12e,0
	dc.w	$130,0,$132,0
	dc.w	$134,0,$136,0
	dc.w	$138,0,$13a,0
	dc.w	$13c,0,$13e,0

CLlogo	dc.w	$e0,0,$e2,0
	dc.w	$e4,0,$e6,0
	dc.w	$e8,0,$ea,0
	dc.w	$ec,0,$ee,0

	dc.w	$180,0
	dc.w	$182,$fff,$184,$6ea,$186,$860,$188,$a00,$18a,$c80,$18c,$44e,$18e,$68e
	dc.w	$190,$c30,$192,$800,$194,$ec7,$196,$7b3,$198,$f40,$19a,$e00,$19c,8,$19e,$383

	dc.w	$5e0f,$fffe,$100,$4200

	dc.w	$c20f,$fffe,$100,0

	dc.w	$d00f,$fffe,$180,0,$182,$fff
	dc.w	$8e,$0571,$90,$40d1		; overscan
	dc.w	$92,$30,$94,$d8
	dc.w	$108,48-44
CLscroll dc.w	$e0,0,$e2,0
	
	dc.w	$d10f,$fffe,$100,$1200

	dc.w	$d101,$fffe,$182,$d70
	dc.w	$d901,$fffe,$182,0
	dc.w	$da01,$fffe,$182,$eb0
	dc.w	$e201,$fffe,$182,0
	dc.w	$e301,$fffe,$182,$ee0
	dc.w	$eb01,$fffe,$182,0
	dc.w	$ec01,$fffe,$182,$bf0
	dc.w	$f401,$fffe,$182,0
	dc.w	$f501,$fffe,$182,$0f8
	dc.w	$fd01,$fffe,$182,0
	dc.w	$fe01,$fffe,$182,$0fb
	dc.w	$ffdf,$fffe	; pal
	dc.w	$0701,$fffe,$182,0 
	dc.w	$0801,$fffe,$182,$0fd
	dc.w	$1001,$fffe,$182,$000
	dc.w	$1101,$fffe,$182,$0de
	dc.w	$1901,$fffe,$182,$000
	dc.w	$1a01,$fffe,$182,$09f
	dc.w	$2101,$fffe,$182,0,$100,0

	dc.l	-2
* -----------------------------------------------------
	ifd	HIP
	include	"Hippel.s"
	endc
	ifd	FC
	include	"FutureComposer.s"
	endc
	ifd	DMU
	include	"player(pc).s"
	endc
Module:
mt_data:
modadr:
mod:
muzak:
	ifd	PT
	incbin	"here_we_come.mod"
	endc
	ifd	HIP
	incbin	"HIP.GATES_OF_JAMBALA-TITLE"
	endc
	ifd	FRED
	incbin	"FRED.NoName"
	endc
	ifd	FC
	incbin	"FC13.THALION"
	endc
	ifd	FC14
	incbin	"FC14.BLAIZER"
	endc
	ifd	SID
	incbin	"SID1.SCOOPEX-2"
	endc
	ifd	SID2
	incbin	"SID2.POSSESSED"
	endc
	ifd	SA
	incbin	"sonic.behind_the_wall"
	endc
	ifd	MA
	incbin	"MA.OneMan&Droid"
	endc
	ifd	DM
	incbin	"DM2.PARADOX-3"
	endc
	ifd	DMU
	incbin	"dmu.ferry_tell"
	endc

logo	incbin	"logo.rw"	; 40*100*4 (16c)
font	incbin	"font.rw"	; 32*144
grid	incbin	"grid.rw"	; 24*108*2
logomsk	incbin	"logomsk.rw"	; 24*94
Bobs	incbin	"bobanim.rw"	; 4*48*4+(4*48*5)*3

	section	bss,bss_c
	ds.b	2
BpScroll ds.b	48*80
SaveBuf	ds.b	6*48*4
	END
