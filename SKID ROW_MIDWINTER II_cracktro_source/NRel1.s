section aa,code_C
prog=$50000
bitplane=$74000-$800
bitplane2=$6e000
spdat=$6fc00

jmp  prog
org  prog
load prog
	
	move.l	a7,SPsave
	movem.l	d0-d7/a0-a6,-(a7)
	move.w	#$4000,$dff09a
	move.l	$80.w,Tsave
	lea	run(pc),a0
	move.l	a0,$0080.w
	trap	#0
	move.l	Tsave(pc),$80.w
	move.w	#$c000,$dff09a
	movem.l	(a7)+,d0-d7/a0-a6
	move.l	SPsave(pc),a7
	rts

run:	move.w	SR,SRsave
	move.w	#$2700,sr
	lea	$dff000,a3
	move.w	$0002(a3),d0
	move.w	$001c(a3),d1
	or.w	#$8000,d0
	or.w	#$8000,d1
	move.w	d0,DMAsave
	move.w	d1,INTsave
	move.w	#$7fff,$009a(a3)
	move.w	#$7fff,$0096(a3)
	move.w	#$0000,$0180(a3)
	bsr.L	copy
	bsr.L	starson
	move.l	#sct,cou
	move.w	#$87e0,$0096(a3)
	bsr.L	textout
	move.l	#newclist,$0080(a3)
wait:	cmp.b	#100,$0006(a3)
	bne.s	wait
	bsr.L	blitten
	bsr.L	blitttit
	eor.l	#$8000,addr
	eor.l	#$8000,addr2
	eor.w	#$8000,bp1+6
	eor.w	#$8000,bp1+6+8
	bsr.L	clear
	bsr.L	stars
wb:	btst	#$e,$dff002
	bne.s	wb
	bsr.L	blitto
	btst	#6,$bfe001
	bne.s	wait
	move.l	$4.w,a6
	move.l	156(a6),a6
	move.l	38(a6),$0080(a3)
	move.w	INTsave,$009a(a3)
	move.w	DMAsave,$0096(a3)
	move.w	SRsave,SR
	rte

copy:	lea	$70000,a0
	move.l	#$3fff,d0
loop:	clr.l	(a0)+
	dbra	d0,loop
	lea	bitplane2,a0
	move.w	#$7ff,d0
clear2:	clr.l	(a0)+
	dbra	d0,clear2
;-------------------------Init Bildschirm pointers------------------
	lea	logo(pc),a0
	lea	bpl(pc),a1
	move.l	a0,d0
	moveq	#4-1,d7
setbp:	move.l	d0,d1
	move.w	d0,6(a1)
	swap	d1
	move.w	d1,2(a1)
	addq.l	#8,a1
	add.l	#44,d0
	dbra	d7,setbp
;-------------------------------------------------------------------
	lea	tabd(pc),a0
	lea	cosend(pc),a1
	move.l	#cosend-tabd,d0
double:	move.w	(a0)+,(a1)+
	dbra	d0,double
	lea	taby(pc),a0
	lea	cols,a1
	move.w	#$090f,$ca(a1)
	move.w	#105,d2
	move.w	#$7e31,d0
loop2:	move.w	d0,(a0)+
	move.w	#$fffe,(a0)+
seto:	move.w	#$0192,(a0)+
	move.w	(a1)+,(a0)+
	add.w	#$100,d0
	dbra	d2,loop2
	rts

blitto:	clr.l   $0064(a3)
	clr.w	$0042(a3)
	move.l	#$ffffffff,$0044(a3)
	move.w	#%1111100111110000,$0040(a3)
	move.l	addr2,a0
	move.l	a0,$0050(a3)
	add.l	#$4000-2,a0
	move.l	a0,$0054(a3)
	move.w	#138*64+20,$0058(a3)
	rts


blitten:addq.w	#1,lab
	clr.l   $0064(a3)
	clr.w	$0042(a3)
	move.l	#$ffffffff,$0044(a3)
	move.w	#%1110100111110000,$0040(a3)
	move.l	#bitplane-2,$0050(a3)
	move.l	#bitplane-4,$0054(a3)
	move.w	#15*64+24,$0058(a3)
;-------------------------------------------------------------
	moveq	#0,d0
	moveq	#0,d2
	cmp.w	#8,lab
	bne.L	back2	
	clr.w	lab
	move.l	cou,a1
	cmp.b	#0,(a1)	
	bne.s	mm
	move.l  #sct,cou
	move.l	cou,a1
mm:	move.b	(a1),d0
	addq.l	#1,cou
	cmp.b	#$31,d0
	bne.s	notadd
	addq.w	#4,lab	
notadd:	subi.b	#$41,d0
	bmi.s	sonderz
	rol.w	#1,d0
	cmpi.b	#$28,d0
	bge.s	nextrow
	add.l   #font,d0
	bra.L	blittit

nextrow:add.l	#font+$280-40,d0
	bra.L	blittit

sonderz:addi.b	#$41,d0
	cmpi.b	#$39,d0
	bgt.s	sz
	cmpi.b	#$30,d0
	bge.L	zahlen

sz:	cmp.b	#" ",d0
	bne.s	asz0
	move.l	#font+640+22,d0
	bra.L	blittit
asz0:	cmp.b	#"!",d0
	bne.s	asz1
	move.l	#font+1280+20,d0
	bra.L	blittit
asz1:	cmp.b	#"'",d0
	bne.s	asz2
	move.l	#font+1280+20+2,d0
	bra.L	blittit
asz2:	cmp.b	#"(",d0
	bne.s	asz3
	move.l	#font+1280+20+4,d0
	bra.L	blittit
asz3:	cmp.b	#")",d0
	bne.s	asz4
	move.l	#font+1280+20+6,d0
	bra.L	blittit
asz4:	cmp.b	#"?",d0
	bne.s	asz5
	move.l	#font+1280+20+8,d0
	bra.L	blittit
asz5:	cmp.b	#":",d0
	bne.s	asz6
	move.l	#font+1280+20+10,d0
	bra.L	blittit
asz6:	cmp.b	#",",d0
	bne.s	asz7
	move.l	#font+1280+20+12,d0
	bra.L	blittit
asz7:	cmp.b	#".",d0
	bne.s	asz8
	move.l	#font+1280+20+14,d0
	bra.L	blittit
asz8:	cmp.b	#"-",d0
	bne.s	asz9
	move.l	#font+1280+20+16,d0
asz9:	bra.L	blittit

zahlen:	addi.b	#$11,d0
	rol.w	#1,d0
	add.l	#font+$47e,d0


blittit:lea	bitplane+$28,a1
move_it2:move.l	#$09f00000,$0040(a3)		;bltcon0 reg (s.224)
	move.w	#$ffff,$0044(a3)		;first word mask
	move.l	d0,$0050(a3)			;quelldaten a
	move.l	a1,$0054(a3)			;zieldaten  d
	move.l	#$0026002a,$0064(a3)		;modulo a
	move.w	#16*64+1,$0058(a3)		;blitsize 1*20 words
back2:	rts
  

clear:	move.w	#$0100,$0040(a3)		;bltcon0 reg (s.224)
	move.l	addr(pc),$0054(a3)			;zieldaten  d
	move.w	#0,$0066(a3)			;modulo d 
	move.w	#200*64+20,$0058(a3)		;blitsize 1*20 words
	rts

blitttit:
	movem.l	d0-d7/a0-a6,-(a7)
	move.l	cos(pc),a1
	lea	taba(pc),a2
	moveq	#19,d6
	move.w	#$8000,d0
	move.w	#$0401,d5
	move.w	#$0dfc,$0040(a3)
	move.l	#$0026002a,$0062(a3)
	move.w	#$0026,$0066(a3)
	move.l	#bitplane,d2
	move.l	addr(pc),d3
	lea	$4c(a3),a0
	lea	$46(a3),a4
	lea	$50(a3),a5
	lea	$58(a3),a6
lolo:	moveq	#15,d7
hello:	move.l	d2,d1			;bitplane
	move.l 	d3,d4			;addr
	add.w	(a2),d1
	add.w	(a2)+,d4
	add.w	(a1)+,d4
;-------------------------
	move.l	d4,(a0)
	move.w	d0,(a4)
	movem.l	d1/d4,(a5)
	move.w	d5,(a6)		;blitsize 1*20 words
	ror.w	#1,d0
	dbra	d7,hello
	dbra	d6,lolo
;---------------------------------------------------------------
	add.l	#$4ea-8,cos		;next pos for sinus taby
	cmp.l	#cosend,cos
	blt.s	ua1
	sub.l	#cosend-tabd,cos
ua1:	movem.l	(a7)+,d0-d7/a0-a6
	rts




textout:movem.l	d0-d7/a0-a6,-(a7)
lines:	move.l	zeilen,newpos
	move.l	#tpointer,a3
	move.l	(a3),a2
	move.l	a2,a1
search:	cmp.b	#$0,(a1)+
	beq.s	found0
	addq.b	#1,zaehler
	bra.s	search
found0:	moveq	#20,d0
	sub.b	zaehler,d0
	asr.w	#1,d0
	mulu	#2,d0
	add.l	d0,newpos
	moveq	#0,d0
	add.b	zaehler,d0
	addq.l	#1,d0
	add.l	d0,Tpointer
	movem.l	d0-d7/a0-a6,-(a7)
	bsr	doline
	movem.l	(a7)+,d0-d7/a0-a6
	clr.l	newpos
	add.l	#18*40,zeilen
	move.l	zeilen,newpos
	clr.b	zaehler
	move.l	#tpointer,a0
	move.l	(a0),a0
	cmp.b	#$ff,(a0)
	bne.s	lines
	movem.l	(a7)+,d0-d7/a0-a6
	rts

doline:	moveq	#0,d2
	move.b	zaehler,d2
	subq.w	#1,d2
repeat:	moveq	#0,d0
	move.b	(a2)+,d0
	subi.b	#$41,d0
	bmi.s	bsonderz
	rol.w	#1,d0
	cmpi.b	#$28,d0
	bge.s	bnextrow
	add.l   #font,d0
	bra     bblittit

bnextrow:add.l	#font+$280-40,d0
	bra	bblittit

bsonderz:addi.b	#$41,d0
	cmpi.b	#$39,d0
	bgt.s	bsz
	cmpi.b	#$30,d0
	bge.s	bzahlen

bsz:	cmp.b	#" ",d0
	bne.s	sz0
	move.l	#font+640+22,d0
sz0:	cmp.b	#"!",d0
	bne.s	sz1
	move.l	#font+1280+20,d0
sz1:	cmp.b	#"'",d0
	bne.L	sz2
	move.l	#font+1280+20+2,d0
sz2:	cmp.b	#"(",d0
	bne.s	sz3
	move.l	#font+1280+20+4,d0
sz3:	cmp.b	#")",d0
	bne.s	sz4
	move.l	#font+1280+20+6,d0
sz4:	cmp.b	#"?",d0
	bne.s	sz5
	move.l	#font+1280+20+8,d0
sz5:	cmp.b	#":",d0
	bne.s	sz6
	move.l	#font+1280+20+10,d0
sz6:	cmp.b	#",",d0
	bne.s	sz7
	move.l	#font+1280+20+12,d0
sz7:	cmp.b	#".",d0
	bne.s	sz8
	move.l	#font+1280+20+14,d0
sz8:	cmp.b	#"-",d0
	bne.s	sz9
	move.l	#font+1280+20+16,d0
sz9:	bra.s	bblittit

bzahlen:addi.b	#$11,d0
	rol.w	#1,d0
	add.l	#font+$47e,d0


bblittit:move.l	d0,a0
	lea	bitplane2,a1
	add.l	newpos,a1
;		  fedcba9876543210
	move.w	#%0000100111110000,$dff040 ;bltcon0 reg (s.224)
	move.w	#%0000000000000000,$dff042 ;bltcon1 reg (s.225)
	move.l	#$ffffffff,$dff044     ;first word mask
	move.l	a0,$dff050            ;quelldaten a
	move.l	a1,$dff054            ;zieldaten  d
	move.w	#38,$dff064                ;modulo a
	move.w	#38,$dff066                ;modulo d 
	move.w	#16*64+1,$dff058      ;blitsize 1*20 words
	addq.l	#2,newpos
	dbra	d2,repeat
	rts


StarsON:lea	pos(pc),a0
	move.w	many(pc),d0
zufall:	move.w	$dff006,d1
	move.w	$dff004,d2
	add.w	d1,d1
	rol.w	d2,d1
	and.w	#$00ff,d1
	move.w	d1,(a0)+
	dbra	d0,zufall
init:	lea	spdat,a0
	move.l	a0,d0
	move.w	d0,sprites+6
	swap	d0
	move.w	d0,sprites+2
aco01:	moveq	#0,d0
	moveq	#0,d1
	lea	spdat,a0
	lea	pos(pc),a1
	lea	col(pc),a2
	lea 	speed(pc),a4
	move.w	#$6b00,d0
	move.w	many(pc),d1
aco02:	move.w	(a1)+,d2
	add.w	d0,d2
	move.w	d2,(a0)+
	add.w	#$0100,d0
	move.w	d0,d3
	add.w	pal(pc),d3
	move.w	d3,(a0)+
	cmp.w	#0,(a4)
	blt.s	q1
	cmp.w	#2,(a4)
	blt.s	q2
	cmp.w	#4,(a4)
	blt.s	q3
q1:	move.l	col(pc),(a0)+
	bra.s	bul
q2:	move.l	col+4(pc),(a0)+		
	bra.s	bul
q3:	move.l	col+8(pc),(a0)+
bul:	add.w	#$0100,d0
	addq	#2,a4
	cmp.w	#$0000,d0
	bne.s	contall
	move.w	#$0007,pal
contall:dbra	d1,aco02
	rts




stars:	lea	spdat,a0
	lea	speed(pc),a1
	move.w	many(pc),d0
sloop1:	move.w	(a1)+,d1
nexts:	add.b	d1,$1(a0)
	addq.w	#8,a0
	dbra	d0,sloop1
	rts



;------------------------------------------------------
newclist:			
dc.w 	$008e,$2a71,$0090,$ffd1
dc.w 	$0092,$0030,$0094,$00d8
dc.w 	$0108,44*3,$010a,44*3
dc.w 	$0102,$0000,$0104,$0000 
dc.w	$0100,$1200
sprites:
dc.w	$0120,$0000,$0122,$0000,$0124,$0000,$0126,$0000
dc.w	$0128,$0000,$012a,$0000,$012c,$0000,$012e,$0000
dc.w	$0130,$0000,$0132,$0000,$0134,$0000,$0136,$0000
dc.w	$0138,$0000,$013a,$0000,$013c,$0000,$013e,$0000
dc.w	$01a2,$0aaa,$01a4,$0666,$01a6,$0fff
bpl:
dc.w	$00e0,$0000,$00e2,0
dc.w	$00e4,$0000,$00e6,44
dc.w	$00e8,$0000,$00ea,88
dc.w	$00ec,$0000,$00ee,132
dc.w	$0180,$0000,$0182,$0eff,$0184,$0cee,$0186,$0bdd
dc.w	$0188,$0acc,$018a,$09bb,$018c,$08aa,$018e,$0799
dc.w	$0190,$0688,$0192,$0577,$0194,$0466,$0196,$0355
dc.w	$0198,$0244,$019a,$0233,$019c,$0122,$019e,$0122
dc.w	$0100,$4200
dc.w	$6109,$fffe,$0100,$0200,$0180,$0000,$0182,$0000
dc.w	$6909,$fffe,$0180,$0fff
dc.w	$6a09,$fffe,$0180,$0000
dc.w	$0090,$ffbf,$0092,$0038,$0094,$00d0
bp1:
dc.w	$00e0,$0007,$00e2,$8000 
dc.w	$00e8,$0007,$00ea,$c000+40
dc.w	$00e4,bitplane2/$10000,$00e6,bitplane2
dc.w	$0100,$3400
dc.w	$0108,$0000,$010a,$0000
dc.w	$0102,$0000
dc.w	$0180,$0000,$0182,$0006,$0184,$000f,$0186,$000a
taby:	blk.w	106*4
dc.w	$f709,$fffe,$0180,$0fff
dc.w	$f809,$fffe,$0180,$0000
dc.w	$ffff,$fffe


taba:
dc.w	0,0,0,0,0,0,0,0
dc.w	0,0,0,0,0,0,0,0
dc.w	2,2,2,2,2,2,2,2
dc.w	2,2,2,2,2,2,2,2
dc.w	4,4,4,4,4,4,4,4
dc.w	4,4,4,4,4,4,4,4
dc.w	6,6,6,6,6,6,6,6
dc.w	6,6,6,6,6,6,6,6
dc.w	8,8,8,8,8,8,8,8
dc.w	8,8,8,8,8,8,8,8
dc.w	10,10,10,10,10,10,10,10
dc.w	10,10,10,10,10,10,10,10
dc.w	12,12,12,12,12,12,12,12
dc.w	12,12,12,12,12,12,12,12
dc.w	14,14,14,14,14,14,14,14
dc.w	14,14,14,14,14,14,14,14
dc.w	16,16,16,16,16,16,16,16
dc.w	16,16,16,16,16,16,16,16
dc.w	18,18,18,18,18,18,18,18
dc.w	18,18,18,18,18,18,18,18
dc.w	20,20,20,20,20,20,20,20
dc.w	20,20,20,20,20,20,20,20
dc.w	22,22,22,22,22,22,22,22
dc.w	22,22,22,22,22,22,22,22
dc.w	24,24,24,24,24,24,24,24
dc.w	24,24,24,24,24,24,24,24
dc.w	26,26,26,26,26,26,26,26
dc.w	26,26,26,26,26,26,26,26
dc.w	28,28,28,28,28,28,28,28
dc.w	28,28,28,28,28,28,28,28
dc.w	30,30,30,30,30,30,30,30
dc.w	30,30,30,30,30,30,30,30
dc.w	32,32,32,32,32,32,32,32
dc.w	32,32,32,32,32,32,32,32
dc.w	34,34,34,34,34,34,34,34
dc.w	34,34,34,34,34,34,34,34
dc.w	36,36,36,36,36,36,36,36
dc.w	36,36,36,36,36,36,36,36
dc.w	38,38,38,38,38,38,38,38
dc.w	38,38,38,38,38,38,38,38
dc.w	40,40,40,40,40,40,40,40
dc.w	40,40,40,40,40,40,40,40

tabd:
dc.w 120*40,120*40,120*40,120*40,120*40,120*40,120*40,120*40
dc.w 120*40,120*40,120*40,120*40,120*40,119*40,119*40,119*40
dc.w 119*40,119*40,119*40,119*40,119*40,119*40,119*40,118*40
dc.w 118*40,118*40,118*40,118*40,118*40,117*40,117*40,117*40
dc.w 117*40,117*40,117*40,116*40,116*40,116*40,116*40,115*40
dc.w 115*40,115*40,115*40,115*40,114*40,114*40,114*40,113*40
dc.w 113*40,113*40,113*40,112*40,112*40,112*40,111*40,111*40
dc.w 111*40,111*40,110*40,110*40,110*40,109*40,109*40,108*40
dc.w 108*40,108*40,107*40,107*40,107*40,106*40,106*40,106*40
dc.w 105*40,105*40,104*40,104*40,103*40,103*40,103*40,102*40
dc.w 102*40,101*40,101*40,100*40,100*40,100*40,99*40,99*40,98*40
dc.w 98*40,97*40,97*40,96*40,96*40,95*40,95*40,94*40,94*40,93*40
dc.w 93*40,92*40,92*40,91*40,91*40,90*40,90*40,89*40,89*40,88*40
dc.w 88*40,87*40,87*40,86*40,86*40,85*40,85*40,84*40,83*40,83*40
dc.w 82*40,82*40,81*40,81*40,80*40,79*40,79*40,78*40,78*40,77*40
dc.w 77*40,76*40,75*40,75*40,74*40,74*40,73*40,73*40,72*40,71*40
dc.w 71*40,70*40,70*40,69*40,68*40,68*40,67*40,67*40,66*40,65*40
dc.w 65*40,64*40,64*40,63*40,62*40,62*40,61*40,61*40,60*40,59*40
dc.w 59*40,58*40,58*40,57*40,56*40,56*40,55*40,55*40,54*40,53*40
dc.w 53*40,52*40,52*40,51*40,50*40,50*40,49*40,49*40,48*40,48*40
dc.w 47*40,46*40,46*40,45*40,45*40,44*40,43*40,43*40,42*40,42*40
dc.w 41*40,41*40,40*40,39*40,39*40,38*40,38*40,37*40,37*40,36*40
dc.w 36*40,35*40,34*40,34*40,33*40,33*40,32*40,32*40,31*40,31*40
dc.w 30*40,30*40,29*40,29*40,28*40,28*40,27*40,27*40,26*40,26*40
dc.w 25*40,25*40,24*40,24*40,23*40,23*40,22*40,22*40,21*40,21*40
dc.w 20*40,20*40,20*40,19*40,19*40,18*40,18*40,17*40,17*40,17*40
dc.w 16*40,16*40,15*40,15*40,15*40,14*40,14*40,13*40,13*40,13*40
dc.w 12*40,12*40,12*40,11*40,11*40,11*40,10*40,10*40,10*40,9*40
dc.w 9*40,9*40,8*40,8*40,8*40,7*40,7*40,7*40,7*40,6*40,6*40,6*40
dc.w 6*40,5*40,5*40,5*40,5*40,4*40,4*40,4*40,4*40,3*40,3*40,3*40
dc.w 3*40,3*40,3*40,2*40,2*40,2*40,2*40,2*40,2*40,1*40,1*40,1*40
dc.w 1*40,1*40,1*40,1*40,1*40,1*40,1*40,0*40,0*40,0*40,0*40,0*40
dc.w 0*40,0*40,0*40,0*40,0*40,0*40,0*40,0*40,0*40,0*40,0*40,0*40
dc.w 0*40,0*40,0*40,0*40,0*40,0*40,0*40,0*40,0*40,1*40,1*40,1*40
dc.w 1*40,1*40,1*40,1*40,1*40,1*40,2*40,2*40,2*40,2*40,2*40,2*40
dc.w 2*40,3*40,3*40,3*40,3*40,3*40,4*40,4*40,4*40,4*40,4*40,5*40
dc.w 5*40,5*40,5*40,6*40,6*40,6*40,6*40,7*40,7*40,7*40,8*40,8*40
dc.w 8*40,8*40,9*40,9*40,9*40,10*40,10*40,10*40,11*40,11*40,11*40
dc.w 12*40,12*40,13*40,13*40,13*40,14*40,14*40,14*40,15*40,15*40
dc.w 16*40,16*40,16*40,17*40,17*40,18*40,18*40,19*40,19*40,19*40
dc.w 20*40,20*40,21*40,21*40,22*40,22*40,23*40,23*40,24*40,24*40
dc.w 25*40,25*40,26*40,26*40,26*40,27*40,28*40,28*40,29*40,29*40
dc.w 30*40,30*40,31*40,31*40,32*40,32*40,33*40,33*40,34*40,34*40
dc.w 35*40,35*40,36*40,37*40,37*40,38*40,38*40,39*40,39*40,40*40
dc.w 40*40,41*40,42*40,42*40,43*40,43*40,44*40,44*40,45*40,46*40
dc.w 46*40,47*40,47*40,48*40,49*40,49*40,50*40,50*40,51*40,51*40
dc.w 52*40,53*40,53*40,54*40,54*40,55*40,56*40,56*40,57*40,57*40
dc.w 58*40,59*40,59*40,60*40,60*40,61*40,62*40,62*40,63*40,63*40
dc.w 64*40,65*40,65*40,66*40,66*40,67*40,68*40,68*40,69*40,69*40
dc.w 70*40,71*40,71*40,72*40,72*40,73*40,74*40,74*40,75*40,75*40
dc.w 76*40,76*40,77*40,78*40,78*40,79*40,79*40,80*40,80*40,81*40
dc.w 82*40,82*40,83*40,83*40,84*40,84*40,85*40,85*40,86*40,87*40
dc.w 87*40,88*40,88*40,89*40,89*40,90*40,90*40,91*40,91*40,92*40
dc.w 92*40,93*40,93*40,94*40,94*40,95*40,95*40,96*40,96*40,97*40
dc.w 97*40,98*40,98*40,99*40,99*40,99*40,100*40,100*40,101*40
dc.w 101*40,102*40,102*40,103*40,103*40,103*40,104*40,104*40
dc.w 105*40,105*40,105*40,106*40,106*40,107*40,107*40,107*40
dc.w 108*40,108*40,108*40,109*40,109*40,109*40,110*40,110*40
dc.w 110*40,111*40,111*40,111*40,112*40,112*40,112*40,113*40
dc.w 113*40,113*40,113*40,114*40,114*40,114*40,114*40,115*40
dc.w 115*40,115*40,115*40,116*40,116*40,116*40,116*40,117*40
dc.w 117*40,117*40,117*40,117*40,117*40,118*40,118*40,118*40
dc.w 118*40,118*40,118*40,119*40,119*40,119*40,119*40,119*40
dc.w 119*40,119*40,119*40,119*40,119*40,120*40,120*40,120*40
dc.w 120*40,120*40,120*40,120*40,120*40,120*40,120*40,120*40
dc.w 120*40,120*40
cosend:
blk.w	$4ea+20,0

			;fedcba9876543210fedcba9876543210
col:		dc.l	%00000000000000010000000000000001
		dc.l	%00000000000000000000000000000001
		dc.l	%00000000000000010000000000000000
pos:		dc.w	$24,$21,$15,$38,$64,$79,$42,$4f,$38,$77
		dc.w	$55,$6d,$13,$56,$85,$74,$62,$cc,$8a,$7b
		dc.w	$ed,$e5,$76,$ac,$be,$78,$71,$27,$90,$56
		dc.w	$49,$a8,$2b,$83,$c7,$8d,$76,$e8,$73,$f6
		dc.w	$34,$56,$78,$12,$90,$cd,$ea,$56,$14,$78
		dc.w	$78,$45,$67,$12,$90,$eb,$a3,$3c,$c4,$4c
		dc.w	$88,$76,$53,$76,$5a,$97,$4c,$35,$98,$d7
		dc.w	$e4,$68,$96,$72,$4a,$3d,$58,$6c,$2d,$6e
		dc.w	$82,$7e,$64,$c4,$56,$53,$87,$63,$a9,$c8
		dc.w	$c4,$ed,$4d,$6c,$8a,$7c,$6d,$28,$37,$63
		dc.w	$68,$7a,$a6,$c8,$06,$d3,$d4,$e7,$70,$f0
		dc.w	$78,$45,$67,$12,$90,$eb,$a3,$3c,$c4,$4c
		dc.w	$c4,$ed,$4d,$6c,$8a,$7c,$6d,$28,$37,$63
		dc.w	$c4,$ed,$4d,$6c,$8a,$7c,$6d,$28,$37,$63

speed:		dc.w	1,3,2,4,3,5,1,4,2,3,1,3,2,4,3,4,1,2,1,2
		dc.w	1,2,1,3,1,4,1,5,2,1,2,2,2,3,2,4,2,5,3,1
		dc.w	3,2,3,3,3,4,3,5,4,1,4,2,4,3,4,4,4,5,1,5
		dc.w	2,5,3,2,4,5,5,4,1,4,2,4,3,4,4,4,5,3,1,3
		dc.w	2,3,3,3,4,3,5,2,1,2,2,2,3,2,4,2,5,1,1,1
		dc.w	2,1,3,1,4,1,5,5,5,3,2,3,1,2,3,4,3,2,1,3
		dc.w	1,2,1,3,1,4,1,5,2,1,3,1,2,3,2,4,2,5,3,1


pal:		dc.w	0
many:		dc.w	70
zaehler:	dc.b	0
even
Tpointer:	dc.l	text
newpos:		dc.l	0
zeilen:		dc.l	20*40
cou:		dc.l	0
lab:		dc.w	0
cos:dc.l	tabd
Tsave:dc.l	0
SRsave:	dc.w	0
INTsave:dc.w	0
DMAsave:dc.w	0
SPsave:dc.l	0
addr:dc.l	$70000+120
addr2:dc.l	$78000+120
font:		incbin	"fontbm"
logo:		incbin	"skid.raw"
cols:		incbin	"allergb"
;-------------------------------------------------------
TEXT:;	 12345678901234567890
dc.b	"SKID ROW",0
dc.b	"--------",0
dc.b	"PRESENTS",0
dc.b	"------------",0
dc.b	"MIDWINTER II",0
dc.b	"------------",0
dc.b	$ff,$ff
;-------------------------------------------------------
sct:		DC.B	"     SKID ROW     PROVIDES YOU WITH ANOTHER "
		DC.B	"HOT RELEASE... - FLAMES OF FREEDOM "
		DC.B	"  MIDWINTER II   GET IT BEFORE IT GETS YOU! - "						
                DC.B	"CRACKED BY THE PUBLIC ENEMY NO.1...   F F C   "
                DC.B    "... AND THE ORIGINAL " 
		DC.B	"WAS SUPPLIED (AS ALWAYS) BY...   S.S.R   "
                DC.B	" FOR THE LATEST UPCOMING WAREZ WRITE TO... "
		DC.B 	" P.O. BOX  10 - 4540 AMAY   BELGIUM   ... OR TO "
		DC.B	"- SKID ROW - POSTE RESTANTE, 8450 HAMMEL   DENMARK   "
		DC.B 	" OR ... PLK 052135   D   4300 ESSEN   UNITED GERMANY   "
		DC.B 	" OTHERWISE CALL OUR BULLETIN BOARD SYSTEMS... "
		DC.B	"   ALCATRAZ: 703-323-5997    -    AMIGA EAST:"
		DC.B 	" 804-499-2266    -    INQUISITION: 805-967-8833  "
		DC.B	"  -    MOTHERBOARD I: 215-944-9712    -  "
		DC.B	"  CREEPING DEATH: 314-781-5539    -    H.M.S. BOUNTY:"
		DC.B	" 714-563-2206    -  "
		DC.B	"OR CALL IN EUROPE...   THE JAM: 49-201-626-047    -  "
		DC.B	"  LIGHTHOUSE: 49-212-592-543    -    DIABOLIKA: 39-519-365-77   "
		DC.B	"      AND REMEMBER...   WE ARE THE WISDOM BEHIND THE CROWN   "
		DC.B	" ... GREAT INTRO BY...   SKYWALKER     AND THE   "
		DC.B	"LITTLE MUSIX (6KB!) BY...   SUBZERO          "
                DC.B	0
		even
Ende:
