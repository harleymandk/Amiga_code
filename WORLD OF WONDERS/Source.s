org  $40000
load $40000
>Extern "ram:cara",Cara
>Extern "ram:Pict-$98",Pict
>Extern "ram:data",Data

; ----- graphics.library -----
scrollraster=	-396
setapen=	-342
initbitmap= 	-390
initrastport= 	-198
; ----- exec.library     -----
openlibrary= 	-408
closelibrary= 	-414
forbid= 	-132
permit= 	-138

execbase= 	$04
movem.l 	d0-d7/a0-a6,-(a7)
move.l execbase,a6
; ---- open graphics.library ----
lea		gfxname,a1
jsr		openlibrary(a6)
move.l		d0,gfxbase	
jsr		forbid(a6)		

;-------------initialise l'ecran------------------------
;
move.l		gfxbase,a6		; base de la graphics
move.l		#bitmap,a0		;pointeur sur la bitmap 
move.l		#$02,d0			;nbre de 
move.l		#368,d1			;largeur de l'ecran
move.l		#200,d2			;hauteur de l'ecran

jsr		initbitmap(a6)
jsr pf1
lea		rastport,a1	
jsr		initrastport(a6)
move.l		#bitmap,r_bitmap
move.l #$70000,a0
move.l #$35e8,d0
here:
clr.l (a0)+
dbra d0,here
;--------------------initialise routines

jsr sterne
jsr setco
jsr courbe
jsr ppicture
jsr initoc
;----
move.l gfxbase,a0
add.l #$32,a0
move.l (a0),oldcopper
	move.l	#newcopper,(a0)	;activate new Coplist
jsr start_muzak
;-------------------Test the mouse-------------------------
wait:
cmp.b #242,$dff006 
bne wait
jsr rout
jsr effets
jsr effets2
jsr stern_move
btst		#6,$bfe001		
bne.s wait
;
;-----------------On ferme tout--------------------
;
jsr stop_muzak
	move.l	gfxbase,a1		;activate old Copperlist
	add.l #$32,a1
move.l oldcopper,(a1)
move.l execbase,a6
move.l		gfxbase,a1		
jsr		closelibrary(a6)	
move.l		gfxbase,a0		
move.l		execbase,a6		 
jsr		permit(a6)		
movem.l		(a7)+,d0-d7/a0-a6
clr.l d0
rts
newcopper:
dc.w $9c,$8010
dc.w $180,0,$190,0
dc.w $008e,$2c81,$0090,$2ec1
dc.w $0092,$0038,$0094,$00d0
dc.w $0108,$0006,$010a,$0006
dc.w $0102,$0000,$0104,$0000
dc.w $0100,$3200
dc.w $00e0,$0007,$00e2,$0000;bitplane 1
dc.w $00e4,$0007,$00e6,$23f0;bitplane 2
dc.w $00e8,$0007,$00ea,$47e0;bitplane 3
dc.w $00ec,$0007,$00ee,$6bd0;bitplane 4
dc.w $00f0,$0007,$00f2,$8fc0;bitplane 5
dc.w $00f4,$0007,$00f6,$b3b0;bitplane 6
sprite0:
dc.l $01200000,$01220000,$01240000,$01260000,$01280000,$012a0000
dc.l $012c0000,$012e0000,$01300000,$01320000,$01340000,$01360000
dc.l $01380000,$013a0000,$013c0000,$013e0000
dc.w $182,$fff,$184,$ddd,$186,$bbb,$188,$999,$18a,$777
dc.w $18c,$555,$18e,$930 ;,$18c,$559,$18e,$77f
dc.w $1a0,$111,$1a2,$333,$1a4,$555,$1a6,$777

eff:
blk.b 109*24
dc.w $9a09,$fffe,$100,$1200
dc.w $9b09,$fffe,$e2,$6bd0,$e6,$6bd0,$ea,$6bd0,$ee,$6bd0
ml1: dc.w $9d09,$fffe,$100,$4600
dc.w $e2,$1226
dc.w $ea,$5a06
dc.w $e6,$3616
dc.w $ee,$7df6
DC.W $182,0,$184,0,$186,0,$192,0,$194,0,$196,0,$102,0
ml2: dc.w $9e09,$fffe
dc.w $182,$004,$184,$fff,$186,$444,$192,$456,$194,$789,$196,$234
coco: blk.b 32*12
ml3: dc.w $Be09,$fffe
dc.w $100,$1200
DC.W $182,0,$184,0,$186,0,$192,0,$194,0,$196,0,$102,0
dc.w $ffff,$fffe	;end of copper list
gfxbase:
dc.l 	0
bitmap:
blk.w 	4,0
plane1:
dc.l 	0,0,0
rastport:
dc.l  	0
r_bitmap:
blk.l 26,0
gfxname:
dc.b 	"graphics.library",0
even
dosbase:
dc.l 0
mg1=46*102
mg2=46*133
b1=$70000
b2=$723f0
b3=$747e0
b4=$76bd0
b5=$78fc0
b6=$7b3b0
haut1 = b1+mg1+2
haut2 = b3+mg1+2
haut21 = b2+mg1
haut22 = b4+mg1
bas1 = b1+mg2+2
bas2 = b3+mg2+2
bas21 = b2+mg2
bas22 = b4+mg2
picdata: dc.l cara+$98
pdata: dc.l pict

rout:
move.l gfxbase,a6
jsr aVANT
jsr scrollhor
rts
;------effets couleurs sur scrolling
ccol:
move.l ccolor,d0
move.l drc,d1
add.l d1,d0
add.l d1,d0
move.l d0,ccolor
move.w culig,d4
lea		coco,a1
move.l #31,d3
move.l  ccolor,d1
cmp.l #chcolf,d1
bne.L bcb2
move.l #chcol,ccolor
bra bcb3
bcb2:
cmp.l #chcol-2,d1
bne bcb3
move.l #chcolf-2,ccolor
bcb3:
move.l ccolor,a0
boucle:
move.w d4,(a1)
add.l #8,a1
move.w #$182,(a1)+
move.w (a0)+,(a1)+
add.l #$100,d4
cmp.l #chcolf,a0
bne boucle2
lea chcol,a0
boucle2:
dbra d3,boucle
exity2:
rts
scrup:
cmp.b #0,wt
bne coo

cmp.l #0,drc
beq coo
jsr trans
jsr anum2
coo:
jsr pf1
move.l drc,d1
jsr scrold
rts
move.l gfxbase,a6
scrold:
clr.l d0
clr.l d2
move.l dlig,d3
move.l #320,d4
move.l flig,d5
lea rastport,a1
jsr scrollraster(a6)
rts
trans:
cmp.l #1,drc
bne adrc2
move.l #haut1,a0
move.l #haut2,a1
move.l #haut21-46,a3
move.l #haut22-46,a4
bra suite
adrc2:
move.l #haut1-46,a3
move.l #haut2-46,a4
move.l #haut21,a0
move.l #haut22,a1
bra suite
anum2:
cmp.l #1,drc2
bne bdrc2
move.l #bas1,a0
move.l #bas2,a1
move.l #bas21+46,a3
move.l #bas22+46,a4
bra suite
bdrc2:
move.l #bas1+46,a3
move.l #bas2+46,a4
move.l #bas21,a0
move.l #bas22,a1
suite:
move.l #8,d3
bobo:
move.l (a0),(a3)+
move.l (a1),(a4)+
add.l #4,a0
add.l #4,a1
dbra d3,bobo
move.w (a0),(a3)
move.w (a1),(a4)
rts
dlig: dc.l 101
flig: dc.l 134
courbe:
move.w culig,d0
lea coco,a0
lea colist,a1
move.l #31,d1
bb3:
move.w d0,(a0)+
move.w #$fffe,(a0)+
move.w #$102,(a0)+
clr.b (a0)+
move.b (a1),(a0)+
add.w #$100,d0
add.l #1,a1
add.l #4,a0
dbra d1,bb3
rts

setco:
move.l #31,d3
lea colist,a0
la:
move.b (a0),d0
move.b #13,d1
move.b d0,d2
asl.b #4,d2
sub.b d0,d1
add.b d1,d2
move.b d2,(a0)+
dbra d3,la
rts
pf1:
move.l #b1,plane1
move.l #b3,plane1+4
move.l #b5,plane1+8
rts
pf2:
move.l #b2,plane1
move.l #b4,plane1+4
move.l #b6,plane1+8
rts
culig: dc.w $9e09

colist:
dc.b12,10,8,6,5,4,3,3,2,2,1,1,0,0,0,0
dc.b0,0,0,0,1,1,2,2,3,3,4,5,6,8,10,12
even
drc2: dc.l 1
drc: dc.l -1
num: dc.l 0

;------ scrolling horizontal
avant:
move.w ml1,d0
cmp.w #$9b09,d0
bne.s  prd
move.w #$100,inc
bra.s  pol
prd:
cmp.w #$cb09,d0
bne.s  prd2
move.w #-$100,inc
pol:
cmp.b #0,rol
beq prd2
move.l drc,d0
move.l drc2,drc
move.l d0,drc2
prd2:
move.w inc,d0
add.w d0,ml3
add.w d0,ml1
add.w d0,ml2
add.w d0,culig
rts
scrollhor:
jsr ccol
jsr scrup
jsr pf1
move.l		gfxbase,a6		; scrolling
lea		rastport,a1
move.l		scspeed2,d0	 
move.l #0,d1		
move.l		#16,d2		
move.l		dlig,d3		
move.l		#360,d4		
move.l		flig,d5		
jsr		 scrollraster(a6);deplacement d'un rectangle image
JSR PF2
lea rastport,a1
move.l drc2,d1
move.l scspeed2,d0
cmp.b #0,wt
beq ty
clr.l d0
ty:
clr.l d2
move.l dlig,d3
move.l #320,d4
move.l flig,d5
jsr scrollraster(a6)
move.l scspeed2,d0
cmp.l #0,scspeed2
bne.s sspeed
sub.l #1,scount
cmp.l #0,scount
bne.L jeaN
move.l #4,scspeed2
move.b #10,scspeed
bra.L nxtcar
sspeed:
cmp.l #40,scspeed2
beq.s uu
sub.b #$01,rows
bne.L		continue1
move.b scspeed,rows	;
uu:
move.l		gfxbase,a6
lea		rastport,a1
move.l		dlig,d0	
add.l #1,d0
move.l		#46,d1
move.l		#40,d2
nxtcar:
move.l char_adresse,a0
move.b (a0),d3
cmp.b #$ff,d3
beq stt
cmp.b #$fe,d3
beq stt2
cmp.b #$fd,d3
beq stt3
cmp.b #$fc,d3
beq stc
cmp.b #$fb,d3
beq stc2
cmp.b #$fa,d3
beq sreb
cmp.b #$f9,d3
beq upreb
cmp.b #$f8,d3
beq dwreb
cmp.b #$f7,d3
beq rolst
cmp.b #$f6,d3
beq rolup
cmp.b #$f5,d3
beq roldw
cmp.b #$20,d3
bcc.l plusloin
cmp.b #0,d3
beq.L stopsc
clr.w d4;vitesse du scrolling
chscc:
move.w d3,d4
move.w #40,d5
divu d4,d5
move.l d4,scspeed2
move.b d5,scspeed
add.l #1,char_adresse
bra.L avant
rolst:;roulement
clr.b rol
bra stt
rolup:
move.w #-$100,inc
move.b #1,rol
bra stt2
roldw:
move.w #$100,inc
move.b #1,rol
bra stt3
stc:; scrolling arriere
move.b #1,wt
bra rep
stc2:
move.b #0,wt
bra rep
stopsc:;arret sur place du scrolling
add.l #1,a0
clr.l 0
move.b (a0),d0
mulu #2,d0
move.l d0,scount
clr.l scspeed2
add.l #2,char_adresse
bra.L avant
stt:
clr.l drc
clr.l drc2
rep:
add.l #1,char_adresse
bra.l nxtcar
sreb: ;rebonds
clr.w inc
bra.s  rep
upreb:
move.w #-$100,inc
bra.s rep
dwreb:
move.w #$100,inc
bra.s rep
stt2:;direction rotation
move.l #1,drc
move.l #-1,drc2
bra.s  rep
stt3:
move.l #-1,drc
move.l #1,drc2
bra.s  rep
bra.s paslebon
ieux:
add.l #$01,char_adresse
bra.L nxtcar
paslebon: 
plusloin:
bsr.s		text1           

add.l #$01,char_adresse		
cmp.l #finmessage,char_adresse	
bne.s		continue1
move.l #message,char_adresse
jean:
continue1:
oap:
rts

;---------------affichage 1 caractere----------------------
text1:
mulu d0,d1
add.l #40,d1
cmp.b #$20,d3
beq espace
lea let,a5
clr.l d6
testl:
cmp.b (a5,d6),d3
beq sort
add.l #1,d6
bra testl
sort:
move.b d6,d3
;----------calcul adresse des datas du caractere------

clr.w d4
move.b d3,d4
divu #8,d4
move.w d4,d5
swap d4
mulu #2560,d5
mulu #5,d4
add.l d4,d5
add.l picdata,d5
;add.l #200,d5
move.l d5,a0
;---------calcul adresse sur l'ecran
move.l d1,d0
add.l #b1,d0
move.l d0,dest1
move.l d1,dest2
add.l #b3,dest2
;--------affiche le caractere
move.l dest1,a1
move.l dest2,a2
move.l #31,d3
caraboucle:
move.l #4,d2
cb2:
move.b (a0),(a1)+
move.b 40(a0),(a2)+
add.l #1,a0
dbra d2,cb2
add.l #75,a0
add.l #41,a1
add.l #41,a2
dbra d3,caraboucle
espace:
rts
dest1: dc.l 0
dest2: dc.l 0
rows: dc.b 1
scspeed: dc.b 10
char_adresse: dc.l message
scount: dc.l 0
scspeed2: dc.l 4
message: 
dc.b $ff,$f8,"...WORLD OF WONDERS IS PROUD TO PRESENT "
dc.b " ???????????????...SPREAD BY"
dc.b "  MEGA   ",$fe,0,16,$ff,$fc
dc.b " FORCE " ,$f5,$fb,$fd,0,128+64,$ff,$fa,$FC,"       ",$FB,$FD,0
DC.B 48,$f7," AGAIN! ",$f8,$fe,0,64,$fa,$FF,
DC.B " ...NOW WE ARE HERE WITH SOME VERY "
dc.b $ff,$f8,"   HOT  ",$fe,0,16,$ff,$fc
dc.b " STUFF ",$f5,$fb,$fd,0,128,$ff
DC.B " ...WATCH OUT FOR MORE HOT   ",$F7
DC.B $FD," STUFF! ",$F5,0,120,"        ",$ff
dc.b "MEGAFORCE HOLLAND WILL SEND SOME SPECIAL HIP HOPS TO   . "
dc.b "MEGAFORCE FRANCE AND TO ALL MEMBERS OF WORLD OF WONDERS"
DC.B "...SEE YOU  ",$F7
DC.B $FD," LATER! ",$F5,0,120,"   ..."
DC.B "                                            "
dc.b 4," "

FINMESSAGE:
; 1,2,4,5,8,20:vitesses
; $ff:pas d'on de rotations
; $fe:rotation vers le haut
; $fd:rotation vers le bas
; $fc:stop scrolling arriere
; $fb:reactive scrolling arriere
; $fa:stop rebonds
; $f9:rebonds depart vers le haut
; $f8:rebonds depart vers le bas
; $f6;fait rouler le scrolling sur l'ecran depart vers le haut
; $f5;fait rouler le scrolling sur l'ecran depart vers le bas
; $f7:arrete le roulement
let: dc.b "ABCDEFGHIJKLMNOPQRSTUVWXYZ.?!90r12345678"
even
ccolor:  dc.l chcol
dc.w $f06
chcol:
		dc.w	$f00,$f00,$f00,$f00,$000
		dc.w	$d00,$d00,$d00,$d00,$000
		dc.w	$b00,$b00,$b00,$b00,$000
		dc.w	$900,$900,$900,$900,$000
		dc.w	$700,$700,$700,$700,$000
		dc.w 	$500,$500,$500,$500,$000
		dc.w	$300,$300,$300,$300
		dc.w	$000,$500,$500,$500,$500
		dc.w	$000,$700,$700,$700,$700
		dc.w	$000,$900,$900,$900,$900
		dc.w 	$000,$b00,$b00,$b00,$b00
		dc.w	$000,$d00,$d00,$d00,$d00
		dc.w	$000,$f00,$f00,$f00,$f00
chcolf:
wt: dc.b 0
rol:dc.b 0
oldcopper: dc.l 0
inc: dc.w 0
sterne: move.l #sprite,d0
	move.w	d0,sprite0+6
	swap	d0
	move.w	d0,sprite0+2
clr.l	d0			;Set stars (as Sprite 1, which
	clr.l	d2			; reuses the DMA channel)
	lea	xpos(pc),a0		;table with X-coordinates
	lea	speeed(pc),a2		;table with speed/color (1-5)
	lea	sprite(pc),a1		;sprite start (Datalist)
	move.b	#$20,d0			;start at line 32
sternloop:
	clr.l	d1
	lea	inhalt-4(pc),a3		;table with color
	move.b	d0,(a1)+		;set sprite vertikal
	move.b	(a0)+,(a1)+		;set sprite horizontal
	addq.w	#1,d0			;add 1 for V-stop position
	move.b	d0,(a1)+		;set vertikal stop
	move.b	d2,(a1)+		;sprite vert. < or > 255 ??
	move.b	(a2)+,d1
	lsl.w	#2,d1
	add.l	d1,a3
	move.l	(a3),(a1)+		;set color
	addq.w	#1,d0			;next sprite reuse !!
	cmp.w	#$00fe,d0		;until line 254 (then I must
	ble.S	sternloop		; change vertikal bit 8 to 1
	moveq	#6,d2			;<= this is it (Bit8 = 1)
	cmp.w	#$0128,d0		;end in line 296
	bne.S	sternloop
	rts				;all stars are set

stern_move:
	move.w	#131,d0			;move 306 points
	lea	speeed(pc),a2		;table with speed (1/5)
	lea	sprite+1(pc),a1		;1. sprite X-position
move_loop:
	move.b	(a1),d1			;sprite X to d1
	move.b	(a2)+,d2		;speed to d2
	add.b	d2,d1			;add d1 to d2 (stars to the
	move.b	d1,(a1)			;              right)
	add.l	#8,a1			;point to next line
	dbf	d0,move_loop		;until all stars are moved
	rts				;end of sprite moving !

inhalt:	dc.l $80000000,$00008000,$00008000,$80008000
	dc.l $80008000

speeed:	dc.b 2,4,1,3,5,3,2,1,3,2,5,1,4,3,4,3,4,1,3,2,5,4,3,2,5
	dc.b 2,3,1,5,4,1,4,2,3,1,4,3,2,5,1,3,2,4,5,1,2,4,5,3,1
	dc.b 4,3,5,1,2,1,4,4,5,3,2,4,1,5,2,4,2,5,3,5,3,4,3,1,4
	dc.b 4,5,3,2,4,1,3,4,1,3,2,5,4,1,3,4,2,3,1,4,3,4,1,2,5
	dc.b 3,5,1,4,3,5,2,4,2,5,1,4,2,3,4,1,3,5,2,4,3,2,1,5,3
	dc.b 4,3,5,1,2,4,1


xpos:	dc.b $1e,$4f,$ab,$dd,$1b,$bb,$6a,$f3,$88,$ab,$f3,$73
	dc.b $27,$8a,$0e,$22,$bc,$3b,$20,$0e,$91,$14,$59,$f0
	dc.b $d5,$79,$b0,$32,$d3,$55,$b1,$95,$89,$5b,$c3,$99
	dc.b $01,$30,$d4,$c3,$b9,$a1,$7e,$91,$09,$4e,$70,$a0
	dc.b $c0,$5b,$1a,$98,$81,$d3,$22,$48,$91,$c5,$ff,$12
	dc.b $45,$95,$a1,$39,$e0,$84,$05,$b4,$af,$76,$72,$19
	dc.b $49,$58,$c0,$d2,$e2,$92,$19,$f5,$06,$35,$a8,$d3
	dc.b $89,$7e,$29,$90,$a4,$12,$b9,$c0,$e3,$88,$41,$2e
	dc.b $59,$12,$f0,$c3,$6e,$91,$2c,$55,$79,$1c,$01,$96
	dc.b $a5,$c1,$d9,$e7,$14,$86,$10,$99,$d3,$69,$a3,$c8
	dc.b $93,$12,$f5,$28,$c2,$39,$92,$a0,$38,$bb,$e1,$e3
sprite: blk.l 265
id=0
max=108
linmem=$68000
tnt1: dc.l anil-4
anil: dc.l pow1
dc.l pow2
dc.l pow3
dc.l pow4
dc.l pow5
dc.l pow4
dc.l pow5
dc.l pow4
dc.l pow5
dc.l pow7
dc.l pow6
dc.l pow8
anilf:
dc.l 0

tnt2: dc.l 0
tnt3: dc.l 0
tnt4: dc.l 0
tnt5: dc.l 53
dtnt5: dc.l linmem+318-6
in5: dc.w 0
effets2:
move.l #eff+14,a1
move.l #linmem,a0
clr.l d2
clr.b d5
dest:
jsr pow
rts
inv:
move.w #1,in5
rts
pow:
cmp.w #0,in5
beq inv
clr.w in5
sub.l #6,dtnt5
move.l dtnt5,a0
cmp.l #0,tnt5
beq bigo
move.l tnt5,d3
move.b #1,d5
jsr oneline
move.l tnt2,d3
clr.b d5
jsr oneline
move.l #444,d3
jsr empline
sub.l #1,tnt5
add.l #1,tnt2
rts
pow1:
cmp.l #54,tnt3
beq pbigo
move.l tnt3,d3
jsr twolines
move.l tnt2,d3
jsr oneline
move.l #444,d3
jsr empline
sub.l #1,tnt2
add.l #1,tnt3
rts

pbigo:
move.l #51,tnt3
bigo:
add.l #4,tnt1
cmp.l #anilf,tnt1
bne nniet
move.l #anil,tnt1
move.l #53,tnt2
clr.l tnt3
clr.l tnt4
nniet:
move.l tnt1,a0
move.l (a0),dest+2
rts
pow2:
cmp.l #53,tnt4
beq bigo
move.l tnt4,d3
jsr empline
move.l tnt3,d3
jsr twolines
move.l #444,d3
jsr oneline
sub.l #1,tnt3
add.l #1,tnt4
rts
pow3:
cmp.l #27,tnt4
beq pbigo2
move.l tnt4,d3
jsr empline
move.l #53,d3
jsr oneline
move.l #444,d3
jsr empline
sub.l #1,tnt4
rts
pbigo2:
clr.l tnt3
move.l #53,tnt2
bra bigo
pow4:
cmp.l #1,tnt4
beq bigo
move.l tnt4,d3
jsr empline
move.l tnt3,d3
jsr twolines
move.l tnt2,d3
jsr oneline
move.l tnt3,d3
jsr twolines

sub.l #1,tnt4
sub.l #2,tnt2
add.l #1,tnt3
rts
pow5:
cmp.l #27,tnt4
beq bigo
move.l tnt4,d3
jsr empline
move.l tnt3,d3
jsr twolines
move.l tnt2,d3
jsr oneline
move.l tnt3,d3
jsr twolines
move.l #100,d3
jsr empline

add.l #1,tnt4

add.l #2,tnt2
sub.l #1,tnt3
rts
pow6:
cmp.l #1,tnt4
beq pbigo3
move.l tnt4,d3
jsr empline
move.l tnt3,d3
jsr twolines
move.l tnt2,d3
jsr oneline
sub.l #1,tnt2
add.l #1,tnt3
sub.l #1,tnt4
rts
pbigo3:
clr.l tnt2
move.l #51,tnt3
bra bigo
clr.l tnt2
bra bigo
pow8:
cmp.l #53,tnt2
beq pbigo
move.l #2,d3
jsr empline
move.l tnt3,d3
jsr twolines
move.l tnt2,d3
jsr oneline
move.l #100,d3
jsr empline
add.l #1,tnt2
sub.l #1,tnt3
rts

pow7:
move.l #53,tnt2
clr.l tnt3
cmp.l #53,tnt4
beq pbigo2
move.l tnt4,d3
jsr empline
move.l #53,d3
jsr oneline
add.l #1,tnt4
rts
oneline:
cmp.l #-1,d3
beq prts
foroot:
move.w (a0),(a1)
add.l #4,a1
move.w 2(a0),(a1)
add.l #4,a1
move.w 4(a0),(a1)
add.l #16,a1
cmp.b #0,d5
bne jumpit
add.l #6,a0
jumpit:
add.l #1,d2
cmp.l #max,d2
bcc prts
dbra d3,foroot
prts:
rts
twolines:
cmp.l #-1,d3
beq prts
foroot2:
move.w (a0),(a1)
add.l #4,a1
move.w 2(a0),(a1)
add.l #4,a1
move.w 4(a0),(a1)
add.l #16,a1
move.w (a0),(a1)
add.l #4,a1
move.w 2(a0),(a1)
add.l #4,a1
move.w 4(a0),(a1)
add.l #16,a1
add.l #6,a0
add.l #2,d2
cmp.l #max,d2
bcc prts
dbra d3,foroot2
rts
empline:
foroot3:
clr.w (a1)
add.l #4,a1
clr.w (a1)
add.l #4,a1
clr.w (a1)
add.l #16,a1
add.l #1,d2
cmp.l #max,d2
bcc prts
dbra d3,foroot3
rts
 
ppicture:
move.l #pict+$98,a3
move.l #linmem,a0
move.l #b6,d0
move.l d0,a1
move.l #60*3*40,d2

move.l d2,d3
jjr3:
move.w (a3),(a1)+
add.w #2,a3
dbra d3,jjr3

move.l #60*3,d3
jjr:
move.w d0,(a0)+
add.l #40,d0
dbra d3,jjr
move.l #50*3,d3
jjr2:
move.w #$6bd0,(a0)+
dbra d3,jjr2
rts
initoc:
move.l #108,d3
move.w #$2b09,d0
move.l #linmem,a2
lea eff,a0
boc:
move.w d0,(a0)+
move.w #$fffe,(a0)+
move.w #$102,(a0)+
add.l #2,a0
move.w #$18e,(a0)+
add.l #2,a0
move.w #$e2,(a0)+
move.w (a2),(a0)+
move.w #$e6,(a0)+
move.w 2(a2),(a0)+
move.w #$eA,(a0)+
move.w 4(a2),(a0)+
add.l #6,a2

add.w #$100,d0
dbra d3,boc
;------effets couleurs sur MegaForce
effets:
lea		eff+10,a1
move.l anbl,d3
move.l  mcu,d1
cmp.l #rainbowf,d1
bne.L br2
move.l #rainbow,mcu
br2:
move.l mcu,a0
bocle:
move (a0)+,(a1)
add.l #24,a1
cmp.l #rainbowf,a0
bne bocle2
lea rainbow,a0
bocle2:
dbra d3,bocle
add.l #2,mcu
;---------ondulement du megaforce
move.l cnbl,d3
lea eff+6,a1
move.l mocu,d1
cmp.l #sinusf,d1
bne epl
move.l #sinus,mocu
epl:
move.l mocu,a0
rap:
move.w (a0)+,(a1)
add.l #24,a1
cmp.l #sinusf,a0
bne epl2
lea sinus,a0
epl2:
dbra d3,rap
add.l #2,mocu
rts

cnbl: dc.l 108

anbl: dc.l 108
mocu: dc.l sinus
mcu: dc.l rainbow
rainbow:	dc.w  $f00,$f10,$f20,$f30,$f40,$f50,$f60,$f70,$f80
		dc.w  $f90,$fa0,$fb0,$fc0,$fd0,$fe0,$ff0,$ef0,$df0
		dc.w  $cf0,$bf0,$af0,$9f0,$8f0,$7f0,$6f0,$5f0,$4f0
		dc.w  $3f0,$2f0,$1f0,$0f0,$0f1,$0f2,$0f3,$0f4,$0f5
		dc.w  $0f6,$0f7,$0f8,$0f9,$0fa,$0fb,$0fc,$0fd,$0fe
		dc.w  $0ff,$0ef,$0df,$0cf,$0bf,$0af,$09f,$08f,$07f
		dc.w  $06f,$05f,$04f,$03f,$02f,$01f,$00f,$00e,$00d
		dc.w  $00c,$00b,$00a,$009,$008,$007,$006,$005,$004
		dc.w  $003,$002,$001,$000,$100,$200,$300,$400,$500
		dc.w  $600,$700,$800,$900,$a00,$b00,$c00,$d00,$e00
rainbowf:;Thanks CRAZY TYPER for the colors
sinus:	dc.w	$77,$66,$66,$55,$55,$44,$44,$33,$33,$22,$22
	dc.w	$11,$11,$11,$00,$00,$00,$00,$00,$11,$11,$11
	dc.w	$22,$22,$33,$33,$44,$44,$55,$55,$66,$66,$77
	dc.w	$88,$88,$99,$99,$AA,$AA,$BB,$BB,$CC,$CC,$DD
	dc.w	$DD,$DD,$EE,$EE,$EE,$EE,$EE,$DD,$DD,$DD,$CC
	dc.w	$CC,$BB,$BB,$AA,$AA,$99,$99,$88,$88
sinusf:

cara: blk.b 13000,0
pict: blk.b 19200,0

******************************************
* Master Soundtracker V1.0 replayroutine *
* based on V9.0 of DOC *******************
******************************************

* Improved by TIP of The New Masters in JULY 1988 *

start:	bsr.s	start_muzak

main:	btst	#6,$bfe001
	bne.s	main

	bsr.L	stop_muzak
	moveq	#0,d0
	rts

start_muzak:
	move.l	#data,muzakoffset	;** get offset

init0:	move.l	muzakoffset,a0		;** get highest used pattern
	add.l	#472,a0
	move.l	#$80,d0
	clr.l	d1
init1:	move.l	d1,d2
	subq.w	#1,d0
init2:	move.b	(a0)+,d1
	cmp.b	d2,d1
	bgt.s	init1
	dbf	d0,init2
	addq.b	#1,d2

init3:	move.l	muzakoffset,a0		;** calc samplepointers
	lea	pointers(pc),a1
	lsl.l	#8,d2
	lsl.l	#2,d2
	add.l	#600,d2
	add.l	a0,d2
	moveq	#14,d0
init4:	move.l	d2,(a1)+
	clr.l	d1
	move.w	42(a0),d1
	lsl.l	#1,d1
	add.l	d1,d2
	add.l	#30,a0
	dbf	d0,init4

init5:	clr.w	$dff0a8			;** clear used values
	clr.w	$dff0b8
	clr.w	$dff0c8
	clr.w	$dff0d8
	clr.w	timpos
	clr.l	trkpos
	clr.l	patpos

init6:	move.l	muzakoffset,a0		;** initialize timer irq
	move.b	470(a0),numpat+1	;number of patterns
	move.l	$6c.w,lev3save+2
	move.l	#lev3interrupt,$6c.w
	rts

stop_muzak:
	move.l	lev3save+2,$6c.w
	clr.w	$dff0a8
	clr.w	$dff0b8
	clr.w	$dff0c8
	clr.w	$dff0d8
	move.w	#$f,$dff096
	rts

lev3interrupt:
	bsr.s	replay_muzak
lev3save:
	jmp	$0

replay_muzak:
	movem.l	d0-d7/a0-a6,-(a7)
	addq.w	#1,timpos
speed:	cmp.w	#6,timpos
	beq.L	replaystep

chaneleffects:				;** seek effects
	lea	datach0(pc),a6
	tst.b	3(a6)
	beq.s	ceff1
	lea	$dff0a0,a5
	bsr.s	ceff5
ceff1:	lea	datach1(pc),a6
	tst.b	3(a6)
	beq.s	ceff2
	lea	$dff0b0,a5
	bsr.s	ceff5
ceff2:	lea	datach2(pc),a6
	tst.b	3(a6)
	beq.s	ceff3
	lea	$dff0c0,a5
	bsr.s	ceff5
ceff3:	lea	datach3(pc),a6
	tst.b	3(a6)
	beq.s	ceff4
	lea	$dff0d0,a5
	bsr.s	ceff5
ceff4:	movem.l	(a7)+,d0-d7/a0-a6
	rts

ceff5:	move.b	2(a6),d0		;room for some more
	and.b	#$f,d0			;implementations below
	tst.b	d0
	beq.s	arpreggiato
	cmp.b	#1,d0
	beq.L	pitchup
	cmp.b	#2,d0
	beq.L	pitchdown
	cmp.b	#12,d0
	beq.L	setvol
	cmp.b	#14,d0
	beq.L	setfilt
	cmp.b	#15,d0
	beq.L	setspeed
	rts

arpreggiato:				;** spread by time
	cmp.w	#1,timpos
	beq.s	arp1
	cmp.w	#2,timpos
	beq.s	arp2
	cmp.w	#3,timpos
	beq.s	arp3
	cmp.w	#4,timpos
	beq.s	arp1
	cmp.w	#5,timpos
	beq.s	arp2
	rts

arp1:	clr.l	d0			;** get higher note-values
	move.b	3(a6),d0		;   or play original
	lsr.b	#4,d0
	bra.s	arp4
arp2:	clr.l	d0
	move.b	3(a6),d0
	and.b	#$f,d0
	bra.s	arp4
arp3:	move.w	16(a6),d2
	bra.s	arp6
arp4:	lsl.w	#1,d0
	clr.l	d1
	move.w	16(a6),d1
	lea	notetable,a0
arp5:	move.w	(a0,d0.w),d2
	cmp.w	(a0),d1
	beq.s	arp6
	addq.l	#2,a0
	bra.s	arp5
arp6:	move.w	d2,6(a5)
	rts

pitchdown:
	bsr.s	newrou
	clr.l	d0
	move.b	3(a6),d0
	and.b	#$f,d0
	add.w	d0,(a4)
	cmp.w	#$358,(a4)
	bmi.s	ok1
	move.w	#$358,(a4)
ok1:	move.w	(a4),6(a5)
	rts

pitchup:bsr.s	newrou
	clr.l	d0
	move.b	3(a6),d0
	and.b	#$f,d0
	sub.w	d0,(a4)
	cmp.w	#$71,(a4)
	bpl.s	ok2
	move.w	#$71,(a4)
ok2:	move.w	(a4),6(a5)
	rts

setvol:	move.b	3(a6),8(a5)
	rts

setfilt:move.b	3(a6),d0
	and.b	#1,d0
	lsl.b	#1,d0
	and.b	#$fd,$bfe001
	or.b	d0,$bfe001
	rts

setspeed:
	clr.l	d0
	move.b	3(a6),d0
	and.b	#$f,d0
	move.w	d0,speed+2
	rts

newrou:	cmp.l	#datach0,a6
	bne.s	next1
	lea	voi1(pc),a4
	rts
next1:	cmp.l	#datach1,a6
	bne.s	next2
	lea	voi2(pc),a4
	rts
next2:	cmp.l	#datach2,a6
	bne.s	next3
	lea	voi3(pc),a4
	rts
next3:	lea	voi4(pc),a4
	rts

replaystep:				;** work next pattern-step
	clr.w	timpos
	move.l	muzakoffset,a0
	move.l	a0,a3
	add.l	#12,a3			;ptr to soundprefs
	move.l	a0,a2
	add.l	#472,a2			;ptr to pattern-table
	add.l	#600,a0			;ptr to first pattern
	clr.l	d1
	move.l	trkpos,d0		;get ptr to current pattern
	move.b	(a2,d0),d1
	lsl.l	#8,d1
	lsl.l	#2,d1
	add.l	patpos,d1		;get ptr to current step
	clr.w	enbits
	lea	$dff0a0,a5		;chanel 0
	lea	datach0(pc),a6
	bsr.L	chanelhandler
	lea	$dff0b0,a5		;chanel 1
	lea	datach1(pc),a6
	bsr.L	chanelhandler
	lea	$dff0c0,a5		;chanel 2
	lea	datach2(pc),a6
	bsr.L	chanelhandler
	lea	$dff0d0,a5		;chanel 3
	lea	datach3(pc),a6
	bsr.L	chanelhandler
	move.w	#400,d0			;** wait a while and set len
rep1:	dbf	d0,rep1			;   of oneshot to 1 word
	move.w	#$8000,d0
	or.w	enbits,d0
	move.w	d0,$dff096
	cmp.w	#1,datach0+14
	bne.s	rep2
	clr.w	datach0+14
	move.w	#1,$dff0a4
rep2:	cmp.w	#1,datach1+14
	bne.s	rep3
	clr.w	datach1+14
	move.w	#1,$dff0b4
rep3:	cmp.w	#1,datach2+14
	bne.s	rep4
	clr.w	datach2+14
	move.w	#1,$dff0c4
rep4:	cmp.w	#1,datach3+14
	bne.s	rep5
	clr.w	datach3+14
	move.w	#1,$dff0d4

rep5:	add.l	#16,patpos		;next step
	cmp.l	#64*16,patpos		;pattern finished ?
	bne.s	rep6
	clr.l	patpos
	addq.l	#1,trkpos		;next pattern in table
	clr.l	d0
	move.w	numpat,d0
	cmp.l	trkpos,d0		;song finished ?
	bne.s	rep6
	clr.l	trkpos
rep6:	movem.l	(a7)+,d0-d7/a0-a6
	rts

chanelhandler:
	move.l	(a0,d1.l),(a6)		;get period & action-word
	addq.l	#4,d1			;point to next chanel
	clr.l	d2
	move.b	2(a6),d2		;get nibble for soundnumber
	lsr.b	#4,d2
	beq.s	chan2			;no soundchange !
	move.l	d2,d4			;** calc ptr to sample
	lsl.l	#2,d2
	mulu	#30,d4
	lea	pointers-4(pc),a1
	move.l	(a1,d2.l),4(a6)		;store sample-address
	move.w	(a3,d4.l),8(a6)		;store sample-len in words
	move.w	2(a3,d4.l),18(a6)	;store sample-volume

	move.l	d0,-(a7)
	move.b	2(a6),d0
	and.b	#$f,d0
	cmp.b	#$c,d0
	bne.s	ok3
	move.b	3(a6),8(a5)
	bra.s	ok4
ok3:	move.w	2(a3,d4.l),8(a5)	;change chanel-volume
ok4:	move.l	(a7)+,d0

	clr.l	d3
	move.w	4(a3,d4),d3		;** calc repeatstart
	add.l	4(a6),d3
	move.l	d3,10(a6)		;store repeatstart
	move.w	6(a3,d4),14(a6)		;store repeatlength
	cmp.w	#1,14(a6)
	beq.s	chan2			;no sustainsound !
	move.l	10(a6),4(a6)		;repstart  = sndstart
	move.w	6(a3,d4),8(a6)		;replength = sndlength
chan2:	tst.w	(a6)
	beq.s	chan4			;no new note set !
	move.w	22(a6),$dff096		;clear dma
	tst.w	14(a6)
	bne.s	chan3			;no oneshot-sample
	move.w	#1,14(a6)		;allow resume (later)
chan3:	bsr.L	newrou
	move.w	(a6),(a4)
	move.w	(a6),16(a6)		;save note for effect
	move.l	4(a6),0(a5)		;set samplestart
	move.w	8(a6),4(a5)		;set samplelength
	move.w	(a6),6(a5)		;set period
	move.w	22(a6),d0
	or.w	d0,enbits		;store dma-bit
	move.w	18(a6),20(a6)		;volume trigger
chan4:	rts

datach0:	blk.w	11,0
		dc.w	1
datach1:	blk.w	11,0
		dc.w	2
datach2:	blk.w	11,0
		dc.w	4
datach3:	blk.w	11,0
		dc.w	8
voi1:		dc.w	0
voi2:		dc.w	0
voi3:		dc.w	0
voi4:		dc.w	0
pointers:	blk.l	15,0
notetable:	dc.w	856,808,762,720,678,640,604,570
		dc.w	538,508,480,453,428,404,381,360
		dc.w	339,320,302,285,269,254,240,226  
		dc.w	214,202,190,180,170,160,151,143
		dc.w	135,127,120,113,000
muzakoffset:	dc.l	0
trkpos:		dc.l	0
patpos:		dc.l	0
numpat:		dc.w	0
enbits:		dc.w	0
timpos:		dc.w	0

data:		dc.l 0
blk.b	36000,0
end:
