org $38000
load $38000

>Extern "sinus",sinus,$5a0
>EXTERN "MS",$48000,1280

screen         = $40000
second_screen  = $42800

sinus          = $45000
ytab           = sinus+$5a0
desti          = ytab+$200



code:	lea	$dff000,a6
	move.w	#$7fff,$9a(a6)
	move.l	$6c.w,oldirq+2
	move.l	#myirq,$6c.w
	move.w	#$7fff,$96(a6)
	move.w	#$87e0,$96(a6)
bbusy1:	btst	#$0e,$02(a6)
	bne.s	bbusy1
	clr.l	$0.w

	move.l	#$70000,a0
	move.w	#$1dff,d0
clear:	move.l	#0,(a0)+
	dbf	d0,clear	
	lea	ytab,a0
	moveq	#$00,d0
	move.w	#$ff,d1
loop6:	move.w	d0,(a0)+
	add.w	#$28,d0
	dbf	d1,loop6

	lea	screen,a0
clear4:	clr.l	(a0)+
	cmp.l	#sinus,a0
	bne.s	clear4
	move.l	Text1pos,Charpos
	move.l	#Text1,c_ptr
	move.l	#40,d4
	move.l	#22,d3
text2print:
	bsr	printchar
	add.l	#1,Charpos
	dbf	d3,text2print
	move.l	Text2pos,Charpos
	move.l	#Text2,c_ptr
	move.l	#40,d4
	move.l	#20,d3		;Länge des Namens
text3print:
	bsr	printchar
	add.l	#1,Charpos
	dbf	d3,text3print

	move.l	#str,c_ptr
	move.l	Scrollpos,Charpos

	move.w	#$c020,$9a(a6)
	move.l	#copper,$80(a6)

mouse:	btst	#$06,$bfe001
	bne.s	mouse

bbusy2:	btst	#$0e,$02(a6)
	bne.s	bbusy2
	move.w	#$7fff,$96(a6)
	move.w	#$83f0,$96(a6)
	move.w	#$7fff,$9a(a6)
oldirq:	move.l	#$00,$6c.w
	move.w	#$e02c,$9a(a6)

	move.l	4.w,a6
	lea	gfxname(pc),a1
	jsr	-408(a6)
	move.l	d0,a1
	move.l	38(a1),$dff080
	jmp	-414(a6)

gfxname:dc.b	'graphics.library',0,0

dreh:	move.l	tri(pc),a0
	lea	desti,a1
	lea	object(pc),a2
	move.w	(a0)+,d7
	subq.w	#$01,d7
nextpoint:
	movem.w	(a0)+,d0-d2	; X,Y und Z coords holen
	movem.w	(a2),d5-d6	; Sinus und Cos nach D5 und D6
	move.w	d1,d3		; Y nach D3
	move.w	d2,d4		; Z nach D4
	muls	d5,d2		; Z=Sin*Z
	muls	d6,d1		; Y=Cos*Y
	add.l	d2,d1		; Y=Y+Z
	lsr.l	#$02,d1		; Y=Y/4
	muls	d5,d3		; Y=Sin*Y
	muls	d6,d4		; Z=Cos*Z
	sub.l	d3,d4		; Z=Z-Y
	move.l	d4,d2		; Z nach D2 retten
	lsr.l	#$02,d2		; Z=Z/4
	movem.w	4(a2),d5-d6	; Sinus und Cos nach D5 und D6
	move.w	d0,d3		; X nach D3
	move.w	d2,d4		; Z nach D4
	muls	d5,d2		; Z=Sin*Z
	asr.l	#$06,d2		; Z=Z/64
	muls	d6,d0		; X=Cos*X
	add.l	d2,d0		; X=X+Z
	lsr.l	#$02,d0		; X=X/4
	muls	d5,d3		; X=X*Sin
	muls	d6,d4		; Z=Z*Cos
	asr.l	#$06,d4		; Z=Z/64
	sub.l	d3,d4		; Z=Z-X
	move.l	d4,d2		; Z nach D2
	lsr.l	#$08,d2		; Z=Z/256
	movem.w	8(a2),d5-d6	; Sinus und Cos nach D5 und D6
	move.w	d0,d3		; X in D3
	move.w	d1,d4		; Y in D4
	muls	d5,d1		; Y=Y*Sin
	muls	d6,d0		; X=X*Cos
	sub.l	d1,d0		; X=X-Y
	asr.l	#$06,d0		; X=X/64
	muls	d5,d3		; X=X*Sin
	muls	d6,d4		; Y=Y*Cos
	add.l	d3,d4		; Y=Y+X
	move.l	d4,d1		; Y nach D1
	asr.l	#$06,d1		; Y=Y/64

	add.w	#300,d2		; ZOOM-FAKTOR: 300
	divs	d2,d0		; X=X/Z
	divs	d2,d1		; Y=Y/Z
zero:	add.w	#160,d0		; X=X+160 (zentrieren)	
	add.w	#80,d1		; Y=Y+128 (-----"----)
	movem.w	d0-d1,(a1)	; Coords abspeichern
	addq.w	#$04,a1		; Zielpointer erhoehen
	dbf	d7,nextpoint	; Alle punkte ???
	rts

calcsinus:
	lea	winkel(pc),a0
	lea	sinus,a1
	lea	object(pc),a2
	moveq	#$02,d1
retest:	move.w	(a0),d0		; Aktuellen Winkel in D0
	bpl.s	drub		; >0 ???
	add.w	#360,(a0)	; Nein, 360 addieren
	bra.s	retest
drub:	cmp.w	#360,d0
	blt.s	kleiner		; <360 ???
	sub.w	#360,(a0)	; Nein, 360 subtrahieren
	bra.s	retest
kleiner:addq.w	#$02,a0		; Naechsten Winkel
	add.w	d0,d0		; Winkel *2
	add.w	d0,d0		; Winkel *2
	move.l	(a1,d0.w),(a2)+	; Sinus und Cos aus Tab holen
	dbf	d1,retest	; Alle 3 Winkel ???
	movem.w	(a0)+,d0-d2	; ???
	movem.w	d0-d2,(a2)	; ???
	rts

object:	dc.l	$0,$0,$0
	dc.w	$0,$0,$0

tri:	dc.l	supplex
lines:	dc.l	supplexl

supplex:
	dc.w	43		; num of coords x,y,z	
	dc.w	-70,-25,0
	dc.w	-90,-25,0
	dc.w	-95,-20,0
	dc.w	-95,-5,0
	dc.w	-90,0,0
	dc.w	-75,0,0
	dc.w	-70,5,0
	dc.w	-70,20,0
	dc.w	-75,25,0
	dc.w	-95,25,0

	dc.w	-65,-25,0
	dc.w	-65,20,0
	dc.w	-60,25,0
	dc.w	-45,25,0
	dc.w	-40,20,0
	dc.w	-40,-25,0

	dc.w	-35,-25,0
	dc.w	-15,-25,0
	dc.w	-10,-20,0
	dc.w	-10,-5,0
	dc.w	-15,0,0
	dc.w	-35,0,0
	dc.w	-35,25,0

	dc.w	-5,-25,0
	dc.w	15,-25,0
	dc.w	20,-20,0
	dc.w	20,-5,0
	dc.w	15,0,0
	dc.w	-5,0,0
	dc.w	-5,25,0

	dc.w	25,-25,0
	dc.w	25,25,0
	dc.w	35,25,0

	dc.w	40,-25,0
	dc.w	65,-25,0
	dc.w	40,0,0
	dc.w	55,0,0
	dc.w	40,25,0
	dc.w	65,25,0

	dc.w	70,-25,0
	dc.w	95,-25,0
	dc.w	70,25,0
	dc.w	95,25,0

supplexl:
	dc.w	34		;num of lines
	dc.w	0,1
	dc.w	1,2
	dc.w	2,3
	dc.w	3,4
	dc.w	4,5
	dc.w	5,6
	dc.w	6,7
	dc.w	7,8
	dc.w	8,9

	dc.w	10,11
	dc.w	11,12
	dc.w	12,13
	dc.w	13,14
	dc.w	14,15

	dc.w	22,16
	dc.w	16,17
	dc.w	17,18
	dc.w	18,19
	dc.w	19,20
	dc.w	20,21

	dc.w	29,23
	dc.w	23,24
	dc.w	24,25
	dc.w	25,26
	dc.w	26,27
	dc.w	27,28

	dc.w	30,31
	dc.w	31,32

	dc.w	34,33
	dc.w	33,37
	dc.w	37,38
	dc.w	35,36

	dc.w	39,42
	dc.w	40,41

winkel:	dc.w	0
winkel2:dc.w	0
winkel3:dc.w	0

clearscreen:
	btst	#$0e,$02(a6)		;warten bis blitter fertig
	bne.s	clearscreen

	move.l	#$01000000,$40(a6)	; fill screen with 0-bytes
	clr.w	$66(a6)			; via blitter
	move.l	cur1(pc),$54(a6)
	move.w	#[192*64]+20,$58(a6)

	addq.w	#$03,winkel		; bewegung der grafik
	addq.w	#$02,winkel2		; in X,Y,Z - Achsen
	rts

liner2:	move.l	lines,a0
	lea	desti,a1
	move.w	(a0)+,d7
	subq.w	#$01,d7
lineloop:
	moveq	#$00,d4
	movem.w	(a0)+,d5-d6
	lsl.w	#$02,d5
	lsl.w	#$02,d6
	move.w	(a1,d5.w),d0
	move.w	(a1,d6.w),d2
	move.w	2(a1,d5.w),d1
	move.w	2(a1,d6.w),d3
	bsr.s	draw
	dbf	d7,lineloop
	rts

; Aus dem AMIGA-INTERN S.253,254

draw:	movem.l	d0-d7/a0-a1,-(a7)
	lea	ytab,a1

mist:	btst	#$0e,$02(a6)
	bne.s	mist

muster:	move.l	#$ffff8000,$72(a6)	; Muster und Index
	move.w	#$ffff,$44(a6)		; Verknuepfung
	move.w	#40,$60(a6)		; Breite

	cmp.w	d0,d2			; Coords alle gleich ???
	bne.s	dl_1
	cmp.w	d1,d3
	beq.s	dl_end			; Wenn ja, ende !
dl_1:	move.l	cur1+4(pc),a0		; Adresse des Screens in A0
	move.w	d1,d4			; Y1 in D4

	lsl.w	#$01,d4
	move.w	(a1,d4.w),d4
	move.w	d0,d5
	lsr.w	#$03,d5			; X1/8
	and.w	#$fffe,d5		; Bit 0 killen
	add.w	d5,d4
	add.l	a0,d4			; Startadresse nun in D4
	moveq	#$00,d5
	sub.w	d1,d3			; Y2=Y2-Y1
	roxl.b	#$01,d5			; C-bit in D5
	tst.w	d3			; Y2 positiv ?
	bge.s	y2gy1			; Ja, dann sprung (y2gy1)
	neg.w	d3			; Minus in plus
y2gy1:	sub.w	d0,d2			; X2=X2-X1
	roxl.b	#$01,d5			; C-bit in D5
	tst.w	d2			; X2 positiv ?
	bge.s	x2gx1			; Ja, dann sprung (x2gx1)
	neg.w	d2			; Minus in plus
x2gx1:	move.w	d3,d1			; Y2 in Y1
	sub.w	d2,d1			; Y1=Y1-X2
	bge.s	dygdx			; Ergebnis positiv
	exg	d2,d3			; Nein, X2 und Y2 vertauschen
dygdx:	roxl.b	#$01,d5			; C-bit in D5
	move.b	oct(pc,d5.w),d5		; Octante in D5
	add.w	d2,d2			; X2*2
	move.w	d2,$62(a6)		; Neigung (4Y-4X) (Y/X)
	sub.w	d3,d2			; X2=X2-Y2
	bge.s	signnl			; Ergebnis positiv
	or.b	#$40,d5			; Bit 6 in D5 setzen
signnl:	move.w	d2,$52(a6)		; Steigung (2Y-X) (Y/X)
	sub.w	d3,d2			; X2=X2-Y2
	move.w	d2,$64(a6)		; Hoehe (4Y)
	and.w	#$000f,d0
	ror.w	#$04,d0
	or.w	#$0bca,d0
	move.w	d0,$40(a6)		; Offset und Minterm
	move.w	d5,$42(a6)		; Offset+Octanten
	move.l	d4,$48(a6)		; Startadresse
	move.l	d4,$54(a6)		; Startadresse
	lsl.w	#$06,d3			; Laenge *64
	addq.w	#$02,d3			; 1 Bit setzen
	move.w	d3,$58(a6)		; Linie zeichnen
dl_end:	movem.l	(a7)+,d0-d7/a0-a1
	rts

oct:	dc.b	$01,$11,$09,$15,$05,$19,$0d,$1d

myirq:	movem.l	d0-d7/a0-a6,-(a7)
	move.w	$1e(a6),d0
	move.w	d0,$9c(a6)
	btst	#$05,d0
	bne.s	copint
endirq:	movem.l	(a7)+,d0-d7/a0-a6
	rte
	
copint:
	movem.l	d0-d7/a0-a6,-(a7)
	bsr	scrollroutine
	movem.l	(a7)+,d0-d7/a0-a6
	bsr	clearscreen
	bsr	screenswap
	bsr	calcsinus
	bsr	dreh
	bsr	liner2
	bra.s	endirq


cur1:	dc.l	screen
	dc.l	second_screen

screenswap:
	lea	cur1(pc),a0
	movem.l	(a0)+,d0-d1
	exg	d0,d1
	movem.l	d0-d1,-(a0)

	lea	b1(pc),a0
	move.w	d1,6(a0)
	swap	d1
	move.w	d1,2(a0)
	rts

ScrollRoutine:
	tst.l	scp
	bne	s3
	bsr	pixelscrollen
s3:
	move.l	#rows,a0
s1:	cmp.l	#3,(a0)
	beq	print
	add.l	#1,(a0)
end:	rts

pixelscrollen:   
	move.w	#00,$dff042
	move.w	#00,$dff044
	move.w	#00,$dff064
	move.w	#00,$dff066
	move.l	#$70002+[60*210]-4,$dff050
	move.l	#$70000+[60*210]-4,$dff054
	bsr	blittercopy
	move.l	#$70002+[60*210]+$3c00-4,$dff050
	move.l	#$70000+[60*210]+$3c00-4,$dff054
	bsr	blittercopy
	bra	end
blittercopy:
s2:	move.w	#$e9f0,$dff040
	move.w	#$0c5e,$dff058
blitterbusy:
	btst	#14,$dff002
	bne	blitterbusy
	rts
stopscroll:
	move.l	#-1,scp
	move.l	#s1,a0
	move.l	#31,2(a0)
	bsr	pc2
	bra	printchar
speed2:
	move.l	#0,scp
	move.l	#s1,a0
	move.l	#s2,a1
	move.l	#3,2(a0)
	move.w	#$e9f0,2(a1)
	bsr	pc2
	bra	printchar
print:
	move.l	#$00,rows
printchar:			
	clr.l	d0
	clr.l	d5
	clr.l	d1
	move.l	c_ptr,a4	
	move.l	#CharTable,a1
	cmp.b	#$61,(a4)
	beq	stopscroll
	cmp.b	#$62,(a4)
	beq	speed2
	cmp.b	#$ff,(a4)
	beq	setnew
Charb:	move.b	(a1),d1	
	cmp.b	(a4),d1
	beq	printit
	add.l	#$1,d5
	cmp.l	#CharTableEnde-1,a1
	add.l	#$1,a1
	bne	Charb
	move.l	#48,d5
PrintIT:		
	move.l	#0,d0
	move.w	d5,d0	
	divu	#40,d0	
	swap	d0
	move.w	d0,d1
	swap	d0
	mulu	#320,d0	
	add.l	d1,d0
	add.l	#$48000,d0
	move.l	d0,d2
	move.l	charpos,a2
	bsr	copychar

	move.l  d2,d0
	add.l	#640,d0
	move.l	charpos,a2
	add.l	#$3c00,a2
	bsr	copychar

pc2:	add.l	#1,c_ptr
	rts
Setnew:
	move.l	#str,c_ptr
 	bra	printchar
CopyChar:
	move.l	d0,a1		
	clr.l	d0
	move.l	#7,d0
cC2:	move.b	(a1),(a2)
	add.l	#40,a1
	add.l	#60,a2
	dbf	d0,cC2
	rts
Charpos:	dc.l	0


CharTable:
dc.b	"ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890:?-'!)(<>+., "
CharTableEnde: equ	*
even

; Scroll text
;	STOP SCROLLTEXT:	'a'
;	RUN SCROLLTEXT :	'b'
;e.g:
;	"HELLOa     b AND"
;	NOW IT STOP FOR THE TIME OF 20(4*5) CHARS

str:
DC.B    "  SUPPLEX                          a "  
DC.B    "b PRESENT A NEW RELEASE CALLED:                           "
DC.B    "    SUPERLEAGUE SOCCER                    a " 
DC.B    "b            CALL OUR BBS IN ITALY: 0039 40 57 54 24 "
DC.B    "RUNS FROM 24 HOUR TILL 8 IN MORNING         "
DC.B	"  INTRO CODED BY ASCOT            "
DC.B	"SEE YOU IN OUR NEXT PRODUCTIONS             "
DC.B    "   GREETINGS GOES TO ONLY OUR GOOD CONTACTS!!!            "
DC.B	"                         "
dc.b	$ff	;SCROLLENDMARKIERUNG!
even

c_ptr:		blk.l	1,0	;Adresse auf aktuelles Scrollzeichen
rows:		blk.l	1,0	;Hilfszähler für den Scroll
scp:		blk.l	1,0
OldCopper:	blk.l	2,0	;Adressen der Original-Copperliste
				;fuer Kick V1.2 und V1.3
Text1:	DC.B	"IS PROUD TO PRESENT YOU"
EVEN
Text2:	DC.B	"  SUPERLEAGUE SOCCER "
EVEN

ScrollPos:	dc.l	$70000+[60*220]-6
Text1pos:	dc.l	$72641
Text2pos:	dc.l	$72a7a
endprg:

copper:	dc.w	$0100,$1200
	dc.w	$008e,$2c00
	dc.w	$0090,$2cff
	dc.w	$0092,$0038
	dc.w	$0094,$00d0
	dc.w	$0102,$0000
	dc.w	$0104,$0004
	dc.w	$0108,$0000
	dc.w	$010a,$0000
	dc.w	$0180,$0000
	dc.w	$0182,$00f8
b1:	dc.w	$00e0,screen/$10000
	dc.w	$00e2,screen
	dc.w	$0120,$0000
	dc.w	$0122,$0000
	dc.w	$0124,$0000
	dc.w	$0126,$0000
	dc.w	$0128,$0000
	dc.w	$012a,$0000
	dc.w	$012c,$0000
	dc.w	$012e,$0000
	dc.w	$0130,$0000
	dc.w	$0132,$0000
	dc.w	$0134,$0000
	dc.w	$0136,$0000
	dc.w	$0138,$0000
	dc.w	$013a,$0000
	dc.w	$013c,$0000
	dc.w	$013e,$0000
	dc.w	$2d0f,$fffe
	dc.w	$0180,$0722
	dc.w	$2e0f,$fffe
	dc.w	$0180,$0000

	dc.w	$c001,$fffe
	DC.W	$0092,$0028,$0094,$00e0
	DC.W	$0180,$077e,$0182,$0fef
	DC.W	$0184,$0a00,$0186,$00f0
	DC.W	$0108,$000e,$010a,$000e
	dc.w	$00e0,$0007,$00e2,$0000+$2328
	dc.w	$00e4,$0007,$00e6,$3c00+$232c
	dc.w	$0100,$2200
	dc.w	$c10f,$fffe
	dc.w	$0180,$0000

	dc.w	$f80f,$fffe
	dc.w	$0180,$0001
	dc.w	$fa0f,$fffe
	dc.w	$0180,$0002
	dc.w	$fe0f,$fffe
	dc.w	$0180,$0003

	dc.w	$ffe1,$fffe
	dc.w	$000f,$fffe
	dc.w	$0180,$0004
	dc.w	$030f,$fffe
	dc.w	$0180,$0005
	dc.w	$050f,$fffe
	dc.w	$0180,$0006

	dc.w	$0c0f,$fffe
	dc.w	$0180,$0006,$0184,$0600,$0182,$0888
	dc.w    $0108,-106
	dc.w    $010a,-106
	dc.w	$1f0f,$fffe
	dc.w	$0180,$0005
	dc.w	$210f,$fffe
	dc.w	$0180,$0004
	dc.w	$230f,$fffe
	dc.w	$0180,$0003
	dc.w	$250f,$fffe
	dc.w	$0180,$0002
	dc.w	$270f,$fffe
	dc.w	$0180,$0001

	dc.w	$290f,$fffe
	dc.w    $0100,$0200
	DC.W	$FFFF,$FFFE	;ENDE der Copperliste
$fe0f,$fffe
	dc.w	$0180,$0003

	dc.w	$ffe1,$fffe
	dc.w	$000f,$fffe
	dc.w	$0180,$0004
	dc.w	$030f,$fffe
	dc.w	$0180,$0005
	dc.w	$050f,$fffe
	dc.w	$0180,$0006

	dc.w	$0c0f,$fffe
	dc.w	$0180,$0006,$0184,$0600,$0182,$0888
	dc.w    $0108,-106
	dc.w    $010a,-106
	dc.w	$1f0f,$fffe
	dc.w	$0180,$0005
	dc.w	$210f,$fffe
	dc.w	$0180,$0004
	dc.w	$230f,$fffe
	dc.w	$0180,$0003
	dc.w	$250f,$fffe
	dc.w	$0180,$0002
	dc.w	$270f,$fffe
	dc.w	$0180,$0001

	dc.w	$290f,$fffe
	dc.w    $0100,$0200
	DC.W	$FFFF,$FFFE	;ENDE der Coppe