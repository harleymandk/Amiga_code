שתשת                                        ;Intro coded by Corto in 1990 

;A,  JRUN pour tester...  as usual
;pour sauvegarder: WI, filename,  run,  end
;texte vers ligne 110
;l*** pour les commentaires

;ceci est la version amelioree de 'Trainer1.s' mais sans le trainer

;org $40000
;load $40000

run:
movem.L d0-d7/a0-a6,-(sp)
lea $50000,a0
move #24575,d7
run1:
move.b #$0,(a0)+
dbra d7,run1
lea $56000,a1
move #37887,d7
run2:clr.l (a1)+
dbra d7,run2
move sin+$200,sin200+2
move cos+$200,cos200+2
move sin+$200,sin200b+2
move cos+$200,cos200b+2
move sin+$600,sin600+2
move cos+$600,cos600+2
move sin+$600,sin600b+2
move cos+$600,cos600b+2
bsr.L save_all
bsr.L makecopper
lea $dff000,a6
move #$3fff,$9a(a6)
move #$c010,$9a(a6)
move #$3fff,$96(a6)
move #$83c0,$96(a6)
move.l #irq,$6c
move.L #%111101111011100111101111,$50000+38
move.L #%100001001010100011001001,$50000+44+38
move.L #%100001001011100011001001,$50000+88+38
move.l #%100001001010010011001001,$50000+132+38
move.l #%111101111010010011001111,$50000+176+38
move.l	#$56000+[48*80]+2,a2 ;70=No de ligne
bsr.L makmask

affiche:
lea	fonts,a1
clr.w	d0
lettre:
lea	text1(pc),a0
lea	quelle(pc),a3
add.w	(a3),a0
move.b	(a0),d0
addq.w	#1,(a3)
nextline:
cmpi.b	#$fe,d0
bne.s	dernier

;add.l	#[48*8]-40,a2	;pour texte normal
addi.l #[48*13]-40,a2	;pour texte double

bra.s	lettre
dernier:
tst.b	d0
beq.L	suite
espace:
cmpi.b	#$20,d0
bne.s	noespace
add.w	#96,a1
bra.s	affichelettre
noespace:
sub.b	#$2b,d0
add.b	d0,d0
add.w	d0,a1
affichelettre:
bsr.l	affichefont
addq.l	#1,a2
bra.s	affiche

affichefont:	;pour text normal: enlever les 6 ';' suivants
move.b	(a1),(a2)
;move.b	[$62*1](a1),[48*1](a2)
;move.b	[$62*2](a1),[48*2](a2)
;move.b	[$62*3](a1),[48*3](a2)
;move.b	[$62*4](a1),[48*4](a2)
;move.b	[$62*5](a1),[48*5](a2)
;rts
		;puis mettre ';' devant chaque ligne
	move.B  (a1),48(a2)
	move.b	[$62*1](a1),[48*2](a2)
	move.b	[$62*1](a1),[48*3](a2)
	move.b	[$62*2](a1),[48*4](a2)
	move.b	[$62*2](a1),[48*5](a2)

	move.b	[$62*3](a1),[48*6](a2)
	move.b	[$62*3](a1),[48*7](a2)
	move.b	[$62*4](a1),[48*8](a2)
	move.b	[$62*4](a1),[48*9](a2)
	move.b	[$62*5](a1),[48*10](a2)
	move.b	[$62*5](a1),[48*11](a2)
rts
		;jusqu'ici !


;*** ici le texte...
;44 carac. par ligne - ne pas descendre trop bas pour le degrade


text1:
dc.b "                PRESENT                 ",$FE
DC.B "                                        ",$FE
dc.b "         XXXXXXXX FINAL VERSION         ",$FE
dc.b "                                        ",$fe
DC.B "                                        ",$FE
dc.b "                                        ",$fe
dc.b "  PRESS LEFT MOUSEBUTTON TO START GAME  ",0
even
quelle:dc.w 0
text:
dc.B "    XXXXXXXX FINAL VERSION ..."
DC.B " CRACKED BY  --- A N G E L S ---"
DC.B "  GREETZ TO ALL OUR "
DC.B "FRIENDS AND CONTACTS ...   INTRO CODED BY CORTO. "
DC.B "                         ",0
even


rout1:move d0,6(a0)
swap d0
move d0,2(a0)
addq.L #8,a0
rts

suite:
lea copspr,a0
move.L #set1,d0
bsr.s rout1
move.l #set2,d0
bsr.s rout1
move.l #set3,d0
bsr.s rout1
move.l #set4,d0
bsr.s rout1
move.l #set5,d0
bsr.s rout1
move.l #set6,d0
bsr.s rout1
move.l #setf,d0
bsr.s rout1
move.l #setf,d0
bsr.s rout1

jsr init_music
lea $dff000,a6
move.L #$3c791cc9,$8e(a6)
move.l #$3000d8,$92(a6)
move.L #copperlist,$80(a6)
clr $88(a6)
wait:
tst flag1
bne.s wait1
;bsr jongle
bsr.L bras
move #1,flag1
wait1:
btst #6,$bfe001
bne.s wait
wb:btst #$e,2(a6)
bne.s wb
bsr.L restore_all
jsr end_music
movem.L (sp)+,d0-d7/a0-a6
rts
flag:dc.w -1
flag1:dc.w 0
juga:dc.w 0
jugw:dc.w 50

makmask:
lea bob1,a0
lea mask1,a1
bsr.s masque1
lea bob2,a0
lea mask2,a1
bsr.s masque1
lea bob8,a0
lea mask8,a1
bsr.s masque1
move #4,d6
lea bob3,a0
lea mask3,a1
masl1:
bsr.s masque2
dbra d6,masl1

lea bob9,a0
lea mask9,a1
bsr.s masque2
rts

masque1:
move #31,d7
masq1:
move.L (a0),d0
addq.L #6,a0
or.l (a0),d0
addq.l #6,a0
or.l (a0),d0
addq.L #6,a0
move.l d0,(a1)+
clr (a1)+
move.l d0,(a1)+
clr (a1)+
move.l d0,(a1)+
clr (a1)+
dbra d7,masq1
rts

masque2:
move #15,d7
masq2:
move (a0),d0
addq.L #4,a0
or.w (a0),d0
addq.l #4,a0
or.w (a0),d0
addq.L #4,a0
move d0,(a1)+
clr (a1)+
move d0,(a1)+
clr (a1)+
move d0,(a1)+
clr (a1)+
dbra d7,masq2
rts

irq:
movem.L d0-d7/a0-a6,-(a7)
lea $dff000,a6
bsr.L altern
bsr.L cycle
bsr.L stars
jsr play
lea $dff000,a6
move #$8400,$96(a6)
cyc:btst #$e,2(a6)
bne.s cyc
move #$400,$96(a6)
tst flag
bne.s irq1
bsr.L clear
bsr.L copscroll
bsr.L print
move #$8400,$96(a6)
cyc2:btst #$e,2(a6)
bne.s cyc2
move #$400,$96(a6)
move #1,flag
irq4:move #$10,$9c(a6)
movem.L (a7)+,d0-d7/a0-a6
rte

irq1:
tst flag1
beq.s irq4
move #$8400,$96(a6)
cyc4:btst #$e,2(a6)
bne.s cyc4
move #$400,$96(a6)
bsr.L bob
;move #$f00,$180(a6)
move #$8400,$96(a6)
cyc3:btst #$e,2(a6)
bne.s cyc3
move #$400,$96(a6)
;move #$ff0,$180(a6)
move #$10,$9c(a6)
clr flag
clr flag1
movem.L (a7)+,d0-d7/a0-a6
rte
t1:dc.l 0
t2:dc.l 0
altern:
tst flag
bne.s alt1
move.L bpw1,d0
move.L bpa1,bpw1
move.l d0,bpa1
move d0,copbpl3+6
swap d0
move d0,copbpl3+2
swap d0
addi.L #64,d0
move d0,copbpl3+14
swap d0
move d0,copbpl3+10
swap d0
addi.L #64,d0
move d0,copbpl3+22
swap d0
move d0,copbpl3+18
alt1:rts

stars:
lea set5+1,a0
move #15,d7
spriteloop:
addq.B #5,(a0)
add.l #8,a0
dbra d7,spriteloop
lea set6+1,a0
move #15,d7
spriteloopa:
addq.B #6,(a0)
add.l #8,a0
dbra d7,spriteloopa
lea set3+1,a0
move #21,d7
spriteloop2:
addq.b #3,(a0)
add.l #8,a0
dbra d7,spriteloop2
lea set4+1,a0
move #21,d7
sprite2loopa:
addq.B #4,(a0)
add.L #8,a0
dbra d7,sprite2loopa
lea set1+1,a0
move #31,d7
spriteloop3:
addq.b #1,(a0)
add.l #8,a0
dbra d7,spriteloop3
lea set2+1,a0
move #31,d7
spriteloop3a:
addq.b #2,(a0)
add.l #8,a0
dbra d7,spriteloop3a
rts

textptr:dc.l text

cycle:
move #$8400,$96(a6)
cyc1:btst #6,2(a6)
bne.s cyc1
move #$400,$96(a6)
move.l #$e9f00000,$40(a6)
move.l #-1,$44(a6)
move.L #$56000+[210*48],$50(a6)
move.l #$56000+[210*48]-2,$54(a6)
move.l #$00000,$64(a6)
move #344,$58(a6)
rts

print:
tst pra
bne.s printa
lea $56000+[210*48]+44,a2
move.L textptr,a0
clr d0
move.b (a0)+,d0
tst.b (a0)
bne.s print1
lea text,a0
print1:move.l a0,textptr

lea	fonts,a1
espace1:
cmpi.b	#$20,d0
bne.s	noespace1
add.w	#96,a1
bra.s	affichefont1
noespace1:
sub.b	#$2b,d0
add.b	d0,d0
add.w	d0,a1
affichelettre1:
bra.l	affichefont1

affichefont1:
move.b	(a1),(a2)
move.b	[$62*1](a1),[48*1](a2)
move.b	[$62*2](a1),[48*2](a2)
move.b	[$62*3](a1),[48*3](a2)
move.b	[$62*4](a1),[48*4](a2)
move.b	[$62*5](a1),[48*5](a2)
printa:bchg #0,pra
rts

pra:dc.w 0


clear:move #$34,$66(a6)
move.l bpw1,d0
addi.L #[10*3*64]+16,d0
move.l d0,$54(a6)
move.L #$1000000,$40(a6)
move #38400+6,$58(a6)
rts

spball:dc.w 0
balls:dc.w 0

moveball:
lea boules+2,a0
tst balls
bne.s mb1
add #12,spball
move spball,d0
sub d0,(a0)
cmpi #-1400,(a0)
blo.s mb2
rts
mb2:move #-1,balls
add d0,(a0)
rts
mb1:
sub #12,spball
move spball,d0
add d0,(a0)
tst spball
beq.s mb3
rts
mb3:clr balls
move #$200,bra1
rts

bras:	;lea sin cos deja fait
bsr.s moveball
lea sin,a0
lea cos,a1
lea hand+2,a2
move bra1,d6
addi #72,d6
andi #$7fe,d6
move d6,bra1
move.L #38*8,d0
muls (a1,d6.w),d0
asr.L #5,d0
asr.l #8,d0
addi #12,d0
;move d0,(a2)
move d0,18(a2)
addq.l #6,a2

move.L #75*8,d0
muls (a1,d6.w),d0
asr.L #5,d0
asr.l #8,d0
subi #25,d0
;move d0,(a2)
move d0,18(a2)
addq.L #6,a2

move.L #100*8,d0
muls (a1,d6.w),d0
asr.L #5,d0
asr.l #8,d0
subi #50,d0
;move d0,(a2)
move d0,18(a2)

;*** speed contient la vitesse de rotation (paire) 
speed=16

calcule:
addi #speed,z+2
andi #$7fe,z+2
;lea sin,a0	;deja fait
;lea cos,a1
lea jug,a2
lea coord,a3
lea val,a4
sub.l a5,a5
movem.L vide,d0-d7
calculoop:
movem.w (a2)+,d0/d1/d2
move d0,d5
move d1,d6
sin200:muls #2,d0	
cos200:muls #2,d1
sub.l d1,d0
cos200b:muls #2,d5
move d6,d1
sin200b:muls #2,d1
add.l d5,d1
asr.l #8,d0
asr.l #2,d0
asr.l #8,d1
asr.l #2,d1
y:move #$600,d3
move d1,d5
move d2,d6
sin600:muls #2,d1
cos600:muls #2,d2
sub.l d2,d1
cos600b:muls #2,d5
move d6,d2
sin600b:muls #2,d2
add.l d5,d2
asr.l #2,d1
asr.l #8,d2
asr.l #2,d2
z:move #$200,d3
move d2,d5
move d0,d6
muls (a0,d3),d2
muls (a1,d3),d0
sub.l d0,d2
muls (a1,d3),d5
move d6,d0
muls (a0,d3),d0
add.l d5,d0
asr.l #2,d0
asr.l #8,d2
asr.l #6,d2

add #300,d2
divs d2,d0
divs d2,d1
move d2,(a4)+
cmpi #3,a5
blo.s cal0
addi #[41*64]+16,d0
addi #[111*16],d1
bra.s cal1
cal0:addi #[40*64]-48,d0
addi #[111*16],d1
cal1:
asr #4,d0
asr #4,d1
move d0,d2
lsr #3,d0
bclr #0,d0
and #$f,d2
lsl #6,d1
;muls #3,d1
move d1,d6
add d6,d1
add d6,d1
add d1,d0
swap d0
clr d0
swap d0
;add.l bpw1,d0
move.l d0,(a3)+
ror #4,d2
move d2,(a3)+
or #$fca,d2
move d2,(a3)+
add #1,a5
cmpi #$7777,(a2)
bne.L calculoop
move #$7777,(a3)
move #$7777,(a4)

lea ordre,a0
lea val,a1
clr.L d0
loop1:
move.L d0,d1
add.l #2,d1
loop2:
move (a0,d0.w),d2
move (a0,d1.w),d3
lsl #1,d2
lsl #1,d3
move (a1,d2.w),d4
move (a1,d3.w),d5
cmp d5,d4
bge.s noexg
lsr #1,d2
lsr #1,d3
move d2,(a0,d1.w)
move d3,(a0,d0.w)
noexg:
addq.L #2,d1
cmpi #$7777,(a0,d1.w)
bne.s loop2
add.l #2,d0
cmpi #$7777,2(a0,d0.w)
bne.s loop1
clr.l d0
rts

bob:
lea ordre,a1
lea bo,a4
lea coord,a3
move.l #$ffff0000,$44(a6)
clr.L $62(a6)
move #$8400,$96(a6)
move.l bpw1,d0
bobl1:
btst #$e,2(a6)
bne.s bobl1
move (a1)+,d2
lsl #3,d2
;clr.L d1
move.l (a3,d2.w),d1
add.L d0,d1
;move 4(a3,d2.w),d5
;lsl #1,d2
move.l (a4,d2.w),d4
move.l d4,$4c(a6)
move.L 4(a4,d2.w),$50(a6)
move.l d1,$48(a6)
move.L d1,$54(a6)
cmpi.L #bob1,d4
beq.s bobb2
cmpi.L #bob2,d4
beq.s bobb2
cmpi.l #bob8,d4
beq.s bobb2
move #$3c,$60(a6)
move #$3c,$66(a6)
;move d5,$42(a6)
;or #$fca,d5
;move d5,$40(a6)
move 4(a3,d2.w),$42(a6)
move 6(a3,d2.w),$40(a6)
move #3074,$58(a6)
cmpi #$7777,(a1)
bne.s bobl1
rts

bobb2:
move #$3a,$60(a6)
move #$3a,$66(a6)
;move d5,$42(a6)
;or #$fca,d5
;move d5,$40(a6)
move 4(a3,d2.w),$42(a6)
move 6(a3,d2.w),$40(a6)
move #6147,$58(a6)
cmpi #$7777,(a1)
bne.L bobl1
rts


copscroll:
lea cop1+6,a0
move #94,d7
move (a0),d0
cops1:
move 12(a0),(a0)
move 12(a0),4(a0)
add.L #12,a0
dbra d7,cops1
move d0,(a0)
move d0,4(a0)
rts

save_all:
move.L 4,a6
jsr -132(a6)
move.b #%10000111,$bfd100
move.l $6c,save_vecteur_irq
move.w $dff01c,save_intena
or.w #$c000,save_intena
move.w $dff002,save_dmacon
or.w #$8100,save_dmacon
rts
restore_all:
move.l save_vecteur_irq,$6c
move.w #$7fff,$dff09a
move.w save_intena,$dff09a
move.w #$7fff,$dff096
move.w save_dmacon,$dff096
move.l 4,a6
lea name_glib,a1
moveq #0,d0
jsr -552(a6)
move.l d0,a0
move.l 38(a0),$dff080
clr.w $dff088
move.L 4,a6
jsr -138(a6)
rts
save_intena:dc.w 0
save_dmacon:dc.w 0
save_vecteur_irq:dc.l 0
name_glib:dc.b "graphics.library",0
even

makecopper:
lea cop1,a0
move.L #$8bdffffe,d0
move #95,d6
mcl0:
move.l d0,(a0)+
move.L #$1920000,(a0)+
move.l #$1960000,(a0)+
add.L #$1000000,d0
dbra d6,mcl0

lea cop1+6,a0
lea deg,a1
move #5,d6
mcl5:
move (a1)+,d0
move (a1)+,d1
move (a1),d2
move #15,d7
mcl6:
move d0,(a0)
move d0,4(a0)
add.L #12,a0
add d1,d0
dbra d7,mcl6
dbra d6,mcl5

move.l #logo,d0
move #2,d7
lea copbpl4+4,a0
mcl9:
move d0,6(a0)
swap d0
move d0,2(a0)
swap d0
addi.l #[44*36],d0
addq.l #8,a0
dbra d7,mcl9
rts
mcs1:dc.w 0

deg:dc.W $f00,$10,$ff0,-$100,$f0,1,$ff,-$10,$f,$100,$f0f,-1
copperlist:
dc.L $10ffffe,$920030,$9400d8,$9683e0
copspr:dc.L $1200000,$1220000,$1240000,$1260000,$1280000,$12a0000
dc.L $12c0000,$12e0000,$1300000,$1320000,$1340000,$1360000,$1380000
dc.l $13a0000,$13c0000,$13e0000
dc.l $1020000,$1040000,$1080094,$10a0004,$1000200
dc.w	$0180,$0000,$0192,$0000

dc.w	$0180,$0000,$0182,$0eca,$0184,$035e,$0186,$023a
dc.w	$0188,$0126,$018a,$0cc0,$018c,$0c80,$018e,$0c50
dc.w	$01a0,$0000,$01a2,$099a,$01a4,$0778,$01a6,$0334
dc.w	$01a8,$0000,$01aa,$0667,$01ac,$0445,$01ae,$0223
dc.w	$01b0,$0000,$01b2,$0fff,$01b4,$0aaa,$01b6,$0666

;logo original
;dc.w	$0190,$0000,$0192,$0fff,$0194,$0bcd,$0196,$07ab
;dc.w	$0198,$0589,$019a,$0257,$019c,$0145,$019e,$0023
;logo flashtro
dc.w	$0190,$0000,$0192,$07ab,$0194,$0fff,$0196,$0589
dc.w	$0198,$0023,$019a,$0bcd,$019c,$0012,$019e,$0257

dc.L $1b80000,$1ba0000,$1bc0000,$1be0000
dc.L $200ffffe,$9683e0
copbpl1:dc.W $e4,5,$e6,$6000,$ec,5,$ee,$6030
copbpl3:dc.w $e0,0,$e2,0,$e8,0,$ea,0,$f0,0,$f2,0

copscr1:dc.L $3d0ffffe,$1005600
dc.L $400ffffe,$1800002
dc.L $410ffffe,$1800004
dc.L $420ffffe,$1800007
dc.L $430ffffe,$1800004
dc.l $440ffffe,$1800002
dc.L $44dffffe,$1800000,$10a0000,$450ffffe
copbpl4:
dc.l $1006600,$e40000,$e60000,$ec0000,$ee0000,$f40000,$f60000
dc.L $68dffffe,$1005600
dc.w $e4,6,$e6,$b000,$ec,6,$ee,$b000

dc.L $7edffffe
dc.w $e4,5,$e6,$6c90,$ec,5,$ee,$6cc0
dc.L $1005600,$10a0004,$1040040
dc.L $1940888
cop1:blk.b 96*12
dc.L $1920fff,$1960fff
dc.l $ffdffffe
dc.L $d0ffffe,$1920fff,$194000f,$1960fff
dc.L $150ffffe,$9c8010,$1000200,$1840fff,$1860fff,$1820004
dc.l $1a20000,$1a40000,$1a60000,$1aa0000,$1ac0000,$1ae0000
dc.l $1b20000,$1b40000,$1b60000,$e00005,$e20000,$1080000
dc.L $160ffffe,$1001200,$960020
bar1:dc.L $1800002,$1820fff
dc.L $170ffffe
bar2:dc.l $1800004,$1820eee
dc.L $180ffffe
bar3:dc.L $1800007,$1820ddd
dc.L $190ffffe
bar4:dc.L $1800004,$1820ccc
dc.l $1a0ffffe
bar5:dc.L $1800002,$1820bbb

dc.L $1b0ffffe
dc.L $1000200,$1800000
dc.L -2

bpa1:dc.L bp3
bpw1:dc.L bp4

***************************************************************
**  Amiga FUTURE COMPOSER V1.0 / 1.2 / 1.3   Replay routine  **
***************************************************************

END_MUSIC:
;clr.w onoff
clr.l $dff0a6
clr.l $dff0b6
clr.l $dff0c6
clr.l $dff0d6
move.w #$000f,$dff096
rts

INIT_MUSIC:
;move.w #1,onoff
lea MODULE(pc),a0
lea 100(a0),a1
lea	SEQpoint(pc),a5
move.l a1,(a5)
move.l a0,a1
add.l 8(a0),a1
lea	PATpoint(pc),a5
move.l a1,(a5)
move.l a0,a1
add.l 16(a0),a1
lea	FRQpoint(pc),a5
move.l a1,(a5)
move.l a0,a1
add.l 24(a0),a1
lea	VOLpoint(pc),a5
move.l a1,(a5)
move.l 4(a0),d0
divu #13,d0

lea 40(a0),a1
lea SOUNDINFO+4(pc),a2
moveq #10-1,d1
initloop:
move.w (a1)+,(a2)+
move.l (a1)+,(a2)+
addq.w #4,a2
dbf d1,initloop
moveq #0,d2
move.l a0,d1
add.l 32(a0),d1
lea	waveforms(pc),a5
sub.l a5,d1
lea SOUNDINFO(pc),a0
move.l d1,(a0)+
moveq #9-1,d3
initloop1:
move.w (a0),d2
add.l d2,d1
add.l d2,d1
addq.w #6,a0
move.l d1,(a0)+
dbf d3,initloop1

move.l SEQpoint(pc),a0
moveq #0,d2
move.b 12(a0),d2		;Get replay speed
bne.s speedok
move.b #3,d2			;Set default speed
speedok:
lea	respcnt(pc),a5
move.w d2,(a5)		;Init repspeed counter
lea	repspd(pc),a5
move.w d2,(a5)
INIT2:
lea	audtemp(pc),a5
clr.w (a5)
move.w #$000f,$dff096		;Disable audio DMA
move.w #$0780,$dff09a		;Disable audio IRQ
moveq #0,d7
mulu #13,d0
moveq #4-1,d6			;Number of soundchannels-1
lea V1data(pc),a0		;Point to 1st voice data area
lea silent(pc),a1
lea o4a0c8(pc),a2
initloop2:
move.l a1,10(a0)
move.l a1,18(a0)
clr.l 14(a0)
clr.b 45(a0)
clr.b 47(a0)
clr.w 8(a0)
clr.l 48(a0)
move.b #$01,23(a0)
move.b #$01,24(a0)
clr.b 25(a0)
clr.l 26(a0)
clr.w 30(a0)
moveq #$00,d3
move.w (a2)+,d1
move.w (a2)+,d3
divu #$0003,d3
move.b d3,32(a0)
mulu #$0003,d3
andi.l #$00ff,d3
andi.l #$00ff,d1
addi.l #$dff0a0,d1
move.l d1,a6
move.l #$0000,(a6)
move.w #$0100,4(a6)
move.w #$0000,6(a6)
move.w #$0000,8(a6)
move.l d1,60(a0)
clr.w 64(a0)
move.l SEQpoint(pc),(a0)
move.l SEQpoint(pc),52(a0)
add.l d0,52(a0)
add.l d3,52(a0)
add.l d7,(a0)
add.l d3,(a0)
move.w #$000d,6(a0)
move.l (a0),a3
move.b (a3),d1
andi.l #$00ff,d1
lsl.w #6,d1
move.l PATpoint(pc),a4
adda.w d1,a4
move.l a4,34(a0)
clr.l 38(a0)
move.b #$01,33(a0)
move.b #$02,42(a0)
move.b 1(a3),44(a0)
move.b 2(a3),22(a0)
clr.b 43(a0)
clr.b 45(a0)
clr.w 56(a0)
adda.w #$004a,a0	;Point to next voice's data area
dbf d6,initloop2
rts

PLAY:
lea pervol(pc),a6
;tst.w onoff
;bne.s music_on
;rts
music_on:
lea	respcnt(pc),a5
subq.w #1,(a5)			;Decrease replayspeed counter
bne.s nonewnote
move.w repspd(pc),(a5)		;Restore replayspeed counter
lea V1data(pc),a0		;Point to voice1 data area
bsr.L new_note
lea V2data(pc),a0		;Point to voice2 data area
bsr.L new_note
lea V3data(pc),a0		;Point to voice3 data area
bsr.L new_note
lea V4data(pc),a0		;Point to voice4 data area
bsr.L new_note

nonewnote:
lea	audtemp(pc),a5
clr.w (a5)
lea V1data(pc),a0
bsr.L effects
move.w d0,(a6)+
move.w d1,(a6)+
lea V2data(pc),a0
bsr.L effects
move.w d0,(a6)+
move.w d1,(a6)+
lea V3data(pc),a0
bsr.L effects
move.w d0,(a6)+
move.w d1,(a6)+
lea V4data(pc),a0
bsr.L effects
move.w d0,(a6)+
move.w d1,(a6)+
lea pervol(pc),a6
move.w audtemp(pc),d0
ori.w #$8000,d0			;Set/clr bit = 1
move.w d0,-(a7)
moveq #0,d1
move.l start1(pc),d2		;Get samplepointers
move.w offset1(pc),d1		;Get offset
add.l d1,d2			;Add offset
move.l start2(pc),d3
move.w offset2(pc),d1
add.l d1,d3
move.l start3(pc),d4
move.w offset3(pc),d1
add.l d1,d4
move.l start4(pc),d5
move.w offset4(pc),d1
add.l d1,d5
move.w ssize1(pc),d0		;Get sound lengths
move.w ssize2(pc),d1
move.w ssize3(pc),d6
move.w ssize4(pc),d7
move.w (a7)+,$dff096		;Enable audio DMA
chan1:
lea V1data(pc),a0
tst.w 72(a0)
beq.l chan2
subq.w #1,72(a0)
cmpi.w #1,72(a0)
bne.s chan2
clr.w 72(a0)
move.l d2,$dff0a0		;Set soundstart
move.w d0,$dff0a4		;Set soundlength
chan2:
lea V2data(pc),a0
tst.w 72(a0)
beq.s chan3
subq.w #1,72(a0)
cmpi.w #1,72(a0)
bne.s chan3
clr.w 72(a0)
move.l d3,$dff0b0
move.w d1,$dff0b4
chan3:
lea V3data(pc),a0
tst.w 72(a0)
beq.s chan4
subq.w #1,72(a0)
cmpi.w #1,72(a0)
bne.s chan4
clr.w 72(a0)
move.l d4,$dff0c0
move.w d6,$dff0c4
chan4:
lea V4data(pc),a0
tst.w 72(a0)
beq.s setpervol
subq.w #1,72(a0)
cmpi.w #1,72(a0)
bne.s setpervol
clr.w 72(a0)
move.l d5,$dff0d0
move.w d7,$dff0d4
setpervol:
lea $dff0a6,a5
move.w (a6)+,(a5)	;Set period
move.w (a6)+,2(a5)	;Set volume
move.w (a6)+,16(a5)
move.w (a6)+,18(a5)
move.w (a6)+,32(a5)
move.w (a6)+,34(a5)
move.w (a6)+,48(a5)
move.w (a6)+,50(a5)
rts

NEW_NOTE:
moveq #0,d5
move.l 34(a0),a1
adda.w 40(a0),a1
cmp.w #64,40(a0)
bne.s samepat
move.l (a0),a2
adda.w 6(a0),a2		;Point to next sequence row
cmpa.l 52(a0),a2	;Is it the end?
bne.s notend
move.w d5,6(a0)		;yes!
move.l (a0),a2		;Point to first sequence
notend:
moveq #0,d1
lea	spdtemp(pc),a5
addq.b #1,(a5)
cmpi.b #4,(a5)
bne.s nonewspd
move.b d5,(a5)
move.b -1(a1),d1	;Get new replay speed
beq.s nonewspd
lea	respcnt(pc),a5
move.w d1,(a5)		;store in counter
lea	repspd(pc),a5
move.w d1,(a5)
nonewspd:
move.b (a2),d1		;Pattern to play
move.b 1(a2),44(a0)	;Transpose value
move.b 2(a2),22(a0)	;Soundtranspose value

move.w d5,40(a0)
lsl.w #6,d1
add.l PATpoint(pc),d1	;Get pattern pointer
move.l d1,34(a0)
addi.w #$000d,6(a0)
move.l d1,a1
samepat:
move.b 1(a1),d1		;Get info byte
move.b (a1)+,d0		;Get note
bne.s ww1
andi.w #%11000000,d1
beq.s noport
bra.s ww11
ww1:
move.w d5,56(a0)
ww11:
move.b d5,47(a0)
move.b (a1),31(a0)

		;31(a0) = PORTAMENTO/INSTR. info
			;Bit 7 = portamento on
			;Bit 6 = portamento off
			;Bit 5-0 = instrument number
		;47(a0) = portamento value
			;Bit 7-5 = always zero
			;Bit 4 = up/down
			;Bit 3-0 = value
t_porton:
btst #7,d1
beq.s noport
move.b 2(a1),47(a0)	
noport:
andi.w #$007f,d0
beq.s nextnote
move.b d0,8(a0)
move.b (a1),9(a0)
move.b 32(a0),d2
moveq #0,d3
bset d2,d3
lea	audtemp(pc),a5
or.w d3,(a5)
move.w d3,$dff096
move.b (a1),d1
andi.w #$003f,d1	;Max 64 instruments
add.b 22(a0),d1
move.l VOLpoint(pc),a2
lsl.w #6,d1
adda.w d1,a2
move.w d5,16(a0)
move.b (a2),23(a0)
move.b (a2)+,24(a0)
move.b (a2)+,d1
andi.w #$00ff,d1
move.b (a2)+,27(a0)
move.b #$40,46(a0)
move.b (a2)+,d0
move.b d0,28(a0)
move.b d0,29(a0)
move.b (a2)+,30(a0)
move.l a2,10(a0)
move.l FRQpoint(pc),a2
lsl.w #6,d1
adda.w d1,a2
move.l a2,18(a0)
move.w d5,50(a0)
move.b d5,26(a0)
move.b d5,25(a0)
nextnote:
addq.w #2,40(a0)
rts

EFFECTS:
moveq #0,d7
testsustain:
tst.b 26(a0)		;Is sustain counter = 0
beq.s sustzero
subq.b #1,26(a0)	;if no, decrease counter
bra.L VOLUfx
sustzero:		;Next part of effect sequence
move.l 18(a0),a1	;can be executed now.
adda.w 50(a0),a1
testeffects:
cmpi.b #$e1,(a1)	;E1 = end of FREQseq sequence
beq.L VOLUfx
cmpi.b #$e0,(a1)	;E0 = loop to other part of sequence
bne.s testnewsound
move.b 1(a1),d0		;loop to start of sequence + 1(a1)
andi.w #$003f,d0
move.w d0,50(a0)
move.l 18(a0),a1
adda.w d0,a1
testnewsound:
cmpi.b #$e2,(a1)	;E2 = set waveform
bne.s o49c64
moveq #0,d0
moveq #0,d1
move.b 32(a0),d1
bset d1,d0
lea	audtemp(pc),a5
or.w d0,(a5)
move.w d0,$dff096
move.b 1(a1),d0
andi.w #$00ff,d0
lea SOUNDINFO(pc),a4
add.w d0,d0
move.w d0,d1
add.w d1,d1
add.w d1,d1
add.w d1,d0
adda.w d0,a4
move.l 60(a0),a3
move.l (a4),d1
lea	waveforms(pc),a5
add.l a5,d1
move.l d1,(a3)
move.l d1,68(a0)
move.w 4(a4),4(a3)
move.l 6(a4),64(a0)
swap d1
move.w #$0003,72(a0)
tst.w d1
bne.s o49c52
move.w #$0002,72(a0)
o49c52:
clr.w 16(a0)
move.b #$01,23(a0)
addq.w #2,50(a0)
bra.L o49d02
o49c64:
cmpi.b #$e4,(a1)
bne.s testpatjmp
move.b 1(a1),d0
andi.w #$00ff,d0
lea SOUNDINFO(pc),a4
add.w d0,d0
move.w d0,d1
add.w d1,d1
add.w d1,d1
add.w d1,d0
adda.w d0,a4
move.l 60(a0),a3
move.l (a4),d1
lea	waveforms(pc),a5
add.l a5,d1
move.l d1,(a3)
move.l d1,68(a0)
move.w 4(a4),4(a3)
move.l 6(a4),64(a0)

swap d1
move.w #$0003,72(a0)
tst.w d1
bne.s o49cae
move.w #$0002,72(a0)
o49cae:
addq.w #2,50(a0)
bra.s o49d02
testpatjmp:
cmpi.b #$e7,(a1)
bne.s testnewsustain
move.b 1(a1),d0
andi.w #$00ff,d0
lsl.w #6,d0
move.l FRQpoint(pc),a1
adda.w d0,a1
move.l a1,18(a0)
move.w d7,50(a0)
bra.L testeffects
testnewsustain:
cmpi.b #$e8,(a1)	;E8 = set sustain time
bne.s o49cea
move.b 1(a1),26(a0)
addq.w #2,50(a0)
bra.L testsustain
o49cea:
cmpi.b #$e3,(a1)
bne.s o49d02
addq.w #3,50(a0)
move.b 1(a1),27(a0)
move.b 2(a1),28(a0)
o49d02:
move.l 18(a0),a1
adda.w 50(a0),a1
move.b (a1),43(a0)
addq.w #1,50(a0)
VOLUfx:
tst.b 25(a0)
beq.s o49d1e
subq.b #1,25(a0)
bra.s o49d70
o49d1e:
subq.b #1,23(a0)
bne.s o49d70
move.b 24(a0),23(a0)
o49d2a:
move.l 10(a0),a1
adda.w 16(a0),a1
move.b (a1),d0
cmpi.b #$e8,d0
bne.s o49d4a
addq.w #2,16(a0)
move.b 1(a1),25(a0)
bra.s VOLUfx
o49d4a:
cmpi.b #$e1,d0
beq.s o49d70
cmpi.b #$e0,d0
bne.s o49d68
move.b 1(a1),d0
andi.l #$003f,d0
subq.b #5,d0
move.w d0,16(a0)
bra.s o49d2a
o49d68:
move.b (a1),45(a0)
addq.w #1,16(a0)
o49d70:
move.b 43(a0),d0
bmi.s o49d7e
add.b 8(a0),d0
add.b 44(a0),d0
o49d7e:
andi.w #$007f,d0
lea PERIODS(pc),a1
add.w d0,d0
move.w d0,d1
adda.w d0,a1
move.w (a1),d0
move.b 46(a0),d7
tst.b 30(a0)
beq.s o49d9e
subq.b #1,30(a0)

bra.s o49df4
o49d9e:
move.b d1,d5
move.b 28(a0),d4
add.b d4,d4
move.b 29(a0),d1
tst.b d7
bpl.s o49db4
btst #0,d7
bne.s o49dda
o49db4:
btst #5,d7
bne.s o49dc8
sub.b 27(a0),d1
bcc.s o49dd6
bset #5,d7
moveq #0,d1
bra.s o49dd6
o49dc8:
add.b 27(a0),d1
cmp.b d4,d1
bcs.s o49dd6
bclr #5,d7
move.b d4,d1
o49dd6:
move.b d1,29(a0)
o49dda:
lsr.b #1,d4
sub.b d4,d1
bcc.s o49de4
subi.w #$0100,d1
o49de4:
addi.b #$a0,d5
bcs.s o49df2
o49dea:
add.w d1,d1
addi.b #$18,d5
bcc.s o49dea
o49df2:
add.w d1,d0
o49df4:
eori.b #$01,d7
move.b d7,46(a0)

; DO THE PORTAMENTO THING
moveq #0,d1
move.b 47(a0),d1	;get portavalue
beq.s a56d0		;0=no portamento
cmpi.b #$1f,d1
bls.s portaup
portadown: 
andi.w #$1f,d1
neg.w d1
portaup:
sub.w d1,56(a0)
a56d0:
add.w 56(a0),d0
o49e3e:
cmpi.w #$0070,d0
bhi.s nn1
move.w #$0071,d0
nn1:
cmpi.w #$06b0,d0
bls.s nn2
move.w #$06b0,d0
nn2:
moveq #0,d1
move.b 45(a0),d1
rts

pervol: blk.b 16,0	;Periods & Volumes temp. store
respcnt: dc.w 0		;Replay speed counter 
repspd:  dc.w 0		;Replay speed counter temp
firseq:	 dc.w 0		;First sequence
lasseq:	 dc.w 0		;Last sequence
audtemp: dc.w 0
spdtemp: dc.w 0

V1data:  blk.b 64,0	;Voice 1 data area
offset1: blk.b 02,0	;Is added to start of sound
ssize1:  blk.b 02,0	;Length of sound
start1:  blk.b 06,0	;Start of sound

V2data:  blk.b 64,0	;Voice 2 data area
offset2: blk.b 02,0
ssize2:  blk.b 02,0
start2:  blk.b 06,0

V3data:  blk.b 64,0	;Voice 3 data area
offset3: blk.b 02,0
ssize3:  blk.b 02,0
start3:  blk.b 06,0

V4data:  blk.b 64,0	;Voice 4 data area
offset4: blk.b 02,0
ssize4:  blk.b 02,0
start4:  blk.b 06,0

o4a0c8: dc.l $00000000,$00100003,$00200006,$00300009
SEQpoint: dc.l 0
PATpoint: dc.l 0
FRQpoint: dc.l 0
VOLpoint: dc.l 0

SILENT: dc.w $0100,$0000,$0000,$00e1

PERIODS:dc.w $06b0,$0650,$05f4,$05a0,$054c,$0500,$04b8,$0474
	dc.w $0434,$03f8,$03c0,$038a,$0358,$0328,$02fa,$02d0
	dc.w $02a6,$0280,$025c,$023a,$021a,$01fc,$01e0,$01c5
	dc.w $01ac,$0194,$017d,$0168,$0153,$0140,$012e,$011d
	dc.w $010d,$00fe,$00f0,$00e2,$00d6,$00ca,$00be,$00b4
	dc.w $00aa,$00a0,$0097,$008f,$0087,$007f,$0078,$0071
	dc.w $0071,$0071,$0071,$0071,$0071,$0071,$0071,$0071
	dc.w $0071,$0071,$0071,$0071,$0d60,$0ca0,$0be8,$0b40
	dc.w $0a98,$0a00,$0970,$08e8,$0868,$07f0,$0780,$0714
	dc.w $1ac0,$1940,$17d0,$1680,$1530,$1400,$12e0,$11d0
	dc.w $10d0,$0fe0,$0f00,$0e28

SOUNDINFO:
;Offset.l , Sound-length.w , Start-offset.w , Repeat-length.w 

;Reserved for samples
	dc.w $0000,$0000,$0000,$0000,$0001 
	dc.w $0000,$0000,$0000,$0000,$0001 
	dc.w $0000,$0000,$0000,$0000,$0001 
	dc.w $0000,$0000,$0000,$0000,$0001 
	dc.w $0000,$0000,$0000,$0000,$0001 
	dc.w $0000,$0000,$0000,$0000,$0001 
	dc.w $0000,$0000,$0000,$0000,$0001 
	dc.w $0000,$0000,$0000,$0000,$0001 
	dc.w $0000,$0000,$0000,$0000,$0001 
	dc.w $0000,$0000,$0000,$0000,$0001 
;Reserved for synth sounds
	dc.w $0000,$0000,$0010,$0000,$0010 
	dc.w $0000,$0020,$0010,$0000,$0010 
	dc.w $0000,$0040,$0010,$0000,$0010 
	dc.w $0000,$0060,$0010,$0000,$0010 
	dc.w $0000,$0080,$0010,$0000,$0010 
	dc.w $0000,$00a0,$0010,$0000,$0010 
	dc.w $0000,$00c0,$0010,$0000,$0010 
	dc.w $0000,$00e0,$0010,$0000,$0010 
	dc.w $0000,$0100,$0010,$0000,$0010 
	dc.w $0000,$0120,$0010,$0000,$0010 
	dc.w $0000,$0140,$0010,$0000,$0010 
	dc.w $0000,$0160,$0010,$0000,$0010 
	dc.w $0000,$0180,$0010,$0000,$0010 
	dc.w $0000,$01a0,$0010,$0000,$0010 
	dc.w $0000,$01c0,$0010,$0000,$0010 
	dc.w $0000,$01e0,$0010,$0000,$0010 
	dc.w $0000,$0200,$0010,$0000,$0010 
	dc.w $0000,$0220,$0010,$0000,$0010 
	dc.w $0000,$0240,$0010,$0000,$0010 
	dc.w $0000,$0260,$0010,$0000,$0010 
	dc.w $0000,$0280,$0010,$0000,$0010 
	dc.w $0000,$02a0,$0010,$0000,$0010 
	dc.w $0000,$02c0,$0010,$0000,$0010 
	dc.w $0000,$02e0,$0010,$0000,$0010 
	dc.w $0000,$0300,$0010,$0000,$0010 
	dc.w $0000,$0320,$0010,$0000,$0010 
	dc.w $0000,$0340,$0010,$0000,$0010 
	dc.w $0000,$0360,$0010,$0000,$0010 
	dc.w $0000,$0380,$0010,$0000,$0010 
	dc.w $0000,$03a0,$0010,$0000,$0010 
	dc.w $0000,$03c0,$0010,$0000,$0010 
	dc.w $0000,$03e0,$0010,$0000,$0010 
	dc.w $0000,$0400,$0008,$0000,$0008 
	dc.w $0000,$0410,$0008,$0000,$0008 
	dc.w $0000,$0420,$0008,$0000,$0008 
	dc.w $0000,$0430,$0008,$0000,$0008 
	dc.w $0000,$0440,$0008,$0000,$0008
	dc.w $0000,$0450,$0008,$0000,$0008
	dc.w $0000,$0460,$0008,$0000,$0008
	dc.w $0000,$0470,$0008,$0000,$0008
	dc.w $0000,$0480,$0010,$0000,$0010
	dc.w $0000,$04a0,$0008,$0000,$0008
	dc.w $0000,$04b0,$0010,$0000,$0010
	dc.w $0000,$04d0,$0010,$0000,$0010
	dc.w $0000,$04f0,$0008,$0000,$0008
	dc.w $0000,$0500,$0008,$0000,$0008
	dc.w $0000,$0510,$0018,$0000,$0018

WAVEFORMS:
dc.w $c0c0,$d0d8,$e0e8,$f0f8,$00f8,$f0e8,$e0d8,$d0c8
dc.w $3f37,$2f27,$1f17,$0f07,$ff07,$0f17,$1f27,$2f37
dc.w $c0c0,$d0d8,$e0e8,$f0f8,$00f8,$f0e8,$e0d8,$d0c8
dc.w $c037,$2f27,$1f17,$0f07,$ff07,$0f17,$1f27,$2f37
dc.w $c0c0,$d0d8,$e0e8,$f0f8,$00f8,$f0e8,$e0d8,$d0c8
dc.w $c0b8,$2f27,$1f17,$0f07,$ff07,$0f17,$1f27,$2f37
dc.w $c0c0,$d0d8,$e0e8,$f0f8,$00f8,$f0e8,$e0d8,$d0c8
dc.w $c0b8,$b027,$1f17,$0f07,$ff07,$0f17,$1f27,$2f37
dc.w $c0c0,$d0d8,$e0e8,$f0f8,$00f8,$f0e8,$e0d8,$d0c8
dc.w $c0b8,$b0a8,$1f17,$0f07,$ff07,$0f17,$1f27,$2f37
dc.w $c0c0,$d0d8,$e0e8,$f0f8,$00f8,$f0e8,$e0d8,$d0c8
dc.w $c0b8,$b0a8,$a017,$0f07,$ff07,$0f17,$1f27,$2f37
dc.w $c0c0,$d0d8,$e0e8,$f0f8,$00f8,$f0e8,$e0d8,$d0c8
dc.w $c0b8,$b0a8,$a098,$0f07,$ff07,$0f17,$1f27,$2f37
dc.w $c0c0,$d0d8,$e0e8,$f0f8,$00f8,$f0e8,$e0d8,$d0c8
dc.w $c0b8,$b0a8,$a098,$9007,$ff07,$0f17,$1f27,$2f37
dc.w $c0c0,$d0d8,$e0e8,$f0f8,$00f8,$f0e8,$e0d8,$d0c8
dc.w $c0b8,$b0a8,$a098,$9088,$ff07,$0f17,$1f27,$2f37
dc.w $c0c0,$d0d8,$e0e8,$f0f8,$00f8,$f0e8,$e0d8,$d0c8
dc.w $c0b8,$b0a8,$a098,$9088,$8007,$0f17,$1f27,$2f37
dc.w $c0c0,$d0d8,$e0e8,$f0f8,$00f8,$f0e8,$e0d8,$d0c8
dc.w $c0b8,$b0a8,$a098,$9088,$8088,$0f17,$1f27,$2f37
dc.w $c0c0,$d0d8,$e0e8,$f0f8,$00f8,$f0e8,$e0d8,$d0c8
dc.w $c0b8,$b0a8,$a098,$9088,$8088,$9017,$1f27,$2f37
dc.w $c0c0,$d0d8,$e0e8,$f0f8,$00f8,$f0e8,$e0d8,$d0c8
dc.w $c0b8,$b0a8,$a098,$9088,$8088,$9098,$1f27,$2f37
dc.w $c0c0,$d0d8,$e0e8,$f0f8,$00f8,$f0e8,$e0d8,$d0c8
dc.w $c0b8,$b0a8,$a098,$9088,$8088,$9098,$a027,$2f37
dc.w $c0c0,$d0d8,$e0e8,$f0f8,$00f8,$f0e8,$e0d8,$d0c8
dc.w $c0b8,$b0a8,$a098,$9088,$8088,$9098,$a0a8,$2f37
dc.w $c0c0,$d0d8,$e0e8,$f0f8,$00f8,$f0e8,$e0d8,$d0c8
dc.w $c0b8,$b0a8,$a098,$9088,$8088,$9098,$a0a8,$b037
dc.w $8181,$8181,$8181,$8181,$8181,$8181,$8181,$8181
dc.w $7f7f,$7f7f,$7f7f,$7f7f,$7f7f,$7f7f,$7f7f,$7f7f
dc.w $8181,$8181,$8181,$8181,$8181,$8181,$8181,$8181
dc.w $817f,$7f7f,$7f7f,$7f7f,$7f7f,$7f7f,$7f7f,$7f7f
dc.w $8181,$8181,$8181,$8181,$8181,$8181,$8181,$8181
dc.w $8181,$7f7f,$7f7f,$7f7f,$7f7f,$7f7f,$7f7f,$7f7f
dc.w $8181,$8181,$8181,$8181,$8181,$8181,$8181,$8181
dc.w $8181,$817f,$7f7f,$7f7f,$7f7f,$7f7f,$7f7f,$7f7f
dc.w $8181,$8181,$8181,$8181,$8181,$8181,$8181,$8181
dc.w $8181,$8181,$7f7f,$7f7f,$7f7f,$7f7f,$7f7f,$7f7f
dc.w $8181,$8181,$8181,$8181,$8181,$8181,$8181,$8181
dc.w $8181,$8181,$817f,$7f7f,$7f7f,$7f7f,$7f7f,$7f7f
dc.w $8181,$8181,$8181,$8181,$8181,$8181,$8181,$8181
dc.w $8181,$8181,$8181,$7f7f,$7f7f,$7f7f,$7f7f,$7f7f
dc.w $8181,$8181,$8181,$8181,$8181,$8181,$8181,$8181
dc.w $8181,$8181,$8181,$817f,$7f7f,$7f7f,$7f7f,$7f7f
dc.w $8181,$8181,$8181,$8181,$8181,$8181,$8181,$8181
dc.w $8181,$8181,$8181,$8181,$7f7f,$7f7f,$7f7f,$7f7f
dc.w $8181,$8181,$8181,$8181,$8181,$8181,$8181,$8181
dc.w $8181,$8181,$8181,$8181,$817f,$7f7f,$7f7f,$7f7f
dc.w $8181,$8181,$8181,$8181,$8181,$8181,$8181,$8181
dc.w $8181,$8181,$8181,$8181,$8181,$7f7f,$7f7f,$7f7f
dc.w $8181,$8181,$8181,$8181,$8181,$8181,$8181,$8181
dc.w $8181,$8181,$8181,$8181,$8181,$817f,$7f7f,$7f7f
dc.w $8181,$8181,$8181,$8181,$8181,$8181,$8181,$8181
dc.w $8181,$8181,$8181,$8181,$8181,$8181,$7f7f,$7f7f
dc.w $8181,$8181,$8181,$8181,$8181,$8181,$8181,$8181
dc.w $8181,$8181,$8181,$8181,$8181,$8181,$817f,$7f7f
dc.w $8080,$8080,$8080,$8080,$8080,$8080,$8080,$8080
dc.w $8080,$8080,$8080,$8080,$8080,$8080,$8080,$7f7f
dc.w $8080,$8080,$8080,$8080,$8080,$8080,$8080,$8080
dc.w $8080,$8080,$8080,$8080,$8080,$8080,$8080,$807f
dc.w $8080,$8080,$8080,$8080,$7f7f,$7f7f,$7f7f,$7f7f
dc.w $8080,$8080,$8080,$807f,$7f7f,$7f7f,$7f7f,$7f7f
dc.w $8080,$8080,$8080,$7f7f,$7f7f,$7f7f,$7f7f,$7f7f
dc.w $8080,$8080,$807f,$7f7f,$7f7f,$7f7f,$7f7f,$7f7f
dc.w $8080,$8080,$7f7f,$7f7f,$7f7f,$7f7f,$7f7f,$7f7f
dc.w $8080,$807f,$7f7f,$7f7f,$7f7f,$7f7f,$7f7f,$7f7f
dc.w $8080,$7f7f,$7f7f,$7f7f,$7f7f,$7f7f,$7f7f,$7f7f
dc.w $8080,$7f7f,$7f7f,$7f7f,$7f7f,$7f7f,$7f7f,$7f7f
dc.w $8080,$9098,$a0a8,$b0b8,$c0c8,$d0d8,$e0e8,$f0f8
dc.w $0008,$1018,$2028,$3038,$4048,$5058,$6068,$707f
dc.w $8080,$a0b0,$c0d0,$e0f0,$0010,$2030,$4050,$6070
dc.w $4545,$797d,$7a77,$7066,$6158,$534d,$2c20,$1812
dc.w $04db,$d3cd,$c6bc,$b5ae,$a8a3,$9d99,$938e,$8b8a
dc.w $4545,$797d,$7a77,$7066,$5b4b,$4337,$2c20,$1812
dc.w $04f8,$e8db,$cfc6,$beb0,$a8a4,$9e9a,$9594,$8d83
dc.w $0000,$4060,$7f60,$4020,$00e0,$c0a0,$80a0,$c0e0
dc.w $0000,$4060,$7f60,$4020,$00e0,$c0a0,$80a0,$c0e0
dc.w $8080,$9098,$a0a8,$b0b8,$c0c8,$d0d8,$e0e8,$f0f8
dc.w $0008,$1018,$2028,$3038,$4048,$5058,$6068,$707f
dc.w $8080,$a0b0,$c0d0,$e0f0,$0010,$2030,$4050,$6070

MODULE:	;*** changer muzik ici
incbin "dh0:angels/mod.thalion"

setf:blk.b 4,0
set:
incbin "dh0:angels/set1"
set1=set
set2=set1+256
set3=set2+256
set4=set3+$b0
set5=set4+$b0
set6=set5+$80
fonts:
incbin "dh0:angels/fonts"

sin:
incbin "dh0:angels/sincos"
cos=sin+$200
vide:blk.b 32,0

jug:
dc.w 0,500,0,0,0,0
dc.w 0,1180,0	;tete
dc.W 0,700,0
dc.w 350,550,0,500,380,0,650,220,0
dc.w -350,550,0,-400,300,0,-400,50,0
dc.W 300,-250,100,-300,-250,100
dc.W 300,-480,100,300,-710,100
dc.W -300,-480,100,-300,-710,100
dc.w 300,-940,90,300,-1170,60
dc.w -300,-940,90,-300,-1170,60
dc.W 300,-1400,20,-300,-1400,20
hand:dc.w 520,100,0,370,25,0,220,-50,0
dc.w -400,-25,200,-400,-100,350,-400,-150,470
boules:dc.w -400,-250,470
dc.w 110,1070,190,-110,1070,190
dc.w $7777

coord:blk.w 138,0
ordre:dc.w 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14
dc.w 15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,$7777

bra1:dc.w $200

bo:dc.L bob1,mask1,bob2,mask2
dc.l bob8,mask8
dc.L bob3,mask3
dc.l bob5,mask5,bob5,mask5,bob5,mask5
dc.l bob5,mask5,bob5,mask5,bob5,mask5
dc.l bob4,mask4,bob4,mask4
dc.l bob5,mask5,bob5,mask5
dc.l bob5,mask5,bob5,mask5
dc.l bob5,mask5,bob5,mask5
dc.l bob5,mask5,bob5,mask5
dc.l bob5,mask5,bob5,mask5
dc.l bob5,mask5,bob6,mask6,bob7,mask7
dc.l bob5,mask5,bob6,mask6,bob7,mask7
dc.L bob9,mask9	;boules
dc.L bob7,mask7,bob7,mask7
val:blk.w 35
bobs:incbin "dh0:angels/bobs"
bob1=bobs
bob2=bob1+576
bob3=bob2+576
bob4=bob3+192
bob5=bob4+192
bob6=bob5+192
bob7=bob6+192
bob8=bob7+192
bob9=bob8+576

logo:
;original 
;incbin "dh0:angels/logo44"
;flashtro
incbin "dh0:angels/logo66"

end:
masks:
mask1:
mask2=mask1+576
mask3=mask2+576
mask4=mask3+192
mask5=mask4+192
mask6=mask5+192
mask7=mask6+192
mask8=mask7+192
mask9=mask8+576
bp1=$50000
;bp2=bp1+11638
bp5=$56000
bp3=$60000
bp4=$70000
bpv=$6b000
