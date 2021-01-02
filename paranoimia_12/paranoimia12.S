;paranoimia intro v1.2
;coded by mr. spiv
;
; font rip, reloc and 64k boundary fix by VrS!

forbid=-132
permit=-138

;	org $49600
;	load $49600

	section	spiv,code_c

st:		move.l	$4.w,a6
		bset	#7,$bfd100
		bset	#3,$bfd100
		bclr	#3,$bfd100

; fix: align screens and buffers to 64k boundary
		move.l	#buffer,d0
		and.l	#$3ff0000,d0		; 64k boundary
		move.l	d0,pBuffer
		move.l	d0,screen_1
		add.l	#$2800,d0
		move.l	d0,screen_2
		add.l	#$2800,d0
		move.l	d0,pSinscr
		add.l	#$b000,d0
		move.l	d0,pix_base
;
		move.l	pBuffer(pc),membase+2	; $60000
		movem.l	d0-d7/a0-a6,-(sp)
		jsr	forbid(a6)
		move.l	$9c(a6),old_copper
;		bsr	clear
		bsr	do_coplist
		bsr	countstars
		bsr	do_spritepoints
		bsr	interrupts
		move.w	#$c030,$dff09a	;vb+copper
		bra	intro

exit:		move.w	#$7fff,$dff09a
		move.w	#$7fff,$dff096
		move.w	#$0000,$dff02e		; copcon = 0
		move.l	intrsafe(pc),$6c.w
		move.w	dmasafe(pc),$dff096
		move.w	intenasafe(pc),$dff09a
		move.l	old_copper(pc),a0
		move.l	38(a0),$dff080
		move.l	50(a0),$dff084
		movem.l	(sp)+,d0-d7/a0-a6
		move.l	$4.w,a6
		jsr	permit(a6)
		moveq	#0,d0
		rts

;---------------------------------

clear:		;lea	buffer,a0	; $60000
c_loop1:	;clr.l	(a0)+
		;cmpa.l	#pixbase,a0	; $70000
		;blo.s	c_loop1
		rts

interrupts:	move.w	$dff01c,intenasafe
		move.w	$dff002,dmasafe
		move.w	#$7fff,$dff09a
		move.w	#$7fff,$dff096
		or.w	#$8000,intenasafe
		or.w	#$8000,dmasafe
		move.l	#coplist,$dff080
		move.w	$dff088,d0
		move.l	#cop,copaddr+2
		move.l	$6c.w,intrsafe
		move.l	#handler,$6c.w
		moveq	#1,d0
		jsr	module	;jsr	$4195e Init custom player
		move.w	#$83e0,$dff096
		rts

do_coplist:	move.l	pSinscr(pc),d0	;sinus screen $65000
		move.l	d0,workbase1
		add.l	#78*40,d0
		swap 	d0
		move.w	d0,scrs+10
		swap	d0
		move.w	d0,scrs+14	
		add.l	#80,d0
		swap 	d0
		move.w	d0,scrs+18
		swap	d0
		move.w	d0,scrs+22
		add.l	#256*40,d0
		move.l	d0,workbase2	;sinus scroll

		lea	cop(pc),a0
		move.w	#19,d0
		move.l	workbase2(pc),d1	;!sinscreen+1 line 
		move.l	workbase1(pc),d4	;!sinscreen
		add.l	#80*40,d4
		move.w	#$c000,d2
d_back:		moveq	#7,d3
d_loop1:	move.l	#$01010000,(a0)+	; bplcon0+1
		move.l	#$01010000,(a0)+
		move.l	#$00520000,(a0)+	; bltAptl
		move.w	d1,-2(a0)
		move.l	#$004a0000,(a0)+	; bltCptl
		move.w	d4,-2(a0)
		move.l	#$00560000,(a0)+	; bltDptl
		move.w	d4,-2(a0)
		move.l	#$00720000,(a0)+	; bltBdat
		move.w	d2,-2(a0)
		ror.w	#2,d2
		move.l	#$00580401,(a0)+	; bltsize
		dbf	d3,d_loop1
		addq.l	#2,d1
		addq.l	#2,d4
		dbf	d0,d_back

setcolor1:	lea	varit(pc),a0
		lea	colorhelp(pc),a1
		clr.w	colortest
		move.w	#112,d0
		move.l	#$7c01fffe,d1
		move.l	#$01920000,d2
colorback:	move.w	(a1)+,d3
		move.w	(a1)+,d4
		move.b	#16,colorcount
colorloop:	move.l	d1,(a0)+	;wait
		move.l	d2,(a0)+	;ink
		move.w	d3,-2(a0)
		add.l	#$01000000,d1
		tst.b	colortest
		beq.s	increase
		sub.w	d4,d3
		bra.s	end_normal
increase:	add.w	d4,d3
end_normal:	subq.w	#1,d0
		beq.s	end_color
		sub.b	#1,colorcount
		cmp.b	#0,colorcount
		bne.s	colorloop
		not.b	colortest
		bra.s	colorback
end_color:	
		lea	logo(pc),a0
		move.l	pSinscr(pc),a1	;lea	sinscreen+$1e74,a1	;lea	$66e74,a1
		adda.l	#$1e74,a1	;
		move.l	(a0)+,(a1)
		add.w	#40,a1
		move.l	(a0)+,(a1)
		add.w	#40,a1
		move.l	(a0)+,(a1)
		add.w	#40,a1
		move.l	(a0)+,(a1)
		add.w	#40,a1
		move.l	(a0)+,(a1)
		add.w	#40,a1
; fix addr in copper
		move.l	pSinscr(pc),d0
		add.l	#$c80,d0
		lea	cbltD,a0
		move.w	d0,6(a0)
		swap	d0
		move.w	d0,2(a0)
		swap	d0
		move.l	screen_1(pc),d0
		swap	d0
		lea	cbltA,a0
		move.w	d0,2(a0)
		lea	cbltB,a0
		move.w	d0,2(a0)
		lea	cbltC,a0
		move.w	d0,2(a0)
		rts
		
;----------------------------------

handler:	movem.l	d0-d7/a0-a6,-(sp)
		pea	pois(pc)
		move.w	$dff01e,d0
		and.w	$dff01c,d0
		btst	#5,d0
		beq.s	not_vb
		jmp	verticalblank(pc)
not_vb:		btst	#4,d0
		beq.s	not_copper
		jmp	copper_intr(pc)
not_copper:	moveq	#$70,d0
		rts
pois:		move.w	d0,$dff09c		
		movem.l	(sp)+,d0-d7/a0-a6
		rte

intro:		
		btst	#6,$bfe001
		bne.s	intro
		bra	exit

;-------------------------------------

verticalblank:	move.w	#2,$dff02e	; copcon = blitter registers access with copper
		bsr	setpicture1
no_frame:	bsr	do_sinus
		bsr	starroutine
		move.l	#0,a0
		move.l	a0,d1
		move.l	a0,d2
		move.l	a0,d3
		move.l	a0,d4
		move.l	a0,d5
		move.l	a0,d6
		moveq	#12,d7
		moveq	#29,d0
		move.l	2+membase(pc),a3
		add.w	#227*40+6+28,a3
cls3:		movem.l	d1-d6/a0,-(a3)
		sub.w	d7,a3
		movem.l	d1-d6/a0,-(a3)
		sub.w	d7,a3
		movem.l	d1-d6/a0,-(a3)
		sub.w	d7,a3
		movem.l	d1-d6/a0,-(a3)
		sub.w	d7,a3
		dbf	d0,cls3
	;move.w	#$00f,$dff180		
		moveq	#$20,d0		;return code
		rts

;-------------------------------------

copper_intr:	lea	$dff000,a6
		;btst	#10,$dff016
		;beq.s	no_scroll
		bsr	scroll_mem
no_scroll:	btst	#14,2(a6)
		bne.s	no_scroll

		move.w	#12,$66(a6)
		move.l	#$01000000,$40(a6)
		move.l	2+membase(pc),d0
		add.w	#28*40+6,d0
		move.l	d0,$54(a6)
		move.w	#$140e,$58(a6)

pixel:		move.w	#$8000,$74(a6)	;source a data
		move.w	texture(pc),$72(a6)	;texture
		move.w	#40,$60(a6)	;modulo c

	;move.w	#$f00,$180(a6)

		jsr	module+$34c	;jsr	$41caa Custom Play

		lea	$dff000,a6
wait2:		btst	#14,2(a6)
		bne.s	wait2
	;move.w	#0,$180(a6)
		move.w	#40,$66(a6)	;modulo d
		move.l	pBuffer(pc),d0	; fix
		swap	d0
		move.w	d0,$50(a6)	; bltApth $6 FIX
		move.w	d0,$54(a6)	; bltDpth $6
		move.w	d0,$48(a6)	; bltCpth $6
		move.w	d0,$4c(a6)	; bltBpth $6
		bsr	rotate

	btst	#2,$16(a5)
	bne.s	no_raster
	move.w	#$500,$dff180

no_raster:	moveq	#$10,d0		;return code
		rts

;--------------------------------

scroll_mem:	tst.b	pause
		beq.s	scroll_cont
		sub.b	#1,pause+1
		beq.s	end_scroll
		rts
end_scroll:	move.b	#50,pause+1
		sub.b	#1,pause
		rts
scroll_cont:	move.l	workbase2(pc),a0
		lea	$dff000,a6
		move.l	a0,a1
		add.l	#702,a1
wait:		btst	#14,$2(a6)
		bne.s	wait
		move.l	a1,$50(a6)	;source a
		move.l	a1,$54(a6)	;destination
		move.l	#$ffffffff,$44(a6)
		clr.l	$64(a6)	;modulos
		move.w	scroll_speed(pc),d0
		lsl.w	#2,d0
		move.l	btlcon(pc,d0.w),$40(a6)	;btlcon 0
		move.w	#$0416,$58(a6)	;start blitter
		bra.s	smem_jatko

btlcon:		dc.l	$09f00002,$19f0f002,$29f0e002,$39f0d002
		dc.l	$49f0c002,$59f0b002,$69f0a002,$79f09002
		dc.l	$89f08002,$99f07002,$a9f06002,$b9f05002
		dc.l	$c9f04002,$d9f03002,$e9f02002,$f9f01002
		dc.l	$09f00002

scroll_speed:	dc.w	3	;1=hitain    0=nopein
scroll_laskuri:	dc.w	0
scroll_font:	dc.w	0
text_counter:	dc.w	0
pause:		dc.w	0
text_base:	dc.l	text

smem_jatko:	move.w	scroll_laskuri(pc),d7
		sub.w	scroll_speed(pc),d7
		bhs.s	normal
		tst.w	scroll_font
		beq.s	no_newfont
		move.w	d7,d6
		bsr.s	new_font
no_newfont:	not.w	scroll_font
normal:		and.w	#$7,d7
		move.w	d7,scroll_laskuri
		rts

new_font:	add.l	#38,a0	;place for new font
		lea	font_width(pc),a4
		lea	font_graf,a2
		move.w	text_counter(pc),d0
		move.l	text_base(pc),a3
font_paluu:	move.b	0(a3,d0.w),d1
		cmp.b	#'a',d1
		blo.s	normal_char
		bsr	control_code
		bra.s	font_paluu
normal_char:	addq.w	#1,d0
		cmp.b	#32,d1
		blo.s	font_paluu
		move.w	d0,text_counter	
		ext.w	d1
		sub.w	#$20,d1
		tst.b	0(a4,d1.w)
		beq.s	normal_width
		not.w	scroll_font
normal_width:	lsl.w	#5,d1
		add.w	d1,a2

	move.w	#$8400,$96(a6)
wait3:		btst	#14,2(a6)
		bne.s	wait3
	move.w	#$0400,$96(a6)

		and.w	#$000f,d6
		lsl.w	#2,d6
		move.l	#$ffff0000,$44(a6)	;btlcon1
		move.w	#$0026,$66(a6)		;mask a & destination
		move.l	#$0026fffe,$60(a6)	;source c & b
		move.l	a2,$4c(a6)	;font_graf B
		move.l	a0,$48(a6)	;dest_help C
		move.l	a0,$54(a6)	;destination D
		move.w	#$ffff,$74(a6)	;source a = mask
		move.l	btlcon2(pc,d6.w),$40(a6)
		move.w	#$0402,$58(a6)	;start blitter
		rts

btlcon2:	dc.l	$07ca0000,$17ca1000,$27ca2000,$37ca3000
		dc.l	$47ca4000,$57ca5000,$67ca6000,$77ca7000
		dc.l	$87ca8000,$97ca9000,$a7caa000,$b7cab000
		dc.l	$c7cac000,$d7cad000,$e7cae000,$f7caf000

control_code:	cmp.b	#'r',d1
		bne.s	control_jatko
		moveq	#0,d0
		rts
control_jatko:	cmp.b	#'p',d1		;dc.b 17,x    pause x seconds
		beq.s	control_jatko2
		sub.b	#96,d1
		bne.s	ok_1
		addq.b	#1,d1
ok_1:		cmp.b	#9,d1
		blo.s	ok_2
		moveq	#8,d1
ok_2:		move.b	d1,scroll_speed+1
		addq.l	#1,d0
		rts
control_jatko2:	move.b	#50,pause+1
		move.b	1(a3,d0.w),d1
		sub.b	#96,d1
		move.b	d1,pause
		addq.l	#2,d0
		rts

;scrollissa	dc.b 0 = restart text
;		dc.b 1-8 = scrollin nopeus
;		dc.b 17,x =  pause

;----------------------------------

do_sinus:	;lea	14+cop2(pc),a0
copaddr:	lea	$000000,a0
		add.l	#14,a0
		lea	sin_taulu(pc),a1
		move.w	sinus_count(pc),d3
		add.w	d3,d3
		moveq	#19,d0
		moveq	#28,d2
		moveq	#0,d5
		move.w	#511,d6
		moveq	#2,d7
s_back:		moveq	#7,d1
s_loop:		move.w	0(a1,d3.w),d4
		add.w	d5,d4
		move.w	d4,(a0)
		move.w	d4,4(a0)
		add.w	d7,d3
		and.w	d6,d3
		add.w	d2,a0
		dbf	d1,s_loop
		add.w	d7,d5
		dbf	d0,s_back
		add.b	#4,sinus_count+1
		rts

;---------------------------------

starroutine:	lea	starbase1(pc),a0
		moveq	#33,d0
staralku:	moveq	#3,d1		
starloop:	move.w	(a0),d2
		add.b	d1,d2
		move.w	d2,(a0)
		addq.l	#8,a0
		subq.w	#1,d1
		bne.s	starloop
		dbf	d0,staralku
		rts

;********** count stars **********

countstars:	move.w	#$2c00,d7
		move.w	#$2d00,d6
		lea	starbase1(pc),a0
		bsr.s	do_count
		rts

do_count:	moveq	#33,d0
		moveq	#0,d5
makeback:	moveq	#2,d4
makeloop:	move.w	$dff006,d2
		move.b	$bfe801,d1
		ror.l	d0,d1
		move.w	d1,d3
		eor.w	d2,d1
		add.w	d3,d3
		add.w	d2,d2
		add.w	d2,d2
		swap	d3
		add.w	d2,d3
		eor	d2,d3
		ror.w	d0,d3
		swap	d3
		eor.w	d3,d1
		and.w	#$1ff,d1
		lsr.w	#1,d1
		move.l	d5,d2
		mulu	#8,d2
		move.w	d7,0(a0,d2.w)
		or.w	d1,0(a0,d2.w)
		move.w	d6,2(a0,d2.w)
		cmp.b	#2,d4
		bne.s	yli1
		move.l	#$80000000,4(a0,d2.w)
		bra.s	end_star
yli1:		cmp.b	#1,d4
		bne.s	yli2
		move.l	#$00008000,4(a0,d2.w)
		bra.s	end_star
yli2:		move.l	#$80008000,4(a0,d2.w)
end_star:	add.w	#$0200,d7
		bcc.s	noover1
		or.b	#4,d6
noover1:	add.w	#$0200,d6
		bcc.s	noover2
		or.b	#2,d6
noover2:	addq.w	#1,d5
		dbf	d4,makeloop
		dbf	d0,makeback
		rts

do_spritepoints:lea	sprites(pc),a0
		move.l	#$01200000,d1
		move.l	#$00020000,d2
		lea	sprite_num1(pc),a1
		bsr.s	do_spr_point
		lea	sprite_num1(pc),a1
		bsr.s	do_spr_point
		lea	sprite_num1(pc),a1
		bsr.s	do_spr_point
		lea	sprite_num1(pc),a1
		bsr.s	do_spr_point
		lea	sprite_num1(pc),a1
		bsr.s	do_spr_point
		lea	sprite_num1(pc),a1
		bsr.s	do_spr_point
		lea	sprite_num1(pc),a1
		bsr.s	do_spr_point
		lea	starbase1(pc),a1
		bsr.s	do_spr_point
		rts

do_spr_point:	move.w	a1,d3
		move.l	a1,d4
		swap	d4
		move.l	d1,(a0)+
		move.w	d4,-2(a0)
		add.l	d2,d1
		move.l	d1,(a0)+
		move.w	d3,-2(a0)
		add.l	d2,d1
		rts

;----------------------------------

setpicture1:	tst.b	screenprior
		bne.s	screen2
		move.l	screen_2(pc),d0
		bra.s	doscreenpoint
screen2:	move.l	screen_1(pc),d0
doscreenpoint:	add.l	#28*40,d0
		move.w	d0,scrs+6
		swap	d0
		move.w	d0,scrs+2
		swap	d0
		sub.l	#28*40,d0
		move.l	d0,membase+2
		not.b	screenprior
		rts

	dc.b "     Intro coded by Mr. Spiv of CAVE... v1.0 didn't work in NTSC "
	dc.b "Amigas but this v1.2 should (?????)  The vector calculation is "
	dc.b "now very much faster... Contact me! Write to: Jouni Korhonen, "
	dc.b "Hiihtomajantie, 11120 Riihimaki, Finland        "
	dc.b "--> Fixed by VrS! 2006 <--"
	even

rotate:		lea	axis(pc),a0
		move.w	#360,d1
		move.w	(a0),d0		;x
x_add:		add.w	#1,d0
		bpl.s	neg_1
		add.w	d1,d0
neg_1:		cmp.w	d1,d0
		blo.s	lower_1
		sub.w	d1,d0
lower_1:	move.w	d0,(a0)
		move.w	2(a0),d0	;y
y_add:		add.w	#-3,d0
		bpl.s	neg_2
		add.w	d1,d0
neg_2:		cmp.w	d1,d0
		blo.s	lower_2
		sub.w	d1,d0
lower_2:	move.w	d0,2(a0)
		move.w	4(a0),d0	;z
z_add:		add.w	#2,d0
		bpl.s	neg_3
		add.w	d1,d0
neg_3:		cmp.w	d1,d0
		blo.s	lower_3
		sub.w	d1,d0
lower_3:	move.w	d0,4(a0)

		lea	sin_table(pc),a1
		lea	cos_table(pc),a2
		lea	m4(pc),a4
		move.l	pix_base(pc),a6
		move.l	8(a0),a3	;object codes

		move.w	(a0),d1
		move.w	2(a0),d2
		move.w	4(a0),d3
		add.w	d1,d1
		add.w	d2,d2
		add.w	d3,d3
		
		move.w	0(a2,d1.w),d4
		muls	0(a2,d2.w),d4	;m1

		move.w	0(a1,d1.w),d5
		muls	0(a2,d2.w),d5
		neg.l	d5		;m2

		move.w	0(a1,d2.w),d6
		asl.w	#7,d6
		neg.w	d6		;m3

		move.w	0(a1,d1.w),d7
		muls	0(a2,d3.w),d7
		move.w	0(a2,d1.w),d0
		muls	0(a1,d2.w),d0
		muls	0(a1,d3.w),d0
		asr.l	#7,d0
		sub.w	d0,d7		;m7

		move.w	0(a2,d1.w),d0
		muls	0(a2,d3.w),d0
		move.w	d0,6(a4)
		move.w	0(a1,d1.w),d0
		muls	0(a1,d2.w),d0
		muls	0(a1,d3.w),d0
		asr.l	#7,d0
		add.w	d0,6(a4)	;m8

		move.w	0(a2,d2.w),d0
		muls	0(a1,d3.w),d0
		neg.l	d0
		move.w	d0,8(a4)	;m9

		move.w	0(a1,d1.w),d0
		muls	0(a1,d3.w),d0
		move.w	d0,(a4)
		move.w	0(a2,d1.w),d0
		muls	0(a1,d2.w),d0
		muls	0(a2,d3.w),d0
		asr.l	#7,d0
		add.w	d0,(a4)		;m4

		move.w	0(a2,d1.w),d0
		muls	0(a1,d3.w),d0
		move.w	d0,2(a4)
		move.w	0(a1,d1.w),d0
		muls	0(a1,d2.w),d0
		muls	0(a2,d3.w),d0
		asr.l	#7,d0
		sub.w	d0,2(a4)	;m5

		move.w	0(a2,d2.w),d0
		muls	0(a2,d3.w),d0
		move.w	d0,4(a4)	;m6

		move.w	(a3)+,loop1	;loop
calc_loop:	move.w	(a3),d0		;x

		neg.w	d0
		muls	d4,d0
		move.w	4(a3),d1	;z
		muls	d5,d1
		move.w	2(a3),d2	;y
		muls	d6,d2
		add.l	d1,d0
		add.l	d2,d0
		lsl.l	#2,d0
		swap	d0		;final x

		move.w	(a3),d1
		neg.w	d1
		muls	m4(pc),d1
		move.w	4(a3),d2
		muls	m5(pc),d2
		move.w	2(a3),d3
		muls	m6(pc),d3
		add.l	d2,d1
		add.l	d3,d1
		lsl.l	#2,d1
		swap	d1		;final	y

		move.w	(a3),d2
		neg.w	d2
		muls	d7,d2
		move.w	4(a3),d3
		muls	m8(pc),d3
		add.l	d3,d2
		move.w	2(a3),d3
		muls	m9(pc),d3
		add.l	d3,d2
		lsl.l	#2,d2
		swap	d2		;final z

;d1=y   d2=z   d0=x

		add.w	mz(pc),d2

		muls	zoom(pc),d0
		divs	d2,d0
zero1:		add.w	#167,d0
		move.w	d0,(a6)+	;en_x

		muls	zoom(pc),d1
		divs	d2,d1
zero2:		add.w	#128,d1
		move.w	d1,(a6)+	;en_y

		addq.l	#6,a3
		sub.w	#1,loop1
		bne	calc_loop
		;rts

draw_line:	lea	$dff000,a5
		move.w	(a3)+,d7
		move.l	pix_base(pc),a6
coords:		move.w	(a3)+,d0	;st_x & st_y
		move.w	(a3)+,d2	;en_x & en_y
		add.w	d0,d0
		add.w	d2,d2
		add.w	d0,d0
		add.w	d2,d2
		move.w	2(a6,d0.w),d1	;st_y
		move.w	0(a6,d0.w),d0	;st_x
		move.w	2(a6,d2.w),d3	;en_y
		move.w	0(a6,d2.w),d2	;en_x

push_ok:	move.w	d1,d4
		mulu	#40,d4
		move.w	d0,d5
		lsr.w	#3,d5
		ext.l	d5
		add.l	d5,d4
membase:	add.l	#$000000,d4	;! buffer->screen1

		moveq	#0,d5
		sub.w	d1,d3		;y2-y1
		roxl.b	#1,d5
		tst.w	d3
		bge.s	ei_neg1
		neg.w	d3
ei_neg1:	sub.w	d0,d2		;x2-x1
		roxl.b	#1,d5
		tst.w	d2
		bge.s	ei_neg2
		neg.w	d2
ei_neg2:	move.w	d3,d1		;d1=y
		sub.w	d2,d1		;y-x
		bge.s	ei_neg3
		exg	d2,d3		;d3=x d2=y
ei_neg3:	roxl.b	#1,d5
		move.b	octtabel(pc,d5.w),d5
		add.w	d2,d2
		
wait_1:		btst	#14,2(a5)
		bne.s	wait_1

		move.w	d2,$62(a5)	;modulo b 
		sub.w	d3,d2
		bge.s	z_sign
		or.b	#$40,d5
z_sign:		move.w	d2,$52(a5)	;source a 
		sub.w	d3,d2
		move.w	d2,$64(a5)	;modulo a 
		and.w	#$000f,d0
		ror.w	#4,d0
		or.w	#$0bca,d0
		move.w	d0,$40(a5)	;btlcon 0
		move.w	d5,$42(a5)	;bltcon 1
		move.w	d4,$4a(a5)	;source c
		move.w	d4,$56(a5)	;destination

		tst.w	d3
		bne.s	not_zero
		addq.w	#1,d3
not_zero:	lsl.w	#6,d3
		addq.w	#2,d3
		move.w	d3,$58(a5)

no_line:	dbf	d7,coords
		rts

octtabel:	dc.b $1,$11,9,$15,5,$19,$d,$1d

;-----------------------------------

sprite_num1:	dc.l	0
		ds.w	2
		dc.l	0
starbase1:	ds.w	102*4
		dc.l	0

dmasafe:	dc.w 0
intenasafe:	dc.w 0
intrsafe:	dc.l 0
old_copper:	dc.l 0
screen_1:	dc.l screen1	;$60000
screen_2:	dc.l screen1b	;$62800
workbase1:	dc.l 0
workbase2:	dc.l 0	;sinus scroll
sinus_count:	dc.w 0
m4:		dc.w 0
m5:		dc.w 0
m6:		dc.w 0
m7:		dc.w 0
m8:		dc.w 0
m9:		dc.w 0
pix_base:	dc.l pixbase	;$70000
loop1:		dc.w 0
pBuffer	dc.l	0
pSinscr	dc.l	0
;----------------------------------
coplist:
	dc.l $008e2c81,$0090f4c1,$00920038,$009400d0,$01040040
	dc.l $01000000,$01800000,$01080000,$010a0000,$01020002
sprites	ds.l 16
scrs:	dc.w	$e0,0,$e2,0	; $70000
	dc.w	$e4,0,$e6,0
	dc.w	$e8,0,$ea,0
	dc.w	$0186,$666
v_col:	dc.l $0182000d,$01840666,$01ba0888,$01bc0666,$01be0444
	dc.l $01001600 ;,$1001fffe
	; blitter in copper
	dc.w	$0101,0	; bplcon0+1
	dc.w	$40,$0100		; bltcon0
	dc.w	$42,0			; bltcon1
	dc.w	$44,$ffff,$46,$ffff	; bltAfwm/bltAlwm
cbltC	dc.w	$48,$6			; bltCpth $6
cbltB	dc.w	$4c,$6			; bltBpth $6
cbltA	dc.w	$50,$6			; bltApth $6
cbltD	dc.w	$54,$6,$56,$5c80	; bltDpt -> $65c80 sinscreen+$c80
	dc.w	$60,$0026,$62,$0028,$64,$0028,$66,0	; bltmod C/B/A/D
	dc.w	$0058,$1c14		; bltsize
	dc.w	$0096,$8400 ;clear sinus screen
	dc.w $0101,0,$96,$400,$66,$0026,$40,$0be2

cop:	ds.l 7*160	; blitter with copper sinuscroll

	dc.l $01010000,$009c8010 ;$01800060
	dc.l $7a01fffe,$01003600
varit:	ds.l 112*2
	dc.l $eb01fffe,$01002600
	dc.l $ee01fff3,$01920dc5,$ef01fffe,$01920b93
	dc.l $f001fffe,$01920a62,$f101fffe,$01920841
	dc.l $f201fffe,$01920620
	dc.l $f405fffe,$01000000,$f437fffe
	dc.l $01800101,$01800202,$01800303,$01800404,$01800505
	dc.l $01800606,$01800707,$01800808,$01800909,$01800a0a
	dc.l $01800b0b,$01800c0c,$01800d0d,$01800e0e,$01800f0f
	dc.l $f4abfffe
	dc.l $01800f0f,$01800e0e,$01800d0d,$01800c0c,$01800b0b
	dc.l $01800a0a,$01800909,$01800808,$01800707,$01800606
	dc.l $01800505,$01800404,$01800303,$01800202,$01800101
	dc.l $f507fffe,$01800000,$ffdffffe;,$0607fffe,$018000f0
	;dc.l $0701fffe,$01800000,$fffffffe
	dc.l $fffffffe
;--------------------------------------------------
font_width:	dc.b	1,1,0,0,0,0,0,1,1,1,0,0,1,1,1,0,0,1,0,0,0
		dc.b	0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1
		dc.b	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		dc.b	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		dc.b	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		even

sin_taulu:
	dc.w  25600, 25640, 25680, 25720, 25760, 25800, 25880, 25920
	dc.w  25960, 26000, 26040, 26080, 26120, 26200, 26240, 26280
	dc.w  26320, 26360, 26400, 26440, 26480, 26520, 26560, 26600
	dc.w  26640, 26680, 26720, 26760, 26800, 26840, 26880, 26920
	dc.w  26920, 26960, 27000, 27040, 27080, 27080, 27120, 27160
	dc.w  27160, 27200, 27240, 27240, 27280, 27280, 27320, 27320
	dc.w  27360, 27360, 27400, 27400, 27400, 27440, 27440, 27440
	dc.w  27480, 27480, 27480, 27480, 27480, 27480, 27480, 27480
	dc.w  27480, 27480, 27480, 27480, 27480, 27480, 27480, 27480
	dc.w  27480, 27440, 27440, 27440, 27400, 27400, 27400, 27360
	dc.w  27360, 27320, 27320, 27280, 27280, 27240, 27240, 27200
	dc.w  27160, 27160, 27120, 27080, 27080, 27040, 27000, 26960
	dc.w  26960, 26920, 26880, 26840, 26800, 26760, 26720, 26680
	dc.w  26640, 26600, 26560, 26520, 26480, 26440, 26400, 26360
	dc.w  26320, 26280, 26240, 26200, 26160, 26080, 26040, 26000
	dc.w  25960, 25920, 25880, 25840, 25760, 25720, 25680, 25640
	dc.w  25600, 25560, 25480, 25440, 25400, 25360, 25320, 25280
	dc.w  25200, 25160, 25120, 25080, 25040, 25000, 24960, 24880
	dc.w  24840, 24800, 24760, 24720, 24680, 24640, 24600, 24560
	dc.w  24520, 24480, 24440, 24400, 24360, 24320, 24280, 24280
	dc.w  24240, 24200, 24160, 24120, 24120, 24080, 24040, 24000
	dc.w  24000, 23960, 23920, 23920, 23880, 23880, 23840, 23840
	dc.w  23800, 23800, 23760, 23760, 23760, 23720, 23720, 23720
	dc.w  23680, 23680, 23680, 23680, 23680, 23680, 23680, 23680
	dc.w  23680, 23680, 23680, 23680, 23680, 23680, 23680, 23680
	dc.w  23680, 23720, 23720, 23720, 23720, 23760, 23760, 23800
	dc.w  23800, 23800, 23840, 23840, 23880, 23920, 23920, 23960
	dc.w  23960, 24000, 24040, 24040, 24080, 24120, 24160, 24160
	dc.w  24200, 24240, 24280, 24320, 24360, 24400, 24440, 24480
	dc.w  24520, 24560, 24560, 24640, 24680, 24720, 24760, 24800
	dc.w  24840, 24880, 24920, 24960, 25000, 25040, 25080, 25160
	dc.w  25200, 25240, 25280, 25320, 25360, 25440, 25480, 25520

sin_table:	incbin	"sin1"
		ds.w 90
cos_table:	incbin	"cos1"
		ds.w 360

mx:	dc.w 0
my:	dc.w 0
mz:	dc.w -256
zoom:	dc.w 165
texture:	dc.w $ffff
screenprior:	dc.b 0
ready:		dc.b 0
allowed:	dc.b 0
		even

axis:		dc.w 0		;x
		dc.w 0		;y
		dc.w 0		;z
		dc.w 28-1 	;number of lines (pixels/2)-1
		dc.l object4	;address of object information

colorhelp:	dc.w	$0f00,$0010,$0ff0,$0100,$00f0,$0001
		dc.w	$00ff,$0010,$000f,$0100,$0f0f,$0001
		dc.w	$0f00,$0010,$0ff0,$0100
colortest:	dc.b	$0
colorcount:	dc.b	$0
logo:	dc.l %00100001100110010101110001000000
	dc.l %11111010001001010101000111110000
	dc.l %01110010001111010101100011100000
	dc.l %01010010001001010101000010100000
	dc.l %00001001101001001001110100000000

object4:
	dc.w 38
	dc.w -140,-30,0, -120,-30,0, -120,0,0, -140,0,0, -140,30,0;p
	dc.w -120,30,0, -105,-30,0, -90,30,0;a
	dc.w -90,-30,0, -60,-30,0, -60,0,0, -80,0,0, -80,30,0
	dc.w -70,0,0, -60,30,0;r
	dc.w -50,30,0, -30,-30,0, -20,30,0;a
	dc.w -10,30,0, -10,-30,0, 10,30,0, 10,-30,0;n
	dc.w 20,-30,0, 40,-30,0, 40,30,0, 20,30,0	;o
	dc.w 50,-30,0, 50,30,0;i
	dc.w 60,30,0, 60,-30,0, 75,0,0, 90,-30,0, 90,30,0;m
	dc.w 100,-30,0, 100,30,0;i
	dc.w 110,30,0, 125,-30,0, 140,30,0

	dc.w 28-1
	dc.w 0,1, 1,2, 2,3, 3,4			;p
	dc.w 5,6, 6,7				;a
	dc.w 8,9, 9,10, 10,11, 11,12, 13,14	;r
	dc.w 15,16, 16,17			;a
	dc.w 18,19, 19,20, 20,21		;n
	dc.w 22,23, 23,24, 24,25, 25,22		;o
	dc.w 26,27				;i
	dc.w 28,29, 29,30, 30,31, 31,32		;m
	dc.w 33,34				;i
	dc.w 35,36, 36,37

font_graf:	incbin	"parafont.raw"		;dcb.b 1888,0

text:	dc.b "**PARANOIMIA**       **PARANOIMIA**      **PARANOIMIA"
	DC.B "**      **PARANOIMIA**              THIS IS  *CAVE* "
	DC.B " PRESENTING A NEW INTRO FOR  *PARANOIMIA*           "
	DC.B "CODED BY  MR. SPIV     pd      HOPE THIS WORKS IN ALL "
	DC.B "AMIGAS....   I MEAN THE INTRO HAD SOME VERY STRANGE "
	DC.B "WORKING PROBLEMS IN A2000... NOW THIS SHOULD (??) "
	DC.B "WORK FINE!!!    I'M NOT SURE DOES THIS WORK IN NTSC "
	DC.B "AMIGAS BUT I TRIED....    I HOPE YOU LIKE THIS ONE... "
	DC.B "                r",0
	even
; ----------------- Custom AST module player reloc
;		org	$4195e
module		incbin	"ast_music"

	section	buffer,bss_c

	ds.b	65536	;64K boundary for blitter in copper instructions
buffer:				; $60000
screen1		ds.b	$2800	; $60000
screen1b	ds.b	$2800	; $62800
sinscreen	ds.b	$b000	; $65000
pixbase: ; +$10000 -> $70000
	ds.w	40*2*2 ; coord stx,sty...

;>extern "df1:loader/load_font",font_graf
;>extern "df1:sin1",sin_table
;>extern "df1:cos1",cos_table
;>extern "ast_music",$4195e,$7ca2

