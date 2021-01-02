*>--------------------------------------------------<*
*
* FutureComposer RIPPER for Vampire's 'Universal Ripper'
*
* ...made by Vampire (of course!)
* just assemble and save as executable
*

saveadr		=  0
lenght		=  4
datadr		=  8 
datlen		= 12
type		= 16
prefix1ptr	= 18
prefix2ptr	= 22
suffix1ptr	= 26
suffix2ptr	= 30
message		= 34
savename	= 38
namelenght	= 42
datnamadr	= 44
callflags	= 48
fromadr		= 50
toadr		= 54
flashreg	= 58
memtableptr	= 62
memtypes	= 78
newname		= 92
dosbase		= 66
;gfxbase		= 70
savecodeptr	=112
PlayCIAA	=116

*****************************************************
**>> FC 1.0-1.3 REPLAY  (FROM DELITRACKER) <<********
*****************************************************

** by SuperSero of Superions **

L_000000a8	dc.w	$40	; main
L_000000aa	dc.w	$40	; left
L_000000ac	dc.w	$40	; right
stop:
	clr.w onoff
	clr.l $dff0a6
	clr.l $dff0b6
	clr.l $dff0c6
	clr.l $dff0d6
	move.w #$000f,$dff096
	bclr #1,$bfe001
	rts

init		LEA	$0064(A0),A1
		MOVE.L	A1,L_000009A6
		MOVEA.L	A0,A1
		ADDA.L	8(A0),A1
		MOVE.L	A1,L_000009AA
		MOVEA.L	A0,A1
		ADDA.L	$0010(A0),A1
		MOVE.L	A1,L_000009AE
		MOVEA.L	A0,A1
		ADDA.L	$0018(A0),A1
		MOVE.L	A1,L_000009B2
		MOVE.L	4(A0),D0
		DIVU	#$000D,D0
		LEA	$0028(A0),A1
		LEA	L_00000A6A(PC),A2
		MOVEQ	#9,D1
L_00000192	MOVE.W	(A1)+,(A2)+
		MOVE.L	(A1)+,(A2)+
		ADDQ.W	#4,A2
		DBRA	D1,L_00000192
		MOVEQ	#0,D2
		MOVE.L	A0,D1
		ADD.L	$0020(A0),D1
		SUBI.L	#L_00000CA0,D1
		LEA	L_00000A66(PC),A0
		MOVE.L	D1,(A0)+
		MOVEQ	#8,D3
L_000001B2	MOVE.W	(A0),D2
		ADD.L	D2,D1
		ADD.L	D2,D1
		ADDQ.W	#6,A0
		MOVE.L	D1,(A0)+
		DBRA	D3,L_000001B2
		MOVEA.L	L_000009A6(PC),A0
		MOVEQ	#0,D2
		MOVE.B	$000C(A0),D2
		BNE.S	L_000001D0
		MOVE.B	#3,D2
L_000001D0	MOVE.W	D2,L_00000860
		MOVE.W	D2,L_00000862
		CLR.W	L_0000086A
		MOVE.W	#$000F,$00DFF096.L
		MOVE.W	#$0780,$00DFF09A.L
		MOVEQ	#0,D7
		MULU	#$000D,D0
		MOVEQ	#3,D6
		LEA	L_0000086E(PC),A0
		LEA	L_000009B6(PC),A1
		LEA	L_00000996(PC),A2
L_00000206	MOVE.L	A1,$000A(A0)
		MOVE.L	A1,$0012(A0)
		CLR.L	$000E(A0)
		CLR.B	$002D(A0)
		CLR.B	$002F(A0)
		CLR.W	8(A0)
		CLR.L	$0030(A0)
		MOVE.B	#1,$0017(A0)
		MOVE.B	#1,$0018(A0)
		CLR.B	$0019(A0)
		CLR.L	$001A(A0)
		CLR.W	$001E(A0)
		MOVEQ	#0,D3
		MOVE.W	(A2)+,D1
		MOVE.W	(A2)+,D3
		DIVU	#3,D3
		MOVE.B	D3,$0020(A0)
		MULU	#3,D3
		ANDI.L	#$000000FF,D3
		ANDI.L	#$000000FF,D1
		ADDI.L	#$00DFF0A0,D1
		MOVEA.L	D1,A6
		MOVE.L	#$00000000,(A6)
		MOVE.W	#$0100,4(A6)
		MOVE.W	#0,6(A6)
		MOVE.W	#0,8(A6)
		MOVE.L	D1,$003C(A0)
		CLR.W	$0040(A0)
		MOVE.L	L_000009A6(PC),(A0)
		MOVE.L	L_000009A6(PC),$0034(A0)
		ADD.L	D0,$0034(A0)
		ADD.L	D3,$0034(A0)
		ADD.L	D7,(A0)
		ADD.L	D3,(A0)
		MOVE.W	#$000D,6(A0)
		MOVEA.L	(A0),A3
		MOVE.B	(A3),D1
		ANDI.L	#$000000FF,D1
		LSL.W	#6,D1
		MOVEA.L	L_000009AA(PC),A4
		ADDA.W	D1,A4
		MOVE.L	A4,$0022(A0)
		CLR.L	$0026(A0)
		MOVE.B	#1,$0021(A0)
		MOVE.B	#2,$002A(A0)
		MOVE.B	1(A3),$002C(A0)
		MOVE.B	2(A3),$0016(A0)
		CLR.B	$002B(A0)
		CLR.B	$002D(A0)
		CLR.W	$0038(A0)
		ADDA.W	#$004A,A0
		DBRA	D6,L_00000206
		RTS	
play		LEA	L_00000850(PC),A6
		SUBQ.W	#1,L_00000860
		BNE.S	L_00000322
		MOVE.W	L_00000862(PC),L_00000860
		LEA	L_0000086E(PC),A0
		BSR	L_000004B6
		LEA	L_000008B8(PC),A0
		BSR	L_000004B6
		LEA	L_00000902(PC),A0
		BSR	L_000004B6
		LEA	L_0000094C(PC),A0
		BSR	L_000004B6
L_00000322	CLR.W	L_0000086A
		LEA	L_0000086E(PC),A0
		BSR	L_000005D4
		MOVE.W	D0,(A6)+
		MOVE.W	D1,(A6)+
		LEA	L_000008B8(PC),A0
		BSR	L_000005D4
		MOVE.W	D0,(A6)+
		MOVE.W	D1,(A6)+
		LEA	L_00000902(PC),A0
		BSR	L_000005D4
		MOVE.W	D0,(A6)+
		MOVE.W	D1,(A6)+
		LEA	L_0000094C(PC),A0
		BSR	L_000005D4
		MOVE.W	D0,(A6)+
		MOVE.W	D1,(A6)+
		LEA	L_00000850(PC),A6
		MOVE.W	L_0000086A(PC),D0
		ORI.W	#-$8000,D0
		MOVE.W	D0,-(A7)
		MOVEQ	#0,D1
		MOVE.L	L_000008B2(PC),D2
		MOVE.W	L_000008AE(PC),D1
		ADD.L	D1,D2
		MOVE.L	L_000008FC(PC),D3
		MOVE.W	L_000008F8(PC),D1
		ADD.L	D1,D3
		MOVE.L	L_00000946(PC),D4
		MOVE.W	L_00000942(PC),D1
		ADD.L	D1,D4
		MOVE.L	L_00000990(PC),D5
		MOVE.W	L_0000098C(PC),D1
		ADD.L	D1,D5
		MOVE.W	L_000008B0(PC),D0
		MOVE.W	L_000008FA(PC),D1
		MOVE.W	L_00000944(PC),D6
		MOVE.W	L_0000098E(PC),D7
		MOVE.W	(A7)+,$00DFF096.L
		LEA	L_0000086E(PC),A0
		TST.W	$0048(A0)
		BEQ	L_000003CE
		SUBQ.W	#1,$0048(A0)
		CMPI.W	#1,$0048(A0)
		BNE.S	L_000003CE
		CLR.W	$0048(A0)
		MOVE.L	D2,$00DFF0A0.L
		MOVE.W	D0,$00DFF0A4.L
L_000003CE	LEA	L_000008B8(PC),A0
		TST.W	$0048(A0)
		BEQ.S	L_000003F4
		SUBQ.W	#1,$0048(A0)
		CMPI.W	#1,$0048(A0)
		BNE.S	L_000003F4
		CLR.W	$0048(A0)
		MOVE.L	D3,$00DFF0B0.L
		MOVE.W	D1,$00DFF0B4.L
L_000003F4	LEA	L_00000902(PC),A0
		TST.W	$0048(A0)
		BEQ.S	L_0000041A
		SUBQ.W	#1,$0048(A0)
		CMPI.W	#1,$0048(A0)
		BNE.S	L_0000041A
		CLR.W	$0048(A0)
		MOVE.L	D4,$00DFF0C0.L
		MOVE.W	D6,$00DFF0C4.L
L_0000041A	LEA	L_0000094C(PC),A0
		TST.W	$0048(A0)
		BEQ.S	L_00000440
		SUBQ.W	#1,$0048(A0)
		CMPI.W	#1,$0048(A0)
		BNE.S	L_00000440
		CLR.W	$0048(A0)
		MOVE.L	D5,$00DFF0D0.L
		MOVE.W	D7,$00DFF0D4.L
L_00000440	LEA	$00DFF0A6.L,A5
		MOVE.W	(A6)+,(A5)
		MOVEQ	#0,D0
		MOVE.W	(A6)+,D0
		MULU	L_000000A8,D0
		MULU	L_000000AA,D0
		DIVU	#$0F81,D0
		MOVE.W	D0,2(A5)
		MOVE.W	(A6)+,$0010(A5)
		MOVEQ	#0,D0
		MOVE.W	(A6)+,D0
		MULU	L_000000A8,D0
		MULU	L_000000AC,D0
		DIVU	#$0F81,D0
		MOVE.W	D0,$0012(A5)
		MOVE.W	(A6)+,$0020(A5)
		MOVEQ	#0,D0
		MOVE.W	(A6)+,D0
		MULU	L_000000A8,D0
		MULU	L_000000AC,D0
		DIVU	#$0F81,D0
		MOVE.W	D0,$0022(A5)
		MOVE.W	(A6)+,$0030(A5)
		MOVEQ	#0,D0
		MOVE.W	(A6)+,D0
		MULU	L_000000A8,D0
		MULU	L_000000AA,D0
		DIVU	#$0F81,D0
		MOVE.W	D0,$0032(A5)
		RTS	
L_000004B6	MOVEQ	#0,D5
		MOVEA.L	$0022(A0),A1
		ADDA.W	$0028(A0),A1
		CMPI.W	#$0040,$0028(A0)
		BNE	L_0000052A
		MOVEA.L	(A0),A2
		ADDA.W	6(A0),A2
		CMPA.L	$0034(A0),A2
		BNE.S	L_000004DC
		MOVE.W	D5,6(A0)
		MOVEA.L	(A0),A2
L_000004DC	MOVEQ	#0,D1
		ADDQ.B	#1,L_0000086C
		CMPI.B	#4,L_0000086C
		BNE.S	L_00000506
		MOVE.B	D5,L_0000086C
		MOVE.B	-1(A1),D1
		BEQ.S	L_00000506
		MOVE.W	D1,L_00000860
		MOVE.W	D1,L_00000862
L_00000506	MOVE.B	(A2),D1
		MOVE.B	1(A2),$002C(A0)
		MOVE.B	2(A2),$0016(A0)
		MOVE.W	D5,$0028(A0)
		LSL.W	#6,D1
		ADD.L	L_000009AA(PC),D1
		MOVE.L	D1,$0022(A0)
		ADDI.W	#$000D,6(A0)
		MOVEA.L	D1,A1
L_0000052A	MOVE.B	1(A1),D1
		MOVE.B	(A1)+,D0
		BNE.S	L_0000053A
		ANDI.W	#$00C0,D1
		BEQ.S	L_00000552
		BRA.S	L_0000053E
L_0000053A	MOVE.W	D5,$0038(A0)
L_0000053E	MOVE.B	D5,$002F(A0)
		MOVE.B	(A1),$001F(A0)
		BTST	#7,D1
		BEQ.S	L_00000552
		MOVE.B	2(A1),$002F(A0)
L_00000552	ANDI.W	#$007F,D0
		BEQ	L_000005CE
		MOVE.B	D0,8(A0)
		MOVE.B	(A1),9(A0)
		MOVE.B	$0020(A0),D2
		MOVEQ	#0,D3
		BSET	D2,D3
		OR.W	D3,L_0000086A
		MOVE.W	D3,$00DFF096.L
		MOVE.B	(A1),D1
		ANDI.W	#$003F,D1
		ADD.B	$0016(A0),D1
		MOVEA.L	L_000009B2(PC),A2
		LSL.W	#6,D1
		ADDA.W	D1,A2
		MOVE.W	D5,$0010(A0)
		MOVE.B	(A2),$0017(A0)
		MOVE.B	(A2)+,$0018(A0)
		MOVE.B	(A2)+,D1
		ANDI.W	#$00FF,D1
		MOVE.B	(A2)+,$001B(A0)
		MOVE.B	#$0040,$002E(A0)
		MOVE.B	(A2)+,D0
		MOVE.B	D0,$001C(A0)
		MOVE.B	D0,$001D(A0)
		MOVE.B	(A2)+,$001E(A0)
		MOVE.L	A2,$000A(A0)
		MOVEA.L	L_000009AE(PC),A2
		LSL.W	#6,D1
		ADDA.W	D1,A2
		MOVE.L	A2,$0012(A0)
		MOVE.W	D5,$0032(A0)
		MOVE.B	D5,$001A(A0)
		MOVE.B	D5,$0019(A0)
L_000005CE	ADDQ.W	#2,$0028(A0)
		RTS	
L_000005D4	MOVEQ	#0,D7
L_000005D6	TST.B	$001A(A0)
		BEQ.S	L_000005E4
		SUBQ.B	#1,$001A(A0)
		BRA	L_00000732
L_000005E4	MOVEA.L	$0012(A0),A1
		ADDA.W	$0032(A0),A1
L_000005EC	CMPI.B	#-$001F,(A1)
		BEQ	L_00000732
		CMPI.B	#-$0020,(A1)
		BNE.S	L_0000060C
		MOVE.B	1(A1),D0
		ANDI.W	#$003F,D0
		MOVE.W	D0,$0032(A0)
		MOVEA.L	$0012(A0),A1
		ADDA.W	D0,A1
L_0000060C	CMPI.B	#-$001E,(A1)
		BNE.S	L_00000682
		MOVEQ	#0,D0
		MOVEQ	#0,D1
		MOVE.B	$0020(A0),D1
		BSET	D1,D0
		OR.W	D0,L_0000086A
		MOVE.W	D0,$00DFF096.L
		MOVE.B	1(A1),D0
		ANDI.W	#$00FF,D0
		LEA	L_00000A66(PC),A4
		ADD.W	D0,D0
		MOVE.W	D0,D1
		ADD.W	D1,D1
		ADD.W	D1,D1
		ADD.W	D1,D0
		ADDA.W	D0,A4
		MOVEA.L	$003C(A0),A3
		MOVE.L	(A4),D1
		ADDI.L	#L_00000CA0,D1
		MOVE.L	D1,(A3)
		MOVE.L	D1,$0044(A0)
		MOVE.W	4(A4),4(A3)
		MOVE.L	6(A4),$0040(A0)
		SWAP	D1
		MOVE.W	#3,$0048(A0)
		TST.W	D1
		BNE.S	L_00000670
		MOVE.W	#2,$0048(A0)
L_00000670	CLR.W	$0010(A0)
		MOVE.B	#1,$0017(A0)
		ADDQ.W	#2,$0032(A0)
		BRA	L_00000722
L_00000682	CMPI.B	#-$001C,(A1)
		BNE.S	L_000006D6
		MOVE.B	1(A1),D0
		ANDI.W	#$00FF,D0
		LEA	L_00000A66(PC),A4
		ADD.W	D0,D0
		MOVE.W	D0,D1
		ADD.W	D1,D1
		ADD.W	D1,D1
		ADD.W	D1,D0
		ADDA.W	D0,A4
		MOVEA.L	$003C(A0),A3
		MOVE.L	(A4),D1
		ADDI.L	#L_00000CA0,D1
		MOVE.L	D1,(A3)
		MOVE.L	D1,$0044(A0)
		MOVE.W	4(A4),4(A3)
		MOVE.L	6(A4),$0040(A0)
		SWAP	D1
		MOVE.W	#3,$0048(A0)
		TST.W	D1
		BNE.S	L_000006D0
		MOVE.W	#2,$0048(A0)
L_000006D0	ADDQ.W	#2,$0032(A0)
		BRA.S	L_00000722
L_000006D6	CMPI.B	#-$0019,(A1)
		BNE.S	L_000006F8
		MOVE.B	1(A1),D0
		ANDI.W	#$00FF,D0
		LSL.W	#6,D0
		MOVEA.L	L_000009AE(PC),A1
		ADDA.W	D0,A1
		MOVE.L	A1,$0012(A0)
		MOVE.W	D7,$0032(A0)
		BRA	L_000005EC
L_000006F8	CMPI.B	#-$0018,(A1)
		BNE.S	L_0000070C
		MOVE.B	1(A1),$001A(A0)
		ADDQ.W	#2,$0032(A0)
		BRA	L_000005D6
L_0000070C	CMPI.B	#-$001D,(A1)
		BNE.S	L_00000722
		ADDQ.W	#3,$0032(A0)
		MOVE.B	1(A1),$001B(A0)
		MOVE.B	2(A1),$001C(A0)
L_00000722	MOVEA.L	$0012(A0),A1
		ADDA.W	$0032(A0),A1
		MOVE.B	(A1),$002B(A0)
		ADDQ.W	#1,$0032(A0)
L_00000732	TST.B	$0019(A0)
		BEQ.S	L_0000073E
		SUBQ.B	#1,$0019(A0)
		BRA.S	L_0000078C
L_0000073E	SUBQ.B	#1,$0017(A0)
		BNE.S	L_0000078C
		MOVE.B	$0018(A0),$0017(A0)
L_0000074A	MOVEA.L	$000A(A0),A1
		ADDA.W	$0010(A0),A1
		MOVE.B	(A1),D0
		CMP.B	#-$0018,D0
		BNE.S	L_00000766
		ADDQ.W	#2,$0010(A0)
		MOVE.B	1(A1),$0019(A0)
		BRA.S	L_00000732
L_00000766	CMP.B	#-$001F,D0
		BEQ.S	L_0000078C
		CMP.B	#-$0020,D0
		BNE.S	L_00000784
		MOVE.B	1(A1),D0
		ANDI.L	#$0000003F,D0
		SUBQ.B	#5,D0
		MOVE.W	D0,$0010(A0)
		BRA.S	L_0000074A
L_00000784	MOVE.B	(A1),$002D(A0)
		ADDQ.W	#1,$0010(A0)
L_0000078C	MOVE.B	$002B(A0),D0
		BMI.S	L_0000079A
		ADD.B	8(A0),D0
		ADD.B	$002C(A0),D0
L_0000079A	ANDI.W	#$007F,D0
		LEA	L_000009BE(PC),A1
		ADD.W	D0,D0
		MOVE.W	D0,D1
		ADDA.W	D0,A1
		MOVE.W	(A1),D0
		MOVE.B	$002E(A0),D7
		TST.B	$001E(A0)
		BEQ.S	L_000007BA
		SUBQ.B	#1,$001E(A0)
		BRA.S	L_00000810
L_000007BA	MOVE.B	D1,D5
		MOVE.B	$001C(A0),D4
		ADD.B	D4,D4
		MOVE.B	$001D(A0),D1
		TST.B	D7
		BPL.S	L_000007D0
		BTST	#0,D7
		BNE.S	L_000007F6
L_000007D0	BTST	#5,D7
		BNE.S	L_000007E4
		SUB.B	$001B(A0),D1
		BCC.S	L_000007F2
		BSET	#5,D7
		MOVEQ	#0,D1
		BRA.S	L_000007F2
L_000007E4	ADD.B	$001B(A0),D1
		CMP.B	D4,D1
		BCS.S	L_000007F2
		BCLR	#5,D7
		MOVE.B	D4,D1
L_000007F2	MOVE.B	D1,$001D(A0)
L_000007F6	LSR.B	#1,D4
		SUB.B	D4,D1
		BCC.S	L_00000800
		SUBI.W	#$0100,D1
L_00000800	ADDI.B	#-$0060,D5
		BCS.S	L_0000080E
L_00000806	ADD.W	D1,D1
		ADDI.B	#$0018,D5
		BCC.S	L_00000806
L_0000080E	ADD.W	D1,D0
L_00000810	EORI.B	#1,D7
		MOVE.B	D7,$002E(A0)
		MOVEQ	#0,D1
		MOVE.B	$002F(A0),D1
		BEQ.S	L_00000830
		CMP.B	#$001F,D1
		BLS.S	L_0000082C
		ANDI.W	#$001F,D1
		NEG.W	D1
L_0000082C	SUB.W	D1,$0038(A0)
L_00000830	ADD.W	$0038(A0),D0
		CMP.W	#$0070,D0
		BHI.S	L_0000083E
		MOVE.W	#$0071,D0
L_0000083E	CMP.W	#$06B0,D0
		BLS.S	L_00000848
		MOVE.W	#$06B0,D0
L_00000848	MOVEQ	#0,D1
		MOVE.B	$002D(A0),D1
		RTS	
L_00000850	DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
L_00000860	DC.W	0
L_00000862	DC.W	0
onoff		DC.L	0
		DC.W	0
L_0000086A	DC.W	0
L_0000086C	DC.W	0
L_0000086E	DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
L_000008AE	DC.W	0
L_000008B0	DC.W	0
L_000008B2	DC.L	0
		DC.W	0
L_000008B8	DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
L_000008F8	DC.W	0
L_000008FA	DC.W	0
L_000008FC	DC.L	0
		DC.W	0
L_00000902	DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
L_00000942	DC.W	0
L_00000944	DC.W	0
L_00000946	DC.L	0
		DC.W	0
L_0000094C	DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
		DC.L	0
L_0000098C	DC.W	0
L_0000098E	DC.W	0
L_00000990	DC.L	0
		DC.W	0
L_00000996	DC.L	0
		DC.L	$100003
		DC.L	$200006
		DC.L	$300009
L_000009A6	DC.L	0
L_000009AA	DC.L	0
L_000009AE	DC.L	0
L_000009B2	DC.L	0
L_000009B6	DC.L	$1000000
		DC.L	$E1
L_000009BE	DC.L	$6B00650
		DC.L	$5F405A0
		DC.L	$54C0500
		DC.L	$4B80474
		DC.L	$43403F8
		DC.L	$3C0038A
		DC.L	$3580328
		DC.L	$2FA02D0
		DC.L	$2A60280
		DC.L	$25C023A
		DC.L	$21A01FC
		DC.L	$1E001C5
		DC.L	$1AC0194
		DC.L	$17D0168
		DC.L	$1530140
		DC.L	$12E011D
		DC.L	$10D00FE
		DC.L	$F000E2
		DC.L	$D600CA
		DC.L	$BE00B4
		DC.L	$AA00A0
		DC.L	$97008F
		DC.L	$87007F
		DC.L	$780071
		DC.L	$710071
		DC.L	$710071
		DC.L	$710071
		DC.L	$710071
		DC.L	$710071
		DC.L	$710071
		DC.L	$D600CA0
		DC.L	$BE80B40
		DC.L	$A980A00
		DC.L	$97008E8
		DC.L	$86807F0
		DC.L	$7800714
		DC.L	$1AC01940
		DC.L	$17D01680
		DC.L	$15301400
		DC.L	$12E011D0
		DC.L	$10D00FE0
		DC.L	$F000E28
L_00000A66	DC.L	0
L_00000A6A	DC.L	0
		DC.L	$10000
		DC.L	0
		DC.L	1
		DC.L	0
		DC.L	0
		DC.L	$10000
		DC.L	0
		DC.L	1
		DC.L	0
		DC.L	0
		DC.L	$10000
		DC.L	0
		DC.L	1
		DC.L	0
		DC.L	0
		DC.L	$10000
		DC.L	0
		DC.L	1
		DC.L	0
		DC.L	0
		DC.L	$10000
		DC.L	0
		DC.L	1
		DC.L	0
		DC.L	$100000
		DC.L	$100000
		DC.L	$200010
		DC.L	$10
		DC.L	$40
		DC.L	$100000
		DC.L	$100000
		DC.L	$600010
		DC.L	$10
		DC.L	$80
		DC.L	$100000
		DC.L	$100000
		DC.L	$A00010
		DC.L	$10
		DC.L	$C0
		DC.L	$100000
		DC.L	$100000
		DC.L	$E00010
		DC.L	$10
		DC.L	$100
		DC.L	$100000
		DC.L	$100000
		DC.L	$1200010
		DC.L	$10
		DC.L	$140
		DC.L	$100000
		DC.L	$100000
		DC.L	$1600010
		DC.L	$10
		DC.L	$180
		DC.L	$100000
		DC.L	$100000
		DC.L	$1A00010
		DC.L	$10
		DC.L	$1C0
		DC.L	$100000
		DC.L	$100000
		DC.L	$1E00010
		DC.L	$10
		DC.L	$200
		DC.L	$100000
		DC.L	$100000
		DC.L	$2200010
		DC.L	$10
		DC.L	$240
		DC.L	$100000
		DC.L	$100000
		DC.L	$2600010
		DC.L	$10
		DC.L	$280
		DC.L	$100000
		DC.L	$100000
		DC.L	$2A00010
		DC.L	$10
		DC.L	$2C0
		DC.L	$100000
		DC.L	$100000
		DC.L	$2E00010
		DC.L	$10
		DC.L	$300
		DC.L	$100000
		DC.L	$100000
		DC.L	$3200010
		DC.L	$10
		DC.L	$340
		DC.L	$100000
		DC.L	$100000
		DC.L	$3600010
		DC.L	$10
		DC.L	$380
		DC.L	$100000
		DC.L	$100000
		DC.L	$3A00010
		DC.L	$10
		DC.L	$3C0
		DC.L	$100000
		DC.L	$100000
		DC.L	$3E00010
		DC.L	$10
		DC.L	$400
		DC.L	$80000
		DC.L	$80000
		DC.L	$4100008
		DC.L	8
		DC.L	$420
		DC.L	$80000
		DC.L	$80000
		DC.L	$4300008
		DC.L	8
		DC.L	$440
		DC.L	$80000
		DC.L	$80000
		DC.L	$4500008
		DC.L	8
		DC.L	$460
		DC.L	$80000
		DC.L	$80000
		DC.L	$4700008
		DC.L	8
		DC.L	$480
		DC.L	$100000
		DC.L	$100000
		DC.L	$4A00008
		DC.L	8
		DC.L	$4B0
		DC.L	$100000
		DC.L	$100000
		DC.L	$4D00010
		DC.L	$10
		DC.L	$4F0
		DC.L	$80000
		DC.L	$80000
		DC.L	$5000008
		DC.L	8
		DC.L	$510
		DC.L	$180000
		DC.W	$18

;	section "",data_c

L_00000CA0	DC.L	-$3F3F2F28
		DC.L	-$1F170F08
		DC.L	$F8F0E8
		DC.L	-$1F272F38
		DC.L	$3F372F27
		DC.L	$1F170F07
		DC.L	-$F8F0E9
		DC.L	$1F272F37
		DC.L	-$3F3F2F28
		DC.L	-$1F170F08
		DC.L	$F8F0E8
		DC.L	-$1F272F38
		DC.L	-$3FC8D0D9
		DC.L	$1F170F07
		DC.L	-$F8F0E9
		DC.L	$1F272F37
		DC.L	-$3F3F2F28
		DC.L	-$1F170F08
		DC.L	$F8F0E8
		DC.L	-$1F272F38
		DC.L	-$3F47D0D9
		DC.L	$1F170F07
		DC.L	-$F8F0E9
		DC.L	$1F272F37
		DC.L	-$3F3F2F28
		DC.L	-$1F170F08
		DC.L	$F8F0E8
		DC.L	-$1F272F38
		DC.L	-$3F474FD9
		DC.L	$1F170F07
		DC.L	-$F8F0E9
		DC.L	$1F272F37
		DC.L	-$3F3F2F28
		DC.L	-$1F170F08
		DC.L	$F8F0E8
		DC.L	-$1F272F38
		DC.L	-$3F474F58
		DC.L	$1F170F07
		DC.L	-$F8F0E9
		DC.L	$1F272F37
		DC.L	-$3F3F2F28
		DC.L	-$1F170F08
		DC.L	$F8F0E8
		DC.L	-$1F272F38
		DC.L	-$3F474F58
		DC.L	-$5FE8F0F9
		DC.L	-$F8F0E9
		DC.L	$1F272F37
		DC.L	-$3F3F2F28
		DC.L	-$1F170F08
		DC.L	$F8F0E8
		DC.L	-$1F272F38
		DC.L	-$3F474F58
		DC.L	-$5F67F0F9
		DC.L	-$F8F0E9
		DC.L	$1F272F37
		DC.L	-$3F3F2F28
		DC.L	-$1F170F08
		DC.L	$F8F0E8
		DC.L	-$1F272F38
		DC.L	-$3F474F58
		DC.L	-$5F676FF9
		DC.L	-$F8F0E9
		DC.L	$1F272F37
		DC.L	-$3F3F2F28
		DC.L	-$1F170F08
		DC.L	$F8F0E8
		DC.L	-$1F272F38
		DC.L	-$3F474F58
		DC.L	-$5F676F78
		DC.L	-$F8F0E9
		DC.L	$1F272F37
		DC.L	-$3F3F2F28
		DC.L	-$1F170F08
		DC.L	$F8F0E8
		DC.L	-$1F272F38
		DC.L	-$3F474F58
		DC.L	-$5F676F78
		DC.L	-$7FF8F0E9
		DC.L	$1F272F37
		DC.L	-$3F3F2F28
		DC.L	-$1F170F08
		DC.L	$F8F0E8
		DC.L	-$1F272F38
		DC.L	-$3F474F58
		DC.L	-$5F676F78
		DC.L	-$7F77F0E9
		DC.L	$1F272F37
		DC.L	-$3F3F2F28
		DC.L	-$1F170F08
		DC.L	$F8F0E8
		DC.L	-$1F272F38
		DC.L	-$3F474F58
		DC.L	-$5F676F78
		DC.L	-$7F776FE9
		DC.L	$1F272F37
		DC.L	-$3F3F2F28
		DC.L	-$1F170F08
		DC.L	$F8F0E8
		DC.L	-$1F272F38
		DC.L	-$3F474F58
		DC.L	-$5F676F78
		DC.L	-$7F776F68
		DC.L	$1F272F37
		DC.L	-$3F3F2F28
		DC.L	-$1F170F08
		DC.L	$F8F0E8
		DC.L	-$1F272F38
		DC.L	-$3F474F58
		DC.L	-$5F676F78
		DC.L	-$7F776F68
		DC.L	-$5FD8D0C9
		DC.L	-$3F3F2F28
		DC.L	-$1F170F08
		DC.L	$F8F0E8
		DC.L	-$1F272F38
		DC.L	-$3F474F58
		DC.L	-$5F676F78
		DC.L	-$7F776F68
		DC.L	-$5F57D0C9
		DC.L	-$3F3F2F28
		DC.L	-$1F170F08
		DC.L	$F8F0E8
		DC.L	-$1F272F38
		DC.L	-$3F474F58
		DC.L	-$5F676F78
		DC.L	-$7F776F68
		DC.L	-$5F574FC9
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	$7F7F7F7F
		DC.L	$7F7F7F7F
		DC.L	$7F7F7F7F
		DC.L	$7F7F7F7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E808081
		DC.L	$7F7F7F7F
		DC.L	$7F7F7F7F
		DC.L	$7F7F7F7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E8081
		DC.L	$7F7F7F7F
		DC.L	$7F7F7F7F
		DC.L	$7F7F7F7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E81
		DC.L	$7F7F7F7F
		DC.L	$7F7F7F7F
		DC.L	$7F7F7F7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	$7F7F7F7F
		DC.L	$7F7F7F7F
		DC.L	$7F7F7F7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E808081
		DC.L	$7F7F7F7F
		DC.L	$7F7F7F7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E8081
		DC.L	$7F7F7F7F
		DC.L	$7F7F7F7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E81
		DC.L	$7F7F7F7F
		DC.L	$7F7F7F7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	$7F7F7F7F
		DC.L	$7F7F7F7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E808081
		DC.L	$7F7F7F7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E8081
		DC.L	$7F7F7F7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E81
		DC.L	$7F7F7F7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	$7F7F7F7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E7E7E7F
		DC.L	-$7E808081
		DC.L	-$7F7F7F80
		DC.L	-$7F7F7F80
		DC.L	-$7F7F7F80
		DC.L	-$7F7F7F80
		DC.L	-$7F7F7F80
		DC.L	-$7F7F7F80
		DC.L	-$7F7F7F80
		DC.L	-$7F7F8081
		DC.L	-$7F7F7F80
		DC.L	-$7F7F7F80
		DC.L	-$7F7F7F80
		DC.L	-$7F7F7F80
		DC.L	-$7F7F7F80
		DC.L	-$7F7F7F80
		DC.L	-$7F7F7F80
		DC.L	-$7F7F7F81
		DC.L	-$7F7F7F80
		DC.L	-$7F7F7F80
		DC.L	$7F7F7F7F
		DC.L	$7F7F7F7F
		DC.L	-$7F7F7F80
		DC.L	-$7F7F7F81
		DC.L	$7F7F7F7F
		DC.L	$7F7F7F7F
		DC.L	-$7F7F7F80
		DC.L	-$7F7F8081
		DC.L	$7F7F7F7F
		DC.L	$7F7F7F7F
		DC.L	-$7F7F7F80
		DC.L	-$7F808081
		DC.L	$7F7F7F7F
		DC.L	$7F7F7F7F
		DC.L	-$7F7F7F80
		DC.L	$7F7F7F7F
		DC.L	$7F7F7F7F
		DC.L	$7F7F7F7F
		DC.L	-$7F7F7F81
		DC.L	$7F7F7F7F
		DC.L	$7F7F7F7F
		DC.L	$7F7F7F7F
		DC.L	-$7F7F8081
		DC.L	$7F7F7F7F
		DC.L	$7F7F7F7F
		DC.L	$7F7F7F7F
		DC.L	-$7F7F8081
		DC.L	$7F7F7F7F
		DC.L	$7F7F7F7F
		DC.L	$7F7F7F7F
		DC.L	-$7F7F6F68
		DC.L	-$5F574F48
		DC.L	-$3F372F28
		DC.L	-$1F170F08
		DC.L	$81018
		DC.L	$20283038
		DC.L	$40485058
		DC.L	$6068707F
		DC.L	-$7F7F5F50
		DC.L	-$3F2F1F10
		DC.L	$102030
		DC.L	$40506070
		DC.L	$4545797D
		DC.L	$7A777066
		DC.L	$6158534D
		DC.L	$2C201812
		DC.L	$4DBD3CD
		DC.L	-$39434A52
		DC.L	-$575C6267
		DC.L	-$6C717476
		DC.L	$4545797D
		DC.L	$7A777066
		DC.L	$5B4B4337
		DC.L	$2C201812
		DC.L	$4F8E8DB
		DC.L	-$30394150
		DC.L	-$575B6166
		DC.L	-$6A6B727D
		DC.L	$4060
		DC.L	$7F604020
		DC.L	$E0C0A0
		DC.L	-$7F5F3F20
		DC.L	$4060
		DC.L	$7F604020
		DC.L	$E0C0A0
		DC.L	-$7F5F3F20
		DC.L	-$7F7F6F68
		DC.L	-$5F574F48
		DC.L	-$3F372F28
		DC.L	-$1F170F08
		DC.L	$81018
		DC.L	$20283038
		DC.L	$40485058
		DC.L	$6068707F
		DC.L	-$7F7F5F50
		DC.L	-$3F2F1F10
		DC.L	$102030
		DC.L	$40506070



