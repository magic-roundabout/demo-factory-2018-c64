;
; DEMO FACTORY 2018
;

; Code and graphics T.M.R/Cosine
; Music by Odie/Cosine


; This source code is formatted for the ACME cross assembler from
; http://sourceforge.net/projects/acme-crossass/
; Compression is handled with Exomizer which can be downloaded at
; http://csdb.dk/release/?id=141402

; build.bat will call both to create an assembled file and then the
; crunched release version.


; Select an output filename
		!to "df_2018.prg",cbm


; Yank in binary data
		* = $1000
music		!binary "data/ninja_rabbits.prg",,2

		* = $2000
char_data	!binary "data/factory.chr"

		* = $2800
sprite_data	!binary "data/sprites.spr"

		* = $2b00
scrn_cols	!binary "data/factory.col"

		* = $2c00
scrn_data	!binary "data/factory.map"


; Constants
rstr1p		= $00
rstr2p		= $5a
rstr3p		= $a2
rstr4p		= $d2
rstr5p		= $f9


; Label assignments
rn		= $40
sync		= $41

convey_cnt	= $42
flip_flop_cnt	= $43
copy_cnt	= $44
arrow_cnt	= $45
cone_cnt	= $46

sparkle_cnt_1	= $47
sparkle_cnt_2	= $48
colour_cnt_1	= $49
pulse_cnt	= $4a
zap_1_cnt	= $4b
zap_2_cnt	= $4c
led_cnt		= $4d

scroll_col	= $4e
char_cnt	= $4f
char_buffer	= $50		; $08 bytes used

effect_work	= $58		; $18 bytes used
cos_at_1	= $70
cos_at_2	= $71




; Add a BASIC startline
		* = $0801
		!word entry-2
		!byte $00,$00,$9e
		!text "2066"
		!byte $00,$00,$00


; Entry point at $0812
		* = $0812
entry		sei

; Turn off ROMs and set up interrupts
		lda #$35
		sta $01

		lda #<nmi
		sta $fffa
		lda #>nmi
		sta $fffb

		lda #<int
		sta $fffe
		lda #>int
		sta $ffff

		lda #$7f
		sta $dc0d
		sta $dd0d

		lda $dc0d
		lda $dd0d

		lda #rstr1p
		sta $d012

		lda #$0b
		sta $d011
		lda #$01
		sta $d019
		sta $d01a

; Set up the screen
		ldx #$00
screen_init	lda scrn_data+$000,x
		sta $0400,x
		tay
		lda scrn_cols,y
		sta $d800,x

		lda scrn_data+$100,x
		sta $0500,x
		tay
		lda scrn_cols,y
		sta $d900,x

		lda scrn_data+$200,x
		sta $0600,x
		tay
		lda scrn_cols,y
		sta $da00,x

		lda scrn_data+$2e8,x
		sta $06e8,x
		tay
		lda scrn_cols,y
		sta $dae8,x

		inx
		bne screen_init

; Clear label space and set some specific labels
		ldx #$40
		lda #$00
nuke_zp		sta $00,x
		inx
		bne nuke_zp

		lda #$01
		sta rn

; Reset the scroller
		jsr reset

; Set initial sprite X positions for floppies
		jsr floppy_reset

; Set up the music
		lda #$01
		jsr music+$00

		cli

; Update various machine elements
main_loop	lda #$00
		sta sync
		cmp sync
		beq main_loop+$04

; Update the conveyor animation
		inc convey_cnt
		lda convey_cnt
		and #$03
		asl
		asl
		asl
		tay

		ldx #$00
convey_copy	lda char_data+$380,y
		sta char_data+$760,x
		lda char_data+$3a0,y
		sta char_data+$768,x
		lda char_data+$3c0,y
		sta char_data+$770,x
		lda char_data+$3e0,y
		sta char_data+$778,x
		iny
		inx
		cpx #$08
		bne convey_copy

; Update "VFX" effect area
		lda cos_at_1
		clc
		adc #$fe
		sta cos_at_1

		lda cos_at_2
		clc
		adc #$03
		sta cos_at_2

		ldx #$00
		ldy cos_at_1
effect_calc_1	lda effect_cos,y
		sta effect_work,x
		iny
		iny
		inx
		cpx #$18
		bne effect_calc_1

		ldx #$00
		ldy cos_at_2
effect_calc_2	lda effect_cos,y
		clc
		adc effect_work,x
		sta effect_work,x
		iny
		iny
		inx
		cpx #$18
		bne effect_calc_2

		ldx #$00
effect_render_1	lda effect_work,x
		and #$0f
		tay
		lda char_data+$288,y
		sta char_data+$780,x
		eor #$ff
		sta char_data+$798,x
		inx
		cpx #$18
		bne effect_render_1

; Update SFX cone
		ldx #$00
		txa
cone_clear	sta char_data+$518,x
		inx
		cpx #$48
		bne cone_clear

		inc cone_cnt
		lda cone_cnt
		lsr
		lsr
		and #$03
		tay
		lda char_data+$268,y
		sta char_data+$518,y
		sta char_data+$548,y

		lda char_data+$26c,y
		sta char_data+$51c,y
		sta char_data+$54c,y

		lda char_data+$278,y
		sta char_data+$520,y
		sta char_data+$558,y

		lda char_data+$27c,y
		sta char_data+$524,y
		sta char_data+$55c,y

		lda char_data+$270,y
		sta char_data+$550,y
		sta char_data+$554,y

		lda char_data+$24c,y
		sta char_data+$52c,y

		lda char_data+$250,y
		sta char_data+$530,y
		lda char_data+$254,y
		sta char_data+$534,y

		lda char_data+$258,y
		sta char_data+$538,y
		lda char_data+$25c,y
		sta char_data+$53c,y

		lda char_data+$264,y
		sta char_data+$544,y

; Update sparkle effect
		dec sparkle_cnt_1
		lda sparkle_cnt_1
		lsr
		and #$1f
		tay

		ldx #$00
sparkle_copy_1	lda char_data+$340,y
		sta char_data+$740,x
		iny
		cpy #$20
		bne *+$04
		ldy #$00
		inx
		cpx #$20
		bne sparkle_copy_1

		dec sparkle_cnt_2
		lda sparkle_cnt_2
		lsr
		lsr
		and #$1f
		tay

		ldx #$00
sparkle_copy_2	lda char_data+$360,y
		ora char_data+$740,x
		sta char_data+$740,x
		iny
		cpy #$20
		bne *+$04
		ldy #$00
		inx
		cpx #$20
		bne sparkle_copy_2

; Update little flashing green and blue LEDs
		ldx led_cnt
		inx
		cpx #$20
		bne lc_xb

		lda $da07
		sta $da2f
		eor #$03
		sta $da07

		ldx #$00
lc_xb		stx led_cnt

; Update the upper text colour pulse - T.M.R
		inc colour_cnt_1
		lda colour_cnt_1
		lsr
		and #$1f
		tax
		lda colour_pulse,x
		ldx #$00
colour_set_1	sta $d87c,x
		inx
		cpx #$04
		bne colour_set_1

; Update the lower text colour pulse - 2018
		lda colour_cnt_1
		clc
		adc #$06
		lsr
		and #$1f
		tax
		lda colour_pulse,x
		ldx #$00
colour_set_2	sta $d919,x
		inx
		cpx #$04
		bne colour_set_2

; Refresh the electricity arcing effects
		inc zap_1_cnt
		lda zap_1_cnt
		lsr
		and #$1f
		tax
		lda zap_frames,x
		asl
		asl
		asl
		tay
		ldx #$00
zap_copy	lda char_data+$200,y
		sta char_data+$7b0,x
		iny
		inx
		cpx #$08
		bne zap_copy

		lda zap_2_cnt
		clc
		adc #$01
		cmp #$48
		bne *+$04
		lda #$00
		sta zap_2_cnt
		lsr
		tax
		cpx #$1f
		bcc *+$04
		ldx #$1f
		lda zap_frames,x
		ora #$40
		sta $0610

; Shift the UPLD arrows once every second frame
		ldx arrow_cnt
		inx
		cpx #$02
		bne ac_xb
		jsr arrow_move
		jsr arrow_move

		ldx #$00
ac_xb		stx arrow_cnt

; Clear the starfield
		lda #$00
		ldx star_temp+$00
		sta char_data+$7e0,x

		ldx star_temp+$01
		sta char_data+$7e3,x

		ldx star_temp+$02
		sta char_data+$7e4,x

		ldx star_temp+$03
		sta char_data+$7e7,x

; Update the starfield
		ldx #$00
star_update	lda star_x_pos,x
		sec
		sbc star_speeds,x
		and #$3f
		sta star_x_pos,x
		inx
		cpx #$04
		bne star_update

; Plot the starfield
		lda star_x_pos+$00
		lsr
		and #$07
		tay
		lda star_x_pos+$00
		lsr
		and #$f8
		tax

		lda star_values,y
		sta char_data+$7e0,x
		stx star_temp+$00

		lda star_x_pos+$01
		lsr
		and #$07
		tay
		lda star_x_pos+$01
		lsr
		and #$f8
		tax

		lda star_values,y
		sta char_data+$7e3,x
		stx star_temp+$01

		lda star_x_pos+$02
		lsr
		and #$07
		tay
		lda star_x_pos+$02
		lsr
		and #$f8
		tax

		lda star_values,y
		sta char_data+$7e4,x
		stx star_temp+$02

		lda star_x_pos+$03
		lsr
		and #$07
		tay
		lda star_x_pos+$03
		lsr
		and #$f8
		tax

		lda star_values,y
		sta char_data+$7e7,x
		stx star_temp+$03

		jmp main_loop


; IRQ interrupt
int		pha
		txa
		pha
		tya
		pha

		lda $d019
		and #$01
		sta $d019
		bne ya
		jmp ea31

ya		lda rn
		cmp #$02
		bne *+$05
		jmp rout2

		cmp #$03
		bne *+$05
		jmp rout3

		cmp #$04
		bne *+$05
		jmp rout4

		cmp #$05
		bne *+$05
		jmp rout5


; Raster split 1
rout1		lda #$09
		sta $d020
		lda #$0b
		sta $d021

		lda #$00
		sta $3fff

		lda #$1b
		sta $d011

		lda #$00
		sta $d022
		lda #$0c
		sta $d023

		lda #$18
		sta $d016
		sta $d018

		lda #$ff
		sta $d015

; Update the scroller
		ldx #$00
mover		asl char_buffer,x
		rol char_data+$338,x
		rol char_data+$330,x
		rol char_data+$328,x
		rol char_data+$320,x
		rol char_data+$318,x
		rol char_data+$310,x
		rol char_data+$308,x
		rol char_data+$300,x
		rol char_data+$2f8,x
		rol char_data+$2f0,x
		rol char_data+$2e8,x
		rol char_data+$2e0,x
		rol char_data+$2d8,x
		rol char_data+$2d0,x
		rol char_data+$2c8,x
		rol char_data+$2c0,x
		rol char_data+$2b8,x
		rol char_data+$2b0,x
		rol char_data+$2a8,x
		rol char_data+$2a0,x
		inx
		cpx #$08
		bne mover

; Is it time to fetch a new character?
		ldx char_cnt
		inx
		cpx #$08
		bne cc_xb

; It is, so here we go...
mread		lda scroll_text
		bne okay
		jsr reset
		jmp mread

; Check for a speed command
okay		cmp #$40
		bcc okay_2

		and #$07
		sta scroll_col

		lda #$20

; Set up the character copier
okay_2		sta def_copy+$01
		lda #$00
		asl def_copy+$01
		rol
		asl def_copy+$01
		rol
		asl def_copy+$01
		rol
		clc
		adc #>char_data
		sta def_copy+$02

; Copy a character into the ROL scroller's buffer and the
; expanded sprite
		ldx #$00
		ldy #$00
def_copy	lda char_data+$08,x
		sta char_buffer,x
		sta sprite_data+$1c8,y
		iny
		iny
		iny
		inx
		cpx #$08
		bne def_copy

; Copy definitions around between the last char and sprites
		ldx #$00
		ldy #$00
def_shift	lda sprite_data+$169,y
		sta sprite_data+$1a9,y

		lda sprite_data+$129,y
		sta sprite_data+$169,y

		lda char_data+$2a0,x
		sta sprite_data+$129,y

		lda #$00
		sta char_data+$2a0,x
		iny
		iny
		iny
		inx
		cpx #$08
		bne def_shift

		inc mread+$01
		bne *+$05
		inc mread+$02

; Shift the scroller's colour data
		ldx #$00
colour_shift	lda scroll_cols+$01,x
		sta scroll_cols+$00,x
		inx
		cpx #$16
		bne colour_shift

		lda scroll_col
		sta scroll_cols+$16

		ldx #$00
cc_xb		stx char_cnt

; Position the scroller's sprites
		lda char_cnt
		eor #$07
		clc
		adc #$a1
		sta scroll_spr_pos+$04
		sec
		sbc #$08
		sta scroll_spr_pos+$02
		sec
		sbc #$08
		sta scroll_spr_pos+$00

		ldy char_cnt
		lda scroll_y_vals+$00,y
		sta scroll_spr_pos+$03
		lda scroll_y_vals+$08,y
		sta scroll_spr_pos+$01

; Set the scroller sprite colours
		ldx #$00
scr_spr_col_set	lda scroll_cols,x
		sta scroll_spr_cols,x
		inx
		cpx #$03
		bne scr_spr_col_set

; Colour the expanded sprite letter (strobe on frame 0)
		lda scroll_col
		ldy char_cnt
		bne *+$04
		lda #$01
		sta scroll_spr_cols+$03

; Clip the lefthand sprite
		ldx #$00
sprite_clip	lda sprite_data+$1a9,x
		and spr_clip_vals,y
		sta sprite_data+$1a9,x
		inx
		inx
		inx
		cpx #$18
		bne sprite_clip

; Update the floppies
		ldx #$02
floppy_move	dec floppy_spr_pos,x
		inx
		inx
		cpx #$10
		bne floppy_move

		dec floppy_spr_pos+$02

		lda floppy_spr_pos+$02
		cmp #$ff
		bne fm_exit

		jsr floppy_reset

fm_exit

; Set up for the second raster split
		lda #$02
		sta rn
		lda #rstr2p
		sta $d012

		jmp ea31


; Raster split 2
rout2		nop
		nop
		lda #$02
		sta $d021

; Relocate sprites for the scroller
		ldx #$00
sprite_set_1a	lda scroll_spr_pos,x
		sta $d000,x
		inx
		cpx #$08
		bne sprite_set_1a

		lda #$08
		sta $d010

		ldx #$00
sprite_set_1b	lda scroll_spr_dp,x
		sta $07f8,x
		lda scroll_spr_cols,x
		sta $d027,x
		inx
		cpx #$04
		bne sprite_set_1b

		lda #$ff
		sta $d01b
		lda #$08
		sta $d017
		sta $d01d

; Recolour the scroller
		ldx #$00
scroll_col_1	lda scroll_cols+$03,x
		sta $d9a4,x
		inx
		cpx #$04
		bne scroll_col_1

		ldx #$05
scroll_col_2	lda scroll_cols+$03,x
		sta $d9a4,x
		inx
		cpx #$0b
		bne scroll_col_2

		ldx #$0c
scroll_col_3	lda scroll_cols+$03,x
		sta $d9a4,x
		inx
		cpx #$14
		bne scroll_col_3

; Tell the runtime code to execute
		lda #$01
		sta sync

; Set up for the third raster split
		lda #$03
		sta rn
		lda #rstr3p
		sta $d012

		jmp ea31


; Raster split 3
rout3		ldx #$00
sprite_setp_2a	lda floppy_spr_pos,x
		sta $d000,x
		inx
		cpx #$11
		bne sprite_setp_2a

; Move sprites for the conveyor belt
		ldx #$00
sprite_set_2b	lda floppy_spr_dp,x
		sta $07f8,x
		lda #$00
		sta $d027,x
		inx
		cpx #$08
		bne sprite_set_2b

		lda #$fe
		sta $d01b
		lda #$00
		sta $d017
		sta $d01d

; Set up for the fourth raster split
		lda #$04
		sta rn
		lda #rstr4p
		sta $d012

		jmp ea31


; Raster split 4
rout4		nop
		nop
		lda #$0b
		sta $d021

; Play the music
		jsr music+$03

; Flash the music note if one of the channels updates
		lda music+$dd
		bne music_skip

		lda #$00
		sta pulse_cnt

; Update the music note's colour
music_skip	lda pulse_cnt
		lsr
		tax

		lda colour_pulse,x
		sta $d9c5
		sta $d9c6
		sta $d9ed
		sta $d9ee

		ldx pulse_cnt
		inx
		cpx #$10
		bne *+$04
		ldx #$0f
		stx pulse_cnt

; Set up for the fifth raster split
		lda #$05
		sta rn
		lda #rstr5p
		sta $d012

		jmp ea31


; Raster split 5
rout5		lda #$13
		sta $d011

		lda #$fc
		cmp $d012
		bne *-$03

		lda #$1b
		sta $d011

; Set up for the first raster split
		lda #$01
		sta rn
		lda #rstr1p
		sta $d012

ea31		pla
		tay
		pla
		tax
		pla
nmi		rti


; Reset the scroller's self mod code
reset		lda #<scroll_text
		sta mread+$01
		lda #>scroll_text
		sta mread+$02
		rts

; Reset the floppies to their start positions
floppy_reset	ldx #$02
		lda #$3f
fr_loop		sta floppy_spr_pos,x
		clc
		adc #$20
		inx
		inx
		cpx #$10
		bne fr_loop

; "Scroll" the sprite data pointers for the floppies
		ldx #$01
fr_sdp_loop	lda floppy_spr_dp+$01,x
		sta floppy_spr_dp+$00,x
		inx
		cpx #$07
		bne fr_sdp_loop

; Once every 32 floppies there'll be one that's upside down...
		ldy #$a1
		lda flip_flop_cnt
		cmp #$0c
		bne *+$03
		iny
		sty floppy_spr_dp+$07

		clc
		adc #$01
		and #$1f
		sta flip_flop_cnt

; ...and far less often it'll be a copyright symbol instead!
		ldx copy_cnt
		inx
		cpx #$65
		bne cct_xb

		lda #$a3
		sta floppy_spr_dp+$07

		ldx #$00
cct_xb		stx copy_cnt

		rts

; Shift the upload arrows - called twice because they're
; multicolour graphics
arrow_move	ldx #$00
am_loop		asl char_data+$568,x
		rol char_data+$560,x
		lda char_data+$568,x
		adc #$00
		sta char_data+$568,x

		asl char_data+$578,x
		rol char_data+$570,x
		lda char_data+$578,x
		adc #$00
		sta char_data+$578,x

		inx
		cpx #$08
		bne am_loop

		rts


; Colour table for pulsing text and the music note
colour_pulse	!byte $08,$0e,$0a,$0c,$0d,$0b,$0f,$09
		!byte $09,$09,$09,$09,$09,$09,$09,$09
		!byte $09,$09,$09,$09,$09,$09,$09,$09
		!byte $09,$09,$0f,$0b,$0d,$0c,$0a,$0e

; Position, data pointers, colours for the scroller's sprites
scroll_spr_pos	!byte $b8,$75,$b0,$75,$a8,$75,$29,$66
scroll_spr_dp	!byte $a6,$a5,$a4,$a7
scroll_spr_cols	!byte $00,$00,$00,$00

; Colour table for each character in the the scrolling message
scroll_cols	!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00

; Vertical positions for scroll chars as they fall off the belt
scroll_y_vals	!byte $76,$76,$76,$77,$77,$78,$79,$7a
		!byte $7c,$7e,$80,$82,$84,$86,$88,$8a

; Sprite definition clipping for the lefthand scroll sprite
; (Otherwise it passes over the black of the pipe!)
spr_clip_vals	!byte $ff,$7f,$3f,$1f,$0f,$07,$03,$01

; Positions of the sprites on the conveyor and the extra sat
; at the right
floppy_spr_pos	!byte $3c,$bd,$20,$ad,$40,$ad,$60,$ad
		!byte $80,$ad,$a0,$ad,$c0,$ad,$e0,$ad
		!byte $01

; Sprite data pointers for the floppy sprites
floppy_spr_dp	!byte $a2,$a0,$a0,$a0,$a0,$a0,$a0,$a0

; Which animation frames to use for the arcing electricity
zap_frames	!byte $07,$08,$06,$08,$00,$08,$01,$08
		!byte $03,$08,$04,$08,$01,$08,$05,$08
		!byte $03,$08,$00,$08,$05,$08,$07,$08
		!byte $08,$08,$08,$08,$08,$08,$08,$08

; UPLD starfield positions and bit values
star_x_pos	!byte $13,$0a,$0d,$05
star_speeds	!byte $01,$03,$02,$05
star_values	!byte $80,$40,$20,$10,$08,$04,$02,$01
star_temp	!byte $00,$00,00,$00

; Cosine table for the VFX effect
effect_cos	!byte $1f,$1f,$1f,$1f,$1f,$1f,$1f,$1f
		!byte $1f,$1f,$1f,$1f,$1f,$1f,$1f,$1e
		!byte $1e,$1e,$1e,$1e,$1e,$1d,$1d,$1d
		!byte $1d,$1d,$1c,$1c,$1c,$1c,$1b,$1b
		!byte $1b,$1b,$1a,$1a,$1a,$19,$19,$19
		!byte $18,$18,$18,$17,$17,$17,$16,$16
		!byte $16,$15,$15,$14,$14,$14,$13,$13
		!byte $13,$12,$12,$11,$11,$11,$10,$10

		!byte $0f,$0f,$0f,$0e,$0e,$0e,$0d,$0d
		!byte $0c,$0c,$0c,$0b,$0b,$0a,$0a,$0a
		!byte $09,$09,$09,$08,$08,$08,$07,$07
		!byte $07,$06,$06,$06,$05,$05,$05,$04
		!byte $04,$04,$04,$03,$03,$03,$03,$02
		!byte $02,$02,$02,$02,$01,$01,$01,$01
		!byte $01,$01,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00

		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$01
		!byte $01,$01,$01,$01,$01,$02,$02,$02
		!byte $02,$02,$03,$03,$03,$03,$04,$04
		!byte $04,$05,$05,$05,$05,$06,$06,$06
		!byte $07,$07,$07,$08,$08,$08,$09,$09
		!byte $09,$0a,$0a,$0b,$0b,$0b,$0c,$0c
		!byte $0c,$0d,$0d,$0e,$0e,$0e,$0f,$0f

		!byte $10,$10,$10,$11,$11,$12,$12,$12
		!byte $13,$13,$13,$14,$14,$15,$15,$15
		!byte $16,$16,$16,$17,$17,$17,$18,$18
		!byte $18,$19,$19,$19,$1a,$1a,$1a,$1b
		!byte $1b,$1b,$1b,$1c,$1c,$1c,$1c,$1d
		!byte $1d,$1d,$1d,$1d,$1e,$1e,$1e,$1e
		!byte $1e,$1e,$1f,$1f,$1f,$1f,$1f,$1f
		!byte $1f,$1f,$1f,$1f,$1f,$1f,$1f,$1f

; Guess what this does...? Values of $40 to $47 change the char colour
		* = $3000
scroll_text	!scr $43,"                    "

		!scr "welcome to   "
		!scr $41,"demo  factory  2018   "
		!scr $43,"a cosine-powered remix of ",$47,"t.m.r's "
		!scr "1987 demo...  ",$45,"now upgraded and "
		!scr "accompanied by a tune from ",$47,"ninja rabbits "
		!scr $45,"which was composed by ",$47,"odie!"
		!scr "         "

		!scr $43,"the code and graphics were thrown together "
		!scr "after i wrote a post for my blog about the "
		!scr "original ",$41,"demo factory ",$43,"which talked "
		!scr "a little about how wonky that code was and what "
		!scr "could potentially be done to improve it."
		!scr "      "

		!scr $47,"and, while this isn't a stunning feat of "
		!scr "software engineering either, it was still fun to "
		!scr "write even if i had to occasionally rethink "
		!scr "things as new elements went in..."
		!scr "      "

		!scr $44,"it started out being a straight re-coding "
		!scr "(adding this rather ironic scroller along the way) "
		!scr "but once that was up and running the dreaded "
		!scr $43,"feature creep ",$44,"began whispering sweet "
		!scr "nothings into my ear, so now there's multiple small "
		!scr "effects and animations."
		!scr "      "

		!scr $43,"this has also been yet another distraction "
		!scr "from the larger project i'm hoping to get finished "
		!scr "in time for x...   although at the current rate of "
		!scr "development that'll just end up being three parts "
		!scr "with a dozen effect presets each to pad things out!"
		!scr "      "

		!scr $45,"speaking of distractions, the development time "
		!scr "was also extended a little because i was also playing "
		!scr $47,"caladrius blaze ",$45,"on steam over the weekend; "
		!scr "i'm pretty rubbish at it right now - i enjoy bullet "
		!scr "hell shoot 'em ups but am not great at them - but "
		!scr "it's been fun so far and i've settled on playing as "
		!scr "maria because i like the wide spread shot and one "
		!scr "of her elemental weapons."
		!scr "      "

		!scr $43,"if procrastination were a sport i could compete "
		!scr "at an olympic level, although it wouldn't exactly be "
		!scr "the most enthralling event to watch..."
		!scr "      "

		!scr $44,"and that almost painful moment of self-awareness "
		!scr "is where i'm going to stop writing and get on with "
		!scr "finishing things up so this demo can be released "
		!scr "into the wild....  "

		!scr $45,"it's been a while, but i think the "
		!scr $41,"cosine ",$45,"greetings list is reasonably up "
		!scr "to date so..."
		!scr "      "

		!scr $47,"hello to the delightful, fluffy bunnies in:   "

		!scr $45,"absence ",$40,"- "
		!scr $43,"abyss connection ",$40,"- "
		!scr $47,"arkanix labs ",$40,"- "
		!scr $41,"artstate ",$40,"- "
		!scr $47,"ate bit ",$40,"- "
		!scr $43,"atlantis ",$40,"- "

		!scr $45,"booze design ",$40,"- "

		!scr $44,"camelot ",$40,"- "
		!scr $45,"censor design ",$40,"- "
		!scr $43,"chorus ",$40,"- "
		!scr $47,"chrome ",$40,"- "
		!scr $41,"cncd ",$40,"- "
		!scr $47,"cpu ",$40,"- "
		!scr $43,"crescent ",$40,"- "
		!scr $45,"crest ",$40,"- "
		!scr $44,"covert bitops ",$40,"- "

		!scr $45,"defence force ",$40,"- "
		!scr $43,"dekadence ",$40,"- "
		!scr $47,"desire ",$40,"- "
		!scr $41,"dac ",$40,"- "
		!scr $47,"dmagic ",$40,"- "
		!scr $43,"dual crew ",$40,"- "

		!scr $45,"exclusive on ",$40,"- "

		!scr $44,"fairlight ",$40,"- "
		!scr $45,"f4cg ",$40,"- "
		!scr $43,"fire ",$40,"- "
		!scr $47,"flat 3 ",$40,"- "
		!scr $41,"focus ",$40,"- "
		!scr $47,"french touch ",$40,"- "
		!scr $43,"funkscientist productions "

		!scr $45,"genesis project ",$40,"- "
		!scr $44,"gheymaid inc ",$40,"- "

		!scr $45,"hitmen ",$40,"- "
		!scr $43,"hokuto force ",$40,"- "

		!scr $47,"legion of doom ",$40,"- "
		!scr $41,"level 64 ",$40,"- "

		!scr $47,"maniacs of noise ",$40,"- "
		!scr $43,"mayday ",$40,"- "
		!scr $45,"meanteam ",$40,"- "
		!scr $44,"metalvotze ",$40,"- "

		!scr $45,"noname ",$40,"- "
		!scr $43,"nostalgia ",$40,"- "
		!scr $47,"nuance ",$40,"- "

		!scr $41,"offence ",$40,"- "
		!scr $47,"onslaught ",$40,"- "
		!scr $43,"orb ",$40,"- "
		!scr $45,"oxyron ",$40,"- "

		!scr $44,"padua ",$40,"- "
		!scr $45,"performers ",$40,"- "
		!scr $43,"plush ",$40,"- "
		!scr $47,"ppcs ",$40,"- "
		!scr $41,"psytronik ",$40,"- "

		!scr $47,"reptilia ",$40,"- "
		!scr $43,"resource ",$40,"- "
		!scr $45,"rgcd ",$40,"- "

		!scr $44,"secure ",$40,"- "
		!scr $45,"shape ",$40,"- "
		!scr $43,"side b ",$40,"- "
		!scr $47,"singular ",$40,"- "
		!scr $41,"slash ",$40,"- "
		!scr $47,"slipstream ",$40,"- "
		!scr $43,"success and trc ",$40,"- "
		!scr $45,"style ",$40,"- "
		!scr $44,"suicyco industries ",$40,"- "

		!scr $45,"taquart ",$40,"- "
		!scr $43,"tempest ",$40,"- "
		!scr $47,"tek ",$40,"- "
		!scr $41,"triad ",$40,"- "
		!scr $47,"tristar and red sector ",$40,"- "

		!scr $43,"viruz ",$40,"- "
		!scr $45,"vision ",$40,"- "

		!scr $44,"wow ",$40,"- "
		!scr $45,"wrath designs ",$40,"- "
		!scr $43,"xenon"
		!scr "      "

		!scr $44,"if you're meant to be on this list but aren't, "
		!scr "please contact your local ",$41,"cosine "
		!scr $44,"representative to inform them of this omission."
		!scr "   "

		!scr $45,"we apologise for any inconvenience or "
		!scr "distress that may have been caused... "
		!scr ""
		!scr "      "

		!scr $43,"and with that done i'll be signing off...   "
		!scr "this has been ",$47,"t.m.r on 2018-09-25 ",$43
		!scr "and don't forget to stick your head around cosine's "
		!scr "virtual door at "
		!scr $41,"http://cosine.org.uk/ "
		!scr $43,"or perhaps visit my blog behind "
		!scr $41,"http://jasonkelk.me.uk/ "
		!scr $43,"for more 8-bit stuffs... .. .  .   ."
		!scr "      "

		!byte $00	; end of text marker
