.segment "HEADER"
.byte 'N','E','S',$1A ; ID
.byte $2 ; num 16k PGR-ROM banks
.byte $1 ; num 8k CHR-ROM banks
.byte $40 ; mirror and mapping lower nibble
.byte $0 ;  mapping higher nibble
.byte $0,$0,$0,$0,$0,$0,$0,$0 ; padding

.segment "CHARS"
.incbin "patternTable1.chr"
.incbin "patternTable1.chr"

.segment "VECTORS"
.word nmi
.word reset
.word irq

.segment "CODE"

COUNTER = $27
COUNTER2 = $27
loop:
  jmp loop

nmi:
  ; get and reset interrupt count
	lda #$00
	sta $00

  ;reset rendering mode
  lda #$88
  sta $2000

  lda #$00
  bit $2002
  sta $2005

  bit $2002
  sta $2006
  sta $2006

  lda #$80
  sta $01

  ;reset mmc3 frame counter
  lda #COUNTER
  sta $C000
  sta $C001
  sta $E000
  sta $E001

  rti

reset:
  ;disable IRQs
  sei

  lda #$40
  sta $4017

  ;Set Nametables
  lda #$20
  sta $2006
  lda #$00
  sta $2006

  ldx #$20 ; columns
  ldy #$1E ; rows
clear_nametable:
  lda #$FF
  sta $2007
  dex
  bne clear_nametable
  ldx #$20
  dey
  bne clear_nametable


  lda #$22
  sta $2006
  lda #$60
  sta $2006

  ;Display Hello World in Nametable
  lda #$20
  sta $2006
  lda #$20
  sta $2006
  lda #$11 ; H
  sta $2007
  lda #$0E ; E
  sta $2007
  lda #$15 ; L
  sta $2007
  lda #$15 ; L
  sta $2007
  lda #$18 ; O
  sta $2007
  lda #$FF ;' '
  sta $2007
  lda #$20 ; W
  sta $2007
  lda #$18 ; O
  sta $2007
  lda #$1B ; R
  sta $2007
  lda #$15 ; L
  sta $2007
  lda #$0D ; D
  sta $2007

  ;Pallets
  lda #$3F
  sta $2006
  lda #$00
  sta $2006
  lda #$0D
  sta $2007

  lda #$3F
  sta $2006
  lda #$03
  sta $2006
  lda #$20
  sta $2007

  ;Set starting address to render at
  lda #$00
  sta $2006
  lda #$00
  sta $2006

  lda #$00
  sta $00
  lda #$80
  sta $01

  ;Enable Rendering
  lda #$1A
  sta $2001

  lda #$88
  sta $2000

	cli
  jmp loop

irq:
  sta $E000
  lda #COUNTER
  sta $C000
  sta $C001
  sta $E001

  lda #$80
  clc
  adc $01
  sta $01
  ;beq skip_scroll

  lda #$81
  sta $2000

  jmp skip_scroll

  ;set scroll position
  test = $FF
  ldx #$40
  ldy #test
set_scroll:
  lda #$90
  sta $2000
: dey
  bne :-
  ldy #test
  lda #$88
  sta $2000

  dex
  bne set_scroll

  lda #$88
  sta $2000

skip_scroll:

  rti
