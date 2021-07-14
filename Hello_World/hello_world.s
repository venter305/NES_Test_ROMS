.segment "HEADER"
.byte 'N','E','S',$1A ; ID
.byte $2 ; num 16k PGR-ROM banks
.byte $1 ; num 8k CHR-ROM banks
.byte $0 ; mirror and mapping lower nibble
.byte $0 ;  mapping higher nibble
.byte $0,$0,$0,$0,$0,$0,$0,$0 ; padding

.segment "CHARS"
.incbin "patternTable1.chr"

.segment "VECTORS"
.word nmi
.word reset
.word irq

.segment "CODE"
loop:
  jmp loop

nmi:
  rti

reset:
  ;disable IRQs
  sei

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


  ;Enable Rendering
  lda #$0A
  sta $2001


  jmp loop

irq:
  rti
