; Program Name: PianoCSE
; Author: Badja <badjagames@gmail.com>
; Porter: Ivoah <ivoahivoah@gmail.com>
; Version: 1.0
; Date: June 21, 2015
; You may modify this source code for personal use only.
; You may NOT distribute the modified source or program file.
; Written for Doors CSE 8.0 and higher (http://dcs.cemetech.net)

.nolist
#include "ti84pcse.inc"
#include "dcse8.inc"

#define DCSE_HEADER

#define saferam1 plotSScreen

tuning      .equ  saferam1

ch1count    .equ  saferam1+2
ch1freq     .equ  saferam1+4
ch1curr     .equ  saferam1+6

ch2count    .equ  saferam1+7
ch2freq     .equ  saferam1+9
ch2curr     .equ  saferam1+11

flgs        .equ  asm_flag1
key         .equ  asm_flag1_0
chord       .equ  asm_flag1_1

#ifdef DCSE_HEADER
	.org UserMem
BinaryStart:
	.db $DE,$2A,"N",$BB,$B4,$BB,$B4,$BB,$B3,$BB,$C3,")D"   ;Disp "Needs D
	.db $BB,$BF,$BB,$BF,$BB,$C2,$BB,$C3,")CSE",$2A,$3F     ;oors CSE"
	.db $EF,$11                                            ;OpenLib(
	.db "D",$BB,$BF,$BB,$BF,$BB,$C2,$BB,$C3,"CSE",$11,$3F  ;(tokenized "DoorsCSE")
	.db $EF,$12,$3F                                        ;ExecLib
	.db $D5,$3F                                            ;Return
	.db tExtTok,tAsm84CPrgm,$3F                            ;Asm84CPrgm
HeaderStart:
	.dw ASMStart-HeaderStart        ;offset to code
	; Header sections start here

	.dw 10
	.db ASMHEADER_FIELD_TYPE_LIB    ;== 3
	.db "DoorsCSE",8,0              ;Lib name, min major version, min minor version

	.dw 163
	.db ASMHEADER_FIELD_TYPE_ICON
	.db 2
	.db $00,$00,$ff,$ff
    .block 28
    .db 32, 32
	.db $00,$00,$00,$00,$00,$00,$00,$00
	.db $01,$11,$00,$01,$10,$00,$11,$10
	.db $01,$11,$00,$01,$10,$00,$11,$10
	.db $01,$11,$00,$01,$10,$00,$11,$10
	.db $01,$11,$00,$01,$10,$00,$11,$10
	.db $01,$11,$00,$01,$10,$00,$11,$10
	.db $01,$11,$00,$01,$10,$00,$11,$10
	.db $01,$11,$00,$01,$10,$00,$11,$10
	.db $01,$11,$00,$01,$10,$00,$11,$10
	.db $01,$11,$10,$11,$11,$01,$11,$10
	.db $01,$11,$10,$11,$11,$01,$11,$10
	.db $01,$11,$10,$11,$11,$01,$11,$10
	.db $01,$11,$10,$11,$11,$01,$11,$10
	.db $01,$11,$10,$11,$11,$01,$11,$10
	.db $01,$11,$10,$11,$11,$01,$11,$10
	.db $00,$00,$00,$00,$00,$00,$00,$00

	.dw 14
	.db ASMHEADER_FIELD_TYPE_DESC
	.db "PianoCSE v1.0",0

	.dw 6
	.db ASMHEADER_FIELD_TYPE_AUTH
	.db "Badja",0

 	.dw 0     ;End of header field: 0 bytes of data
 	.db $ff   ;End of header field: type 255
.list
ASMStart:
	.org UserMem
#else
	.org UserMem-2
	.db tExtTok, tAsm84CCmp
#endif

start:
	ld a,1 \ out ($20),a ; 15MHz mode
      bcall(_DrawStatusBar)
      bcall(_maybe_ClrScrn)
      ld    hl,54             ; initial tuning
      ld    (tuning),hl
      res   key,(iy+flgs)    ; initial key
      res   chord,(iy+flgs)  ; initial chord mode
      res   appAutoScroll,(iy+appflags)
      ld    hl,$0000          ; show instructions
      ld    (currow),hl
      ld    hl,instructions
putInstr:
      set   textInverse,(iy+textflags)
      bcall(_puts)
      res   textInverse,(iy+textflags)
      bcall(_puts)
	ld b,(hl)
      djnz  putInstr
      call  dispMaj
      call  dispTune
      call  dispUnison
	xor a \ out ($20),a ; 8MHz mode

mainLoop:
      ld    e,0         ; E = number of notes pressed
      ld    h,e         ; initialise channels to 0 (no note)
      ld    l,e
      ld    (ch1freq),hl
      ld    (ch2freq),hl

      ld    d,$bf       ; D = key column
      call  getKeyData
      ld    hl,0        ; true pitch octaves
      ld    bc,24
      cp    239
      jp    z,octave1
      cp    247
      jp    z,octave2
      cp    251
      jp    z,octave3
      cp    253
      jp    z,octave4

      bit   5,a         ; check if 2nd is pressed
      jr    nz,not2nd
      ld    bc,2
      push  af
      call  storeNote
      pop   af
not2nd:

      bit   key,(iy+flgs)    ; determine major/minor
      jr    z,useMajKey
      ld    hl,keyMapMin      ; load minor keymap

      bit   6,a               ; if minor, check if MODE is pressed
      jr    nz,notMODE
      ld    bc,0
      push  af
      call  storeNote
      pop   af
notMODE:
      jr    useMinKey

useMajKey:
      ld    hl,keyMapMaj      ; load major keymap

useMinKey:
      bit   7,a               ; check if DEL is pressed
      jr    nz,notDEL
      ld    bc,26
      call  storeNote
notDEL:

nextCol:
      call  getKeyData
      ld    b,8         ; check all 8 keys in key column
checkCol:
      push  bc
      rla               ; get next bit from A
      jr    c,noPlay    ; if bit is 1, key is not pressed
      push  af
      ld    a,(hl)      ; get note from keymap
      cp    255         ; 255 in the key map means no sound
      jr    z,noSound
      ld    b,0         ; store/play the note
      ld    c,a
      call  storeNote
noSound:
      pop   af
noPlay:
      inc   hl
      pop   bc
      djnz  checkCol
      ld    a,d
      cp    $fd         ; continue until key column is $fd
      jr    nz,nextCol

      call  getKeyData
      cp    191         ; check if CLEAR is pressed
      jr    z,exit
      push  de
      cp    223
      call  z,changeChord
      cp    251
      call  z,downOct
      cp    253
      call  z,upOct
      pop   de

      call  getKeyData
      bit   1,a         ; check if left is pressed
      jr    nz,notLeft
      ld    bc,28
      call  storeNote
notLeft:
      bit   0,a
      push  af
      call  z,down
      pop   af
      bit   2,a
      push  af
      call  z,changeKey
      pop   af
      bit   3,a
      call  z,up

      in    a,(3)       ; check if ON is pressed
      bit   3,a
      jr    nz,notOn
      ld    bc,30
      call  storeNote
notOn:

      bit   chord,(iy+flgs)  ; if in unison mode, play the two channels
      call  z,playNotes
      jp    mainLoop


exit:
      set   appAutoScroll,(iy+appflags)
	xor a \ out (0),a
      ret


storeNote:
      bit   1,e               ; maximum of two notes at a time in unison mode
      ret   nz
      push  hl
      ld    hl,pitches        ; address of the pitches table
      add   hl,bc             ; add the pitch
      ld    bc,(tuning)
      add   hl,bc             ; add the tuning
      ld    c,(hl)            ; get the period in BC
      inc   hl
      ld    b,(hl)
      bit   chord,(iy+flgs)  ; jump if in arpeggio mode
      jr    nz,arpeggio
      bit   0,e               ; store note into next channel
      call  z,storeCh1
      call  nz,storeCh2
      inc   e                 ; move to next channel
      jr    doneNote
arpeggio:
      call  storeCh1          ; store into both channels
      call  storeCh2
      push  af
      call  playNotes         ; play note immediately
      pop   af
doneNote:
      pop   hl
      ret


storeCh1:
      ld    (ch1freq),bc
      ret

storeCh2:
      ld    (ch2freq),bc
      ret


octave4:
      add   hl,bc
octave3:
      add   hl,bc
octave2:
      add   hl,bc
octave1:
      bit   key,(iy+flgs)
      jr    nz,isMinKey
      ld    bc,6
      add   hl,bc
isMinKey:
      ld    (tuning),hl
      call  dispTune
      call  delay
      jp    mainLoop


dispTune:
      ld    hl,$0b03
      ld    (currow),hl
      ld    hl,(tuning)
      sra   l
      bcall(_disphl)
	  ld bc,$0B03
	  ld hl,txtTune
	  jr putString


dispMaj:
      ld    bc,$0b02
      ld    hl,txtMajor
      jr    putString

dispMin:
      ld    bc,$0b02
      ld    hl,txtMinor


putString:
      ld    (currow),bc
      bcall(_puts)
      ret


changeKey:
      bit   key,(iy+flgs)
      jr    nz,toMajor
      set   key,(iy+flgs)
      call  dispMin
      jr    delay
toMajor:
      res   key,(iy+flgs)
      call  dispMaj
      jr    delay


changeChord:
      ld    a,(iy+flgs)
      xor   %00000010
      ld    (iy+flgs),a

dispChord:
      bit   chord,(iy+flgs)
      jr    z,dispUnison

dispArpeggio:
      ld    bc,$0907
      ld    hl,txtArpeggio
      call  putString
      jr    delay

dispUnison:
      ld    bc,$0907
      ld    hl,txtUnison
      call  putString
      jr    delay


downOct:
      ld    a,(tuning)
      cp    24
      ret   c
      sub   24
      jr    doneTune

upOct:
      ld    a,(tuning)
      cp    62
      ret   nc
      add   a,24
      jr    doneTune

down:
      ld    a,(tuning)
      cp    84
      ret   z
      add   a,2
      jr    doneTune

up:
      ld    a,(tuning)
      cp    0
      ret   z
      sub   2
doneTune:
      ld    (tuning),a
      call  dispTune


delay:
      ld    bc,$8000
delayLoop:
      dec   bc
      ld    a,b
      or    c
      jr    nz,delayLoop
      ret


getKeyData:
      ld    a,$ff       ; reset keyport
      out   (1),a
      ld    a,d
      out   (1),a
	  nop \ nop \ nop
      in    a,(1)
      sra   d           ; D = next key column
      ret


playNotes:
      ld    b,0         ; loop 256 times
playLoop:
      push  bc

      ld    hl,(ch1count)
      dec   h                 ; decrease MSB of count
      jr    nz,noEdge1a       ; jump if not zero (on edge of square wave when count reaches 0)
      ld    bc,(ch1freq)      ; BC = freq
      ld    a,b
      or    a
      jr    z,noEdge1b        ; skip if MSB is 0 (no note)
      add   hl,bc             ; otherwise add frequency to count
      ld    bc,ch1curr        ; and
      ld    a,(bc)            ; invert bit 0 of curr
      xor   1                 ; 0->1 or 1->0
      ld    (bc),a
      jr    noEdge1c

noEdge1a:                     ; makes the playNotes routine constant time ...
      res   7,(ix)      ; 23
      bit   7,(hl)      ; 12
                        ;=35
noEdge1b:                     ; ... for greater pitch accuracy
      res   7,(ix)      ; 23
      adc   a,(ix)      ; 19
      adc   a,(hl)      ;  7
                        ;=49
noEdge1c:
      ld    (ch1count),hl

      ld    hl,(ch2count)
      dec   h                 ; decrease MSB of count
      jr    nz,noEdge2a       ; jump if not zero (on edge of square wave when count reaches 0)
      ld    bc,(ch2freq)      ; BC = freq
      ld    a,b
      or    a
      jr    z,noEdge2b        ; skip if MSB is 0 (no note)
      add   hl,bc             ; otherwise add frequency to count
      ld    bc,ch2curr        ; and
      ld    a,(bc)            ; invert bit 0 of curr
      xor   1                 ; 0->1 or 1->0
      ld    (bc),a
      jr    noEdge2c

noEdge2a:                     ; makes the playNotes routine constant time ...
      res   7,(ix)      ; 23
      bit   7,(hl)      ; 12
                        ;=35
noEdge2b:                     ; ... for greater pitch accuracy
      res   7,(ix)      ; 23
      adc   a,(ix)      ; 19
      adc   a,(hl)      ;  7
                        ;=49
noEdge2c:
      ld    (ch2count),hl

      ld    a,(ch1curr)
      ld    b,a
      ld    a,(ch2curr)
      ld    c,a

      ld    a,%00110100
      srl   b                 ; rotate channel 1 bit into A
      rla
      srl   c                 ; rotate channel 2 bit into A
      rla
      out   (0),a             ; output the sample

      pop   bc
      djnz  playLoop

      ret


keyMapMaj:
      .db   6,10,12,16,20,24,26,255
      .db   4,8,255,14,18,22,255,28
      .db   30,34,36,40,44,48,50,54
      .db   255,32,255,38,42,46,255,52

keyMapMin:
      .db   6,8,12,16,18,22,26,255
      .db   4,255,10,14,255,20,24,28
      .db   30,32,36,40,42,46,50,54
      .db   255,255,34,38,255,44,48,52

pitches:
      ; lowest note: G# 1
      ; highest note: F 7
      .dw   $898C
      .dw   $81D4
      .dw   $7A8A
      .dw   $73AA
      .dw   $6D2C
      .dw   $670B
      .dw   $6143
      .dw   $5BCD
      .dw   $56A6
      .dw   $51C9
      .dw   $4D32
      .dw   $48DD
      .dw   $44C6
      .dw   $40EA
      .dw   $3D45
      .dw   $39D5
      .dw   $3696
      .dw   $3385
      .dw   $30A1
      .dw   $2DE6
      .dw   $2B53
      .dw   $28E4
      .dw   $2699
      .dw   $246E
      .dw   $2263
      .dw   $2075
      .dw   $1EA2
      .dw   $1CEA
      .dw   $1B4B
      .dw   $19C2
      .dw   $1850
      .dw   $16F3
      .dw   $15A9
      .dw   $1472
      .dw   $134C
      .dw   $1237
      .dw   $1131
      .dw   $103A
      .dw   $0F51
      .dw   $0E75
      .dw   $0DA5
      .dw   $0CE1
      .dw   $0C28
      .dw   $0B79
      .dw   $0AD4
      .dw   $0A39
      .dw   $09A6
      .dw   $091B
      .dw   $0898
      .dw   $081D
      .dw   $07A8
      .dw   $073A
      .dw   $06D2
      .dw   $0670
      .dw   $0614
      .dw   $05BC
      .dw   $056A
      .dw   $051C
      .dw   $04D3
      .dw   $048D
      .dw   $044C
      .dw   $040E
      .dw   $03D4
      .dw   $039D
      .dw   $0369
      .dw   $0338
      .dw   $030A
      .dw   $02DE
      .dw   $02B5
      .dw   $028E


instructions:
      ; 26x10
      .db   "         PianoCSE         ",0
      .db   "   ",0,"badjagames@gmail.com",0,"   ",0
      .db   "Right",0," Key:                ",0
      .db   "Up/Down",0," Tune:             ",0
      .db   "+/-",0," Tune by octaves       ",0
      .db   "     Y/WIN/ZOOM/TRACE     ",0
      .db   "  Change pitch to octave  ",0
      .db   "^",0," Chord:                  ",0,1

txtMajor:
      .db   "C Maj",0

txtMinor:
      .db   "A Min",0

txtTune:
      .db   "e:",0

txtUnison:
      .db   "Unison ",0

txtArpeggio:
      .db   "Arpegio",0

.end
