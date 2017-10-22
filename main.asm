;
; project2121.asm
;
; Created: 19-Oct-17 10:57:25 AM
; Author : Nazif Ahmed
;


; Replace with your application code

.include "m2560def.inc"

;---------------------------------------------------------------------------------
;;	MACROS
;---------------------------------------------------------------------------------
.macro sleep50ms
		rcall sleep_5ms
		rcall sleep_5ms
		rcall sleep_5ms
		rcall sleep_5ms
		rcall sleep_5ms
		rcall sleep_5ms
		rcall sleep_5ms
		rcall sleep_5ms
		rcall sleep_5ms
		rcall sleep_5ms
.endmacro

.macro do_lcd_command
	ldi r16, @0
	rcall lcd_command
	rcall lcd_wait
.endmacro
.macro do_lcd_data
	ldi r16, @0
	rcall lcd_data
	rcall lcd_wait
.endmacro

.macro do_lcd_data_mov
	
	rcall lcd_data
	rcall lcd_wait
.endmacro

;---------------------------------------------------------------------------------
;; END MACROS
;---------------------------------------------------------------------------------

;---------------------------------------------------------------------------------
;; REGISTER DEFS
;---------------------------------------------------------------------------------
.def incrementer = r26
.def temp =r23
.def row =r17
.def col =r18
.def mask =r19
.def temp2 =r20
.def tempNum = r21
.def counter = r22
.equ PORTLDIR = 0xF0
.equ INITCOLMASK = 0xEF
.equ INITROWMASK = 0x01
.equ ROWMASK = 0x0F
;---------------------------------------------------------------------------------
;; END REGISTER DEFS
;---------------------------------------------------------------------------------

;---------------------------------------------------------------------------------
;; EQU declarations
;---------------------------------------------------------------------------------
.equ NUMSTATIONS = 0
.equ STATIONNAMES = 1
.equ TIMESTATIONS = 2
.equ STOPTIME = 3

;---------------------------------------------------------------------------------
;; END EQU declarations
;---------------------------------------------------------------------------------


;---------------------------------------------------------------------------------
;; DSEG
;---------------------------------------------------------------------------------
.dseg 
Num_stations: ;;holds number of stations on network
	.byte 1

Station_names: ;;holds a string of station names separated by '.' character
	.byte 110

Station_times: ;;holds time between consecutive stations
	.byte 21

temp_letters: ;;temporary hold for keys pressed before letter is displayed on lcd
	.byte 8

status:	;;holds an int to represent what stage of simulation we are in
	.byte 1

;---------------------------------------------------------------------------------
;; END DSEG
;---------------------------------------------------------------------------------

;---------------------------------------------------------------------------------
;; CSEG
;---------------------------------------------------------------------------------
.cseg

rjmp RESET
.org 0x72
;---------------------------------------------------------------------------------
;; END CSEG
;---------------------------------------------------------------------------------

RESET:
ldi temp, low(RAMEND)
out SPL, temp
ldi temp, high(RAMEND)
out SPH, temp
ldi temp, PORTLDIR ; columns are outputs, rows are inputs
STS DDRL, temp     ; cannot use out
ser temp
out DDRC, temp ; Make PORTC all outputs
out PORTC, temp ; Turn on all the LEDs
; keypad keeps scanning the keypad to find which key is pressed.

;intialise lcd 

ser r16
out DDRF, r16
out DDRA, r16
clr r16
out PORTF, r16
out PORTA, r16

do_lcd_command 0b00111000 ; 2x5x7
rcall sleep_5ms
do_lcd_command 0b00111000 ; 2x5x7
rcall sleep_1ms
do_lcd_command 0b00111000 ; 2x5x7
do_lcd_command 0b00111000 ; 2x5x7
do_lcd_command 0b00001000 ; display off?
do_lcd_command 0b00000001 ; clear display
do_lcd_command 0b00000110 ; increment, no display shift
do_lcd_command 0b00001110 ; Cursor on, bar, no blink



	do_lcd_data 'N'
	do_lcd_data 'U'
	do_lcd_data 'M'

	do_lcd_data ' '
	do_lcd_data 'S'
	do_lcd_data 'T'
	do_lcd_data 'A'
	do_lcd_data 'T'
	do_lcd_data 'I'
	do_lcd_data 'O'
	do_lcd_data 'N'
	do_lcd_data 'S'
	do_lcd_data ' '
	clr temp
	sts status,temp
	rjmp main

;---------------------------------------------------------------------------------
;; MAIN BLOCK
;---------------------------------------------------------------------------------

main:
	clr temp
	lds temp,status

	cpi temp,NUMSTATIONS	
	breq load_num_stations


	cpi temp,STATIONNAMES
	breq load_station_names



load_num_stations:
	ldi zl, low(Num_stations)
	ldi zh, high(Num_stations)
	clr counter
	rcall keypad
	clr temp
	ldi temp,1
	sts status,temp
	ldi temp2,48
	add temp,temp2
	do_lcd_command 0b00000001 ; clear display
	mov r16,temp
	do_lcd_data_mov
	rjmp main

load_station_names:


	ldi yl,low(Num_stations)
	ldi yh,high(Num_stations)

	ld temp,y+
	mov temp2,temp
	ldi incrementer,1

	input_names:
	cp incrementer,temp2

	breq done_stat_names
	do_lcd_command 0b00000001 ; clear display
	do_lcd_data 'S'
	do_lcd_data 'T'
	do_lcd_data 'A'
	do_lcd_data 'T'
	do_lcd_data 'N'
	do_lcd_data 'A'	
	do_lcd_data 'M'
	
	ldi tempNum,48
	mov r16,incrementer
	add r16,tempNum
	do_lcd_data_mov
	do_lcd_data ' '
	inc incrementer 
	rcall keypad
	rjmp input_names

	done_stat_names:
	do_lcd_command 0b00000001 ; clear display
	do_lcd_data 'D'
	do_lcd_data 'O'
	do_lcd_data 'N'
	do_lcd_data 'E'
	do_lcd_data '!'



	jmp end
	end: rjmp end

return :  //////////////HACK TOFIX RCALL ISSUE 
	ret
keypad:
;push counter
ldi mask, INITCOLMASK ; initial column mask
clr col ; initial column

colloop:
STS PORTL, mask ; set column to mask value
; (sets column 0 off)
ldi temp, 0xFF ; implement a delay so the
; hardware can stabilize

delay:
dec temp
brne delay
LDS temp, PINL ; read PORTL. Cannot use in 
andi temp, ROWMASK ; read only the row bits
cpi temp, 0xF ; check if any rows are grounded
breq nextcol ; if not go to the next column
ldi mask, INITROWMASK ; initialise row check
clr row ; initial row

rowloop:      
mov temp2, temp
and temp2, mask ; check masked bit
brne skipconv ; if the result is non-zero, ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;UNCOMMENT
; we need to look again


rcall convert ; if bit is clear, convert the bitcode            ;;;KEY CHANGE HERE AFTER HERE!!!         
cpi temp,14 ;;;;if user strikes * then rather than jmp back into keypad go back to keypad caller
breq return ;;;;This return will call back to caller from main 
st z+,temp


jmp keypad ; and start again

skipconv:
	inc row ; else move to the next row
	lsl mask ; shift the mask to the next bit
	jmp rowloop 
         
nextcol:     
	cpi col, 3 ; check if we^Òre on the last column
	breq keypad ; if so, no buttons were pushed,
	; so start again.

	sec ; else shift the column mask:
	; We must set the carry bit
	rol mask ; and then rotate left by a bit,
	; shifting the carry into
	; bit zero. We need this to make
	; sure all the rows have
	; pull-up resistors
	inc col ; increment column value
	jmp colloop ; and check the next column
	; convert function converts the row and column given to a
	; binary number and also outputs the value to PORTC.
	; Inputs come from registers row and col and output is in
	; temp.
convert:
	;ldi row,3;                               ;;;;;;;;;;;;;;;;;;;UNCOMMENT 
	cpi col, 3 ; if column is 3 we have a letter
	breq letters
	cpi row, 3 ; if row is 3 we have a symbol or 0
	breq symbols
	ldi tempNum,48
	mov temp, row ; otherwise we have a number (1-9)
	lsl temp ; temp = row * 2
	add temp, row ; temp = row * 3
	add temp, col ; add the column address
	; to get the offset from 1
	inc temp ; add 1. Value of switch is
	; row*3 + col + 1.
	jmp convert_end
letters:
	ldi tempNum,55
	ldi temp, 0xA
	add temp, row ; increment from 0xA by the row value
	jmp convert_end
symbols:
	;ldi col,0 ;;;;;;;;;;;;;;;;;;;;UNCOMMENT TO DEBUG VIA CPU
	cpi col, 0 ; check if we have a star
	breq star
	cpi col, 1 ; or if we have zero
	breq zero
	ldi tempNum,20
	ldi temp, 0xF ; we'll output 0xF for hash
	jmp convert_end
star:
	ldi tempNum,28
	ldi temp, 0xE ; we'll output 0xE for star
	jmp convert_end
zero:
	ldi tempNum,48
	clr temp ; set to zero
convert_end:
	;;cpi tempNum,48 when it's a number for if statement later



	;cpi temp,14 ;if star is encountered break out of keypad input and go to next stage of emulator
	;breq finish_input
	 
	add temp,tempNum
	;;;;;;experimental;;;;
	ldi yl,low(temp_letters)
	ldi yh,high(temp_letters)
	
	st y+,temp
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	inc counter
	cpi counter,10
	brlo lcd_limit
		do_lcd_command 0b00000001 ; clear display
		clr counter	

		do_lcd_data 'T'
		do_lcd_data 'O'
		do_lcd_data 'O'
		do_lcd_data ' '
		do_lcd_data 'L'
		do_lcd_data 'O'
		do_lcd_data 'N'
		do_lcd_data 'G'

													///FIX ERROR MESSAGE BRANCHING HERE FOR MORE THAN 10 CHARS
		lcd_limit:
		lds temp2,status
		cpi temp2,0
		breq num_print
		cpi temp2,1
		breq letter_print
		cpi temp2,2
		breq letter_print
		cpi temp2,3
		breq letter_print

num_print:
	mov r16,temp
	do_lcd_data_mov
	jmp continue
	
letter_print:
													//ADD LETTER PROCESSING HERE //////////////////////
	jmp continue 	

continue:
	sub temp,tempNum
	;ldi temp,14
	sleep50ms
	sleep50ms
	sleep50ms
	sleep50ms

	else:

	out PORTC, temp ; write value to PORTC
	;pop counter
	ret ; return to caller



.equ LCD_RS = 7
.equ LCD_E = 6
.equ LCD_RW = 5
.equ LCD_BE = 4

.macro lcd_set
	sbi PORTA, @0
.endmacro
.macro lcd_clr
	cbi PORTA, @0
.endmacro

;
; Send a command to the LCD (r16)
;

lcd_command:
	out PORTF, r16
	nop
	lcd_set LCD_E
	nop
	nop
	nop
	lcd_clr LCD_E
	nop
	nop
	nop
	ret

lcd_data:
	out PORTF, r16
	lcd_set LCD_RS
	nop
	nop
	nop
	lcd_set LCD_E
	nop
	nop
	nop
	lcd_clr LCD_E
	nop
	nop
	nop
	lcd_clr LCD_RS
	ret

lcd_wait:
	push r16
	clr r16
	out DDRF, r16
	out PORTF, r16
	lcd_set LCD_RW
lcd_wait_loop:
	nop
	lcd_set LCD_E
	nop
	nop
        nop
	in r16, PINF
	lcd_clr LCD_E
	sbrc r16, 7
	rjmp lcd_wait_loop
	lcd_clr LCD_RW
	ser r16
	out DDRF, r16
	pop r16
	ret

.equ F_CPU = 16000000
.equ DELAY_1MS = F_CPU / 4 / 1000 - 4
; 4 cycles per iteration - setup/call-return overhead

sleep_1ms:
	push r24
	push r25
	ldi r25, high(DELAY_1MS)
	ldi r24, low(DELAY_1MS)
delayloop_1ms:
	sbiw r25:r24, 1
	brne delayloop_1ms
	pop r25
	pop r24
	ret

sleep_5ms:
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	ret
