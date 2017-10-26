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

.macro sleep200ms
		rcall sleep_50ms
		rcall sleep_50ms
		rcall sleep_50ms
		rcall sleep_50ms
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
.equ STARTSIM = 4

;---------------------------------------------------------------------------------
;; END EQU declarations
;---------------------------------------------------------------------------------


;---------------------------------------------------------------------------------
;; DSEG
;---------------------------------------------------------------------------------
.dseg 
Num_stations: ;;holds number of stations on network
	.byte 1

Station_names: ;;holds a string of station names separated by '.' character HOLDS ASCII VALS OF STATION_NAMES
	.byte 220

Station_times: ;;holds time between consecutive stations
	.byte 21

temp_letters: ;;temporary hold for keys pressed before letter is displayed on lcd
	.byte 8

status:	;;holds an int to represent what stage of simulation we are in
	.byte 1

Stop_time:
	.byte 1 ;;holds the stations stop time


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
	lds temp,status	;if else block to redirect code to the right stage of simulation

	cpi temp,NUMSTATIONS	
	breq load_num_stations


	cpi temp,STATIONNAMES
	breq load_station_names

	cpi temp,TIMESTATIONS
	breq call_station_times

	cpi temp,STOPTIME
	breq call_stop_time

	cpi temp,STARTSIM
	breq call_start_sim



load_num_stations:
	ldi zl, low(Num_stations)
	ldi zh, high(Num_stations)	;;initalise z pointer to point to beginning of dseg that holds number of stations
	clr counter

	ldi yl,low(temp_letters)
	ldi yh,high(temp_letters)	;;initialise y pointer to point to begininning of a temporary buffer that holds letters from keypad to be processed
	rcall keypad
	clr temp
	ldi temp,1
	sts status,temp



	rjmp main

call_station_times:
	rjmp load_station_times
call_stop_time:
	rjmp load_stop_time
call_start_sim:
	jmp start_sim
load_station_names:

	ldi zl, low(Station_names)
	ldi zh, high(Station_names)
	ldi incrementer,1

	input_names:
		ldi yl,low(Num_stations)	;grab number of stations from dseg
		ldi yh,high(Num_stations)	;grab num_stations every iteration as temp2 changes when keypad is called
		ldi temp,1	
		ld temp2,y
		add temp2,temp				;add one as incrementer is indexed from 1 
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
		mov r16,incrementer	;;print out station number to input name of 
		add r16,tempNum
		do_lcd_data_mov
		do_lcd_data ' '
		
		ldi yl,low(temp_letters)
		ldi yh,high(temp_letters)
		inc incrementer 
		rcall keypad
		rjmp input_names

	done_stat_names:
	ldi temp2, TIMESTATIONS
	sts status,temp2		;;done here time to go to station times



	rjmp main
	

load_station_times:
	ldi zl, low(Station_times)
	ldi zh, high(Station_times)



	ldi incrementer,1
	
	get_times:
		ldi yl,low(Num_stations)	;grab number of stations from dseg
		ldi yh,high(Num_stations)	;grab num_stations every iteration as temp2 changes when keypad is called
		ldi temp,1	
		ld temp2,y
		
		cp incrementer,temp2
		breq done_times
		ldi tempNum,48
		do_lcd_command 0b00000001 ; clear display
		do_lcd_data 'S'
		do_lcd_data 'T'
		mov r16,incrementer
		add r16,tempNum
		do_lcd_data_mov
		do_lcd_data ' '
		do_lcd_data 'T'
		do_lcd_data 'O'
		do_lcd_data ' '
		do_lcd_data 'S'
		do_lcd_data 'T'
		mov r16,incrementer
		inc r16
		add r16,tempNum
		
		
		do_lcd_data_mov
		do_lcd_data ' '
		ldi yl,low(temp_letters)
		ldi yh,high(temp_letters)
		inc incrementer 
		rcall keypad
		rjmp get_times


	done_times:
		do_lcd_command 0b00000001 ; clear display
		do_lcd_data 'S'
		do_lcd_data 'T'
		do_lcd_data ' '
		ldi tempNum,48
		mov r16,incrementer
		add r16,tempNum
		do_lcd_data_mov
		do_lcd_data ' '
		do_lcd_data 'T'
		do_lcd_data 'O'
		do_lcd_data ' '
		do_lcd_data 'S'
		do_lcd_data 'T'
		do_lcd_data '1'
		do_lcd_data ' '
		ldi yl,low(temp_letters)
		ldi yh,high(temp_letters)
		rcall keypad

		ldi temp2, STOPTIME
		sts status,temp2		;;done here time to go to stop time




		rjmp main

		

load_stop_time:
	



	ldi zl, low(Stop_time)	
	ldi zh,high(Stop_time)



	clr counter
	ldi yl,low(temp_letters)
	ldi yh,high(temp_letters)
	do_lcd_command 0b00000001 ; clear display

	do_lcd_data 'S'
	do_lcd_data 'T'
	do_lcd_data 'O'
	do_lcd_data 'P'
	do_lcd_data ' '
	do_lcd_data 'T'
	do_lcd_data 'I'
	do_lcd_data 'M'
	do_lcd_data 'E'
	do_lcd_data ' '
	ldi yl,low(temp_letters)
	ldi yh,high(temp_letters)
	rcall keypad
	
	ldi temp,STARTSIM
	sts status,temp




	rjmp main
	

start_sim:


do_lcd_command 0b00000001 ; clear display
	do_lcd_data 'L'
	do_lcd_data 'O'
	do_lcd_data 'A'
	do_lcd_data 'D'
	do_lcd_data 'I'
	do_lcd_data 'N'
	do_lcd_data 'G'
	do_lcd_data '5'
	do_lcd_data 's'

	rcall sleep_1s
	rcall sleep_1s
	rcall sleep_1s
	rcall sleep_1s
	rcall sleep_1s

	ldi yl,low(Station_names)
	ldi yh,high(Station_names)

	ldi zl,low(Num_stations)
	ldi zh,high(Num_stations)
	ld temp,z
	ldi mask,1 ;; Let mask be new incrementer as incrementer conflicts with x pointer
	
	simulation_loop:
		cp mask,temp
		breq done
		
		do_lcd_command 0b00000001 ; clear display
		print_station_loop:
			ld row,y+
			cpi row,'.'	;;row holds where y is pointing
			breq continue_simulation
			;ld row,y+
			mov r16,row
			do_lcd_data_mov
			rjmp print_station_loop			
	
		continue_simulation:
			ldi zl,low(Stop_time)
			ldi zh,high(Stop_time)	
			ld temp2,z
			ldi col,0
			
			sleep_loop:
				cp col,temp2
				breq done_sleep
				rcall sleep_1s

				inc col
				rjmp sleep_loop
			done_sleep:		
				;ld row,y+ ;DEBUG ALERT	
				inc mask
				rjmp simulation_loop
	done:
	do_lcd_command 0b00000001 ; clear display
		do_lcd_data 'D'
		do_lcd_data 'O'
		do_lcd_data 'N'
		do_lcd_data 'E'
		do_lcd_data '!'	
		
	jmp end_no
	end_no: rjmp end_no

return :  //////////////HACK TOFIX RCALL ISSUE 
	lds temp2,status
	cpi temp2,STATIONNAMES
	breq add_asterisk
	ret
add_asterisk:
	ldi temp2,'.'
	st z+,temp2
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
brne delay ;;;;;;;;;;;;;UNCOMMENT TO STOP DEBUGGING
LDS temp, PINL ; read PORTL. Cannot use in 
andi temp, ROWMASK ; read only the row bits
cpi temp, 0xF ; check if any rows are grounded
breq nextcol ; if not go to the next column
ldi mask, INITROWMASK ; initialise row check
clr row ; initial row

rowloop:      
mov temp2, temp
and temp2, mask ; check masked bit
brne skipconv ; if the result is non-zero, ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;UNCOMMENT DEBUG
; we need to look again


rcall convert ; if bit is clear, convert the bitcode            ;;;KEY CHANGE HERE AFTER HERE!!!         
cpi temp,14 ;;;;if user strikes * then rather than jmp back into keypad go back to keypad caller
breq return ;;;;This return will call back to caller from main 

lds col,status
cpi col,STATIONNAMES;; check if we are on load station name mode, if so have to ignore numbers going into dseg
breq skip
st z+,temp
clr temp

jmp keypad ; and start again

skip:
	cpi temp,10
	brlo skip_integer ;;we have a number which we dont want to add to dseg so skip
	st z+,temp
	clr temp
	rjmp keypad

skip_integer:
	clr temp
	rjmp keypad
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
	;jmp convert_end                             ;;;;;;;;;;;;;;;;;;;UNCOMMENT DEBUG
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



	st y+,temp ;;insert raw number/letter (with ascii subtracted) into temp hold for letters

	;ldi temp,5 ;;;;;;;;	UNCOMMENT DEBUG   (COMMENT)
	;st z+,temp ;;;;;;;;;;;;;;;;;UNCOMMENT DEBUG  (COMMENT)
	
	jmp lcd_limit


lcd_limit:
		lds temp2,status
		cpi temp2,NUMSTATIONS
		breq num_print
		cpi temp2,STATIONNAMES
		breq letter_print
		cpi temp2,TIMESTATIONS
		breq num_print
		cpi temp2,STOPTIME
		breq num_print

num_print:
	add temp,tempNum;;;;;;;;;;;UNCOMMENT DEBUG  (UNCOMMENT)
	;ldi temp,14
	mov r16,temp
	do_lcd_data_mov
	jmp continue
	
letter_print:
	;ldi temp,14						/////UNCOMMENT TO DEBUG
													
	cpi tempNum,55	;;if it's a letter we go into letter processing
	breq process_letters
	add temp,tempNum;;;;;;;;;;;UNCOMMENT DEBUG  (UNCOMMENT)
	jmp continue 	

process_letters:	;;if we hit a letter have to process number + letter pair to make a ascii letter
	ldi yl,low(temp_letters)
	ldi yh,high(temp_letters)	
	ld temp,y+		;;temp holds an integer number
		
		cpi temp,2
		breq process_2
		cpi temp,3
		breq process_3
		cpi temp,4
		breq process_4
		cpi temp,5
		breq process_5
		cpi temp,6					;;;if else block to process numbers 2-9 and set them up for letter processing
		breq process_6
		cpi temp,7
		breq process_7
		cpi temp,8
		breq process_8
		cpi temp,9
		breq process_9
		

	continue_process_letters:

	ld temp2,y+	;;holds the letter to symbolise which number letter user wants from keypad
		
		cpi temp2,10
		breq process_A
		cpi temp2,11
		breq process_B
		cpi temp2,12
		breq process_C
		cpi temp2,13
		breq process_D


	finish_process_letters:	
	add temp,temp2
	add temp,tempNum
	mov r16,temp
	do_lcd_data_mov
	add temp,tempNum ;;add tempNum again as it will be subtracted in continue, we want to add ascii ready names to Station_names dseg
	ldi yl,low(temp_letters)
	ldi yh,high(temp_letters)
	jmp continue	;; watch out in continue it has sub temp,tempNum DEBUG ALERT

process_A:
	ldi temp2,0
	jmp finish_process_letters	

process_B:
	ldi temp2,1
	jmp finish_process_letters	

process_C:
	ldi temp2,2
	jmp finish_process_letters	
process_D:
	ldi temp2,3
	jmp finish_process_letters	

process_2:
	ldi temp,10
	jmp continue_process_letters

process_3:
	ldi temp,13
	jmp continue_process_letters

process_4:
	ldi temp,16
	jmp continue_process_letters

process_5:
	ldi temp,19
	jmp continue_process_letters

process_6:
	ldi temp,22
	jmp continue_process_letters

process_7:
	ldi temp,25
	jmp continue_process_letters

process_8:
	ldi temp,29
	jmp continue_process_letters

process_9:
	ldi temp,32
	jmp continue_process_letters


continue:
	sub temp,tempNum    ;;;;;;; UNCOMMENT DEBUG
	;ldi temp,14  ;;COMMENT TO DEBUG
	sleep50ms
	sleep50ms
	sleep50ms						////UNCOMMENT TO STOP DEBUG
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

sleep_50ms:
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
	ret

sleep_1s:
	sleep200ms
	sleep200ms
	sleep200ms
	sleep200ms
	sleep200ms	
	ret