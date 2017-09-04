
;THIS CONVERTOR GOOD ONLY UNTIL WORD VALS*************
                       
Data segment
    
OPENMSG1 DB "PLEASE ENTER THE BASE YOU NEED",10,13,'$'
OPENMSG2 DB "B->BINARY,D->DECIMAL O->OCTALY,H ->HEX", 10,13,'$'
OPENMSG3 DB "PLEASE ENTER THE NUMBER YOU NEED",10,13,'$'
MSG4 DB  "YOU CHOOSE BASE :",'$'
MSG5 DB  "THE NUMBER IS: ",'$'
WRONG_NUMBER_MSG DB "YOU ENTER A WRONG NUMBER",10,13,'$'    
CASEB DB "IN BASE BINARY :         ",'$'
CASEO DB "IN BASE OCTALY  :      ",'$'    
CASED DB "IN BASE DECIMALY  :      ",'$'    
CASEH DB "IN BASE HEX  :       ",'$'
CONTMSG DB "DO YOU WISH TO CONVRER A NOTHER NUMBER ? <Y/N> ",10,13,'$'
B EQU 16
H EQU 4
D EQU 5                              ;FIRST I CONVERT THE STRING TO 
O EQU 6                              ;INTEGER NUMBER
NEWLINE DB 10,13,'$'
CHOSEN_BASE DB ?                     ;AND THE I CINVERT ALL BASES
INB DB B+1,?,B+1 DUP ('$')                  
INB1 DB 16 DUP ('0'),'$'         ;FOR EACH BASE I CREATE:
INH DB H+1,?,H+1 DUP ('$') ;<--- STRING FOR USER INPUT
INH1 DB 4 DUP ('0') ,'$'   ;<--- ARRAY TO WORK WITH
IND DB D+1,?,D+1 DUP ('$')
IND1 DB 5 DUP ('0') ,'$'
INO DB O+1,?,O+1 DUP ('$')
INO1 DB 6 DUP ('0'),'$'
LEN DW 0                    ;HOW MANY DIGIT IN THE INPUT ARRAY                           
DECVAL DW 0                 ;DECIMAL VALUE OF INPUT STARING 
MULTYNUM DW 1
DIVID DW ?      
BASE DW ?      
               
data ends

sseg segment stack
  DW 100 dup  (?)
sseg ends

code segment
                  
assume cs:code,ds:data,ss:sseg

start:   mov ax,data
         mov ds,ax 
          
INPUT:   XOR AX,AX
         XOR DX,DX
         MOV DX,OFFSET OPENMSG1
         MOV AH,9               ;OPENNING STATMENT
         INT 21H
         MOV DX,OFFSET OPENMSG2
         MOV AH,9
         INT 21H 
                 ;INPUT OF CHOOSEN BASE
         MOV AH,1;STORE IT IN A VAR
         INT 21H 
         MOV CHOSEN_BASE ,AL
         LEA DX,NEWLINE
         MOV AH,9
         INT 21H 
        
         XOR CX,CX      
         ;CHECH WHICH BASE WAS CHOOSEN  
 CHECK1: CMP CHOSEN_BASE,'B'
         JNE CHECK2
         LEA DX,INB    ;INPUT TO STRING
		 MOV AH,0AH
		 INT 21H
		 XOR CX,CX 
         XOR AX,AX 
         LEA SI,INB[1]   ;SI - > LEN
         MOV CL,[SI]   ; MAKE COUNTER 
         MOV LEN ,CX
         LEA DI,INB[2]  ; DI=> INB[0]
 VALIDB: CMP [DI],'0'
         JE CONTB      ;CHECH IF VALID BINARY BUMBER
         CMP [DI],'1'
         JE CONTB
         JMP WRONG_NUM  ;WRONG NUM MSG
 CONTB:  INC DI
         LOOP VALIDB    
         ADD SI,LEN        ;SI=>STRING[LSB]
         LEA DI,INB1[15]   ;DI =ARR[LSB]
         PUSH DI
         PUSH SI
         PUSH LEN
         CALL COPY_STRING_ARR    
 
         LEA  DI,INB1[15]
         MOV  BASE,2
         PUSH DECVAL
         PUSH BASE
         PUSH MULTYNUM 
         PUSH DI
         PUSH LEN
         CALL CON_STR_TO_DECVAL  ;FUNC THAT CONVERT STRING TO 
         POP  DECVAL   ;DECVAL = DECIMAL VALUE OF THE INPUT STRING
                        
         MOV  DIVID,8      ;OCTALY BASE = 8
         LEA  DI,INO1[5]
         PUSH DECVAL
         PUSH DIVID
         PUSH DI
         CALL DEC_TO_NUM      ;CONVERT DECIMAL VALUE INTO OCTALY BASE
        
        
        LEA DI,INH1[3]
        MOV DIVID ,16       ;CONVERT THE INTEGER INTO HEXA NUMBER
        PUSH DECVAL
        PUSH DIVID
        PUSH DI
        CALL DEC_TO_NUM
        
        LEA DI,IND1[4]     ;CONVERT THE INTEGER INTO DECIMAL NUMBER
        MOV DIVID,10
        PUSH DECVAL
        PUSH DIVID
        PUSH DI
        CALL DEC_TO_NUM  
        
        LEA DI,INB1[15] 
        MOV DIVID ,2      ;CONVERT THE INTEGER INTO BINARY NUMBER
        PUSH DECVAL
        PUSH DIVID
        PUSH DI
        CALL DEC_TO_NUM
  ; PRINT ALL NUMBERS**********      
        LEA DX,MSG5
        MOV AH,9
        INT 21H
        LEA DX, INB1   ;PRINT BIN
        MOV AH,9
        INT 21H              
        LEA DX,NEWLINE
        
        LEA DX,CASEB
        MOV AH,9
        INT 21H  
        LEA DX,NEWLINE
        MOV AH,9
        INT 21H              
        LEA DX,CASED    ;PRINT DECIMAL
        MOV AH,9
        INT 21H                 
        LEA DX,IND1
        MOV AH,9
        INT 21H  
        LEA DX,NEWLINE
        MOV AH,9
        INT 21H 
        LEA DX,CASEH     ;PRINT HEXA
        MOV AH,9
        INT 21H  
       
        LEA DX,INH1 
        MOV AH,9
        INT 21H  
        LEA DX,NEWLINE
        MOV AH,9
        INT 21H 
        LEA DX,CASEO    ;PRINT OCTALY
        MOV AH,9
        INT 21H 
        LEA DX,INO1
        MOV AH,9
        INT 21H
        MOV DX,OFFSET NEWLINE
        MOV AH,9
        INT 21H    
       
        JMP IF_CONT 

              ;CONVERT DECOMAL VALUE INTO HEXA BASE 
CHECK2: CMP CHOSEN_BASE,'D'
        JNE CHECK3
        XOR AX,AX
        XOR CX,CX
        LEA DX, IND
		MOV AH,0AH
		INT 21H
		LEA DX,NEWLINE
        MOV AH,9
        INT 21H 
        XOR AX,AX
        XOR CX,CX
        LEA DI,IND[1]
        MOV CL,[DI]   ; MAKE COUNTER
        MOV LEN,CX
        LEA DI,IND[2] 
        PUSH LEN
        PUSH DI
        CALL VALID_DEC_NUMBER ;FUNC TO CHECK IF ALL INPUT  
                              ;IS CURRECT
        POP AX
        CMP AX,0 
        JE WRONG_NUM
        
        LEA SI,IND[1]
        ADD SI,LEN      ;DI = LSB OF IND1
        LEA DI,IND1[4]
        PUSH DI
        PUSH SI
        PUSH LEN
        CALL COPY_STRING_ARR 
         
        LEA DI,IND1[4]
        MOV BASE,10
        PUSH DECVAL
        PUSH BASE
        PUSH MULTYNUM 
        PUSH DI
        PUSH LEN
        CALL CON_STR_TO_DECVAL  
        
        POP DECVAL
        MOV DIVID ,2
        LEA DI,INB1[15]
        PUSH DECVAL
        PUSH DIVID
        PUSH DI
        CALL DEC_TO_NUM     ;CONVERT DECIMAL VALUE INTO BINARY BASE
        
        
        LEA DI,INH1[3]
        MOV DIVID ,16
        PUSH DECVAL
        PUSH DIVID
        PUSH DI
        CALL DEC_TO_NUM 
        
         MOV DIVID,8 
         LEA DI,INO1[5]
         PUSH DECVAL
         PUSH DIVID
         PUSH DI
         CALL DEC_TO_NUM 
         
         LEA DI,IND1[4]
         MOV DIVID,10
         PUSH DECVAL
         PUSH DIVID
         PUSH DI
         CALL DEC_TO_NUM
         
         
                 
        LEA DX,MSG5
        MOV AH,9
        INT 21H
        LEA DX, IND1
        MOV AH,9
        INT 21H 
        LEA DX,NEWLINE
        
        LEA DX,CASED
        MOV AH,9
        INT 21H  
        LEA DX,NEWLINE
        MOV AH,9
        INT 21H 
        LEA DX,CASEB 
        MOV AH,9
        INT 21H
        LEA DX,INB1
        MOV AH,9
        INT 21H  
        LEA DX,NEWLINE
        MOV AH,9
        INT 21H 
        LEA DX,CASEH
        MOV AH,9
        INT 21H  
       
        LEA DX,INH1 
        MOV AH,9
        INT 21H  
        LEA DX,NEWLINE
        MOV AH,9
        INT 21H 
        LEA DX,CASEO
        MOV AH,9
        INT 21H 
        LEA DX,INO1
        MOV AH,9
        INT 21H
        LEA DX,NEWLINE
        MOV AH,9
        INT 21H   
     JMP IF_CONT  
           
    
           
        
 CHECK3: CMP CHOSEN_BASE,'H'
         JNE CHECK4 
         XOR AX,AX
         XOR CX,CX
         LEA DX, INH
		 MOV AH,0AH
		 INT 21H 
		 LEA DX,NEWLINE
         MOV AH,9
         INT 21H
         LEA SI,INH[1]
         MOV CL,[SI]
         MOV LEN,CX
         LEA DI,INH[2]   ;CHECL IF ALL CHARS IN STRING 
 VALIDH: CMP [DI],'0'    ; BETWEEN 0-9 AND A-F
         JB  WRONG_NUM
         CMP [DI],'9'
         JBE CONTH      
         CMP [DI],'A'
         JB  WRONG_NUM
         CMP [DI],'F'
         JA WRONG_NUM 
   CONTH:INC DI
         LOOP VALIDH      
         LEA SI,INH[1]
         ADD SI,LEN      ;DI = LSB OF IND1
         LEA DI,INH1[3]
         PUSH DI
         PUSH SI
         PUSH LEN
         CALL COPY_STRING_ARR 
         LEA DI,INH1[3]
         MOV BASE,16
         PUSH DECVAL
         PUSH BASE
         PUSH MULTYNUM 
         PUSH DI
         PUSH LEN
         CALL CON_STR_TO_DECVAL
         POP DECVAL      
         MOV DIVID ,2
         LEA DI,INB1[15]
         PUSH DECVAL
         PUSH DIVID
         PUSH DI
         CALL DEC_TO_NUM
         
         MOV DIVID,8 
         LEA DI,INO1[5]
         PUSH DECVAL
         PUSH DIVID
         PUSH DI
         CALL DEC_TO_NUM 
         
         LEA DI,IND1[4]
         MOV DIVID,10
         PUSH DECVAL
         PUSH DIVID
         PUSH DI
         CALL DEC_TO_NUM 
         
         LEA DI,INH1[3]
         MOV DIVID ,16
         PUSH DECVAL
         PUSH DIVID
         PUSH DI
         CALL DEC_TO_NUM
                      
        LEA DX,MSG5
        MOV AH,9
        INT 21H
        LEA DX, INH1
        MOV AH,9
        INT 21H 
        LEA DX,NEWLINE
        
        LEA DX,CASEH
        MOV AH,9
        INT 21H  
        LEA DX,NEWLINE
        MOV AH,9
        INT 21H 
        LEA DX,CASEB 
        MOV AH,9
        INT 21H
        LEA DX,INB1
        MOV AH,9
        INT 21H  
        LEA DX,NEWLINE
        MOV AH,9
        INT 21H 
        LEA DX,CASED
        MOV AH,9
        INT 21H  
       
        LEA DX,IND1 
        MOV AH,9
        INT 21H  
        LEA DX,NEWLINE
        MOV AH,9
        INT 21H 
        LEA DX,CASEO
        MOV AH,9
        INT 21H 
        LEA DX,INO1
        MOV AH,9
        INT 21H
        LEA DX,NEWLINE
        MOV AH,9
        INT 21H   
        JMP IF_CONT
       
          
          
 CHECK4:CMP CHOSEN_BASE,'O'
            XOR AX,AX
            XOR CX,CX
            LEA DX, INO
		    MOV AH,0AH
		    INT 21H  
	  	    LEA DX,NEWLINE
            MOV AH,9
            INT 21H
            LEA SI,INO[1]
            MOV CL,[SI]
            MOV LEN,CX
            LEA DI,INO[2]
            PUSH LEN
            PUSH DI
            CALL CHECK_VALID_OCT   
            POP AX
            CMP AX,0
            JE WRONG_NUM
            LEA SI,INO[1]
            ADD SI,LEN      ;DI = LSB OF IND1
            LEA DI,INO1[5]
            PUSH DI
            PUSH SI
            PUSH LEN
            CALL COPY_STRING_ARR
             
            LEA DI,INO1[5]
            MOV BASE,8
            PUSH DECVAL
            PUSH BASE
            PUSH MULTYNUM 
            PUSH DI
            PUSH LEN
            CALL CON_STR_TO_DECVAL
            POP DECVAL
       
            MOV DIVID ,2
            LEA DI,INB1[15]
            PUSH DECVAL
            PUSH DIVID
            PUSH DI
            CALL DEC_TO_NUM
         
       
            LEA DI,IND1[4]
            MOV DIVID,10
            PUSH DECVAL
            PUSH DIVID
            PUSH DI
            CALL DEC_TO_NUM 
               
             
            LEA DI,INH1[3]
            MOV DIVID ,16
            PUSH DECVAL
            PUSH DIVID
            PUSH DI
            CALL DEC_TO_NUM   
            
            MOV DIVID,8 
            LEA DI,INO1[5]
            PUSH DECVAL
            PUSH DIVID
            PUSH DI
            CALL DEC_TO_NUM 
             
                      
        LEA DX,MSG5
        MOV AH,9
        INT 21H
        LEA DX, INO1
        MOV AH,9
        INT 21H 
        LEA DX,NEWLINE
        
        LEA DX,CASEO
        MOV AH,9
        INT 21H  
        LEA DX,NEWLINE
        MOV AH,9
        INT 21H 
        LEA DX,CASEB 
        MOV AH,9
        INT 21H
        LEA DX,INB1
        MOV AH,9
        INT 21H  
        LEA DX,NEWLINE
        MOV AH,9
        INT 21H 
        LEA DX,CASEH
        MOV AH,9
        INT 21H  
       
        LEA DX,INH1 
        MOV AH,9
        INT 21H  
        LEA DX,NEWLINE
        MOV AH,9
        INT 21H 
        LEA DX,CASED
        MOV AH,9
        INT 21H 
        LEA DX,IND1
        MOV AH,9
        INT 21H
        LEA DX,NEWLINE
        MOV AH,9
        INT 21H   
        JMP IF_CONT
            
                                       
     
 WRONG_NUM:LEA DX,WRONG_NUMBER_MSG 
           MOV AH,9
           INT 21H
           LEA DX,NEWLINE
           MOV AH,9
           INT 21H 
 IF_CONT:MOV DX,OFFSET CONTMSG
         MOV AH,9
         INT 21H
         LEA DX,NEWLINE
         MOV AH,9
         INT 21H 
        
        MOV AH,1
        INT 21H
        CMP AL,'Y'
        JE CONTI
        CMP AL,'N'
        JE NOT_AGAIN
        LEA DX,WRONG_NUMBER_MSG 
        MOV AH,9
        INT 21H
        LEA DX,NEWLINE
        MOV AH,9
        INT 21H   
        JMP IF_CONT   
        CONTI: PUSH OFFSET INB1
        PUSH OFFSET IND1
        PUSH OFFSET INH1
        PUSH OFFSET INO1      
        CALL CLEAR_ARRS 
        PUSH OFFSET INB
        PUSH OFFSET INH
        PUSH OFFSET IND
        PUSH OFFSET INO
        CALL CLEAR_STRINGS
        MOV DECVAL,0
        LEA DX,NEWLINE
        MOV AH,9
        INT 21H
        XOR DX,DX
        XOR AX,AX 
        
        JMP INPUT
 NOT_AGAIN:       
        JMP EXIT          
   

exit:    mov ah,4ch
         int 21h 
         
       COPY_STRING_ARR PROC
        XOR AX,AX 
        MOV BP,SP
        MOV CX,[BP+2]
        MOV DI,[BP+6]
        MOV SI,[BP+4]
 COPY: 
        MOV AL,[SI]        ;COPY FROM INPUT STRING
        CMP AL,'9'         ;AND MAKE A NUMBER FROM CHAR
        JA SUBING   
        SUB AL,30H
        JMP CONT4 
 SUBING:SUB AL,37H       
 CONT4: MOV [DI],AL
        DEC DI
        DEC SI
        LOOP COPY             
        RET 8
        COPY_STRING_ARR ENDP  
 

       
   CON_STR_TO_DECVAL PROC
         MOV BP,SP                           
         XOR AX,AX
         XOR BX,BX
         XOR CX,CX                           ;STACK:
         XOR DX,DX 
         MOV BX,[BP+8]   ;BX=BASE
         MOV DI,[BP+4]   ;DI=> INB1[LSB]     ;DECIMAL->SUM THE INTEGER         
         MOV CX,[BP+2]   ;COUNTER            ;BASE   ->2,8,10,16
 CONVSD: PUSH CX                             ;MULTYNUM->1
         MOV CX,[BP+6]   ;                   ;DI     ->
         MOV AL ,[DI]    ;                   ;LEN    
         CBW                         ;   BP->FUNC 
         MUL CX                      ;       CX
         ADD  [BP+10],AX ;
         MOV AX,[BP+6]
         MUL BX
         MOV [BP+6],AX
         DEC DI
         POP CX
         LOOP CONVSD
         RET 8        
    CON_STR_TO_DECVAL ENDP 
         
    
    
       DEC_TO_NUM PROC ;UNIVERSAL FUNC FOR ALL BASES
        
        MOV BP,SP
        XOR AX,AX
        XOR CX,CX
        XOR DX,DX
        MOV SI,[BP+2]  ;SI=> ARR[LSB]
        MOV AX,[BP+6]  ;AX CONTAIN THE INTEGER NUMBER
        MOV CX,[BP+4]  ;CA=LEN
 LOOPDH:DIV CX 
        CMP DL,9       ;0-9 ADD 30H
        JA ADDING      ;A-F ADD 37H
        ADD DL,30H
        JMP CONT6
 ADDING:ADD DL,37H       
  CONT6:MOV [SI],DL
        XOR DL,DL
        DEC SI
        CMP AL,0     ;CONDITION TO STOP THE LOOP 
        JNE LOOPDH
        RET 6
        
       DEC_TO_NUM ENDP
       
    
       
       VALID_DEC_NUMBER PROC 
        
        MOV BP,SP
        XOR CX,CX
        MOV CX,[BP+4]
        MOV DI,[BP+2]
  LULA: CMP [DI],'0'
        JB FINISH_WRONG     ;CHECK 0-9
        CMP [DI],'9'
        JA FINISH_WRONG
        INC DI
        LOOP LULA
        MOV DI,[BP+2]
        MOV CX,[BP+4]          ;TOP NUMBER FOR DECIMAL WORD
        CMP CX,5               ; 65535
        JNE FINISH_GOOD        ;CHECK ONLY IF  LEN=5
        CMP [DI],'6'  
        JA  FINISH_WRONG
        JE NEXT
        JMP FINISH_GOOD 
 NEXT: INC DI
       CMP [DI],'5'
         JA  FINISH_WRONG
        JE NEXT1
        JMP FINISH_GOOD
 NEXT1:INC DI
       CMP [DI],'5'
         JA  FINISH_WRONG
        JE NEXT2
        JMP FINISH_GOOD
NEXT2:  INC DI
        CMP [DI],'3'
        JA  FINISH_WRONG
        JE NEXT3
        JMP FINISH_GOOD
NEXT3:  INC DI
         CMP [DI],'5'
        JA  FINISH_WRONG
        JE NEXT
        JMP FINISH_GOOD
                  
FINISH_GOOD:MOV [BP+4],1
            RET 2        
FINISH_WRONG:MOV [BP+4],0
             RET 2                           
                            
       VALID_DEC_NUMBER ENDP 
                       
                       
                       
       CHECK_VALID_OCT PROC
        MOV BP,SP
        XOR CX,CX
        MOV CX,[BP+4]
        MOV DI,[BP+2]
  LULAA: CMP [DI],'0'
        JB FINISH_WRONG     ;CHECK 0-9
        CMP [DI],'7'
        JA FINISH_WRONG
        INC DI
        LOOP LULAA
        MOV DI,[BP+2]
        MOV CX,[BP+4]          ;TOP NUMBER FOR OCTALY WORD
        CMP CX,6               ; 177777
        JNE FINISH_GOOD        ;CHECK ONLY IF  LEN=6
        CMP [DI],'1'           ;IN CASE OF LEN = 6
        JA  FINISH_WRONG       ;CHECK FIRST DIGIT
                  
FINISH_GOODD:MOV [BP+4],1
            RET 2        
FINISH_WRONGG:MOV [BP+4],0
             RET 2 
       CHECK_VALID_OCT ENDP  
       
       
       CLEAR_ARRS PROC
        MOV BP,SP
        MOV DI,[BP+8]
        MOV CX,B
   LUL: MOV [DI],'0'
        INC DI
        LOOP LUL     
       
        
        MOV DI,[BP+6]
        MOV CX,D
   LUL1:MOV [DI],'0'
        INC DI
        LOOP LUL1 
       
        
        MOV DI,[BP+4]
        MOV CX,H
   LUL2: MOV [DI],'0'
        INC DI
        LOOP LUL2 
            
        
        MOV DI,[BP+2]
        MOV CX,O
   LUL3:MOV [DI],'0'
        INC DI
        LOOP LUL3     
       
        
        XOR AX,AX
        XOR BX,BX 
        XOR CX,CX
        XOR DX,DX
        
        RET 8  ;=>RETURN EMPTY STACK
        
       CLEAR_ARRS ENDP 
       
        CLEAR_STRINGS  PROC
            
        MOV BP,SP
        MOV DI,[BP+8]
        MOV CX,B+2
 LUL4:  MOV [DI],'$'
        INC DI
        LOOP LUL4 
                 
            
        MOV DI,[BP+6]
        MOV CX,H+2
 LUL5:  MOV [DI],'$'
        INC DI
        LOOP LUL5    
        
        MOV DI,[BP+4]
        MOV CX,D+2
 LUL6:  MOV [DI],'$'
        INC DI
        LOOP LUL6
        
        MOV DI,[BP+2]
        MOV CX,O+2
 LUL7:  MOV [DI],'$'
        INC DI
        LOOP LUL7
        RET 8
        CLEAR_STRINGS  ENDP
       
       
       
       
code ends
end start


