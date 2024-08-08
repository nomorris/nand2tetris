     @0
     D=M
     @3
     M=-D 
     @2
     M=0
(LOOP)
     @3
     D=M
     @END
     D;JGE 
     @3
     M=M+1
     @2
     D=M 
     @1
     D=D+M
     @2
     M=D
     @LOOP
     0;JMP // Goto LOOP
(END)
     @END
     0;JMP // Infinite loop