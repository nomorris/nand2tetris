    
    @16383
    D=A
    @R0
    M=D
(E)
    @16383
    D=A
    @R0
    M=D
(ERASE)

    @KBD
    D=M
    @DE
    D;JGT
    
    @R0
    D=M
    A=D+A
    M=0
    
    @24576
    D=A
    @R0
    D=M-D
    @E
    D;JEQ
    
    @R0
    M=M+1
    
    @ERASE
    0;JMP
(DE)
    @16383
    D=A
    @R0
    M=D
(DRAW)
    @KBD
    D=M
    @E
    D;JEQ

    
    @R0
    D=M
    A=D+A
    M=-1
    
    @24576
    D=A
    @R0
    D=M-D
    @DE
    D;JEQ
    
    @R0
    M=M+1
        
    @DRAW
    0;JMP