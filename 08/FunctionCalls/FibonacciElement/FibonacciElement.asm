@256
D=A
@SP
M=D
@Sys.o$end-1
D=A
@SP
A=M
M=D
@SP
M=M+1
@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1
@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1
@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1
@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1
@SP
D=M
@LCL
M=D
@5
D=D-A
@0
D=D-A
@ARG
M=D
@Sys.init
0;JMP
(Sys.o$end-1)
(Sys.init)
@4
D=A
@SP
A=M
M=D
@SP
M=M+1
@Sys.init$end0
D=A
@SP
A=M
M=D
@SP
M=M+1
@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1
@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1
@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1
@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1
@SP
D=M
@LCL
M=D
@5
D=D-A
@1
D=D-A
@ARG
M=D
@Main.fibonacci
0;JMP
(Sys.init$end0)
(Sys.init$END)
@Sys.init$END
0;JMP
(Main.fibonacci)
@ARG
D=M
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
@2
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
A=A-1
A=M
D=A-D
@TRUE0
D;JLT
@SP
A=M-1
M=0
@FALSE0
0;JMP
(TRUE0)
@SP
A=M-1
M=-1
(FALSE0)
@SP
AM=M-1
D=M
@Main.fibonacci$N_LT_2
D;JNE
@Main.fibonacci$N_GE_2
0;JMP
(Main.fibonacci$N_LT_2)
@ARG
D=M
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
@LCL
D=M
@15
M=D
@5
A=D-A
D=M
@14
M=D
@SP
AM=M-1
D=M
@ARG
A=M
M=D
@ARG
D=M+1
@SP
M=D
@15
AM=M-1
D=M
@THAT
M=D
@15
AM=M-1
D=M
@THIS
M=D
@15
AM=M-1
D=M
@ARG
M=D
@15
AM=M-1
D=M
@LCL
M=D
@14
A=M
0;JMP
(Main.fibonacci$N_GE_2)
@ARG
D=M
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
@2
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
A=A-1
M=M-D
@Main.fibonacci$end1
D=A
@SP
A=M
M=D
@SP
M=M+1
@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1
@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1
@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1
@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1
@SP
D=M
@LCL
M=D
@5
D=D-A
@1
D=D-A
@ARG
M=D
@Main.fibonacci
0;JMP
(Main.fibonacci$end1)
@ARG
D=M
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
@1
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
A=A-1
M=M-D
@Main.fibonacci$end2
D=A
@SP
A=M
M=D
@SP
M=M+1
@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1
@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1
@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1
@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1
@SP
D=M
@LCL
M=D
@5
D=D-A
@1
D=D-A
@ARG
M=D
@Main.fibonacci
0;JMP
(Main.fibonacci$end2)
@SP
AM=M-1
D=M
A=A-1
M=D+M
@LCL
D=M
@15
M=D
@5
A=D-A
D=M
@14
M=D
@SP
AM=M-1
D=M
@ARG
A=M
M=D
@ARG
D=M+1
@SP
M=D
@15
AM=M-1
D=M
@THAT
M=D
@15
AM=M-1
D=M
@THIS
M=D
@15
AM=M-1
D=M
@ARG
M=D
@15
AM=M-1
D=M
@LCL
M=D
@14
A=M
0;JMP
