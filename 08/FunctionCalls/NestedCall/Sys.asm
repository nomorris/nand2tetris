@256
D=A
@SP
M=D
(Sys.vm$Sys.init)
@4000
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
@THIS
M=D
@5000
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
@THAT
M=D
(Sys.vm$end0)
@Sys.vm$end0
D=M
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
@Sys.vm$Sys.main
0;JMP
(Sys.vm$end0)
@5
D=A
@1
D=A+D
@13
M=D
@SP
A=M-1
D=M
@13
A=M
M=D
@SP
M=M-1
(Sys.vm$LOOP)
@Sys.vm$LOOP
0;JMP
(Sys.vm$Sys.main)
@SP
A=M
M=0
@SP
M=M+1
@SP
A=M
M=0
@SP
M=M+1
@SP
A=M
M=0
@SP
M=M+1
@SP
A=M
M=0
@SP
M=M+1
@SP
A=M
M=0
@SP
M=M+1
@4001
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
@THIS
M=D
@5001
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
@THAT
M=D
@200
D=A
@SP
A=M
M=D
@SP
M=M+1
@LCL
D=M
@1
D=A+D
@13
M=D
@SP
A=M-1
D=M
@13
A=M
M=D
@SP
M=M-1
@40
D=A
@SP
A=M
M=D
@SP
M=M+1
@LCL
D=M
@2
D=A+D
@13
M=D
@SP
A=M-1
D=M
@13
A=M
M=D
@SP
M=M-1
@6
D=A
@SP
A=M
M=D
@SP
M=M+1
@LCL
D=M
@3
D=A+D
@13
M=D
@SP
A=M-1
D=M
@13
A=M
M=D
@SP
M=M-1
@123
D=A
@SP
A=M
M=D
@SP
M=M+1
(Sys.vm$end0)
@Sys.vm$end0
D=M
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
@Sys.vm$Sys.add12
0;JMP
(Sys.vm$end0)
@5
D=A
@0
D=A+D
@13
M=D
@SP
A=M-1
D=M
@13
A=M
M=D
@SP
M=M-1
@LCL
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
@1
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
@LCL
D=M
@2
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
@LCL
D=M
@3
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
@LCL
D=M
@4
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
A=A-1
M=D+M
@SP
AM=M-1
D=M
A=A-1
M=D+M
@SP
AM=M-1
D=M
A=A-1
M=D+M
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
D=D-A
@LCL
M=D
@ARG
D=M
@0
D=A+D
@13
M=D
@SP
A=M-1
D=M
@13
A=M
M=D
@SP
M=M-1
@ARG
D=M+1
@15
A=M-1
D=M
@THAT
M=D
@15
A=M-1
A=A-1
D=M
@THIS
M=D
@Sys.vm.$end0
0;JMP
(Sys.vm$Sys.add12)
@4002
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
@THIS
M=D
@5002
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
@THAT
M=D
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
@12
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
M=D+M
@LCL
D=M
@15
M=D
@5
D=D-A
@LCL
M=D
@ARG
D=M
@0
D=A+D
@13
M=D
@SP
A=M-1
D=M
@13
A=M
M=D
@SP
M=M-1
@ARG
D=M+1
@15
A=M-1
D=M
@THAT
M=D
@15
A=M-1
A=A-1
D=M
@THIS
M=D
@Sys.vm.$end0
0;JMP
(END)
@END
0;JMP