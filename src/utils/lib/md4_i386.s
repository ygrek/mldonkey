	.text
       .align    4,0x90

	.globl   MD4Init
	.type	MD4Init,@function
MD4Init:
.L11:                          
        movl      4(%esp), %edx                                 
        xorl      %eax, %eax                                    
        movl      %eax, 20(%edx)                                
        movl      %eax, 16(%edx)                                
        movl      $1732584193, (%edx)                           
        movl      $-271733879, 4(%edx)                          
        movl      $-1732584194, 8(%edx)                         
        movl      $271733878, 12(%edx)                          
        ret                                                     
        .align    4,0x90
                                

	.size	MD4Init,.-MD4Init
	.data

	.data
	.text


       .align    4,0x90



	.globl   MD4Update
	.type	MD4Update,@function
MD4Update:
.L21:                          
        pushl     %edi                                          
        pushl     %esi                                          
        pushl     %ebp                                          
        pushl     %ebx                                          
        subl      $8, %esp                                      
        movl      28(%esp), %ebp                                
        movl      36(%esp), %ecx                                
        movl      16(%ebp), %edx                                
        movl      %edx, %eax                                    
        shrl      $3, %eax                                      
        andl      $63, %eax                                     
        lea       (%ecx,%ecx), %ebx                             
        addl      %ebx, %ebx                                    
        addl      %ebx, %ebx                                    
        lea       (%edx,%ecx,8), %edx                           
        movl      %edx, 16(%ebp)                                
        cmpl      %ebx, %edx                                    
        jae       .L23         
                                
.L22:                          
        movl      20(%ebp), %edx                                
        incl      %edx                                          
        jmp       .L24         
                                
.L23:                          
        movl      20(%ebp), %edx                                
                                
.L24:                          
        movl      36(%esp), %esi                                
        movl      %esi, %ebx                                    
        shrl      $29, %ebx                                     
        addl      %ebx, %edx                                    
        movl      %edx, 20(%ebp)                                
        movl      %eax, %edx                                    
        negl      %edx                                          
        lea       64(%edx), %ebx                                
        movl      %edx, 4(%esp)                                 
        cmpl      %ebx, %esi                                    
        jae       .L26         
                                
.L25:                          
        xorl      %ebx, %ebx                                    
        jmp       .L214        
                                
.L26:                          
        lea       24(%ebp,%eax), %edi                           
        movl      32(%esp), %esi                                
        movl      %ebx, %ecx                                    
        rep                                                     
        movsb                                                   
                                
.L27:                          
        lea       24(%ebp), %eax                                
        pushl     %eax                                          
        pushl     %ebp                                          
        call      MD4Transform                                  
                                
.L218:                         
        addl      $8, %esp                                      
                                
.L28:                          
        movl      4(%esp), %edx                                 
        addl      $127, %edx                                    
        movl      36(%esp), %eax                                
        cmpl      %edx, %eax                                    
        jbe       .L213        
                                
.L29:                          
        lea       63(%ebx), %edi                                
        movl      32(%esp), %eax                                
        lea       (%ebx,%eax), %esi                             
        
                                
.L210:                         
        pushl     %esi                                          
        pushl     %ebp                                          
        call      MD4Transform                                  
                                
.L219:                         
        addl      $8, %esp                                      
                                
.L211:                         
        addl      $64, %edi                                     
        addl      $64, %esi                                     
        addl      $64, %ebx                                     
        movl      36(%esp), %eax                                
        cmpl      %edi, %eax                                    
        ja        .L210        
                                
.L213:                         
        xorl      %eax, %eax                                    
                                
.L214:                         
        lea       24(%ebp,%eax), %edi                           
        movl      32(%esp), %eax                                
        lea       (%eax,%ebx), %esi                             
        movl      36(%esp), %eax                                
        subl      %ebx, %eax                                    
        movl      %eax, %ecx                                    
        rep                                                     
        movsb                                                   
                                
.L215:                         
        addl      $8, %esp                                      
        popl      %ebx                                          
        popl      %ebp                                          
        popl      %esi                                          
        popl      %edi                                          
        ret                                                     
        .align    4,0x90
                                

	.size	MD4Update,.-MD4Update
	.data

	.data
	.type	PADDING,@object
PADDING:
	.byte 128	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.size	PADDING,64
	.text


       .align    4,0x90


	.globl   MD4Final
	.type	MD4Final,@function
MD4Final:
.L31:                          
        pushl     %ebp                                          
        subl      $8, %esp                                      
        movl      20(%esp), %ebp                                
        lea       (%esp), %edx                                  
        lea       16(%ebp), %eax                                
        push      $8                                            
        pushl     %eax                                          
        pushl     %edx                                          
        call      Encode                                        
                                
.L312:                         
        addl      $12, %esp                                     
                                
.L32:                          
        movl      16(%ebp), %eax                                
        shrl      $3, %eax                                      
        andl      $63, %eax                                     
        cmpl      $56, %eax                                     
        jae       .L34         
                                
.L33:                          
        negl      %eax                                          
        addl      $56, %eax                                     
        jmp       .L35         
                                
.L34:                          
        negl      %eax                                          
        addl      $120, %eax                                    
                                
.L35:                          
        pushl     %eax                                          
        push      $PADDING                                      
        pushl     %ebp                                          
        call      MD4Update                                     
                                
.L36:                          
        lea       12(%esp), %eax                                
        push      $8                                            
        pushl     %eax                                          
        pushl     %ebp                                          
        call      MD4Update                                     
                                
.L37:                          
        push      $16                                           
        pushl     %ebp                                          
        pushl     48(%esp)                                      
        call      Encode                                        
                                
.L313:                         
        addl      $36, %esp                                     
                                
.L38:                          
        xorl      %eax, %eax                                    
        andl      $255, %eax                                    
        movl      %eax, %edx                                    
        shll      $8, %edx                                      
        orl       %edx, %eax                                    
        movl      %eax, %ecx                                    
        shll      $16, %ecx                                     
        orl       %ecx, %eax                                    
        movl      %eax, (%ebp)                                  
        movl      %eax, 4(%ebp)                                 
        movl      %eax, 8(%ebp)                                 
        movl      %eax, 12(%ebp)                                
        movl      %eax, 16(%ebp)                                
        movl      %eax, 20(%ebp)                                
        movl      %eax, 24(%ebp)                                
        movl      %eax, 28(%ebp)                                
        movl      %eax, 32(%ebp)                                
        movl      %eax, 36(%ebp)                                
        movl      %eax, 40(%ebp)                                
        movl      %eax, 44(%ebp)                                
        movl      %eax, 48(%ebp)                                
        movl      %eax, 52(%ebp)                                
        movl      %eax, 56(%ebp)                                
        movl      %eax, 60(%ebp)                                
        movl      %eax, 64(%ebp)                                
        movl      %eax, 68(%ebp)                                
        movl      %eax, 72(%ebp)                                
        movl      %eax, 76(%ebp)                                
        movl      %eax, 80(%ebp)                                
        movl      %eax, 84(%ebp)                                
                                
.L39:                          
        addl      $8, %esp                                      
        popl      %ebp                                          
        ret                                                     
        .align    4,0x90
                                

	.size	MD4Final,.-MD4Final
	.data

	.text


       .align    4,0x90

	.type	MD4Transform,@function
MD4Transform:
.L41:                          
        pushl     %edi                                          
        pushl     %esi                                          
        pushl     %ebp                                          
        pushl     %ebx                                          
        subl      $64, %esp                                     
        movl      84(%esp), %eax                                
        movl      (%eax), %ebp                                  
        movl      4(%eax), %edi                                 
        movl      8(%eax), %esi                                 
        movl      12(%eax), %ebx                                
        lea       (%esp), %edx                                  
        push      $64                                           
        pushl     92(%esp)                                      
        pushl     %edx                                          
        call      Decode                                        
                                
.L46:                          
        addl      $12, %esp                                     
                                
.L42:                          
        addl      (%esp), %ebp                                  
        movl      %edi, %edx                                    
        andl      %esi, %edx                                    
        movl      %edi, %eax                                    
        xorl      $-1, %eax                                     
        andl      %ebx, %eax                                    
        orl       %eax, %edx                                    
        addl      %edx, %ebp                                    
        roll      $3, %ebp                                      
        addl      4(%esp), %ebx                                 
        movl      %edi, %edx                                    
        andl      %ebp, %edx                                    
        movl      %ebp, %eax                                    
        xorl      $-1, %eax                                     
        andl      %esi, %eax                                    
        orl       %eax, %edx                                    
        addl      %edx, %ebx                                    
        roll      $7, %ebx                                      
        addl      8(%esp), %esi                                 
        movl      %ebp, %edx                                    
        andl      %ebx, %edx                                    
        movl      %ebx, %eax                                    
        xorl      $-1, %eax                                     
        andl      %edi, %eax                                    
        orl       %eax, %edx                                    
        addl      %edx, %esi                                    
        roll      $11, %esi                                     
        addl      12(%esp), %edi                                
        movl      %esi, %edx                                    
        andl      %ebx, %edx                                    
        movl      %esi, %eax                                    
        xorl      $-1, %eax                                     
        andl      %ebp, %eax                                    
        orl       %eax, %edx                                    
        addl      %edx, %edi                                    
        roll      $19, %edi                                     
        addl      16(%esp), %ebp                                
        movl      %edi, %edx                                    
        andl      %esi, %edx                                    
        movl      %edi, %eax                                    
        xorl      $-1, %eax                                     
        andl      %ebx, %eax                                    
        orl       %eax, %edx                                    
        addl      %edx, %ebp                                    
        roll      $3, %ebp                                      
        movl      %edi, %eax                                    
        andl      %ebp, %eax                                    
        movl      %ebp, %edx                                    
        xorl      $-1, %edx                                     
        andl      %esi, %edx                                    
        orl       %edx, %eax                                    
        addl      20(%esp), %eax                                
        addl      %ebx, %eax                                    
        roll      $7, %eax                                      
        addl      24(%esp), %esi                                
        movl      %ebp, %ecx                                    
        andl      %eax, %ecx                                    
        movl      %eax, %edx                                    
        xorl      $-1, %edx                                     
        andl      %edi, %edx                                    
        orl       %edx, %ecx                                    
        addl      %ecx, %esi                                    
        roll      $11, %esi                                     
        addl      28(%esp), %edi                                
        movl      %esi, %ecx                                    
        andl      %eax, %ecx                                    
        movl      %esi, %edx                                    
        xorl      $-1, %edx                                     
        andl      %ebp, %edx                                    
        orl       %edx, %ecx                                    
        addl      %ecx, %edi                                    
        roll      $19, %edi                                     
        addl      32(%esp), %ebp                                
        movl      %edi, %ecx                                    
        andl      %esi, %ecx                                    
        movl      %edi, %edx                                    
        xorl      $-1, %edx                                     
        andl      %eax, %edx                                    
        orl       %edx, %ecx                                    
        addl      %ecx, %ebp                                    
        roll      $3, %ebp                                      
        movl      %edi, %edx                                    
        andl      %ebp, %edx                                    
        movl      %ebp, %ecx                                    
        xorl      $-1, %ecx                                     
        andl      %esi, %ecx                                    
        orl       %ecx, %edx                                    
        addl      36(%esp), %edx                                
        addl      %eax, %edx                                    
        roll      $7, %edx                                      
        addl      40(%esp), %esi                                
        movl      %ebp, %ecx                                    
        andl      %edx, %ecx                                    
        movl      %edx, %eax                                    
        xorl      $-1, %eax                                     
        andl      %edi, %eax                                    
        orl       %eax, %ecx                                    
        addl      %ecx, %esi                                    
        roll      $11, %esi                                     
        addl      44(%esp), %edi                                
        movl      %esi, %ecx                                    
        andl      %edx, %ecx                                    
        movl      %esi, %eax                                    
        xorl      $-1, %eax                                     
        andl      %ebp, %eax                                    
        orl       %eax, %ecx                                    
        addl      %ecx, %edi                                    
        roll      $19, %edi                                     
        addl      48(%esp), %ebp                                
        movl      %edi, %ecx                                    
        andl      %esi, %ecx                                    
        movl      %edi, %eax                                    
        xorl      $-1, %eax                                     
        andl      %edx, %eax                                    
        orl       %eax, %ecx                                    
        addl      %ecx, %ebp                                    
        roll      $3, %ebp                                      
        movl      %edi, %eax                                    
        andl      %ebp, %eax                                    
        movl      %ebp, %ecx                                    
        xorl      $-1, %ecx                                     
        andl      %esi, %ecx                                    
        orl       %ecx, %eax                                    
        addl      52(%esp), %eax                                
        addl      %edx, %eax                                    
        roll      $7, %eax                                      
        addl      56(%esp), %esi                                
        movl      %ebp, %ecx                                    
        andl      %eax, %ecx                                    
        movl      %eax, %edx                                    
        xorl      $-1, %edx                                     
        andl      %edi, %edx                                    
        orl       %edx, %ecx                                    
        addl      %ecx, %esi                                    
        roll      $11, %esi                                     
        movl      %esi, %ecx                                    
        andl      %eax, %ecx                                    
        movl      %esi, %edx                                    
        addl      60(%esp), %edi                                
        xorl      $-1, %edx                                     
        andl      %ebp, %edx                                    
        orl       %ecx, %edx                                    
        addl      %edx, %edi                                    
        roll      $19, %edi                                     
        movl      %esi, %edx                                    
        orl       %eax, %edx                                    
        andl      %edi, %edx                                    
        orl       %ecx, %edx                                    
        lea       1518500249(%ebp,%edx), %edx                   
        movl      %edi, %ebx                                    
        addl      (%esp), %edx                                  
        roll      $3, %edx                                      
        orl       %esi, %ebx                                    
        andl      %edx, %ebx                                    
        movl      %edi, %ecx                                    
        andl      %esi, %ecx                                    
        orl       %ecx, %ebx                                    
        lea       1518500249(%ebx,%eax), %eax                   
        movl      %edi, %ebx                                    
        addl      16(%esp), %eax                                
        roll      $5, %eax                                      
        orl       %edx, %ebx                                    
        andl      %eax, %ebx                                    
        movl      %edi, %ecx                                    
        andl      %edx, %ecx                                    
        orl       %ecx, %ebx                                    
        lea       1518500249(%esi,%ebx), %ebx                   
        movl      %edx, %ebp                                    
        addl      32(%esp), %ebx                                
        roll      $9, %ebx                                      
        orl       %eax, %ebp                                    
        andl      %ebx, %ebp                                    
        movl      %edx, %ecx                                    
        andl      %eax, %ecx                                    
        orl       %ecx, %ebp                                    
        lea       1518500249(%edi,%ebp), %ecx                   
        movl      %ebx, %esi                                    
        addl      48(%esp), %ecx                                
        roll      $13, %ecx                                     
        orl       %eax, %esi                                    
        andl      %ecx, %esi                                    
        movl      %ebx, %ebp                                    
        andl      %eax, %ebp                                    
        orl       %ebp, %esi                                    
        lea       1518500249(%edx,%esi), %edx                   
        movl      %ecx, %esi                                    
        addl      4(%esp), %edx                                 
        roll      $3, %edx                                      
        orl       %ebx, %esi                                    
        andl      %edx, %esi                                    
        movl      %ecx, %ebp                                    
        andl      %ebx, %ebp                                    
        orl       %ebp, %esi                                    
        lea       1518500249(%esi,%eax), %eax                   
        movl      %ecx, %esi                                    
        addl      20(%esp), %eax                                
        roll      $5, %eax                                      
        orl       %edx, %esi                                    
        andl      %eax, %esi                                    
        movl      %ecx, %ebp                                    
        andl      %edx, %ebp                                    
        orl       %ebp, %esi                                    
        lea       1518500249(%ebx,%esi), %ebx                   
        movl      %edx, %esi                                    
        addl      36(%esp), %ebx                                
        roll      $9, %ebx                                      
        orl       %eax, %esi                                    
        andl      %ebx, %esi                                    
        movl      %edx, %ebp                                    
        andl      %eax, %ebp                                    
        orl       %ebp, %esi                                    
        lea       1518500249(%ecx,%esi), %ecx                   
        movl      %ebx, %esi                                    
        addl      52(%esp), %ecx                                
        roll      $13, %ecx                                     
        orl       %eax, %esi                                    
        andl      %ecx, %esi                                    
        movl      %ebx, %ebp                                    
        andl      %eax, %ebp                                    
        orl       %ebp, %esi                                    
        lea       1518500249(%edx,%esi), %edx                   
        movl      %ecx, %esi                                    
        addl      8(%esp), %edx                                 
        roll      $3, %edx                                      
        orl       %ebx, %esi                                    
        andl      %edx, %esi                                    
        movl      %ecx, %ebp                                    
        andl      %ebx, %ebp                                    
        orl       %ebp, %esi                                    
        lea       1518500249(%esi,%eax), %eax                   
        movl      %ecx, %esi                                    
        addl      24(%esp), %eax                                
        roll      $5, %eax                                      
        orl       %edx, %esi                                    
        andl      %eax, %esi                                    
        movl      %ecx, %ebp                                    
        andl      %edx, %ebp                                    
        orl       %ebp, %esi                                    
        lea       1518500249(%ebx,%esi), %ebx                   
        movl      %edx, %esi                                    
        addl      40(%esp), %ebx                                
        roll      $9, %ebx                                      
        orl       %eax, %esi                                    
        andl      %ebx, %esi                                    
        movl      %edx, %ebp                                    
        andl      %eax, %ebp                                    
        orl       %ebp, %esi                                    
        lea       1518500249(%ecx,%esi), %ecx                   
        movl      %ebx, %esi                                    
        addl      56(%esp), %ecx                                
        roll      $13, %ecx                                     
        orl       %eax, %esi                                    
        andl      %ecx, %esi                                    
        movl      %ebx, %ebp                                    
        andl      %eax, %ebp                                    
        orl       %ebp, %esi                                    
        lea       1518500249(%edx,%esi), %edx                   
        movl      %ecx, %esi                                    
        addl      12(%esp), %edx                                
        roll      $3, %edx                                      
        orl       %ebx, %esi                                    
        andl      %edx, %esi                                    
        movl      %ecx, %ebp                                    
        andl      %ebx, %ebp                                    
        orl       %ebp, %esi                                    
        lea       1518500249(%esi,%eax), %eax                   
        movl      %ecx, %esi                                    
        addl      28(%esp), %eax                                
        roll      $5, %eax                                      
        orl       %edx, %esi                                    
        andl      %eax, %esi                                    
        movl      %ecx, %ebp                                    
        andl      %edx, %ebp                                    
        orl       %ebp, %esi                                    
        lea       1518500249(%ebx,%esi), %ebx                   
        movl      %edx, %esi                                    
        addl      44(%esp), %ebx                                
        roll      $9, %ebx                                      
        orl       %eax, %esi                                    
        andl      %ebx, %esi                                    
        movl      %edx, %ebp                                    
        andl      %eax, %ebp                                    
        orl       %ebp, %esi                                    
        lea       1518500249(%ecx,%esi), %ecx                   
        addl      60(%esp), %ecx                                
        roll      $13, %ecx                                     
        movl      %ecx, %ebp                                    
        xorl      %ebx, %ebp                                    
        xorl      %eax, %ebp                                    
        lea       1859775393(%edx,%ebp), %edx                   
        addl      (%esp), %edx                                  
        roll      $3, %edx                                      
        movl      %edx, %ebp                                    
        xorl      %ecx, %ebp                                    
        xorl      %ebx, %ebp                                    
        lea       1859775393(%ebp,%eax), %eax                   
        addl      32(%esp), %eax                                
        roll      $9, %eax                                      
        movl      %eax, %ebp                                    
        xorl      %edx, %ebp                                    
        xorl      %ecx, %ebp                                    
        lea       1859775393(%ebx,%ebp), %ebx                   
        addl      16(%esp), %ebx                                
        roll      $11, %ebx                                     
        movl      %ebx, %ebp                                    
        xorl      %eax, %ebp                                    
        xorl      %edx, %ebp                                    
        lea       1859775393(%ecx,%ebp), %ecx                   
        addl      48(%esp), %ecx                                
        roll      $15, %ecx                                     
        movl      %ecx, %ebp                                    
        xorl      %ebx, %ebp                                    
        xorl      %eax, %ebp                                    
        lea       1859775393(%edx,%ebp), %edx                   
        addl      8(%esp), %edx                                 
        roll      $3, %edx                                      
        movl      %edx, %ebp                                    
        xorl      %ecx, %ebp                                    
        xorl      %ebx, %ebp                                    
        lea       1859775393(%ebp,%eax), %eax                   
        addl      40(%esp), %eax                                
        roll      $9, %eax                                      
        movl      %eax, %ebp                                    
        xorl      %edx, %ebp                                    
        xorl      %ecx, %ebp                                    
        lea       1859775393(%ebx,%ebp), %ebx                   
        addl      24(%esp), %ebx                                
        roll      $11, %ebx                                     
        movl      %ebx, %ebp                                    
        xorl      %eax, %ebp                                    
        xorl      %edx, %ebp                                    
        lea       1859775393(%ecx,%ebp), %ecx                   
        addl      56(%esp), %ecx                                
        roll      $15, %ecx                                     
        movl      %ecx, %ebp                                    
        xorl      %ebx, %ebp                                    
        xorl      %eax, %ebp                                    
        lea       1859775393(%edx,%ebp), %edx                   
        addl      4(%esp), %edx                                 
        roll      $3, %edx                                      
        movl      %edx, %ebp                                    
        xorl      %ecx, %ebp                                    
        xorl      %ebx, %ebp                                    
        lea       1859775393(%ebp,%eax), %eax                   
        addl      36(%esp), %eax                                
        roll      $9, %eax                                      
        movl      %eax, %ebp                                    
        xorl      %edx, %ebp                                    
        xorl      %ecx, %ebp                                    
        lea       1859775393(%ebx,%ebp), %ebx                   
        addl      20(%esp), %ebx                                
        roll      $11, %ebx                                     
        movl      %ebx, %ebp                                    
        xorl      %eax, %ebp                                    
        xorl      %edx, %ebp                                    
        lea       1859775393(%ecx,%ebp), %ebp                   
        addl      52(%esp), %ebp                                
        roll      $15, %ebp                                     
        movl      %ebp, %ecx                                    
        xorl      %ebx, %ecx                                    
        xorl      %eax, %ecx                                    
        lea       1859775393(%edx,%ecx), %esi                   
        addl      12(%esp), %esi                                
        roll      $3, %esi                                      
        movl      %esi, %edx                                    
        xorl      %ebp, %edx                                    
        xorl      %ebx, %edx                                    
        lea       1859775393(%edx,%eax), %edi                   
        addl      44(%esp), %edi                                
        roll      $9, %edi                                      
        movl      %edi, %eax                                    
        xorl      %esi, %eax                                    
        xorl      %ebp, %eax                                    
        lea       1859775393(%ebx,%eax), %edx                   
        addl      28(%esp), %edx                                
        roll      $11, %edx                                     
        movl      %edx, %eax                                    
        xorl      %edi, %eax                                    
        xorl      %esi, %eax                                    
        lea       1859775393(%ebp,%eax), %eax                   
        addl      60(%esp), %eax                                
        roll      $15, %eax                                     
        movl      84(%esp), %ecx                                
        addl      %esi, (%ecx)                                  
        addl      %eax, 4(%ecx)                                 
        addl      %edx, 8(%ecx)                                 
        xorl      %eax, %eax                                    
        addl      %edi, 12(%ecx)                                
        lea       (%esp), %ecx                                  
        andl      $255, %eax                                    
        movl      %eax, %edx                                    
        shll      $8, %edx                                      
        orl       %edx, %eax                                    
        movl      %eax, %edx                                    
        shll      $16, %edx                                     
        orl       %edx, %eax                                    
        movl      %eax, (%ecx)                                  
        movl      %eax, 4(%ecx)                                 
        movl      %eax, 8(%ecx)                                 
        movl      %eax, 12(%ecx)                                
        movl      %eax, 16(%ecx)                                
        movl      %eax, 20(%ecx)                                
        movl      %eax, 24(%ecx)                                
        movl      %eax, 28(%ecx)                                
        movl      %eax, 32(%ecx)                                
        movl      %eax, 36(%ecx)                                
        movl      %eax, 40(%ecx)                                
        movl      %eax, 44(%ecx)                                
        movl      %eax, 48(%ecx)                                
        movl      %eax, 52(%ecx)                                
        movl      %eax, 56(%ecx)                                
        movl      %eax, 60(%ecx)                                
                                
.L43:                          
        addl      $64, %esp                                     
        popl      %ebx                                          
        popl      %ebp                                          
        popl      %esi                                          
        popl      %edi                                          
        ret                                                     
        .align    4,0x90
                                

	.size	MD4Transform,.-MD4Transform
	.data

	.data
	.text


       .align    4,0x90


	.type	Encode,@function
Encode:
.L51:                          
        pushl     %esi                                          
        pushl     %ebx                                          
        subl      $8, %esp                                      
        movl      20(%esp), %ebx                                
        movl      24(%esp), %esi                                
        movl      28(%esp), %eax                                
        xorl      %edx, %edx                                    
        xorl      %ecx, %ecx                                    
        testl     %eax, %eax                                    
        jbe       .L55         
                                
.L52:                          
        movl      %ecx, 4(%esp)                                 
        
                                
.L53:                          
        movl      24(%esp), %ecx                                
        movb      (%ecx,%edx,4), %bl                            
        movl      20(%esp), %esi                                
        movl      4(%esp), %eax                                 
        movl      24(%esp), %ecx                                
        movb      %bl, (%eax,%esi)                              
        movl      (%ecx,%edx,4), %ecx                           
        shrl      $8, %ecx                                      
        movb      %cl, 1(%eax,%esi)                             
        movl      24(%esp), %ecx                                
        movb      2(%ecx,%edx,4), %cl                           
        movb      %cl, 2(%eax,%esi)                             
        movl      24(%esp), %ecx                                
        movb      3(%ecx,%edx,4), %cl                           
        movb      %cl, 3(%eax,%esi)                             
        incl      %edx                                          
        addl      $4, %eax                                      
        movl      %eax, 4(%esp)                                 
        movl      28(%esp), %ecx                                
        cmpl      %ecx, %eax                                    
        jb        .L53         
                                
.L55:                          
        addl      $8, %esp                                      
        popl      %ebx                                          
        popl      %esi                                          
        ret                                                     
        .align    4,0x90
                                

	.size	Encode,.-Encode
	.data

	.text


       .align    4,0x90


	.type	Decode,@function
Decode:
.L61:                          
        pushl     %edi                                          
        pushl     %esi                                          
        pushl     %ebp                                          
        pushl     %ebx                                          
        movl      20(%esp), %edi                                
        movl      24(%esp), %esi                                
        movl      28(%esp), %eax                                
        xorl      %ebx, %ebx                                    
        xorl      %ecx, %ecx                                    
        testl     %eax, %eax                                    
        jbe       .L69         
                                
.L62:                          
        cmpl      $16, %eax                                     
        jb        .L67         
                                
.L63:                          
        lea       -16(%eax), %edx                               
        
                                
.L64:                          
        movzbl    (%ecx,%esi), %eax                             
        movzbl    1(%ecx,%esi), %ebp                            
        shll      $8, %ebp                                      
        orl       %ebp, %eax                                    
        movzbl    2(%ecx,%esi), %ebp                            
        shll      $16, %ebp                                     
        orl       %ebp, %eax                                    
        movzbl    3(%ecx,%esi), %ebp                            
        shll      $24, %ebp                                     
        orl       %ebp, %eax                                    
        movl      %eax, (%edi,%ebx,4)                           
        movzbl    4(%ecx,%esi), %ebp                            
        movzbl    5(%ecx,%esi), %eax                            
        shll      $8, %eax                                      
        orl       %eax, %ebp                                    
        movzbl    6(%ecx,%esi), %eax                            
        shll      $16, %eax                                     
        orl       %eax, %ebp                                    
        movzbl    7(%ecx,%esi), %eax                            
        shll      $24, %eax                                     
        orl       %eax, %ebp                                    
        movl      %ebp, 4(%edi,%ebx,4)                          
        movzbl    8(%ecx,%esi), %eax                            
        movzbl    9(%ecx,%esi), %ebp                            
        shll      $8, %ebp                                      
        orl       %ebp, %eax                                    
        movzbl    10(%ecx,%esi), %ebp                           
        shll      $16, %ebp                                     
        orl       %ebp, %eax                                    
        movzbl    11(%ecx,%esi), %ebp                           
        shll      $24, %ebp                                     
        orl       %ebp, %eax                                    
        addl      $12, %ecx                                     
        movl      %eax, 8(%edi,%ebx,4)                          
        addl      $3, %ebx                                      
        cmpl      %edx, %ecx                                    
        jbe       .L64         
                                
.L65:                          
        movl      28(%esp), %eax                                
        
                                
.L67:                          
        movzbl    (%ecx,%esi), %edx                             
        movzbl    1(%ecx,%esi), %ebp                            
        shll      $8, %ebp                                      
        orl       %ebp, %edx                                    
        movzbl    2(%ecx,%esi), %ebp                            
        shll      $16, %ebp                                     
        orl       %ebp, %edx                                    
        movzbl    3(%ecx,%esi), %ebp                            
        shll      $24, %ebp                                     
        orl       %ebp, %edx                                    
        addl      $4, %ecx                                      
        movl      %edx, (%edi,%ebx,4)                           
        incl      %ebx                                          
        cmpl      %eax, %ecx                                    
        jb        .L67         
                                
.L69:                          
        popl      %ebx                                          
        popl      %ebp                                          
        popl      %esi                                          
        popl      %edi                                          
        ret                                                     
        .align    4,0x90
                                

	.size	Decode,.-Decode
	.data

	.data

#if defined(__linux__) && defined(__ELF__)
.section .note.GNU-stack,"",%progbits
#endif

