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
        subl      $20, %esp
        movl      40(%esp), %ebp
        movl      48(%esp), %ecx
        movl      16(%ebp), %edx
        movl      %edx, %eax
        shrl      $3, %eax
        andl      $63, %eax
        movl      %ecx, %ebx
        shll      $3, %ebx
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
        movl      48(%esp), %esi
        movl      %esi, %ebx
        shrl      $29, %ebx
        addl      %ebx, %edx
        movl      %edx, 20(%ebp)
        movl      %eax, %edx
        negl      %edx
        movl      %edx, 12(%esp)
        lea       64(%edx), %ebx
        cmpl      %ebx, %esi
        jae       .L26

.L25:
        xorl      %ebx, %ebx
        jmp       .L214

.L26:
        lea       24(%ebp,%eax), %edi
        movl      44(%esp), %esi
        movl      %ebx, %ecx
        rep
        movsb

.L27:
        movl      %ebp, (%esp)
        lea       24(%ebp), %eax
        movl      %eax, 4(%esp)
        call      MD4Transform

.L28:
        movl      12(%esp), %edx
        addl      $127, %edx
        movl      48(%esp), %eax
        cmpl      %edx, %eax
        jbe       .L213

.L29:
        lea       63(%ebx), %edi
        movl      44(%esp), %eax
        lea       (%ebx,%eax), %esi
        

.L210:
        movl      %ebp, (%esp)
        movl      %esi, 4(%esp)
        call      MD4Transform

.L211:
        addl      $64, %edi
        addl      $64, %esi
        addl      $64, %ebx
        movl      48(%esp), %eax
        cmpl      %edi, %eax
        ja        .L210

.L213:
        xorl      %eax, %eax

.L214:
        lea       24(%ebp,%eax), %edi
        movl      44(%esp), %eax
        lea       (%eax,%ebx), %esi
        movl      48(%esp), %eax
        subl      %ebx, %eax
        movl      %eax, %ecx
        rep
        movsb

.L215:
        addl      $20, %esp
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
        pushl     %ebx
        subl      $28, %esp
        movl      40(%esp), %ebp
        movl      44(%esp), %ebx
        lea       16(%esp), %eax
        movl      %eax, (%esp)
        lea       16(%ebx), %edx
        movl      %edx, 4(%esp)
        movl      $8, 8(%esp)
        call      Encode

.L32:
        movl      16(%ebx), %eax
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
        movl      %ebx, (%esp)
        movl      $PADDING, 4(%esp)
        movl      %eax, 8(%esp)
        call      MD4Update

.L36:
        movl      %ebx, (%esp)
        lea       16(%esp), %eax
        movl      %eax, 4(%esp)
        movl      $8, 8(%esp)
        call      MD4Update

.L37:
        movl      %ebp, (%esp)
        movl      %ebx, 4(%esp)
        movl      $16, 8(%esp)
        call      Encode

.L38:
        xorl      %eax, %eax
        andl      $255, %eax
        movl      %eax, %edx
        shll      $8, %edx
        orl       %edx, %eax
        movl      %eax, %ecx
        shll      $16, %ecx
        orl       %ecx, %eax
        movl      %eax, (%ebx)
        movl      %eax, 4(%ebx)
        movl      %eax, 8(%ebx)
        movl      %eax, 12(%ebx)
        movl      %eax, 16(%ebx)
        movl      %eax, 20(%ebx)
        movl      %eax, 24(%ebx)
        movl      %eax, 28(%ebx)
        movl      %eax, 32(%ebx)
        movl      %eax, 36(%ebx)
        movl      %eax, 40(%ebx)
        movl      %eax, 44(%ebx)
        movl      %eax, 48(%ebx)
        movl      %eax, 52(%ebx)
        movl      %eax, 56(%ebx)
        movl      %eax, 60(%ebx)
        movl      %eax, 64(%ebx)
        movl      %eax, 68(%ebx)
        movl      %eax, 72(%ebx)
        movl      %eax, 76(%ebx)
        movl      %eax, 80(%ebx)
        movl      %eax, 84(%ebx)

.L39:
        addl      $28, %esp
        popl      %ebx
        popl      %ebp
        ret
        .align    4,0x90


	.size	MD4Final,.-MD4Final
	.data

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
        subl      $132, %esp
        movl      152(%esp), %edx
        movl      156(%esp), %ebp
        movl      (%edx), %eax
        movl      %eax, 124(%esp)
        movl      4(%edx), %edi
        movl      8(%edx), %esi
        movl      12(%edx), %eax
        movl      %eax, 128(%esp)
        xorl      %eax, %eax
        movb      (%ebp), %al
        xorl      %edx, %edx
        movb      1(%ebp), %dl
        shll      $8, %edx
        orl       %edx, %eax
        xorl      %edx, %edx
        movb      2(%ebp), %dl
        shll      $16, %edx
        orl       %edx, %eax
        xorl      %edx, %edx
        movb      3(%ebp), %dl
        shll      $24, %edx
        orl       %edx, %eax
        movl      %eax, 84(%esp)
        movl      %eax, (%esp)
        xorl      %eax, %eax
        movb      4(%ebp), %al
        xorl      %edx, %edx
        movb      5(%ebp), %dl
        shll      $8, %edx
        orl       %edx, %eax
        xorl      %edx, %edx
        movb      6(%ebp), %dl
        shll      $16, %edx
        orl       %edx, %eax
        xorl      %edx, %edx
        movb      7(%ebp), %dl
        shll      $24, %edx
        orl       %edx, %eax
        movl      %eax, 68(%esp)
        movl      %eax, 4(%esp)
        xorl      %eax, %eax
        movb      8(%ebp), %al
        xorl      %edx, %edx
        movb      9(%ebp), %dl
        shll      $8, %edx
        orl       %edx, %eax
        xorl      %edx, %edx
        movb      10(%ebp), %dl
        shll      $16, %edx
        orl       %edx, %eax
        xorl      %edx, %edx
        movb      11(%ebp), %dl
        shll      $24, %edx
        orl       %edx, %eax
        movl      %eax, 80(%esp)
        movl      %eax, 8(%esp)
        xorl      %eax, %eax
        movb      12(%ebp), %al
        xorl      %edx, %edx
        movb      13(%ebp), %dl
        shll      $8, %edx
        orl       %edx, %eax
        xorl      %edx, %edx
        movb      14(%ebp), %dl
        shll      $16, %edx
        orl       %edx, %eax
        xorl      %edx, %edx
        movb      15(%ebp), %dl
        shll      $24, %edx
        orl       %edx, %eax
        movl      %eax, 64(%esp)
        movl      %eax, 12(%esp)
        xorl      %eax, %eax
        movb      16(%ebp), %al
        xorl      %edx, %edx
        movb      17(%ebp), %dl
        shll      $8, %edx
        orl       %edx, %eax
        xorl      %edx, %edx
        movb      18(%ebp), %dl
        shll      $16, %edx
        orl       %edx, %eax
        xorl      %edx, %edx
        movb      19(%ebp), %dl
        shll      $24, %edx
        orl       %edx, %eax
        movl      %eax, 92(%esp)
        movl      %eax, 16(%esp)
        xorl      %eax, %eax
        movb      20(%ebp), %al
        xorl      %edx, %edx
        movb      21(%ebp), %dl
        shll      $8, %edx
        orl       %edx, %eax
        xorl      %edx, %edx
        movb      22(%ebp), %dl
        shll      $16, %edx
        orl       %edx, %eax
        xorl      %edx, %edx
        movb      23(%ebp), %dl
        shll      $24, %edx
        orl       %edx, %eax
        movl      %eax, 72(%esp)
        movl      %eax, 20(%esp)
        xorl      %eax, %eax
        movb      24(%ebp), %al
        xorl      %edx, %edx
        movb      25(%ebp), %dl
        shll      $8, %edx
        orl       %edx, %eax
        xorl      %edx, %edx
        movb      26(%ebp), %dl
        shll      $16, %edx
        orl       %edx, %eax
        xorl      %edx, %edx
        movb      27(%ebp), %dl
        shll      $24, %edx
        orl       %edx, %eax
        movl      %eax, 88(%esp)
        movl      %eax, 24(%esp)
        xorl      %eax, %eax
        movb      28(%ebp), %al
        xorl      %edx, %edx
        movb      29(%ebp), %dl
        shll      $8, %edx
        orl       %edx, %eax
        xorl      %edx, %edx
        movb      30(%ebp), %dl
        shll      $16, %edx
        orl       %edx, %eax
        xorl      %edx, %edx
        movb      31(%ebp), %dl
        shll      $24, %edx
        orl       %edx, %eax
        movl      %eax, 76(%esp)
        movl      %eax, 28(%esp)
        xorl      %eax, %eax
        movb      32(%ebp), %al
        xorl      %edx, %edx
        movb      33(%ebp), %dl
        shll      $8, %edx
        orl       %edx, %eax
        xorl      %edx, %edx
        movb      34(%ebp), %dl
        shll      $16, %edx
        orl       %edx, %eax
        xorl      %edx, %edx
        movb      35(%ebp), %dl
        shll      $24, %edx
        orl       %edx, %eax
        movl      %eax, 108(%esp)
        movl      %eax, 32(%esp)
        xorl      %eax, %eax
        movb      36(%ebp), %al
        xorl      %edx, %edx
        movb      37(%ebp), %dl
        shll      $8, %edx
        orl       %edx, %eax
        xorl      %edx, %edx
        movb      38(%ebp), %dl
        shll      $16, %edx
        orl       %edx, %eax
        xorl      %edx, %edx
        movb      39(%ebp), %dl
        shll      $24, %edx
        orl       %edx, %eax
        movl      %eax, 96(%esp)
        movl      %eax, 36(%esp)
        xorl      %eax, %eax
        movb      40(%ebp), %al
        xorl      %edx, %edx
        movb      41(%ebp), %dl
        shll      $8, %edx
        orl       %edx, %eax
        xorl      %edx, %edx
        movb      42(%ebp), %dl
        shll      $16, %edx
        orl       %edx, %eax
        xorl      %edx, %edx
        movb      43(%ebp), %dl
        shll      $24, %edx
        orl       %edx, %eax
        movl      %eax, 104(%esp)
        movl      %eax, 40(%esp)
        xorl      %ebx, %ebx
        movb      44(%ebp), %bl
        xorl      %eax, %eax
        movb      45(%ebp), %al
        shll      $8, %eax
        orl       %eax, %ebx
        xorl      %eax, %eax
        movb      46(%ebp), %al
        shll      $16, %eax
        orl       %eax, %ebx
        xorl      %eax, %eax
        movb      47(%ebp), %al
        shll      $24, %eax
        orl       %eax, %ebx
        movl      %ebx, 100(%esp)
        movl      %ebx, 44(%esp)
        xorl      %ecx, %ecx
        movb      48(%ebp), %cl
        xorl      %eax, %eax
        movb      49(%ebp), %al
        shll      $8, %eax
        orl       %eax, %ecx
        xorl      %eax, %eax
        movb      50(%ebp), %al
        shll      $16, %eax
        orl       %eax, %ecx
        xorl      %eax, %eax
        movb      51(%ebp), %al
        shll      $24, %eax
        orl       %eax, %ecx
        movl      %ecx, 116(%esp)
        movl      %ecx, 48(%esp)
        xorl      %edx, %edx
        movb      52(%ebp), %dl
        xorl      %eax, %eax
        movb      53(%ebp), %al
        shll      $8, %eax
        orl       %eax, %edx
        xorl      %eax, %eax
        movb      54(%ebp), %al
        shll      $16, %eax
        orl       %eax, %edx
        xorl      %eax, %eax
        movb      55(%ebp), %al
        shll      $24, %eax
        orl       %eax, %edx
        movl      %edx, 112(%esp)
        movl      %edx, 52(%esp)
        xorl      %eax, %eax
        movb      56(%ebp), %al
        xorl      %edx, %edx
        movb      57(%ebp), %dl
        shll      $8, %edx
        orl       %edx, %eax
        xorl      %edx, %edx
        movb      58(%ebp), %dl
        shll      $16, %edx
        orl       %edx, %eax
        xorl      %edx, %edx
        movb      59(%ebp), %dl
        shll      $24, %edx
        orl       %edx, %eax
        movl      %eax, 120(%esp)
        movl      %eax, 56(%esp)
        xorl      %eax, %eax
        movb      60(%ebp), %al
        xorl      %edx, %edx
        movb      61(%ebp), %dl
        shll      $8, %edx
        orl       %edx, %eax
        xorl      %edx, %edx
        movb      62(%ebp), %dl
        shll      $16, %edx
        orl       %edx, %eax
        xorl      %edx, %edx
        movb      63(%ebp), %dl
        shll      $24, %edx
        orl       %edx, %eax
        movl      %eax, 60(%esp)
        movl      124(%esp), %edx
        addl      84(%esp), %edx
        movl      %edx, 124(%esp)
        movl      %edi, %ebx
        andl      %esi, %ebx
        movl      %edi, %edx
        xorl      $-1, %edx
        movl      128(%esp), %ecx
        andl      %ecx, %edx
        orl       %edx, %ebx
        movl      124(%esp), %ebp
        addl      %ebx, %ebp
        roll      $3, %ebp
        movl      %ebp, 124(%esp)
        movl      %edi, %edx
        andl      %ebp, %edx
        xorl      $-1, %ebp
        andl      %esi, %ebp
        orl       %ebp, %edx
        addl      68(%esp), %edx
        addl      %ecx, %edx
        roll      $7, %edx
        addl      80(%esp), %esi
        movl      124(%esp), %ecx
        andl      %edx, %ecx
        movl      %edx, %ebx
        xorl      $-1, %ebx
        andl      %edi, %ebx
        orl       %ebx, %ecx
        addl      %ecx, %esi
        roll      $11, %esi
        addl      64(%esp), %edi
        movl      %esi, %ebx
        andl      %edx, %ebx
        movl      %esi, %ecx
        xorl      $-1, %ecx
        andl      124(%esp), %ecx
        orl       %ecx, %ebx
        addl      %ebx, %edi
        roll      $19, %edi
        movl      124(%esp), %ecx
        addl      92(%esp), %ecx
        movl      %ecx, 124(%esp)
        movl      %edi, %ebx
        andl      %esi, %ebx
        movl      %edi, %ecx
        xorl      $-1, %ecx
        andl      %edx, %ecx
        orl       %ecx, %ebx
        movl      124(%esp), %ebp
        addl      %ebx, %ebp
        roll      $3, %ebp
        movl      %ebp, 124(%esp)
        movl      %edi, %ecx
        andl      %ebp, %ecx
        xorl      $-1, %ebp
        andl      %esi, %ebp
        orl       %ebp, %ecx
        addl      72(%esp), %ecx
        addl      %edx, %ecx
        roll      $7, %ecx
        addl      88(%esp), %esi
        movl      124(%esp), %edx
        andl      %ecx, %edx
        movl      %ecx, %ebx
        xorl      $-1, %ebx
        andl      %edi, %ebx
        orl       %ebx, %edx
        addl      %edx, %esi
        roll      $11, %esi
        addl      76(%esp), %edi
        movl      %esi, %ebx
        andl      %ecx, %ebx
        movl      %esi, %edx
        xorl      $-1, %edx
        andl      124(%esp), %edx
        orl       %edx, %ebx
        addl      %ebx, %edi
        roll      $19, %edi
        movl      124(%esp), %edx
        addl      108(%esp), %edx
        movl      %edx, 124(%esp)
        movl      %edi, %ebx
        andl      %esi, %ebx
        movl      %edi, %edx
        xorl      $-1, %edx
        andl      %ecx, %edx
        orl       %edx, %ebx
        movl      124(%esp), %ebp
        addl      %ebx, %ebp
        roll      $3, %ebp
        movl      %ebp, 124(%esp)
        movl      %edi, %edx
        andl      %ebp, %edx
        xorl      $-1, %ebp
        andl      %esi, %ebp
        orl       %ebp, %edx
        addl      96(%esp), %edx
        addl      %ecx, %edx
        roll      $7, %edx
        addl      104(%esp), %esi
        movl      124(%esp), %ecx
        andl      %edx, %ecx
        movl      %edx, %ebx
        xorl      $-1, %ebx
        andl      %edi, %ebx
        orl       %ebx, %ecx
        addl      %ecx, %esi
        roll      $11, %esi
        addl      100(%esp), %edi
        movl      %esi, %ebx
        andl      %edx, %ebx
        movl      %esi, %ecx
        xorl      $-1, %ecx
        andl      124(%esp), %ecx
        orl       %ecx, %ebx
        addl      %ebx, %edi
        roll      $19, %edi
        movl      124(%esp), %ecx
        addl      116(%esp), %ecx
        movl      %ecx, 124(%esp)
        movl      %edi, %ebx
        andl      %esi, %ebx
        movl      %edi, %ecx
        xorl      $-1, %ecx
        andl      %edx, %ecx
        orl       %ecx, %ebx
        movl      124(%esp), %ebp
        addl      %ebx, %ebp
        roll      $3, %ebp
        movl      %ebp, 124(%esp)
        movl      %edi, %ecx
        andl      %ebp, %ecx
        xorl      $-1, %ebp
        andl      %esi, %ebp
        orl       %ebp, %ecx
        addl      112(%esp), %ecx
        addl      %edx, %ecx
        roll      $7, %ecx
        addl      120(%esp), %esi
        movl      124(%esp), %edx
        andl      %ecx, %edx
        movl      %ecx, %ebx
        xorl      $-1, %ebx
        andl      %edi, %ebx
        orl       %ebx, %edx
        addl      %edx, %esi
        roll      $11, %esi
        movl      %esi, %ebx
        andl      %ecx, %ebx
        addl      %eax, %edi
        movl      %esi, %edx
        xorl      $-1, %edx
        andl      124(%esp), %edx
        orl       %ebx, %edx
        addl      %edx, %edi
        roll      $19, %edi
        movl      %esi, %ebp
        orl       %ecx, %ebp
        andl      %edi, %ebp
        orl       %ebx, %ebp
        movl      124(%esp), %edx
        lea       1518500249(%edx,%ebp), %edx
        addl      84(%esp), %edx
        roll      $3, %edx
        movl      %edi, %ebp
        orl       %esi, %ebp
        andl      %edx, %ebp
        movl      %edi, %ebx
        andl      %esi, %ebx
        orl       %ebx, %ebp
        lea       1518500249(%ebp,%ecx), %ecx
        addl      92(%esp), %ecx
        roll      $5, %ecx
        movl      %edi, %ebp
        orl       %edx, %ebp
        andl      %ecx, %ebp
        movl      %edi, %ebx
        andl      %edx, %ebx
        orl       %ebx, %ebp
        lea       1518500249(%esi,%ebp), %ebx
        addl      108(%esp), %ebx
        roll      $9, %ebx
        movl      %edx, %esi
        orl       %ecx, %esi
        andl      %ebx, %esi
        movl      %edx, %ebp
        andl      %ecx, %ebp
        orl       %ebp, %esi
        lea       1518500249(%edi,%esi), %edi
        addl      116(%esp), %edi
        roll      $13, %edi
        movl      %ebx, %esi
        orl       %ecx, %esi
        andl      %edi, %esi
        movl      %ebx, %ebp
        andl      %ecx, %ebp
        orl       %ebp, %esi
        lea       1518500249(%edx,%esi), %edx
        addl      68(%esp), %edx
        roll      $3, %edx
        movl      %edi, %esi
        orl       %ebx, %esi
        andl      %edx, %esi
        movl      %edi, %ebp
        andl      %ebx, %ebp
        orl       %ebp, %esi
        lea       1518500249(%esi,%ecx), %ebp
        addl      72(%esp), %ebp
        roll      $5, %ebp
        movl      %edi, %esi
        orl       %edx, %esi
        andl      %ebp, %esi
        movl      %edi, %ecx
        andl      %edx, %ecx
        orl       %ecx, %esi
        lea       1518500249(%ebx,%esi), %esi
        addl      96(%esp), %esi
        roll      $9, %esi
        movl      %edx, %ebx
        orl       %ebp, %ebx
        andl      %esi, %ebx
        movl      %edx, %ecx
        andl      %ebp, %ecx
        orl       %ecx, %ebx
        lea       1518500249(%edi,%ebx), %ecx
        addl      112(%esp), %ecx
        roll      $13, %ecx
        movl      %esi, %edi
        orl       %ebp, %edi
        andl      %ecx, %edi
        movl      %esi, %ebx
        andl      %ebp, %ebx
        orl       %ebx, %edi
        lea       1518500249(%edx,%edi), %ebx
        addl      80(%esp), %ebx
        roll      $3, %ebx
        movl      %ecx, %edi
        orl       %esi, %edi
        andl      %ebx, %edi
        movl      %ecx, %edx
        andl      %esi, %edx
        orl       %edx, %edi
        lea       1518500249(%edi,%ebp), %ebp
        addl      88(%esp), %ebp
        roll      $5, %ebp
        movl      %ecx, %edi
        orl       %ebx, %edi

.L45:
        andl      %ebp, %edi
        movl      %ecx, %edx
        andl      %ebx, %edx
        orl       %edx, %edi
        lea       1518500249(%esi,%edi), %edx
        addl      104(%esp), %edx
        roll      $9, %edx
        movl      %ebx, %edi
        orl       %ebp, %edi
        andl      %edx, %edi
        movl      %ebx, %esi
        andl      %ebp, %esi
        orl       %esi, %edi
        lea       1518500249(%ecx,%edi), %ecx
        addl      120(%esp), %ecx
        roll      $13, %ecx
        movl      %edx, %edi
        orl       %ebp, %edi
        andl      %ecx, %edi
        movl      %edx, %esi
        andl      %ebp, %esi
        orl       %esi, %edi
        lea       1518500249(%ebx,%edi), %ebx
        addl      64(%esp), %ebx
        roll      $3, %ebx
        movl      %ecx, %edi
        orl       %edx, %edi
        andl      %ebx, %edi
        movl      %ecx, %esi
        andl      %edx, %esi
        orl       %esi, %edi
        lea       1518500249(%edi,%ebp), %edi
        addl      76(%esp), %edi
        roll      $5, %edi
        movl      %ecx, %esi
        orl       %ebx, %esi
        andl      %edi, %esi
        movl      %ecx, %ebp
        andl      %ebx, %ebp
        orl       %ebp, %esi
        lea       1518500249(%edx,%esi), %esi
        addl      100(%esp), %esi
        roll      $9, %esi
        movl      %ebx, %ebp
        orl       %edi, %ebp
        andl      %esi, %ebp
        movl      %ebx, %edx
        andl      %edi, %edx
        orl       %edx, %ebp
        lea       1518500249(%ecx,%ebp), %ebp
        addl      %eax, %ebp
        roll      $13, %ebp
        movl      %ebp, %edx
        xorl      %esi, %edx
        xorl      %edi, %edx
        lea       1859775393(%ebx,%edx), %ebx
        addl      84(%esp), %ebx
        roll      $3, %ebx
        movl      %ebx, %edx
        xorl      %ebp, %edx
        xorl      %esi, %edx
        lea       1859775393(%edx,%edi), %ecx
        addl      108(%esp), %ecx
        roll      $9, %ecx
        movl      %ecx, %edx
        xorl      %ebx, %edx
        xorl      %ebp, %edx
        lea       1859775393(%esi,%edx), %edx
        addl      92(%esp), %edx
        roll      $11, %edx
        movl      %edx, %esi
        xorl      %ecx, %esi
        xorl      %ebx, %esi
        lea       1859775393(%ebp,%esi), %ebp
        addl      116(%esp), %ebp
        roll      $15, %ebp
        movl      %ebp, %esi
        xorl      %edx, %esi
        xorl      %ecx, %esi
        lea       1859775393(%ebx,%esi), %ebx
        addl      80(%esp), %ebx
        roll      $3, %ebx
        movl      %ebx, %esi
        xorl      %ebp, %esi
        xorl      %edx, %esi
        lea       1859775393(%esi,%ecx), %ecx
        addl      104(%esp), %ecx
        roll      $9, %ecx
        movl      %ecx, %esi
        xorl      %ebx, %esi
        xorl      %ebp, %esi
        lea       1859775393(%edx,%esi), %esi
        addl      88(%esp), %esi
        roll      $11, %esi
        movl      %esi, %edx
        xorl      %ecx, %edx
        xorl      %ebx, %edx
        lea       1859775393(%ebp,%edx), %ebp
        addl      120(%esp), %ebp
        roll      $15, %ebp
        movl      %ebp, %edx
        xorl      %esi, %edx
        xorl      %ecx, %edx
        lea       1859775393(%ebx,%edx), %edx
        addl      68(%esp), %edx
        roll      $3, %edx
        movl      %edx, %ebx
        xorl      %ebp, %ebx
        xorl      %esi, %ebx
        lea       1859775393(%ebx,%ecx), %edi
        addl      96(%esp), %edi
        roll      $9, %edi
        movl      %edi, %ecx
        xorl      %edx, %ecx
        xorl      %ebp, %ecx
        lea       1859775393(%esi,%ecx), %ebx
        addl      72(%esp), %ebx
        roll      $11, %ebx
        movl      %ebx, %ecx
        xorl      %edi, %ecx
        xorl      %edx, %ecx
        lea       1859775393(%ebp,%ecx), %ecx
        addl      112(%esp), %ecx
        roll      $15, %ecx
        movl      %ecx, %ebp
        xorl      %ebx, %ebp
        xorl      %edi, %ebp
        lea       1859775393(%edx,%ebp), %esi
        addl      64(%esp), %esi
        roll      $3, %esi
        movl      %esi, %edx
        xorl      %ecx, %edx
        xorl      %ebx, %edx
        lea       1859775393(%edx,%edi), %ebp
        addl      100(%esp), %ebp
        roll      $9, %ebp
        movl      %ebp, %edx
        xorl      %esi, %edx
        xorl      %ecx, %edx
        lea       1859775393(%ebx,%edx), %edx
        addl      76(%esp), %edx
        roll      $11, %edx
        movl      %edx, %ebx
        xorl      %ebp, %ebx
        xorl      %esi, %ebx
        lea       1859775393(%ecx,%ebx), %ecx
        addl      %eax, %ecx
        roll      $15, %ecx
        movl      152(%esp), %eax
        addl      %esi, (%eax)
        addl      %ecx, 4(%eax)
        addl      %edx, 8(%eax)
        addl      %ebp, 12(%eax)
        lea       (%esp), %ecx
        xorl      %eax, %eax
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

.L42:
        addl      $132, %esp
        popl      %ebx
        popl      %ebp
        popl      %esi
        popl      %edi
        ret
        .align    4,0x90


	.size	MD4Transform,.-MD4Transform
	.data

	.text


       .align    4,0x90



	.type	Encode,@function
Encode:
.L51:
        pushl     %esi
        pushl     %ebx
        subl      $12, %esp
        movl      24(%esp), %ebx
        movl      28(%esp), %esi
        movl      32(%esp), %eax
        xorl      %edx, %edx
        xorl      %ecx, %ecx
        testl     %eax, %eax
        jbe       .L55

.L52:
        movl      %ecx, 4(%esp)
        

.L53:
        movl      28(%esp), %ecx
        movb      (%ecx,%edx,4), %bl
        movl      24(%esp), %esi
        movl      4(%esp), %eax
        movb      %bl, (%eax,%esi)
        movl      28(%esp), %ecx
        movl      (%ecx,%edx,4), %ecx
        shrl      $8, %ecx
        movb      %cl, 1(%eax,%esi)
        movl      28(%esp), %ecx
        movb      2(%ecx,%edx,4), %cl
        movb      %cl, 2(%eax,%esi)
        movl      28(%esp), %ecx
        movb      3(%ecx,%edx,4), %cl
        movb      %cl, 3(%eax,%esi)
        incl      %edx
        addl      $4, %eax
        movl      %eax, 4(%esp)
        movl      32(%esp), %ecx
        cmpl      %ecx, %eax
        jb        .L53

.L55:
        addl      $12, %esp
        popl      %ebx
        popl      %esi
        ret
        .align    4,0x90



	.size	Encode,.-Encode
	.data

	.data

