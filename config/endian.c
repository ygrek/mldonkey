#include <stdio.h>

int litend() {
	int i = 0;
	((char *)(&i))[0] = 1;
	return(i == 1);
}

int bigend() {
	return !litend();
}

int main() {
	printf("#define __LITTLE_ENDIAN 1234\n");
	printf("#define __BIG_ENDIAN    4321\n");
	printf("#define __BYTE_ORDER __%s_ENDIAN\n",
	  litend() ? "LITTLE" : "BIG");
	exit(0);
}
