#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <netinet/in.h>
#include <netdb.h>
#include <sys/socket.h>

#include <fcntl.h>
#include <errno.h>


void parse_url( char *input, char *host, int *port, char *pathname, char* outfile )
{
    char *p;

    p = strrchr(input,'/')+1;                      /* find start of pathname     */

    strcpy(outfile, p);

    *port = 80;                                 /* default port number        */
    if (!strncmp( input, "http://", 7 ))
        input += 7;
    else
        if (!strncmp( input, "http:", 5 )) input += 5;

    p = strchr(input,'/');                      /* find start of pathname     */
    if (p) {                                    /* pathname found             */
        strcpy( pathname, p );                  /* rest is pathname           */
        *p = 0;                                 /* terminator for host name   */
    } else {                                    /* else: no path              */
        strcpy( pathname, "/" );                /* path name defaults to '/'  */
    }

    p = strchr(input,':');                      /* port number specified ?    */
    if (p) {                                    /* port found                 */
        *p = 0;                                 /* terminator for host name   */
        p++;                                    /* move over '\0'             */
        *port = atoi( p );                      /* convert port to int        */
    }

    strcpy( host, input );
} /* end of parse_url */


int main(int argc, char ** argv){

  char *url = argv[1];
  char    host[256];
  char    pathname[4096];
  int port = 80;
  char outfile[256];
  int f;  
  struct  sockaddr_in address ;
  struct  hostent *hp;
    unsigned long ha;
  int     fd ;
  char buf[2000];
  int nread;

 parse_url (url, host, &port, pathname, outfile);



    printf( "host    = %s\n", host );

    printf( "port    = %d\n", port );
    printf( "path    = %s\n", pathname );
    printf( "outfile    = %s\n", outfile );


    f = open(outfile,  O_CREAT | O_TRUNC | O_WRONLY, 0644);

    ha = inet_addr ( host );
    if (ha == -1) {
        hp = gethostbyname ( host );
        if (!hp)
        {
            printf( "Host '%s' not found\n", host );
            exit(0);
        }
        ha = *(long *)*hp->h_addr_list;
    }

    address.sin_addr.s_addr = ha;
    address.sin_family = AF_INET;
    address.sin_port = htons(port);

    if ( ( fd =  socket(AF_INET, SOCK_STREAM, 0 ) ) <= 0 )
    {
        perror( "Socket failed" );
        close(f);
        exit(-1) ;
    }

    printf( "Connecting to %s\n", inet_ntoa(address.sin_addr) );

    if ( connect(fd, (struct sockaddr *)&address, (int)sizeof(address)) != 0 )
    {
        perror( "Connect failed" );
        close(f);
        exit(-1) ;
    }

    printf( "Connected\n") ;
    sprintf( buf, "GET %s\r\n", pathname );

     write(fd, buf, strlen(buf));

    while ((nread = read(fd, buf, sizeof(buf)))>0){
       write(f, buf, nread);
    }
    printf( "Downloaded\n") ;

    close(f);
    close(fd);
    return 0;
}



























#if 0
int     f;
char    *outfile;
int     rlen;
long    count;
int     port = 80;


int main(int argc, char ** argv)
{
    char   *url;
    char    buf[30000] ;
    short   f;
    short   error;
    short   len;

    int     header;
    int     lf;
    long long starttime;
    long    secs;
    long    left;
    long    size = 0;

    outfile = NULL;

    if (argc < 2) {
        fprintf( stderr, "Usage: wwwget [-proxy proxyhost] url [outfile]\n" );
        return 0;
    }

    if (argv[1][0] == '-')
    {
        if (!strcmp(argv[1], "-proxy"))
        {
            parse_url( argv[2], host, &port, pathname, &outfile );
            strcpy( pathname, argv[3] );
            outfile = argv[4];
        } else
        {
            printf("Unknown option: %s\n", argv[1] );
            exit(1);
        }
    } else
    {
        url = argv[1];
        outfile = argv[2];
        parse_url( url, host, &port, pathname , &outfile);
    }


    printf( "host    = %s\n", host );

    printf( "port    = %d\n", port );
    printf( "path    = %s\n", pathname );
    printf( "outfile    = %s\n", outfile );


    {
       FILE *f = fopen("wwwini","r");
       char buf2[200];
       while (!feof(f)) {
          fgets(buf2,200,f);
          if (!feof(f)) strcat( buf, buf2 );
       }
       fclose(f);
    }
    strcat(buf,"\r\n\r\n");

    rlen = (int)strlen(buf);
    if ( send(fd, buf, rlen, 0) <= 0 )
    {
        perror("send failed");
        if (outfile) {
           close(f);
           exit(1) ;
       }
    }

    count = 0;
    header = 1;
    lf = 0;
    starttime = gettimeofday();
    while ( (rlen = recv(fd, buf, sizeof(buf), 0 )) > 0) {
        char *p;
        buf[rlen] = 0;
        count += rlen;
        secs = (long)((gettimeofday() - starttime)/1000000);
        if (secs == 0) secs = 1;
        if (size > 0)
           left = (size-count)/(count/secs);
        else
           left = 0;
        if (!header && outfile)
            printf( "Received %ld bytes in %ld seconds (%ld bytes/sec;"
                    "%ld seconds left)\r",
                    count, secs, (count/secs), left );
        p = buf;
        while (header && rlen) {
           if (*p == '\n') {
               char *q;
               char cmpbuf[31];
               int i;
               if (lf) {       /* second lf in a row */
                  header = 0;
                  count = rlen-1;
               }
               lf = 1;
               strncpy( cmpbuf, p+1, 30 );
               for (i=0;i<30;i++) cmpbuf[i] = tolower(cmpbuf[i]);
               if (!strncmp( cmpbuf, "content-length:", 15 ))
               {
                   q = strchr( p+1, ':');
                   size = atol( q+1 );
               }
           } else {
              if (*p != '\r') lf = 0;
           }
           putchar(*p);
           p++;
           rlen--;
        }
        if (!header) {
           if (!outfile) {
              printf("%s", p );
           }
           else
           while (rlen) {
              int   wrcount;
              short countwritten;
              short error;

              wrcount = rlen;
              if (wrcount > 4096) wrcount = 4096;
              write( f, p, (short)wrcount, &countwritten );
              if (countwritten < wrcount) exit (2);
              rlen -= countwritten;
              if (rlen < 0) exit(2);
              p += countwritten;
           }
        }
    } /* end while */

    puts("");
    if (rlen<0) perror( "recv failed: ");
    if (outfile) close(f);

    close(fd) ;
    return 0;

} /* end main */

#endif

