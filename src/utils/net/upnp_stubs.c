/**************************************************************************/
/*  Copyright (c) 2010 zzz <jian@doatest.com>                             */
/*                                                                        */
/*  mlupnp, a ocaml wraps of libminiupnpc & libnatpmp for mldonkey        */
/*                                                                        */
/*  Permission is hereby granted, free of charge, to any person           */
/*  obtaining a copy of this software and associated documentation files  */
/*  (the "Software"), to deal in the Software without restriction,        */
/*  including without limitation the rights to use, copy, modify, merge,  */
/*  publish, distribute, sublicense, and/or sell copies of the Software,  */
/*  and to permit persons to whom the Software is furnished to do so,     */
/*  subject to the following conditions:                                  */
/*                                                                        */
/*  The above copyright notice and this permission notice shall be        */
/*  included in all copies or substantial portions of the Software.       */
/*                                                                        */
/*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       */
/*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       */
/*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              */
/*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   */
/*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    */
/*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     */
/*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      */
/*  SOFTWARE.                                                             */
/**************************************************************************/
/* wraps for libminiupnpc libnatpmp*/

#include "../../../config/config.h"
#include "../../utils/lib/os_stubs.h"

#ifdef ENABLE_UPNP_NATPMP

#include <string.h>
#include <ctype.h>
#include <time.h>

#ifdef __MORPHOS__
#include <inttypes.h>
#endif  /* __MORPHOS__ */

#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif /* HAVE_SYS_RESOURCE_H */

#include <pthread.h>
#include <signal.h>
#include <sys/time.h>

#ifdef WIN32
 #include <inttypes.h>
 #include <winsock2.h>
 #include <WS2tcpip.h>
 typedef int socklen_t;
#else
 #include <sys/types.h>
 #include <sys/socket.h>
 #include <netinet/in.h>
 #include <arpa/inet.h>
#endif

#ifdef WIN32
 #define ECONNREFUSED            WSAECONNREFUSED
 #define ECONNRESET              WSAECONNRESET
 #define EHOSTUNREACH            WSAEHOSTUNREACH
 #define EINPROGRESS             WSAEINPROGRESS
 #define ENOTCONN                WSAENOTCONN
 #define EWOULDBLOCK             WSAEWOULDBLOCK
 #define EAFNOSUPPORT            WSAEAFNOSUPPORT
 #define ENETUNREACH             WSAENETUNREACH
 #define sockerrno               WSAGetLastError( )
#else
 #include <errno.h>
 #define sockerrno errno
#endif

#include <miniupnpc/miniupnpc.h>
#include <miniupnpc/upnpcommands.h>
#define ENABLE_STRNATPMPERR
#include <natpmp.h>

#undef  DEBUG_PRINTF_UPNP
#ifdef  DEBUG_PRINTF_UPNP
#define dbg_printf printf
#else
static void dbg_printf (const char *str, ... ){}
#endif

/* can hold max 20 port forwarding, is it enough for all mldonkey bt/ed2k/kad/ft clients I think */
#define MAX_MAPS 20
#define MAX_THREAD_WAIT 30
#define COMMAND_WAIT_SECS 8
#define LIFETIME_SECS 3600

enum
{
	UPNP_IGD_NONE = 0,
	UPNP_IGD_VALID_CONNECTED = 1,
	UPNP_IGD_VALID_NOT_CONNECTED = 2,
	UPNP_IGD_INVALID = 3
};

typedef enum
{
    ML_UPNP_IDLE = 0,
    ML_UPNP_ERR,
    ML_UPNP_DISCOVER,
    ML_UPNP_MAP,
    ML_UPNP_UNMAP
}
ml_upnp_state;

typedef enum
{
	ML_NATPMP_IDLE = 0,
	ML_NATPMP_ERR,
	ML_NATPMP_DISCOVER,
	ML_NATPMP_RECV_PUB,
	ML_NATPMP_SEND_MAP,
	ML_NATPMP_RECV_MAP,
	ML_NATPMP_SEND_UNMAP,
	ML_NATPMP_RECV_UNMAP
}
ml_natpmp_state;

typedef enum
{
    ML_PORT_ERROR  = 0,
    ML_PORT_UNMAPPED,
    ML_PORT_UNMAPPING,
    ML_PORT_MAPPING,
    ML_PORT_MAPPED
}
ml_port_forwarding;

typedef struct ml_upnpmp_t
{
	int                enabled;
	unsigned short int intPort;
	unsigned short int extPort;
	int                isTcp;	/*tcp=1, udp=0*/
	char               notes[32];
    char               lanaddr[16];
    int                doPortCheck;
    time_t             overTime;
    ml_port_forwarding natpmpStatus;
    ml_port_forwarding upnpStatus;

    int                upnpDiscovered;
    unsigned int       upnpMapped;
    ml_upnp_state      upnpState;
    struct UPNPUrls    upnpUrls;
    struct IGDdatas    upnpData;

    unsigned int       natpmpMapped;
    int                natpmpDiscovered;
    ml_natpmp_state    natpmpState;
    time_t             renewTime;
    time_t             commandTime;
    natpmp_t           natpmp;
}ml_upnpmp_t;

static ml_upnpmp_t g_maps[MAX_MAPS];
static ml_upnpmp_t g_unmaps[MAX_MAPS];
static int g_inited = 0;
static int g_running = 0;
static int g_stop = 0;
static pthread_t g_pthread;
//static pthread_cond_t cond;
static pthread_mutex_t g_mutex;
static pthread_mutex_t g_delay_mutex;


/** @brief init memset 0 g_maps g_unmaps */
static void init_maps()
{
	if ( ! g_inited ){
		memset( g_maps, 0, sizeof(g_maps) );
		memset( g_unmaps, 0, sizeof(g_unmaps) );
		g_inited = 1;
	}
}

/** @brief add a ml_upnpmp_t item to g_maps */
static int maps_add_item( ml_upnpmp_t * map )
{
	int i;
	int found;

	if ( ! g_inited ){
		dbg_printf("g_maps not initialize!\n");
		return -1;
	}

	found = 0;
	for (i = 0; i < MAX_MAPS; i++){
		if ( ! g_maps[i].enabled ){
			continue;
		}
		if (g_maps[i].intPort == map->intPort && \
			g_maps[i].extPort == map->extPort && \
			g_maps[i].isTcp == map->isTcp){
			found = 1;
			dbg_printf("intPort %d, extPort %d, isTcp %d existed\n", map->intPort, map->extPort, map->isTcp);
			break;
		}
	}

	if ( ! found ){
		for(i = 0; i < MAX_MAPS; i++){
			if ( ! g_maps[i].enabled ){
				pthread_mutex_lock(&g_mutex);
				memcpy(&g_maps[i], map, sizeof(ml_upnpmp_t));
				g_maps[i].enabled = 1;
				g_maps[i].upnpState   = ML_UPNP_DISCOVER;
				g_maps[i].natpmpState = ML_NATPMP_DISCOVER;
				g_maps[i].natpmp.s = -1; /* socket */
				g_maps[i].upnpStatus   = ML_PORT_UNMAPPED;
				g_maps[i].natpmpStatus = ML_PORT_UNMAPPED;
				g_maps[i].overTime = 0;
				pthread_mutex_unlock(&g_mutex);
				dbg_printf("intPort %d, extPort %d, isTcp %d added\n", map->intPort, map->extPort, map->isTcp);
				return 0;
				break;
			}
		}
	}

	return 1;
}

/** @brief remove a ml_upnpmp_t item form g_maps, and add to g_unmaps */
static int maps_remove_item( ml_upnpmp_t * map )
{
	int i,j;
	int found;

	if ( ! g_inited ){
		dbg_printf("g_maps not initialize!\n");
		return -1;
	}

	found = 0;
	for (i = 0; i < MAX_MAPS; i++){
		if ( ! g_maps[i].enabled ){
			continue;
		}
		if (g_maps[i].intPort == map->intPort && \
			g_maps[i].extPort == map->extPort && \
			g_maps[i].isTcp == map->isTcp){
			found = 1;
			for(j = 0; j < MAX_MAPS; j++){
				if ( !g_unmaps[j].extPort ){
					pthread_mutex_lock(&g_mutex);
					memcpy(&g_unmaps[j], &g_maps[i], sizeof(ml_upnpmp_t));
					g_unmaps[j].enabled = 0;
					g_unmaps[j].overTime = 0;
					memset(&g_maps[i], 0, sizeof(ml_upnpmp_t));
					pthread_mutex_unlock(&g_mutex);
					dbg_printf("intPort %d, extPort %d, isTcp %d moved from maps to unmaps\n", map->intPort, map->extPort, map->isTcp);
					return 0;
					break;
				}
			}
			break;
		}
	}

	if ( ! found ){
		for (i = 0; i < MAX_MAPS; i++){
			if (g_unmaps[i].intPort == map->intPort && \
				g_unmaps[i].extPort == map->extPort && \
				g_unmaps[i].isTcp == map->isTcp){
				dbg_printf("intPort %d, extPort %d, isTcp %d existed in unmaps\n", map->intPort, map->extPort, map->isTcp);
				return 0;
				break;
			}
		}
		for(j = 0; j < MAX_MAPS; j++){
			if ( !g_unmaps[j].extPort ){
				pthread_mutex_lock(&g_mutex);
				memcpy(&g_unmaps[j], map, sizeof(ml_upnpmp_t));
				g_unmaps[j].enabled = 0;
				g_unmaps[j].overTime = 0;
				pthread_mutex_unlock(&g_mutex);
				dbg_printf("intPort %d, extPort %d, isTcp %d add to unmaps\n", map->intPort, map->extPort, map->isTcp);
				return 0;
				break;
			}
		}
	}

	return 1;
}

#if 0
/** @brief new ml_upnpmp_t struct from int_port, ext_port, type, notes */
value
ml_upnpSetMaps(value map_list_v) {
	ml_upnpmp_t map;
	int i,j;
	int found;
	char *s = NULL;
	value map_v;

	if ( ! g_inited ){
		dbg_printf("g_maps not initialize!\n");
		return Val_unit;
	}
	const int len = Wosize_val(map_list_v);
	//move the map from g_maps to g_unmaps that noexist in new map_list_v
	for (i = 0; i < MAX_MAPS; i++){
		if ( ! g_maps[i].enabled ){
			continue;
		}
		found = 0;
		for(j = 0; j<len && j < MAX_MAPS; j++){
			//memset((void*) &map, 0, sizeof(map));
			map_v = Field(map_list_v, j);
			map.enabled  = Int_val(Field(map_v, 0));
			if ( ! map.enabled ){
				continue;
			}
			map.intPort = Int_val(Field(map_v, 1));
			map.extPort = Int_val(Field(map_v, 2));
			map.isTcp   = Int_val(Field(map_v, 3));
			//strncpy(map.notes, String_val(Field(map_v, 4)), 32-1);

			if (g_maps[i].intPort == map.intPort && \
				g_maps[i].extPort == map.extPort && \
				g_maps[i].isTcp   == map.isTcp){
				found = 1;
				break;
			}
		}

		if ( ! found ){
			for(j = 0; j < MAX_MAPS; j++){
				if ( ! g_unmaps[j].enabled && ! g_unmaps[j].extPort ){
					pthread_mutex_lock(&g_mutex);
					memcpy(&g_unmaps[j], &g_maps[i], sizeof(ml_upnpmp_t));
					g_unmaps[j].enabled = 0;
					memset(&g_maps[i], 0, sizeof(ml_upnpmp_t));
					pthread_mutex_unlock(&g_mutex);
					break;
				}
			}
		}
	}

	//add the new map from map_list_v to g_maps that noexist in g_maps
	for (i = 0; i < len && i<MAX_MAPS; i++){
		memset((void*) &map, 0, sizeof(map));
		map_v = Field(map_list_v, i);
		map.enabled  = Int_val(Field(map_v, 0));
		if ( ! map.enabled ){
			continue;
		}
		map.intPort = Int_val(Field(map_v, 1));
		map.extPort = Int_val(Field(map_v, 2));
		map.isTcp   = Int_val(Field(map_v, 3));
		s = String_val(Field(map_v, 4));
		if (s && *s){
			strncpy(map.notes, s, 32-1);
		}else{
			strncpy(map.notes, "mldonkey", 32-1);
		}

		found = 0;
		for(j = 0; j < MAX_MAPS; j++){
			if (g_maps[j].intPort == map.intPort && \
				g_maps[j].extPort == map.extPort && \
				g_maps[j].isTcp   == map.isTcp){
				found = 1;
				break;
			}
		}
		if ( ! found  ){
			maps_add_item( &map );
		}
	}

	return Val_unit;
}
#endif

/** @brief dump ml_upnpmp_t struct */
static void
dumpMap(const ml_upnpmp_t * map, int type)
{
	if (type){
		printf("map->enabled = %d\n", map->enabled);
		printf("map->intPort = %d\n", map->intPort);
		printf("map->extPort = %d\n", map->extPort);
		printf("map->isTcp   = %d\n", map->isTcp);
		printf("map->notes   = %s\n", map->notes);
		printf("map->lanaddr = %02x:%02x:%02x::::%02x:%02x:%02x\n", map->lanaddr[0], map->lanaddr[1], map->lanaddr[2], map->lanaddr[13], map->lanaddr[14], map->lanaddr[15]);

		printf("map->upnpDscv    = %d\n", map->upnpDiscovered);
		printf("map->upnpMapped  = %d\n", map->upnpMapped);
		printf("map->upnpState   = %d\n", map->upnpState);

		printf("map->natpmpDscv  = %d\n", map->natpmpDiscovered);
		printf("map->natpmpMapped= %d\n", map->natpmpMapped);
		printf("map->natpmpState = %d\n", map->natpmpState);

		printf("map->overTime    = %d\n", (unsigned int)map->overTime);
		printf("map->natpmpStatus= %d\n", map->natpmpStatus);
		printf("map->upnpStatus  = %d\n", map->upnpStatus);
	}else{
		printf("%1d ", map->enabled);
		printf("%6d ", map->intPort);
		printf("%6d ", map->extPort);
		printf("%1d ", map->isTcp);
		printf("%16s ", map->notes);
		printf("%02x:%02x:%02x::::%02x:%02x:%02x ", map->lanaddr[0], map->lanaddr[1], map->lanaddr[2], map->lanaddr[13], map->lanaddr[14], map->lanaddr[15]);

		printf("%1d ", map->upnpDiscovered);
		printf("%1d ", map->upnpMapped);
		printf("%1d ", map->upnpState);

		printf("%1d ", map->natpmpDiscovered);
		printf("%1d ", map->natpmpMapped);
		printf("%1d ", map->natpmpState);
		printf("%10d ", (unsigned int)map->overTime);
		printf("%1d ", map->natpmpStatus);
		printf("%1d ", map->upnpStatus);
		printf("\n");
	}
}

value
ml_upnpDumpMaps(value unused)
{
	int i;
	ml_upnpmp_t * map;

	printf("=============== g_unmaps ===============\n");
	printf("enabled intPort extPort isTcp notes lanaddr upnpDscv upnpMapped upnpState natpmpDscv natpmpMapped natpmpState overTime natpmpStatus upnpStatus\n");
	for(i = 0; i < MAX_MAPS; i++ ){
		map = &g_unmaps[i];
		dumpMap( map, 0 );
	}
	printf("=============== g_maps ===============\n");
	printf("enabled intPort extPort isTcp notes lanaddr upnpDscv upnpMapped upnpState natpmpDscv natpmpMapped natpmpState overTime natpmpStatus upnpStatus\n");
	for(i = 0; i < MAX_MAPS; i++ ){
		map = &g_maps[i];
		dumpMap( map, 0 );
	}

	return Val_unit;
}


static inline int
getStatus( const ml_upnpmp_t * map )
{
	if (map->natpmpStatus > map->upnpStatus)
		return map->natpmpStatus;
	else
		return map->upnpStatus;
}

static const char*
getNatStateStr( const int state )
{
    switch( state )
    {
        case ML_PORT_MAPPING:   return "Starting";
        case ML_PORT_MAPPED:    return "Forwarded";
        case ML_PORT_UNMAPPING: return "Stopping";
        case ML_PORT_UNMAPPED:  return "Not forwarded";
        default:                return "???";
    }
}

const char*
str_errno( const int i )
{
    const char * ret = strerror( i );

    if( ret == NULL )
        ret = "Unknown Error";
    return ret;
}

static int
canSendCommand( ml_upnpmp_t * map )
{
    return time(NULL) >= map->commandTime;
}

static void
setCommandTime( ml_upnpmp_t * map )
{
    map->commandTime = time(NULL) + COMMAND_WAIT_SECS;
}

static int
natpmpPulse( ml_upnpmp_t * map )
{
    int ret;

    if( map->enabled && ( map->natpmpState == ML_NATPMP_DISCOVER ) )
    {
        int val = initnatpmp( &map->natpmp, 0, 0 );
        dbg_printf( "initnatpmp = %d\n", val );
        val = sendpublicaddressrequest( &map->natpmp );
        dbg_printf( "sendpublicaddressrequest = %d\n", val );
        map->natpmpState = val < 0 ? ML_NATPMP_ERR : ML_NATPMP_RECV_PUB;
        map->natpmpDiscovered = 1;
        setCommandTime( map );
    }

    if( ( map->natpmpState == ML_NATPMP_RECV_PUB ) && canSendCommand( map ) )
    {
        natpmpresp_t response;
        const int    val = readnatpmpresponseorretry( &map->natpmp,
                                                      &response );
        dbg_printf( "readnatpmpresponseorretry = %d\n", val );
        if( val >= 0 )
        {
            dbg_printf( "Found public address \"%s\"\n", inet_ntoa( response.pnu.publicaddress.addr ) );
            map->natpmpState = ML_NATPMP_IDLE;
        }
        else if( val != NATPMP_TRYAGAIN )
        {
        	 map->natpmpState = ML_NATPMP_ERR;
        }
    }

    if( (  map->natpmpState == ML_NATPMP_IDLE ) || (  map->natpmpState == ML_NATPMP_ERR ) )
    {
        if(  map->natpmpMapped && ( ! map->enabled ) )
            map->natpmpState = ML_NATPMP_SEND_UNMAP;
    }

    if( ( map->natpmpState == ML_NATPMP_SEND_UNMAP ) && canSendCommand( map ) )
    {
        const int val =
            sendnewportmappingrequest( &map->natpmp, NATPMP_PROTOCOL_TCP,
                                       map->intPort, map->extPort,
                                       0 );
        dbg_printf( "sendnewportmappingrequest = %d\n", val );
        map->natpmpState = val < 0 ? ML_NATPMP_ERR : ML_NATPMP_RECV_UNMAP;
        setCommandTime( map );
    }

    if( map->natpmpState == ML_NATPMP_RECV_UNMAP )
    {
        natpmpresp_t resp;
        const int    val = readnatpmpresponseorretry( &map->natpmp, &resp );
        dbg_printf( "readnatpmpresponseorretry = %d\n", val );
        if( val >= 0 )
        {
            const int p = resp.pnu.newportmapping.privateport;
            dbg_printf( "no longer forwarding port %d\n", p );
            if( map->extPort == p )
            {
            	map->extPort = 0;
            	map->natpmpState = ML_NATPMP_IDLE;
                map->natpmpMapped = 0;
            }
        }
        else if( val != NATPMP_TRYAGAIN )
        {
        	map->natpmpState = ML_NATPMP_ERR;
        }
    }

    if( map->natpmpState == ML_NATPMP_IDLE )
    {
        if( map->enabled && !map->natpmpMapped && map->natpmpDiscovered )
        	map->natpmpState = ML_NATPMP_SEND_MAP;

        else if( map->natpmpMapped && time(NULL) >= map->renewTime )
            map->natpmpState = ML_NATPMP_SEND_MAP;
    }

    if( ( map->natpmpState == ML_NATPMP_SEND_MAP ) && canSendCommand( map ) )
    {
        const int val =
            sendnewportmappingrequest( &map->natpmp, NATPMP_PROTOCOL_TCP,
                                       map->intPort,
                                       map->extPort,
                                       LIFETIME_SECS );
        dbg_printf( "sendnewportmappingrequest = %d\n", val );
        map->natpmpState = val < 0 ? ML_NATPMP_ERR : ML_NATPMP_RECV_MAP;
        setCommandTime( map );
    }

    if( map->natpmpState == ML_NATPMP_RECV_MAP )
    {
        natpmpresp_t resp;
        const int    val = readnatpmpresponseorretry( &map->natpmp, &resp );
        dbg_printf( "readnatpmpresponseorretry = %d\n", val );
        if( val >= 0 )
        {
        	map->natpmpState = ML_NATPMP_IDLE;
            map->natpmpMapped = 1;
            map->renewTime = time(NULL) + LIFETIME_SECS;
            map->extPort = resp.pnu.newportmapping.privateport;
            dbg_printf( "Port %d forwarded successfully\n", map->extPort );
        }
        else if( val != NATPMP_TRYAGAIN )
        {
            map->natpmpState = ML_NATPMP_ERR;
        }
    }

    switch( map->natpmpState )
    {
        case ML_NATPMP_IDLE:
            ret = map->natpmpMapped ? ML_PORT_MAPPED : ML_PORT_UNMAPPED; break;

        case ML_NATPMP_DISCOVER:
            ret = ML_PORT_UNMAPPED; break;

        case ML_NATPMP_RECV_PUB:
        case ML_NATPMP_SEND_MAP:
        case ML_NATPMP_RECV_MAP:
            ret = ML_PORT_MAPPING; break;

        case ML_NATPMP_SEND_UNMAP:
        case ML_NATPMP_RECV_UNMAP:
            ret = ML_PORT_UNMAPPING; break;

        default:
            ret = ML_PORT_ERROR; break;
    }
    return ret;
}


static int
upnpPulse( ml_upnpmp_t * map )
{
    int ret;

    if( map->enabled && ( map->upnpState == ML_UPNP_DISCOVER ) )
    {
        struct UPNPDev * devlist;
        errno = 0;
        devlist = upnpDiscover(
          2000, /* delay */
          NULL, /* multicastif */
          NULL, /* minissdpsock */
          0,    /* localport */
          0,    /* ipv6 */
#if MINIUPNPC_API_VERSION >= 14
          2,    /* ttl */
#endif
          NULL  /* error */
        );
        if( devlist == NULL )
        {
            dbg_printf( "upnpDiscover failed (errno %d - %s)\n", errno,  str_errno( errno ) );
        }
        errno = 0;
        if( UPNP_IGD_VALID_CONNECTED == UPNP_GetValidIGD( devlist, &map->upnpUrls, &map->upnpData,
        		map->lanaddr, sizeof( map->lanaddr ) ) )
        {
            dbg_printf( "Found Internet Gateway Device \"%s\" \n", map->upnpUrls.controlURL );
            dbg_printf( "Local Address is \"%s\" \n", map->lanaddr );
            map->upnpState = ML_UPNP_IDLE;
            map->upnpDiscovered = 1;
        }
        else
        {
            map->upnpState = ML_UPNP_ERR;
            dbg_printf( "UPNP_GetValidIGD failed (errno %d - %s)\n", errno, str_errno( errno ) );
            dbg_printf( "If your router supports UPnP, please make sure UPnP is enabled!\n" );
        }
        freeUPNPDevlist( devlist );
    }

    if( map->upnpState == ML_UPNP_IDLE )
    {
        if( map->upnpMapped && ( !map->enabled ) )
        	map->upnpState = ML_UPNP_UNMAP;
    }

    if( map->enabled && map->upnpMapped && map->doPortCheck )
    {
        char portStr[8];
        char intPort[8];
        char intClient[16];
        char type[8];
        int i;

        snprintf( portStr, sizeof( portStr ), "%d", map->extPort );
        snprintf( type, sizeof( type ), "%s", ( map->isTcp ? "TCP" : "UDP" ) );
        i = UPNP_GetSpecificPortMappingEntry( map->upnpUrls.controlURL,
                                              map->upnpData.first.servicetype, portStr,
                                              type,
#if MINIUPNPC_API_VERSION >= 10
                                              NULL, /* remoteHost */
#endif
                                              intClient, intPort, NULL, NULL, NULL );
        if( i != UPNPCOMMAND_SUCCESS )
        {
            dbg_printf( "Port %d isn't forwarded\n", map->extPort );
            map->upnpMapped = 0;
        }
        map->doPortCheck = 0;
    }

    if( map->upnpState == ML_UPNP_UNMAP )
    {
        char portStr[16];
        char type[8];
        snprintf( portStr, sizeof( portStr ), "%d", map->extPort );
        snprintf( type, sizeof( type ), "%s", ( map->isTcp ? "TCP" : "UDP" ) );
        UPNP_DeletePortMapping( map->upnpUrls.controlURL,
                                map->upnpData.first.servicetype,
                                portStr, type, NULL );
        dbg_printf( "Stopping port forwarding through \"%s\", service \"%s\"\n", map->upnpUrls.controlURL, map->upnpData.first.servicetype );
        map->upnpMapped = 0;
        map->upnpState = ML_UPNP_IDLE;
        map->extPort = 0;
    }

    if( map->upnpState == ML_UPNP_IDLE )
    {
        if( map->enabled && !map->upnpMapped )
            map->upnpState = ML_UPNP_MAP;
    }

    if( map->upnpState == ML_UPNP_MAP )
    {
        int  err = -1;
        errno = 0;

        if( !map->upnpUrls.controlURL || !map->upnpData.first.servicetype )
            map->upnpMapped = 0;
        else
        {
            char intPortStr[16];
            char extPortStr[16];
            char desc[64];
            char type[8];
            snprintf( intPortStr, sizeof( intPortStr ), "%d", map->intPort );
            snprintf( extPortStr, sizeof( extPortStr ), "%d", map->extPort );
            snprintf( desc, sizeof( desc ), "%s", map->notes );
            snprintf( type, sizeof( type ), "%s", ( map->isTcp ? "TCP" : "UDP" ) );
            err = UPNP_AddPortMapping( map->upnpUrls.controlURL,
                                       map->upnpData.first.servicetype,
                                       extPortStr, intPortStr, map->lanaddr,
                                       desc, type, NULL, "0" );
            map->upnpMapped = !err;
        }
        dbg_printf( "Port forwarding through \"%s\", service \"%s\". (local address[%s:%d])\n", map->upnpUrls.controlURL, map->upnpData.first.servicetype, map->lanaddr, map->intPort );
        if( map->upnpMapped )
        {
            dbg_printf( "Port forwarding successful!\n" );
            //handle->port = port;
            map->upnpState = ML_UPNP_IDLE;
        }
        else
        {
            dbg_printf( "Port forwarding failed with error %d (errno %d - %s)\n", err, errno, str_errno( errno ) );
            dbg_printf( "If your router supports UPnP, please make sure UPnP is enabled!\n" );
            //handle->port = -1;
            map->upnpState = ML_UPNP_ERR;
        }
    }

    switch( map->upnpState )
    {
        case ML_UPNP_DISCOVER:
            ret = ML_PORT_UNMAPPED; break;

        case ML_UPNP_MAP:
            ret = ML_PORT_MAPPING; break;

        case ML_UPNP_UNMAP:
            ret = ML_PORT_UNMAPPING; break;

        case ML_UPNP_IDLE:
            ret = map->upnpMapped ? ML_PORT_MAPPED
                  : ML_PORT_UNMAPPED; break;

        default:
            ret = ML_PORT_ERROR; break;
    }

    return ret;
}


static void *
upnpNatpmpThread( )
{
	int oldStatus, newStatus;
	int i, err;
	time_t now;
	struct timespec deltatime;

	pthread_mutex_lock(&g_mutex);
	g_stop = 0;
	pthread_mutex_unlock(&g_mutex);

	while( g_running ){
		//clear in g_unmaps
		now = time(NULL);
		for(i = 0; i < MAX_MAPS; i++){
			if ( ! g_running )	break;

			if ( ! g_unmaps[i].enabled && g_unmaps[i].extPort > 0 ){
				if ( g_unmaps[i].overTime < now ){
					oldStatus = getStatus( &g_unmaps[i] );

					pthread_mutex_lock(&g_mutex);

					switch (oldStatus){
			        case ML_PORT_MAPPED:
			        	g_unmaps[i].overTime = now + 60 * 20;
			        	break;
			        case ML_PORT_ERROR:
			        	g_unmaps[i].overTime = now + 60;
			        	break;
			        default:
			            /* in progress.  pulse frequently. */
			        	g_unmaps[i].overTime = now + 333;
			            break;
					}
					g_unmaps[i].natpmpStatus = natpmpPulse( &g_unmaps[i] );
					g_unmaps[i].doPortCheck = 0;
					g_unmaps[i].upnpStatus  = upnpPulse( &g_unmaps[i]);
					dbg_printf( "upnpNatpmpThread g_unmaps[%d]\n", i);

#ifdef DEBUG_PRINTF_UPNP
					dumpMap(&g_unmaps[i], 1);
#endif
					newStatus    = getStatus( &g_unmaps[i] );

					if( newStatus != oldStatus )
						dbg_printf( "port forwarding state changed from \"%s\" to \"%s\"\n", getNatStateStr( oldStatus ),	getNatStateStr( newStatus ) );
					if( newStatus == ML_PORT_UNMAPPED ){
						memset(&g_unmaps[i], 0, sizeof(ml_upnpmp_t));
					}
					pthread_mutex_unlock(&g_mutex);
				}
			}
		}
		//create,check in g_maps
		now = time(NULL);
		for(i = 0; i < MAX_MAPS; i++){
			if ( ! g_running )	break;

			if ( g_maps[i].enabled && g_maps[i].extPort ){
				if ( g_maps[i].overTime < now ){
					oldStatus = getStatus( &g_maps[i] );

					pthread_mutex_lock(&g_mutex);

					switch (oldStatus){
			        case ML_PORT_MAPPED:
			        	g_maps[i].overTime = now + 60 * 20;
			        	break;
			        case ML_PORT_ERROR:
			        	g_maps[i].overTime = now + 60;
			        	break;
			        default:
			            /* in progress.  pulse frequently. */
			        	g_maps[i].overTime = now + 333;
			            break;
					}
					g_maps[i].natpmpStatus = natpmpPulse( &g_maps[i] );
					g_maps[i].doPortCheck = 1;
					g_maps[i].upnpStatus   = upnpPulse( &g_maps[i] );

					pthread_mutex_unlock(&g_mutex);

					dbg_printf( "upnpNatpmpThread g_maps[%d]\n", i);
#ifdef DEBUG_PRINTF_UPNP
					dumpMap(&g_maps[i], 1);
#endif

					newStatus = getStatus( &g_maps[i] );

					if( newStatus != oldStatus )
						dbg_printf( "port forwarding state changed from \"%s\" to \"%s\"\n", getNatStateStr( oldStatus ), getNatStateStr( newStatus ) );
				}
			}
		}
		if ( g_running ){
			deltatime.tv_sec = time(NULL) + 30;
			deltatime.tv_nsec = 0;
#if defined(_POSIX_TIMEOUTS) && (_POSIX_TIMEOUTS - 200112L) >= 0L
			err = pthread_mutex_timedlock(&g_delay_mutex, &deltatime);
			dbg_printf("%d seconds timedlock err=%d, running...\n", deltatime.tv_sec, err);
#else
			do {
				err = pthread_mutex_trylock(&g_delay_mutex);
				if(err == EBUSY){
					struct timespec ts;
					ts.tv_sec = 0;
					ts.tv_nsec = 100000000;
					int status = -1;
					while (status == -1) status = nanosleep(&ts, &ts);
				} else break;
				//dbg_printf("trylock err=%d, running...\n", err);
			} while (err != 0 && (time(NULL) < deltatime.tv_sec));
#endif
		}else{
			break;
		}
	}
	pthread_mutex_lock(&g_mutex);
	g_stop = 1;
	pthread_mutex_unlock(&g_mutex);
	dbg_printf("upnp thread stopped!\n");

	return NULL;
}

static void
upnp_thread_start( )
{
	int err;
	if ( ! g_inited ){
		init_maps();
	}

	if ( ! g_running ){
	    pthread_attr_t attr;
	    pthread_attr_init(&attr);
	    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);

	    //pthread_cond_init(&cond, NULL);
	    pthread_mutex_init(&g_mutex, NULL);
	    pthread_mutex_init(&g_delay_mutex, NULL);

	    err = pthread_create(&g_pthread, &attr, upnpNatpmpThread, NULL);

	    if(err){
	    	perror("Error while starting upnp thread\n");
	    	exit(2);
	    }
		g_inited  = 1;
	    pthread_mutex_lock(&g_mutex);
		g_running = 1;
		g_stop = 0;
	    pthread_mutex_unlock(&g_mutex);
	}else{
		dbg_printf("threading had started\n");
	}
	return;
}

static void
upnp_thread_stop( int wait_seconds )
{
	int i = 0;
	int wait = wait_seconds;

	if (wait < 1) wait = MAX_THREAD_WAIT;

	pthread_mutex_lock(&g_mutex);
	g_stop = 0;
	g_running = 0;
	pthread_mutex_unlock(&g_mutex);
	pthread_mutex_unlock(&g_delay_mutex);

	for (i=0; i<wait; i++){
		if (g_stop){
			pthread_mutex_lock(&g_mutex);
			g_stop = 0;
			pthread_mutex_unlock(&g_mutex);
			break;
		}else{
			sleep(1);
		}
	}

	if (i >= wait){
		pthread_cancel(g_pthread);
	}
	pthread_mutex_destroy(&g_mutex);
	pthread_mutex_destroy(&g_delay_mutex);
	return;
}

/** @brief ocaml api, init memset g_maps g_unmaps to 0 */
value
ml_upnpInitMaps(value unused)
{
	init_maps();
	return Val_unit;
}


/** @brief ocaml api, start the upnp thread */
value
ml_upnp_job_start( value unused )
{
	upnp_thread_start( );
	return Val_unit;
}


/** @brief ocaml api, stop the upnp thread */
value
ml_upnp_job_stop( value wait_v )
{
	int wait;
	wait = Val_int( wait_v );

	upnp_thread_stop( wait );
	return Val_unit;
}

/** @brief caml api, new ml_upnpmp_t struct from int_port, ext_port, type, notes, and add to g_maps */
value
ml_upnpAddMap(value m_enabled, value m_intPort, value m_extPort, value m_type, value m_notes)
{
	ml_upnpmp_t map;
	const char *s = String_val(m_notes);

	memset(&map, 0, sizeof(ml_upnpmp_t));

	map.enabled = Int_val(m_enabled);
	map.intPort = Int_val(m_intPort);
	map.extPort = Int_val(m_extPort);
	map.isTcp   = Int_val(m_type);
	if (s && *s){
		strncpy(map.notes, s, 32-1);
	}else{
		strncpy(map.notes, "mldonkey", 32-1);
	}

	maps_add_item( &map );

	return Val_unit;
}

/** @brief caml api, new ml_upnpmp_t struct from int_port, ext_port, type, notes, and remove from g_maps, add to g_unmaps */
value
ml_upnpRemoveMap(value m_enabled, value m_intPort, value m_extPort, value m_type, value m_notes)
{
	ml_upnpmp_t map;
	const char *s = String_val(m_notes);

	memset(&map, 0, sizeof(ml_upnpmp_t));

	map.enabled = Int_val(m_enabled);
	map.intPort = Int_val(m_intPort);
	map.extPort = Int_val(m_extPort);
	map.isTcp   = Int_val(m_type);
	if (s && *s){
		strncpy(map.notes, s, 32-1);
	}else{
		strncpy(map.notes, "mldonkey", 32-1);
	}
	maps_remove_item( &map );

	return Val_unit;
}

/** @brief caml api, stop all maps in g_maps, call before mldonkey exit to clear router */
value
ml_upnpRemoveAllMaps(value wait_v)
{
	int i,j;
	int found;
	int wait;
	wait = Val_int( wait_v );
	if (wait < 1) wait = MAX_THREAD_WAIT;

	if ( ! g_inited ){
		dbg_printf("g_maps not initialize!\n");
		return Val_unit;
	}

	for (i = 0; i < MAX_MAPS; i++){
		if ( ! g_maps[i].enabled ){
			continue;
		}
		found = 0;
		for(j = 0; j < MAX_MAPS; j++){
			if (g_maps[i].intPort == g_unmaps[j].intPort && \
				g_maps[i].extPort == g_unmaps[j].extPort && \
				g_maps[i].isTcp   == g_unmaps[j].isTcp){
				found = 1;
				break;
			}
		}

		if ( ! found ){
			for(j = 0; j < MAX_MAPS; j++){
				if ( ! g_unmaps[j].enabled && ! g_unmaps[j].extPort ){
					pthread_mutex_lock(&g_mutex);
					memcpy(&g_unmaps[j], &g_maps[i], sizeof(ml_upnpmp_t));
					g_unmaps[j].enabled = 0;
					g_unmaps[j].overTime = 0;
					memset(&g_maps[i], 0, sizeof(ml_upnpmp_t));
					pthread_mutex_unlock(&g_mutex);
					break;
				}
			}
		}
	}
	pthread_mutex_unlock( &g_delay_mutex );

	for (i=0; i<wait; i++){
		sleep(1);
		found = 0;
		for(j = 0; j < MAX_MAPS; j++){
			if (g_unmaps[j].extPort){
				found = 1;
				break;
			}
		}
		if ( ! found ){
			break;
		}
	}
	return Val_unit;
}

/** @brief caml api, reutrn all maps in g_maps to a list[tuple(7 items)] */
value
ml_upnpGetMaps(value unused)
{
	CAMLparam0 ();
	int i;

	CAMLlocal3( maps, map, cons );
	maps = Val_emptylist;

	if ( ! g_inited ){
		dbg_printf("g_maps not initialize!\n");
		CAMLreturn( Val_unit );
	}

	for (i = MAX_MAPS - 1; i > 0; i--){
		if ( ! g_maps[i].enabled ){
			continue;
		}

		map = caml_alloc_tuple( 7 );
		Store_field( map, 0, Val_int(g_maps[i].enabled) );
		Store_field( map, 1, Val_int(g_maps[i].intPort) );
		Store_field( map, 2, Val_int(g_maps[i].extPort) );
		Store_field( map, 3, Val_int(g_maps[i].isTcp) );
		Store_field( map, 4, Val_int(g_maps[i].natpmpStatus) );
		Store_field( map, 5, Val_int(g_maps[i].upnpStatus) );
		Store_field( map, 6, caml_copy_string(g_maps[i].notes) );

		cons = caml_alloc( 2, 0 );
		Store_field( cons, 0, map ); // head
		Store_field( cons, 1, maps ); // tail
		maps = cons;
	}
	CAMLreturn( maps );
}


#else /* for ifdef ENABLE_UPNP_NATPMP */
/* no include upnp natpmp, dummy api here */
value
ml_upnpInitMaps(value unused)
{
	return Val_unit;
}

/** @brief ocaml api, start the upnp thread */
value
ml_upnp_job_start( value unused )
{
	return Val_unit;
}

/** @brief ocaml api, stop the upnp thread */
value
ml_upnp_job_stop( value wait_v )
{
	return Val_unit;
}

value
ml_upnpDumpMaps(value unused)
{
	return Val_unit;
}

value
ml_upnpAddMap(value m_enabled, value m_intPort, value m_extPort, value m_type, value m_notes)
{
	return Val_unit;
}

value
ml_upnpRemoveMap(value m_enabled, value m_intPort, value m_extPort, value m_type, value m_notes)
{
	return Val_unit;
}

value
ml_upnpRemoveAllMaps(value wait_v)
{
	return Val_unit;
}

value
ml_upnpGetMaps(value unused)
{
	return Val_emptylist;
}

#endif
