

#include "../../utils/lib/os_stubs.h"



/*******************************************************************


                         os_read


*******************************************************************/

#define UNIX_BUFFER_SIZE 16384

extern void enter_blocking_section(); 
extern void leave_blocking_section();

extern ssize_t os_read(OS_FD fd, char *buf, size_t len)
{
  DWORD numread;
  BOOL ret;

  if (len > UNIX_BUFFER_SIZE) len = UNIX_BUFFER_SIZE;

  enter_blocking_section();
  ret = ReadFile(fd, buf, len, &numread, NULL);
  leave_blocking_section();
  if (! ret) {
    win32_maperr(GetLastError());
    uerror("os_read", Nothing);
  }
  return numread;
}

/*******************************************************************


                         os_ftruncate


*******************************************************************/

#include <winioctl.h>

void os_ftruncate(OS_FD fd, OFF_T size, /* bool */ int sparse)
{
  DWORD curpos;
  long ofs_low = (long) size;
  long ofs_high = (long) (size >> 32);

  if (sparse) {
	DWORD dw;
	BOOL bRet = DeviceIoControl(fd, FSCTL_SET_SPARSE, NULL, 0, NULL, 0, &dw, NULL);
	if (!bRet) {
		// No sparse files for you, sucker...
		// DWORD err = GetLastError();
	}
  }
  curpos = SetFilePointer (fd, 0, NULL, FILE_CURRENT);
  if (curpos == 0xFFFFFFFF
      || SetFilePointer (fd, ofs_low, &ofs_high, FILE_BEGIN) == 0xFFFFFFFF
      || !SetEndOfFile (fd))
    {
      long err = GetLastError();
      if (err != NO_ERROR) {
	win32_maperr(err);
	uerror("os_ftruncate", Nothing);
      }
    }
}

/*******************************************************************


                         os_getdtablesize


*******************************************************************/

int os_getdtablesize()
{
  return 32767;
}

/*******************************************************************


                         os_getfdsize
                         

*******************************************************************/

int64 os_getfdsize(OS_FD fd)
{
  long len_high;
  int64 ret;

  ret = GetFileSize(fd, &len_high);
  return ((int64) len_high << 32 | ret);
}

/*******************************************************************


                         os_getfilesize


*******************************************************************/

int64 os_getfilesize(char *path)
{
  OS_FD fd = CreateFile(path, GENERIC_READ, FILE_SHARE_READ,
			NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL,
			NULL);
  long len_high;
  long ret;
  if (fd != INVALID_HANDLE_VALUE){
    ret = GetFileSize(fd, &len_high);
    CloseHandle(fd);
    return  ((int64) len_high << 32 | ret);
  } else {
    long err = GetLastError();
    if (err != NO_ERROR) {
	win32_maperr(err);
	uerror("os_getfilesize", Nothing);
    }
    return 0;
  }
}

/*******************************************************************


                         os_lseek


*******************************************************************/

OFF_T os_lseek(OS_FD fd, OFF_T ofs, int cmd)
{
  DWORD ret;
  long ofs_low = ofs;
  long ofs_high = (long) (ofs >> 32);
  long err;

  ret = SetFilePointer(fd, ofs_low, &ofs_high, cmd);
  if ((unsigned long)ret == INVALID_SET_FILE_POINTER) {
    err = GetLastError();
    if (err != NO_ERROR) {
      win32_maperr(err);
      uerror("os_lseek", Nothing);
    }
  }
  return ((OFF_T) ofs_high << 32 | ret);
}

/*******************************************************************


                         os_set_nonblock


*******************************************************************/

#include <winsock2.h>

void os_set_nonblock(OS_SOCKET fd)
{
  u_long optval = 1;

  if( ioctlsocket(fd, FIONBIO, &optval) != 0){
    long err = GetLastError();
    if (err != NO_ERROR) {
      win32_maperr(err);
      uerror("os_set_nonblock", Nothing);
    }
  }
}

/*******************************************************************


                         os_uname


*******************************************************************/

// http://msdn.microsoft.com/library/default.asp?url=/library/en-us/sysinfo/base/getting_the_system_version.asp
#define BUFSIZE 80
#define SM_SERVERR2 89
typedef void (WINAPI *PGNSI)(LPSYSTEM_INFO);


void os_uname(char buf[])
{
   OSVERSIONINFOEX osvi;
   BOOL bOsVersionInfoEx;
   SYSTEM_INFO si;
   PGNSI pGNSI;
   char tbuf[4096];
   char pbuf[32];
   char binull='\0';

   // Try calling GetVersionEx using the OSVERSIONINFOEX structure.
   // If that fails, try using the OSVERSIONINFO structure.

   ZeroMemory(&osvi, sizeof(OSVERSIONINFOEX));
   osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEX);

   if( !(bOsVersionInfoEx = GetVersionEx ((OSVERSIONINFO *) &osvi)) )
   {
      osvi.dwOSVersionInfoSize = sizeof (OSVERSIONINFO);
      if (! GetVersionEx ( (OSVERSIONINFO *) &osvi) ) 
         return;
   }

   switch (osvi.dwPlatformId)
   {
      // Test for the Windows NT product family.
      case VER_PLATFORM_WIN32_NT:

      // Test for the specific product.
      if ( osvi.dwMajorVersion == 6 && osvi.dwMinorVersion == 0 )
      {
	     if( osvi.wProductType == VER_NT_WORKSTATION )
              strcat (buf, "Microsoft Windows Vista \0");
         else strcat (buf, "Windows Server \"Longhorn\" \0" );
      }    

      if ( osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 2 )
      {
         // Use GetProcAddress to avoid load issues on Windows 2000
         pGNSI = (PGNSI) GetProcAddress (GetModuleHandle("kernel32.dll"), "GetNativeSystemInfo");
         if(NULL != pGNSI)
             pGNSI(&si);

         if( GetSystemMetrics(SM_SERVERR2) )
            strcat (buf, "Microsoft Windows Server 2003 \"R2\" \0");
         else if( osvi.wProductType == VER_NT_WORKSTATION && si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_AMD64)
         {
            strcat (buf, "Microsoft Windows XP Professional x64 Edition \0");
         }
         else strcat (buf, "Microsoft Windows Server 2003, \0");
      }

      if ( osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 1 )
         strcat(buf, "Microsoft Windows XP \0");

      if ( osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 0 )
         strcat(buf, "Microsoft Windows 2000 \0");

      if ( osvi.dwMajorVersion <= 4 )
         strcat(buf, "Microsoft Windows NT \0");

      // Test for specific product on Windows NT 4.0 SP6 and later.
      if( bOsVersionInfoEx )
      {
         // Test for the workstation type.
         if ( osvi.wProductType == VER_NT_WORKSTATION  &&  si.wProcessorArchitecture != PROCESSOR_ARCHITECTURE_AMD64 ) 
         {
            if( osvi.dwMajorVersion == 4 )
               strcat(buf, "Workstation 4.0 \0" );
            else if( osvi.wSuiteMask & VER_SUITE_PERSONAL )
               strcat(buf, "Home Edition \0" );
            else strcat(buf, "Professional \0" );
         }
            
         // Test for the server type.
         else if ( osvi.wProductType == VER_NT_SERVER || 
                   osvi.wProductType == VER_NT_DOMAIN_CONTROLLER )
         {
            if(osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 2)
            {
               if ( si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_IA64 )
               {
                   if( osvi.wSuiteMask & VER_SUITE_DATACENTER )
                      strcat (buf, "Datacenter Edition for Itanium-based Systems \0" );
                   else if( osvi.wSuiteMask & VER_SUITE_ENTERPRISE )
                      strcat (buf, "Enterprise Edition for Itanium-based Systems \0" );
               }

               else if ( si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_AMD64 )
               {
                   if( osvi.wSuiteMask & VER_SUITE_DATACENTER )
                      strcat (buf, "Datacenter x64 Edition \0" );
                   else if( osvi.wSuiteMask & VER_SUITE_ENTERPRISE )
                      strcat (buf, "Enterprise x64 Edition \0" );
                   else strcat (buf, "Standard x64 Edition \0" );
               }

               else
               {
                   if( osvi.wSuiteMask & VER_SUITE_DATACENTER )
                      strcat (buf, "Datacenter Edition \0" );
                   else if( osvi.wSuiteMask & VER_SUITE_ENTERPRISE )
                      strcat (buf, "Enterprise Edition \0" );
                   else if ( osvi.wSuiteMask == VER_SUITE_BLADE )
                      strcat (buf, "Web Edition \0" );
                   else strcat (buf, "Standard Edition \0" );
               }
            }            

            else if(osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 0)
            {
               if( osvi.wSuiteMask & VER_SUITE_DATACENTER )
                  strcat(buf, "Datacenter Server \0" );
               else if( osvi.wSuiteMask & VER_SUITE_ENTERPRISE )
                  strcat(buf, "Advanced Server \0" );
               else strcat(buf, "Server \0" );
            }
            else  // Windows NT 4.0 
            {
               if( osvi.wSuiteMask & VER_SUITE_ENTERPRISE )
                  strcat(buf, "Server 4.0, Enterprise Edition \0" );
               else strcat(buf, "Server 4.0 \0" );
            }
         }
      }
      // Test for specific product on Windows NT 4.0 SP5 and earlier
      else  
      {
         HKEY hKey;
         char szProductType[BUFSIZE];
         DWORD dwBufLen=BUFSIZE;
         LONG lRet;

         lRet = RegOpenKeyEx( HKEY_LOCAL_MACHINE,
            "SYSTEM\\CurrentControlSet\\Control\\ProductOptions",
            0, KEY_QUERY_VALUE, &hKey );
         if( lRet != ERROR_SUCCESS )
            return;

         lRet = RegQueryValueEx( hKey, "ProductType", NULL, NULL,
            (LPBYTE) szProductType, &dwBufLen);
         if( (lRet != ERROR_SUCCESS) || (dwBufLen > BUFSIZE) )
            return;

         RegCloseKey( hKey );

         if ( lstrcmpi( "WINNT", szProductType) == 0 )
            strcat(buf, "Workstation \0" );
         if ( lstrcmpi( "LANMANNT", szProductType) == 0 )
            strcat(buf, "Server \0" );
         if ( lstrcmpi( "SERVERNT", szProductType) == 0 )
            strcat(buf, "Advanced Server \0" );
         sprintf(tbuf, "%d.%d %c", (int)osvi.dwMajorVersion, (int)osvi.dwMinorVersion, binull );
      }

      // Display service pack (if any), build number and processor count (if it`s > 1).

   	  GetSystemInfo(&si);
	  
      if( si.dwNumberOfProcessors > 1)
         
          sprintf(pbuf, "(CPUs: %lu)%c", si.dwNumberOfProcessors, binull);
      else
          pbuf[0] = binull;
                 

      if( osvi.dwMajorVersion == 4 && lstrcmpi( osvi.szCSDVersion, "Service Pack 6" ) == 0 )
      { 
         HKEY hKey;
         LONG lRet;

         // Test for SP6 versus SP6a.
         lRet = RegOpenKeyEx( HKEY_LOCAL_MACHINE, 
                "SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Hotfix\\Q246009",
            0, KEY_QUERY_VALUE, &hKey );
         if( lRet == ERROR_SUCCESS )
								 
            sprintf(tbuf, "Service Pack 6a (Build %lu) %s", osvi.dwBuildNumber & 0xFFFF, pbuf );
         else // Windows NT 4.0 prior to SP6a
         {
            sprintf(tbuf, "%s (Build %lu) %s",osvi.szCSDVersion, osvi.dwBuildNumber & 0xFFFF, pbuf);
         }
         RegCloseKey( hKey );
      }
      else // not Windows NT 4.0 
      {
         sprintf(tbuf, "%s (Build %lu) %s", osvi.szCSDVersion, osvi.dwBuildNumber & 0xFFFF, pbuf);
      }

      strcat(buf, tbuf);
      break;

      // Test for the Windows Me/98/95.
      case VER_PLATFORM_WIN32_WINDOWS:

      if (osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 0)
      {
          strcat (buf, "Microsoft Windows 95 ");
          if (osvi.szCSDVersion[1] == 'C' || osvi.szCSDVersion[1] == 'B')
             strcat(buf, "OSR2 \0" );
      } 

      if (osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 10)
      {
          strcat(buf, "Microsoft Windows 98 ");
          if ( osvi.szCSDVersion[1] == 'A' )
             strcat(buf, "SE \0" );
      } 

      if (osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 90)
      {
          strcat(buf, "Microsoft Windows Millennium Edition\0");
      } 
      break;

      case VER_PLATFORM_WIN32s:

      strcat(buf, "Microsoft Win32s\0");
      break;
   }
   return; 
}

/*******************************************************************


                          os_os_supported


*******************************************************************/

int os_os_supported()
{
   OSVERSIONINFO pVersion;
   pVersion.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
   int check = GetVersionEx(&pVersion);

   if ( check != 0 )
   {
   if ( pVersion.dwPlatformId == VER_PLATFORM_WIN32_NT )
       return 1;
     else
       return 0;
   }
   else  /* expect an NT system because GetVersionEx failed */
   {
     return 1;
   }
}

/*******************************************************************


                      os_set_console_title


 *******************************************************************/

value os_set_console_title(value buf)
{
   char *pbuf = String_val (buf);

   SetConsoleTitle((LPCTSTR)pbuf);
   return Val_unit;
}
