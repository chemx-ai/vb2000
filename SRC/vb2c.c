/*
 A Small piece of C code for dynamic memory allocation.
 First created: Oct. 24, 2004
 Author: Jiabo Li
 Modified: June 2005, to use Gamess(US) code. Brian Duke
 */

/*

   These routines are taken, with permission from Mike Schmidt, from the 
   Gamess(US) code. They should assist with porting VB2000 to other systems.
   They have only been tested for -DLINUX and -DSGI64.

   Interface from FORTRAN/GAMESS to C/system calls
   ===============================================
 
          The first three routines amount to a FORTRAN wrapper around
          the C calls malloc and free, by Steve Elbert in January 1990.
   memory allocation: function invocation only: LOCMEM = MEMGET(NWORDS)
          NWORDS is the no. of 8-byte words of memory requested.
          The return value is the address of the allocated memory, if
          this is zero, the system doesn't have the memory available.
   memory release: subroutine call only: CALL MEMREL(LOCMEM)
          Deallocate all memory above the address LOCMEM.
          (deallocated memory is not returned to system)
   memory address: function invocation only: LOCX = LADDRS(X)
          Returns the absolute address of X, in bytes.  This is needed
          only if the FORTRAN library does not include the LOC function.

Changes from Gamess zunix.c:-

In Linux return type specified to afford warning with latest compilers.

*/

/*--------------- Compaq (aka Digital) --------------*/

#ifdef COMPAQ
#include <stdlib.h>

long memget_(nwords) long *nwords;
   { size_t nbytes;
     nbytes = (size_t) (*nwords+2)*8;
     return (long) malloc(nbytes); }

#endif
 
/*--------------------------- Cray --------------------------*/
/*  --  Cray PVP or T3E --   */

#ifdef CRAY

int MEMGET(nwords) int *nwords;
   { int nbytes;
     nbytes = (*nwords+2)*8;
     return malloc(nbytes); }

void MEMREL(locmem) int *locmem;
   { free(*locmem); }

#endif

/*  --  Cray X1 --   */

#ifdef CRAYX1

#include <unistd.h>

long memget_(nwords) long *nwords;
   { size_t nbytes;
     nbytes = (size_t) (*nwords+2)*8;
     return (long) malloc(nbytes); }

void memrel_(locmem) long *locmem;
   { free(*locmem); }

#endif

/*  --  Cray XD1 --   */

#ifdef CRAYXD1
   
#include <stdlib.h>
#include "/usr/local/include/gpshmem-1.0/gpshmem.h"
#define FORTINT long

FORTINT memget_(nwords) FORTINT *nwords;
   { size_t nbytes;
     nbytes = (*nwords+2)*8;
     return (FORTINT) gpshmalloc(nbytes); }

void memrel_(locmem) FORTINT *locmem;
   { gpshfree((void*)*locmem); }

#endif

/*-------------- Fujitsu VPP and AP ---------------*/

#ifdef FUJITSU

int memget_(nwords) int *nwords;
   { int nbytes;
     nbytes = (*nwords+2)*8;
     return malloc(nbytes); }

void memrel_(locmem) int *locmem;
   { free(*locmem); }

int laddrs_(arg) int arg;
   { return(arg); }

#endif

/*--------------------- Hewlett-Packard --------------------------*/
/*    this version uses FORTRAN callable library routines only    */
/*                 no need to compile this file                   */



/*------------ International Business Machines ------------*/
/*  code is for 32 bit Power, Power2, and PowerPC systems, */
/*       or for 64 bit Power3 and Power4 systems.          */
/*  The term "system" can mean either workstations or SP.  */
 
#ifdef IBM32

int memget(nwords) int *nwords;
   { int nbytes;
     nbytes = (*nwords+2)*8;
     return malloc(nbytes); }

void memrel(locmem) int *locmem;
   { free(*locmem); }

int laddrs(arg) int arg;
   { return(arg); }

#endif

#ifdef IBM64

#include <stdlib.h>
long memget(long *nwords)
  { size_t nbytes;
    nbytes = (*nwords + 2L) * 8L;
    return (long) malloc (nbytes); }

void memrel(long **locmem)
  { free (*locmem); }

long laddrs(void *arg)
  { return (long) arg; }

#endif

/*----- 32 bit PC (e.g. Pentium, Athlon, ...) running Linux -------*\
\*------- this also includes the Apple G4 running MAC OS X --------*/

#ifdef LINUX

int memget_(nwords) int *nwords;
   { int nbytes;
     nbytes = (*nwords+2)*8;
     return (int) malloc(nbytes); }

void memrel_(locmem) int *locmem;
   { free((void*)*locmem); }

int laddrs_(arg) int arg;
   { return (int) (arg); }

#endif

/*--- Absoft compiler...one might find this in Linux or MAC OS X worlds ---*/

#ifdef ABSOFT

int MEMGET(nwords) int *nwords;
   { int nbytes;
     nbytes = (*nwords+2)*8;
     return malloc(nbytes); }

void MEMREL(locmem) int *locmem;
   { free(*locmem); }

int LADDRS(arg) int arg;
   { return(arg); }

#endif

/*----------------- Itanium (IA64) running Linux -----------------*/

#ifdef LINUXIA64

#include <stdlib.h>
#include <malloc.h>
#define FORTINT long

FORTINT memget_(nwords) FORTINT *nwords;
   { size_t nbytes;
     nbytes = (*nwords+2)*8;
     return (FORTINT) malloc(nbytes); }

void memrel_(locmem) FORTINT *locmem;
   { free((void*)*locmem); }

#endif

#ifdef LINUX64

#include <stdlib.h>
#include <malloc.h>

#define FORTINT long

FORTINT memget_(nwords) FORTINT *nwords;
   { size_t nbytes;
     nbytes = (*nwords+2)*8;
     return (FORTINT) malloc(nbytes); }

void memrel_(locmem) FORTINT *locmem;
   { free((void*)*locmem); }

#endif
/*----------------- NEC SX series -----------------*/

#ifdef NECSX
#include <unistd.h>

void memget_(nwords,sxaddr)
 long long *nwords, **sxaddr ;
   {char *malloc(),*card;
    unsigned nbytes;
    nbytes = (unsigned) ((*nwords+2)*8);
    card =  malloc (nbytes);
    *sxaddr = (long long *) card; }

void memrel_(locmem)
 long long *locmem;
 {void free();
  char* ptrmem;
  ptrmem = (char *)locmem;
  free(ptrmem); }

void laddrs_(arg,addrarg)
long long *arg, **addrarg;
   { *addrarg = arg ; }

#endif

/*-------------- Silicon Graphics, Incorporated --------------*/

#ifdef SGI32
#define FORTINT int
#endif
#ifdef SGI64
#define FORTINT long
#endif

#if (defined SGI32) || (defined SGI64)

int memget_(nwords) FORTINT *nwords;
   { FORTINT nbytes;
     nbytes = (*nwords+2)*8;
     return malloc(nbytes); }

void memrel_(locmem) FORTINT *locmem;
   { free(*locmem); }

#endif

/*----------------- SUN -----------------*/

#ifdef SUN32
#define FORTINT int
#endif
#ifdef SUN64
#define FORTINT long
#endif

#if (defined SUN32) || (defined SUN64)

FORTINT memget_(nwords) FORTINT *nwords;
   { FORTINT nbytes; FORTINT malloc();
     nbytes = (*nwords+2)*8;
     return malloc(nbytes); }

void memrel_(locmem) FORTINT *locmem;
   { free(*locmem); }

#endif
