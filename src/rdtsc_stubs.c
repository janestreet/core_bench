#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <caml/mlvalues.h>

#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

#include <time.h>
#include <sys/time.h>
#include <stdint.h>

#ifdef __MACH__
#include <mach/clock.h>
#include <mach/mach.h>
#endif

#define NANOS_PER_SECOND 1000000000

/* http://en.wikipedia.org/wiki/Time_Stamp_Counter */
CAMLprim value bench_rdtsc( )
{
  uint32_t hi, lo;
  __asm__ __volatile__ ("rdtsc" : "=a"(lo), "=d"(hi));
  return Val_long( ((uint64_t)lo) | (((uint64_t)hi)<<32) );
}

