
#ifndef __ORDMA_DEBUG_H
#define __ORDMA_DEBUG_H

#include <stdio.h>

//#define ORDMA_DEBUG 1

#if ORDMA_DEBUG
#define ORDMA_LOG(...) fprintf(stderr, __VA_ARGS__); fputc('\n', stderr); fflush(stderr);
#else
#define ORDMA_LOG(...)
#endif

#endif /* __ORDMA_DEBUG_H */
