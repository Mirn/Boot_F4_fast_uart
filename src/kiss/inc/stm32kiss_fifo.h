#ifndef __STM32KISS_FIFO_H__
#define __STM32KISS_FIFO_H__

#include "stdint.h"

typedef struct
{
	uint8_t *data;
	uint32_t size;

	uint32_t wr;
	uint32_t rd;

	uint32_t stat_underfulls;
	uint32_t stat_overfulls;
	uint32_t stat_min;
	uint32_t stat_max;
} tFIFO;

void _fifo_init(tFIFO *fifo, void *_data, uint32_t _size);

int32_t fifo_count(tFIFO *fifo);
int32_t fifo_free(tFIFO *fifo);

void fifo_reset_all(tFIFO *fifo);
void fifo_reset_stats(tFIFO *fifo);

uint32_t fifo_read(tFIFO *fifo, uint8_t *buf, uint32_t size);
uint32_t fifo_write(tFIFO *fifo, uint8_t *buf, uint32_t size);

/*#define fifo_init(fifo_name) _fifo_init(fifo_name, fifo_name##_data, sizeof(fifo_name##_data))

#define fifo_declare(fifo_name, size) \
	uint8_t   fifo_name##_data[size] __attribute__((__aligned__(4)))   = {[0 ... (size-1)] = 0}; \
	tFIFO __##fifo_name##_stuct; \
	tFIFO    *fifo_name = &__##fifo_name##_stuct;*/

#define fifo_init(fifo_name, size) \
	static uint8_t fifo_name##_data[size] __attribute__((__aligned__(4)))   = {[0 ... (size-1)] = 0}; \
	static tFIFO __##fifo_name##_stuct; \
	               fifo_name = &(__##fifo_name##_stuct); \
	    _fifo_init(fifo_name, fifo_name##_data, size)

#endif //#ifndef __STM32KISS_FIFO_H__
