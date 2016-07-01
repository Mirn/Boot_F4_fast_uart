#include "stm32kiss.h"
#include "stm32kiss_fifo.h"

void _fifo_init(tFIFO *fifo, void *_data, uint32_t _size)
{
	STRUCT_CLEAR(*fifo);
	fifo->data = _data;
	fifo->size = _size;
}

inline int32_t _fifo_count(tFIFO *fifo)
{
	int32_t cnt = fifo->wr - fifo->rd;
	return cnt;
}

int32_t fifo_count(tFIFO *fifo)
{
	int32_t cnt = _fifo_count(fifo);

	if (cnt > fifo->size) return fifo->size;
	if (cnt < 0) return 0;

	return cnt;
}

int32_t fifo_free(tFIFO *fifo)
{
	return fifo->size - fifo_count(fifo);
}

void fifo_reset_all(tFIFO *fifo)
{
	 void    *data = fifo->data;
	 uint32_t size = fifo->size;

	 _fifo_init(fifo, data, size);
}

void fifo_reset_stats(tFIFO *fifo)
{
	fifo->stat_min = fifo->size;
	fifo->stat_max = 0;

	fifo->stat_underfulls = 0;
	fifo->stat_overfulls  = 0;
}

void fifo_check_read(tFIFO *fifo)
{
	int32_t cnt = fifo_count(fifo);

	if (fifo->stat_min > cnt) fifo->stat_min = cnt;
	if (fifo->stat_max < cnt) fifo->stat_max = cnt;

	cnt = _fifo_count(fifo);

	if (cnt < 0)
	{
		fifo->stat_underfulls++;
		fifo->rd = fifo->wr;
	}

	if (cnt > fifo->size)
	{
		fifo->stat_overfulls++;
		fifo->rd = fifo->wr - fifo->size;
	}
}

void fifo_check_write(tFIFO *fifo)
{
	int32_t cnt = fifo_count(fifo);

	if (fifo->stat_min > cnt) fifo->stat_min = cnt;
	if (fifo->stat_max < cnt) fifo->stat_max = cnt;

	if (cnt < 0)
	{
		fifo->stat_underfulls++;
		fifo->wr = fifo->rd;
	}

	if (cnt > fifo->size)
	{
		fifo->stat_overfulls++;
		fifo->wr = fifo->rd + fifo->size;
	}
}

uint32_t fifo_read(tFIFO *fifo, uint8_t *buf, uint32_t size)
{
	if (size == 0) return 0;

	fifo_check_read(fifo);
	uint32_t pos = fifo->rd % fifo->size;
	uint32_t cnt = fifo_count(fifo);

	if (size > cnt) size = cnt;
	if (size == 0) return 0;

	if ((pos + size) > fifo->size)
	{
		uint32_t part  = fifo->size - pos;

		memcpy(buf,      fifo->data + pos, part);
		memcpy(buf+part, fifo->data,       size-part);
	}
	else
		memcpy(buf, fifo->data + pos, size);

	fifo->rd += size;

	return size;
}

uint32_t fifo_write(tFIFO *fifo, uint8_t *buf, uint32_t size)
{
	if (size == 0) return 0;

	fifo_check_write(fifo);
	uint32_t pos  = fifo->wr % fifo->size;
	uint32_t free = fifo_free(fifo);

	if (size > free) size = free;
	if (size == 0) return 0;

	if ((pos + size) > fifo->size)
	{
		uint32_t part = fifo->size - pos;

		memcpy(fifo->data + pos, buf,      part);
		memcpy(fifo->data,       buf+part, size-part);
	}
	else
		memcpy(fifo->data + pos, buf,      size);

	fifo->wr += size;

	return size;
}


/*#include <string.h>
#include <stdbool.h>
#include <stdint.h>

#include "stm32f10x.h"
#include "core_cm3.h"
#include "stm32kiss_fifo.h"

void fifo_init(tFIFO *fifo, void *_data, uint16_t _size, uint16_t _overfull_reserv, uint8_t _default_value, uint16_t _io_size, void *_io_write, void *_io_read)
{
	fifo->data   = _data;
	fifo->wr = 0;
	fifo->rd = 0;
	fifo->io = 0;
	fifo->size            = _size;
	fifo->overfull_reserv = _overfull_reserv;
	fifo->default_value   = _default_value;

	fifo->io_size      = _io_size;
	fifo->io_buf_write = _io_read;
	fifo->io_buf_read  = _io_write;

	fifo->stat_wr_underfulls = 0;
	fifo->stat_wr_overfulls  = 0;
	fifo->stat_wr_min = fifo->size;
	fifo->stat_wr_max = 0;

	fifo->stat_rd_underfulls = 0;
	fifo->stat_rd_overfulls  = 0;
	fifo->stat_rd_min = fifo->size;
	fifo->stat_rd_max = 0;

	fifo->first_write = 0x10;
}

__inline__ static void fifo_write_check(tFIFO *fifo)
{
	if (fifo->stat_wr_min > (fifo->wr-fifo->io))
		fifo->stat_wr_min =  fifo->wr-fifo->io;
	if (fifo->stat_wr_max < (fifo->wr-fifo->io))
		fifo->stat_wr_max =  fifo->wr-fifo->io;

	if (fifo->io > fifo->wr)
	{
		if ((fifo->wr!=0) &&(fifo->first_write == 0))
			fifo->stat_wr_underfulls++;

		fifo->wr  = fifo->io;
		fifo->wr += fifo->overfull_reserv;
		if (fifo->io_size!=0)
			fifo->wr += fifo->io_size - (fifo->wr % fifo->io_size);
	};

}

__inline__ static void fifo_read_check(tFIFO *fifo)
{
	uint32_t count = fifo->io - fifo->rd;

	if (fifo->stat_rd_max < count)
		fifo->stat_rd_max = count;
	if (fifo->stat_rd_min > count)
		fifo->stat_rd_min = count;

	if (fifo->io < fifo->rd)
	{
		fifo->stat_rd_underfulls++;
		fifo->rd  = fifo->io + fifo->overfull_reserv - fifo->size;
		if (fifo->io_size!=0)
			fifo->rd += fifo->io_size - (fifo->rd % fifo->io_size);
	}
	else
	if (count > fifo->size)
	{
		fifo->stat_rd_overfulls++;
		fifo->rd  = fifo->io + fifo->overfull_reserv - fifo->size;
		if (fifo->io_size!=0)
			fifo->rd += fifo->io_size - (fifo->rd % fifo->io_size);
	};
}

__inline__ static uint32_t __fifo_read_size(tFIFO *fifo)
{
	if ((fifo->io - fifo->rd) > fifo->size)
	{
		if (fifo->rd > fifo->io)
			return 0;
		else
			return fifo->size;
	}

	return fifo->io - fifo->rd;
}

__inline__ static uint32_t __fifo_write_size(tFIFO *fifo)
{
	if (fifo->io >= fifo->wr)
		if (fifo->io < (0xFFFFFFFF - fifo->size))
			return 0;

	return fifo->wr - fifo->io;
}

__inline__ static uint32_t __fifo_read_free(tFIFO *fifo)
{
	uint32_t read_size = __fifo_read_size(fifo);

	if (read_size>fifo->size)
		return 0;
	return fifo->size - read_size;
}

uint32_t fifo_read_size(tFIFO *fifo)
{
	__disable_irq();
	uint32_t value = __fifo_read_size(fifo);
	__enable_irq();

	return value;
}

uint32_t fifo_write_size(tFIFO *fifo)
{
	__disable_irq();
	uint32_t value = __fifo_write_size(fifo);
	__enable_irq();

	return value;
}

uint32_t fifo_read_free(tFIFO *fifo)
{
	__disable_irq();
	uint32_t value = __fifo_read_free(fifo);
	__enable_irq();

	return value;
}


uint32_t fifo_write_free(tFIFO *fifo)
{
	__disable_irq();
	uint32_t read_free  = __fifo_read_free(fifo);
	uint32_t write_size = __fifo_write_size(fifo);
	__enable_irq();

	if (write_size>read_free)
		return 0;
	else
		return read_free - write_size;
}

uint32_t fifo_write_check_time(tFIFO *fifo)
{
	fifo_write_check(fifo);
	return fifo->wr;
}

void fifo_read(tFIFO *fifo, uint8_t *buf, uint32_t size)
{
	fifo_read_check(fifo);

	uint32_t fifo_pos = fifo->rd % fifo->size;
	if ((fifo_pos + size) > fifo->size)
	{
		uint32_t part  = fifo->size - fifo_pos;

		memcpy(buf,      fifo->data + fifo_pos, part);
		memcpy(buf+part, fifo->data,            size-part);
	}
	else
		memcpy(buf, fifo->data + fifo_pos, size);

	fifo->rd+=size;
	fifo_read_check(fifo);
}

void fifo_write(tFIFO *fifo, uint8_t *buf, uint32_t size)
{
	fifo_write_check(fifo);
	if (fifo_write_free(fifo) < (size*2))
	{
		fifo->stat_wr_overfulls++;
		return;
	}

	uint32_t fifo_pos = fifo->wr % fifo->size;
	if ((fifo_pos+size) > fifo->size)
	{
		uint32_t part = fifo->size - fifo_pos;

		memcpy(fifo->data + fifo_pos, buf,      part);
		memcpy(fifo->data,            buf+part, size-part);
	}
	else
		memcpy(fifo->data + fifo_pos, buf,      size);

	fifo->wr+=size;

	if (fifo->first_write>0)
		fifo->first_write--;

	fifo_write_check(fifo);
}

void fifo_io_exchange(tFIFO *fifo)
{
	uint32_t fifo_pos = fifo->io % fifo->size;

	if ((fifo->wr >= (fifo->io + fifo->io_size)))
		if ((fifo_pos + fifo->io_size) > fifo->size)
		{
			uint32_t part = fifo->size - fifo_pos;

			memcpy(fifo->io_buf_read,        fifo->data + fifo_pos,     part);
			memcpy(fifo->io_buf_read + part, fifo->data,                fifo->io_size - part);
			memcpy(fifo->data+fifo_pos,      fifo->io_buf_write,        part);
			memcpy(fifo->data,               fifo->io_buf_write + part, fifo->io_size - part);
		}
		else
		{
			memcpy(fifo->io_buf_read,     fifo->data + fifo_pos, fifo->io_size);
			memcpy(fifo->data + fifo_pos, fifo->io_buf_write,    fifo->io_size);
		}
	else
	{
		if ((fifo_pos + fifo->io_size) > fifo->size)
		{
			uint32_t part = fifo->size - fifo_pos;

			memcpy(fifo->data + fifo_pos, fifo->io_buf_write,        part);
			memcpy(fifo->data,            fifo->io_buf_write + part, fifo->io_size - part);
		}
		else
			memcpy(fifo->data + fifo_pos, fifo -> io_buf_write,  fifo->io_size);

		memset(fifo->io_buf_read, fifo->default_value, fifo->io_size);
	};

	fifo->io+=fifo->io_size;
}

bool fifo_io_get(tFIFO *fifo, uint8_t *buf_read, uint32_t size)
{
	uint32_t fifo_pos = fifo->io % fifo->size;

	if ((fifo->wr >= (fifo->io + size)))
	{
		if ((fifo_pos + size) > fifo->size)
		{
			uint32_t part = fifo->size - fifo_pos;

			memcpy(buf_read,        fifo->data + fifo_pos,     part);
			memcpy(buf_read + part, fifo->data,                size - part);
		}
		else
			memcpy(buf_read,     fifo->data + fifo_pos, size);
	}
	else
		return false;

	return true;
}

void fifo_io_put(tFIFO *fifo, uint8_t *buf_write, uint32_t size)
{
	uint32_t fifo_pos = fifo->io % fifo->size;

	if ((fifo_pos + size) > fifo->size)
	{
		uint32_t part=fifo->size-fifo_pos;

		memcpy(fifo->data + fifo_pos, buf_write,        part);
		memcpy(fifo->data,            buf_write + part, size-part);
	}
	else
		memcpy(fifo->data + fifo_pos, buf_write,  size);

	fifo->io+=size;
}

void fifo_errors_reset(tFIFO *fifo)
{
	fifo->stat_wr_underfulls = 0;
	fifo->stat_wr_overfulls  = 0;

	fifo->stat_rd_underfulls = 0;
	fifo->stat_rd_overfulls  = 0;
}

void fifo_stat_reset(tFIFO *fifo)
{
	fifo->stat_wr_min = fifo->size;
	fifo->stat_wr_max = 0;

	fifo->stat_rd_min = fifo->size;
	fifo->stat_rd_max = 0;
}

void fifo_reset(tFIFO *fifo)
{
	__disable_irq();

	fifo->wr=0;
	fifo->rd=0;
	fifo->io=0;

	fifo_stat_reset(fifo);

	__enable_irq();
}

void fifo_reset_write(tFIFO *fifo)
{
	__disable_irq();

	fifo->first_write = 0x10;
	fifo->stat_wr_min = fifo->size;
	fifo->stat_wr_max = 0;
	fifo->wr = 0;

	__enable_irq();
}
*/
