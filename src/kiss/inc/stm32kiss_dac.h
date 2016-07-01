#ifndef __STM32KISS_DAC_H__
#define __STM32KISS_DAC_H__

#include "stm32f4xx_adc.h"

void dac_on();
void dac_off();

static inline uint16_t dac_write_a(uint16_t new_value)
{
	DAC->DHR12L1 = new_value;
	return new_value;
}

static inline uint16_t dac_write_b(uint16_t new_value)
{
	DAC->DHR12L2 = new_value;
	return new_value;
}

static inline void dac_write_both(uint16_t new_value_a, uint16_t new_value_b)
{
	DAC->DHR12LD = (new_value_a) | ((uint32_t)new_value_b << 16);
}

#endif //__STM32KISS_DAC_H__
