#ifndef __STM32KISS_ADC_H__
#define __STM32KISS_ADC_H__

#include "stm32kiss.h"

typedef struct
{
	uint32_t SampleTime;
	uint32_t chanel_num;
	bool	 enabled;
} KS_ADC_CONTEXT;

extern KS_ADC_CONTEXT adc_context;

void adc_on(uint8_t adc_chanel, uint8_t ADC_SampleTime);
void adc_off();

uint32_t adc_read();
void     adc_chanel(uint8_t adc_chanel);

#endif //__STM32KISS_ADC_H__
