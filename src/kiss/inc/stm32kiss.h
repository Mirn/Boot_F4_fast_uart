#ifndef __STM32KISS_H__
#define __STM32KISS_H__

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "misc.h"
#include "stm32f4xx.h"
#include "stm32f4xx_adc.h"
//#include "stm32f4xx_bkp.h"
//#include "stm32f4xx_can.h"
//#include "stm32f4xx_cec.h"
#include "stm32f4xx_crc.h"
#include "stm32f4xx_dac.h"
//#include "stm32f4xx_dbgmcu.h"
#include "stm32f4xx_dma.h"
//#include "stm32f4xx_exti.h"
#include "stm32f4xx_flash.h"
//#include "stm32f4xx_fsmc.h"
#include "stm32f4xx_gpio.h"
//#include "stm32f4xx_i2c.h"
//#include "stm32f4xx_iwdg.h"
//#include "stm32f4xx_pwr.h"
#include "stm32f4xx_rcc.h"
//#include "stm32f4xx_rtc.h"
//#include "stm32f4xx_sdio.h"
#include "stm32f4xx_spi.h"
#include "stm32f4xx_tim.h"
//#include "stm32f4xx_usart.h"
//#include "stm32f4xx_wwdg.h"

typedef struct
{
	uint32_t min;
	uint32_t max;
} tLIMIT;

#ifdef STM32F4XX
#define DEVICE_ID_BLOCK_PTR ((uint8_t*)0x1FFF7A10)

#define FLASH_PAGE_SIZE      1024
#define FLASH_SIZE          (*((uint16_t *)0x1FFF7A22))

#define BOOTLOADER_SIZE     0x8000
#define BOOTLOADER_FROM    (FLASH_BASE)
#define BOOTLOADER_TO      (FLASH_BASE + BOOTLOADER_SIZE)
#define MAIN_START_FROM    (BOOTLOADER_TO)
#define MAIN_RUN_FROM      (FLASH_BASE + 0x1000)
#endif

#define FLASH_SIZE_CORRECT   ((FLASH_SIZE*1024 - BOOTLOADER_SIZE) / 1024)
#define FLASH_SIZE_CORRECT_L (FLASH_SIZE_CORRECT & 0xFF)
#define FLASH_SIZE_CORRECT_H (FLASH_SIZE_CORRECT >> 8)

#define MIN(a,b)  (((a) < (b)) ? (a) : (b))
#define MAX(a,b)  (((a) > (b)) ? (a) : (b))
#define AVRG(a,b) (((a) + (b))/2)
#define DELTA(a,b, scale) ((MAX(a, b) - MIN(a, b))*scale) / AVRG(a, b)
#define IN_LIMIT(value, limit) ((limit.min <= value) && (value <= limit.max))

#define MILLION 1000000.0

#define STRUCT_CLEAR(v) memset((void *)&v, 0, sizeof(v))
#define ZERO_MEMORY(v) STRUCT_CLEAR(v)
#define LENGTH(v) (sizeof(v) / sizeof(v[0]))
#define OPT_BARRIER() asm volatile ("": : :"memory")

#define PI 3.1415926535897932384626433832795f

#define UNUSED __attribute__ ((unused))

#pragma GCC diagnostic ignored "-Wformat"

//#include "stm32kiss_adc.h"
//#include "stm32kiss_dac.h"
//#include "stm32kiss_gpio.h"
#include "stm32kiss_ticks.h"
//#include "stm32kiss_button.h"
//#include "stm32kiss_fifo.h"
//#include "stm32kiss_dma_usarts.h"

void PrintChar(char c);
signed int printf(const char *pFormat, ...);

#endif //#ifndef __STM32KISS_H__
