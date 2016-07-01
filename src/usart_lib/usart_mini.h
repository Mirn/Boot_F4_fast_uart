#ifndef __USART_MINI_H__
#define __USART_MINI_H__

#ifdef STM32F10X_LD_VL
#include "stm32f10x_usart.h"
#endif

#ifdef STM32F4XX
#include "stm32f4xx_usart.h"
#endif


static inline bool recive_ready()
{
	return ((USART1->SR & USART_FLAG_RXNE) != (u16)RESET);
}

static inline uint8_t recive()
{
	return USART1->DR;
}

void usart_init();
bool wait(uint8_t *rx_data);
void send_str(const char *str);
void send(uint8_t tx_data);

void __send_block(uint8_t *data, uint32_t size);
#define send_block(v) __send_block((uint8_t*)v, sizeof(v))

#endif
