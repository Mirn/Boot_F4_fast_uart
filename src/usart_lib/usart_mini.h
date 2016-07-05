#ifndef __USART_MINI_H__
#define __USART_MINI_H__

#ifdef STM32F10X_LD_VL
#include "stm32f10x_usart.h"
#endif

#ifdef STM32F4XX
#include "stm32f4xx_usart.h"
#endif


void usart_init();

void send_str(const char *str);
void send(const uint8_t tx_data);

bool recive_byte(uint8_t *rx_data);
uint32_t recive_count();
uint32_t recive_free();

void __send_block(const uint8_t *data, const uint32_t size);
#define send_block(v) __send_block((uint8_t*)v, sizeof(v))

#endif
