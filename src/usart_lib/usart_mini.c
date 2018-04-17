#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#ifdef STM32F10X_LD_VL
#include "stm32f10x.h"
#include "stm32f10x_usart.h"
#include "stm32f10x_rcc.h"
#include "stm32f10x_gpio.h"
#endif

#ifdef STM32F4XX
#include "misc.h"
#include "stm32f4xx.h"
#include "stm32f4xx_rcc.h"
#include "stm32f4xx_gpio.h"
#include "stm32f4xx_usart.h"

//#define USART_BOD_DEBUG

#include "misc_inline.h"
#include "stm32f4xx_rcc_inline.h"
#include "stm32f4xx_gpio_inline.h"
#include "stm32f4xx_usart_inline.h"
#endif

#include "usart_mini.h"

//#define USART_BOD 500000
#define USART_BOD 921600
//#define USART_BOD 115200

//#define USE_USART1
#define USE_USART3

#ifdef USE_USART1
#define USART USART1
#endif

#ifdef USE_USART3
#define USART USART3
#endif

uint8_t rx_buffer[0x1C000]  __attribute__ ((section (".usart_mini_rx_buffer"), used));

volatile uint32_t rx_pos_write = 0;
volatile uint32_t rx_pos_read  = 0;

uint32_t rx_errors = 0;
uint32_t rx_overfulls = 0;
uint32_t rx_count_max = 0;

void usart_deinit()
{
#ifdef USE_USART1
    NVIC_InitTypeDef NVIC_InitStructure = {
    		.NVIC_IRQChannel = USART1_IRQn,
    		.NVIC_IRQChannelPreemptionPriority = 0,
    		.NVIC_IRQChannelSubPriority = 0,
    		.NVIC_IRQChannelCmd = DISABLE,
    };
    NVIC_Init_inline(&NVIC_InitStructure);

	USART_DeInit_inline(USART1);
	GPIO_DeInit_inline(GPIOA);

	RCC_APB2PeriphClockCmd_inline(RCC_APB2Periph_USART1, DISABLE);
    RCC_AHB1PeriphClockCmd_inline(RCC_AHB1Periph_GPIOA, DISABLE);
#endif

#ifdef USE_USART3
    NVIC_InitTypeDef NVIC_InitStructure = {
    		.NVIC_IRQChannel = USART3_IRQn,
    		.NVIC_IRQChannelPreemptionPriority = 0,
    		.NVIC_IRQChannelSubPriority = 0,
    		.NVIC_IRQChannelCmd = DISABLE,
    };
    NVIC_Init_inline(&NVIC_InitStructure);

	USART_DeInit_inline(USART3);
	GPIO_DeInit_inline(GPIOB);

	RCC_APB1PeriphClockCmd_inline(RCC_APB1Periph_USART3, DISABLE);
    RCC_AHB1PeriphClockCmd_inline(RCC_AHB1Periph_GPIOB, DISABLE);
#endif
}

void usart_init()
{
#ifdef STM32F4XX
	usart_deinit();

#ifdef USE_USART1
	RCC_APB2PeriphClockCmd_inline(RCC_APB2Periph_USART1, ENABLE);
    RCC_AHB1PeriphClockCmd_inline(RCC_AHB1Periph_GPIOA, ENABLE);

	GPIO_PinAFConfig_inline(GPIOA, GPIO_PinSource9,  GPIO_AF_USART1);
	GPIO_PinAFConfig_inline(GPIOA, GPIO_PinSource10, GPIO_AF_USART1);


    GPIO_InitTypeDef GPIO_InitStructure =
    {
    		.GPIO_OType = GPIO_OType_PP,
    		.GPIO_PuPd  = GPIO_PuPd_UP,
    		.GPIO_Mode  = GPIO_Mode_AF,
    		.GPIO_Pin   = GPIO_Pin_9 | GPIO_Pin_10
    };
    GPIO_Init_inline(GPIOA, &GPIO_InitStructure);
#endif

#ifdef USE_USART3
	RCC_APB1PeriphClockCmd_inline(RCC_APB1Periph_USART3, ENABLE);
    RCC_AHB1PeriphClockCmd_inline(RCC_AHB1Periph_GPIOB, ENABLE);

	GPIO_PinAFConfig_inline(GPIOB, GPIO_PinSource10,  GPIO_AF_USART3);
	GPIO_PinAFConfig_inline(GPIOB, GPIO_PinSource11, GPIO_AF_USART3);


    GPIO_InitTypeDef GPIO_InitStructure =
    {
    		.GPIO_OType = GPIO_OType_PP,
    		.GPIO_PuPd  = GPIO_PuPd_UP,
    		.GPIO_Mode  = GPIO_Mode_AF,
    		.GPIO_Pin   = GPIO_Pin_10 | GPIO_Pin_11
    };
    GPIO_Init_inline(GPIOB, &GPIO_InitStructure);
#endif

    USART_InitTypeDef USART_InitStructure = {
            .USART_BaudRate            = USART_BOD,
            .USART_WordLength          = USART_WordLength_8b,
            .USART_StopBits            = USART_StopBits_1,
            .USART_Parity              = USART_Parity_No,
            .USART_HardwareFlowControl = USART_HardwareFlowControl_None,
            .USART_Mode                = USART_Mode_Rx | USART_Mode_Tx
    };

    USART_Init_inline(USART, &USART_InitStructure);

    NVIC_InitTypeDef NVIC_InitStructure = {
#ifdef USE_USART1
    		.NVIC_IRQChannel = USART1_IRQn,
#endif
#ifdef USE_USART3
    		.NVIC_IRQChannel = USART3_IRQn,
#endif
    		.NVIC_IRQChannelPreemptionPriority = 0,
    		.NVIC_IRQChannelSubPriority = 0,
    		.NVIC_IRQChannelCmd = ENABLE,
    };
    NVIC_Init_inline(&NVIC_InitStructure);
    USART_ITConfig_inline(USART, USART_IT_RXNE, ENABLE);

    USART_Cmd_inline(USART, ENABLE);
#endif

    send('\r');
    send('\r');

#ifdef USART_BOD_DEBUG
#pragma GCC diagnostic ignored "-Wformat"
	  printf("USART_BaudRate\t%i\r", debug[0]);
	  printf("apbclock      \t%i\r", debug[1]);
	  printf("integerdivider\t%i\r", debug[2]);
	  printf("tmpreg        \t0x%04X\r", debug[3]);
	  printf(" \r");
#endif
}

__attribute__ ((long_call, section(".data")))
void USART1_IRQHandler(void)
{
	if (USART_GetITStatus_inline(USART, USART_IT_RXNE) != RESET)
	{
		rx_buffer[rx_pos_write % sizeof(rx_buffer)] = USART_ReceiveData_inline(USART);
		rx_pos_write++;
	}

	if (USART_GetITStatus_inline(USART, USART_IT_ORE_RX) != RESET) {USART_ReceiveData_inline(USART); rx_errors++;};
//	if (USART_GetITStatus_inline(USART1, USART_IT_ORE_ER) != RESET) {send('2'); };
//	if (USART_GetITStatus_inline(USART1, USART_IT_NE    ) != RESET) {send('3'); };
//	if (USART_GetITStatus_inline(USART1, USART_IT_FE    ) != RESET) {send('4'); };
//	if (USART_GetITStatus_inline(USART1, USART_IT_PE    ) != RESET) {send('5'); };
}

bool recive_byte(uint8_t *rx_data)
{
	if (rx_pos_read == rx_pos_write) return false;

	uint32_t count = recive_count();
	if (rx_count_max < count)
		rx_count_max = count;

	if (count >= sizeof(rx_buffer))
	{
		rx_pos_read = rx_pos_write + 1 - sizeof(rx_buffer);
		rx_overfulls++;
		send_str("Over!\r");
	}

	*rx_data = rx_buffer[rx_pos_read % sizeof(rx_buffer)];
	rx_pos_read++;
	return true;
}

uint32_t recive_count()
{
	return rx_pos_write - rx_pos_read;
}

uint32_t recive_size()
{
	return sizeof(rx_buffer);
}

void send(const uint8_t tx_data)
{
    USART->DR = tx_data;
	while ((USART->SR & USART_FLAG_TC) == RESET);
}

void send_block(const uint8_t *data, const uint32_t size)
{
	uint32_t cnt = size;
	while (cnt--)
		send(*(data++));
}

void send_str(const char *str)
{
	while (*str)
		send(*(str++));
}

