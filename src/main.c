#include "stm32kiss.h"
#include "usart_mini.h"

#define SIMPLE_TEST
#ifndef SIMPLE_TEST

#pragma GCC diagnostic ignored "-Wformat" //for simple printf without format %l modificator

volatile uint32_t testvalue = 0x12345677; //test data section initialization

void main(void)
{
	//SystemCoreClockUpdate();
	usart_init();
	ticks_init();
	delay_ms(1000);
	printf("SystemCoreClock\t%i\tHz\r\n", SystemCoreClock);

	while (1)
	{
		delay_ms(1000); //test delay and systemclock
		testvalue++;
		printf("testvalue\t%08X\r\n", testvalue);
	}
}

#else

volatile uint8_t testvalue = '.'; //test data section initialization

void main(void)
{
	//SystemCoreClockUpdate();
	usart_init();
	ticks_init();

	while (1)
	{
		delay_ms(1000); //test delay and systemclock
		send(testvalue);
		testvalue ^= 1;
	}
}
#endif
