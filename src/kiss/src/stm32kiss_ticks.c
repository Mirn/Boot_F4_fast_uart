#include "stm32kiss.h"

//void (*systick_func)() = NULL;

//void SysTick_Handler()
//{
////	if (systick_func != NULL)
////		(*systick_func)();
//}
//
////void systick_on(uint16_t freq, void (*func)())
//void systick_on(uint16_t freq)
//{
////	systick_func = func;
//
////	SysTick_CLKSourceConfig(SysTick_CLKSource_HCLK);
//	SysTick_Config(SystemCoreClock / freq);
//
//	SysTick->CTRL |= SysTick_CLKSource_HCLK;
//	SysTick->LOAD  = (SystemCoreClock / freq) - 1;
//	SysTick->VAL   = 0;                                          /* Load the SysTick Counter Value */
//	SysTick->CTRL  = SysTick_CTRL_CLKSOURCE_Msk |
//	                   SysTick_CTRL_TICKINT_Msk   |
//	                   SysTick_CTRL_ENABLE_Msk;                    /* Enable SysTick IRQ and SysTick Timer */
//
//}

/*void systick_set_func(void (*func)())
{
	systick_func = func;
}

void systick_off()
{
	SysTick_CLKSourceConfig(SysTick_CLKSource_HCLK_Div8);
	SysTick_Config(SysTick_LOAD_RELOAD_Msk);
	SysTick->CTRL = 0x00000000;
	systick_func = NULL;
}
*/

void ticks_init()
{
    SCB_DEMCR   |= 0x01000000;
    //DWT_CONTROL &= ~1; // disable the counter
    //DWT_CYCCNT   = 0xF0000000ul;
    DWT_CONTROL |= 1; // enable the counter
}

void delay_next_us(uint16_t time_us)
{
	static uint32_t old_time = 0;
	uint32_t time = DWT_CYCCNT;
	//uint32_t limit = (((uint64_t)SystemCoreClock*time_us) / 1000000);
	uint32_t limit = (SystemCoreClock/ 1000000) * time_us;

	while ((time - old_time) < limit)
		time = DWT_CYCCNT;
	old_time = time;
}

void __delay_next_us(uint16_t time_us)
{
	static uint32_t old_time = 0;
	uint32_t time = DWT_CYCCNT;
	//uint32_t limit = (((uint64_t)SystemCoreClock*time_us) / 1000000);
	uint32_t limit = (SystemCoreClock/ 1000000) * time_us;

	while (time - old_time < limit)
		time = DWT_CYCCNT;
	old_time = time;
}

void __delay_ms(uint32_t time_ms)
{
	while (time_ms--)
		delay_next_us(1000);
}

void delay_ms(uint32_t time_ms)
{
	delay_next_us(0);
	__delay_ms(time_ms);
}

void delay_seconds(uint16_t seconds)
{
	delay_next_us(0);
	while (seconds--)
		__delay_ms(1000);
}
