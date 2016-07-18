#ifndef __STM32KISS_TICKS_H__
#define __STM32KISS_TICKS_H__


#define    DWT_CYCCNT    *(volatile uint32_t *)0xE0001004
#define    DWT_CONTROL   *(volatile uint32_t *)0xE0001000
#define    SCB_DEMCR     *(volatile uint32_t *)0xE000EDFC

//void systick_on(uint16_t freq);
//void systick_on(uint16_t freq, void (*func)());
//void systick_set_func(void (*func)());
//void systick_off();

void ticks_init();

#define ticks_get()          (DWT_CYCCNT)
#define ticks_time_start(v)  {v = DWT_CYCCNT;}
#define ticks_time_ms(v)     ((DWT_CYCCNT - v) / (SystemCoreClock / 1000))
#define ticks_time_us(v)     ((DWT_CYCCNT - v) / (SystemCoreClock / 1000000))

#define delay_init() ticks_init();
void delay_ms(uint32_t time_ms);
void delay_seconds(uint16_t seconds);

static __inline__ void delay_us(uint16_t time_us)
{
	uint32_t limit = DWT_CYCCNT + ((SystemCoreClock / (1000000 / time_us)));
	while (DWT_CYCCNT < limit);
}

void delay_next_us(uint16_t time_us);

#endif //#ifndef __STM32KISS_TICKS_H__
