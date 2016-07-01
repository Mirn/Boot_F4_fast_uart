#ifndef __STM32KISS_BUTTON_H__
#define __STM32KISS_BUTTON_H__

typedef struct
{
	const KS_PIN  *pin;
	bool     old_state;
	bool     new_state;
	uint32_t antibounds_time;
	uint32_t timeout_ms;
} KS_BUTTON;

void button_init(KS_BUTTON *button, const KS_PIN *pin, bool pull_up);
bool button_check(KS_BUTTON *button);

#endif //#ifndef __STM32KISS_BUTTON_H__
