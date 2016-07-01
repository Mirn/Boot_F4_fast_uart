#include "stm32kiss.h"

void button_init(KS_BUTTON *button, const KS_PIN *pin, bool pull_up)
{
	ticks_init();

	if (pull_up)
		pin_input_up(pin);
	else
		pin_input_down(pin);

	button->pin             = pin;
	button->old_state       = pin_read(button->pin);
	button->antibounds_time = 25;
	button->timeout_ms      = 0;
}

bool button_check(KS_BUTTON *button)
{
	bool state;
	bool result;

	result = false;
	state  = pin_read(button->pin);

	if ((button->old_state == true) && (state == false))
		if (button->timeout_ms == 0)
			ticks_time_start(button->timeout_ms)
		else
		{
			if (ticks_time_ms(button->timeout_ms) > button->antibounds_time)
			{
				button->old_state = state;
				button->timeout_ms = 0;
				result = true;
			}
		}
	else
		button->old_state = state;

	return result;
}

