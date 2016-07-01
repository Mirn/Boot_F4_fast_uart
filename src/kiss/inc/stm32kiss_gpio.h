#ifndef __STM32KISS_GPIO_H__
#define __STM32KISS_GPIO_H__

typedef struct
{
	uint16_t clock_enabled_count;

} KS_PORT_CONTEXT;

typedef struct
{
	KS_PORT_CONTEXT *context;
	uint32_t clock_enable_const;
	void (*RCC_PeriphClockCmd)(uint32_t RCC_AHBPeriph, FunctionalState NewState);

} KS_PORT;

typedef struct
{
	GPIO_TypeDef  *GPIO;
	const KS_PORT *port;
	uint16_t       mask;
} KS_PIN;

extern KS_PORT_CONTEXT __PORTA_CONTEXT;
extern KS_PORT_CONTEXT __PORTB_CONTEXT;
extern KS_PORT_CONTEXT __PORTC_CONTEXT;
extern KS_PORT_CONTEXT __PORTD_CONTEXT;
extern KS_PORT_CONTEXT __PORTE_CONTEXT;
extern KS_PORT_CONTEXT __PORTF_CONTEXT;
extern KS_PORT_CONTEXT __PORTG_CONTEXT;

#define PORTA_CONTEXT (&__PORTA_CONTEXT)
#define PORTB_CONTEXT (&__PORTB_CONTEXT)
#define PORTC_CONTEXT (&__PORTC_CONTEXT)
#define PORTD_CONTEXT (&__PORTD_CONTEXT)
#define PORTE_CONTEXT (&__PORTE_CONTEXT)
#define PORTF_CONTEXT (&__PORTF_CONTEXT)
#define PORTG_CONTEXT (&__PORTG_CONTEXT)

extern const KS_PORT __PORTA;
extern const KS_PORT __PORTB;
extern const KS_PORT __PORTC;
extern const KS_PORT __PORTD;
extern const KS_PORT __PORTE;
extern const KS_PORT __PORTF;
extern const KS_PORT __PORTG;

#define PORTA (&__PORTA)
#define PORTB (&__PORTB)
#define PORTC (&__PORTC)
#define PORTD (&__PORTD)
#define PORTE (&__PORTE)
#define PORTF (&__PORTF)
#define PORTG (&__PORTG)



extern const KS_PIN __PIN_A0;
extern const KS_PIN __PIN_A1;
extern const KS_PIN __PIN_A2;
extern const KS_PIN __PIN_A3;
extern const KS_PIN __PIN_A4;
extern const KS_PIN __PIN_A5;
extern const KS_PIN __PIN_A6;
extern const KS_PIN __PIN_A7;
extern const KS_PIN __PIN_A8;
extern const KS_PIN __PIN_A9;
extern const KS_PIN __PIN_A10;
extern const KS_PIN __PIN_A11;
extern const KS_PIN __PIN_A12;
extern const KS_PIN __PIN_A13;
extern const KS_PIN __PIN_A14;
extern const KS_PIN __PIN_A15;

#define PIN_A0 (&__PIN_A0)
#define PIN_A1 (&__PIN_A1)
#define PIN_A2 (&__PIN_A2)
#define PIN_A3 (&__PIN_A3)
#define PIN_A4 (&__PIN_A4)
#define PIN_A5 (&__PIN_A5)
#define PIN_A6 (&__PIN_A6)
#define PIN_A7 (&__PIN_A7)
#define PIN_A8 (&__PIN_A8)
#define PIN_A9 (&__PIN_A9)
#define PIN_A10 (&__PIN_A10)
#define PIN_A11 (&__PIN_A11)
#define PIN_A12 (&__PIN_A12)
#define PIN_A13 (&__PIN_A13)
#define PIN_A14 (&__PIN_A14)
#define PIN_A15 (&__PIN_A15)



extern const KS_PIN __PIN_B0;
extern const KS_PIN __PIN_B1;
extern const KS_PIN __PIN_B2;
extern const KS_PIN __PIN_B3;
extern const KS_PIN __PIN_B4;
extern const KS_PIN __PIN_B5;
extern const KS_PIN __PIN_B6;
extern const KS_PIN __PIN_B7;
extern const KS_PIN __PIN_B8;
extern const KS_PIN __PIN_B9;
extern const KS_PIN __PIN_B10;
extern const KS_PIN __PIN_B11;
extern const KS_PIN __PIN_B12;
extern const KS_PIN __PIN_B13;
extern const KS_PIN __PIN_B14;
extern const KS_PIN __PIN_B15;

#define PIN_B0 (&__PIN_B0)
#define PIN_B1 (&__PIN_B1)
#define PIN_B2 (&__PIN_B2)
#define PIN_B3 (&__PIN_B3)
#define PIN_B4 (&__PIN_B4)
#define PIN_B5 (&__PIN_B5)
#define PIN_B6 (&__PIN_B6)
#define PIN_B7 (&__PIN_B7)
#define PIN_B8 (&__PIN_B8)
#define PIN_B9 (&__PIN_B9)
#define PIN_B10 (&__PIN_B10)
#define PIN_B11 (&__PIN_B11)
#define PIN_B12 (&__PIN_B12)
#define PIN_B13 (&__PIN_B13)
#define PIN_B14 (&__PIN_B14)
#define PIN_B15 (&__PIN_B15)



extern const KS_PIN __PIN_C0;
extern const KS_PIN __PIN_C1;
extern const KS_PIN __PIN_C2;
extern const KS_PIN __PIN_C3;
extern const KS_PIN __PIN_C4;
extern const KS_PIN __PIN_C5;
extern const KS_PIN __PIN_C6;
extern const KS_PIN __PIN_C7;
extern const KS_PIN __PIN_C8;
extern const KS_PIN __PIN_C9;
extern const KS_PIN __PIN_C10;
extern const KS_PIN __PIN_C11;
extern const KS_PIN __PIN_C12;
extern const KS_PIN __PIN_C13;
extern const KS_PIN __PIN_C14;
extern const KS_PIN __PIN_C15;

#define PIN_C0 (&__PIN_C0)
#define PIN_C1 (&__PIN_C1)
#define PIN_C2 (&__PIN_C2)
#define PIN_C3 (&__PIN_C3)
#define PIN_C4 (&__PIN_C4)
#define PIN_C5 (&__PIN_C5)
#define PIN_C6 (&__PIN_C6)
#define PIN_C7 (&__PIN_C7)
#define PIN_C8 (&__PIN_C8)
#define PIN_C9 (&__PIN_C9)
#define PIN_C10 (&__PIN_C10)
#define PIN_C11 (&__PIN_C11)
#define PIN_C12 (&__PIN_C12)
#define PIN_C13 (&__PIN_C13)
#define PIN_C14 (&__PIN_C14)
#define PIN_C15 (&__PIN_C15)



extern const KS_PIN __PIN_D0;
extern const KS_PIN __PIN_D1;
extern const KS_PIN __PIN_D2;
extern const KS_PIN __PIN_D3;
extern const KS_PIN __PIN_D4;
extern const KS_PIN __PIN_D5;
extern const KS_PIN __PIN_D6;
extern const KS_PIN __PIN_D7;
extern const KS_PIN __PIN_D8;
extern const KS_PIN __PIN_D9;
extern const KS_PIN __PIN_D10;
extern const KS_PIN __PIN_D11;
extern const KS_PIN __PIN_D12;
extern const KS_PIN __PIN_D13;
extern const KS_PIN __PIN_D14;
extern const KS_PIN __PIN_D15;

#define PIN_D0 (&__PIN_D0)
#define PIN_D1 (&__PIN_D1)
#define PIN_D2 (&__PIN_D2)
#define PIN_D3 (&__PIN_D3)
#define PIN_D4 (&__PIN_D4)
#define PIN_D5 (&__PIN_D5)
#define PIN_D6 (&__PIN_D6)
#define PIN_D7 (&__PIN_D7)
#define PIN_D8 (&__PIN_D8)
#define PIN_D9 (&__PIN_D9)
#define PIN_D10 (&__PIN_D10)
#define PIN_D11 (&__PIN_D11)
#define PIN_D12 (&__PIN_D12)
#define PIN_D13 (&__PIN_D13)
#define PIN_D14 (&__PIN_D14)
#define PIN_D15 (&__PIN_D15)



extern const KS_PIN __PIN_E0;
extern const KS_PIN __PIN_E1;
extern const KS_PIN __PIN_E2;
extern const KS_PIN __PIN_E3;
extern const KS_PIN __PIN_E4;
extern const KS_PIN __PIN_E5;
extern const KS_PIN __PIN_E6;
extern const KS_PIN __PIN_E7;
extern const KS_PIN __PIN_E8;
extern const KS_PIN __PIN_E9;
extern const KS_PIN __PIN_E10;
extern const KS_PIN __PIN_E11;
extern const KS_PIN __PIN_E12;
extern const KS_PIN __PIN_E13;
extern const KS_PIN __PIN_E14;
extern const KS_PIN __PIN_E15;

#define PIN_E0 (&__PIN_E0)
#define PIN_E1 (&__PIN_E1)
#define PIN_E2 (&__PIN_E2)
#define PIN_E3 (&__PIN_E3)
#define PIN_E4 (&__PIN_E4)
#define PIN_E5 (&__PIN_E5)
#define PIN_E6 (&__PIN_E6)
#define PIN_E7 (&__PIN_E7)
#define PIN_E8 (&__PIN_E8)
#define PIN_E9 (&__PIN_E9)
#define PIN_E10 (&__PIN_E10)
#define PIN_E11 (&__PIN_E11)
#define PIN_E12 (&__PIN_E12)
#define PIN_E13 (&__PIN_E13)
#define PIN_E14 (&__PIN_E14)
#define PIN_E15 (&__PIN_E15)



extern const KS_PIN __PIN_F0;
extern const KS_PIN __PIN_F1;
extern const KS_PIN __PIN_F2;
extern const KS_PIN __PIN_F3;
extern const KS_PIN __PIN_F4;
extern const KS_PIN __PIN_F5;
extern const KS_PIN __PIN_F6;
extern const KS_PIN __PIN_F7;
extern const KS_PIN __PIN_F8;
extern const KS_PIN __PIN_F9;
extern const KS_PIN __PIN_F10;
extern const KS_PIN __PIN_F11;
extern const KS_PIN __PIN_F12;
extern const KS_PIN __PIN_F13;
extern const KS_PIN __PIN_F14;
extern const KS_PIN __PIN_F15;

#define PIN_F0 (&__PIN_F0)
#define PIN_F1 (&__PIN_F1)
#define PIN_F2 (&__PIN_F2)
#define PIN_F3 (&__PIN_F3)
#define PIN_F4 (&__PIN_F4)
#define PIN_F5 (&__PIN_F5)
#define PIN_F6 (&__PIN_F6)
#define PIN_F7 (&__PIN_F7)
#define PIN_F8 (&__PIN_F8)
#define PIN_F9 (&__PIN_F9)
#define PIN_F10 (&__PIN_F10)
#define PIN_F11 (&__PIN_F11)
#define PIN_F12 (&__PIN_F12)
#define PIN_F13 (&__PIN_F13)
#define PIN_F14 (&__PIN_F14)
#define PIN_F15 (&__PIN_F15)



extern const KS_PIN __PIN_G0;
extern const KS_PIN __PIN_G1;
extern const KS_PIN __PIN_G2;
extern const KS_PIN __PIN_G3;
extern const KS_PIN __PIN_G4;
extern const KS_PIN __PIN_G5;
extern const KS_PIN __PIN_G6;
extern const KS_PIN __PIN_G7;
extern const KS_PIN __PIN_G8;
extern const KS_PIN __PIN_G9;
extern const KS_PIN __PIN_G10;
extern const KS_PIN __PIN_G11;
extern const KS_PIN __PIN_G12;
extern const KS_PIN __PIN_G13;
extern const KS_PIN __PIN_G14;
extern const KS_PIN __PIN_G15;

#define PIN_G0 (&__PIN_G0)
#define PIN_G1 (&__PIN_G1)
#define PIN_G2 (&__PIN_G2)
#define PIN_G3 (&__PIN_G3)
#define PIN_G4 (&__PIN_G4)
#define PIN_G5 (&__PIN_G5)
#define PIN_G6 (&__PIN_G6)
#define PIN_G7 (&__PIN_G7)
#define PIN_G8 (&__PIN_G8)
#define PIN_G9 (&__PIN_G9)
#define PIN_G10 (&__PIN_G10)
#define PIN_G11 (&__PIN_G11)
#define PIN_G12 (&__PIN_G12)
#define PIN_G13 (&__PIN_G13)
#define PIN_G14 (&__PIN_G14)
#define PIN_G15 (&__PIN_G15)

void pin_mode(const KS_PIN *pin, GPIOMode_TypeDef pin_mode, GPIOOType_TypeDef pin_out_type, GPIOPuPd_TypeDef pin_pull_type);

#define pin_input(pin)	        pin_mode(pin, GPIO_Mode_IN,  GPIO_OType_PP, GPIO_PuPd_NOPULL)
#define pin_input_up(pin)       pin_mode(pin, GPIO_Mode_IN,  GPIO_OType_PP, GPIO_PuPd_UP)
#define pin_input_down(pin)     pin_mode(pin, GPIO_Mode_IN,  GPIO_OType_PP, GPIO_PuPd_DOWN)
#define pin_output(pin, val)    {pin_mode(pin, GPIO_Mode_OUT, GPIO_OType_PP, GPIO_PuPd_NOPULL); pin_write(pin, val);}
#define pin_open_drain(pin,val) {pin_mode(pin, GPIO_Mode_OUT, GPIO_OType_OD, GPIO_PuPd_NOPULL); pin_write(pin, val);}

#define pin_alt_input(pin)      pin_mode(pin, GPIO_Mode_AF, GPIO_OType_PP, GPIO_PuPd_NOPULL)
#define pin_alt_input_up(pin)   pin_mode(pin, GPIO_Mode_AF, GPIO_OType_PP, GPIO_PuPd_UP)
#define pin_alt_input_down(pin) pin_mode(pin, GPIO_Mode_AF, GPIO_OType_PP, GPIO_PuPd_DOWN)
#define pin_alt_output(pin)     pin_mode(pin, GPIO_Mode_AF, GPIO_OType_PP, GPIO_PuPd_NOPULL)
#define pin_alt_drain(pin)      pin_mode(pin, GPIO_Mode_AF, GPIO_OType_OD, GPIO_PuPd_NOPULL)

#define pin_analog_input(pin)   pin_mode(pin, GPIO_Mode_AN, GPIO_OType_PP, GPIO_PuPd_NOPULL)
#define pin_analog_output(pin)  pin_mode(pin, GPIO_Mode_AN, GPIO_OType_PP, GPIO_PuPd_NOPULL)

void pin_disable(const KS_PIN *pin);

void pin_enable_clock(const KS_PIN *pin);

static __inline__ bool pin_write_1(const KS_PIN *pin)
{
	if (pin == NULL) return true;
	pin->GPIO->BSRRL = pin->mask;
	return true;
}

static __inline__ bool pin_write_0(const KS_PIN *pin)
{
	if (pin == NULL) return false;
	pin->GPIO->BSRRH = pin->mask;
	return false;
}

static __inline__ bool pin_read(const KS_PIN *pin)
{
	if (pin == NULL) return false;
	return (pin->GPIO->IDR & pin->mask) != false;
}

static __inline__ bool pin_invert(const KS_PIN *pin)
{
	if (pin == NULL) return false;
	bool result = pin_read(pin);
	pin->GPIO->ODR ^= pin->mask;
	return result;
}

static __inline__ bool pin_write(const KS_PIN *pin, bool new_value)
{
	if (new_value == false)
		pin_write_0(pin);
	else
		pin_write_1(pin);

	return new_value;
}

#define pin_set(pin)   pin_write_1(pin)
#define pin_reset(pin) pin_write_0(pin)

#define pin_write_vcc(pin) pin_write_1(pin)
#define pin_write_gnd(pin) pin_write_0(pin)

#define VCC true
#define GND false

static __inline__ void ___bind_mode_n(const KS_PIN **bind, GPIOMode_TypeDef mode, GPIOOType_TypeDef pin_out_type, GPIOPuPd_TypeDef pin_pull_type, uint8_t count)
{
	uint32_t k;

	for (k=0; k < count; k++)
		pin_mode(bind[k], mode, pin_out_type, pin_pull_type);
}

static __inline__ void ___bind_write_n(const KS_PIN **bind, uint32_t value, uint8_t count)
{
	uint32_t k;

	for (k=0; k < count; k++)
	{
		if (value & 1)
			pin_write_1(bind[k]);
		else
			pin_write_0(bind[k]);

		value = value >> 1;
	}
}

static __inline__ uint32_t ___bind_read_n(const KS_PIN **bind, uint8_t count)
{
	uint32_t k;
	uint32_t value = 0;

	for (k=0; k < count; k++)
		if (pin_read(bind[k]))
			value = value | (1 << k);

	return value;
}

#define bind_pins(name, count) const KS_PIN *name[count]
#define bind_mode(bind, mode)   ___bind_mode_n(bind, mode, LENGTH(bind))
#define bind_write(bind, value) ___bind_write_n(bind, value, LENGTH(bind))
#define bind_read(bind) 		___bind_read_n(bind, LENGTH(bind))


#endif //#ifndef __STM32KISS_GPIO_H__
