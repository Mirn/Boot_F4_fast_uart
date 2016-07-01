#include "stm32kiss.h"

const KS_PIN *adc_chanel_pins[19] = {
		PIN_A0, //0
		PIN_A1, //1
		PIN_A2, //2
		PIN_A3, //3
		PIN_A4, //4
		PIN_A5, //5
		PIN_A6, //6
		PIN_A7, //7
		PIN_B0, //8
		PIN_B1, //9
		PIN_C0, //10
		PIN_C1, //11
		PIN_C2, //12
		PIN_C3, //13
		PIN_C4, //14
		PIN_C5, //15
		NULL,
		NULL,
		NULL,
};

const uint32_t adc_chanels_nums[LENGTH(adc_chanel_pins)] = {
		ADC_Channel_0,
		ADC_Channel_1,
		ADC_Channel_2,
		ADC_Channel_3,
		ADC_Channel_4,
		ADC_Channel_5,
		ADC_Channel_6,
		ADC_Channel_7,
		ADC_Channel_8,
		ADC_Channel_9,
		ADC_Channel_10,
		ADC_Channel_11,
		ADC_Channel_12,
		ADC_Channel_13,
		ADC_Channel_14,
		ADC_Channel_15,
		ADC_Channel_TempSensor,
		ADC_Channel_Vrefint,
		ADC_Channel_Vbat,
};

KS_ADC_CONTEXT adc_context;

void adc_on(uint8_t adc_chanel, uint8_t ADC_SampleTime)
{
	if (adc_chanel >= LENGTH(adc_chanel_pins)) return;

	pin_analog_input(adc_chanel_pins[adc_chanel]);

	RCC_APB2PeriphClockCmd(RCC_APB2Periph_ADC, ENABLE);

	ADC_CommonInitTypeDef ADC_CommonInitStructure = {
			.ADC_Mode             = ADC_Mode_Independent,
			.ADC_Prescaler        = ADC_Prescaler_Div2,
			.ADC_DMAAccessMode    = ADC_DMAAccessMode_Disabled,
			.ADC_TwoSamplingDelay = ADC_TwoSamplingDelay_5Cycles
	};
	ADC_CommonInit(&ADC_CommonInitStructure);

	ADC_InitTypeDef ADC_InitStructure = {
			.ADC_NbrOfConversion      = 1,
			.ADC_Resolution           = ADC_Resolution_12b,
			.ADC_DataAlign            = ADC_DataAlign_Left,
			.ADC_ScanConvMode         = DISABLE,
			.ADC_ContinuousConvMode   = DISABLE,
			.ADC_ExternalTrigConvEdge = ADC_ExternalTrigConvEdge_None,
			.ADC_ExternalTrigConv     = ADC_ExternalTrigConv_T1_CC1,
	};
	ADC_Init(ADC1, &ADC_InitStructure);

	if ((adc_chanel == ADC_Channel_Vbat)) ADC_VBATCmd(ENABLE);
	if ((adc_chanel == ADC_Channel_TempSensor) || (adc_chanel == ADC_Channel_Vrefint)) ADC_TempSensorVrefintCmd(ENABLE);

	ADC_RegularChannelConfig(ADC1, adc_chanels_nums[adc_chanel], 1, ADC_SampleTime);
	ADC_Cmd(ADC1, ENABLE);

	adc_context.SampleTime = ADC_SampleTime;
	adc_context.chanel_num = adc_chanel;
	adc_context.enabled    = true;

	if ((adc_chanel == ADC_Channel_Vbat)) ADC_VBATCmd(ENABLE);
	if ((adc_chanel == ADC_Channel_TempSensor) || (adc_chanel == ADC_Channel_Vrefint)) ADC_TempSensorVrefintCmd(ENABLE);

	if ((adc_chanel == ADC_Channel_TempSensor) || (adc_chanel == ADC_Channel_Vrefint) || (adc_chanel == ADC_Channel_Vbat))
		delay_ms(10);
}

void adc_chanel(uint8_t adc_chanel)
{
	pin_disable(adc_chanel_pins[adc_context.chanel_num]);
	pin_analog_input(adc_chanel_pins[adc_chanel]);

	ADC_RegularChannelConfig(ADC1, adc_chanels_nums[adc_chanel], 1, adc_context.SampleTime);

	adc_context.chanel_num = adc_chanel;
}

void adc_off()
{
	ADC_Cmd(ADC1, DISABLE);
	ADC_DeInit();
	RCC_APB2PeriphClockCmd(RCC_APB2Periph_ADC1, DISABLE);

	adc_context.chanel_num = 0;
	adc_context.SampleTime = 0;
	adc_context.enabled    = false;
}

uint32_t adc_read()
{
	ADC_SoftwareStartConv(ADC1);

	while (ADC_GetSoftwareStartConvStatus(ADC1) == SET);
	while (ADC_GetFlagStatus(ADC1, ADC_FLAG_EOC) == RESET);

	uint16_t result = ADC_GetConversionValue(ADC1);
	ADC_ClearFlag(ADC1, ADC_FLAG_EOC);

	return result;
}
