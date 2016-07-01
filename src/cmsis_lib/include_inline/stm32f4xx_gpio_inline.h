/*
 * stm32f4xx_gpio_inline.h
 *
 *  Created on: 30 θώνÿ 2016 γ.
 *      Author: Easy
 */

#ifndef STM32F4XX_GPIO_INLINE_H_
#define STM32F4XX_GPIO_INLINE_H_

#include "stm32f4xx_gpio.h"
#include "stm32f4xx_rcc.h"

/**
  * @brief  Changes the mapping of the specified pin.
  * @param  GPIOx: where x can be (A..I) to select the GPIO peripheral.
  * @param  GPIO_PinSource: specifies the pin for the Alternate function.
  *         This parameter can be GPIO_PinSourcex where x can be (0..15).
  * @param  GPIO_AFSelection: selects the pin to used as Alternate function.
  *          This parameter can be one of the following values:
  *            @arg GPIO_AF_RTC_50Hz: Connect RTC_50Hz pin to AF0 (default after reset)
  *            @arg GPIO_AF_MCO: Connect MCO pin (MCO1 and MCO2) to AF0 (default after reset)
  *            @arg GPIO_AF_TAMPER: Connect TAMPER pins (TAMPER_1 and TAMPER_2) to AF0 (default after reset)
  *            @arg GPIO_AF_SWJ: Connect SWJ pins (SWD and JTAG)to AF0 (default after reset)
  *            @arg GPIO_AF_TRACE: Connect TRACE pins to AF0 (default after reset)
  *            @arg GPIO_AF_TIM1: Connect TIM1 pins to AF1
  *            @arg GPIO_AF_TIM2: Connect TIM2 pins to AF1
  *            @arg GPIO_AF_TIM3: Connect TIM3 pins to AF2
  *            @arg GPIO_AF_TIM4: Connect TIM4 pins to AF2
  *            @arg GPIO_AF_TIM5: Connect TIM5 pins to AF2
  *            @arg GPIO_AF_TIM8: Connect TIM8 pins to AF3
  *            @arg GPIO_AF_TIM9: Connect TIM9 pins to AF3
  *            @arg GPIO_AF_TIM10: Connect TIM10 pins to AF3
  *            @arg GPIO_AF_TIM11: Connect TIM11 pins to AF3
  *            @arg GPIO_AF_I2C1: Connect I2C1 pins to AF4
  *            @arg GPIO_AF_I2C2: Connect I2C2 pins to AF4
  *            @arg GPIO_AF_I2C3: Connect I2C3 pins to AF4
  *            @arg GPIO_AF_SPI1: Connect SPI1 pins to AF5
  *            @arg GPIO_AF_SPI2: Connect SPI2/I2S2 pins to AF5
  *            @arg GPIO_AF_SPI3: Connect SPI3/I2S3 pins to AF6
  *            @arg GPIO_AF_I2S3ext: Connect I2S3ext pins to AF7
  *            @arg GPIO_AF_USART1: Connect USART1 pins to AF7
  *            @arg GPIO_AF_USART2: Connect USART2 pins to AF7
  *            @arg GPIO_AF_USART3: Connect USART3 pins to AF7
  *            @arg GPIO_AF_UART4: Connect UART4 pins to AF8
  *            @arg GPIO_AF_UART5: Connect UART5 pins to AF8
  *            @arg GPIO_AF_USART6: Connect USART6 pins to AF8
  *            @arg GPIO_AF_CAN1: Connect CAN1 pins to AF9
  *            @arg GPIO_AF_CAN2: Connect CAN2 pins to AF9
  *            @arg GPIO_AF_TIM12: Connect TIM12 pins to AF9
  *            @arg GPIO_AF_TIM13: Connect TIM13 pins to AF9
  *            @arg GPIO_AF_TIM14: Connect TIM14 pins to AF9
  *            @arg GPIO_AF_OTG_FS: Connect OTG_FS pins to AF10
  *            @arg GPIO_AF_OTG_HS: Connect OTG_HS pins to AF10
  *            @arg GPIO_AF_ETH: Connect ETHERNET pins to AF11
  *            @arg GPIO_AF_FSMC: Connect FSMC pins to AF12
  *            @arg GPIO_AF_OTG_HS_FS: Connect OTG HS (configured in FS) pins to AF12
  *            @arg GPIO_AF_SDIO: Connect SDIO pins to AF12
  *            @arg GPIO_AF_DCMI: Connect DCMI pins to AF13
  *            @arg GPIO_AF_EVENTOUT: Connect EVENTOUT pins to AF15
  * @retval None
  */
static inline void GPIO_PinAFConfig_inline(GPIO_TypeDef* GPIOx, uint16_t GPIO_PinSource, uint8_t GPIO_AF)
{
  uint32_t temp = 0x00;
  uint32_t temp_2 = 0x00;

  /* Check the parameters */
  assert_param(IS_GPIO_ALL_PERIPH(GPIOx));
  assert_param(IS_GPIO_PIN_SOURCE(GPIO_PinSource));
  assert_param(IS_GPIO_AF(GPIO_AF));

  temp = ((uint32_t)(GPIO_AF) << ((uint32_t)((uint32_t)GPIO_PinSource & (uint32_t)0x07) * 4)) ;
  GPIOx->AFR[GPIO_PinSource >> 0x03] &= ~((uint32_t)0xF << ((uint32_t)((uint32_t)GPIO_PinSource & (uint32_t)0x07) * 4)) ;
  temp_2 = GPIOx->AFR[GPIO_PinSource >> 0x03] | temp;
  GPIOx->AFR[GPIO_PinSource >> 0x03] = temp_2;
}


/**
  * @brief  Initializes the GPIOx peripheral according to the specified parameters in the GPIO_InitStruct.
  * @param  GPIOx: where x can be (A..I) to select the GPIO peripheral.
  * @param  GPIO_InitStruct: pointer to a GPIO_InitTypeDef structure that contains
  *         the configuration information for the specified GPIO peripheral.
  * @retval None
  */
static inline void GPIO_Init_inline(GPIO_TypeDef* GPIOx, GPIO_InitTypeDef* GPIO_InitStruct)
{
  uint32_t pinpos = 0x00, pos = 0x00 , currentpin = 0x00;

  /* Check the parameters */
  assert_param(IS_GPIO_ALL_PERIPH(GPIOx));
  assert_param(IS_GPIO_PIN(GPIO_InitStruct->GPIO_Pin));
  assert_param(IS_GPIO_MODE(GPIO_InitStruct->GPIO_Mode));
  assert_param(IS_GPIO_PUPD(GPIO_InitStruct->GPIO_PuPd));

  /* -------------------------Configure the port pins---------------- */
  /*-- GPIO Mode Configuration --*/
  for (pinpos = 0x00; pinpos < 0x10; pinpos++)
  {
    pos = ((uint32_t)0x01) << pinpos;
    /* Get the port pins position */
    currentpin = (GPIO_InitStruct->GPIO_Pin) & pos;

    if (currentpin == pos)
    {
      GPIOx->MODER  &= ~(GPIO_MODER_MODER0 << (pinpos * 2));
      GPIOx->MODER |= (((uint32_t)GPIO_InitStruct->GPIO_Mode) << (pinpos * 2));

      if ((GPIO_InitStruct->GPIO_Mode == GPIO_Mode_OUT) || (GPIO_InitStruct->GPIO_Mode == GPIO_Mode_AF))
      {
        /* Check Speed mode parameters */
        assert_param(IS_GPIO_SPEED(GPIO_InitStruct->GPIO_Speed));

        /* Speed mode configuration */
        GPIOx->OSPEEDR &= ~(GPIO_OSPEEDER_OSPEEDR0 << (pinpos * 2));
        GPIOx->OSPEEDR |= ((uint32_t)(GPIO_InitStruct->GPIO_Speed) << (pinpos * 2));

        /* Check Output mode parameters */
        assert_param(IS_GPIO_OTYPE(GPIO_InitStruct->GPIO_OType));

        /* Output mode configuration*/
        GPIOx->OTYPER  &= ~((GPIO_OTYPER_OT_0) << ((uint16_t)pinpos)) ;
        GPIOx->OTYPER |= (uint16_t)(((uint16_t)GPIO_InitStruct->GPIO_OType) << ((uint16_t)pinpos));
      }

      /* Pull-up Pull down resistor configuration*/
      GPIOx->PUPDR &= ~(GPIO_PUPDR_PUPDR0 << ((uint16_t)pinpos * 2));
      GPIOx->PUPDR |= (((uint32_t)GPIO_InitStruct->GPIO_PuPd) << (pinpos * 2));
    }
  }
}

#endif /* STM32F4XX_GPIO_INLINE_H_ */
