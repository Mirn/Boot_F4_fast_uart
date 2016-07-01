/*
 * stm32f4xx_rcc_inline.h
 *
 *  Created on: 30 θώνÿ 2016 γ.
 *      Author: Easy
 */

#ifndef STM32F4XX_RCC_INLINE_H_
#define STM32F4XX_RCC_INLINE_H_

#include "stm32f4xx_rcc.h"

/** @addtogroup STM32F4xx_StdPeriph_Driver
  * @{
  */

/** @defgroup RCC
  * @brief RCC driver modules
  * @{
  */

/* Private typedef -----------------------------------------------------------*/
/* Private define ------------------------------------------------------------*/
/* ------------ RCC registers bit address in the alias region ----------- */
#define RCC_OFFSET                (RCC_BASE - PERIPH_BASE)
/* --- CR Register ---*/
/* Alias word address of HSION bit */
#define CR_OFFSET                 (RCC_OFFSET + 0x00)
#define HSION_BitNumber           0x00
#define CR_HSION_BB               (PERIPH_BB_BASE + (CR_OFFSET * 32) + (HSION_BitNumber * 4))
/* Alias word address of CSSON bit */
#define CSSON_BitNumber           0x13
#define CR_CSSON_BB               (PERIPH_BB_BASE + (CR_OFFSET * 32) + (CSSON_BitNumber * 4))
/* Alias word address of PLLON bit */
#define PLLON_BitNumber           0x18
#define CR_PLLON_BB               (PERIPH_BB_BASE + (CR_OFFSET * 32) + (PLLON_BitNumber * 4))
/* Alias word address of PLLI2SON bit */
#define PLLI2SON_BitNumber        0x1A
#define CR_PLLI2SON_BB            (PERIPH_BB_BASE + (CR_OFFSET * 32) + (PLLI2SON_BitNumber * 4))

/* --- CFGR Register ---*/
/* Alias word address of I2SSRC bit */
#define CFGR_OFFSET               (RCC_OFFSET + 0x08)
#define I2SSRC_BitNumber          0x17
#define CFGR_I2SSRC_BB            (PERIPH_BB_BASE + (CFGR_OFFSET * 32) + (I2SSRC_BitNumber * 4))

/* --- BDCR Register ---*/
/* Alias word address of RTCEN bit */
#define BDCR_OFFSET               (RCC_OFFSET + 0x70)
#define RTCEN_BitNumber           0x0F
#define BDCR_RTCEN_BB             (PERIPH_BB_BASE + (BDCR_OFFSET * 32) + (RTCEN_BitNumber * 4))
/* Alias word address of BDRST bit */
#define BDRST_BitNumber           0x10
#define BDCR_BDRST_BB             (PERIPH_BB_BASE + (BDCR_OFFSET * 32) + (BDRST_BitNumber * 4))
/* --- CSR Register ---*/
/* Alias word address of LSION bit */
#define CSR_OFFSET                (RCC_OFFSET + 0x74)
#define LSION_BitNumber           0x00
#define CSR_LSION_BB              (PERIPH_BB_BASE + (CSR_OFFSET * 32) + (LSION_BitNumber * 4))
/* ---------------------- RCC registers bit mask ------------------------ */
/* CFGR register bit mask */
#define CFGR_MCO2_RESET_MASK      ((uint32_t)0x07FFFFFF)
#define CFGR_MCO1_RESET_MASK      ((uint32_t)0xF89FFFFF)

/* RCC Flag Mask */
#define FLAG_MASK                 ((uint8_t)0x1F)

/* CR register byte 3 (Bits[23:16]) base address */
#define CR_BYTE3_ADDRESS          ((uint32_t)0x40023802)

/* CIR register byte 2 (Bits[15:8]) base address */
#define CIR_BYTE2_ADDRESS         ((uint32_t)(RCC_BASE + 0x0C + 0x01))

/* CIR register byte 3 (Bits[23:16]) base address */
#define CIR_BYTE3_ADDRESS         ((uint32_t)(RCC_BASE + 0x0C + 0x02))

/* BDCR register base address */
#define BDCR_ADDRESS              (PERIPH_BASE + BDCR_OFFSET)

/* Private macro -------------------------------------------------------------*/
/* Private variables ---------------------------------------------------------*/
//static const uint8_t APBAHBPrescTable[16] = {0, 0, 0, 0, 1, 2, 3, 4, 1, 2, 3, 4, 6, 7, 8, 9};

/**
  * @brief  Enables or disables the AHB1 peripheral clock.
  * @note   After reset, the peripheral clock (used for registers read/write access)
  *         is disabled and the application software has to enable this clock before
  *         using it.
  * @param  RCC_AHBPeriph: specifies the AHB1 peripheral to gates its clock.
  *          This parameter can be any combination of the following values:
  *            @arg RCC_AHB1Periph_GPIOA:       GPIOA clock
  *            @arg RCC_AHB1Periph_GPIOB:       GPIOB clock
  *            @arg RCC_AHB1Periph_GPIOC:       GPIOC clock
  *            @arg RCC_AHB1Periph_GPIOD:       GPIOD clock
  *            @arg RCC_AHB1Periph_GPIOE:       GPIOE clock
  *            @arg RCC_AHB1Periph_GPIOF:       GPIOF clock
  *            @arg RCC_AHB1Periph_GPIOG:       GPIOG clock
  *            @arg RCC_AHB1Periph_GPIOG:       GPIOG clock
  *            @arg RCC_AHB1Periph_GPIOI:       GPIOI clock
  *            @arg RCC_AHB1Periph_CRC:         CRC clock
  *            @arg RCC_AHB1Periph_BKPSRAM:     BKPSRAM interface clock
  *            @arg RCC_AHB1Periph_CCMDATARAMEN CCM data RAM interface clock
  *            @arg RCC_AHB1Periph_DMA1:        DMA1 clock
  *            @arg RCC_AHB1Periph_DMA2:        DMA2 clock
  *            @arg RCC_AHB1Periph_ETH_MAC:     Ethernet MAC clock
  *            @arg RCC_AHB1Periph_ETH_MAC_Tx:  Ethernet Transmission clock
  *            @arg RCC_AHB1Periph_ETH_MAC_Rx:  Ethernet Reception clock
  *            @arg RCC_AHB1Periph_ETH_MAC_PTP: Ethernet PTP clock
  *            @arg RCC_AHB1Periph_OTG_HS:      USB OTG HS clock
  *            @arg RCC_AHB1Periph_OTG_HS_ULPI: USB OTG HS ULPI clock
  * @param  NewState: new state of the specified peripheral clock.
  *          This parameter can be: ENABLE or DISABLE.
  * @retval None
  */
static inline void RCC_AHB1PeriphClockCmd_inline(uint32_t RCC_AHB1Periph, FunctionalState NewState)
{
  /* Check the parameters */
  assert_param(IS_RCC_AHB1_CLOCK_PERIPH(RCC_AHB1Periph));

  assert_param(IS_FUNCTIONAL_STATE(NewState));
  if (NewState != DISABLE)
  {
    RCC->AHB1ENR |= RCC_AHB1Periph;
  }
  else
  {
    RCC->AHB1ENR &= ~RCC_AHB1Periph;
  }
}

/**
  * @brief  Enables or disables the AHB2 peripheral clock.
  * @note   After reset, the peripheral clock (used for registers read/write access)
  *         is disabled and the application software has to enable this clock before
  *         using it.
  * @param  RCC_AHBPeriph: specifies the AHB2 peripheral to gates its clock.
  *          This parameter can be any combination of the following values:
  *            @arg RCC_AHB2Periph_DCMI:   DCMI clock
  *            @arg RCC_AHB2Periph_CRYP:   CRYP clock
  *            @arg RCC_AHB2Periph_HASH:   HASH clock
  *            @arg RCC_AHB2Periph_RNG:    RNG clock
  *            @arg RCC_AHB2Periph_OTG_FS: USB OTG FS clock
  * @param  NewState: new state of the specified peripheral clock.
  *          This parameter can be: ENABLE or DISABLE.
  * @retval None
  */
static inline void RCC_AHB2PeriphClockCmd_inline(uint32_t RCC_AHB2Periph, FunctionalState NewState)
{
  /* Check the parameters */
  assert_param(IS_RCC_AHB2_PERIPH(RCC_AHB2Periph));
  assert_param(IS_FUNCTIONAL_STATE(NewState));

  if (NewState != DISABLE)
  {
    RCC->AHB2ENR |= RCC_AHB2Periph;
  }
  else
  {
    RCC->AHB2ENR &= ~RCC_AHB2Periph;
  }
}

/**
  * @brief  Enables or disables the AHB3 peripheral clock.
  * @note   After reset, the peripheral clock (used for registers read/write access)
  *         is disabled and the application software has to enable this clock before
  *         using it.
  * @param  RCC_AHBPeriph: specifies the AHB3 peripheral to gates its clock.
  *          This parameter must be: RCC_AHB3Periph_FSMC
  * @param  NewState: new state of the specified peripheral clock.
  *          This parameter can be: ENABLE or DISABLE.
  * @retval None
  */
static inline void RCC_AHB3PeriphClockCmd_inline(uint32_t RCC_AHB3Periph, FunctionalState NewState)
{
  /* Check the parameters */
  assert_param(IS_RCC_AHB3_PERIPH(RCC_AHB3Periph));
  assert_param(IS_FUNCTIONAL_STATE(NewState));

  if (NewState != DISABLE)
  {
    RCC->AHB3ENR |= RCC_AHB3Periph;
  }
  else
  {
    RCC->AHB3ENR &= ~RCC_AHB3Periph;
  }
}

/**
  * @brief  Enables or disables the Low Speed APB (APB1) peripheral clock.
  * @note   After reset, the peripheral clock (used for registers read/write access)
  *         is disabled and the application software has to enable this clock before
  *         using it.
  * @param  RCC_APB1Periph: specifies the APB1 peripheral to gates its clock.
  *          This parameter can be any combination of the following values:
  *            @arg RCC_APB1Periph_TIM2:   TIM2 clock
  *            @arg RCC_APB1Periph_TIM3:   TIM3 clock
  *            @arg RCC_APB1Periph_TIM4:   TIM4 clock
  *            @arg RCC_APB1Periph_TIM5:   TIM5 clock
  *            @arg RCC_APB1Periph_TIM6:   TIM6 clock
  *            @arg RCC_APB1Periph_TIM7:   TIM7 clock
  *            @arg RCC_APB1Periph_TIM12:  TIM12 clock
  *            @arg RCC_APB1Periph_TIM13:  TIM13 clock
  *            @arg RCC_APB1Periph_TIM14:  TIM14 clock
  *            @arg RCC_APB1Periph_WWDG:   WWDG clock
  *            @arg RCC_APB1Periph_SPI2:   SPI2 clock
  *            @arg RCC_APB1Periph_SPI3:   SPI3 clock
  *            @arg RCC_APB1Periph_USART2: USART2 clock
  *            @arg RCC_APB1Periph_USART3: USART3 clock
  *            @arg RCC_APB1Periph_UART4:  UART4 clock
  *            @arg RCC_APB1Periph_UART5:  UART5 clock
  *            @arg RCC_APB1Periph_I2C1:   I2C1 clock
  *            @arg RCC_APB1Periph_I2C2:   I2C2 clock
  *            @arg RCC_APB1Periph_I2C3:   I2C3 clock
  *            @arg RCC_APB1Periph_CAN1:   CAN1 clock
  *            @arg RCC_APB1Periph_CAN2:   CAN2 clock
  *            @arg RCC_APB1Periph_PWR:    PWR clock
  *            @arg RCC_APB1Periph_DAC:    DAC clock
  * @param  NewState: new state of the specified peripheral clock.
  *          This parameter can be: ENABLE or DISABLE.
  * @retval None
  */
static inline void RCC_APB1PeriphClockCmd_inline(uint32_t RCC_APB1Periph, FunctionalState NewState)
{
  /* Check the parameters */
  assert_param(IS_RCC_APB1_PERIPH(RCC_APB1Periph));
  assert_param(IS_FUNCTIONAL_STATE(NewState));

  if (NewState != DISABLE)
  {
    RCC->APB1ENR |= RCC_APB1Periph;
  }
  else
  {
    RCC->APB1ENR &= ~RCC_APB1Periph;
  }
}

/**
  * @brief  Enables or disables the High Speed APB (APB2) peripheral clock.
  * @note   After reset, the peripheral clock (used for registers read/write access)
  *         is disabled and the application software has to enable this clock before
  *         using it.
  * @param  RCC_APB2Periph: specifies the APB2 peripheral to gates its clock.
  *          This parameter can be any combination of the following values:
  *            @arg RCC_APB2Periph_TIM1:   TIM1 clock
  *            @arg RCC_APB2Periph_TIM8:   TIM8 clock
  *            @arg RCC_APB2Periph_USART1: USART1 clock
  *            @arg RCC_APB2Periph_USART6: USART6 clock
  *            @arg RCC_APB2Periph_ADC1:   ADC1 clock
  *            @arg RCC_APB2Periph_ADC2:   ADC2 clock
  *            @arg RCC_APB2Periph_ADC3:   ADC3 clock
  *            @arg RCC_APB2Periph_SDIO:   SDIO clock
  *            @arg RCC_APB2Periph_SPI1:   SPI1 clock
  *            @arg RCC_APB2Periph_SYSCFG: SYSCFG clock
  *            @arg RCC_APB2Periph_TIM9:   TIM9 clock
  *            @arg RCC_APB2Periph_TIM10:  TIM10 clock
  *            @arg RCC_APB2Periph_TIM11:  TIM11 clock
  * @param  NewState: new state of the specified peripheral clock.
  *          This parameter can be: ENABLE or DISABLE.
  * @retval None
  */
static inline void RCC_APB2PeriphClockCmd_inline(uint32_t RCC_APB2Periph, FunctionalState NewState)
{
  /* Check the parameters */
  assert_param(IS_RCC_APB2_PERIPH(RCC_APB2Periph));
  assert_param(IS_FUNCTIONAL_STATE(NewState));

  if (NewState != DISABLE)
  {
    RCC->APB2ENR |= RCC_APB2Periph;
  }
  else
  {
    RCC->APB2ENR &= ~RCC_APB2Periph;
  }
}

/**
  * @brief  Forces or releases AHB1 peripheral reset.
  * @param  RCC_AHB1Periph: specifies the AHB1 peripheral to reset.
  *          This parameter can be any combination of the following values:
  *            @arg RCC_AHB1Periph_GPIOA:   GPIOA clock
  *            @arg RCC_AHB1Periph_GPIOB:   GPIOB clock
  *            @arg RCC_AHB1Periph_GPIOC:   GPIOC clock
  *            @arg RCC_AHB1Periph_GPIOD:   GPIOD clock
  *            @arg RCC_AHB1Periph_GPIOE:   GPIOE clock
  *            @arg RCC_AHB1Periph_GPIOF:   GPIOF clock
  *            @arg RCC_AHB1Periph_GPIOG:   GPIOG clock
  *            @arg RCC_AHB1Periph_GPIOG:   GPIOG clock
  *            @arg RCC_AHB1Periph_GPIOI:   GPIOI clock
  *            @arg RCC_AHB1Periph_CRC:     CRC clock
  *            @arg RCC_AHB1Periph_DMA1:    DMA1 clock
  *            @arg RCC_AHB1Periph_DMA2:    DMA2 clock
  *            @arg RCC_AHB1Periph_ETH_MAC: Ethernet MAC clock
  *            @arg RCC_AHB1Periph_OTG_HS:  USB OTG HS clock
  *
  * @param  NewState: new state of the specified peripheral reset.
  *          This parameter can be: ENABLE or DISABLE.
  * @retval None
  */
static inline void RCC_AHB1PeriphResetCmd_inline(uint32_t RCC_AHB1Periph, FunctionalState NewState)
{
  /* Check the parameters */
  assert_param(IS_RCC_AHB1_RESET_PERIPH(RCC_AHB1Periph));
  assert_param(IS_FUNCTIONAL_STATE(NewState));

  if (NewState != DISABLE)
  {
    RCC->AHB1RSTR |= RCC_AHB1Periph;
  }
  else
  {
    RCC->AHB1RSTR &= ~RCC_AHB1Periph;
  }
}

/**
  * @brief  Forces or releases AHB2 peripheral reset.
  * @param  RCC_AHB2Periph: specifies the AHB2 peripheral to reset.
  *          This parameter can be any combination of the following values:
  *            @arg RCC_AHB2Periph_DCMI:   DCMI clock
  *            @arg RCC_AHB2Periph_CRYP:   CRYP clock
  *            @arg RCC_AHB2Periph_HASH:   HASH clock
  *            @arg RCC_AHB2Periph_RNG:    RNG clock
  *            @arg RCC_AHB2Periph_OTG_FS: USB OTG FS clock
  * @param  NewState: new state of the specified peripheral reset.
  *          This parameter can be: ENABLE or DISABLE.
  * @retval None
  */
static inline void RCC_AHB2PeriphResetCmd_inline(uint32_t RCC_AHB2Periph, FunctionalState NewState)
{
  /* Check the parameters */
  assert_param(IS_RCC_AHB2_PERIPH(RCC_AHB2Periph));
  assert_param(IS_FUNCTIONAL_STATE(NewState));

  if (NewState != DISABLE)
  {
    RCC->AHB2RSTR |= RCC_AHB2Periph;
  }
  else
  {
    RCC->AHB2RSTR &= ~RCC_AHB2Periph;
  }
}

/**
  * @brief  Forces or releases AHB3 peripheral reset.
  * @param  RCC_AHB3Periph: specifies the AHB3 peripheral to reset.
  *          This parameter must be: RCC_AHB3Periph_FSMC
  * @param  NewState: new state of the specified peripheral reset.
  *          This parameter can be: ENABLE or DISABLE.
  * @retval None
  */
static inline void RCC_AHB3PeriphResetCmd_inline(uint32_t RCC_AHB3Periph, FunctionalState NewState)
{
  /* Check the parameters */
  assert_param(IS_RCC_AHB3_PERIPH(RCC_AHB3Periph));
  assert_param(IS_FUNCTIONAL_STATE(NewState));

  if (NewState != DISABLE)
  {
    RCC->AHB3RSTR |= RCC_AHB3Periph;
  }
  else
  {
    RCC->AHB3RSTR &= ~RCC_AHB3Periph;
  }
}

/**
  * @brief  Forces or releases Low Speed APB (APB1) peripheral reset.
  * @param  RCC_APB1Periph: specifies the APB1 peripheral to reset.
  *          This parameter can be any combination of the following values:
  *            @arg RCC_APB1Periph_TIM2:   TIM2 clock
  *            @arg RCC_APB1Periph_TIM3:   TIM3 clock
  *            @arg RCC_APB1Periph_TIM4:   TIM4 clock
  *            @arg RCC_APB1Periph_TIM5:   TIM5 clock
  *            @arg RCC_APB1Periph_TIM6:   TIM6 clock
  *            @arg RCC_APB1Periph_TIM7:   TIM7 clock
  *            @arg RCC_APB1Periph_TIM12:  TIM12 clock
  *            @arg RCC_APB1Periph_TIM13:  TIM13 clock
  *            @arg RCC_APB1Periph_TIM14:  TIM14 clock
  *            @arg RCC_APB1Periph_WWDG:   WWDG clock
  *            @arg RCC_APB1Periph_SPI2:   SPI2 clock
  *            @arg RCC_APB1Periph_SPI3:   SPI3 clock
  *            @arg RCC_APB1Periph_USART2: USART2 clock
  *            @arg RCC_APB1Periph_USART3: USART3 clock
  *            @arg RCC_APB1Periph_UART4:  UART4 clock
  *            @arg RCC_APB1Periph_UART5:  UART5 clock
  *            @arg RCC_APB1Periph_I2C1:   I2C1 clock
  *            @arg RCC_APB1Periph_I2C2:   I2C2 clock
  *            @arg RCC_APB1Periph_I2C3:   I2C3 clock
  *            @arg RCC_APB1Periph_CAN1:   CAN1 clock
  *            @arg RCC_APB1Periph_CAN2:   CAN2 clock
  *            @arg RCC_APB1Periph_PWR:    PWR clock
  *            @arg RCC_APB1Periph_DAC:    DAC clock
  * @param  NewState: new state of the specified peripheral reset.
  *          This parameter can be: ENABLE or DISABLE.
  * @retval None
  */
static inline void RCC_APB1PeriphResetCmd_inline(uint32_t RCC_APB1Periph, FunctionalState NewState)
{
  /* Check the parameters */
  assert_param(IS_RCC_APB1_PERIPH(RCC_APB1Periph));
  assert_param(IS_FUNCTIONAL_STATE(NewState));
  if (NewState != DISABLE)
  {
    RCC->APB1RSTR |= RCC_APB1Periph;
  }
  else
  {
    RCC->APB1RSTR &= ~RCC_APB1Periph;
  }
}

/**
  * @brief  Forces or releases High Speed APB (APB2) peripheral reset.
  * @param  RCC_APB2Periph: specifies the APB2 peripheral to reset.
  *          This parameter can be any combination of the following values:
  *            @arg RCC_APB2Periph_TIM1:   TIM1 clock
  *            @arg RCC_APB2Periph_TIM8:   TIM8 clock
  *            @arg RCC_APB2Periph_USART1: USART1 clock
  *            @arg RCC_APB2Periph_USART6: USART6 clock
  *            @arg RCC_APB2Periph_ADC1:   ADC1 clock
  *            @arg RCC_APB2Periph_ADC2:   ADC2 clock
  *            @arg RCC_APB2Periph_ADC3:   ADC3 clock
  *            @arg RCC_APB2Periph_SDIO:   SDIO clock
  *            @arg RCC_APB2Periph_SPI1:   SPI1 clock
  *            @arg RCC_APB2Periph_SYSCFG: SYSCFG clock
  *            @arg RCC_APB2Periph_TIM9:   TIM9 clock
  *            @arg RCC_APB2Periph_TIM10:  TIM10 clock
  *            @arg RCC_APB2Periph_TIM11:  TIM11 clock
  * @param  NewState: new state of the specified peripheral reset.
  *          This parameter can be: ENABLE or DISABLE.
  * @retval None
  */
static inline void RCC_APB2PeriphResetCmd_inline(uint32_t RCC_APB2Periph, FunctionalState NewState)
{
  /* Check the parameters */
  assert_param(IS_RCC_APB2_RESET_PERIPH(RCC_APB2Periph));
  assert_param(IS_FUNCTIONAL_STATE(NewState));
  if (NewState != DISABLE)
  {
    RCC->APB2RSTR |= RCC_APB2Periph;
  }
  else
  {
    RCC->APB2RSTR &= ~RCC_APB2Periph;
  }
}

static const uint8_t APBAHBPrescTable[16] = {0, 0, 0, 0, 1, 2, 3, 4, 1, 2, 3, 4, 6, 7, 8, 9};
/**
  * @brief  Returns the frequencies of different on chip clocks; SYSCLK, HCLK,
  *         PCLK1 and PCLK2.
  *
  * @note   The system frequency computed by this function is not the real
  *         frequency in the chip. It is calculated based on the predefined
  *         constant and the selected clock source:
  * @note     If SYSCLK source is HSI, function returns values based on HSI_VALUE(*)
  * @note     If SYSCLK source is HSE, function returns values based on HSE_VALUE(**)
  * @note     If SYSCLK source is PLL, function returns values based on HSE_VALUE(**)
  *           or HSI_VALUE(*) multiplied/divided by the PLL factors.
  * @note     (*) HSI_VALUE is a constant defined in stm32f4xx.h file (default value
  *               16 MHz) but the real value may vary depending on the variations
  *               in voltage and temperature.
  * @note     (**) HSE_VALUE is a constant defined in stm32f4xx.h file (default value
  *                25 MHz), user has to ensure that HSE_VALUE is same as the real
  *                frequency of the crystal used. Otherwise, this function may
  *                have wrong result.
  *
  * @note   The result of this function could be not correct when using fractional
  *         value for HSE crystal.
  *
  * @param  RCC_Clocks: pointer to a RCC_ClocksTypeDef structure which will hold
  *          the clocks frequencies.
  *
  * @note   This function can be used by the user application to compute the
  *         baudrate for the communication peripherals or configure other parameters.
  * @note   Each time SYSCLK, HCLK, PCLK1 and/or PCLK2 clock changes, this function
  *         must be called to update the structure's field. Otherwise, any
  *         configuration based on this function will be incorrect.
  *
  * @retval None
  */
static inline void RCC_GetClocksFreq_inline(RCC_ClocksTypeDef* RCC_Clocks)
{
  uint32_t tmp = 0, presc = 0, pllvco = 0, pllp = 2, pllsource = 0, pllm = 2;

  /* Get SYSCLK source -------------------------------------------------------*/
  tmp = RCC->CFGR & RCC_CFGR_SWS;

  switch (tmp)
  {
    case 0x00:  /* HSI used as system clock source */
      RCC_Clocks->SYSCLK_Frequency = HSI_VALUE;
      break;
    case 0x04:  /* HSE used as system clock  source */
      RCC_Clocks->SYSCLK_Frequency = HSE_VALUE;
      break;
    case 0x08:  /* PLL used as system clock  source */

      /* PLL_VCO = (HSE_VALUE or HSI_VALUE / PLLM) * PLLN
         SYSCLK = PLL_VCO / PLLP
         */
      pllsource = (RCC->PLLCFGR & RCC_PLLCFGR_PLLSRC) >> 22;
      pllm = RCC->PLLCFGR & RCC_PLLCFGR_PLLM;

      if (pllsource != 0)
      {
        /* HSE used as PLL clock source */
        pllvco = (HSE_VALUE / pllm) * ((RCC->PLLCFGR & RCC_PLLCFGR_PLLN) >> 6);
      }
      else
      {
        /* HSI used as PLL clock source */
        pllvco = (HSI_VALUE / pllm) * ((RCC->PLLCFGR & RCC_PLLCFGR_PLLN) >> 6);
      }

      pllp = (((RCC->PLLCFGR & RCC_PLLCFGR_PLLP) >>16) + 1 ) *2;
      RCC_Clocks->SYSCLK_Frequency = pllvco/pllp;
      break;
    default:
      RCC_Clocks->SYSCLK_Frequency = HSI_VALUE;
      break;
  }
  /* Compute HCLK, PCLK1 and PCLK2 clocks frequencies ------------------------*/

  /* Get HCLK prescaler */
  tmp = RCC->CFGR & RCC_CFGR_HPRE;
  tmp = tmp >> 4;
  presc = APBAHBPrescTable[tmp];
  /* HCLK clock frequency */
  RCC_Clocks->HCLK_Frequency = RCC_Clocks->SYSCLK_Frequency >> presc;

  /* Get PCLK1 prescaler */
  tmp = RCC->CFGR & RCC_CFGR_PPRE1;
  tmp = tmp >> 10;
  presc = APBAHBPrescTable[tmp];
  /* PCLK1 clock frequency */
  RCC_Clocks->PCLK1_Frequency = RCC_Clocks->HCLK_Frequency >> presc;

  /* Get PCLK2 prescaler */
  tmp = RCC->CFGR & RCC_CFGR_PPRE2;
  tmp = tmp >> 13;
  presc = APBAHBPrescTable[tmp];
  /* PCLK2 clock frequency */
  RCC_Clocks->PCLK2_Frequency = RCC_Clocks->HCLK_Frequency >> presc;
}

#endif /* STM32F4XX_RCC_INLINE_H_ */
