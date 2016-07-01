/*
 * stm32f4xx_usart_inline.h
 *
 *  Created on: 30 θώνÿ 2016 γ.
 *      Author: Easy
 */

#ifndef STM32F4XX_USART_INLINE_H_
#define STM32F4XX_USART_INLINE_H_

#include "stm32f4xx_usart.h"
#include "stm32f4xx_rcc.h"
#include "stm32f4xx_rcc_inline.h"

/*!< USART CR1 register clear Mask ((~(uint16_t)0xE9F3)) */
#define CR1_CLEAR_MASK            ((uint16_t)(USART_CR1_M | USART_CR1_PCE | \
                                              USART_CR1_PS | USART_CR1_TE | \
                                              USART_CR1_RE))

/*!< USART CR2 register clock bits clear Mask ((~(uint16_t)0xF0FF)) */
#define CR2_CLOCK_CLEAR_MASK      ((uint16_t)(USART_CR2_CLKEN | USART_CR2_CPOL | \
                                              USART_CR2_CPHA | USART_CR2_LBCL))

/*!< USART CR3 register clear Mask ((~(uint16_t)0xFCFF)) */
#define CR3_CLEAR_MASK            ((uint16_t)(USART_CR3_RTSE | USART_CR3_CTSE))

/*!< USART Interrupts mask */
#define IT_MASK                   ((uint16_t)0x001F)

/**
  * @brief  Deinitializes the USARTx peripheral registers to their default reset values.
  * @param  USARTx: where x can be 1, 2, 3, 4, 5 or 6 to select the USART or
  *         UART peripheral.
  * @retval None
  */
static inline void USART_DeInit_inline(USART_TypeDef* USARTx)
{
  /* Check the parameters */
  assert_param(IS_USART_ALL_PERIPH(USARTx));

  if (USARTx == USART1)
  {
    RCC_APB2PeriphResetCmd_inline(RCC_APB2Periph_USART1, ENABLE);
    RCC_APB2PeriphResetCmd_inline(RCC_APB2Periph_USART1, DISABLE);
  }
  else if (USARTx == USART2)
  {
    RCC_APB1PeriphResetCmd_inline(RCC_APB1Periph_USART2, ENABLE);
    RCC_APB1PeriphResetCmd_inline(RCC_APB1Periph_USART2, DISABLE);
  }
  else if (USARTx == USART3)
  {
    RCC_APB1PeriphResetCmd_inline(RCC_APB1Periph_USART3, ENABLE);
    RCC_APB1PeriphResetCmd_inline(RCC_APB1Periph_USART3, DISABLE);
  }
  else if (USARTx == UART4)
  {
    RCC_APB1PeriphResetCmd_inline(RCC_APB1Periph_UART4, ENABLE);
    RCC_APB1PeriphResetCmd_inline(RCC_APB1Periph_UART4, DISABLE);
  }
  else if (USARTx == UART5)
  {
    RCC_APB1PeriphResetCmd_inline(RCC_APB1Periph_UART5, ENABLE);
    RCC_APB1PeriphResetCmd_inline(RCC_APB1Periph_UART5, DISABLE);
  }
  else
  {
    if (USARTx == USART6)
    {
      RCC_APB2PeriphResetCmd_inline(RCC_APB2Periph_USART6, ENABLE);
      RCC_APB2PeriphResetCmd_inline(RCC_APB2Periph_USART6, DISABLE);
    }
  }
}

/**
  * @brief  Enables or disables the specified USART peripheral.
  * @param  USARTx: where x can be 1, 2, 3, 4, 5 or 6 to select the USART or
  *         UART peripheral.
  * @param  NewState: new state of the USARTx peripheral.
  *          This parameter can be: ENABLE or DISABLE.
  * @retval None
  */
static inline void USART_Cmd_inline(USART_TypeDef* USARTx, FunctionalState NewState)
{
  /* Check the parameters */
  assert_param(IS_USART_ALL_PERIPH(USARTx));
  assert_param(IS_FUNCTIONAL_STATE(NewState));

  if (NewState != DISABLE)
  {
    /* Enable the selected USART by setting the UE bit in the CR1 register */
    USARTx->CR1 |= USART_CR1_UE;
  }
  else
  {
    /* Disable the selected USART by clearing the UE bit in the CR1 register */
    USARTx->CR1 &= (uint16_t)~((uint16_t)USART_CR1_UE);
  }
}

/**
  * @brief  Initializes the USARTx peripheral according to the specified
  *         parameters in the USART_InitStruct .
  * @param  USARTx: where x can be 1, 2, 3, 4, 5 or 6 to select the USART or
  *         UART peripheral.
  * @param  USART_InitStruct: pointer to a USART_InitTypeDef structure that contains
  *         the configuration information for the specified USART peripheral.
  * @retval None
  */
static inline void USART_Init_inline(USART_TypeDef* USARTx, USART_InitTypeDef* USART_InitStruct)
{
  uint32_t tmpreg = 0x00, apbclock = 0x00;
  uint32_t integerdivider = 0x00;
  uint32_t fractionaldivider = 0x00;
  RCC_ClocksTypeDef RCC_ClocksStatus;

  /* Check the parameters */
  assert_param(IS_USART_ALL_PERIPH(USARTx));
  assert_param(IS_USART_BAUDRATE(USART_InitStruct->USART_BaudRate));
  assert_param(IS_USART_WORD_LENGTH(USART_InitStruct->USART_WordLength));
  assert_param(IS_USART_STOPBITS(USART_InitStruct->USART_StopBits));
  assert_param(IS_USART_PARITY(USART_InitStruct->USART_Parity));
  assert_param(IS_USART_MODE(USART_InitStruct->USART_Mode));
  assert_param(IS_USART_HARDWARE_FLOW_CONTROL(USART_InitStruct->USART_HardwareFlowControl));

  /* The hardware flow control is available only for USART1, USART2, USART3 and USART6 */
  if (USART_InitStruct->USART_HardwareFlowControl != USART_HardwareFlowControl_None)
  {
    assert_param(IS_USART_1236_PERIPH(USARTx));
  }

/*---------------------------- USART CR2 Configuration -----------------------*/
  tmpreg = USARTx->CR2;

  /* Clear STOP[13:12] bits */
  tmpreg &= (uint32_t)~((uint32_t)USART_CR2_STOP);

  /* Configure the USART Stop Bits, Clock, CPOL, CPHA and LastBit :
      Set STOP[13:12] bits according to USART_StopBits value */
  tmpreg |= (uint32_t)USART_InitStruct->USART_StopBits;

  /* Write to USART CR2 */
  USARTx->CR2 = (uint16_t)tmpreg;

/*---------------------------- USART CR1 Configuration -----------------------*/
  tmpreg = USARTx->CR1;

  /* Clear M, PCE, PS, TE and RE bits */
  tmpreg &= (uint32_t)~((uint32_t)CR1_CLEAR_MASK);

  /* Configure the USART Word Length, Parity and mode:
     Set the M bits according to USART_WordLength value
     Set PCE and PS bits according to USART_Parity value
     Set TE and RE bits according to USART_Mode value */
  tmpreg |= (uint32_t)USART_InitStruct->USART_WordLength | USART_InitStruct->USART_Parity |
            USART_InitStruct->USART_Mode;

  /* Write to USART CR1 */
  USARTx->CR1 = (uint16_t)tmpreg;

/*---------------------------- USART CR3 Configuration -----------------------*/
  tmpreg = USARTx->CR3;

  /* Clear CTSE and RTSE bits */
  tmpreg &= (uint32_t)~((uint32_t)CR3_CLEAR_MASK);

  /* Configure the USART HFC :
      Set CTSE and RTSE bits according to USART_HardwareFlowControl value */
  tmpreg |= USART_InitStruct->USART_HardwareFlowControl;

  /* Write to USART CR3 */
  USARTx->CR3 = (uint16_t)tmpreg;

/*---------------------------- USART BRR Configuration -----------------------*/
  /* Configure the USART Baud Rate */
  RCC_GetClocksFreq_inline(&RCC_ClocksStatus);

  if ((USARTx == USART1) || (USARTx == USART6))
  {
    apbclock = RCC_ClocksStatus.PCLK2_Frequency;
  }
  else
  {
    apbclock = RCC_ClocksStatus.PCLK1_Frequency;
  }

  /* Determine the integer part */
  if ((USARTx->CR1 & USART_CR1_OVER8) != 0)
  {
    /* Integer part computing in case Oversampling mode is 8 Samples */
    integerdivider = ((25 * apbclock) / (2 * (USART_InitStruct->USART_BaudRate)));
  }
  else /* if ((USARTx->CR1 & USART_CR1_OVER8) == 0) */
  {
    /* Integer part computing in case Oversampling mode is 16 Samples */
    integerdivider = ((25 * apbclock) / (4 * (USART_InitStruct->USART_BaudRate)));
  }
  tmpreg = (integerdivider / 100) << 4;

  /* Determine the fractional part */
  fractionaldivider = integerdivider - (100 * (tmpreg >> 4));

  /* Implement the fractional part in the register */
  if ((USARTx->CR1 & USART_CR1_OVER8) != 0)
  {
    tmpreg |= ((((fractionaldivider * 8) + 50) / 100)) & ((uint8_t)0x07);
  }
  else /* if ((USARTx->CR1 & USART_CR1_OVER8) == 0) */
  {
    tmpreg |= ((((fractionaldivider * 16) + 50) / 100)) & ((uint8_t)0x0F);
  }

  /* Write to USART BRR register */
  USARTx->BRR = (uint16_t)tmpreg;
}

#endif /* STM32F4XX_USART_INLINE_H_ */
