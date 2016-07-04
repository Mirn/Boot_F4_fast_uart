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

/** @defgroup USART_Group9 Interrupts and flags management functions
 *  @brief   Interrupts and flags management functions
 *
@verbatim
 ===============================================================================
                   Interrupts and flags management functions
 ===============================================================================

  This subsection provides a set of functions allowing to configure the USART
  Interrupts sources, DMA channels requests and check or clear the flags or
  pending bits status.
  The user should identify which mode will be used in his application to manage
  the communication: Polling mode, Interrupt mode or DMA mode.

  Polling Mode
  =============
  In Polling Mode, the SPI communication can be managed by 10 flags:
     1. USART_FLAG_TXE : to indicate the status of the transmit buffer register
     2. USART_FLAG_RXNE : to indicate the status of the receive buffer register
     3. USART_FLAG_TC : to indicate the status of the transmit operation
     4. USART_FLAG_IDLE : to indicate the status of the Idle Line
     5. USART_FLAG_CTS : to indicate the status of the nCTS input
     6. USART_FLAG_LBD : to indicate the status of the LIN break detection
     7. USART_FLAG_NE : to indicate if a noise error occur
     8. USART_FLAG_FE : to indicate if a frame error occur
     9. USART_FLAG_PE : to indicate if a parity error occur
     10. USART_FLAG_ORE : to indicate if an Overrun error occur

  In this Mode it is advised to use the following functions:
      - FlagStatus USART_GetFlagStatus(USART_TypeDef* USARTx, uint16_t USART_FLAG);
      - void USART_ClearFlag(USART_TypeDef* USARTx, uint16_t USART_FLAG);

  Interrupt Mode
  ===============
  In Interrupt Mode, the USART communication can be managed by 8 interrupt sources
  and 10 pending bits:

  Pending Bits:
  -------------
     1. USART_IT_TXE : to indicate the status of the transmit buffer register
     2. USART_IT_RXNE : to indicate the status of the receive buffer register
     3. USART_IT_TC : to indicate the status of the transmit operation
     4. USART_IT_IDLE : to indicate the status of the Idle Line
     5. USART_IT_CTS : to indicate the status of the nCTS input
     6. USART_IT_LBD : to indicate the status of the LIN break detection
     7. USART_IT_NE : to indicate if a noise error occur
     8. USART_IT_FE : to indicate if a frame error occur
     9. USART_IT_PE : to indicate if a parity error occur
     10. USART_IT_ORE : to indicate if an Overrun error occur

  Interrupt Source:
  -----------------
     1. USART_IT_TXE : specifies the interrupt source for the Tx buffer empty
                       interrupt.
     2. USART_IT_RXNE : specifies the interrupt source for the Rx buffer not
                        empty interrupt.
     3. USART_IT_TC : specifies the interrupt source for the Transmit complete
                       interrupt.
     4. USART_IT_IDLE : specifies the interrupt source for the Idle Line interrupt.
     5. USART_IT_CTS : specifies the interrupt source for the CTS interrupt.
     6. USART_IT_LBD : specifies the interrupt source for the LIN break detection
                       interrupt.
     7. USART_IT_PE : specifies the interrupt source for the parity error interrupt.
     8. USART_IT_ERR :  specifies the interrupt source for the errors interrupt.

@note Some parameters are coded in order to use them as interrupt source or as pending bits.

  In this Mode it is advised to use the following functions:
     - void USART_ITConfig(USART_TypeDef* USARTx, uint16_t USART_IT, FunctionalState NewState);
     - ITStatus USART_GetITStatus(USART_TypeDef* USARTx, uint16_t USART_IT);
     - void USART_ClearITPendingBit(USART_TypeDef* USARTx, uint16_t USART_IT);

  DMA Mode
  ========
  In DMA Mode, the USART communication can be managed by 2 DMA Channel requests:
     1. USART_DMAReq_Tx: specifies the Tx buffer DMA transfer request
     2. USART_DMAReq_Rx: specifies the Rx buffer DMA transfer request

  In this Mode it is advised to use the following function:
     - void USART_DMACmd(USART_TypeDef* USARTx, uint16_t USART_DMAReq, FunctionalState NewState);

@endverbatim
  * @{
  */

/**
  * @brief  Enables or disables the specified USART interrupts.
  * @param  USARTx: where x can be 1, 2, 3, 4, 5 or 6 to select the USART or
  *         UART peripheral.
  * @param  USART_IT: specifies the USART interrupt sources to be enabled or disabled.
  *          This parameter can be one of the following values:
  *            @arg USART_IT_CTS:  CTS change interrupt
  *            @arg USART_IT_LBD:  LIN Break detection interrupt
  *            @arg USART_IT_TXE:  Transmit Data Register empty interrupt
  *            @arg USART_IT_TC:   Transmission complete interrupt
  *            @arg USART_IT_RXNE: Receive Data register not empty interrupt
  *            @arg USART_IT_IDLE: Idle line detection interrupt
  *            @arg USART_IT_PE:   Parity Error interrupt
  *            @arg USART_IT_ERR:  Error interrupt(Frame error, noise error, overrun error)
  * @param  NewState: new state of the specified USARTx interrupts.
  *          This parameter can be: ENABLE or DISABLE.
  * @retval None
  */
static inline void USART_ITConfig_inline(USART_TypeDef* USARTx, uint16_t USART_IT, FunctionalState NewState)
{
  uint32_t usartreg = 0x00, itpos = 0x00, itmask = 0x00;
  uint32_t usartxbase = 0x00;
  /* Check the parameters */
  assert_param(IS_USART_ALL_PERIPH(USARTx));
  assert_param(IS_USART_CONFIG_IT(USART_IT));
  assert_param(IS_FUNCTIONAL_STATE(NewState));

  /* The CTS interrupt is not available for UART4 and UART5 */
  if (USART_IT == USART_IT_CTS)
  {
    assert_param(IS_USART_1236_PERIPH(USARTx));
  }

  usartxbase = (uint32_t)USARTx;

  /* Get the USART register index */
  usartreg = (((uint8_t)USART_IT) >> 0x05);

  /* Get the interrupt position */
  itpos = USART_IT & IT_MASK;
  itmask = (((uint32_t)0x01) << itpos);

  if (usartreg == 0x01) /* The IT is in CR1 register */
  {
    usartxbase += 0x0C;
  }
  else if (usartreg == 0x02) /* The IT is in CR2 register */
  {
    usartxbase += 0x10;
  }
  else /* The IT is in CR3 register */
  {
    usartxbase += 0x14;
  }
  if (NewState != DISABLE)
  {
    *(__IO uint32_t*)usartxbase  |= itmask;
  }
  else
  {
    *(__IO uint32_t*)usartxbase &= ~itmask;
  }
}

/**
  * @brief  Checks whether the specified USART interrupt has occurred or not.
  * @param  USARTx: where x can be 1, 2, 3, 4, 5 or 6 to select the USART or
  *         UART peripheral.
  * @param  USART_IT: specifies the USART interrupt source to check.
  *          This parameter can be one of the following values:
  *            @arg USART_IT_CTS:  CTS change interrupt (not available for UART4 and UART5)
  *            @arg USART_IT_LBD:  LIN Break detection interrupt
  *            @arg USART_IT_TXE:  Transmit Data Register empty interrupt
  *            @arg USART_IT_TC:   Transmission complete interrupt
  *            @arg USART_IT_RXNE: Receive Data register not empty interrupt
  *            @arg USART_IT_IDLE: Idle line detection interrupt
  *            @arg USART_IT_ORE_RX : OverRun Error interrupt if the RXNEIE bit is set
  *            @arg USART_IT_ORE_ER : OverRun Error interrupt if the EIE bit is set
  *            @arg USART_IT_NE:   Noise Error interrupt
  *            @arg USART_IT_FE:   Framing Error interrupt
  *            @arg USART_IT_PE:   Parity Error interrupt
  * @retval The new state of USART_IT (SET or RESET).
  */
static inline ITStatus USART_GetITStatus_inline(USART_TypeDef* USARTx, uint16_t USART_IT)
{
  uint32_t bitpos = 0x00, itmask = 0x00, usartreg = 0x00;
  ITStatus bitstatus = RESET;
  /* Check the parameters */
  assert_param(IS_USART_ALL_PERIPH(USARTx));
  assert_param(IS_USART_GET_IT(USART_IT));

  /* The CTS interrupt is not available for UART4 and UART5 */
  if (USART_IT == USART_IT_CTS)
  {
    assert_param(IS_USART_1236_PERIPH(USARTx));
  }

  /* Get the USART register index */
  usartreg = (((uint8_t)USART_IT) >> 0x05);
  /* Get the interrupt position */
  itmask = USART_IT & IT_MASK;
  itmask = (uint32_t)0x01 << itmask;

  if (usartreg == 0x01) /* The IT  is in CR1 register */
  {
    itmask &= USARTx->CR1;
  }
  else if (usartreg == 0x02) /* The IT  is in CR2 register */
  {
    itmask &= USARTx->CR2;
  }
  else /* The IT  is in CR3 register */
  {
    itmask &= USARTx->CR3;
  }

  bitpos = USART_IT >> 0x08;
  bitpos = (uint32_t)0x01 << bitpos;
  bitpos &= USARTx->SR;
  if ((itmask != (uint16_t)RESET)&&(bitpos != (uint16_t)RESET))
  {
    bitstatus = SET;
  }
  else
  {
    bitstatus = RESET;
  }

  return bitstatus;
}

/**
  * @brief  Returns the most recent received data by the USARTx peripheral.
  * @param  USARTx: where x can be 1, 2, 3, 4, 5 or 6 to select the USART or
  *         UART peripheral.
  * @retval The received data.
  */
static inline uint16_t USART_ReceiveData_inline(USART_TypeDef* USARTx)
{
  /* Check the parameters */
  assert_param(IS_USART_ALL_PERIPH(USARTx));

  /* Receive Data */
  return (uint16_t)(USARTx->DR & (uint16_t)0x01FF);
}


#endif /* STM32F4XX_USART_INLINE_H_ */
