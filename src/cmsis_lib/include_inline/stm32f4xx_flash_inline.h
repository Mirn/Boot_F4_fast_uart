/*
 * stm32f4xx_flash_inline.h
 *
 *  Created on: 20 июля 2016 г.
 *      Author: Easy
 */

#ifndef STM32F4XX_FLASH_INLINE_H_
#define STM32F4XX_FLASH_INLINE_H_

#include "stm32f4xx_flash.h"

/* Private typedef -----------------------------------------------------------*/
/* Private define ------------------------------------------------------------*/
#define SECTOR_MASK               ((uint32_t)0xFFFFFF07)

/**
  * @brief  Unlocks the FLASH control register access
  * @param  None
  * @retval None
  */
static inline void FLASH_Unlock_inline(void)
{
  if((FLASH->CR & FLASH_CR_LOCK) != RESET)
  {
    /* Authorize the FLASH Registers access */
    FLASH->KEYR = FLASH_KEY1;
    FLASH->KEYR = FLASH_KEY2;
  }
}

/**
  * @brief  Locks the FLASH control register access
  * @param  None
  * @retval None
  */
static inline void FLASH_Lock_inline(void)
{
  /* Set the LOCK Bit to lock the FLASH Registers access */
  FLASH->CR |= FLASH_CR_LOCK;
}

/**
  * @brief  Clears the FLASH's pending flags.
  * @param  FLASH_FLAG: specifies the FLASH flags to clear.
  *          This parameter can be any combination of the following values:
  *            @arg FLASH_FLAG_EOP: FLASH End of Operation flag
  *            @arg FLASH_FLAG_OPERR: FLASH operation Error flag
  *            @arg FLASH_FLAG_WRPERR: FLASH Write protected error flag
  *            @arg FLASH_FLAG_PGAERR: FLASH Programming Alignment error flag
  *            @arg FLASH_FLAG_PGPERR: FLASH Programming Parallelism error flag
  *            @arg FLASH_FLAG_PGSERR: FLASH Programming Sequence error flag
  * @retval None
  */
static inline void FLASH_ClearFlag_inline(uint32_t FLASH_FLAG)
{
  /* Check the parameters */
  assert_param(IS_FLASH_CLEAR_FLAG(FLASH_FLAG));

  /* Clear the flags */
  FLASH->SR = FLASH_FLAG;
}

/**
  * @brief  Returns the FLASH Status.
  * @param  None
  * @retval FLASH Status: The returned value can be: FLASH_BUSY, FLASH_ERROR_PROGRAM,
  *                       FLASH_ERROR_WRP, FLASH_ERROR_OPERATION or FLASH_COMPLETE.
  */
//__attribute__((always_inline))
__attribute__ ((section(".data")))
static inline FLASH_Status FLASH_GetStatus_inline(void)
{
  FLASH_Status flashstatus = FLASH_COMPLETE;

  uint32_t FLASH_SR_reg = FLASH->SR;

  if((FLASH_SR_reg & FLASH_FLAG_BSY) != (uint32_t)0x00) // ==FLASH_FLAG_BSY)
  {
    flashstatus = FLASH_BUSY;
  }
  else
  {
    if((FLASH_SR_reg & FLASH_FLAG_WRPERR) != (uint32_t)0x00)
    {
      flashstatus = FLASH_ERROR_WRP;
    }
    else
    {
      if((FLASH_SR_reg & (uint32_t)0xEF) != (uint32_t)0x00)
      {
        flashstatus = FLASH_ERROR_PROGRAM;
      }
      else
      {
        if((FLASH_SR_reg & FLASH_FLAG_OPERR) != (uint32_t)0x00)
        {
          flashstatus = FLASH_ERROR_OPERATION;
        }
        else
        {
          flashstatus = FLASH_COMPLETE;
        }
      }
    }
  }
  /* Return the FLASH Status */
  return flashstatus;
}

/**
  * @brief  Waits for a FLASH operation to complete.
  * @param  None
  * @retval FLASH Status: The returned value can be: FLASH_BUSY, FLASH_ERROR_PROGRAM,
  *                       FLASH_ERROR_WRP, FLASH_ERROR_OPERATION or FLASH_COMPLETE.
  */
//__attribute__((always_inline))
__attribute__ ((section(".data")))
static inline FLASH_Status FLASH_WaitForLastOperation_inline(void)
{
  __IO FLASH_Status status = FLASH_COMPLETE;

  /* Check for the FLASH Status */
  status = FLASH_GetStatus_inline();

  /* Wait for the FLASH operation to complete by polling on BUSY flag to be reset.
     Even if the FLASH operation fails, the BUSY flag will be reset and an error
     flag will be set */
  while(status == FLASH_BUSY)
  {
    status = FLASH_GetStatus_inline();
  }
  /* Return the operation status */
  return status;
}

/**
  * @brief  Programs a word (32-bit) at a specified address.
  * @param  Address: specifies the address to be programmed.
  *         This parameter can be any address in Program memory zone or in OTP zone.
  * @note   This function must be used when the device voltage range is from 2.7V to 3.6V.
  * @param  Data: specifies the data to be programmed.
  * @retval FLASH Status: The returned value can be: FLASH_BUSY, FLASH_ERROR_PROGRAM,
  *                       FLASH_ERROR_WRP, FLASH_ERROR_OPERATION or FLASH_COMPLETE.
  */
static inline FLASH_Status FLASH_ProgramWord_inline(uint32_t Address, uint32_t Data)
{
  FLASH_Status status = FLASH_COMPLETE;

  /* Check the parameters */
  assert_param(IS_FLASH_ADDRESS(Address));

  /* Wait for last operation to be completed */
  status = FLASH_WaitForLastOperation_inline();

  if(status == FLASH_COMPLETE)
  {
    /* if the previous operation is completed, proceed to program the new data */
    FLASH->CR &= CR_PSIZE_MASK;
    FLASH->CR |= FLASH_PSIZE_WORD;
    FLASH->CR |= FLASH_CR_PG;

    *(__IO uint32_t*)Address = Data;

    /* Wait for last operation to be completed */
    status = FLASH_WaitForLastOperation_inline();

    /* if the program operation is completed, disable the PG Bit */
    FLASH->CR &= (~FLASH_CR_PG);
  }
  /* Return the Program Status */
  return status;
}

/**
  * @brief  Erases a specified FLASH Sector.
  *
  * @param  FLASH_Sector: The Sector number to be erased.
  *          This parameter can be a value between FLASH_Sector_0 and FLASH_Sector_11
  *
  * @param  VoltageRange: The device voltage range which defines the erase parallelism.
  *          This parameter can be one of the following values:
  *            @arg VoltageRange_1: when the device voltage range is 1.8V to 2.1V,
  *                                  the operation will be done by byte (8-bit)
  *            @arg VoltageRange_2: when the device voltage range is 2.1V to 2.7V,
  *                                  the operation will be done by half word (16-bit)
  *            @arg VoltageRange_3: when the device voltage range is 2.7V to 3.6V,
  *                                  the operation will be done by word (32-bit)
  *            @arg VoltageRange_4: when the device voltage range is 2.7V to 3.6V + External Vpp,
  *                                  the operation will be done by double word (64-bit)
  *
  * @retval FLASH Status: The returned value can be: FLASH_BUSY, FLASH_ERROR_PROGRAM,
  *                       FLASH_ERROR_WRP, FLASH_ERROR_OPERATION or FLASH_COMPLETE.
  */
static inline FLASH_Status FLASH_EraseSector_inline(uint32_t FLASH_Sector, uint8_t VoltageRange)
{
  uint32_t tmp_psize = 0x0;
  FLASH_Status status = FLASH_COMPLETE;

  /* Check the parameters */
  assert_param(IS_FLASH_SECTOR(FLASH_Sector));
  assert_param(IS_VOLTAGERANGE(VoltageRange));

  if(VoltageRange == VoltageRange_1)
  {
     tmp_psize = FLASH_PSIZE_BYTE;
  }
  else if(VoltageRange == VoltageRange_2)
  {
    tmp_psize = FLASH_PSIZE_HALF_WORD;
  }
  else if(VoltageRange == VoltageRange_3)
  {
    tmp_psize = FLASH_PSIZE_WORD;
  }
  else
  {
    tmp_psize = FLASH_PSIZE_DOUBLE_WORD;
  }
  /* Wait for last operation to be completed */
  status = FLASH_WaitForLastOperation_inline();

  if(status == FLASH_COMPLETE)
  {
    /* if the previous operation is completed, proceed to erase the sector */
    FLASH->CR &= CR_PSIZE_MASK;
    FLASH->CR |= tmp_psize;
    FLASH->CR &= SECTOR_MASK;
    FLASH->CR |= FLASH_CR_SER | FLASH_Sector;
    FLASH->CR |= FLASH_CR_STRT;

    /* Wait for last operation to be completed */
    status = FLASH_WaitForLastOperation_inline();

    /* if the erase operation is completed, disable the SER Bit */
    FLASH->CR &= (~FLASH_CR_SER);
    FLASH->CR &= SECTOR_MASK;
  }
  /* Return the Erase Status */
  return status;
}

#endif /* STM32F4XX_FLASH_INLINE_H_ */
