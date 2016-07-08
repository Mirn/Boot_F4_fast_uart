/*
 * sfu_commands.c
 *
 *  Created on: 08 июля 2016 г.
 *      Author: Easy
 */

#include "stm32kiss.h"
#include "usart_mini.h"
#include "packet_receiver.h"

#define SFU_VER_L 0
#define SFU_VER_H 1

#define SFU_CMD_INFO  0x97
#define SFU_CMD_ERASE 0xC5
#define SFU_CMD_WRITE 0x38

static void sfu_command_info(uint8_t code, uint8_t *body, uint32_t size);
static void sfu_command_erase(uint8_t code, uint8_t *body, uint32_t size);
static void sfu_command_write(uint8_t code, uint8_t *body, uint32_t size);

void sfu_command_timeout()
{

}

void sfu_command_parser(uint8_t code, uint8_t *body, uint32_t size)
{
	if (code == SFU_CMD_INFO)  sfu_command_info(code, body, size);
	if (code == SFU_CMD_ERASE) sfu_command_erase(code, body, size);
	if (code == SFU_CMD_WRITE) sfu_command_write(code, body, size);
}

///////////////////////////////////////

static void sfu_command_info(uint8_t code, uint8_t *body, UNUSED uint32_t size)
{
	const uint32_t CPU_TYPE = DBGMCU->IDCODE;

	uint8_t info[20] = {
			[0 ... 11] = 0,
			(CPU_TYPE >>  0) & 0xFF,
			(CPU_TYPE >>  8) & 0x0F, //7..4 bits reserved
			(CPU_TYPE >> 16) & 0xFF,
			(CPU_TYPE >> 24) & 0xFF,
			FLASH_SIZE_CORRECT_L,
			FLASH_SIZE_CORRECT_H,
			SFU_VER_L,
			SFU_VER_H,
	};

	for (uint32_t index = 0; index < 12; index++)
		info[index] = DEVICE_ID_BLOCK_PTR[index];

	packet_send(code, info, sizeof(info));
}

static void sfu_command_erase(uint8_t code, uint8_t *body, uint32_t size)
{

}

static void sfu_command_write(uint8_t code, uint8_t *body, uint32_t size)
{

}
