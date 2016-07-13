/*
 * sfu_commands.c
 *
 *  Created on: 08 июля 2016 г.
 *      Author: Easy
 */

#include "stm32kiss.h"
#include "stm32f4xx_flash.h"

#include "usart_mini.h"
#include "packet_receiver.h"

#define SFU_VER_L 0
#define SFU_VER_H 1

#define SFU_CMD_ERASE_PART   0xB3
#define SFU_CMD_INFO    0x97
#define SFU_CMD_ERASE   0xC5
#define SFU_CMD_WRITE   0x38
#define SFU_CMD_TIMEOUT 0xAA
#define SFU_CMD_ERROR   0x55
#define SFU_CMD_HWRESET 0x11

static void sfu_command_info(uint8_t code, uint8_t *body, uint32_t size);
static void sfu_command_erase(uint8_t code, uint8_t *body, uint32_t size);
static void sfu_command_write(uint8_t code, uint8_t *body, uint32_t size);

static uint32_t write_addr = 0;

void sfu_command_init()
{
	uint32_t temp;
	packet_send(SFU_CMD_HWRESET, (void*)&temp, 0);
}

void sfu_command_timeout()
{
	if (write_addr == 0) return;
	write_addr = 0;
	packet_send(SFU_CMD_TIMEOUT, (uint8_t*)&write_addr, sizeof(write_addr));
}

void sfu_command_parser(uint8_t code, uint8_t *body, uint32_t size)
{
	if (code == SFU_CMD_INFO)  sfu_command_info(code, body, size);
	if (code == SFU_CMD_ERASE) sfu_command_erase(code, body, size);
	if (code == SFU_CMD_WRITE) sfu_command_write(code, body, size);
}

///////////////////////////////////////

static void serialize_uint32(uint8_t *body, uint32_t value)
{
	body[0] = (value >>  0) & 0xFF;
	body[1] = (value >>  8) & 0xFF;
	body[2] = (value >> 16) & 0xFF;
	body[3] = (value >> 24) & 0xFF;
}

static uint32_t deserialize_uint32(uint8_t *body)
{
	return  ((uint32_t)body[0] <<  0) |
			((uint32_t)body[1] <<  8) |
			((uint32_t)body[2] << 16) |
			((uint32_t)body[3] << 24);
}

///////////////////////////////////////

static void sfu_command_info(uint8_t code, UNUSED uint8_t *body, UNUSED uint32_t size)
{
	const uint32_t CPU_TYPE = DBGMCU->IDCODE;

	uint8_t info[28] = {
			[0 ... 11] = 0,
			(CPU_TYPE >>  0) & 0xFF,
			(CPU_TYPE >>  8) & 0x0F, //7..4 bits reserved
			(CPU_TYPE >> 16) & 0xFF,
			(CPU_TYPE >> 24) & 0xFF,

			FLASH_SIZE_CORRECT_L,
			FLASH_SIZE_CORRECT_H,

			SFU_VER_L,
			SFU_VER_H,

			(MAIN_START_FROM >>  0) & 0xFF,
			(MAIN_START_FROM >>  8) & 0xFF,
			(MAIN_START_FROM >> 16) & 0xFF,
			(MAIN_START_FROM >> 24) & 0xFF,

			(MAIN_RUN_FROM >>  0) & 0xFF,
			(MAIN_RUN_FROM >>  8) & 0xFF,
			(MAIN_RUN_FROM >> 16) & 0xFF,
			(MAIN_RUN_FROM >> 24) & 0xFF,
	};

	for (uint32_t index = 0; index < 12; index++)
		info[index] = DEVICE_ID_BLOCK_PTR[index];

	packet_send(code, info, sizeof(info));
}

static void sfu_command_erase(uint8_t code, uint8_t *body, uint32_t size)
{
	if (size != 4) return;

	if ((body[0] == 0xFF) && (body[1] == 0xFF) && (body[2] == 0xFF) && (body[3] == 0xFF))
	{
		const uint16_t sectors[] = {
				FLASH_Sector_1,
				FLASH_Sector_2,
				FLASH_Sector_3,
				FLASH_Sector_4,
				FLASH_Sector_5,
				FLASH_Sector_6,
				FLASH_Sector_7,
				FLASH_Sector_8,
				FLASH_Sector_9,
				FLASH_Sector_10,
				FLASH_Sector_11,
		};

		FLASH_Status status = FLASH_BUSY;
		FLASH_Unlock();
		FLASH_ClearFlag(FLASH_FLAG_EOP | FLASH_FLAG_OPERR | FLASH_FLAG_WRPERR |
	                    FLASH_FLAG_PGAERR | FLASH_FLAG_PGPERR | FLASH_FLAG_PGSERR);

		for (uint32_t pos = 0; pos < LENGTH(sectors); pos++)
		{
			status = FLASH_EraseSector(sectors[pos], VoltageRange_3);
			packet_send(SFU_CMD_ERASE_PART, (uint8_t *)&pos, sizeof(pos));

			if (status != FLASH_COMPLETE) break;
			if ((FLASH_SIZE == 512) && (sectors[pos] == FLASH_Sector_8)) break;
		}
		FLASH_Lock();

		if (status == FLASH_COMPLETE)
		{
			write_addr = MAIN_START_FROM;
			packet_send(code, body, size);
			return;
		}
	}

	packet_send(SFU_CMD_ERROR, body, 0);
}

static void sfu_command_write(uint8_t code, uint8_t *body, uint32_t size)
{
	if (size > 4)
	{
		uint32_t body_addr = deserialize_uint32(body);

		if ((write_addr & 0xFF000000) != FLASH_BASE)
		{
			packet_send(SFU_CMD_ERROR, body, 0);
			return;
		}

		uint32_t *word_data = (uint32_t*)&(body[4]);
		uint32_t word_count = (size - 4) / 4;

		//printf("WR:\t%08X\t%08X\t%u\r", body_addr, write_addr, word_count);

		if ((body_addr == write_addr) && (word_count > 0))
		{
			FLASH_Status status = FLASH_BUSY;
			FLASH_Unlock();

			FLASH_ClearFlag(FLASH_FLAG_EOP | FLASH_FLAG_OPERR | FLASH_FLAG_WRPERR |
		                    FLASH_FLAG_PGAERR | FLASH_FLAG_PGPERR | FLASH_FLAG_PGSERR);

			while (word_count--)
			{
				status = FLASH_ProgramWord(write_addr, *word_data);
				if (status != FLASH_COMPLETE) break;

				write_addr += 4;
				word_data++;
			}
			FLASH_Lock();

			if (status != FLASH_COMPLETE)
			{
				packet_send(SFU_CMD_ERROR, body, 0);
				write_addr = 0;
			}
		}
	}

	uint32_t free = recive_free();
	uint32_t count = recive_count();

	serialize_uint32(body + 0, write_addr);
	serialize_uint32(body + 4, free);
	serialize_uint32(body + 8, count);

	packet_send(code, body, 12);
}
