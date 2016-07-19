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

#include "stm32f4xx_crc_inline.h"

#define SFU_VER_L 0
#define SFU_VER_H 1

#define SFU_CMD_ERASE_PART   0xB3
#define SFU_CMD_INFO    0x97
#define SFU_CMD_ERASE   0xC5
#define SFU_CMD_WRITE   0x38
#define SFU_CMD_START   0x26
#define SFU_CMD_TIMEOUT 0xAA
#define SFU_CMD_WRERROR 0x55
#define SFU_CMD_HWRESET 0x11

static void sfu_command_info(uint8_t code, uint8_t *body, uint32_t size);
static void sfu_command_erase(uint8_t code, uint8_t *body, uint32_t size);
static void sfu_command_write(uint8_t code, uint8_t *body, uint32_t size);
static void sfu_command_start(uint8_t code, uint8_t *body, uint32_t size);

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
	if (code == SFU_CMD_START) sfu_command_start(code, body, size);
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

typedef struct {
	uint8_t sector_id;
	uint8_t total_size;
} tFLASH_sectors;

#define ADDR_COMPRESS 0x00004000

const tFLASH_sectors sectors[] = {
		{FLASH_Sector_1, (0x00008000 / ADDR_COMPRESS)},
		{FLASH_Sector_2, (0x0000C000 / ADDR_COMPRESS)},
		{FLASH_Sector_3, (0x00010000 / ADDR_COMPRESS)},
		{FLASH_Sector_4, (0x00020000 / ADDR_COMPRESS)},
		{FLASH_Sector_5, (0x00040000 / ADDR_COMPRESS)},
		{FLASH_Sector_6, (0x00060000 / ADDR_COMPRESS)},
		{FLASH_Sector_7, (0x00080000 / ADDR_COMPRESS)},
		{FLASH_Sector_8, (0x000A0000 / ADDR_COMPRESS)},
		{FLASH_Sector_9, (0x000B0000 / ADDR_COMPRESS)},
		{FLASH_Sector_10,(0x000C0000 / ADDR_COMPRESS)},
		{FLASH_Sector_11,(0x000E0000 / ADDR_COMPRESS)},
};

static void sfu_command_erase(uint8_t code, uint8_t *body, uint32_t size)
{
	if (size != 4) return;

	uint32_t firmware_size = deserialize_uint32(body);

	if (firmware_size > 0)
	{
		FLASH_Status status = FLASH_BUSY;
		FLASH_Unlock();
		FLASH_ClearFlag(FLASH_FLAG_EOP | FLASH_FLAG_OPERR | FLASH_FLAG_WRPERR |
	                    FLASH_FLAG_PGAERR | FLASH_FLAG_PGPERR | FLASH_FLAG_PGSERR);

		for (uint32_t pos = 0; pos < LENGTH(sectors); pos++)
		{
			if ((FLASH_SIZE == 512) && (sectors[pos].sector_id == FLASH_Sector_8)) break;

			status = FLASH_EraseSector(sectors[pos].sector_id, VoltageRange_3);
			if (status != FLASH_COMPLETE)
				break;

			packet_send(SFU_CMD_ERASE_PART, (uint8_t *)&pos, sizeof(pos));

			uint32_t erased_size = ((uint32_t)sectors[pos].total_size) * ADDR_COMPRESS;
			if (erased_size >= firmware_size)
				break;
		}
		FLASH_Lock();

		if (status == FLASH_COMPLETE)
		{
			write_addr = MAIN_START_FROM;
			packet_send(code, body, size);
			return;
		}
	}

	packet_send(code, body, 0);
}

static void sfu_command_write(uint8_t code, uint8_t *body, uint32_t size)
{
	if (size > 4)
	{
		uint32_t body_addr = deserialize_uint32(body);

		if ((write_addr & 0xFF000000) != FLASH_BASE)
		{
			packet_send(SFU_CMD_WRERROR, body, 0);
			return;
		}

		uint32_t *word_data = (uint32_t*)&(body[4]);
		uint32_t word_count = (size - 4) / 4;

		//printf("WR:\t%08X\t%08X\t%u\r", body_addr, write_addr, word_count);
		//send_str("WR\r");

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
				write_addr = 0;
				packet_send(SFU_CMD_WRERROR, body, 0);
				return;
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

static void sfu_command_start(uint8_t code, uint8_t *body, uint32_t size)
{
	if (size != 4) return;

	uint32_t *from = (uint32_t*)MAIN_START_FROM;
	uint32_t count = (write_addr - MAIN_START_FROM);

	CRC_ResetDR_inline();
	uint32_t crc = CRC_CalcBlockCRC_inline(from, count / 4);
	uint32_t need = deserialize_uint32(body);

	serialize_uint32(body + 0, (uint32_t)from);
	serialize_uint32(body + 4, count);
	serialize_uint32(body + 8, crc);

	packet_send(code, body, 12);

	if (crc == need)
	{
		write_addr = 0;

		send_str("CRC OK\r");

		uint32_t *boot_from = (uint32_t*)MAIN_RUN_FROM;
		if ((boot_from[0] >> 24) != (SRAM_BASE >> 24)) return send_str("SRAM ERROR\r");
		if (((boot_from[1] >> 24) != (FLASH_BASE >> 24)) && (boot_from[1] > MAIN_RUN_FROM)) return send_str("FLASH ERROR\r");

		send_str("CONTEXT OK\r\r");

		__set_MSP(boot_from[0]);
		(*(void (*)())(boot_from[1]))();
	}
	else
		send_str("CRC != NEED\r");
}
