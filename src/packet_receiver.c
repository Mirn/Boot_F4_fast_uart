/*
 * packet_receiver.c
 *
 *  Created on: 05 июля 2016 г.
 *      Author: Easy
 */

#include "stm32kiss.h"
#include "usart_mini.h"
#include "packet_receiver.h"

#define PACKET_START_SIGN 0x817EA345
#define TIMEOUT_TURNS 1000000

static bool recive_check_start();
static bool recive_check_info();
static bool recive_check_body();
static bool recive_check_crc();

static bool (*recive_check)() = NULL;
static uint32_t packet_start = 0;
static uint32_t packet_timeout = 0;

uint8_t  packet_buf[4096] __attribute__ ((aligned (sizeof(uint32_t))));
uint32_t packet_cnt = 0;

#define PACKET_MAX_SIZE (sizeof(packet_buf) - 8) //header 4 bytes + crc 4 bytes

uint8_t  packet_code = 0;
uint8_t  packet_code_n = 0;
uint16_t packet_size = 0;
uint8_t *packet_body = NULL;
uint32_t packet_crc = 0;

static bool ERROR_RESET(const char *err_msg)
{
	send_str("ERROR: ");
	send_str(err_msg);
	send('\r');
	recive_packets_init();
	return false;
}

static bool recive_n_bytes(uint32_t cnt)
{
	if (recive_count() < cnt)
		return false;

	while (cnt--)
	{
		uint8_t rx = 0;
		recive_byte(&rx);

		if (packet_cnt < sizeof(packet_buf))
			packet_buf[packet_cnt++] = rx;
	};

	if (packet_cnt >= sizeof(packet_buf))
		return ERROR_RESET("packet_buf overfull");

	return true;
}

///////////////////////////////////////////////////////

static bool recive_check_start()
{
	uint8_t rx = 0;
	if (!recive_byte(&rx))
		return false;

	packet_start = (packet_start << 8) | ((uint32_t)rx);
	if (packet_start == PACKET_START_SIGN)
	{
		recive_check = recive_check_info;
		send_str("packet_start_sign OK\r\n");
	}
	return true;
}

static bool recive_check_info()
{
	if (!recive_n_bytes(4))
		return false;
	recive_check = recive_check_body;

	packet_code   = packet_buf[0] ^ 0x00;
	packet_code_n = packet_buf[1] ^ 0xFF;

	if (packet_code != packet_code_n)
		return ERROR_RESET("recive_check_info: packet_code != ~packet_code_n");

	packet_size = (((uint16_t)packet_buf[2]) << 0) |
				  (((uint16_t)packet_buf[3]) << 8);

	if (packet_size > PACKET_MAX_SIZE)
		return ERROR_RESET("recive_check_info: packet_size > PACKET_MAX_SIZE");

	packet_body = &packet_buf[4];
	return true;
}

static bool recive_check_body()
{
	if (!recive_n_bytes(packet_size))
		return false;

	recive_check = recive_check_crc;
	return true;
}

static bool recive_check_crc()
{
	if (!recive_n_bytes(4))
		return false;

	packet_crc =
			(((uint32_t)packet_buf[packet_cnt - 4]) <<  0) |
			(((uint32_t)packet_buf[packet_cnt - 3]) <<  8) |
			(((uint32_t)packet_buf[packet_cnt - 2]) << 16) |
			(((uint32_t)packet_buf[packet_cnt - 1]) << 24);

	recive_packets_init();
	return true;
}

///////////////////////////////////////////////////////

void recive_packets_init()
{
	packet_timeout = TIMEOUT_TURNS;
	recive_check = recive_check_start;
	packet_cnt = 0;
}

void recive_packets()
{
	if (packet_timeout == 0)
	{
		recive_packets_init();
		send_str("timeout\r\n");
	}
	else
		packet_timeout--;

	if (recive_check == NULL)
		recive_check = recive_check_start;

	if ((*recive_check)())
		packet_timeout = TIMEOUT_TURNS;
}
