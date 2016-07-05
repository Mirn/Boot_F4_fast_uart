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

uint16_t packet_size = 0;
uint32_t packet_code = 0;
uint8_t *packet_body = NULL;
uint32_t packet_crc = 0;


static bool recive_check_start()
{
	uint8_t rx = 0;
	if (!recive_byte(&rx))
		return false;

	packet_start = (packet_start << 8) | ((uint32_t)rx);
	if (packet_start == PACKET_START_SIGN)
	{
		recive_check = recive_check_info;
		send_str("packet_start_sign\r\n");
	}
	return true;
}

static bool recive_check_info()
{
	if (recive_count() < 4)
		return false;

	recive_check = recive_check_body;
	return true;
}

static bool recive_check_body()
{
	if (recive_count() < packet_size)
		return false;

	recive_check = recive_check_crc;
	return true;
}

static bool recive_check_crc()
{
	if (recive_count() < 4)
		return false;

	recive_packets_init();
	return true;
}

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
