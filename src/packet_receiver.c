/*
 * packet_receiver.c
 *
 *  Created on: 05 июля 2016 г.
 *      Author: Easy
 */

#include "stm32kiss.h"
#include "usart_mini.h"
#include "packet_receiver.h"

#include "stm32f4xx_rcc_inline.h"
#include "stm32f4xx_crc_inline.h"

//#define LOG_DETAILS

#ifdef LOG_DETAILS
#define send_status(msg) send_str(msg)
#else
#define send_status(msg)
#endif

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

uint32_t stat_normals = 0;

uint32_t stat_error_timeout = 0;
uint32_t stat_error_overfull = 0;
uint32_t stat_error_start = 0;
uint32_t stat_error_code = 0;
uint32_t stat_error_size = 0;
uint32_t stat_error_crc = 0;

void recive_packets_init()
{
	packet_timeout = TIMEOUT_TURNS;
	recive_check = recive_check_start;
	packet_cnt = 0;
	RCC_AHB1PeriphClockCmd_inline(RCC_AHB1Periph_CRC, ENABLE);
}

static bool ERROR_RESET(const char *err_msg, uint32_t *stat_inc)
{
	(*stat_inc)++;
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
		return ERROR_RESET("packet_buf overfull", &stat_error_overfull);

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
		send_status("packet_start_sign OK\r");
		recive_check = recive_check_info;
		stat_error_start -= 3;
	}
	else
		stat_error_start++;

	return true;
}

static bool recive_check_info()
{
	if (!recive_n_bytes(4))
		return false;

	packet_code   = packet_buf[0] ^ 0x00;
	packet_code_n = packet_buf[1] ^ 0xFF;

	if (packet_code != packet_code_n)
		return ERROR_RESET("recive_check_info: packet_code != ~packet_code_n", &stat_error_code);

	packet_size = (((uint16_t)packet_buf[2]) << 0) |
				  (((uint16_t)packet_buf[3]) << 8);

	if (packet_size > PACKET_MAX_SIZE)
		return ERROR_RESET("recive_check_info: packet_size > PACKET_MAX_SIZE", &stat_error_size);

	packet_body = &packet_buf[4];

	send_status("packet_check_info OK\r");
	recive_check = recive_check_body;
	return true;
}

static bool recive_check_body()
{
	if (!recive_n_bytes(packet_size))
		return false;

	send_status("recive_check_body OK\r");
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

	CRC_ResetDR_inline();
	uint32_t real_crc = CRC_CalcBlockCRC_inline((uint32_t*)packet_buf, (packet_cnt-4)/4);

	//printf("real_crc  \t%08X\r", real_crc);
	//printf("packet_crc\t%08X\r", packet_crc);

	if (real_crc != packet_crc)
		return ERROR_RESET("recive_check_crc: real_crc != packet_crc", &stat_error_crc);

	stat_normals++;

	send_status("recive_check_crc OK\r");
	recive_packets_init();
	return true;
}

///////////////////////////////////////////////////////

void recive_packets_print_stat()
{
#ifndef LOG_DETAILS
	static uint32_t last_time = 0;
	uint32_t now_time = DWT_CYCCNT;

	if ((now_time - last_time) < SystemCoreClock) return;
	last_time = now_time;

	printf("%i\t", stat_normals);
	printf("\t");
	printf("%i\t", stat_error_timeout);
	printf("%i\t", stat_error_overfull);
	printf("\t");
	printf("%i\t", stat_error_start);
	printf("%i\t", stat_error_code);
	printf("%i\t", stat_error_size);
	printf("%i\t", stat_error_crc);
	printf("\r");
#endif
}

///////////////////////////////////////////////////////

void recive_packets_worker()
{
	packet_timeout--;
	if (packet_timeout == 0)
	{
		stat_error_timeout++;
		recive_packets_init();
		send_status("timeout\r");
	}

	while ((*recive_check)())
		packet_timeout = TIMEOUT_TURNS;
}
