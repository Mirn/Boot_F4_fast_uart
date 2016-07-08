/*
 * sfu_commands.c
 *
 *  Created on: 08 июля 2016 г.
 *      Author: Easy
 */

#include "stm32kiss.h"
#include "usart_mini.h"
#include "packet_receiver.h"

#define SFU_CMD_INFO  0x97
#define SFU_CMD_ERASE 0xC5
#define SFU_CMD_WRITE 0x38

void sfu_command_info()
{

}

void sfu_command_timeout()
{

}

void sfu_command_parser(uint8_t code, uint8_t *body, uint32_t size)
{

}
