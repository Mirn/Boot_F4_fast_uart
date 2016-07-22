/*
 * sfu_commands.h
 *
 *  Created on: 08 июля 2016 г.
 *      Author: Easy
 */

#ifndef SFU_COMMANDS_H_
#define SFU_COMMANDS_H_

void sfu_command_init();
void sfu_command_parser(uint8_t code, uint8_t *body, uint32_t size);
void sfu_command_timeout();

void main_start();

#endif /* SFU_COMMANDS_H_ */
