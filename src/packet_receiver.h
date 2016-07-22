/*
 * packet_receiver.h
 *
 *  Created on: 05 июля 2016 г.
 *      Author: Easy
 */

#ifndef PACKET_RECEIVER_H_
#define PACKET_RECEIVER_H_

void recive_packets_init();
void recive_packets_worker();
void recive_packets_print_stat();

void packet_send(const uint8_t code, const uint8_t *body, const uint32_t size);

#define PACKET_TIMEOUT_mS 500
extern uint32_t stat_error_timeout;

#endif /* PACKET_RECEIVER_H_ */
