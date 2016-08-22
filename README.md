# Boot_F4_fast_uart
Small, fast and simple STM32F4xx UART bootloader.
With very small footprint but without assembler code.

подробное описание на русском:
https://habrahabr.ru/post/305800/

* **Target MCU**: STM32F405 / STM32F407 
* **clocks**: 147.5 MHz (for minimal BOD error)
* **uart**: 921600 BOD, 8bit, noParity, recive from IRQ to fifo buffer, direct send
* **Protocol**: Packets with H/W CRC32 and acknowledgment, lost and corrupt data protection
* **Can work from locked flash**
* **Firmware update ONLY**
* **fast write speed** 400kb firmware in 9 seconds
* **partital erase** and full erase
* **timeout for start main** 5 seconds

Project finished
#### Current footprint:
|text|data|bss|filename|
|----|----|----|--------------|
|**3390**|304|123692|Boot_F4_fast_uart.elf|
Total Flash image is 3.7k - less than first flash page in STM32 (4k ... 16k)