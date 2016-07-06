# Boot_F4_fast_uart
Small, fast and simple STM32F4xx UART bootloader.
With very small footprint but without assembler code.

* **Target MCU**: STM32F4xx and STM32F3xx
* **clocks**: 160MHz with power save in sleep
* **uart**: 500k BOD 8bit noParity, recive from IRQ to fifo buffer, direct send
* **System tick timer**: 1000Hz
* **Protocol**: Packets with H/W CRC32 and acknowledgment, lost and corrupt data protection
* **Crypto protection**: may be used individual ChipID in STM32 architecture
* **Can work from locked flash**
* **Firmware update ONLY**

#### Done:
System initialization, delays,  UART, packets recive with HW CRC32, packet reciver statistics, status messages to console.


#### Current footprint:
|text|data|bss|dec|hex|filename|
|----|----|----|----|----|--------------|
|**3270**|0|70220|73490|11f12|Boot_F4_fast_uart.elf|
Flash image is 3.3k - less than first flash page in STM32 (4k ... 16k)