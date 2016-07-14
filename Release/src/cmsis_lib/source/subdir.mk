################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
C_SRCS += \
../src/cmsis_lib/source/misc.c \
../src/cmsis_lib/source/stm32f4xx_adc.c \
../src/cmsis_lib/source/stm32f4xx_crc.c \
../src/cmsis_lib/source/stm32f4xx_dac.c \
../src/cmsis_lib/source/stm32f4xx_dma.c \
../src/cmsis_lib/source/stm32f4xx_flash.c \
../src/cmsis_lib/source/stm32f4xx_gpio.c \
../src/cmsis_lib/source/stm32f4xx_pwr.c \
../src/cmsis_lib/source/stm32f4xx_rcc.c \
../src/cmsis_lib/source/stm32f4xx_spi.c \
../src/cmsis_lib/source/stm32f4xx_tim.c \
../src/cmsis_lib/source/stm32f4xx_usart.c 

OBJS += \
./src/cmsis_lib/source/misc.o \
./src/cmsis_lib/source/stm32f4xx_adc.o \
./src/cmsis_lib/source/stm32f4xx_crc.o \
./src/cmsis_lib/source/stm32f4xx_dac.o \
./src/cmsis_lib/source/stm32f4xx_dma.o \
./src/cmsis_lib/source/stm32f4xx_flash.o \
./src/cmsis_lib/source/stm32f4xx_gpio.o \
./src/cmsis_lib/source/stm32f4xx_pwr.o \
./src/cmsis_lib/source/stm32f4xx_rcc.o \
./src/cmsis_lib/source/stm32f4xx_spi.o \
./src/cmsis_lib/source/stm32f4xx_tim.o \
./src/cmsis_lib/source/stm32f4xx_usart.o 

C_DEPS += \
./src/cmsis_lib/source/misc.d \
./src/cmsis_lib/source/stm32f4xx_adc.d \
./src/cmsis_lib/source/stm32f4xx_crc.d \
./src/cmsis_lib/source/stm32f4xx_dac.d \
./src/cmsis_lib/source/stm32f4xx_dma.d \
./src/cmsis_lib/source/stm32f4xx_flash.d \
./src/cmsis_lib/source/stm32f4xx_gpio.d \
./src/cmsis_lib/source/stm32f4xx_pwr.d \
./src/cmsis_lib/source/stm32f4xx_rcc.d \
./src/cmsis_lib/source/stm32f4xx_spi.d \
./src/cmsis_lib/source/stm32f4xx_tim.d \
./src/cmsis_lib/source/stm32f4xx_usart.d 


# Each subdirectory must supply rules for building sources it contributes
src/cmsis_lib/source/%.o: ../src/cmsis_lib/source/%.c
	@echo 'Building file: $<'
	@echo 'Invoking: Cross ARM C Compiler'
	arm-none-eabi-gcc -mcpu=cortex-m4 -mthumb -mfloat-abi=hard -mfpu=fpv4-sp-d16 -Os -fmessage-length=0 -ffunction-sections -fdata-sections -ffreestanding -fno-builtin -Wunused -Wuninitialized -Wall -Wextra -Wpointer-arith -Wshadow -Wlogical-op -Waggregate-return -Wfloat-equal -Wno-sign-compare -DSTM32F405RG -DSTM32F4XX -DUSE_STDPERIPH_DRIVER -D__ASSEMBLY__ -D__FPU_PRESENT -D__FPU_USED -I../src/cmsis -I../src/cmsis_boot -I../src/cmsis_lib/include -I../src/kiss/inc -I../src/usart_lib -I../src/cmsis_lib/include_inline -I../src -std=gnu11 -Wbad-function-cast -MMD -MP -MF"$(@:%.o=%.d)" -MT"$@" -c -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


