################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
C_SRCS += \
../src/kiss/src/stm32kiss_adc.c \
../src/kiss/src/stm32kiss_button.c \
../src/kiss/src/stm32kiss_dac.c \
../src/kiss/src/stm32kiss_fifo.c \
../src/kiss/src/stm32kiss_gpio.c \
../src/kiss/src/stm32kiss_startup.c \
../src/kiss/src/stm32kiss_ticks.c 

OBJS += \
./src/kiss/src/stm32kiss_adc.o \
./src/kiss/src/stm32kiss_button.o \
./src/kiss/src/stm32kiss_dac.o \
./src/kiss/src/stm32kiss_fifo.o \
./src/kiss/src/stm32kiss_gpio.o \
./src/kiss/src/stm32kiss_startup.o \
./src/kiss/src/stm32kiss_ticks.o 

C_DEPS += \
./src/kiss/src/stm32kiss_adc.d \
./src/kiss/src/stm32kiss_button.d \
./src/kiss/src/stm32kiss_dac.d \
./src/kiss/src/stm32kiss_fifo.d \
./src/kiss/src/stm32kiss_gpio.d \
./src/kiss/src/stm32kiss_startup.d \
./src/kiss/src/stm32kiss_ticks.d 


# Each subdirectory must supply rules for building sources it contributes
src/kiss/src/%.o: ../src/kiss/src/%.c
	@echo 'Building file: $<'
	@echo 'Invoking: Cross ARM C Compiler'
	arm-none-eabi-gcc -mcpu=cortex-m4 -mthumb -mfloat-abi=hard -mfpu=fpv4-sp-d16 -Os -fmessage-length=0 -ffunction-sections -fdata-sections -ffreestanding -fno-builtin -Wunused -Wuninitialized -Wall -Wextra -Wpointer-arith -Wshadow -Wlogical-op -Waggregate-return -Wfloat-equal -Wno-sign-compare -DSTM32F405RG -DSTM32F4XX -DUSE_STDPERIPH_DRIVER -D__ASSEMBLY__ -D__FPU_PRESENT -D__FPU_USED -I../src/cmsis -I../src/cmsis_boot -I../src/cmsis_lib/include -I../src/kiss/inc -I../src/usart_lib -I../src/cmsis_lib/include_inline -I../src -std=gnu11 -Wbad-function-cast -MMD -MP -MF"$(@:%.o=%.d)" -MT"$@" -c -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


