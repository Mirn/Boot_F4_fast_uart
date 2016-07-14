################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
C_SRCS += \
../src/cmsis_boot/startup/startup_stm32f4xx.c 

OBJS += \
./src/cmsis_boot/startup/startup_stm32f4xx.o 

C_DEPS += \
./src/cmsis_boot/startup/startup_stm32f4xx.d 


# Each subdirectory must supply rules for building sources it contributes
src/cmsis_boot/startup/%.o: ../src/cmsis_boot/startup/%.c
	@echo 'Building file: $<'
	@echo 'Invoking: Cross ARM C Compiler'
	arm-none-eabi-gcc -mcpu=cortex-m4 -mthumb -mfloat-abi=hard -mfpu=fpv4-sp-d16 -Os -fmessage-length=0 -ffunction-sections -fdata-sections -ffreestanding -fno-builtin -Wunused -Wuninitialized -Wall -Wextra -Wpointer-arith -Wshadow -Wlogical-op -Waggregate-return -Wfloat-equal -Wno-sign-compare -DSTM32F405RG -DSTM32F4XX -DUSE_STDPERIPH_DRIVER -D__ASSEMBLY__ -D__FPU_PRESENT -D__FPU_USED -I../src/cmsis -I../src/cmsis_boot -I../src/cmsis_lib/include -I../src/kiss/inc -I../src/usart_lib -I../src/cmsis_lib/include_inline -I../src -std=gnu11 -Wbad-function-cast -MMD -MP -MF"$(@:%.o=%.d)" -MT"$@" -c -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


