################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
C_SRCS += \
../src/syscalls/syscalls.c 

OBJS += \
./src/syscalls/syscalls.o 

C_DEPS += \
./src/syscalls/syscalls.d 


# Each subdirectory must supply rules for building sources it contributes
src/syscalls/%.o: ../src/syscalls/%.c
	@echo 'Building file: $<'
	@echo 'Invoking: Cross ARM C Compiler'
	arm-none-eabi-gcc -mcpu=cortex-m4 -mthumb -mfloat-abi=hard -mfpu=fpv4-sp-d16 -Os -fmessage-length=0 -ffunction-sections -fdata-sections -ffreestanding -fno-builtin -Wunused -Wuninitialized -Wall -Wextra -Wpointer-arith -Wshadow -Wlogical-op -Waggregate-return -Wfloat-equal -Wno-sign-compare -DSTM32F405RG -DSTM32F4XX -DUSE_STDPERIPH_DRIVER -D__ASSEMBLY__ -D__FPU_PRESENT -D__FPU_USED -I../src/cmsis -I../src/cmsis_boot -I../src/cmsis_lib/include -I../src/kiss/inc -I../src/usart_lib -I../src/cmsis_lib/include_inline -I../src -std=gnu11 -Wbad-function-cast -MMD -MP -MF"$(@:%.o=%.d)" -MT"$@" -c -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


