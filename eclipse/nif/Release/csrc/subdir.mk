################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
C_SRCS += \
../csrc/core_nif.c 

OBJS += \
./csrc/core_nif.o 

C_DEPS += \
./csrc/core_nif.d 


# Each subdirectory must supply rules for building sources it contributes
csrc/%.o: ../csrc/%.c
	@echo 'Building file: $<'
	@echo 'Invoking: GCC C Compiler'
	gcc -I"/usr/lib64/erlang/usr/include" -O3 -Wall -c -fmessage-length=0 -std=gnu99 -fPIC -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@:%.o=%.d)" -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


