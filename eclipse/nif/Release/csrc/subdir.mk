################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
C_SRCS += \
/home/erarafo/wednesday/antonius/csrc/core_nif.c 

OBJS += \
./csrc/core_nif.o 

C_DEPS += \
./csrc/core_nif.d 


# Each subdirectory must supply rules for building sources it contributes
csrc/core_nif.o: /home/erarafo/wednesday/antonius/csrc/core_nif.c
	@echo 'Building file: $<'
	@echo 'Invoking: GCC C Compiler'
	gcc -I/home/erarafo/local/lib/erlang/usr/include -O3 -Wall -c -fmessage-length=0 -std=gnu99 -fPIC -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@:%.o=%.d)" -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


