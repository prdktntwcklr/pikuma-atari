SRC_DIR   := src
INC_DIR   := inc
BUILD_DIR := build
ASM_FLAGS := -f3 -v0

.PHONY: all clean

all:
	dasm $(SRC_DIR)/*.asm $(ASM_FLAGS) -I./$(INC_DIR) -o$(BUILD_DIR)/cart.bin -l$(BUILD_DIR)/cart.lst -s$(BUILD_DIR)/cart.sym
	
clean:
	rm -rf $(BUILD_DIR)/*.*
