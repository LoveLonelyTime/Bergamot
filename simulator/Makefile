# Build bergamot verilator testbench
#
# Copyright (C) 2024-2025 LoveLonelyTime

VERILATOR ?= verilator
DTC ?= dtc

CFLAGS += -DDUMP_MEM
VERILATOR_FLAG += --cc -CFLAGS $(CFLAGS) --exe --build --trace -Wno-WIDTH

# The default "make all" rule
.PHONY: all
all: obj_dir/VVerilatorTestCore sim_dt.dtb

# Rule for "make clean"
.PHONY: clean
clean:
	$(RM) sim_dt.dtb 
	$(RM) -r obj_dir

# Call verilator
obj_dir/VVerilatorTestCore: sim_main.cpp VerilatorTestCore.sv
	$(VERILATOR) $(VERILATOR_FLAG)  $^

# Compile dtb
sim_dt.dtb: sim_dt.dts
	$(DTC) -I dts -O dtb $< -o $@
