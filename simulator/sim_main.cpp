/*
 * Bergamot verilator testbench
 *
 * Usage: VVerilatorTestCore [+trace] +B<binary file> [+D<device tree file>] [+T<timeout>] [+W<write host>]
 *     +trace : Output waveform file.
 *     +B<binary file> : The RISC-V binary file to be executed.
 *     +D<device tree file> : The device tree file (.dtb).
 *     +T<timeout> : Maximum testing cycle.
 *     +W<write host> : Write host address (base16).
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

#include "VVerilatorTestCore.h"
#include "verilated.h"

#include <stdio.h>

#define P_ERROR "ERROR: "
#define P_WARN "WARN: "
#define P_INFO "INFO: "

#ifdef DUMP_MEM
#include <unistd.h>
#include <sys/wait.h>
#include <sys/types.h>
#endif

#ifdef VM_TRACE
#include <verilated_vcd_c.h>
#endif

// 256MB = 1024 * 1024 * 256 / 4
const unsigned int RAM_START = 0x80000000;
const unsigned long DRAM_SIZE = 67108864;
static unsigned int ram[DRAM_SIZE];

// Binary address = 0x80000000
const unsigned long BIN_OFFSET = 0;
const unsigned long BIN_SIZE = 66846720;

// Device tree address = 0x8ff00000
const unsigned long DT_OFFSET = 66846720;
const unsigned long DT_SIZE = 262144;

#ifdef DUMP_MEM
void dump_mem(int signo)
{
    FILE *fp = fopen("mem.bin", "wb");
    if (!fp)
    {
        VL_PRINTF(P_ERROR "Memory file open failed!\n");
        exit(-1);
    }
    fwrite(ram, sizeof(unsigned int), DRAM_SIZE, fp);
    fclose(fp);
    VL_PRINTF("Memory has been dumped into mem.bin.\n");
    exit(0);
}
#endif

int main(int argc, char **argv)
{
    const char *flag = nullptr;
    FILE *fp = nullptr;
    vluint64_t main_time = 0;

#ifdef DUMP_MEM
    signal(SIGINT, dump_mem);
#endif

    Verilated::commandArgs(argc, argv);

    // Get binary file
    flag = Verilated::commandArgsPlusMatch("B");
    if (flag && strncmp(flag, "+B", 2) == 0)
    {
        fp = fopen(&flag[2], "rb");
        if (!fp)
        {
            VL_PRINTF(P_ERROR "Binary file open failed!\n");
            return -1;
        }
        fread(ram + BIN_OFFSET, sizeof(unsigned int), BIN_SIZE, fp);
        fclose(fp);
    }
    else
    {
        VL_PRINTF(P_ERROR "No binary file specified!\n");
        return -1;
    }

    // Get device tree file
    flag = Verilated::commandArgsPlusMatch("D");
    if (flag && strncmp(flag, "+D", 2) == 0)
    {
        fp = fopen(&flag[2], "rb");
        if (!fp)
        {
            VL_PRINTF(P_ERROR "Device tree file open failed!\n");
            return -1;
        }
        fread(ram + DT_OFFSET, sizeof(unsigned int), DT_SIZE, fp);
        fclose(fp);
    }
    else
    {
        VL_PRINTF(P_WARN "No device tree file specified!\n");
    }

    // Create verilator top
    VVerilatorTestCore *top = new VVerilatorTestCore;
#ifdef VM_TRACE
    // If verilator was invoked with --trace argument,
    // and if at run time passed the +trace argument, turn on tracing
    VerilatedVcdC *tfp = NULL;
    flag = Verilated::commandArgsPlusMatch("trace");
    if (flag && 0 == strcmp(flag, "+trace"))
    {
        Verilated::traceEverOn(true); // Verilator must compute traced signals
        VL_PRINTF(P_INFO "Enabling waves into logs/vlt_dump.vcd...\n");
        tfp = new VerilatedVcdC;
        top->trace(tfp, 99); // Trace 99 levels of hierarchy
        Verilated::mkdir("logs");
        tfp->open("logs/vlt_dump.vcd"); // Open the dump file
    }
#endif

    // Set max time
    vluint64_t max_time = 0;

    flag = Verilated::commandArgsPlusMatch("T");
    if (flag && strncmp(flag, "+T", 2) == 0)
    {
        max_time = strtoul(&flag[2], NULL, 10);
    }

    if (max_time == 0)
        VL_PRINTF(P_WARN "Simulation will be executed infinitely!\n");

    // Set write host address
    unsigned long write_host = 0;
    flag = Verilated::commandArgsPlusMatch("W");
    if (flag && strncmp(flag, "+W", 2) == 0)
    {
        write_host = strtoul(&flag[2], NULL, 16) - RAM_START;
    }

    while (!Verilated::gotFinish() && (max_time == 0 || main_time <= max_time))
    {
        main_time++;
        top->clock = !top->clock;
        if (main_time % 1000000 == 0)
        {
            VL_PRINTF(P_INFO "Clock : %lu\n", main_time);
        }

        if (main_time > 1 && main_time < 10)
            top->reset = 1;
        else
            top->reset = 0;

        // Virtual RAM
        unsigned int readAddress = top->io_rdAddress >> 2;
        if (readAddress >= DRAM_SIZE)
            top->io_rdData = 0;
        else
            top->io_rdData = ram[readAddress];

        unsigned int wrData = top->io_wrData;
        if (top->io_wrStrobe & 1)
            ram[top->io_wrAddress >> 2] = (ram[top->io_wrAddress >> 2] & 0xffffff00) | (wrData & 0x000000ff);
        if (top->io_wrStrobe & 2)
            ram[top->io_wrAddress >> 2] = (ram[top->io_wrAddress >> 2] & 0xffff00ff) | (wrData & 0x0000ff00);
        if (top->io_wrStrobe & 4)
            ram[top->io_wrAddress >> 2] = (ram[top->io_wrAddress >> 2] & 0xff00ffff) | (wrData & 0x00ff0000);
        if (top->io_wrStrobe & 8)
            ram[top->io_wrAddress >> 2] = (ram[top->io_wrAddress >> 2] & 0x00ffffff) | (wrData & 0xff000000);

        // Virtual UART
        if (top->io_send && top->clock)
            VL_PRINTF("%c", top->io_dataOut);

        top->eval();

        // if (top->io_debug_hit) {
#ifdef VM_TRACE
        // Dump trace data for this cycle
        if (tfp)
            tfp->dump(main_time);
#endif
        // }
    }

    top->final();

#ifdef VM_TRACE
    if (tfp)
    {
        tfp->close();
        tfp = NULL;
    }
#endif
    delete top;

    if (write_host)
    {
        unsigned int val = ram[write_host >> 2];

        if (val != 1)
            VL_PRINTF(P_WARN "Expected %u, but got %u.\n", 1, val);
    }

#ifdef DUMP_MEM
    dump_mem(0);
#endif
    return 0;
}
