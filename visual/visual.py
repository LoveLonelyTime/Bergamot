import curses
from curses.textpad import rectangle
import bisect

# Bergamot verilator visual program
#
# Build by curses
#
# Copyright (C) 2024-2025 LoveLonelyTime

stdscr = None

def init_scr():
    global stdscr
    stdscr = curses.initscr()
    curses.noecho()
    curses.cbreak()
    curses.curs_set(0)
    curses.start_color()
    curses.use_default_colors()
    curses.init_pair(1, curses.COLOR_GREEN, -1)
    curses.init_pair(2, curses.COLOR_BLACK, curses.COLOR_WHITE)
    curses.init_pair(3, curses.COLOR_WHITE, curses.COLOR_RED)
    stdscr.keypad(True)
    stdscr.timeout(0)

def exit_scr():
    curses.nocbreak()
    curses.echo()
    stdscr.keypad(False)
    curses.endwin()

class CodeWindow:
    def __init__(self):
        win_height, win_width = stdscr.getmaxyx()
        self.win = stdscr.subwin(8, win_width - 20, 0, 0)
        self.code_list = []
        self.show_list = []
        self.pc = ""
        self.pc2 = ""
        with open("kernel6.txt","r") as file:
            for line in file.readlines():
                if len(line) >= 9 and line[8] == ":":
                    self.code_list.append((line.split(":")[0],line.split(":")[1]))
    def clock(self,clk):
        pass
    def update(self, msg):
        if msg[0] != "DecodeStage":
            return
        self.pc = msg[1]["I0PC"]
        self.pc2 = msg[1]["I1PC"]
        idx = bisect.bisect(self.code_list, self.pc,key=lambda x:x[0]) - 1

        if self.code_list[idx][0] == self.pc:
            self.show_list.clear()
            for line in self.code_list[idx - 3:idx+3]:
                self.show_list.append(line)


    def draw(self):
        win = self.win
        win_height, win_width = win.getmaxyx()
        win.box()
        win.addstr(0, 3, "Code")

        row = 1
        for item in self.show_list:
            win.addstr(row,1,item[0] + ":" + item[1], curses.color_pair(2) if self.pc == item[0] or self.pc2 == item[0] else curses.color_pair(0))
            row = row + 1

class ClockWindow:
    def __init__(self):
        win_height, win_width = stdscr.getmaxyx()
        self.clock_time = 0
        self.win = stdscr.subwin(8, 20, 0, win_width - 20)
    def clock(self,clk):
        pass
    def update(self, msg):
        pass
    def draw(self):
        win = self.win
        win_height, win_width = win.getmaxyx()
        win.box()
        win.addstr(0, 3, "Clock")
        win.addstr(1, 1, f"Clock:{self.clock_time}")

        if (self.clock_time & 1) == 1:
            win.addstr(2, 2, "        ┌────────")
            win.addstr(3, 2, "        │")
            win.addstr(4, 2, "        │")
            win.addstr(5, 2, "        │")
            win.addstr(6, 2, "────────┘")
        else:
            win.addstr(2, 2, "────────┐")
            win.addstr(3, 2, "        │")
            win.addstr(4, 2, "        │")
            win.addstr(5, 2, "        │")
            win.addstr(6, 2, "        └────────")

class FetcherWindow:
    def __init__(self):
        win_height, win_width = stdscr.getmaxyx()
        self.win = stdscr.subwin(9, 87, 8, 0)
        self.fetcher_pc = ""
        self.l0 = ""
        self.l1 = ""
    def clock(self,clk):
        pass
    def update(self, msg):
        if msg[0] != "InstructionFetcher":
            return
        self.fetcher_pc = msg[1]["PC"]
        self.l0 = msg[1]["L0"]
        self.l1 = msg[1]["L1"]

    def draw(self):
        win = self.win
        win_height, win_width = win.getmaxyx()
        win.box()
        win.addstr(0, 3, "Fetcher")


        win.addstr(1, 1, f"Fetcher PC: {self.fetcher_pc}")
        win.addstr(3, 1, "Buffer Line0: ")
        rectangle(win, 2, 15, 4, 85)
        win.addstr(3, 16, self.l0)

        win.addstr(6, 1, "Buffer Line1: ")
        rectangle(win, 5, 15, 7 ,85)
        win.addstr(6, 16, self.l1)


class SpeculationWindow:
    def __init__(self):
        win_height, win_width = stdscr.getmaxyx()
        self.win = stdscr.subwin(9, 70, 8, 87)
        self.i0pc = ""
        self.i0 = ""
        self.i0e = ""
        self.i0s = ""
        self.i1pc = ""
        self.i1 = ""
        self.i1e = ""
        self.i1s = ""
    def clock(self,clk):
        pass
    def update(self, msg):
        if msg[0] != "SpeculationStage":
            return
        self.i0pc = msg[1]["I0PC"]
        self.i0 = msg[1]["I0"]
        self.i0e = msg[1]["I0E"]
        self.i0s = msg[1]["I0S"]
        self.i1pc = msg[1]["I1PC"]
        self.i1 = msg[1]["I1"]
        self.i1e = msg[1]["I1E"]
        self.i1s = msg[1]["I1S"]
    def draw(self):
        win = self.win
        win_height, win_width = win.getmaxyx()
        win.box()
        win.addstr(0, 3, "Speculation")

        win.addstr(1,1,f"Instruction0: {self.i0} >-Extend-> {self.i0e}")
        win.addstr(2,1,f" - PC : {self.i0pc}")
        win.addstr(3,1,f" - Speculative Next PC ? : {self.i0s}")
        win.addstr(4,1,f"Instruction1: {self.i1} >-Extend-> {self.i1e}")
        win.addstr(5,1,f" - PC : {self.i1pc}")
        win.addstr(6,1,f" - Speculative Next PC ? : {self.i1s}")

class AXIWindow:
    def __init__(self):
        win_height, win_width = stdscr.getmaxyx()
        self.win = stdscr.subwin(9, win_width - 157, 8, 157)
        self.ar = False
        self.r = False
        self.aw = False
        self.w = False
        self.b = False
    def clock(self,clk):
        self.ar = False
        self.r = False
        self.aw = False
        self.w = False
        self.b = False
    def update(self, msg):
        if msg[0] != "AXI":
            return
        bus = msg[1]["BUS"]
        self.ar = bus == "AR"
        self.r = bus == "R"
        self.aw = bus == "AW"
        self.w = bus == "W"
        self.b = bus == "B"
    def draw(self):
        win = self.win
        win_height, win_width = win.getmaxyx()
        win.box()
        win.addstr(0, 3, "Core System Bus")

        win.addstr(1,1,"C  ─────────  ────────")
        win.addstr(2,1,"O  L1 ICache  ────────")
        win.addstr(3,1,"R  ─────────  L2 Cache")
        win.addstr(4,1,"E  L1 DCache  ────────")
        win.addstr(5,1,"   ─────────  ────────")


        win.addstr(1,25,"AR ------------>", curses.color_pair(1) if self.ar else curses.color_pair(0))
        win.addstr(2,25,"R  <------------", curses.color_pair(1) if self.r else curses.color_pair(0))
        win.addstr(3,25,"AW ------------>", curses.color_pair(1) if self.aw else curses.color_pair(0))
        win.addstr(4,25,"W  ------------>", curses.color_pair(1) if self.w else curses.color_pair(0))
        win.addstr(5,25,"B  <------------", curses.color_pair(1) if self.b else curses.color_pair(0))

        win.addstr(1,43,"├── S")
        win.addstr(2,43,"├── L")
        win.addstr(3,43,"├── A")
        win.addstr(4,43,"├── V")
        win.addstr(5,43,"├── E")

        win.addstr(1,50,"0x00000000 : Hole")
        win.addstr(2,50,"0x02000000 : Machine Timer(CLINT)")
        win.addstr(3,50,"0x10000000 : UART")
        win.addstr(4,50,"0x80000000 : RAM")
        win.addstr(5,50,"0xffff0000 : ROM")


class DecodeWindow:
    def __init__(self):
        win_height, win_width = stdscr.getmaxyx()
        self.win = stdscr.subwin(12, 70, 17, 0)
        self.i0pc = ""
        self.i0 = ""
        self.i0t = ""
        self.i0e = ""
        self.i1pc = ""
        self.i1 = ""
        self.i1t = ""
        self.i1e = ""
    def clock(self,clk):
        pass
    def update(self, msg):
        if msg[0] != "DecodeStage":
            return
        self.i0pc = msg[1]["I0PC"]
        self.i0 = msg[1]["I0"]
        self.i0t = msg[1]["I0T"]
        self.i0e = msg[1]["I0E"]
        self.i1pc = msg[1]["I1PC"]
        self.i1 = msg[1]["I1"]
        self.i1t = msg[1]["I1T"]
        self.i1e = msg[1]["I1E"]
    def draw(self):
        win = self.win
        win_height, win_width = win.getmaxyx()
        win.box()
        win.addstr(0, 3, "Decode")

        types = {"0":"Unknown","1":"R","2":"I","3":"S","4":"B","5":"U","6":"J"}
        queues = {"0":"None","1":"Memory","2":"ALU","3":"Branch","4":"Float"}

        win.addstr(1,1,f"Instruction0: ")
        win.addstr(2,1,f" - PC : {self.i0pc}")
        win.addstr(3,1,f" - {self.i0}")
        win.addstr(4,1,f" - Type : {types.get(self.i0t,'')}")
        win.addstr(5,1,f" - Execute Queue : {queues.get(self.i0e,'')}")

        win.addstr(6,1,f"Instruction1: ")
        win.addstr(7,1,f" - PC : {self.i1pc}")
        win.addstr(8,1,f" - {self.i1}")
        win.addstr(9,1,f" - Type : {types.get(self.i1t,'')}")
        win.addstr(10,1,f" - Execute Queue : {queues.get(self.i1e,'')}")

class RemappingWindow:
    def __init__(self):
        win_height, win_width = stdscr.getmaxyx()
        self.win = stdscr.subwin(12, win_width - 70, 17, 70)
        self.i0 = ""
        self.i0pc = ""
        self.i1 = ""
        self.i1pc = ""
        self.regs = []
    def clock(self,clk):
        pass
    def update(self, msg):
        if msg[0] == "RegisterMappingStage":
            self.i0pc = msg[1]["I0PC"]
            self.i0 = msg[1]["I0"]
            self.i1pc = msg[1]["I1PC"]
            self.i1 = msg[1]["I1"]
        if msg[0] == "Registers":
            self.regs = msg[1]["R"].split(" ")
    def draw(self):
        win = self.win
        win_height, win_width = win.getmaxyx()
        win.box()
        win.addstr(0, 3, "Remapping")

        win.addstr(1,1,f"Instruction0: ")
        win.addstr(2,1,f" - PC : {self.i0pc}")
        win.addstr(3,1,f" - Mapping : {self.i0}")
        win.addstr(4,1,f"Instruction1: ")
        win.addstr(5,1,f" - PC : {self.i1pc}")
        win.addstr(6,1,f" - Mapping : {self.i1}")

        win.addstr(1,90,"│")
        win.addstr(2,90,"│")
        win.addstr(3,90,"│")
        win.addstr(4,90,"│")
        win.addstr(5,90,"│")
        win.addstr(6,90,"│")
        win.addstr(7,90,"│")
        win.addstr(8,90,"│")
        win.addstr(9,90,"│")
        win.addstr(10,90,"│")

        win.addstr(1,92,"Registers: value(remapping source)")
        if self.regs:
            win.addstr(2,92,self.regs[0])
            win.addstr(2,115,self.regs[1])
            win.addstr(2,138,self.regs[2])
            win.addstr(2,161,self.regs[3])

            win.addstr(3,92,self.regs[4])
            win.addstr(3,115,self.regs[5])
            win.addstr(3,138,self.regs[6])
            win.addstr(3,161,self.regs[7])

            win.addstr(4,92,self.regs[8])
            win.addstr(4,115,self.regs[9])
            win.addstr(4,138,self.regs[10])
            win.addstr(4,161,self.regs[11])

            win.addstr(5,92,self.regs[12])
            win.addstr(5,115,self.regs[13])
            win.addstr(5,138,self.regs[14])
            win.addstr(5,161,self.regs[15])

            win.addstr(6,92,self.regs[16])
            win.addstr(6,115,self.regs[17])
            win.addstr(6,138,self.regs[18])
            win.addstr(6,161,self.regs[19])

            win.addstr(7,92,self.regs[20])
            win.addstr(7,115,self.regs[21])
            win.addstr(7,138,self.regs[22])
            win.addstr(7,161,self.regs[23])

            win.addstr(8,92,self.regs[24])
            win.addstr(8,115,self.regs[25])
            win.addstr(8,138,self.regs[26])
            win.addstr(8,161,self.regs[27])

            win.addstr(9,92,self.regs[28])
            win.addstr(9,115,self.regs[29])
            win.addstr(9,138,self.regs[30])


class ALUWindow:
    def __init__(self):
        win_height, win_width = stdscr.getmaxyx()
        self.win = stdscr.subwin(19, 35, 29, 0)
        self.active = ""
        self.decode_pc = ""
        self.op = ""
        self.op1 = ""
        self.op2 = ""
        self.execute_pc = ""
        self.result = ""
    def clock(self,clk):
        pass
    def update(self, msg):
        if msg[0] == "ALUDecode":
            self.active = msg[1]["V"]
            self.decode_pc = msg[1]["PC"]
            self.op = msg[1]["OP"]
            self.op1 = msg[1]["OP0"]
            self.op2 = msg[1]["OP1"]
        elif msg[0] == "ALUExecute":
            self.execute_pc = msg[1]["PC"]
            self.result = msg[1]["RES"]

    def draw(self):
        win = self.win
        win_height, win_width = win.getmaxyx()

        win.box()
        win.addstr(0, 3, "ALU")

        color = curses.color_pair(1) if self.active == "1" else curses.color_pair(0)

        ops = {"00":"Undefined",
               "01":"None",
               "02":"Add",
               "03":"Sub",
               "04":"And",
               "05":"Or",
               "06":"Xor",
               "07":"Sll",
               "08":"Srl",
               "09":"Sra",
               "0a":"Slt",
               "0b":"Sltu",
               "0c":"Csrrw",
               "0d":"Csrrs",
               "0e":"Csrrc",
               "0f":"Env",
               "10":"Ebreak",
               "11":"Mret",
               "12":"Sret",
               "13":"Fence",
               "14":"Fencei",
               "15":"SFence",
               "16":"Mul",
               "17":"Mulh",
               "18":"Mulhsu",
               "19":"Mulhu",
               "1a":"Div",
               "1b":"Divu",
               "1c":"Rem",
               "1d":"Remu"
               }

        win.addstr(1,1,"--------------Decode-------------",color)
        win.addstr(2,1,f"PC : {self.decode_pc}",color)
        win.addstr(3,1,f"OP : {ops.get(self.op,'')}",color)
        win.addstr(4,1,f"OP1 : {self.op1}",color)
        win.addstr(5,1,f"OP2 : {self.op2}",color)

        win.addstr(6,1,"--------------Execute------------",color)
        win.addstr(7,1,f"PC : {self.execute_pc}",color)
        win.addstr(8,1,f"Result : {self.result}",color)

class BranchWindow:
    def __init__(self):
        win_height, win_width = stdscr.getmaxyx()
        self.win = stdscr.subwin(19, 35, 29, 35)
        self.active = ""
        self.decode_pc = ""
        self.op = ""
        self.address = ""
        self.op1 = ""
        self.op2 = ""
        self.execute_pc = ""
        self.next_pc = ""
        self.goto = ""
    def clock(self,clk):
        pass
    def update(self, msg):
        if msg[0] == "BranchDecode":
            self.active = msg[1]["V"]
            self.decode_pc = msg[1]["PC"]
            self.op = msg[1]["OP"]
            self.address = msg[1]["ADD"]
            self.op1 = msg[1]["OP1"]
            self.op2 = msg[1]["OP2"]
        elif msg[0] == "BranchExecute":
            self.execute_pc = msg[1]["PC"]
            self.next_pc = msg[1]["NEXT"]
            self.goto = msg[1]["TO"]

    def draw(self):
        win = self.win
        win_height, win_width = win.getmaxyx()

        win.box()
        win.addstr(0, 3, "Branch")

        color = curses.color_pair(1) if self.active == "1" else curses.color_pair(0)

        ops = {
            "0": "Undefined",
            "1": "None",
            "2": "Eq",
            "3": "Ne",
            "4": "Lt",
            "5": "Ge",
            "6": "Ltu",
            "7": "Geu",
            "8": "Jal"
        }
        win.addstr(1,1,"--------------Decode------------",color)
        win.addstr(2,1,f"PC : {self.decode_pc}",color)
        win.addstr(3,1,f"OP : {ops.get(self.op,'')}",color)
        win.addstr(4,1,f"Address : {self.address}",color)
        win.addstr(5,1,f"OP1 : {self.op1}",color)
        win.addstr(6,1,f"OP2 : {self.op2}",color)

        win.addstr(7,1,"--------------Execute------------",color)
        win.addstr(8,1,f"PC : {self.execute_pc}",color)
        win.addstr(9,1,f"Next PC : {self.next_pc}",color)
        win.addstr(10,1,f"Goto PC : {self.goto}",color)

class MemoryWindow:
    def __init__(self):
        win_height, win_width = stdscr.getmaxyx()
        self.win = stdscr.subwin(19, 35, 29, 70)
        self.active = ""
        self.decode_pc = ""
        self.op = ""
        self.address = ""
        self.op1 = ""
        self.execute_pc = ""
        self.vaddress = ""
        self.tlb_pc = ""
        self.paddress = ""
        self.rw_pc = ""
        self.result = ""
        self.id = ""
    def clock(self,clk):
        pass
    def update(self, msg):
        if msg[0] == "MemoryDecode":
            self.active = msg[1]["V"]
            self.decode_pc = msg[1]["PC"]
            self.op = msg[1]["OP"]
            self.address = msg[1]["ADD"]
            self.op1 = msg[1]["OP1"]
        if msg[0] == "MemoryExecute":
            self.execute_pc = msg[1]["PC"]
            self.vaddress = msg[1]["VADDR"]
        if msg[0] == "MemoryTLB":
            self.tlb_pc = msg[1]["PC"]
            self.paddress = msg[1]["PADDR"]
        if msg[0] == "MemoryReadAndWrite":
            self.rw_pc = msg[1]["PC"]
            self.result = msg[1]["RES"]
            self.id = msg[1]["ID"]

    def draw(self):
        win = self.win
        win_height, win_width = win.getmaxyx()

        win.box()
        win.addstr(0, 3, "Memory")

        color = curses.color_pair(1) if self.active == "1" else curses.color_pair(0)

        ops = {
            "00": "Undefined",
            "01": "None",
            "02": "Lb",
            "03": "Lh",
            "04": "Lw",
            "05": "Lbu",
            "06": "Lhu",
            "07": "Sb",
            "08": "Sh",
            "09": "Sw",
            "0a": "Amoswap",
            "0b": "Amoadd",
            "0c": "Amoxor",
            "0d": "Amoand",
            "0e": "Amoor",
            "0f": "Amomin",
            "10": "Amomax",
            "11": "Amominu",
            "12": "Amomaxu",
            "13": "LR",
            "14": "SC"
        }

        win.addstr(1,1,"--------------Decode------------",color)
        win.addstr(2,1,f"PC : {self.decode_pc}",color)
        win.addstr(3,1,f"OP : {ops.get(self.op,'')}",color)
        win.addstr(4,1,f"Address : {self.address}",color)
        win.addstr(5,1,f"OP1 : {self.op1}",color)

        win.addstr(7,1,"--------------Execute------------",color)
        win.addstr(8,1,f"PC : {self.execute_pc}",color)
        win.addstr(9,1,f"Virtual Address : {self.vaddress}",color)

        win.addstr(10,1,"----------------TLB-------------",color)
        win.addstr(11,1,f"PC : {self.tlb_pc}",color)
        win.addstr(12,1,f"Physical Address : {self.paddress}",color)

        win.addstr(13,1,"----------------RW--------------",color)
        win.addstr(14,1,f"PC : {self.rw_pc}",color)
        win.addstr(15,1,f"Read Result : {self.result}",color)
        win.addstr(16,1,f"Write Store Queue ID : {self.id}",color)

class RetireWindow:
    def __init__(self):
        win_height, win_width = stdscr.getmaxyx()
        self.win = stdscr.subwin(19, 50, 29, 105)
        self.rob = []
    def clock(self,clk):
        self.rob = []
    def update(self,msg):
        if msg[0] == "ROB":
            self.rob.append((msg[1]["ID"],msg[1]["PC"],msg[1]["RD"],msg[1]["COM"]))
    def draw(self):
        win = self.win
        win_height, win_width = win.getmaxyx()

        win.box()
        win.addstr(0, 3, "Retire")

        win.addstr(1,1,"ROB ID      PC       RD       COMMIT           ",curses.color_pair(2))
        row = 2
        for item in self.rob:
            win.addstr(row,1,f"{item[0]}      {item[1]}       {item[2]}       {item[3]}       ")
            row = row + 1

class ConsoleWindow:
    def __init__(self):
        win_height, win_width = stdscr.getmaxyx()
        self.win = stdscr.subwin(19, 85, 29, 155)

        self.rows = [""]
    def clock(self,clk):
        pass
    def update(self,msg):
        if msg[0] == "Console":
            ch = chr(int(msg[1]["CH"]))
            if ch == "\n":
                self.rows.append("")
            else:
                self.rows[-1] = self.rows[-1] + ch
    def draw(self):
        win = self.win
        win_height, win_width = win.getmaxyx()

        win.box()
        win.addstr(0, 3, "Console TTY0")

        row = 1
        for line in self.rows[-16:]:
            win.addstr(row,1,line)
            row = row + 1

class StatusWindow:
    def __init__(self):
        win_height, win_width = stdscr.getmaxyx()
        self.win = stdscr.subwin(19, win_width - 240, 29, 240)
        self.show = "No Event"
    def clock(self,clk):
        pass
    def update(self,msg):
        if msg[0] != "Status":
            return
        self.show = msg[1]["R"]
    def draw(self):
        win = self.win
        win_height, win_width = win.getmaxyx()

        win.box()
        win.addstr(0, 3, "Core Status")

        win.addstr(1,1,"Last Status:")

        win.addstr(2,1,self.show, curses.color_pair(0) if self.show == "No Event" else curses.color_pair(3))

        win.addstr(4,1,"------------------------")
        win.addstr(5,7,"╭─┴─┴─┴─┴──╮")
        win.addstr(6,7,"┤  V    V  ├")
        win.addstr(7,7,"┤ Bergamot ├")
        win.addstr(8,7,"┤   Core   ├")
        win.addstr(9,7,"╰─┬─┬─┬─┬──╯")
        win.addstr(10,1,"An exquisite superscalar")
        win.addstr(11,1,"RV32GC processor.")
        win.addstr(12,1,"https://github.com/")
        win.addstr(13,1,"LoveLonelyTime/Bergamot")

def parse_message(line):
    source = line.split("[>]")[0]
    params = {item.split("=")[0]:item.split("=")[1] for item in line.split("[>]")[1].split(",")}
    return (source, params)

def main():
    file = open("log.txt","r")
    rise_edge = False
    
    init_scr()
    loop = True
    clock = ClockWindow()
    clock.clock_time = 0
    windows = [CodeWindow(), clock, FetcherWindow(), SpeculationWindow(), AXIWindow(), DecodeWindow(), RemappingWindow(), ALUWindow(), BranchWindow(), MemoryWindow(), RetireWindow(), ConsoleWindow(), StatusWindow()]

    while loop:
        if rise_edge:
            line = file.readline().strip()
            while line != "[p:":
                line = file.readline().strip()
            for window in windows:
                window.clock(clock.clock_time)
            while True:
                line = file.readline().strip()
                if line == "[n:":
                    break
                msg = parse_message(line)
                for window in windows:
                    window.update(msg)
        clock.clock_time = clock.clock_time + 1
        rise_edge = not rise_edge

        if clock.clock_time % 1000 == 0:
            for window in windows:
                window.win.erase()
                window.draw()
                window.win.refresh()
            # time.sleep(0.01)
            key = stdscr.getch()
            if key == ord('q'):
                loop = False
    exit_scr()

if __name__ == "__main__":
    main()
