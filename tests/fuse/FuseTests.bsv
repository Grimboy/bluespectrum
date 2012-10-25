package FuseTests;

import Z80a::*;
import TestDRAM::*;
import FuseTestTypes::*;
import GeneratedTests::*;
import Connectable::*;
import List::*;

module mkFuseTests(Empty);
    Reg#(UInt#(8)) cycle <- mkReg(0);
    Z80a_ifc cpu <- mkZ80a(False);
    TestT test = tests[`testnum];
    TestDRAM_ifc dram <- mkTestDRAM(test.minit);

    Bit#(16) cpu_addr = cpu.addr();
    mkConnection(cpu.data, dram.data);

    rule inc_cycle;
       cycle <= cycle + 1;
    endrule

    rule at_start(cycle == 0);
        $display("Running: %s", test.comment);
    endrule

    rule finished(cycle > 50);
        $display("Test timed out after 50 cycles");
        // Print fail/success
        $finish(0);
        //$finish(-1);
    endrule

    rule dram_addr(cpu_addr == 0);
        dram.addr(cpu_addr());
    endrule

    rule dont_wait;
        cpu.n_wait((cycle > 0) ? 1 : 0);
    endrule
endmodule

endpackage
