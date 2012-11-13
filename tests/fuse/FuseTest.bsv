package FuseTest;

import Z80a::*;
import TestDRAM::*;
import Connectable::*;
import List::*;
import TriState::*;
import Probe::*;

module mkFuseTest(Empty);
    Reg#(UInt#(8)) cycle <- mkReg(0);

    Z80a_ifc cpu <- mkZ80a();
    TestDRAM_ifc dram <- mkTestDRAM("testinit.rmh", "testprog.rmh");

    mkConnection(cpu.bus.data, dram.data);
    mkConnection(cpu.bus.addr, dram.addr);
    mkConnection(cpu.bus.n_wr, dram.n_wr);
    //method Action n_wait(Bit#(1) w);

    Probe#(Bit#(1)) n_m1_probe <- mkProbe();
    Probe#(Bit#(1)) n_mreq_probe <- mkProbe();
    Probe#(Bit#(1)) n_iorq_probe <- mkProbe();
    Probe#(Bit#(1)) n_rd_probe <- mkProbe();
    Probe#(Bit#(1)) n_wr_probe <- mkProbe();
    Probe#(Bit#(1)) n_rfsh_probe <- mkProbe();
    Probe#(Bit#(1)) n_busack_probe <- mkProbe();
    Probe#(Bit#(16)) addr_probe <- mkProbe();
    Probe#(Bit#(8)) cpu_data_probe <- mkProbe();
    Probe#(Bit#(8)) dram_data_probe <- mkProbe();

    TriState#(Bit#(8)) cpu_data_probe_tri <- mkTriState(False, ?);
    mkConnection(cpu.bus.data, cpu_data_probe_tri.io);
    TriState#(Bit#(8)) dram_data_probe_tri <- mkTriState(False, ?);
    mkConnection(dram.data, dram_data_probe_tri.io);

    rule startdump(cycle == 0);
        $dumpvars();
    endrule

    rule probes;
        n_m1_probe <= cpu.bus.n_m1();
        n_mreq_probe <= cpu.bus.n_mreq();
        n_iorq_probe <= cpu.bus.n_iorq();
        n_rd_probe <= cpu.bus.n_rd();
        n_wr_probe <= cpu.bus.n_wr();
        n_rfsh_probe <= cpu.bus.n_rfsh();
        n_busack_probe <= cpu.n_busack();
        addr_probe <= cpu.bus.addr();
        cpu_data_probe <= cpu_data_probe_tri;
        dram_data_probe <= dram_data_probe_tri;
    endrule

    rule dont_wait;
        cpu.bus.n_wait(1);
    endrule

    rule inc_cycle;
       cycle <= cycle + 1;
    endrule

    rule finished(cycle > 200);
        $display("Test timed out after 200 cycles");
        $finish(-1);
    endrule
endmodule

endpackage
