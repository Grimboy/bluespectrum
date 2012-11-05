package TestDRAM;

import FIFO::*;
import FIFOF::*;
import GetPut::*;
import BRAM::*;
import Vector::*;
import TriState::*;
import RegFile::*;
import List::*;

`define INIT_SIZE 16

(* always_ready, always_enabled *)
interface TestDRAM_ifc;
    // system control
    method Action cs(Bit#(1) cs);
    method Action n_oe(Bit#(1) n_oe);
    method Action n_ce(Bit#(1) n_ce);

    // address bus
    method Action addr(Bit#(16) addr);

    // data bus - output only but still needs to be tristate
    interface Inout#(Bit#(8)) data;
endinterface

//(* synthesize *)
module mkTestDRAM#(String initfile, String progfile)(TestDRAM_ifc);
    Wire#(Bit#(1)) cs_in <- mkBypassWire;
    Wire#(Bit#(1)) n_oe_in <- mkBypassWire;
    Wire#(Bit#(1)) n_ce_in <- mkBypassWire;

    Reg#(Bool) init_done <- mkReg(False);
    RegFile#(Bit#(16), Bit#(8)) ram_init <- mkRegFileFullLoad(initfile);
    RegFile#(Bit#(16), Bit#(8)) ram_prog <- mkRegFileFullLoad(progfile);
    RegFile#(Bit#(16), Bit#(8)) ram = init_done ? ram_prog : ram_init;
    RWire#(Bit#(8)) data_out <- mkRWire;
    TriState#(Bit#(8)) data_tri <- mkTriState(isValid(data_out.wget()), fromMaybe(?, data_out.wget()));

    // control
    method Action cs(Bit#(1) c);
        cs_in <= c;
    endmethod

    method Action n_oe(Bit#(1) o);
        n_oe_in <= o;
    endmethod

    method Action n_ce(Bit#(1) c);
        n_ce_in <= c;
    endmethod

    // address bus
    method Action addr(Bit#(16) a);
        data_out.wset(ram.sub(a));
        if (a >= `INIT_SIZE)
            init_done <= True;
    endmethod

    // data bus
    interface data = data_tri.io;
endmodule

endpackage
