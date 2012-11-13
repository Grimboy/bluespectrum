package TestDRAM;

import FIFO::*;
import FIFOF::*;
import GetPut::*;
import BRAM::*;
import Vector::*;
import TriState::*;
import RegFile::*;
import List::*;

`define INIT_SIZE 55

(* always_ready, always_enabled *)
interface TestDRAM_ifc;
    // system control
    method Action cs(Bit#(1) cs);
    method Action n_wr(Bit#(1) n_wr);
    method Action n_oe(Bit#(1) n_oe);
    method Action n_ce(Bit#(1) n_ce);

    // address bus
    method Action addr(Bit#(16) addr);

    // data bus - output only but still needs to be tristate
    interface Inout#(Bit#(8)) data;
endinterface

(* synthesize *)
module mkTestDRAM#(parameter String initfile, parameter String progfile)(TestDRAM_ifc);
    Wire#(Bit#(1)) cs_in <- mkBypassWire;
    Wire#(Bit#(1)) n_wr_in <- mkBypassWire;
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

    method Action n_wr(Bit#(1) w);
        n_wr_in <= w;
    endmethod

    method Action n_oe(Bit#(1) o);
        n_oe_in <= o;
    endmethod

    method Action n_ce(Bit#(1) c);
        n_ce_in <= c;
    endmethod

    // address bus
    method Action addr(Bit#(16) a);
        if (n_wr_in > 0) begin
            data_out.wset(ram.sub(a));
            $display("Read address %h: %h", a, ram.sub(a));
            if (a == `INIT_SIZE) begin
                $display("Swapping RAMs");
                init_done <= True;
            end
        end else begin
            ram.upd(a, data_tri);
        end
    endmethod

    // data bus
    interface data = data_tri.io;
endmodule

endpackage
