package TestDRAM;

import FIFO::*;
import FIFOF::*;
import GetPut::*;
import BRAM::*;
import Vector::*;
import TriState::*;
import RegFile::*;
import FuseTestTypes::*;
import List::*;

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
module mkTestDRAM#(List#(MemoryRunT) mem)(TestDRAM_ifc);
    Wire#(Bit#(1)) cs_in <- mkBypassWire;
    Wire#(Bit#(1)) n_oe_in <- mkBypassWire;
    Wire#(Bit#(1)) n_ce_in <- mkBypassWire;

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
        for (Integer i=0;i<length(mem);i=i+1) begin
            MemoryRunT run = mem[i];
            for (Integer j=0;j<length(run.payload);j=j+1) begin
                if (a == (run.startaddr + fromInteger(j))) begin
                    data_out.wset(run.payload[j]);
                end
            end
        end
    endmethod

    // data bus
    interface data = data_tri.io;
endmodule

endpackage
