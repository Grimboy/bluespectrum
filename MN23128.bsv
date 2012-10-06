package MN23128;

import FIFO::*;
import FIFOF::*;
import GetPut::*;
import BRAM::*;
import Vector::*;
import TriState::*;
import RegFile::*;

(* always_ready, always_enabled *)
interface MN23128_ifc;
    // system control
    method Action cs(Bit#(1) cs);
    method Action n_oe(Bit#(1) n_oe);
    method Action n_ce(Bit#(1) n_ce);

    // address bus
    method Action addr(Bit#(14) addr);

    // data bus - output only but still needs to be tristate
    interface Inout#(Bit#(8)) data;
endinterface

(* synthesize *)
module mkMN23128(MN23128_ifc);
    Wire#(Bit#(1)) cs_in <- mkBypassWire;
    Wire#(Bit#(1)) n_oe_in <- mkBypassWire;
    Wire#(Bit#(1)) n_ce_in <- mkBypassWire;

    RegFile#(Bit#(14), Bit#(8)) rom <- mkRegFileFullLoad("rom.rmh"); // XXX: Make a parameter
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
    method Action addr(Bit#(14) a);
        data_out.wset(rom.sub(a));
    endmethod

    // data bus
    interface data = data_tri.io;
endmodule
endpackage
