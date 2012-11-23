package TestDRAM;

import FIFO::*;
import FIFOF::*;
import GetPut::*;
import BRAM::*;
import Vector::*;
import TriState::*;
import RegFile::*;
import List::*;
import StmtFSM::*;
import StmtUtils::*;

`define INIT_SIZE 56

interface TestDRAM_ifc;
    method Action dump();

    // system control
    (* always_ready, always_enabled *)
    method Action cs(Bit#(1) cs);
    (* always_ready, always_enabled *)
    method Action n_wr(Bit#(1) n_wr);
    (* always_ready, always_enabled *)
    method Action n_oe(Bit#(1) n_oe);
    (* always_ready, always_enabled *)
    method Action n_ce(Bit#(1) n_ce);

    // address bus
    (* always_ready, always_enabled *)
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

    Reg#(UInt#(3)) init_doneness <- mkReg(0);
    RegFile#(Bit#(16), Bit#(8)) ram_init <- mkRegFileFullLoad(initfile);
    RegFile#(Bit#(16), Bit#(8)) ram_prog <- mkRegFileFullLoad(progfile);
    RegFile#(Bit#(16), Bit#(8)) ram = (init_doneness == 2) ? ram_prog : ram_init;
    RWire#(Bit#(8)) data_out <- mkRWire;
    TriState#(Bit#(8)) data_tri <- mkTriState(isValid(data_out.wget()), fromMaybe(?, data_out.wget()));

    Reg#(Bool) dumping <- mkReg(False);
    Reg#(File) dumpfile <- mkRegU;
    Reg#(Bit#(16)) dumpaddr <- mkRegU;

    Stmt dumpfsmstmt =
    (seq
        storeAV($fopen("dram_dump.rmh"), dumpfile);
        $fdisplay(dumpfile, "@0000");
        for (dumpaddr <= 0; True; dumpaddr <= dumpaddr + 1) seq
            $fdisplay(dumpfile, "%h", ram_prog.sub(dumpaddr));
            if (dumpaddr == ((1 << 16) - 1))
                break;
        endseq
        $fclose(dumpfile);
        $finish();
    endseq);

    FSM dumpfsm <- mkFSM(dumpfsmstmt);

    method Action dump();
        dumpfsm.start;
        dumping <= True;
    endmethod

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
        if (!dumping)
            if (n_wr_in > 0) begin
                data_out.wset(ram.sub(a));
                $display("Read address %h: %h", a, ram.sub(a));
                if (a == `INIT_SIZE) begin
                    init_doneness <= init_doneness + 1;
                    if (init_doneness == 1)
                        $display("Swapped RAMs");
                end
            end else begin
                $display("Wrote address %h: %h", a, data_tri);
                ram.upd(a, data_tri);
            end
    endmethod

    // data bus
    interface data = data_tri.io;
endmodule

endpackage
