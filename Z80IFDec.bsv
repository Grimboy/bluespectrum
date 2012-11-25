package Z80IFDec;

import ConfigReg::*;
import FShow::*;
import StmtUtils::*;
import ClientServer::*;
import GetPut::*;

import Z80BusMan::*;
import Z80Types::*;
import Z80Decode::*;

interface Z80IFDec_ifc;
    method Bool inactive();
    method Bool first_cycle();
    method Bool second_cycle();
    method Action start();
endinterface

//(* synthesize *)
module [Module] mkZ80IFDec(Z80BusManager_ifc bus_man, Reg#(Bit#(16)) pc, Reg#(DecodedInstructionT) decoded, Z80IFDec_ifc ifc);
    Reg#(Bit#(8)) instr_buff <- mkRegU;

    ConfigReg#(UInt#(3)) bytes_read <- mkConfigReg(0);
    ConfigReg#(UInt#(2)) cycle_count <- mkReg(0);

    Reg#(Bool) if_cycle_done <- mkReg(False);
    Reg#(Bool) dec_cycle_done <- mkReg(False);

    Reg#(Bool) need_displacement <- mkReg(False);
    Reg#(Bool) displacement_done <- mkReg(False);

    Bool if_done = if_cycle_done && dec_cycle_done && (decoded.incomp == IncNo) && (displacement_done || !need_displacement);
    PulseWire early_done <- mkPulseWire();

    rule inc_cycle_count(cycle_count < 2);
        cycle_count <= cycle_count + 1;
    endrule

    rule if_start(!if_done && !if_cycle_done);
        pc <= pc + 1;
        bus_man.server.request.put(Z80BusManRqT{
            addr: pc,
            payload: tagged BPMemRdRfsh 0
        });
    endrule

    rule if_end(!if_done && !if_cycle_done);
        storeAV(bus_man.server.response.get(), instr_buff);

        if_cycle_done <= True;
    endrule

    rule dec_start(!if_done && if_cycle_done && !dec_cycle_done);
        if (need_displacement) begin
            decoded.displacement <= instr_buff;
            displacement_done <= True;
        end else begin
            let d = decode_simple(instr_buff, bytes_read == 0 ? IncNo : decoded.incomp);
            $display(fshow(d));
            decoded <= d;
            need_displacement <= d.need_displacement;
        end
        bytes_read <= bytes_read + 1;
        dec_cycle_done <= True;
    endrule

    rule dec_again(!if_done && if_cycle_done && dec_cycle_done);
        dropAV(bus_man.server.response.get());
        if_cycle_done <= False;
        dec_cycle_done <= False;
    endrule

    method Bool first_cycle();
        return (cycle_count == 0) && !if_done; // && !if_done is to keep compiler happy
    endmethod

    method Bool second_cycle();
        return (cycle_count == 1) && !if_done; // && !if_done is to keep compiler happy
    endmethod

    method Bool inactive();
        return if_done;
    endmethod

    method Action start();
        if_cycle_done <= False;
        dec_cycle_done <= False;
        need_displacement <= False;
        displacement_done <= False;
        cycle_count <= 0;
        bytes_read <= 0;
    endmethod
endmodule

endpackage
