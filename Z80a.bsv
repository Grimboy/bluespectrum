package Z80a;

import FIFO::*;
import FIFOF::*;
import GetPut::*;
import BRAM::*;
import Vector::*;
import TriState::*;
import Z80aTypes::*;
import ConfigReg::*;

// Next milestone: Z80 can execute LD reg, literal; LD reg, reg and ADD reg instructions loaded from ROM

(* always_ready, always_enabled *)
interface Z80a_ifc;
    // system control
    method Bit#(1) n_m1();
    method Bit#(1) n_mreq(); // XXX: Should be tristate
    method Bit#(1) n_iorq(); // "
    method Bit#(1) n_rd(); // "
    method Bit#(1) n_wr(); // "
    method Bit#(1) n_rfsh();

    // cpu control
    method Bit#(1) n_halt();
    method Action n_wait(Bit#(1) w);
    method Action n_int(Bit#(1) i);
    method Action n_nmi(Bit#(1) n);
    method Action n_reset(Bit#(1) r);

    // cpu bus control
    method Action n_busrq(Bit#(1) w);
    method Bit#(1) n_busack();

    // address bus
    method Bit#(16) addr();

    // data bus
    interface Inout#(Bit#(8)) data;
endinterface

module mkDLWireU(Wire#(element_type))
    provisos (Bits#(element_type, element_width));

    ConfigReg#(element_type) storage <- mkConfigRegU;
    Wire#(element_type) internal_wire <- mkDWire(storage);

    method Action _write(element_type x1);
        storage <= x1;
        internal_wire <= x1;
    endmethod

    method element_type _read();
        return internal_wire;
    endmethod
endmodule

(* synthesize *)
module mkZ80a(Z80a_ifc);
    // Input/Output Wires
    Wire#(Bit#(1)) n_m1_out <- mkDLWireU;
    Wire#(Bit#(1)) n_mreq_out <- mkDLWireU;
    Wire#(Bit#(1)) n_rd_out <- mkDLWireU;

    Wire#(Bit#(16)) addr_out <- mkDLWireU;
    RWire#(Bit#(8)) data_out <- mkRWire;
    TriState#(Bit#(8)) data_tri <- mkTriState(isValid(data_out.wget()), fromMaybe(?, data_out.wget()));

    Wire#(Bit#(1)) n_wait_in <- mkBypassWire;

    // Cyclic state
    Reg#(UInt#(3)) mem_cycle <- mkReg(0);
    Reg#(UInt#(3)) sub_cycle <- mkReg(0);

    // Registery registers
    Reg#(Bit#(16)) pc <- mkReg(0);
    Reg#(Bit#(8)) instr_b1 <- mkRegU;
    Reg#(Bit#(8)) acc <- mkReg(0);
    Reg#(Bit#(8)) res <- mkRegU;
    RegisterFileIfc rf <- mkRegisterFile;

    // Decoded
    Reg#(DecodedInstructionT) decoded <- mkRegU;

    rule m1t1_pc_out(mem_cycle == 0 && sub_cycle == 0);
        addr_out <= pc;
        n_m1_out <= 0;
        n_mreq_out <= 0; // XXX: Should be on following negative clk edge
        n_rd_out <= 0; // XXX: "
        sub_cycle <= sub_cycle + 1;
        pc <= pc + 1;
    endrule

    rule m1t2n_wait(mem_cycle == 0 && sub_cycle == 1 && (n_wait_in > 0));
        sub_cycle <= sub_cycle + 1;
    endrule

    rule m1t3_decode(mem_cycle == 0 && sub_cycle == 2);
        // TODO: DRAM Refresh
        instr_b1 <= data_tri;
        decoded <= decode_simple(data_tri);
        n_mreq_out <= 1;
        n_rd_out <= 1;
        sub_cycle <= sub_cycle + 1;
    endrule

    rule m1t4_ldgetreg(mem_cycle == 0 && sub_cycle == 3 && decoded.op == OpLd &&& decoded.src1 matches tagged DirectOperand (tagged DOReg8 .r));
        res <= rf.read_8b(r);
        sub_cycle <= 0;
    endrule

    rule m1t4_ldgetacc(mem_cycle == 0 && sub_cycle == 3 && decoded.op == OpLd &&& decoded.src1 matches tagged DirectOperand (tagged DOAcc));
        res <= acc;
        sub_cycle <= 0;
    endrule

    rule m1t1_ldputreg(mem_cycle == 0 && sub_cycle == 0 && decoded.op == OpLd &&& decoded.dest matches tagged DirectOperand (tagged DOReg8 .r));
        rf.write_8b(r, res);
    endrule

    rule m1t1_ldputacc(mem_cycle == 0 && sub_cycle == 0 && decoded.op == OpLd &&& decoded.dest matches tagged DirectOperand (tagged DOAcc));
        acc <= res;
    endrule

    rule m1t4_ldimmgetnextbytestart(mem_cycle == 0 && sub_cycle == 3 && decoded.op == OpLd &&& decoded.src1 matches tagged DirectOperand (tagged DONext8Bits));
        mem_cycle <= 1;
        sub_cycle <= 0;
    endrule

    rule m2t1_ldimmgetnextbyte(mem_cycle == 1 && sub_cycle == 0 && decoded.op == OpLd &&& decoded.src1 matches tagged DirectOperand (tagged DONext8Bits));
        addr_out <= pc;
        n_mreq_out <= 0; // XXX: Should be on following negative clk edge
        n_rd_out <= 0; // XXX: "
        sub_cycle <= sub_cycle + 1;
        pc <= pc + 1;
    endrule

    rule m2t2_ldimmwait(mem_cycle == 1 && sub_cycle == 1 && (n_wait_in > 0) && decoded.op == OpLd &&& decoded.src1 matches tagged DirectOperand (tagged DONext8Bits));
        sub_cycle <= sub_cycle + 1;
    endrule

    rule m2t3_ldimmread(mem_cycle == 1 && sub_cycle == 2 && decoded.op == OpLd &&& decoded.src1 matches tagged DirectOperand (tagged DONext8Bits));
        res <= data_tri;
        mem_cycle <= 0;
        sub_cycle <= 0;
    endrule

    rule trace_regs;
        $display("pc: %h m: %h s: %h i: %h a: %h res: %h", pc, mem_cycle, sub_cycle, instr_b1, acc, res);
        $display("b: %h c: %h d: %h e: %h h: %h l: %h",
            rf.read_8b(RgB), rf.read_8b(RgC),
            rf.read_8b(RgD), rf.read_8b(RgE),
            rf.read_8b(RgH), rf.read_8b(RgL));
    endrule

    rule trace_wires;
        $display("addr_out: %h", addr_out);
    endrule

    // system control
    method Bit#(1) n_m1();
        return n_m1_out;
    endmethod

    method Bit#(1) n_mreq();
        return n_mreq_out;
    endmethod

    method Bit#(1) n_iorq();
        return 0;
    endmethod

    method Bit#(1) n_rd();
        return n_rd_out;
    endmethod

    method Bit#(1) n_wr();
        return 0;
    endmethod

    method Bit#(1) n_rfsh();
        return 0;
    endmethod

    // cpu control
    method Bit#(1) n_halt();
        return 0;
    endmethod

    method Action n_wait(Bit#(1) w);
        n_wait_in <= w;
    endmethod

    method Action n_int(Bit#(1) i);
    endmethod

    method Action n_nmi(Bit#(1) n);
    endmethod

    method Action n_reset(Bit#(1) r);
    endmethod

    method Action n_busrq(Bit#(1) w);
    endmethod

    method Bit#(1) n_busack();
        return 1;
    endmethod

    // address bus
    method Bit#(16) addr();
        return addr_out;
    endmethod

    // data bus
    interface data = data_tri.io;
endmodule

interface Bram_If; 
   method Action write(Bit#(8) data, Bit#(8) address); 
endinterface 

interface RegisterFileIfc;
    method Action write_8b(Reg8T regg, Bit#(8) data);
    method Bit#(8) read_8b(Reg8T regg);
    method Action write_16b(Reg16T regg, Bit#(16) data);
    method Bit#(16) read_16b(Reg16T regg);
endinterface

module mkRegisterFile(RegisterFileIfc); // Use RegFile module?
    Vector#(TExp#(SizeOf#(Reg8T)), Reg#(Bit#(8))) rf <- replicateM(mkReg(0));
    Reg#(Bool) is_shadow <- mkReg(False); // XXX: Might be bad idea because shadow registers are switched in two stages.

    method Action write_8b(Reg8T regg, Bit#(8) data);
        rf[pack(regg)] <= data;
    endmethod 

    method Bit#(8) read_8b(Reg8T regg);
        return rf[pack(regg)];
    endmethod 

    method Action write_16b(Reg16T regg, Bit#(16) data);
        rf[pack(regg) * 2] <= data[15:8];
        rf[pack(regg) * 2 + 1] <= data[7:0];
    endmethod 

    method Bit#(16) read_16b(Reg16T regg);
        return {rf[pack(regg) * 2], rf[pack(regg) * 2 + 1]};
    endmethod 
endmodule

endpackage
