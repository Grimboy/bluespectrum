package Z80a;

import FShow::*;
import FIFO::*;
import FIFOF::*;
import GetPut::*;
import BRAM::*;
import Vector::*;
import TriState::*;
import Z80aTypes::*;
import ConfigReg::*;

// Next milestone: Z80 can execute LD reg, literal; LD reg, reg and ADD reg instructions loaded from ROM

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

(* synthesize *)
module mkZ80a #(parameter Bool no_exec) (Z80a_ifc);
    // Submodules
    ALU_ifc alu <- mkALU;

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

    Reg#(UInt#(3)) bytes_read <- mkReg(0);
    Reg#(Bool) need_displacement <- mkReg(False);
    Reg#(Bool) decoding_done <- mkReg(False);
    Reg#(Bool) displacement_done <- mkReg(False);
    Bool if_done = decoding_done && (displacement_done || !need_displacement);
    Bool is_m1 = !decoding_done && (bytes_read == 0);
    Reg#(Bool) op1_done <- mkReg(False);
    Reg#(Bool) op2_done <- mkReg(False);
    Reg#(Bool) alu_done <- mkReg(False);
    Reg#(Bool) written_back <- mkReg(False);

    // Registery registers
    Reg#(Bit#(16)) pc <- mkReg(0);
    Reg#(Bit#(8)) displacement <- mkRegU;
    Reg#(Bit#(8)) instr_b1 <- mkRegU;
    Reg#(Bit#(8)) res <- mkRegU;
    RegisterFileIfc rf <- mkRegisterFile;

    // Decoded
    Reg#(DecodedInstructionT) decoded <- mkRegU;

    function Action next_instruction();
        return (action
            $display("---Instruction Start---");
            mem_cycle <= 0;
            sub_cycle <= 0;
            bytes_read <= 0;
            need_displacement <= False;
            decoding_done <= False;
            displacement_done <= False;
            op1_done <= False;
            op2_done <= False;
            alu_done <= False;
            written_back <= False;
        endaction);
    endfunction


    rule m1_out_start(is_m1 && sub_cycle == 0);
        n_m1_out <= 0;
    endrule

    rule m1_out_end(is_m1 && sub_cycle == 2);
        n_m1_out <= 1;
    endrule

    rule m1_pc_out(!if_done && sub_cycle == 0);
        addr_out <= pc;
        n_mreq_out <= 0; // XXX: Should be on following negative clk edge
        n_rd_out <= 0; // XXX: "
        sub_cycle <= sub_cycle + 1;
        pc <= pc + 1;
    endrule

    rule m1_n_wait(!if_done && sub_cycle == 1 && (n_wait_in > 0));
        sub_cycle <= sub_cycle + 1;
    endrule

    rule m1_decode(!if_done && sub_cycle == 2);
        // TODO: DRAM Refresh
        if (need_displacement) begin
            displacement <= data_tri;
            displacement_done <= True;
        end else begin
            instr_b1 <= data_tri;
            DecodedInstructionT d = decode_simple(data_tri, bytes_read == 0 ? IncNo : decoded.incomp);
            decoded <= d;
            decoding_done <= d.incomp == IncNo;
            need_displacement <= d.displacement;
        end
        bytes_read <= bytes_read + 1;
        n_mreq_out <= 1;
        n_rd_out <= 1;
        sub_cycle <= 0;
    endrule

/*
    (* preempts = "do_no_exec, m1t4_ldgetreg" *)
    (* preempts = "do_no_exec, m1t4_ldgetacc" *)
    (* preempts = "do_no_exec, m2t1_ldimmgetnextbyte" *)
    rule do_no_exec(if_done && no_exec);
        next_instruction();
    endrule
    */

    rule ld8_getreg(if_done && sub_cycle == 0 && decoded.op == OpLd &&& decoded.src1 matches tagged DirectOperand (tagged DOReg8 .r));
        res <= rf.read_8b(r);
        next_instruction();
    endrule

    rule ld8_putreg(is_m1 && sub_cycle == 0 && decoded.op == OpLd &&& decoded.dest matches tagged DirectOperand (tagged DOReg8 .r));
        rf.write_8b(r, res);
    endrule

    rule add1(if_done && sub_cycle == 0 && decoded.op == OpAdd
      &&& decoded.src1 matches tagged DirectOperand (tagged DOReg8 .r1)
      &&& decoded.src2 matches tagged DirectOperand (tagged DOReg8 .r2));
        alu.request.put(ALUReqT{in1: rf.read_4b(r1, False), in2: rf.read_4b(r2, False), carry_in: False, is_sub: False});
        next_instruction();
    endrule

    rule add2(is_m1 && sub_cycle == 0 && decoded.op == OpAdd
      &&& decoded.src1 matches tagged DirectOperand (tagged DOReg8 .r1)
      &&& decoded.src2 matches tagged DirectOperand (tagged DOReg8 .r2));
        ALURespT resp <- alu.response.get();
        alu.request.put(ALUReqT{in1: rf.read_4b(r1, True), in2: rf.read_4b(r2, True), carry_in: resp.carry_out, is_sub: False});
        rf.write_4b(r1, False, resp.out);
    endrule

    rule add3(is_m1 && sub_cycle == 1 && decoded.op == OpAdd
      &&& decoded.src1 matches tagged DirectOperand (tagged DOReg8 .r1)
      &&& decoded.src2 matches tagged DirectOperand (tagged DOReg8 .r2));
        ALURespT resp <- alu.response.get();
        rf.write_4b(r1, True, resp.out);
    endrule

    rule halt(if_done && sub_cycle == 0 && decoded.op == OpHalt);
        $finish();
    endrule

    rule ldimmgetnextbyte(if_done && sub_cycle == 0 && decoded.op == OpLd &&& decoded.src1 matches tagged DirectOperand (tagged DONext8Bits));
        addr_out <= pc;
        n_mreq_out <= 0; // XXX: Should be on following negative clk edge
        n_rd_out <= 0; // XXX: "
        sub_cycle <= sub_cycle + 1;
        pc <= pc + 1;
    endrule

    rule ldimmwait(if_done && sub_cycle == 1 && (n_wait_in > 0) && decoded.op == OpLd &&& decoded.src1 matches tagged DirectOperand (tagged DONext8Bits));
        sub_cycle <= sub_cycle + 1;
    endrule

    rule ldimmread(if_done && sub_cycle == 2 && decoded.op == OpLd &&& decoded.src1 matches tagged DirectOperand (tagged DONext8Bits));
        res <= data_tri;
        next_instruction();
    endrule

    rule trace_decoded_instruction(if_done && sub_cycle == 0);
        $display(fshow(decoded));
    endrule

    rule trace_regs;
        $display("pc: %h m: %h s: %h i: %h res: %h",
            pc, mem_cycle, sub_cycle, instr_b1, res);
        $display("a: %h f: %h b: %h c: %h d: %h e: %h h: %h l: %h w: %h z: %h s: %h p: %h",
            rf.read_8b(RgA), rf.read_8b(RgF),
            rf.read_8b(RgB), rf.read_8b(RgC),
            rf.read_8b(RgD), rf.read_8b(RgE),
            rf.read_8b(RgH), rf.read_8b(RgL),
            rf.read_8b(RgW), rf.read_8b(RgZ),
            rf.read_8b(RgS), rf.read_8b(RgP));
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

typedef Server#(ALUReqT, ALURespT) ALU_ifc;

module mkALU(ALU_ifc);
    FIFO#(ALURespT) f_out <- mkFIFO;

    interface Put request;
        method Action put(ALUReqT req);
            Bit#(5) in1 = extend(req.in1);
            Bit#(5) in2 = extend(req.in2);
            Bit#(5) carry_in = (req.carry_in ? 1 : 0);
            Bool is_sub = req.is_sub;
            Bit#(5) res = in1 + (is_sub ? -in2 : in2) + carry_in;
            ALURespT resp = ALURespT{carry_out: res[4] > 0, out: res[3:0]};
            $display("ALU REQ in1: %h, in2: %h, carry_in: %h, is_sub: %h", in1, in2, carry_in, is_sub ? 1 : 0);
            $display("ALU RESP carry_out: %h, out: %h", resp.carry_out ? 1 : 0, resp.out);
            f_out.enq(resp);
        endmethod
    endinterface
    interface response = toGet(f_out);
endmodule

interface RegisterFileIfc;
    method Action write_4b(Reg8T regg, Bool high, Bit#(4) data);
    method Bit#(4) read_4b(Reg8T regg, Bool high);
    method Action write_8b(Reg8T regg, Bit#(8) data);
    method Bit#(8) read_8b(Reg8T regg);
    method Action write_16b(Reg16T regg, Bit#(16) data);
    method Bit#(16) read_16b(Reg16T regg);
endinterface

module mkRegisterFile(RegisterFileIfc); // Use RegFile module?
    Vector#(TExp#(SizeOf#(Reg8T)), Reg#(Bit#(8))) rf <- replicateM(mkReg(0));
    Reg#(Bool) is_shadow <- mkReg(False); // XXX: Might be bad idea because shadow registers are switched in two stages.

    method Action write_4b(Reg8T regg, Bool high, Bit#(4) data);
        rf[pack(regg)][(high ? 7 : 3):(high ? 4 : 0)] <= data;
    endmethod 

    method Bit#(4) read_4b(Reg8T regg, Bool high);
        return rf[pack(regg)][(high ? 7 : 3):(high ? 4 : 0)];
    endmethod 

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
