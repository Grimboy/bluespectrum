package Z80;

import FShow::*;
import GetPut::*;
import ConfigReg::*;
import Probe::*;
import Connectable::*;
import StmtUtils::*;
import ClientServer::*;

import Z80Types::*;
import Z80Alu::*;
import Z80BusMan::*;
import Z80RF::*;
import Z80IFDec::*;
import Z80Decode::*;

//import TraceUtils::*;

// Next milestone: Z80 can execute LD reg, literal; LD reg, reg and ADD reg instructions loaded from ROM

module mkDLWire#(element_type init) (Wire#(element_type))
    provisos (Bits#(element_type, element_width));

    ConfigReg#(element_type) storage <- mkConfigReg(init);
    Wire#(element_type) internal_wire <- mkDWire(storage);

    method Action _write(element_type x1);
        storage <= x1;
        internal_wire <= x1;
    endmethod

    method element_type _read();
        return internal_wire;
    endmethod
endmodule

function Module#(Wire#(element_type)) mkDLWireU
    provisos (Bits#(element_type, element_width)) = mkDLWire(?);

(* always_ready, always_enabled *)
interface Z80a_ifc;
    // cpu control
    method Bit#(1) n_halt();
    method Action n_int(Bit#(1) i);
    method Action n_nmi(Bit#(1) n);
    method Action n_reset(Bit#(1) r);

    // cpu bus control
    method Action n_busrq(Bit#(1) w);
    method Bit#(1) n_busack();

    // managed bus
    interface Z80Bus_ifc bus;
endinterface

(* synthesize *)
module mkZ80a(Z80a_ifc);
    // Input/Output Wires
    Wire#(Bit#(1)) n_halt_wire <- mkDWire(1);

    // Registery registers
    Reg#(DecodedInstructionT) decoded <- mkReg(decoded_nop);
    Reg#(Bool) ie <- mkReg(False);
    Reg#(Bit#(2)) im <- mkReg(0); // XXX: Is IM 0 at startup?
    Reg#(Bit#(16)) pc <- mkReg(0);
    Reg#(Bit#(8)) displacement <- mkRegU;
    Reg#(Bit#(8)) res <- mkRegU;
    Reg#(Bit#(8)) tmp8 <- mkRegU;

    Reg#(Bool) halted <- mkReg(False);

    // Submodules
    ALU_ifc alu <- mkALU;
    Z80BusManager_ifc bus_man <- mkZ80BusManager;
    RegisterFileIfc rf <- mkRegisterFile;
    Z80IFDec_ifc ifdec <- mkZ80IFDec(bus_man, pc, decoded);

    // Cyclic state
    Reg#(UInt#(3)) mem_cycle <- mkReg(0);

    Reg#(Bool) m1_done <- mkReg(False);
    Bool m1_done_nc = ifdec.inactive() && m1_done;

    PulseWire first_cycle <- mkPulseWire();
    Bool ifex_overlap_cycle_1 = ifdec.first_cycle();
    Bool ifex_overlap_cycle_2 = ifdec.second_cycle();

    // Decoded

    function Action next_instruction();
        return (action
            $display("---Instruction Start---");
            mem_cycle <= 0;
            m1_done <= False;
            ifdec.start();
        endaction);
    endfunction

    /*** IF/DEC ***/

    rule nop(ifdec.inactive() && decoded.op == OpNop);
        dropAV(bus_man.server.response.get());
        next_instruction();
    endrule

    /*** Ld16 ***/
    
    rule ld16_start(ifdec.inactive() &&& decoded.op == OpLd16 &&& !m1_done);
        dropAV(bus_man.server.response.get());
        m1_done <= True;
    endrule

    rule ld16_immgetnextbyte(m1_done_nc &&& decoded.op == OpLd16 &&& decoded.src1 matches tagged DirectOperand (tagged DONext16Bits));
        first_cycle.send();
        bus_man.server.request.put(Z80BusManRqT{
            addr: pc,
            payload: BPMemRd
        });
        pc <= pc + 1;
    endrule

    rule ld16_immread(m1_done_nc &&& decoded.op == OpLd16 &&& decoded.src1 matches tagged DirectOperand (tagged DONext16Bits));
        storeAV(bus_man.server.response.get(), res);
        if (mem_cycle == 0) begin
            mem_cycle <= 1;
        end else
            next_instruction();
    endrule

    (* mutually_exclusive = "ld16_immread, ld16_setlimm" *)
    rule ld16_setlimm(m1_done_nc &&& first_cycle && decoded.op == OpLd16 &&& decoded.dest matches tagged DirectOperand (tagged DOReg16 .r) &&& mem_cycle == 1);
        rf.write_8b(low_reg_byte(r), res);
    endrule

    rule ld16_sethimm(ifex_overlap_cycle_1 &&& decoded.op == OpLd16 &&& decoded.dest matches tagged DirectOperand (tagged DOReg16 .r));
        rf.write_8b(high_reg_byte(r), res);
    endrule

    /*** Ld8 ***/

    rule ld8_getreg(ifdec.inactive() && mem_cycle == 0 && decoded.op == OpLd8 &&& decoded.src1 matches tagged DirectOperand (tagged DOReg8 .r));
        dropAV(bus_man.server.response.get());
        res <= rf.read_8b(r);
        if (decoded.dest matches tagged DirectOperand .*)
            next_instruction();
        else
            mem_cycle <= 1;
    endrule

    rule ld8_putreg(ifex_overlap_cycle_1 && decoded.op == OpLd8 &&& decoded.dest matches tagged DirectOperand (tagged DOReg8 .r));
        rf.write_8b(r, res);
    endrule

    rule ld8_start(ifdec.inactive() && mem_cycle == 0 && decoded.op == OpLd8 &&& (decoded.src1 matches tagged DirectOperand (tagged DONext8Bits) ? True : decoded.src1 matches tagged IndirectOperand (tagged IOReg16 .r) ? True : False) &&& !m1_done);
        dropAV(bus_man.server.response.get());
        m1_done <= True;
    endrule

    rule ld8_immgetnextbyte(m1_done_nc && mem_cycle == 0 && decoded.op == OpLd8 &&& decoded.src1 matches tagged DirectOperand (tagged DONext8Bits));
        bus_man.server.request.put(Z80BusManRqT{
            addr: pc,
            payload: BPMemRd
        });
        pc <= pc + 1;
    endrule

    rule ld8_immread(m1_done_nc && mem_cycle == 0 && decoded.op == OpLd8 &&& decoded.src1 matches tagged DirectOperand (tagged DONext8Bits));
        storeAV(bus_man.server.response.get(), res);
        if (decoded.dest matches tagged DirectOperand .*)
            next_instruction();
        else
            mem_cycle <= 1;
    endrule

    //rule ld8_immread(ifdec.inactive() && mem_cycle == 0 && decoded.op == OpLd8 &&& decoded.src1 matches tagged IndirectOperand (tagged IONext16Bits));
    rule ld8_readindirregstart(m1_done_nc && mem_cycle == 0 && decoded.op == OpLd8 &&& decoded.src1 matches tagged IndirectOperand (tagged IOReg16 .r));
        bus_man.server.request.put(Z80BusManRqT{
            addr: rf.read_16b(r),
            payload: BPMemRd
        });
    endrule

    rule ld8_readindirregend(m1_done_nc && mem_cycle == 0 && decoded.op == OpLd8 &&& decoded.src1 matches tagged IndirectOperand (tagged IOReg16 .r));
        storeAV(bus_man.server.response.get(), res);
        if (decoded.dest matches tagged DirectOperand .*)
            next_instruction();
        else
            mem_cycle <= 1;
    endrule

    //rule ld8_immgetnextbyte(ifdec.inactive() && mem_cycle == 0 && decoded.op == OpLd8 &&& decoded.dest matches tagged IndirectOperand (tagged IONext16Bits));
    rule ld8_writeindirregstart(ifdec.inactive() && mem_cycle == 1 && decoded.op == OpLd8 &&& decoded.dest matches tagged IndirectOperand (tagged IOReg16 .r));
        bus_man.server.request.put(Z80BusManRqT{
            addr: rf.read_16b(r),
            payload: tagged BPMemWrData res
        });
    endrule

    rule ld8_writeindirregfinish(ifdec.inactive() && mem_cycle == 1 && decoded.op == OpLd8 &&& decoded.dest matches tagged IndirectOperand (tagged IOReg16 .r));
        dropAV(bus_man.server.response.get());
        next_instruction();
    endrule

    /*** JUMP/BRANCH ***/

    rule op_jp_start(ifdec.inactive() && decoded.op == OpJp && !m1_done);
        dropAV(bus_man.server.response.get());
        m1_done <= True;
    endrule

    rule op_jp_readstart(m1_done_nc && decoded.op == OpJp &&& decoded.src1 matches tagged DirectOperand (tagged DONext16Bits));
        bus_man.server.request.put(Z80BusManRqT{
            addr: pc,
            payload: BPMemRd
        });
        pc <= pc + 1;
    endrule

    rule op_jp_readfin(m1_done_nc && decoded.op == OpJp &&& decoded.src1 matches tagged DirectOperand (tagged DONext16Bits));
        if (mem_cycle == 0) begin
            mem_cycle <= 1;
            storeAV(bus_man.server.response.get(), res);
        end else begin
            let jpaddrhigh <- bus_man.server.response.get();
            $display("Setting pc to %h", {jpaddrhigh, res});
            pc <= {jpaddrhigh, res};
            next_instruction();
        end
    endrule

    /*** SWAP ***/

    rule op_exaf(ifdec.inactive() && decoded.op == OpExAF);
        rf.swap_af();
        dropAV(bus_man.server.response.get());
        next_instruction();
    endrule

    rule op_exgp(ifdec.inactive() && decoded.op == OpExGP);
        rf.swap_gp();
        dropAV(bus_man.server.response.get());
        next_instruction();
    endrule

    /*** PUSH/POP ***/

    rule push_pop_start(ifdec.inactive() && (decoded.op == OpPush || decoded.op == OpPop) && !m1_done);
        dropAV(bus_man.server.response.get());
        m1_done <= True;
    endrule

    rule push_sp_dec(m1_done_nc && mem_cycle == 0 && decoded.op == OpPush);
        rf.write_16b(RgSP, rf.read_16b(RgSP) - 1);
        mem_cycle <= mem_cycle + 1;
    endrule

    rule push_stackwritestart(m1_done_nc &&& mem_cycle > 0 && decoded.op == OpPush &&& decoded.src1 matches tagged DirectOperand (tagged DOReg16 .r));
        if (mem_cycle == 1)
            bus_man.server.request.put(Z80BusManRqT{
                addr: rf.read_16b(RgSP),
                payload: tagged BPMemWrData rf.read_8b(high_reg_byte(r))
            });
        else
            bus_man.server.request.put(Z80BusManRqT{
                addr: rf.read_16b(RgSP),
                payload: tagged BPMemWrData rf.read_8b(low_reg_byte(r))
            });
    endrule

    rule push_stackwritedone(m1_done_nc &&& mem_cycle > 0 && decoded.op == OpPush);
        dropAV(bus_man.server.response.get());
        if (mem_cycle == 1) begin
            rf.write_16b(RgSP, rf.read_16b(RgSP) - 1);
            mem_cycle <= 2;
        end else
            next_instruction();
    endrule

    rule pop_getbyteaddr(m1_done_nc && decoded.op == OpPop &&& decoded.dest matches tagged DirectOperand (tagged DOReg16 .r));
        let sp = rf.read_16b(RgSP);
        bus_man.server.request.put(Z80BusManRqT{
            addr: sp,
            payload: BPMemRd
        });
        rf.write_16b(RgSP, sp + 1);
        if (mem_cycle == 1 && r != RgSP) begin // r != RgSP is just to keep compiler happy
            rf.write_8b(low_reg_byte(r), res);
        end
    endrule

    rule pop_getbytewrite(m1_done_nc && decoded.op == OpPop);
        storeAV(bus_man.server.response.get(), res);
        if (mem_cycle == 0) begin
            mem_cycle <= 1;
        end else
            next_instruction();
    endrule

    rule pop_seth(ifex_overlap_cycle_1 && decoded.op == OpPop &&& decoded.dest matches tagged DirectOperand (tagged DOReg16 .r));
        rf.write_8b(high_reg_byte(r), res);
    endrule

    /*** ALU ***/

    rule alu_start(ifdec.inactive() && decoded.op == OpAlu8
      &&& decoded.src2 matches tagged IndirectOperand .* &&& !m1_done);
        dropAV(bus_man.server.response.get());
        m1_done <= True;
    endrule

    rule alu_indirfetchstart(m1_done_nc && decoded.op == OpAlu8 &&& decoded.src2 matches tagged IndirectOperand (tagged IOReg16 .r));
        bus_man.server.request.put(Z80BusManRqT{
            addr: rf.read_16b(r),
            payload: BPMemRd
        });
    endrule

    rule alu_indirfetchend(m1_done_nc && decoded.op == OpAlu8 &&& decoded.src1 matches tagged DirectOperand (tagged DOReg8 .rd) &&& decoded.src2 matches tagged IndirectOperand (tagged IOReg16 .*));
        let res <- bus_man.server.response.get();
        alu.request.put(AluReqT{cmd: decoded.alu_op, in1: rf.read_8b(rd), in2: res, flags_in: unpack(rf.read_8b(RgF))});
        next_instruction();
    endrule

    rule alu1(ifdec.inactive() && decoded.op == OpAlu8
      &&& decoded.src1 matches tagged DirectOperand (tagged DOReg8 .r1)
      &&& decoded.src2 matches tagged DirectOperand (tagged DOReg8 .r2));
        dropAV(bus_man.server.response.get());
        alu.request.put(AluReqT{cmd: decoded.alu_op, in1: rf.read_8b(r1), in2: rf.read_8b(r2), flags_in: unpack(rf.read_8b(RgF))});
        next_instruction();
    endrule

    rule alu2(ifex_overlap_cycle_1 && decoded.op == OpAlu8 &&& decoded.dest matches tagged DirectOperand (tagged DOReg8 .r));
        AluRespT resp <- alu.response.get();
        rf.write_8b(r, resp.out);
        res <= pack(resp.flags_out);
    endrule

    rule alu3(ifex_overlap_cycle_2 && decoded.op == OpAlu8);
        rf.write_8b(RgF, res);
    endrule

    /*** Interrupts ***/

    rule op_si(ifdec.inactive() && decoded.op == OpSi &&& decoded.src1 matches tagged DirectOperand (tagged DOLiteral .l));
        ie <= l > 0;
        dropAV(bus_man.server.response.get());
        next_instruction();
    endrule

    rule op_im(ifdec.inactive() && decoded.op == OpIm &&& decoded.src1 matches tagged DirectOperand (tagged DOLiteral .l));
        im <= l;
        dropAV(bus_man.server.response.get());
        next_instruction();
    endrule

    rule op_halt(ifdec.inactive() && decoded.op == OpHalt && !halted);
        dropAV(bus_man.server.response.get());
        halted <= True;
        n_halt_wire <= 0;
        rf.trace_regs(pc);
    endrule

    /*** Trace ***/

    rule trace_regs(!halted);
        $display("pc: %h m: %h res: %h",
            pc, mem_cycle, res);
        $display("a: %h f: %h b: %h c: %h d: %h e: %h h: %h l: %h w: %h z: %h s: %h p: %h i: %h r: %h",
            rf.read_8b(RgA), rf.read_8b(RgF),
            rf.read_8b(RgB), rf.read_8b(RgC),
            rf.read_8b(RgD), rf.read_8b(RgE),
            rf.read_8b(RgH), rf.read_8b(RgL),
            rf.read_8b(RgW), rf.read_8b(RgZ),
            rf.read_8b(RgS), rf.read_8b(RgP),
            rf.read_8b(RgI), rf.read_8b(RgR));
    endrule

    /*** Methods ***/

    // cpu control
    method Bit#(1) n_halt();
        return n_halt_wire;
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

    interface bus = bus_man.bus;
endmodule

endpackage
