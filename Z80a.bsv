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
import Probe::*;
import Connectable::*;
import StmtFSM::*;
import StmtUtils::*;

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
    // Submodules
    ALU_ifc alu <- mkALU;
    Z80BusManager_ifc bus_man <- mkZ80BusManager;

    // Input/Output Wires
    Wire#(Bit#(1)) n_m1_out <- mkDLWireU;

    // Cyclic state
    Reg#(UInt#(3)) mem_cycle <- mkReg(0);
    Reg#(UInt#(3)) sub_cycle <- mkReg(0);

    ConfigReg#(UInt#(3)) bytes_read <- mkConfigReg(0);
    Reg#(Bool) need_displacement <- mkReg(False);
    ConfigReg#(Bool) decoding_done <- mkConfigReg(False);
    Reg#(Bool) displacement_done <- mkReg(False);
    Bool if_done = decoding_done && (displacement_done || !need_displacement);
    Bool inst_first_fetch = !decoding_done && (bytes_read == 0);

    PulseWire inst_fetch_first_cycle <- mkPulseWire();
    PulseWire inst_fetched <- mkPulseWire();
    Bool ifex_overlap_cycle_1 = inst_first_fetch && inst_fetch_first_cycle;
    Bool ifex_overlap_cycle_2 = inst_first_fetch && !inst_fetch_first_cycle && !inst_fetched;
    Reg#(Bool) op1_done <- mkReg(False);
    Reg#(Bool) op2_done <- mkReg(False);
    Reg#(Bool) alu_done <- mkReg(False);
    Reg#(Bool) written_back <- mkReg(False);

    // Registery registers
    Reg#(Bool) ie <- mkReg(False);
    Reg#(Bit#(2)) im <- mkReg(0); // XXX: Is IM 0 at startup?
    Reg#(Bit#(16)) pc <- mkReg(0);
    Reg#(Bit#(8)) displacement <- mkRegU;
    Reg#(Bit#(8)) instr_b1 <- mkRegU;
    Reg#(Bit#(8)) res <- mkRegU;
    Reg#(Bit#(8)) tmp8 <- mkRegU;
    RegisterFileIfc rf <- mkRegisterFile;

    // Decoded
    ConfigReg#(DecodedInstructionT) decoded <- mkConfigRegU;

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

    /*** IF/DEC ***/

    rule m1_pc_out(!if_done);
        inst_fetch_first_cycle.send();
        pc <= pc + 1;
        bus_man.server.request.put(Z80BusManRqT{
            addr: pc,
            payload: tagged BPMemRdRfsh 0
        });
        sub_cycle <= sub_cycle + 1;
    endrule

    rule m1_decode(!if_done);
        // TODO: DRAM Refresh
        inst_fetched.send();
        Bit#(8) data <- bus_man.server.response.get();
        if (need_displacement) begin
            displacement <= data;
            displacement_done <= True;
        end else begin
            instr_b1 <= data;
            DecodedInstructionT d = decode_simple(data, bytes_read == 0 ? IncNo : decoded.incomp);
            decoded <= d;
            $display(fshow(d));
            decoding_done <= d.incomp == IncNo;
            need_displacement <= d.displacement;
        end
        bytes_read <= bytes_read + 1;
    endrule

    /*** Ld16 ***/

    rule ld16_immgetnextbyte(if_done && decoded.op == OpLd16 &&& decoded.src1 matches tagged DirectOperand (tagged DONext16Bits) &&& decoded.dest matches tagged DirectOperand (tagged DOReg16 .r));
        bus_man.server.request.put(Z80BusManRqT{
            addr: pc,
            payload: BPMemRd
        });
        if (mem_cycle == 1) begin
            rf.write_8b(low_reg_byte(r), res);
        end
        pc <= pc + 1;
    endrule

    rule ld16_immread(if_done && decoded.op == OpLd16 &&& decoded.src1 matches tagged DirectOperand (tagged DONext16Bits));
        storeAV(bus_man.server.response.get(), res);
        if (mem_cycle == 0) begin
            mem_cycle <= 1;
        end else
            next_instruction();
    endrule

    rule ld16_seth(ifex_overlap_cycle_1 && decoded.op == OpLd16 &&& decoded.dest matches tagged DirectOperand (tagged DOReg16 .r));
        rf.write_8b(high_reg_byte(r), res);
    endrule

    /*** Ld8 ***/

    rule ld8_getreg(if_done && decoded.op == OpLd8 &&& decoded.src1 matches tagged DirectOperand (tagged DOReg8 .r));
        res <= rf.read_8b(r);
        next_instruction();
    endrule

    rule ld8_putreg(ifex_overlap_cycle_1 && decoded.op == OpLd8 &&& decoded.dest matches tagged DirectOperand (tagged DOReg8 .r));
        rf.write_8b(r, res);
    endrule

    rule ld8_immgetnextbyte(if_done && decoded.op == OpLd8 &&& decoded.src1 matches tagged DirectOperand (tagged DONext8Bits));
        bus_man.server.request.put(Z80BusManRqT{
            addr: pc,
            payload: BPMemRd
        });
        pc <= pc + 1;
    endrule

    rule ld8_immread(if_done && decoded.op == OpLd8 &&& decoded.src1 matches tagged DirectOperand (tagged DONext8Bits));
        storeAV(bus_man.server.response.get(), res);
        next_instruction();
    endrule

    /*** JUMP/BRANCH ***/

    rule op_jp(if_done && decoded.op == OpJp &&& decoded.src1 matches tagged DirectOperand tagged DONext16Bits);
        bus_man.server.request.put(Z80BusManRqT{
            addr: pc,
            payload: BPMemRd
        });
        pc <= pc + 1;
    endrule
    
    /*** SWAP ***/

    rule op_exaf(if_done && decoded.op == OpExAF);
        rf.swap_af();
        next_instruction();
    endrule

    rule op_exgp(if_done && decoded.op == OpExGP);
        rf.swap_gp();
        next_instruction();
    endrule

    /*** PUSH/POP ***/

    rule push_sp_dec(if_done && mem_cycle == 0 && decoded.op == OpPush);
        rf.write_16b(RgSP, rf.read_16b(RgSP) - 1);
        mem_cycle <= mem_cycle + 1;
    endrule

    rule push_stackwritestart(mem_cycle > 0 && decoded.op == OpPush &&& decoded.src1 matches tagged DirectOperand (tagged DOReg16 .r));
        if (mem_cycle == 1)
            bus_man.server.request.put(Z80BusManRqT{
                addr: rf.read_16b(RgSP),
                payload: tagged BPMemWrData rf.read_8b(low_reg_byte(r))
            });
        else
            bus_man.server.request.put(Z80BusManRqT{
                addr: rf.read_16b(RgSP),
                payload: tagged BPMemWrData rf.read_8b(high_reg_byte(r))
            });
    endrule

    rule push_stackwritedone(mem_cycle > 0 && decoded.op == OpPush);
        dropAV(bus_man.server.response.get());
        if (mem_cycle == 1) begin
            rf.write_16b(RgSP, rf.read_16b(RgSP) - 1);
            mem_cycle <= 2;
        end else
            next_instruction();
    endrule

    (* preempts = "pop_getbyteaddr, pop_write_lowbyte" *)
    rule pop_getbyteaddr(if_done && decoded.op == OpPop);
        bus_man.server.request.put(Z80BusManRqT{
            addr: rf.read_16b(RgSP),
            payload: BPMemRd
        });
        rf.write_16b(RgSP, rf.read_16b(RgSP) + 1);
    endrule

    rule pop_write_lowbyte(if_done && decoded.op == OpPop &&& mem_cycle == 1 &&& decoded.dest matches tagged DirectOperand (tagged DOReg16 .r));
        rf.write_8b(low_reg_byte(r), res);
    endrule

    (* preempts = "pop_getbytewrite, pop_write_lowbyte" *)
    rule pop_getbytewrite(if_done && decoded.op == OpPop);
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

    rule alu1(if_done && decoded.op == OpAlu8
      &&& decoded.src1 matches tagged DirectOperand (tagged DOReg8 .r1)
      &&& decoded.src2 matches tagged DirectOperand (tagged DOReg8 .r2));
        alu.request.put(AluReqT{cmd: decoded.alu_op, in1: rf.read_8b(r1), in2: rf.read_8b(r2), flags_in: unpack(rf.read_8b(RgF))});
        next_instruction();
    endrule

    rule alu2(ifex_overlap_cycle_1 && decoded.op == OpAlu8
      &&& decoded.dest matches tagged DirectOperand (tagged DOReg8 .r));
        AluRespT resp <- alu.response.get();
        rf.write_8b(r, resp.out);
        tmp8 <= pack(resp.flags_out);
    endrule

    rule alu3(ifex_overlap_cycle_2 && decoded.op == OpAlu8);
        rf.write_8b(RgF, tmp8);
    endrule

    /*** Interrupts ***/

    rule op_si(if_done && decoded.op == OpSi &&& decoded.src1 matches tagged DirectOperand (tagged DOLiteral .l));
        ie <= l > 0;
        next_instruction();
    endrule

    rule op_im(if_done && decoded.op == OpIm &&& decoded.src1 matches tagged DirectOperand (tagged DOLiteral .l));
        im <= l;
        next_instruction();
    endrule

    rule halt(if_done && decoded.op == OpHalt);
        $finish();
    endrule

    /*** Trace ***/

    rule trace_regs;
        $display("pc: %h m: %h s: %h i: %h res: %h",
            pc, mem_cycle, sub_cycle, instr_b1, res);
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
        return 0;
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

typedef Server#(AluReqT, AluRespT) ALU_ifc;


module mkALU(ALU_ifc);
    FIFO#(AluRespT) f_out <- mkFIFO;

    interface Put request;
        method Action put(AluReqT req);
            Bit#(8) in1 = req.in1;
            Bit#(8) in2 = req.in2;
            FlagRegT flags = req.flags_in;
            $display("ALU REQ in1: %h, in2: %h, flags: %h", in1, in2, pack(flags));
            Bit#(8) res = ?;
            case(req.cmd.op)
                AluOpAdd, AluOpSub: begin
                    if (req.cmd.op == AluOpSub)
                        in2 = -in2;
                    Bit#(1) carry_in = (req.cmd.carry_in && flags.carry) ? 1 : 0;
                    Bit#(5) half_res = extend(in1[3:0]) + extend(in2[3:0]) + extend(carry_in);
                    Bit#(8) sign_carry_in_res = extend(in1[6:0]) + extend(in2[6:0]) + extend(carry_in);
                    Bit#(9) ext_res = extend(in1) + extend(in2) + extend(carry_in);
                    // I really hope Bluespec and Quartus does clever expression availability and makes this one adder chain
                    res = ext_res[7:0];
                    flags.sign = ext_res[7] > 0;
                    flags.zero = res == 0;
                    flags.bit5 = res[5] > 0;
                    flags.half_carry = half_res[4] > 0;
                    flags.bit3 = res[3] > 0;
                    flags.parity_overflow = (sign_carry_in_res[7] ^ ext_res[8]) > 0;
                    flags.subtract = req.cmd.op == AluOpSub;
                    flags.carry = ext_res[8] > 0;
                end
                AluOpAnd: begin
                    res = in1 & in2;
                end
                AluOpXor: begin
                    res = in1 ^ in2;
                end
                AluOpOr: begin
                    res = in1 | in2;
                end
            endcase
            AluRespT resp = AluRespT{flags_out: flags, out: res};
            $display("ALU RESP out: %h flags: %h", resp.out, pack(resp.flags_out));
            f_out.enq(resp);
        endmethod
    endinterface
    interface response = toGet(f_out);
endmodule

// Consider having AddrT and WordT?

interface Z80Bus_ifc;
    method Bit#(1) n_m1(); // XXX: Should be tristate
    method Bit#(1) n_mreq(); // "
    method Bit#(1) n_iorq(); // "
    method Bit#(1) n_rd(); // "
    method Bit#(1) n_wr(); // "
    method Bit#(1) n_rfsh();
    method Bit#(16) addr();
    interface Inout#(Bit#(8)) data;
    method Action n_wait(Bit#(1) w);
endinterface

typedef union tagged {
    Bit#(7) BPMemRdRfsh;
    void BPMemRd;
    Bit#(8) BPMemWrData;
    void BPIORd;
    Bit#(8) BPIOWrData;
} BusRqPayloadT deriving (Bits, Eq, Bounded);

typedef struct {
    Bit#(16) addr;
    BusRqPayloadT payload;
} Z80BusManRqT deriving (Bits, Eq, Bounded);

interface Z80BusManager_ifc;
    interface Server#(Z80BusManRqT, Bit#(8)) server;
    interface Z80Bus_ifc bus;
endinterface

(* synthesize *)
module mkZ80BusManager(Z80BusManager_ifc);
    // XXX: Could use M1 too
    ConfigReg#(Maybe#(Z80BusManRqT)) req_reg <- mkConfigReg(tagged Invalid);
    RWire#(Z80BusManRqT) req_in_wire <- mkRWire;
    PulseWire inv_cur_req <- mkPulseWire;
    Maybe#(Z80BusManRqT) cur_req_in = req_in_wire.wget() matches tagged Valid .* ? req_in_wire.wget() : req_reg;
    Bool cur_req_out_valid = !inv_cur_req || (req_reg matches tagged Valid .* ? True : False);

    ConfigReg#(UInt#(1)) tstate <- mkConfigReg(0);

    // utilise default wires more
    Wire#(Bit#(1)) n_m1_out <- mkBypassWire;
    Wire#(Bit#(1)) n_mreq_out <- mkBypassWire;
    Wire#(Bit#(1)) n_iorq_out <- mkBypassWire;
    Wire#(Bit#(1)) n_rd_out <- mkBypassWire;
    Wire#(Bit#(1)) n_wr_out <- mkBypassWire;
    Wire#(Bit#(1)) n_rfsh_out <- mkBypassWire;
    Wire#(Bit#(1)) n_wait_in <- mkBypassWire;
    Wire#(Bit#(16)) addr_out <- mkBypassWire;
    RWire#(Bit#(8)) data_out <- mkRWire;
    TriState#(Bit#(8)) data_tri <- mkTriState(isValid(data_out.wget()), fromMaybe(?, data_out.wget()));

    rule do_memrdrfsh_s1_req(cur_req_in matches tagged Valid .req &&& req.payload matches tagged BPMemRdRfsh .rfshaddr &&& tstate == 0);
        n_m1_out <= 0;
        n_mreq_out <= 0; // should change on next neg edge
        n_rd_out <= 0;
        addr_out <= req.addr;
    endrule

    rule do_memrdrfsh_s2_req(req_reg matches tagged Valid .req &&& req.payload matches tagged BPMemRdRfsh .rfshaddr &&& tstate == 1);
        n_m1_out <= 0;
        n_mreq_out <= 0; // should change on next neg edge
        addr_out <= req.addr;

        req_reg <= tagged Invalid;
        inv_cur_req.send();
        tstate <= 0;
    endrule

    rule do_memrd_req(cur_req_in matches tagged Valid .req &&& req.payload matches tagged BPMemRd);
        n_mreq_out <= 0; // should change on next neg edge
        n_rd_out <= 0;
        addr_out <= req.addr;
    endrule

    rule do_memwr_req(cur_req_in matches tagged Valid .req &&& req.payload matches tagged BPMemWrData .wrdata);
        n_mreq_out <= 0; // should change on next neg edge
        n_wr_out <= 0;
        data_out.wset(wrdata);
        addr_out <= req.addr;
    endrule

    rule do_iord_req(cur_req_in matches tagged Valid .req &&& req.payload matches tagged BPIORd);
        n_iorq_out <= 0; // should change on next neg edge
        n_rd_out <= 0;
        addr_out <= req.addr;
    endrule

    rule do_iowr_req(cur_req_in matches tagged Valid .req &&& req.payload matches tagged BPIOWrData .wrdata);
        n_iorq_out <= 0; // should change on next neg edge
        n_wr_out <= 0;
        data_out.wset(wrdata);
        addr_out <= req.addr;
    endrule

    rule no_req(cur_req_in matches tagged Invalid);
        n_mreq_out <= 1;
        n_iorq_out <= 1;
        n_rd_out <= 1;
        n_wr_out <= 1;
        n_rfsh_out <= 1;
        addr_out <= ?;
    endrule

    interface Z80Bus_ifc bus;
        method Bit#(1) n_m1();
            return n_m1_out;
        endmethod

        method Bit#(1) n_mreq();
            return n_mreq_out;
        endmethod

        method Bit#(1) n_iorq();
            return n_iorq_out;
        endmethod

        method Bit#(1) n_rd();
            return n_rd_out;
        endmethod

        method Bit#(1) n_wr();
            return n_wr_out;
        endmethod

        method Bit#(1) n_rfsh();
            return n_rfsh_out;
        endmethod

        method Bit#(16) addr();
            return addr_out;
        endmethod

        interface data = data_tri.io;

        method Action n_wait(Bit#(1) w);
            n_wait_in <= w;
        endmethod
    endinterface

    interface Server server;
        interface Put request;
            method Action put(Z80BusManRqT req) if (inv_cur_req || (req_reg matches tagged Invalid ? True : False));
                req_in_wire.wset(req);
                req_reg <= tagged Valid req;
            endmethod
        endinterface

        interface Get response;
            method ActionValue#(Bit#(8)) get() if ((n_wait_in != 0) &&& req_reg matches tagged Valid .req &&& tstate == 0); // wait should be sampled on previous neg clock edge
                if (req.payload matches tagged BPMemRdRfsh .*) begin
                    tstate <= 1;
                end else begin
                    req_reg <= tagged Invalid;
                    inv_cur_req.send();
                end
                return data_tri;
            endmethod
        endinterface
    endinterface
endmodule

(* synthesize *)
module mkZ80BusManagerTb(Empty);
    Z80BusManager_ifc z80busman <- mkZ80BusManager();
    Z80Bus_ifc bus = z80busman.bus;
    Server#(Z80BusManRqT, Bit#(8)) server = z80busman.server;

    Wire#(Bit#(1)) n_wait_in <- mkDWire(0);
    mkConnection(n_wait_in._read, bus.n_wait);

    RWire#(Bit#(8)) data_out <- mkRWire;
    TriState#(Bit#(8)) data_tri <- mkTriState(isValid(data_out.wget()), fromMaybe(?, data_out.wget()));
    mkConnection(bus.data, data_tri.io);

    // Probiness
    Probe#(Bit#(1)) n_mreq_out_probe <- mkProbe();
    mkConnection(bus.n_mreq, n_mreq_out_probe._write);
    Probe#(Bit#(1)) n_iorq_out_probe <- mkProbe();
    mkConnection(bus.n_iorq, n_iorq_out_probe._write);
    Probe#(Bit#(1)) n_rd_out_probe <- mkProbe();
    mkConnection(bus.n_rd, n_rd_out_probe._write);
    Probe#(Bit#(1)) n_wr_out_probe <- mkProbe();
    mkConnection(bus.n_wr, n_wr_out_probe._write);
    Probe#(Bit#(1)) n_rfsh_out_probe <- mkProbe();
    mkConnection(bus.n_rfsh, n_rfsh_out_probe._write);
    Probe#(Bit#(1)) n_wait_in_probe <- mkProbe();
    mkConnection(n_wait_in, n_wait_in_probe._write);
    Probe#(Bit#(16)) addr_probe <- mkProbe();
    mkConnection(bus.addr, addr_probe._write);
    Probe#(Bit#(8)) data_probe <- mkProbe();
    mkConnection(data_tri._read, data_probe._write);

    mkAutoFSM(seq
        $dumpvars();
        server.request.put(Z80BusManRqT{
            payload: tagged BPMemRdRfsh 101,
            addr: 4242
        });
        par
            data_out.wset(77);
            n_wait_in <= 1;
            $display("M1 read %d", server.response.get());
        endpar
        server.request.put(Z80BusManRqT{
            payload: tagged BPMemRd,
            addr: 4244
        });
        par
            data_out.wset(79);
            n_wait_in <= 1;
            $display("M1 read %d", server.response.get());
        endpar
        server.request.put(Z80BusManRqT{
            payload: tagged BPIORd,
            addr: 4246
        });
        noAction;
        par
            n_wait_in <= 1;
            data_out.wset(81);
            $display("IO read %d", server.response.get());
        endpar
        noAction;
        server.request.put(Z80BusManRqT{
            payload: tagged BPMemWrData 42,
            addr: 4248
        });
        noAction;
        par
            n_wait_in <= 1;
            dropAV(server.response.get());
            $display("Normal write finished");
        endpar
        noAction;
        server.request.put(Z80BusManRqT{
            payload: tagged BPIOWrData 42,
            addr: 4250
        });
        noAction;
        par
            n_wait_in <= 1;
            dropAV(server.response.get());
            $display("Normal read finished");
        endpar
        noAction;
    endseq);
endmodule

interface RegisterFileIfc;
    // other
    method Action write_8b(Reg8T regg, Bit#(8) data);
    method Bit#(8) read_8b(Reg8T regg);
    method Action write_16b(Reg16T regg, Bit#(16) data);
    method Bit#(16) read_16b(Reg16T regg);
    method Action swap_af();
    method Action swap_gp();
endinterface

module mkRegisterFile(RegisterFileIfc); // Use RegFile module?
    Vector#(TExp#(SizeOf#(Reg8T)), Reg#(Bit#(8))) rf <- replicateM(mkReg(0));
    Vector#(8, Reg#(Bit#(8))) rf_shad <- replicateM(mkReg(0));

    Reg#(Bool) af_shad <- mkReg(False);
    Reg#(Bool) gp_shad <- mkReg(False);

    function Reg#(Bit#(8)) regreg(Reg8T regg);
        let addr = pack(regg);
        if (((addr < 2) && af_shad) || ((2 <= addr) && (addr < 8) && gp_shad))
            return rf_shad[addr];
        else
            return rf[addr];
    endfunction

    function Tuple2#(Reg8T, Reg8T) reg16s_regs(Reg16T regg);
        return tuple2(unpack(extend(pack(regg) * 2)), unpack(extend(pack(regg) * 2 + 1)));
    endfunction

    method Action write_8b(Reg8T regg, Bit#(8) data);
        regreg(regg) <= data;
    endmethod 

    method Bit#(8) read_8b(Reg8T regg);
        return regreg(regg)._read();
    endmethod 

    method Action write_16b(Reg16T regg, Bit#(16) data);
        let regs = reg16s_regs(regg);
        regreg(tpl_1(regs)) <= data[15:8];
        regreg(tpl_2(regs)) <= data[7:0];
    endmethod 

    method Bit#(16) read_16b(Reg16T regg);
        let regs = reg16s_regs(regg);
        return {regreg(tpl_1(regs))._read(), regreg(tpl_2(regs))._read()};
    endmethod 

    method Action swap_af();
        af_shad <= !af_shad;
    endmethod

    method Action swap_gp();
        gp_shad <= !gp_shad;
    endmethod
endmodule

endpackage
