package Z80Alu;

import Z80Types::*;
import FIFO::*;
import GetPut::*;
import ClientServer::*;

typedef Server#(AluReqT, AluRespT) ALU_ifc;

Bool hc_add_tab[8] = {
    False, True, True, True, False, False, False, True
};

Bool hc_sub_tab[8] = {
    False, False, True, False, True, False, True, True
};

Bool of_add_tab[8] = {
    False, False, False, True, True, False, False, False
};

Bool of_sub_tab[8] = {
    False, True, False, False, False, False, True, False
};

module mkALU(ALU_ifc);
    FIFO#(AluRespT) f_out <- mkFIFO;

    interface Put request;
        method Action put(AluReqT req);
            Bit#(8) in1 = req.in1;
            Bit#(8) in2 = req.in2;
            FlagRegT flags = req.flags_in;
            $display("ALU REQ in1: %h, in2: %h, flags: %h", in1, in2, pack(flags));
            Bit#(8) res = ?;
            Bit#(1) carry_in = (req.cmd.carry_in && flags.carry) ? 1 : 0;
            case(req.cmd.op)
                AluOpAdd: begin
                    Bit#(9) ext_res = extend(in1) + extend(in2) + extend(carry_in);
                    res = ext_res[7:0];
                    flags.half_carry = hc_add_tab[{res[3], in2[3], in1[3]}];
                    flags.parity_overflow = of_add_tab[{res[7], in2[7], in1[7]}];
                    flags.carry = ext_res[8] == 1;
                    flags.subtract = False;
                end
                AluOpSub, AluOpCp: begin
                    Bit#(9) ext_res = extend(in1) - extend(in2) - extend(carry_in);
                    res = ext_res[7:0];
                    flags.half_carry = hc_sub_tab[{res[3], in2[3], in1[3]}];
                    flags.parity_overflow = of_sub_tab[{res[7], in2[7], in1[7]}];
                    flags.carry = ext_res[8] == 1;
                    flags.subtract = True;
                end
                AluOpAnd, AluOpXor, AluOpOr: begin
                    case(req.cmd.op)
                        AluOpAnd: res = in1 & in2;
                        AluOpXor: res = in1 ^ in2;
                        AluOpOr: res = in1 | in2;
                    endcase
                    flags.half_carry = req.cmd.op == AluOpAnd;
                    flags.parity_overflow = ^res == 0;
                    flags.carry = False;
                    flags.subtract = False;
                end
            endcase
            let flag_bits = (req.cmd.op == AluOpCp) ? in2 : res;
            flags.sign = res[7] > 0;
            flags.zero = res == 0;
            flags.bit5 = flag_bits[5] > 0;
            flags.bit3 = flag_bits[3] > 0;
            AluRespT resp = AluRespT{flags_out: flags, out: (req.cmd.op == AluOpCp) ? in1 : res};
            $display("ALU RESP out: %h flags: %h", resp.out, pack(resp.flags_out));
            f_out.enq(resp);
        endmethod
    endinterface
    interface response = toGet(f_out);
endmodule

endpackage
