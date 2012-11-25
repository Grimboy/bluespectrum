package Z80Alu;

import Z80Types::*;
import FIFO::*;
import GetPut::*;
import ClientServer::*;

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

endpackage
