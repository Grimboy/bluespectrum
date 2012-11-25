package Z80Decode;

import Z80Types::*;

DecodedOperandT tab_r[8] = {
    tagged DirectOperand (tagged DOReg8 RgB),
    tagged DirectOperand (tagged DOReg8 RgC),
    tagged DirectOperand (tagged DOReg8 RgD),
    tagged DirectOperand (tagged DOReg8 RgE),
    tagged DirectOperand (tagged DOReg8 RgH),
    tagged DirectOperand (tagged DOReg8 RgL),
    tagged IndirectOperand (tagged IOReg16 RgHL),
    tagged DirectOperand (tagged DOReg8 RgA)
};

Reg16T tab_rp[4] = {
    RgBC, RgDE, RgHL, RgSP
};

Reg16T tab_rp2[4] = {
    RgBC, RgDE, RgHL, RgAF
};

ConditionCodeT tab_cc[8] = {
    CcNZ, CcZ, CcNC, CcC,
    CcPO, CcPE, CcP, CcM
};

AluCmdT tab_alu[8] = {
    AluCmdT{op: AluOpAdd, carry_in: False},
    AluCmdT{op: AluOpAdd, carry_in: True},
    AluCmdT{op: AluOpSub, carry_in: False},
    AluCmdT{op: AluOpSub, carry_in: True},
    AluCmdT{op: AluOpAnd, carry_in: False},
    AluCmdT{op: AluOpXor, carry_in: False},
    AluCmdT{op: AluOpOr, carry_in: False},
    AluCmdT{op: AluOpSub, carry_in: False}
};

/*
InternalOpTypeT tab_rot[8] = {
}

InternalOpTypeT tab_im[8] = {
}

InternalOpTypeT tab_bli[8][4] = {
}
*/

AluCmdT nop_cmd = AluCmdT{op: AluOpNop, carry_in: False};
DecodedInstructionT decoded_nop = DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo};

function DecodedInstructionT indexify(DecodedInstructionT decoded, Reg16T indexreg, Reg8T indexregh, Reg8T indexregl);
    if (decoded.dest == tagged IndirectOperand (tagged IOReg16 RgHL)) begin
        decoded.dest = tagged IndirectOperand (tagged IOReg16 indexreg);
        decoded.need_displacement = True;
    end else if (decoded.src1 == tagged IndirectOperand (tagged IOReg16 RgHL)) begin
        decoded.src1 = tagged IndirectOperand (tagged IOReg16 indexreg);
        decoded.need_displacement = True;
    end else begin
        `define direct_indexify(attr)\
            if (decoded.``attr == tagged DirectOperand (tagged DOReg16 RgHL))\
                decoded.``attr = tagged DirectOperand (tagged DOReg16 indexreg);\
            else if (decoded.``attr == tagged DirectOperand (tagged DOReg8 RgH))\
                decoded.``attr = tagged DirectOperand (tagged DOReg8 indexregh);\
            else if (decoded.``attr == tagged DirectOperand (tagged DOReg8 RgL))\
                decoded.``attr= tagged DirectOperand (tagged DOReg8 indexregl);
        `direct_indexify(dest)
        `direct_indexify(src1)
        `direct_indexify(src2)
    end
    return decoded;
endfunction

function DecodedInstructionT decode_simple(Bit#(8) inst, IncompleteInstructionT incomp);
    Bit#(2) x = inst[7:6];
    Bit#(3) y = inst[5:3];
    Bit#(3) z = inst[2:0];

    Bit#(2) p = inst[5:4];
    Bit#(1) q = inst[3];

    //DecodedInstructionT d = DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo};

    case(incomp)
        IncNo: case(x)
            0: case(z)
                0: case(y)
                    0: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // NOP
                    1: return DecodedInstructionT{op: OpExAF, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // EX AF, AF'
                    2: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // DJNZ d
                    3: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // JR d
                    default: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // JR cc[y-4], d
                endcase
                1: case(q)
                    0: return DecodedInstructionT{op: OpLd16, alu_op: nop_cmd, dest: tagged DirectOperand (tagged DOReg16 tab_rp[p]), src1: tagged DirectOperand (tagged DONext16Bits), src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // LD rp[p], nn
                    1: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // ADD HL, rp[p]
                endcase
                2: case(q)
                    0: case(p)
                        0, 1: return DecodedInstructionT{op: OpLd8, alu_op: nop_cmd, dest: tagged IndirectOperand (tagged IOReg16 tab_rp[p]), src1: tagged DirectOperand (tagged DOReg8 RgA), src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // LD (rp[p]), A
                        2: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented LD (nn), HL
                        3: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // LD (nn), A
                    endcase
                    1: case(p)
                        0, 1: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // LD A, (rp[p])
                        2: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // LD HL, (nn)
                        3: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // LD A, (nn)
                    endcase
                endcase
                3: case(q)
                    0: return DecodedInstructionT{op: OpInc16, alu_op: nop_cmd, dest: tagged DirectOperand (tagged DOReg16 tab_rp[p]), src1: tagged DirectOperand (tagged DOReg16 tab_rp[p]), src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // INC rp[p]
                    1: return DecodedInstructionT{op: OpDec16, alu_op: nop_cmd, dest: tagged DirectOperand (tagged DOReg16 tab_rp[p]), src1: tagged DirectOperand (tagged DOReg16 tab_rp[p]), src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // DEC rp[p]
                endcase
                4: return DecodedInstructionT{op: OpInc, alu_op: nop_cmd, dest: tab_r[y], src1: tab_r[y], src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // INC r[y]
                5: return DecodedInstructionT{op: OpDec, alu_op: nop_cmd, dest: tab_r[y], src1: tab_r[y], src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // DEC r[y]
                6: return DecodedInstructionT{op: OpLd8, alu_op: nop_cmd, dest: tab_r[y], src1: tagged DirectOperand tagged DONext8Bits, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // LD r[y], n
                7: case(y)
                    0: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // RLCA
                    1: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // RRCA
                    2: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // RLA
                    3: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // RRA
                    4: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // DAA
                    5: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // CPL
                    6: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // SCF
                    7: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // CCF
                endcase
            endcase
            1: if(z==6 && y==6)
                return DecodedInstructionT{op: OpHalt, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // HALT
            else
                return DecodedInstructionT{op: OpLd8, alu_op: nop_cmd, dest: tab_r[y], src1: tab_r[z], src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // LD r[y], r[z]
            2: begin 
                return DecodedInstructionT{op: OpAlu8, alu_op: tab_alu[y], dest: y == 7 ? tagged NoOperand : tagged DirectOperand (tagged DOReg8 RgA), src1: tagged DirectOperand (tagged DOReg8 RgA), src2: tab_r[z], need_displacement: False, incomp: IncNo}; // alu[y] r[z]
            end
            3: case(z)
                0: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // RET cc[y]
                1: case(q)
                    0: return DecodedInstructionT{op: OpPop, alu_op: nop_cmd, dest: tagged DirectOperand tagged DOReg16 tab_rp2[p], src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // POP rp2[p]
                    1: case (p)
                        0: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // RET
                        1: return DecodedInstructionT{op: OpExGP, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // EXX
                        2: return DecodedInstructionT{op: OpJp, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged DirectOperand tagged DOReg16 RgHL, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // JP HL
                        3: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // LD SP,HL
                    endcase
                endcase
                2: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // JP cc[y], nn
                3: case(y)
                    0: return DecodedInstructionT{op: OpJp, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged DirectOperand tagged DONext16Bits, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // JP nn
                    1: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncBits}; // (CB prefix)
                    2: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // OUT (n), A
                    3: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // IN A, (n)
                    4: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // EX (SP), HL
                    5: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // EX DE, HL
                    6: return DecodedInstructionT{op: OpSi, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged DirectOperand tagged DOLiteral 0, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // DI
                    7: return DecodedInstructionT{op: OpSi, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged DirectOperand tagged DOLiteral 1, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // EI
                endcase
                4: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // CALL cc[y], nn
                5: case (q)
                    0: return DecodedInstructionT{op: OpPush, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged DirectOperand tagged DOReg16 tab_rp2[p], src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // PUSH rp2[p]
                    1: case(p)
                        0: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // CALL nn
                        1: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncIx}; // (DD prefix)
                        2: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncExtd}; // (ED prefix)
                        3: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncIy}; // (FD prefix)
                    endcase
                endcase
                6: return DecodedInstructionT{op: OpAlu8, alu_op: tab_alu[y], dest: tagged DirectOperand (tagged DOReg8 RgA), src1: tagged DirectOperand (tagged DOReg8 RgA), src2: tagged DirectOperand tagged DONext8Bits, need_displacement: False, incomp: IncNo}; // alu[y] n
                7: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // RST y*8
            endcase
        endcase
        IncExtd: case(x)
            0,3: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // NONI, NOP
            1: case(z)
                0: if(y!=6)
                    return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // IN r[y], (C)
                else
                    return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // IN (C)
                1: if(y!=6)
                    return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // OUT (C), r[y]
                else
                    return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // OUT (C), 0
            2: case(q)
                    0: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // SBC HL, rp[p]
                    1: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // ADC HL, rp[p]
                endcase
                3: case(q)
                    0: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // LD (nn), rp[p]
                    1: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // LD rp[p], (nn)
                endcase
                4: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // NEG
                5: if(y!=1)
                    return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // RETN
                else
                    return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // RETI
                6: return DecodedInstructionT{op: OpIm, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged DirectOperand tagged DOLiteral (extend(y[0]) + extend(y[1])), src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // IM im[y] // XXX: "Odd" interrupt mode 0/1 unimplemented
                7: case(y)
                    0: return DecodedInstructionT{op: OpLd8, alu_op: nop_cmd, dest: tagged DirectOperand tagged DOReg8 RgI, src1: tagged DirectOperand tagged DOReg8 RgA, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // LD I, A
                    1: return DecodedInstructionT{op: OpLd8, alu_op: nop_cmd, dest: tagged DirectOperand tagged DOReg8 RgR, src1: tagged DirectOperand tagged DOReg8 RgA, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // LD R, A
                    2: return DecodedInstructionT{op: OpLd8, alu_op: nop_cmd, dest: tagged DirectOperand tagged DOReg8 RgA, src1: tagged DirectOperand tagged DOReg8 RgI, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // LD A, I
                    3: return DecodedInstructionT{op: OpLd8, alu_op: nop_cmd, dest: tagged DirectOperand tagged DOReg8 RgA, src1: tagged DirectOperand tagged DOReg8 RgR, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // LD A, R
                    4: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // RRD
                    5: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // RLD
                    6,7: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // NOP
                endcase
            endcase
            2: if(z<4 && y>=4)
                return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // bli[y,z]
            else
                return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented // NONI, NOP
        endcase
        IncBits: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, need_displacement: False, incomp: IncNo}; // XXX: Unimplemented
        IncIx: begin
            let decoded = decode_simple(inst, IncNo);
            return indexify(decoded, RgIX, RgIXh, RgIXl);
        end
        IncIxBits: begin
            let decoded = decode_simple(inst, IncBits);
            return indexify(decoded, RgIX, RgIXh, RgIXl);
        end
        IncIy: begin
            let decoded = decode_simple(inst, IncNo);
            return indexify(decoded, RgIY, RgIYh, RgIYl);
        end
        IncIyBits: begin
            let decoded = decode_simple(inst, IncBits);
            return indexify(decoded, RgIY, RgIYh, RgIYl);
        end
    endcase
endfunction

endpackage
