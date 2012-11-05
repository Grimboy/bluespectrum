package Z80aTypes;

import FShow::*;
import Reserved::*;

typedef enum {
    RgA, RgF, RgB, RgC,
    RgD, RgE, RgH, RgL,
    RgW, RgZ, RgS, RgP,
    RgI, RgR
} Reg8T deriving (Bits, Eq, Bounded);

typedef enum {
    RgAF, RgBC, RgDE, RgHL,
    RgWZ, RgSP
} Reg16T deriving (Bits, Eq, Bounded);

typedef enum {
    CcNZ, CcZ, CcNC, CcC, CcPO, CcPE, CcP, CcM
} ConditionCodeT deriving (Bits, Eq, Bounded);

typedef enum {
    OpNop, OpHalt,
    OpLd8, OpLd16,
    OpInc, OpDec, OpInc16, OpDec16, 
    OpAdd, OpAdc, OpSub, OpSbc, OpCp, OpAnd, OpOr, OpXor,
    OpAdd16,
    OpPush, OpPop,
    OpSi, OpIm,
    OpAlu8, OpAlu16,
    OpRlca, OpRla, OpDaa, OpScf, OpRrca, OpRra, OpCpl, OpCcf
} InternalOpTypeT deriving (Bits, Eq, Bounded);

typedef enum {
    IncNo, IncExtd, IncBits, IncIx, IncIxBits, IncIy, IncIyBits
} IncompleteInstructionT deriving (Bits, Eq, Bounded);

typedef union tagged {
    Reg8T DOReg8;
    Reg16T DOReg16;
    void DONext8Bits;
    void DONext16Bits;
    Bit#(2) DOLiteral;
} DirectOperandT deriving (Bits, Eq, Bounded);

typedef union tagged {
    Reg16T IOReg16;
    void IONext16Bits;
} IndirectOperandT deriving (Bits, Eq, Bounded);

typedef union tagged {
    DirectOperandT DirectOperand;
    IndirectOperandT IndirectOperand;
    void NoOperand;
} DecodedOperandT deriving (Bits, Eq, Bounded);

// XXX: Should maybe consider having DecodingResultT = DecodedInstructionT | PartialDecodingResultT
typedef struct {
    InternalOpTypeT op;
    AluCmdT alu_op;
    DecodedOperandT dest;
    DecodedOperandT src1;
    DecodedOperandT src2;
    Bool displacement;
    IncompleteInstructionT incomp;
} DecodedInstructionT deriving (Bits, Eq, Bounded);

typedef struct {
    Bool sign;
    Bool zero;
    Bool bit5;
    Bool half_carry;
    Bool bit3;
    Bool parity_overflow;
    Bool subtract;
    Bool carry;
} FlagRegT deriving (Bits, Eq, Bounded);

typedef enum {
    AluOpNop, AluOpAdd, AluOpSub, AluOpAnd, AluOpXor, AluOpOr
} AluOpT deriving (Bits, Eq, Bounded);

typedef struct {
    AluOpT op;
    Bool carry_in;
} AluCmdT deriving (Bits, Eq, Bounded);

typedef struct {
    AluCmdT cmd;
    Bit#(8) in1;
    Bit#(8) in2;
    FlagRegT flags_in;
} AluReqT deriving (Bits, Eq, Bounded);

typedef struct {
    FlagRegT flags_out;
    Bit#(8) out;
} AluRespT deriving (Bits, Eq, Bounded);

function Reg8T low_reg_byte(Reg16T regg);
    return unpack(extend(pack(regg)) * 2 + 1);
endfunction

function Reg8T high_reg_byte(Reg16T regg);
    return unpack(extend(pack(regg)) * 2);
endfunction

instance FShow#(IncompleteInstructionT);
    function Fmt fshow (IncompleteInstructionT incomp);
        case(incomp)
            IncNo: return $format("IncNo");
            IncExtd: return $format("IncExtd");
            IncBits: return $format("IncBits");
            IncIx: return $format("IncIx");
            IncIxBits: return $format("IncIxBits");
            IncIy: return $format("IncIy");
            IncIyBits: return $format("IncIyBits");
        endcase
    endfunction
endinstance

instance FShow#(InternalOpTypeT);
    function Fmt fshow (InternalOpTypeT op);
        case(op) matches
            OpNop: return $format("OpNop");
            OpHalt: return $format("OpHalt");
            OpLd8: return $format("OpLd8");
            OpLd16: return $format("OpLd16");
            OpInc: return $format("OpInc");
            OpDec: return $format("OpDec");
            OpAdd: return $format("OpAdd");
            OpAdc: return $format("OpAdc");
            OpSub: return $format("OpSub");
            OpSbc: return $format("OpSbc");
            OpCp: return $format("OpCp");
            OpAnd: return $format("OpAnd");
            OpOr: return $format("OpOr");
            OpXor: return $format("OpXor");
            OpAdd16: return $format("OpAdd16");
            OpPush: return $format("OpPush");
            OpPop: return $format("OpPop");
            OpSi: return $format("OpSi");
            OpIm: return $format("OpIm");
            OpRlca: return $format("OpRlca");
            OpRla: return $format("OpRla");
            OpDaa: return $format("OpDaa");
            OpScf: return $format("OpScf");
            OpRrca: return $format("OpRrca");
            OpRra: return $format("OpRra");
            OpCpl: return $format("OpCpl");
            OpCcf: return $format("OpCcf");
            OpAlu8: return $format("OpAlu8");
        endcase
    endfunction
endinstance

instance FShow#(Reg8T);
    function Fmt fshow (Reg8T reg8);
        case(reg8) matches
            RgB: return $format("RgB");
            RgC: return $format("RgC");
            RgD: return $format("RgD");
            RgE: return $format("RgE");
            RgH: return $format("RgH");
            RgL: return $format("RgL");
            RgA: return $format("RgA");
            RgF: return $format("RgF");
            RgI: return $format("RgI");
            RgR: return $format("RgR");
        endcase
    endfunction
endinstance

instance FShow#(Reg16T);
    function Fmt fshow (Reg16T reg16);
        case(reg16)
            RgBC: return $format("RgBC");
            RgDE: return $format("RgDE");
            RgHL: return $format("RgHL");
            RgSP: return $format("RgSP");
            RgAF: return $format("RgAF");
        endcase
    endfunction
endinstance

instance FShow#(DirectOperandT);
    function Fmt fshow (DirectOperandT dir);
        case(dir) matches
            tagged DOReg8 .reg8:
                return $format("Reg8: ") + fshow(reg8);
            tagged DOReg16 .reg16:
                return $format("Reg16: ") + fshow(reg16);
            tagged DONext8Bits:
                return $format("Next8Bits");
            tagged DONext16Bits:
                return $format("Next16Bits");
            tagged DOLiteral .l:
                return $format("Literal: %h", l);
        endcase
    endfunction
endinstance

instance FShow#(IndirectOperandT);
    function Fmt fshow (IndirectOperandT indir);
        case(indir) matches
            tagged IOReg16 .reg16:
                return $format("Reg16: ") + fshow(reg16);
            tagged IONext16Bits:
                return $format("Next16Bits");
        endcase
    endfunction
endinstance

instance FShow#(DecodedOperandT);
    function Fmt fshow (DecodedOperandT dec);
        case(dec) matches
            tagged DirectOperand .do_:
                return $format("<DirectOperand ") + fshow(do_) + $format(">");
            tagged IndirectOperand .io:
                return $format("<IndirectOperand ") + fshow(io) + $format(">");
            tagged NoOperand:
                return $format("<NoOperand>");
        endcase
    endfunction
endinstance

instance FShow#(AluOpT);
    function Fmt fshow (AluOpT op);
        case(op)
            AluOpNop: return $format("AluOpNop");
            AluOpAdd: return $format("AluOpAdd");
            AluOpSub: return $format("AluOpSub");
            AluOpAnd: return $format("AluOpAnd");
            AluOpXor: return $format("AluOpXor");
            AluOpOr: return $format("AluOpOr");
        endcase
    endfunction
endinstance

instance FShow#(AluCmdT);
    function Fmt fshow (AluCmdT cmd);
        return $format("<AluCmd op: ") + fshow(cmd.op) +
            $format("\n  carry_in: ") + fshow(cmd.carry_in) +
            $format(">");
    endfunction
endinstance

instance FShow#(DecodedInstructionT);
    function Fmt fshow (DecodedInstructionT inst);
        return ($format("<DecodedInstruction") +
            $format("\n op: ") + fshow(inst.op) +
            $format("\n alu_op: ") + fshow(inst.alu_op) +
            $format("\n dest: ") + fshow(inst.dest) +
            $format("\n src1: ") + fshow(inst.src1) +
            $format("\n src2: ") + fshow(inst.src2) +
            $format("\n incomp: ") + fshow(inst.incomp) +
            $format("\n displacement: ") + fshow(inst.displacement) +
            $format(">"));
    endfunction
endinstance

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
                    0: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // NOP
                    1: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // EX AF, AF'
                    2: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // DJNZ d
                    3: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // JR d
                    default: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // JR cc[y-4], d
                endcase
                1: case(q)
                    0: return DecodedInstructionT{op: OpLd16, alu_op: nop_cmd, dest: tagged DirectOperand (tagged DOReg16 tab_rp[p]), src1: tagged DirectOperand (tagged DONext16Bits), src2: tagged NoOperand, displacement: False, incomp: IncNo}; // LD rp[p], nn
                    1: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // ADD HL, rp[p]
                endcase
                2: case(q)
                    0: case(p)
                        0, 1: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // LD (rp[p]), A
                        2: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented LD (nn), HL
                        3: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // LD (nn), A
                    endcase
                    1: case(p)
                        0, 1: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // LD A, (rp[p])
                        2: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // LD HL, (nn)
                        3: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // LD A, (nn)
                    endcase
                endcase
                3: case(q)
                    0: return DecodedInstructionT{op: OpInc16, alu_op: nop_cmd, dest: tagged DirectOperand (tagged DOReg16 tab_rp[p]), src1: tagged DirectOperand (tagged DOReg16 tab_rp[p]), src2: tagged NoOperand, displacement: False, incomp: IncNo}; // INC rp[p]
                    1: return DecodedInstructionT{op: OpDec16, alu_op: nop_cmd, dest: tagged DirectOperand (tagged DOReg16 tab_rp[p]), src1: tagged DirectOperand (tagged DOReg16 tab_rp[p]), src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // DEC rp[p]
                endcase
                4: return DecodedInstructionT{op: OpInc, alu_op: nop_cmd, dest: tab_r[y], src1: tab_r[y], src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // INC r[y]
                5: return DecodedInstructionT{op: OpDec, alu_op: nop_cmd, dest: tab_r[y], src1: tab_r[y], src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // DEC r[y]
                6: return DecodedInstructionT{op: OpLd8, alu_op: nop_cmd, dest: tab_r[y], src1: tagged DirectOperand tagged DONext8Bits, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // LD r[y], n
                7: case(y)
                    0: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // RLCA
                    1: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // RRCA
                    2: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // RLA
                    3: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // RRA
                    4: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // DAA
                    5: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // CPL
                    6: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // SCF
                    7: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // CCF
                endcase
            endcase
            1: if(z==6 && y==6)
                return DecodedInstructionT{op: OpHalt, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // HALT
            else
                return DecodedInstructionT{op: OpLd8, alu_op: nop_cmd, dest: tab_r[y], src1: tab_r[z], src2: tagged NoOperand, displacement: False, incomp: IncNo}; // LD r[y], r[z]
            2: begin 
                return DecodedInstructionT{op: OpAlu8, alu_op: tab_alu[y], dest: y == 7 ? tagged NoOperand : tagged DirectOperand (tagged DOReg8 RgA), src1: tagged DirectOperand (tagged DOReg8 RgA), src2: tab_r[z], displacement: False, incomp: IncNo}; // alu[y] r[z]
            end
            3: case(z)
                0: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // RET cc[y]
                1: case(q)
                    0: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged DirectOperand tagged DOReg16 tab_rp2[p], src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // POP rp2[p]
                    1: case (p)
                        0: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // RET
                        1: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // EXX
                        2: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // JP HL
                        3: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // LD SP,HL
                    endcase
                endcase
                2: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // JP cc[y], nn
                3: case(y)
                    0: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // JP nn
                    1: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncBits}; // (CB prefix)
                    2: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // OUT (n), A
                    3: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // IN A, (n)
                    4: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // EX (SP), HL
                    5: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // EX DE, HL
                    6: return DecodedInstructionT{op: OpSi, alu_op: nop_cmd, dest: tagged DirectOperand tagged DOLiteral 0, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // DI
                    7: return DecodedInstructionT{op: OpSi, alu_op: nop_cmd, dest: tagged DirectOperand tagged DOLiteral 1, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // EI
                endcase
                4: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // CALL cc[y], nn
                5: case (q)
                    0: return DecodedInstructionT{op: OpPush, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged DirectOperand tagged DOReg16 tab_rp2[p], src2: tagged NoOperand, displacement: False, incomp: IncNo}; // PUSH rp2[p]
                    1: case(p)
                        0: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // CALL nn
                        1: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncIx}; // (DD prefix)
                        2: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncExtd}; // (ED prefix)
                        3: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncIy}; // (FD prefix)
                    endcase
                endcase
                6: return DecodedInstructionT{op: OpAlu8, alu_op: tab_alu[y], dest: tagged DirectOperand (tagged DOReg8 RgA), src1: tagged DirectOperand (tagged DOReg8 RgA), src2: tagged DirectOperand tagged DONext8Bits, displacement: False, incomp: IncNo}; // alu[y] n
                7: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // RST y*8
            endcase
        endcase
        IncExtd: case(x)
            0,3: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // NONI, NOP
            1: case(z)
                0: if(y!=6)
                    return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // IN r[y], (C)
                else
                    return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // IN (C)
                1: if(y!=6)
                    return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // OUT (C), r[y]
                else
                    return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // OUT (C), 0
                2: case(q)
                    0: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // SBC HL, rp[p]
                    1: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // ADC HL, rp[p]
                endcase
                3: case(q)
                    0: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // LD (nn), rp[p]
                    1: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // LD rp[p], (nn)
                endcase
                4: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // NEG
                5: if(y!=1)
                    return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // RETN
                else
                    return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // RETI
                6: return DecodedInstructionT{op: OpIm, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged DirectOperand tagged DOLiteral (extend(y[0]) + extend(y[1])), src2: tagged NoOperand, displacement: False, incomp: IncNo}; // IM im[y] // XXX: "Odd" interrupt mode 0/1 unimplemented
                7: case(y)
                    0: return DecodedInstructionT{op: OpLd8, alu_op: nop_cmd, dest: tagged DirectOperand tagged DOReg8 RgI, src1: tagged DirectOperand tagged DOReg8 RgA, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // LD I, A
                    1: return DecodedInstructionT{op: OpLd8, alu_op: nop_cmd, dest: tagged DirectOperand tagged DOReg8 RgR, src1: tagged DirectOperand tagged DOReg8 RgA, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // LD R, A
                    2: return DecodedInstructionT{op: OpLd8, alu_op: nop_cmd, dest: tagged DirectOperand tagged DOReg8 RgA, src1: tagged DirectOperand tagged DOReg8 RgI, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // LD A, I
                    3: return DecodedInstructionT{op: OpLd8, alu_op: nop_cmd, dest: tagged DirectOperand tagged DOReg8 RgA, src1: tagged DirectOperand tagged DOReg8 RgR, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // LD A, R
                    4: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // RRD
                    5: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // RLD
                    6,7: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // NOP
                endcase
            endcase
            2: if(z<4 && y>=4)
                return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // bli[y,z]
            else
                return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // NONI, NOP
        endcase
        IncBits: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented
        IncIx: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented
        IncIxBits: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented
        IncIy: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented
        IncIyBits: return DecodedInstructionT{op: OpNop, alu_op: nop_cmd, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented
    endcase
endfunction

endpackage
