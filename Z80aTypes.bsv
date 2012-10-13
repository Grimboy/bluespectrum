package Z80aTypes;

import FShow::*;
import Reserved::*;

typedef enum {
    RgA, RgF, RgB, RgC,
    RgD, RgE, RgH, RgL,
    RgW, RgZ, RgS, RgP
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
    OpLd,
    OpInc, OpDec, OpInc16, OpDec16, 
    OpAdd, OpAdc, OpSub, OpSbc, OpCp, OpAnd, OpOr, OpXor,
    OpAdd16,

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
    DecodedOperandT dest;
    DecodedOperandT src1;
    DecodedOperandT src2;
    Bool displacement;
    IncompleteInstructionT incomp;
} DecodedInstructionT deriving (Bits, Eq, Bounded);

typedef struct {
    Bool sign;
    Bool zero;
    Reserved#(1) not_used1;
    Bool add_subtract1;
    Reserved#(1) not_used2;
    Bool parity_overflow;
    Bool add_subtract2;
    Bool carry;
} StatusRegT deriving (Bits, Eq, Bounded);

typedef struct {
    Bit#(4) in1;
    Bit#(4) in2;
    Bool carry_in;
    Bool is_sub;
} ALUReqT deriving (Bits, Eq, Bounded);

typedef struct {
    Bool carry_out;
    Bit#(4) out;
} ALURespT deriving (Bits, Eq, Bounded);

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
            OpLd: return $format("OpLd");
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
            OpRlca: return $format("OpRlca");
            OpRla: return $format("OpRla");
            OpDaa: return $format("OpDaa");
            OpScf: return $format("OpScf");
            OpRrca: return $format("OpRrca");
            OpRra: return $format("OpRra");
            OpCpl: return $format("OpCpl");
            OpCcf: return $format("OpCcf");
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

instance FShow#(DecodedInstructionT);
    function Fmt fshow (DecodedInstructionT inst);
        return ($format("<DecodedInstruction") +
            $format("\n op: ") + fshow(inst.op) +
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

InternalOpTypeT tab_alu[8] = {
    OpAdd, OpAdc, OpSub, OpSbc, OpAnd, OpXor, OpOr, OpCp
};

/*
InternalOpTypeT tab_rot[8] = {
}

InternalOpTypeT tab_im[8] = {
}

InternalOpTypeT tab_bli[8][4] = {
}
*/

function DecodedInstructionT decode_simple(Bit#(8) inst, IncompleteInstructionT incomp);
    Bit#(2) x = inst[7:6];
    Bit#(3) y = inst[5:3];
    Bit#(3) z = inst[2:0];

    Bit#(2) p = inst[5:4];
    Bit#(1) q = inst[3];

    //DecodedInstructionT d = DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo};

    case(incomp)
        IncNo: case(x)
            0: case(z)
                0: case(y)
                    0: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // NOP
                    1: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // EX AF, AF'
                    2: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // DJNZ d
                    3: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // JR d
                    default: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // JR cc[y-4], d
                endcase
                1: case(q)
                    0: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // LD rp[p], nn
                    1: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // ADD HL, rp[p]
                endcase
                2: case(q)
                    0: case(p)
                        0, 1: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // LD (rp[p]), A
                        2: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented LD (nn), HL
                        3: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // LD (nn), A
                    endcase
                    1: case(p)
                        0, 1: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // LD A, (rp[p])
                        2: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // LD HL, (nn)
                        3: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // LD A, (nn)
                    endcase
                endcase
                3: case(q)
                    0: return DecodedInstructionT{op: OpInc16, dest: tagged DirectOperand (tagged DOReg8 tab_rp[p]), src1: tagged DirectOperand (tagged DOReg8 tab_rp[p]), src2: tagged NoOperand, displacement: False, incomp: IncNo}; // INC rp[p]
                    1: return DecodedInstructionT{op: OpDec16, dest: tagged DirectOperand (tagged DOReg8 tab_rp[p]), src1: tagged DirectOperand (tagged DOReg8 tab_rp[p]), src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // DEC rp[p]
                endcase
                4: return DecodedInstructionT{op: OpInc, dest: tab_r[y], src1: tab_r[y], src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // INC r[y]
                5: return DecodedInstructionT{op: OpDec, dest: tab_r[y], src1: tab_r[y], src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // DEC r[y]
                6: return DecodedInstructionT{op: OpLd, dest: tab_r[y], src1: tagged DirectOperand tagged DONext8Bits, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // LD r[y], n
                7: case(y)
                    0: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // RLCA
                    1: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // RRCA
                    2: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // RLA
                    3: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // RRA
                    4: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // DAA
                    5: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // CPL
                    6: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // SCF
                    7: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // CCF
                endcase
            endcase
            1: if(z==6 && y==6)
                return DecodedInstructionT{op: OpHalt, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // HALT
            else
                return DecodedInstructionT{op: OpLd, dest: tab_r[y], src1: tab_r[z], src2: tagged NoOperand, displacement: False, incomp: IncNo}; // LD r[y], r[z]
            2: begin 
                return DecodedInstructionT{op: tab_alu[y], dest: tagged DirectOperand (tagged DOReg8 RgA), src1: tagged DirectOperand (tagged DOReg8 RgA), src2: tab_r[z], displacement: False, incomp: IncNo}; // alu[y] r[z]
            end
            3: case(z)
                0: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // RET cc[y]
                1: case(q)
                    0: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // POP rp2[p]
                    1: case (p)
                        0: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // RET
                        1: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // EXX
                        2: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // JP HL
                        3: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // LD SP,HL
                    endcase
                endcase
                2: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // JP cc[y], nn
                3: case(y)
                    0: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // JP nn
                    1: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncBits}; // (CB prefix)
                    2: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // OUT (n), A
                    3: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // IN A, (n)
                    4: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // EX (SP), HL
                    5: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // EX DE, HL
                    6: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // DI
                    7: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // EI
                endcase
                4: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // CALL cc[y], nn
                5: case (q)
                    0: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // PUSH rp2[p]
                    1: case(p)
                        0: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // CALL nn
                        1: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncIx}; // (DD prefix)
                        2: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncExtd}; // (ED prefix)
                        3: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncIy}; // (FD prefix)
                    endcase
                endcase
                6: return DecodedInstructionT{op: tab_alu[y], dest: tagged DirectOperand (tagged DOReg8 RgA), src1: tagged DirectOperand (tagged DOReg8 RgA), src2: tagged DirectOperand tagged DONext8Bits, displacement: False, incomp: IncNo}; // alu[y] n
                7: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // RST y*8
            endcase
        endcase
        IncExtd: case(x)
            0,3: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // NONI, NOP
            1: case(z)
                0: if(y!=6)
                    return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // IN r[y], (C)
                else
                    return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // IN (C)
                1: if(y!=6)
                    return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // OUT (C), r[y]
                else
                    return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // OUT (C), 0
                2: case(q)
                    0: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // SBC HL, rp[p]
                    1: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // ADC HL, rp[p]
                endcase
                3: case(q)
                    0: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // LD (nn), rp[p]
                    1: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // LD rp[p], (nn)
                endcase
                4: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // NEG
                5: if(y!=1)
                    return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // RETN
                else
                    return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // RETI
                6: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // IM im[y]
                7: case(y)
                    0: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // LD I, A
                    1: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // LD R, A
                    2: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // LD A, I
                    3: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // LD A, R
                    4: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // RRD
                    5: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // RLD
                    6,7: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // NOP
                endcase
            endcase
            2: if(z<4 && y>=4)
                return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // bli[y,z]
            else
                return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented // NONI, NOP
        endcase
        IncBits: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented
        IncIx: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented
        IncIxBits: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented
        IncIy: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented
        IncIyBits: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, displacement: False, incomp: IncNo}; // XXX: Unimplemented
    endcase
endfunction

endpackage
