package Z80aTypes;

import FShow::*;

typedef enum {
    RgB, RgC, RgD, RgE, RgH, RgL
} Reg8T deriving (Bits, Eq, Bounded);

typedef enum {
    RgBC, RgDE, RgHL,
    RgSP, RgAF
} Reg16T deriving (Bits, Eq, Bounded);

typedef enum {
    CcNZ, CcZ, CcNC, CcC, CcPO, CcPE, CcP, CcM
} ConditionCodeT deriving (Bits, Eq, Bounded);

typedef enum {
    OpNop, OpHalt,
    OpLd,
    OpInc, OpDec, 
    OpAdd, OpAdc, OpSub, OpSbc, OpCp, OpAnd, OpOr, OpXor,
    OpAdd16,

    OpRlca, OpRla, OpDaa, OpScf, OpRrca, OpRra, OpCpl, OpCcf
} InternalOpTypeT deriving (Bits, Eq, Bounded);

typedef enum {
    IncNo, IncExtd, IncBits, IncIx, IncIxBits, IncIy, IncIyBits
} IncompleteInstructionT deriving (Bits, Eq, Bounded);

typedef union tagged {
    void DOAcc;
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
  IncompleteInstructionT incomp;
} DecodedInstructionT deriving (Bits, Eq, Bounded);

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
            tagged DOAcc:
                return $format("Acc");
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
    tagged DirectOperand DOAcc
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

function DecodedInstructionT decode_simple(Bit#(8) inst);
    Bit#(2) x = inst[7:6];
    Bit#(3) y = inst[5:3];
    Bit#(3) z = inst[2:0];

    Bit#(2) p = inst[5:4];
    Bit#(1) q = inst[3];

    //DecodedInstructionT d = DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo};

    return case(x)
        0: case(z)
            0: case(y)
                0: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // NOP
                1: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // EX AF, AF'
                2: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // DJNZ d
                3: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // JR d
                default: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // JR cc[y-4], d
            endcase
            1: case(q)
                0: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // LD rp[p], nn
                1: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // ADD HL, rp[p]
            endcase
            2: case(q)
                0: case(p)
                    0, 1: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // LD (rp[p]), A
                    2: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented LD (nn), HL
                    3: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // LD (nn), A
                endcase
                1: case(p)
                    0, 1: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // LD A, (rp[p])
                    2: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // LD HL, (nn)
                    3: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // LD A, (nn)
                endcase
            endcase
            3: case(q)
                0: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // INC rp[p]
                1: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // DEC rp[p]
            endcase
            4: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // INC r[y]
            5: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // DEC r[y]
            6: return DecodedInstructionT{op: OpLd, dest: tab_r[y], src1: tagged DirectOperand tagged DONext8Bits, src2: tagged NoOperand, incomp: IncNo}; // LD r[y], n
            7: case(y)
                0: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // RLCA
                1: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // RRCA
                2: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // RLA
                3: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // RRA
                4: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // DAA
                5: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // CPL
                6: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // SCF
                7: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // CCF
            endcase
        endcase
        1: if(z==6 && y==6)
            return DecodedInstructionT{op: OpHalt, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // HALT
        else
            return DecodedInstructionT{op: OpLd, dest: tab_r[y], src1: tab_r[z], src2: tagged NoOperand, incomp: IncNo}; // LD r[y], r[z]
        2: begin 
            return DecodedInstructionT{op: tab_alu[y], dest: tagged DirectOperand DOAcc, src1: tagged DirectOperand DOAcc, src2: tab_r[z], incomp: IncNo}; // alu[y] r[z]
        end
        3: case(z)
            0: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // RET cc[y]
            1: case(q)
                0: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // POP rp2[p]
                1: case (p)
                    0: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // RET
                    1: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // EXX
                    2: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // JP HL
                    3: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // LD SP,HL
                endcase
            endcase
            2: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // JP cc[y], nn
            3: case(y)
                0: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // JP nn
                1: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncBits}; // (CB prefix)
                2: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // OUT (n), A
                3: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // IN A, (n)
                4: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // EX (SP), HL
                5: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // EX DE, HL
                6: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // DI
                7: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // EI
            endcase
            4: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // CALL cc[y], nn
            5: case (q)
                0: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // PUSH rp2[p]
                1: case(p)
                    0: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // CALL nn
                    1: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncIx}; // (DD prefix)
                    2: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncExtd}; // (ED prefix)
                    3: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncIy}; // (FD prefix)
                endcase
            endcase
            6: return DecodedInstructionT{op: tab_alu[y], dest: tagged DirectOperand DOAcc, src1: tagged DirectOperand DOAcc, src2: tagged DirectOperand tagged DONext8Bits, incomp: IncNo}; // alu[y] n
            7: return DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}; // XXX: Unimplemented // RST y*8
        endcase
    endcase;
endfunction

DecodedInstructionT main_dt[256] = {
    DecodedInstructionT{op: OpNop, dest: tagged NoOperand, src1: tagged NoOperand, src2: tagged NoOperand, incomp: IncNo}, // 0x00
    ?, // 0x01
    ?, // 0x02
    ?, // 0x03
    ?, // 0x04
    ?, // 0x05
    DecodedInstructionT{op: OpLd, dest: tagged DirectOperand tagged DOReg8 RgB, src1: tagged DirectOperand tagged DONext8Bits, src2: tagged NoOperand, incomp: IncNo}, // 0x06
    ?, // 0x07
    ?, // 0x08
    ?, // 0x09
    ?, // 0x0a
    ?, // 0x0b
    ?, // 0x0c
    ?, // 0x0d
    DecodedInstructionT{op: OpLd, dest: tagged DirectOperand tagged DOReg8 RgC, src1: tagged DirectOperand tagged DONext8Bits, src2: tagged NoOperand, incomp: IncNo}, // 0x0e
    ?, // 0x0f
    ?, // 0x10
    ?, // 0x11
    ?, // 0x12
    ?, // 0x13
    ?, // 0x14
    ?, // 0x15
    DecodedInstructionT{op: OpLd, dest: tagged DirectOperand tagged DOReg8 RgD, src1: tagged DirectOperand tagged DONext8Bits, src2: tagged NoOperand, incomp: IncNo}, // 0x16
    ?, // 0x17
    ?, // 0x18
    ?, // 0x19
    ?, // 0x1a
    ?, // 0x1b
    ?, // 0x1c
    ?, // 0x1d
    DecodedInstructionT{op: OpLd, dest: tagged DirectOperand tagged DOReg8 RgE, src1: tagged DirectOperand tagged DONext8Bits, src2: tagged NoOperand, incomp: IncNo}, // 0x1e
    ?, // 0x1f
    ?, // 0x20
    ?, // 0x21
    ?, // 0x22
    ?, // 0x23
    ?, // 0x24
    ?, // 0x25
    DecodedInstructionT{op: OpLd, dest: tagged DirectOperand tagged DOReg8 RgH, src1: tagged DirectOperand tagged DONext8Bits, src2: tagged NoOperand, incomp: IncNo}, // 0x26
    ?, // 0x27
    ?, // 0x28
    ?, // 0x29
    ?, // 0x2a
    ?, // 0x2b
    ?, // 0x2c
    ?, // 0x2d
    DecodedInstructionT{op: OpLd, dest: tagged DirectOperand tagged DOReg8 RgL, src1: tagged DirectOperand tagged DONext8Bits, src2: tagged NoOperand, incomp: IncNo}, // 0x2e
    ?, // 0x2f
    ?, // 0x30
    ?, // 0x31
    ?, // 0x32
    ?, // 0x33
    ?, // 0x34
    ?, // 0x35
    ?, // 0x36
    ?, // 0x37
    ?, // 0x38
    ?, // 0x39
    ?, // 0x3a
    ?, // 0x3b
    ?, // 0x3c
    ?, // 0x3d
    DecodedInstructionT{op: OpLd, dest: tagged DirectOperand tagged DOAcc, src1: tagged DirectOperand tagged DONext8Bits, src2: tagged NoOperand, incomp: IncNo}, // 0x3e
    ?, // 0x3f
    DecodedInstructionT{op: OpLd, dest: tagged DirectOperand tagged DOReg8 RgB, src1: tagged DirectOperand tagged DOReg8 RgB, src2: tagged NoOperand, incomp: IncNo}, // 0x40
    DecodedInstructionT{op: OpLd, dest: tagged DirectOperand tagged DOReg8 RgB, src1: tagged DirectOperand tagged DOReg8 RgC, src2: tagged NoOperand, incomp: IncNo}, // 0x41
    DecodedInstructionT{op: OpLd, dest: tagged DirectOperand tagged DOReg8 RgB, src1: tagged DirectOperand tagged DOReg8 RgD, src2: tagged NoOperand, incomp: IncNo}, // 0x42
    ?, // 0x43
    ?, // 0x44
    ?, // 0x45
    ?, // 0x46
    DecodedInstructionT{op: OpLd, dest: tagged DirectOperand tagged DOReg8 RgB, src1: tagged DirectOperand tagged DOAcc, src2: tagged NoOperand, incomp: IncNo}, // 0x47
    DecodedInstructionT{op: OpLd, dest: tagged DirectOperand tagged DOReg8 RgC, src1: tagged DirectOperand tagged DOReg8 RgB, src2: tagged NoOperand, incomp: IncNo}, // 0x48
    DecodedInstructionT{op: OpLd, dest: tagged DirectOperand tagged DOReg8 RgC, src1: tagged DirectOperand tagged DOReg8 RgC, src2: tagged NoOperand, incomp: IncNo}, // 0x49
    DecodedInstructionT{op: OpLd, dest: tagged DirectOperand tagged DOReg8 RgC, src1: tagged DirectOperand tagged DOReg8 RgD, src2: tagged NoOperand, incomp: IncNo}, // 0x4a
    ?, // 0x4b
    ?, // 0x4c
    ?, // 0x4d
    ?, // 0x4e
    ?, // 0x4f
    ?, // 0x50
    ?, // 0x51
    ?, // 0x52
    ?, // 0x53
    ?, // 0x54
    ?, // 0x55
    ?, // 0x56
    DecodedInstructionT{op: OpLd, dest: tagged DirectOperand tagged DOReg8 RgD, src1: tagged DirectOperand tagged DOAcc, src2: tagged NoOperand, incomp: IncNo}, // 0x57
    ?, // 0x58
    ?, // 0x59
    ?, // 0x5a
    ?, // 0x5b
    ?, // 0x5c
    ?, // 0x5d
    ?, // 0x5e
    ?, // 0x5f
    ?, // 0x60
    ?, // 0x61
    ?, // 0x62
    ?, // 0x63
    ?, // 0x64
    ?, // 0x65
    ?, // 0x66
    ?, // 0x67
    ?, // 0x68
    ?, // 0x69
    ?, // 0x6a
    ?, // 0x6b
    ?, // 0x6c
    ?, // 0x6d
    ?, // 0x6e
    ?, // 0x6f
    ?, // 0x70
    ?, // 0x71
    ?, // 0x72
    ?, // 0x73
    ?, // 0x74
    ?, // 0x75
    ?, // 0x76
    ?, // 0x77
    ?, // 0x78
    DecodedInstructionT{op: OpLd, dest: tagged DirectOperand tagged DOAcc, src1: tagged DirectOperand tagged DOReg8 RgC, src2: tagged NoOperand, incomp: IncNo}, // 0x79
    ?, // 0x7a
    ?, // 0x7b
    ?, // 0x7c
    ?, // 0x7d
    ?, // 0x7e
    ?, // 0x7f
    ?, // 0x80
    ?, // 0x81
    ?, // 0x82
    ?, // 0x83
    ?, // 0x84
    ?, // 0x85
    ?, // 0x86
    ?, // 0x87
    ?, // 0x88
    ?, // 0x89
    ?, // 0x8a
    ?, // 0x8b
    ?, // 0x8c
    ?, // 0x8d
    ?, // 0x8e
    ?, // 0x8f
    ?, // 0x90
    ?, // 0x91
    ?, // 0x92
    ?, // 0x93
    ?, // 0x94
    ?, // 0x95
    ?, // 0x96
    ?, // 0x97
    ?, // 0x98
    ?, // 0x99
    ?, // 0x9a
    ?, // 0x9b
    ?, // 0x9c
    ?, // 0x9d
    ?, // 0x9e
    ?, // 0x9f
    ?, // 0xa0
    ?, // 0xa1
    ?, // 0xa2
    ?, // 0xa3
    ?, // 0xa4
    ?, // 0xa5
    ?, // 0xa6
    ?, // 0xa7
    ?, // 0xa8
    ?, // 0xa9
    ?, // 0xaa
    ?, // 0xab
    ?, // 0xac
    ?, // 0xad
    ?, // 0xae
    ?, // 0xaf
    ?, // 0xb0
    ?, // 0xb1
    ?, // 0xb2
    ?, // 0xb3
    ?, // 0xb4
    ?, // 0xb5
    ?, // 0xb6
    ?, // 0xb7
    ?, // 0xb8
    ?, // 0xb9
    ?, // 0xba
    ?, // 0xbb
    ?, // 0xbc
    ?, // 0xbd
    ?, // 0xbe
    ?, // 0xbf
    ?, // 0xc0
    ?, // 0xc1
    ?, // 0xc2
    ?, // 0xc3
    ?, // 0xc4
    ?, // 0xc5
    ?, // 0xc6
    ?, // 0xc7
    ?, // 0xc8
    ?, // 0xc9
    ?, // 0xca
    ?, // 0xcb
    ?, // 0xcc
    ?, // 0xcd
    ?, // 0xce
    ?, // 0xcf
    ?, // 0xd0
    ?, // 0xd1
    ?, // 0xd2
    ?, // 0xd3
    ?, // 0xd4
    ?, // 0xd5
    ?, // 0xd6
    ?, // 0xd7
    ?, // 0xd8
    ?, // 0xd9
    ?, // 0xda
    ?, // 0xdb
    ?, // 0xdc
    ?, // 0xdd
    ?, // 0xde
    ?, // 0xdf
    ?, // 0xe0
    ?, // 0xe1
    ?, // 0xe2
    ?, // 0xe3
    ?, // 0xe4
    ?, // 0xe5
    ?, // 0xe6
    ?, // 0xe7
    ?, // 0xe8
    ?, // 0xe9
    ?, // 0xea
    ?, // 0xeb
    ?, // 0xec
    ?, // 0xed
    ?, // 0xee
    ?, // 0xef
    ?, // 0xf0
    ?, // 0xf1
    ?, // 0xf2
    ?, // 0xf3
    ?, // 0xf4
    ?, // 0xf5
    ?, // 0xf6
    ?, // 0xf7
    ?, // 0xf8
    ?, // 0xf9
    ?, // 0xfa
    ?, // 0xfb
    ?, // 0xfc
    ?, // 0xfd
    ?, // 0xfe
    ? // 0xff
};

endpackage
