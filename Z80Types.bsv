package Z80Types;

import FShow::*;
import Reserved::*;

typedef Bit#(16) Z80AddrT;
typedef Bit#(8) Z80WordT;

typedef enum {
    RgA, RgF, RgB, RgC,
    RgD, RgE, RgH, RgL,
    RgW, RgZ, RgS, RgP,
    RgIXl, RgIXh, RgIYl, RgIYh,
    RgI, RgR
} Reg8T deriving (Bits, Eq, Bounded);

typedef enum {
    RgAF, RgBC, RgDE, RgHL,
    RgWZ, RgSP, RgIX, RgIY
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
    OpRlca, OpRla, OpDaa, OpScf, OpRrca, OpRra, OpCpl, OpCcf,
    OpEx, OpExAF, OpExGP,
    OpJp
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
    Bool need_displacement;
    Bit#(8) displacement;
    IncompleteInstructionT incomp;
} DecodedInstructionT deriving (Bits, Eq, Bounded);

typedef struct {
    Bool sign; // S
    Bool zero; // Z
    Bool bit5;
    Bool half_carry; // H
    Bool bit3;
    Bool parity_overflow; // P/V
    Bool subtract; // N
    Bool carry; // C
} FlagRegT deriving (Bits, Eq, Bounded);

typedef enum {
    AluOpNop, AluOpAdd, AluOpSub, AluOpAnd, AluOpXor, AluOpOr, AluOpCp
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
            OpEx: return $format("OpEx");
            OpExAF: return $format("OpExAF");
            OpExGP: return $format("OpExGP");
            OpJp: return $format("OpJp");
        endcase
    endfunction
endinstance

instance FShow#(Reg8T);
    function Fmt fshow (Reg8T reg8);
        case(reg8) matches
            RgA: return $format("RgA");
            RgF: return $format("RgF");
            RgB: return $format("RgB");
            RgC: return $format("RgC");
            RgD: return $format("RgD");
            RgE: return $format("RgE");
            RgH: return $format("RgH");
            RgL: return $format("RgL");
            RgW: return $format("RgW");
            RgZ: return $format("RgZ");
            RgS: return $format("RgS");
            RgP: return $format("RgP");
            RgIXl: return $format("RgIXl");
            RgIXh: return $format("RgIXh");
            RgIYl: return $format("RgIYl");
            RgIYh: return $format("RgIYh");
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
            RgWZ: return $format("RgWZ");
            RgSP: return $format("RgSP");
            RgIX: return $format("RgIX");
            RgIY: return $format("RgIY");
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
            AluOpCp: return $format("AluOpCp");
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
            $format("\n need_displacement: ") + fshow(inst.need_displacement) +
            $format(">"));
    endfunction
endinstance

endpackage
