package Z80RF;

import Z80Types::*;
import Vector::*;

interface RegisterFileIfc;
    // other
    method Action write_8b(Reg8T regg, Bit#(8) data);
    method Bit#(8) read_8b(Reg8T regg);
    method Action write_16b(Reg16T regg, Bit#(16) data);
    method Bit#(16) read_16b(Reg16T regg);
    method Action swap_af();
    method Action swap_gp();
    method Action trace_regs(Bit#(16) pc);
endinterface

function Reg8T low_reg_byte(Reg16T regg);
    return unpack(extend(pack(regg)) * 2 + 1);
endfunction

function Reg8T high_reg_byte(Reg16T regg);
    return unpack(extend(pack(regg)) * 2);
endfunction

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
        return tuple2(high_reg_byte(regg), low_reg_byte(regg));
    endfunction

    method Action trace_regs(Bit#(16) pc);
        $display("**%h%h %h%h %h%h %h%h %h%h %h%h %h%h %h%h %h%h %h%h %h%h %h",
            rf[0], rf[1], rf[2], rf[3],
            rf[4], rf[5], rf[6], rf[7],
            rf_shad[0], rf_shad[1], rf_shad[2], rf_shad[3],
            rf_shad[4], rf_shad[5], rf_shad[6], rf_shad[7],
            rf[12], rf[13], rf[14], rf[15],
            rf[10], rf[11], pc
        );
    endmethod

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
