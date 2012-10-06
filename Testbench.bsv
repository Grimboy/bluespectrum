package Testbench;
import Spectrum::*;

(* synthesize *)
module mkSpectrumTb(Empty);
    mkSpectrum;
    Reg#(UInt#(16)) counter <- mkReg(0);

    rule increment;
        counter <= counter + 1;
    endrule

    rule done(counter > 100);
        $finish();
    endrule
endmodule

endpackage
