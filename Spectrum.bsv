package Spectrum;
import Connectable::*;

import MN23128::*;
import Z80a::*;

(* synthesize *)
module mkSpectrum(Empty);
    MN23128_ifc rom <- mkMN23128;
    Z80a_ifc cpu <- mkZ80a;
    Bit#(16) cpu_addr = cpu.bus.addr();

    mkConnection(cpu.bus.data, rom.data);
    rule rom_addr(cpu_addr[15:14] == 0);
        rom.addr(cpu_addr()[13:0]);
    endrule

    rule dont_wait;
        cpu.bus.n_wait(1);
    endrule
endmodule

endpackage
