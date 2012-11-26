package DisplayGenerator;

import Connectable::*;
import RegFile::*;
import StmtFSM::*;

typedef struct {
    Bit#(1) g;
    Bit#(1) r;
    Bit#(1) b;
} ZXRGB deriving (Bits, Eq, Bounded);

typedef struct {
    Bit#(1) b;
    Bit#(1) r;
    Bit#(1) g;
    Bit#(1) i;
} ZXRGBI deriving (Bits, Eq, Bounded);

typedef struct {
    Bool fl;
    Bool hl;
    ZXRGB fg;
    ZXRGB bg;
} AttrByte deriving (Bits, Eq, Bounded);

(* always_ready, always_enabled *)
interface PALGenerator_ifc;
    // inputs
    //method Action border(Bit#(1));
    // outputs
    method Bit#(1) n_hl();
    method Bit#(1) red();
    method Bit#(1) green();
    method Bit#(1) blue();
    method Bit#(1) n_sync();
endinterface

ZXRGB border = ZXRGB{r: 1, g: 1, b: 1};

(* synthesize *)
module mkPALGenerator(PALGenerator_ifc);
    // XXX: Waveform time
    RegFile#(Bit#(14), Bit#(8)) vid_mem <- mkRegFileFullLoad("screendump.rmh");

    Reg#(Bit#(9)) pal_line <- mkReg(0);
    Reg#(Bit#(9)) pal_col <- mkReg(0);

    Bit#(14) attr_addr = {4'b0110, pal_line[7:3], pal_col[7:3]};
    Bit#(14) display_addr = {1'b0, pal_line[7:6], pal_line[2:0], pal_line[5:3], pal_col[7:3]};
    
    AttrByte attr_byte = unpack(vid_mem.sub(attr_addr));
    Bit#(8) pixel_byte = vid_mem.sub(display_addr);
    Bit#(1) pixel_bit = pixel_byte[pal_col[2:0]];
    ZXRGB pixel_color = (pixel_bit > 0) ? attr_byte.fg : attr_byte.bg;
    Wire#(ZXRGB) output_color <- mkBypassWire();

    (* preempts = "pal_screen, pal_line_end" *)
    rule pal_screen(pal_line == 311 && pal_col == 447);
        pal_col <= 0;
        pal_line <= 0;
    endrule

    (* preempts = "pal_line_end, pal_next_pixel" *)
    rule pal_line_end(pal_col == 447);
        pal_col <= 0;
        pal_line <= pal_line + 1;
    endrule

    rule pal_next_pixel;
        pal_col <= pal_col + 1;
    endrule

    rule set_output_color;
        if (pal_line < 192) begin
            if (pal_col < 256 ) begin
                output_color <= pixel_color; // Display
                /*
                if (pal_line == 0 && pal_col == 0) begin
                    $display("###CUT HERE###");
                    $display("P3");
                    $display("256 192");
                    $display("255");
                end
                $display(
                    "%d %d %d",
                    extend(pixel_color.r) * (attr_byte.hl ? 8'hff : 8'hcd),
                    extend(pixel_color.g) * (attr_byte.hl ? 8'hff : 8'hcd),
                    extend(pixel_color.b) * (attr_byte.hl ? 8'hff : 8'hcd)
                );
                */
            end else if ((pal_col >= 416) || (pal_col < 320)) begin
                output_color <= border; // Left and right border
            end else if (pal_col < 416) begin
                output_color <= unpack(0); // HBlank
            end
        end else if ((pal_line >= 256) || (pal_line < 248)) begin
            if ((pal_col < 320) || (pal_col >= 416)) begin
                output_color <= border; // Top and bottom border
            end else begin
                output_color <= unpack(0); // HBlank
            end
        end else begin
            output_color <= unpack(0); // VBlank
        end
    endrule

    method Bit#(1) n_hl();
        return attr_byte.hl ? 0 : 1;
    endmethod

    method Bit#(1) red();
        return output_color.r;
    endmethod

    method Bit#(1) green();
        return output_color.g;
    endmethod

    method Bit#(1) blue();
        return output_color.b;
    endmethod

    method Bit#(1) n_sync();
        if ((pal_line >= 252) || (pal_line < 248)) begin // Not Vsync
            if (pal_col < 336) begin
                return 1; // Not Hsync
            end else if (pal_col < 368) begin // 5C timings
                return 0; // HSync
            end else begin
                return 1; // Not Hsync
            end
        end else begin
            return 0; // VSync
        end
    endmethod
endmodule

`define WIDTH 320
`define HEIGHT 248
`define AREA (WIDTH * HEIGHT)

(* always_ready, always_enabled *)
interface PAL2Framebuffer_ifc;
    // inputs
    method Action n_hl(Bit#(1) h);
    method Action red(Bit#(1) r);
    method Action green(Bit#(1) g);
    method Action blue(Bit#(1) b);
    method Action n_sync(Bit#(1) s);
endinterface

(* synthesize *)
module mkPAL2Framebuffer(PAL2Framebuffer_ifc);
    // 352 * 304 * 2
    RegFile#(Bit#(19), ZXRGBI) triple_buffer <- mkRegFile(0, (1 << 17) * 3);
    Reg#(UInt#(2)) write_buff0 <- mkReg(0);
    Reg#(UInt#(2)) write_buff1 <- mkReg(1);
    Reg#(UInt#(1)) write_buff_index <- mkReg(0);
    UInt#(2) write_buff = write_buff_index > 0 ? write_buff1 : write_buff0;
    Reg#(UInt#(2)) inactive_write_buff = write_buff_index > 0 ?  write_buff0 : write_buff1;
    UInt#(2) read_buff = 3 - (write_buff0 + write_buff1);

    Reg#(Int#(10)) pal_line <- mkReg(0);
    Reg#(Int#(10)) pal_col <- mkReg(0);
    Reg#(UInt#(10)) sync_length <- mkReg(0);


    Wire#(Bit#(1)) n_hl_w <- mkBypassWire();
    Wire#(Bit#(1)) red_w <- mkBypassWire();
    Wire#(Bit#(1)) green_w <- mkBypassWire();
    Wire#(Bit#(1)) blue_w <- mkBypassWire();
    Wire#(Bit#(1)) n_sync_w <- mkBypassWire();

    Reg#(UInt#(9)) ppmi <- mkReg(0);
    Reg#(UInt#(9)) ppmj <- mkReg(0);

    function Action print_buffs();
        return (action
            $display("wb0: %d wb1: %d wbi: %d wb: %d iwb: %d rb: %d", write_buff0, write_buff1, write_buff_index, write_buff, inactive_write_buff, read_buff);
        endaction);
    endfunction

    Stmt framebuffer2netppmstmt =
    seq
        $display("###CUT HERE###");
        $display("P3");
        $display("%d %d", `WIDTH, `HEIGHT);
        $display("255");
        for (ppmj<=0;ppmj<`HEIGHT;ppmj<=ppmj+1)
            for (ppmi<=0;ppmi<`WIDTH;ppmi<=ppmi+1) action
                Bit#(17) read_addr = pack(extend(ppmj) * `WIDTH + extend(ppmi));
                ZXRGBI pixcol = triple_buffer.sub({pack(read_buff), read_addr});
                $display(
                    "%d %d %d",
                    extend(pixcol.r) * ((pixcol.i > 0) ? 8'hff : 8'hcd),
                    extend(pixcol.g) * ((pixcol.i > 0) ? 8'hff : 8'hcd),
                    extend(pixcol.b) * ((pixcol.i > 0) ? 8'hff : 8'hcd)
                );
            endaction
        inactive_write_buff <= read_buff;
        //print_buffs();
    endseq;

    FSM framebuffer2netppmfsm <- mkFSM(framebuffer2netppmstmt);

    (* preempts = "end_line_or_screen, inc_col" *)
    rule inc_col;
        pal_col <= pal_col + 1;
    endrule

/*
    rule trace;
        $display("#wrbf: %d wrad: %d line: %d col: %d r: %d g: %d b: %d i: %d", write_buff, write_addr, pal_line, pal_col, red_w, blue_w, green_w, ~n_hl_w);
    endrule
*/

    rule write_col((0 <= pal_line) && (pal_line < `HEIGHT) && (0 <= pal_col) && (pal_col < `WIDTH));
        /*
        if (pal_line == 0 && pal_col == 0) begin
            $display("###CUT HERE###");
            $display("P3");
            $display("%d %d", `WIDTH, `HEIGHT);
            $display("255");
        end
        $display(
            "%d %d %d",
            8'd127 * extend(red_w),
            8'd127 * extend(blue_w),
            8'd127 * extend(green_w)
        );
        */
        Bit#(17) write_addr = pack(extend(pal_line) * `WIDTH + extend(pal_col));
        triple_buffer.upd({pack(write_buff), write_addr}, ZXRGBI{r: red_w, b: blue_w, g: green_w, i: ~n_hl_w});
    endrule

    rule printfb(pal_line == (`HEIGHT - 1) && pal_col == (`WIDTH - 1));
        framebuffer2netppmfsm.start();
        write_buff_index <= write_buff_index == 1 ? 0 : 1;
    endrule

    /*rule printbuffs(pal_line == (`HEIGHT - 1) && pal_col == `WIDTH);
        print_buffs();
    endrule*/

    rule synching(n_sync_w == 0);
        sync_length <= sync_length + 1;
    endrule

    rule end_line_or_screen(n_sync_w == 1 && sync_length >= 30);
        if (sync_length >= 448) begin
            pal_line <= -60;
            pal_col <= -79;
        end else begin
            pal_line <= pal_line + 1;
            pal_col <= -79;
        end
    endrule

    rule end_sync(n_sync_w == 1);
        sync_length <= 0;
    endrule

    method Action n_hl(Bit#(1) h);
        n_hl_w <= h;
    endmethod

    method Action red(Bit#(1) r);
        red_w <= r;
    endmethod

    method Action green(Bit#(1) g);
        green_w <= g;
    endmethod

    method Action blue(Bit#(1) b);
        blue_w <= b;
    endmethod

    method Action n_sync(Bit#(1) s);
        n_sync_w <= s;
    endmethod
endmodule

(* synthesize *)
module mkDisplayGeneratorTb(Empty);
    Reg#(UInt#(32)) step <- mkReg(0);

    PALGenerator_ifc palgen <- mkPALGenerator();
    PAL2Framebuffer_ifc pal2fb <- mkPAL2Framebuffer();

    mkConnection(palgen.n_hl, pal2fb.n_hl);
    mkConnection(palgen.red, pal2fb.red);
    mkConnection(palgen.green, pal2fb.green);
    mkConnection(palgen.blue, pal2fb.blue);
    mkConnection(palgen.n_sync, pal2fb.n_sync);

    rule inc_step;
        step <= step + 1;
    endrule

    rule dumpvars(step == 0);
        $dumpvars();
    endrule

    // 312 * 448
    rule done(step > 10000000);
        $finish();
    endrule
endmodule

endpackage
