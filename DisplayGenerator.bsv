package DisplayGenerator;

import Connectable::*;
import RegFile::*;

typedef struct {
    Bit#(1) r;
    Bit#(1) g;
    Bit#(1) b;
} ZXRGB deriving (Bits, Eq, Bounded);

typedef struct {
    Bit#(1) r;
    Bit#(1) g;
    Bit#(1) b;
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

ZXRGB border = ZXRGB{r: 0, g: 0, b: 0};

module mkPALGenerator(PALGenerator_ifc);
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

    (* descending_urgency = "pal_screen, pal_line_end, pal_next_pixel" *)
    rule pal_screen(pal_line == 311 && pal_col == 447);
        pal_col <= 0;
        pal_line <= 0;
    endrule

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
            end else if (pal_col < 320) begin
                output_color <= border; // Right border
            end else if (pal_col < 416) begin
                output_color <= unpack(0); // HBlank
            end else begin
                output_color <= border; // Left border
            end
        end else if (pal_line < 248) begin
            output_color <= border; // Bottom border
        end else if (pal_line < 256) begin
            output_color <= unpack(0); // VBlank
        end else begin
            output_color <= border; // Top border
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
        if (pal_line < 192) begin // Not Vsync
            if (pal_col < 336 ) begin
                return 1; // Not Hsync
            end else if (pal_col < 368) begin
                return 0; // HSync
            end else begin
                return 1; // Not Hsync
            end
        end else if (pal_line < 252) begin
            return 0; // VSync
        end else begin
            return 1; // Not Vsync
        end
    endmethod
endmodule

`define WIDTH 352
`define HEIGHT 304

(* always_ready, always_enabled *)
interface PAL2Framebuffer_ifc;
    // inputs
    method Action n_hl(Bit#(1) h);
    method Action red(Bit#(1) r);
    method Action green(Bit#(1) g);
    method Action blue(Bit#(1) b);
    method Action n_sync(Bit#(1) s);
endinterface

module mkPAL2Framebuffer(PAL2Framebuffer_ifc);
    // 352 * 304 * 2
    RegFile#(Bit#(18), ZXRGBI) double_buffer <- mkRegFile(0, 262143);
    Reg#(Bit#(1)) write_buff <- mkReg(0);
    Bit#(1) read_buff = ~write_buff;

    Reg#(Bit#(9)) pal_line <- mkReg(0);
    Reg#(Bit#(9)) pal_col <- mkReg(0);
    Reg#(Bit#(10)) sync_length <- mkReg(0);

    Bit#(17) write_addr = extend(pal_line) * `WIDTH + extend(pal_col);

    Wire#(Bit#(1)) n_hl_w <- mkBypassWire();
    Wire#(Bit#(1)) red_w <- mkBypassWire();
    Wire#(Bit#(1)) green_w <- mkBypassWire();
    Wire#(Bit#(1)) blue_w <- mkBypassWire();

    function Action framebuffer2netppm();
        return (action
            $display("###CUT HERE###");
            $display("P3");
            $display("%d %d", `WIDTH, `HEIGHT);
            $display("255");
            for (int j=0;j<`HEIGHT;j=j+1) begin
                for (int i=0;i<`WIDTH;i=i+1) begin
                    Bit#(17) read_addr = truncate(pack(j)) * `WIDTH + truncate(pack(i));
                    ZXRGBI pixcol = double_buffer.sub({read_buff, read_addr});
                    $display(
                        "%d %d %d",
                        8'd127 * (extend(pixcol.r) + extend(pixcol.i)),
                        8'd127 * (extend(pixcol.g) + extend(pixcol.i)),
                        8'd127 * (extend(pixcol.b) + extend(pixcol.i))
                    );
                end
            end
        endaction);
    endfunction

    rule write_col ((pal_line < `WIDTH) && (pal_col < `HEIGHT));
        double_buffer.upd({write_buff, write_addr}, ZXRGBI{r: red_w, b: blue_w, g: green_w, i: ~n_hl_w});
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
        if (s == 0) begin
            sync_length <= sync_length + 1;
        end else begin
            if (sync_length >= 448) begin
                pal_line <= 0;
                framebuffer2netppm();
            end
            pal_col <= 0;
            sync_length <= 0;
        end
    endmethod
endmodule

module mkDisplayGeneratorTb(Empty);
    Reg#(UInt#(20)) step <- mkReg(0);

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

    // 312 * 448
    rule done(step > 150000);
        $finish();
    endrule
endmodule

endpackage
