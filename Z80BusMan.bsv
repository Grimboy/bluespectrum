package Z80BusMan;
// Consider having AddrT and WordT?

import TriState::*;
import StmtFSM::*;
import StmtUtils::*;
import GetPut::*;
import ConfigReg::*;
import Probe::*;
import ClientServer::*;
import Connectable::*;

(* always_ready, always_enabled *)
interface Z80Bus_ifc;
    method Bit#(1) n_m1(); // XXX: Should be tristate
    method Bit#(1) n_mreq(); // "
    method Bit#(1) n_iorq(); // "
    method Bit#(1) n_rd(); // "
    method Bit#(1) n_wr(); // "
    method Bit#(1) n_rfsh();
    method Bit#(16) addr();
    interface Inout#(Bit#(8)) data;
    method Action n_wait(Bit#(1) w);
endinterface

typedef union tagged {
    Bit#(7) BPMemRdRfsh;
    void BPMemRd;
    Bit#(8) BPMemWrData;
    void BPIORd;
    Bit#(8) BPIOWrData;
} BusRqPayloadT deriving (Bits, Eq, Bounded);

typedef struct {
    Bit#(16) addr;
    BusRqPayloadT payload;
} Z80BusManRqT deriving (Bits, Eq, Bounded);

interface Z80BusManager_ifc;
    interface Server#(Z80BusManRqT, Bit#(8)) server;
    interface Z80Bus_ifc bus;
endinterface

(* synthesize *)
module mkZ80BusManager(Z80BusManager_ifc);
    // XXX: Could use M1 too
    ConfigReg#(Maybe#(Z80BusManRqT)) req_reg <- mkConfigReg(tagged Invalid);
    RWire#(Z80BusManRqT) req_in_wire <- mkRWire;
    PulseWire inv_cur_req <- mkPulseWire;
    Maybe#(Z80BusManRqT) cur_req_in = req_in_wire.wget() matches tagged Valid .* ? req_in_wire.wget() : req_reg;
    Bool cur_req_out_valid = !inv_cur_req || (req_reg matches tagged Valid .* ? True : False);

    ConfigReg#(Bool) doing_rfsh <- mkConfigReg(False);
    ConfigReg#(UInt#(1)) rfsh_count <- mkConfigReg(0);
    PulseWire rfsh_done <- mkPulseWire;
    ConfigReg#(UInt#(64)) tstate_count <- mkConfigReg(0);

    // utilise default wires more
    Wire#(Bit#(1)) n_m1_out <- mkDWire(1);
    Wire#(Bit#(1)) n_mreq_out <- mkDWire(1);
    Wire#(Bit#(1)) n_iorq_out <- mkDWire(1);
    Wire#(Bit#(1)) n_rd_out <- mkDWire(1);
    Wire#(Bit#(1)) n_wr_out <- mkDWire(1);
    Wire#(Bit#(1)) n_rfsh_out <- mkDWire(1);
    Wire#(Bit#(1)) n_wait_in <- mkBypassWire;
    Wire#(Bit#(16)) addr_out <- mkDWire(?);
    RWire#(Bit#(8)) data_out <- mkRWire;
    TriState#(Bit#(8)) data_tri <- mkTriState(isValid(data_out.wget()), fromMaybe(?, data_out.wget()));

    rule tstate_inc;
        tstate_count <= tstate_count + 1;
    endrule

    rule do_memrdrfsh_s1_req(cur_req_in matches tagged Valid .req &&& req.payload matches tagged BPMemRdRfsh .rfshaddr &&& !doing_rfsh);
        n_m1_out <= 0;
        n_mreq_out <= 0; // should change on next neg edge
        n_rd_out <= 0;
        addr_out <= req.addr;
    endrule

    rule do_memrdrfsh_s2_req(req_reg matches tagged Valid .req &&& req.payload matches tagged BPMemRdRfsh .rfshaddr &&& doing_rfsh);
        n_mreq_out <= 0; // should change on next neg edge
        n_rfsh_out <= 0;
        addr_out <= {9'b0, rfshaddr};

        rfsh_count <= rfsh_count + 1;
        if (rfsh_count == 1) begin
            rfsh_done.send();
        end
    endrule

    rule do_memrd_req(cur_req_in matches tagged Valid .req &&& req.payload matches tagged BPMemRd);
        n_mreq_out <= 0; // should change on next neg edge
        n_rd_out <= 0;
        addr_out <= req.addr;
    endrule

    rule do_memwr_req(cur_req_in matches tagged Valid .req &&& req.payload matches tagged BPMemWrData .wrdata);
        n_mreq_out <= 0; // should change on next neg edge
        n_wr_out <= 0;
        data_out.wset(wrdata);
        addr_out <= req.addr;
    endrule

    rule do_iord_req(cur_req_in matches tagged Valid .req &&& req.payload matches tagged BPIORd);
        n_iorq_out <= 0; // should change on next neg edge
        n_rd_out <= 0;
        addr_out <= req.addr;
    endrule

    rule do_iowr_req(cur_req_in matches tagged Valid .req &&& req.payload matches tagged BPIOWrData .wrdata);
        n_iorq_out <= 0; // should change on next neg edge
        n_wr_out <= 0;
        data_out.wset(wrdata);
        addr_out <= req.addr;
    endrule

    interface Z80Bus_ifc bus;
        method Bit#(1) n_m1();
            return n_m1_out;
        endmethod

        method Bit#(1) n_mreq();
            return n_mreq_out;
        endmethod

        method Bit#(1) n_iorq();
            return n_iorq_out;
        endmethod

        method Bit#(1) n_rd();
            return n_rd_out;
        endmethod

        method Bit#(1) n_wr();
            return n_wr_out;
        endmethod

        method Bit#(1) n_rfsh();
            return n_rfsh_out;
        endmethod

        method Bit#(16) addr();
            return addr_out;
        endmethod

        interface data = data_tri.io;

        method Action n_wait(Bit#(1) w);
            n_wait_in <= w;
        endmethod
    endinterface

    interface Server server;
        interface Put request;
            method Action put(Z80BusManRqT req) if (inv_cur_req || (req_reg matches tagged Invalid ? True : False));
                req_in_wire.wset(req);
                req_reg <= tagged Valid req;

                // tracy
                String access_type = "";
                Fmt access_data = $format("");
                case (req.payload) matches
                    tagged BPMemRdRfsh .*: begin
                        access_type = "MR";
                    end
                    tagged BPMemRd: begin
                        access_type = "MR";
                    end
                    tagged BPMemWrData .data: begin
                        access_type = "MW";
                        access_data = $format("%h", data);
                    end
                    tagged BPIORd: begin
                        access_type = "PR";
                    end
                    tagged BPIOWrData .data: begin
                        access_type = "PW";
                        access_data = $format("%h", data);
                    end
                endcase
                $write("**%d\t%s\t%h\t", tstate_count, access_type, req.addr);
                $display(access_data);
            endmethod
        endinterface

        interface Get response;
            method ActionValue#(Bit#(8)) get() if (req_reg matches tagged Valid .req &&& (!doing_rfsh && (n_wait_in != 0)) || (doing_rfsh && rfsh_done)); // wait should be sampled on previous neg clock edge
            if (req.payload matches tagged BPMemRdRfsh .* &&& !doing_rfsh) begin
                    doing_rfsh <= True;
                end else begin
                    doing_rfsh <= False;
                    req_reg <= tagged Invalid;
                    inv_cur_req.send();
                end
                return data_tri;
            endmethod
        endinterface
    endinterface
endmodule

(* synthesize *)
module mkZ80BusManagerTb(Empty);
    Z80BusManager_ifc z80busman <- mkZ80BusManager();
    Z80Bus_ifc bus = z80busman.bus;
    Server#(Z80BusManRqT, Bit#(8)) server = z80busman.server;

    Wire#(Bit#(1)) n_wait_in <- mkDWire(0);
    mkConnection(n_wait_in._read, bus.n_wait);

    RWire#(Bit#(8)) data_out <- mkRWire;
    TriState#(Bit#(8)) data_tri <- mkTriState(isValid(data_out.wget()), fromMaybe(?, data_out.wget()));
    mkConnection(bus.data, data_tri.io);

    // Probiness
    Probe#(Bit#(1)) n_mreq_out_probe <- mkProbe();
    mkConnection(bus.n_mreq, n_mreq_out_probe._write);
    Probe#(Bit#(1)) n_iorq_out_probe <- mkProbe();
    mkConnection(bus.n_iorq, n_iorq_out_probe._write);
    Probe#(Bit#(1)) n_rd_out_probe <- mkProbe();
    mkConnection(bus.n_rd, n_rd_out_probe._write);
    Probe#(Bit#(1)) n_wr_out_probe <- mkProbe();
    mkConnection(bus.n_wr, n_wr_out_probe._write);
    Probe#(Bit#(1)) n_rfsh_out_probe <- mkProbe();
    mkConnection(bus.n_rfsh, n_rfsh_out_probe._write);
    Probe#(Bit#(1)) n_wait_in_probe <- mkProbe();
    mkConnection(n_wait_in, n_wait_in_probe._write);
    Probe#(Bit#(16)) addr_probe <- mkProbe();
    mkConnection(bus.addr, addr_probe._write);
    Probe#(Bit#(8)) data_probe <- mkProbe();
    mkConnection(data_tri._read, data_probe._write);

    mkAutoFSM(seq
        $dumpvars();
        server.request.put(Z80BusManRqT{
            payload: tagged BPMemRdRfsh 101,
            addr: 4242
        });
        par
            data_out.wset(77);
            n_wait_in <= 1;
            $display("M1 read %d", server.response.get());
        endpar
        server.request.put(Z80BusManRqT{
            payload: tagged BPMemRd,
            addr: 4244
        });
        par
            data_out.wset(79);
            n_wait_in <= 1;
            $display("M1 read %d", server.response.get());
        endpar
        server.request.put(Z80BusManRqT{
            payload: tagged BPIORd,
            addr: 4246
        });
        noAction;
        par
            n_wait_in <= 1;
            data_out.wset(81);
            $display("IO read %d", server.response.get());
        endpar
        noAction;
        server.request.put(Z80BusManRqT{
            payload: tagged BPMemWrData 42,
            addr: 4248
        });
        noAction;
        par
            n_wait_in <= 1;
            dropAV(server.response.get());
            $display("Normal write finished");
        endpar
        noAction;
        server.request.put(Z80BusManRqT{
            payload: tagged BPIOWrData 42,
            addr: 4250
        });
        noAction;
        par
            n_wait_in <= 1;
            dropAV(server.response.get());
            $display("Normal read finished");
        endpar
        noAction;
    endseq);
endmodule

endpackage
