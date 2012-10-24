package FuseTestTypes;

//import GeneratedTests::*;

typedef struct {
    String comment;

    Bit#(16) af;
    Bit#(16) bc;
    Bit#(16) de;
    Bit#(16) hl;
    Bit#(16) af_;
    Bit#(16) bc_;
    Bit#(16) de_;
    Bit#(16) hl_;
    Bit#(16) ix;
    Bit#(16) iy;
    Bit#(16) sp;
    Bit#(16) pc;

    Bit#(8) i; // interrupt vector base register
    Bit#(8) r; // refresh register
    Bit#(1) iff1; // interrupt flip flip 1
    Bit#(1) iff2; // interrupt flip flip 2
    Bit#(2) im; // interrupt mode

    Bit#(1) halted;
    Bit#(1) tstates;
    
    EventListT events;
    MemoryInitT minit;
} TestT;

typedef List#(MemoryInitRunT) MemoryInitT;

typedef struct {
    Bit#(16) startaddr;
    List#(Bit#(8)) payload;
} MemoryInitRunT;

typedef List#(EventT) EventListT;

typedef struct {
    int time_;
    EventTypeT type_;
    Bit#(16) addr;
    Bit#(8) data;
} EventT;

typedef enum {
    EvMR, EvMW, EvMC,
    EvPR, EvPW, EvPC
} EventTypeT;

endpackage
