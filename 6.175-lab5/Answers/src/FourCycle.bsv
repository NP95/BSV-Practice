// FourCycle.bsv
//
// This is a four cycle implementation of the SMIPS processor.

import Types::*;
import ProcTypes::*;
import MemTypes::*;
import RFile::*;
import DelayedMemory::*;
import MemInit::*;
import Decode::*;
import Exec::*;
import Cop::*;
import Vector::*;
import Fifo::*;
import Ehr::*;

typedef enum {Fetch, Decode, Execute, WriteBack} Stage deriving (Bits, Eq, FShow);

(* synthesize *)
module mkProc(Proc);

    Reg#(Addr)     pc <- mkRegU;
    RFile          rf <- mkRFile;
    DelayedMemory mem <- mkDelayedMemory;
    Cop           cop <- mkCop;

    MemInitIfc dummyMemInit <- mkDummyMemInit;
    Bool memReady = mem.init.done() && dummyMemInit.done();


    // TODO: Complete the implementation of this processor

    Reg#(Stage)       stage  <- mkReg(Fetch);
    Reg#(DecodedInst) dInst  <- mkRegU;
    Reg#(ExecInst)    eInst  <- mkRegU;

    rule doFetch(cop.started && stage == Fetch);
            mem.req(MemReq{op: Ld, addr: pc, data: ?});
            stage <= Decode;
    endrule
    
    rule doDecode(cop.started && stage == Decode);
        Data inst <- mem.resp();
        dInst <= decode(inst);
        stage <= Execute;
    endrule
    
    rule doExecute(cop.started && stage == Execute);
        
        // Register Read
        Data rVal1  = rf.rd1(validRegValue(dInst.src1));
        Data rVal2  = rf.rd2(validRegValue(dInst.src2));
        Data copVal = cop.rd(validRegValue(dInst.src1));

        // Execute
        ExecInst _eInst = exec(dInst, rVal1, rVal2, pc, ?, copVal); eInst <= _eInst;
        if(_eInst.iType == Unsupported) begin
            $fwrite(stderr, "Executing unsupported instruction at pc: %x. Exiting\n", pc);
            $finish;
        end
        
        // Memory Read/Write
        if(_eInst.iType == Ld) begin
            mem.req(MemReq{op: Ld, addr: _eInst.addr, data: ?});
        end else if(_eInst.iType == St) begin
            mem.req(MemReq{op: St, addr: _eInst.addr, data: _eInst.data});
        end

        stage <= WriteBack;
        
    endrule
    
    rule doWriteBack(cop.started && stage == WriteBack);
        
        Data data = eInst.data;
        if(eInst.iType == Ld) begin
            data <- mem.resp();
        end
        
        // Write Back
        if(isValid(eInst.dst) && validValue(eInst.dst).regType == Normal) begin
            rf.wr(validRegValue(eInst.dst), data);
        end
        cop.wr(eInst.dst, data);
        
        // update the pc depending on whether the branch is taken or not
        pc <= eInst.brTaken ? eInst.addr : pc + 4;

        stage <= Fetch;

    endrule


    method ActionValue#(Tuple2#(RIndx, Data)) cpuToHost;
        let ret <- cop.cpuToHost;
        return ret;
    endmethod

    method Action hostToCpu(Bit#(32) startpc) if ( !cop.started && memReady );
        cop.start;
        pc <= startpc;
    endmethod

    interface MemInit iMemInit = dummyMemInit;
    interface MemInit dMemInit = mem.init;
endmodule

