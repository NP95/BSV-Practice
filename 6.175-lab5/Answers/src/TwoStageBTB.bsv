// TwoStageBTB.bsv
//
// This is a two stage pipelined implementation of the SMIPS processor with a BTB.

import Types::*;
import ProcTypes::*;
import MemTypes::*;
import RFile::*;
import IMemory::*;
import DMemory::*;
import Decode::*;
import Exec::*;
import Cop::*;
import Vector::*;
import Fifo::*;
import Ehr::*;
import Btb::*;

(* synthesize *)
module mkProc(Proc);

    Ehr#(2, Bit#(32)) pc   <- mkEhr(0);
    RFile             rf   <- mkRFile;
    IMemory           iMem <- mkIMemory;
    DMemory           dMem <- mkDMemory;
    Cop               cop  <- mkCop;
    Btb#(6,8)         btb  <- mkBtb;

    Bool memReady = iMem.init.done() && dMem.init.done();


    // TODO: Complete the implementation of this processor
    
    Fifo#(2, Bit#(32)) f2d_pc   <- mkCFFifo;
    Fifo#(2, Bit#(32)) f2d_ppc  <- mkCFFifo;
    Fifo#(2, Data)     f2d_inst <- mkCFFifo;

    rule doFetch(cop.started);

        // Fetch
        let inst = iMem.req(pc[0]);
        let ppc = btb.predPc(pc[0]); pc[0] <= ppc;
        f2d_pc.enq(pc[0]);
        f2d_ppc.enq(ppc);
        f2d_inst.enq(inst);

        $display("pc: %h inst: (%h) expanded: ", pc[0], inst, showInst(inst));

    endrule

    rule doExecute(cop.started);

        // Decode
        let inpc  = f2d_pc.first;
        let ppc   = f2d_ppc.first;
        let inst  = f2d_inst.first;
        let dInst = decode(inst);

        // Register Read
        let rVal1  = rf.rd1(validRegValue(dInst.src1));
        let rVal2  = rf.rd2(validRegValue(dInst.src2));
        let copVal = cop.rd(validRegValue(dInst.src1));

        // Execute
        let eInst = exec(dInst, rVal1, rVal2, inpc, ppc, copVal);
        if(eInst.iType == Unsupported) begin
            $fwrite(stderr, "Executing unsupported instruction at pc: %x. Exiting\n", inpc);
            $finish;
        end

        // Memory Read/Write
        if(eInst.iType == Ld) begin
            eInst.data <- dMem.req(MemReq{op: Ld, addr: eInst.addr, data: ?});
        end else if(eInst.iType == St) begin
            let d <- dMem.req(MemReq{op: St, addr: eInst.addr, data: eInst.data});
        end

        // Register Write
        if(isValid(eInst.dst) && validValue(eInst.dst).regType == Normal) begin
            rf.wr(validRegValue(eInst.dst), eInst.data);
        end
        cop.wr(eInst.dst, eInst.data);

        // Branching
        if(eInst.mispredict) begin
            btb.update(inpc, eInst.addr);
            pc[1] <= eInst.addr;
            f2d_pc.clear;
            f2d_ppc.clear;
            f2d_inst.clear;
        end else begin
            f2d_pc.deq;
            f2d_ppc.deq;
            f2d_inst.deq;
        end

    endrule


    method ActionValue#(Tuple2#(RIndx, Data)) cpuToHost;
        let ret <- cop.cpuToHost;
        return ret;
    endmethod

    method Action hostToCpu(Bit#(32) startpc) if ( !cop.started && memReady );
        cop.start;
        pc[0] <= startpc;
    endmethod

    interface MemInit iMemInit = iMem.init;
    interface MemInit dMemInit = dMem.init;
endmodule

