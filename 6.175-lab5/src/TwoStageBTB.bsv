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
    
 
    endmethod

    method Action hostToCpu(Bit#(32) startpc) if ( !cop.started && memReady );
        cop.start;
        pc[0] <= startpc;
    endmethod

    interface MemInit iMemInit = iMem.init;
    interface MemInit dMemInit = dMem.init;
endmodule

