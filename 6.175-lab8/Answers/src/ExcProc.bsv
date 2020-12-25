// ExcProc.bsv
//
// This is a one cycle implementation of the SMIPS processor.

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

(* synthesize *)
module mkProc( Proc );
    
    Reg#( Addr ) pc   <- mkRegU;
    Reg#( Data ) hi   <- mkReg( 0 );
    Reg#( Data ) lo   <- mkReg( 0 );
    
    RFile        rf   <- mkRFile;
    IMemory      iMem <- mkIMemory;
    DMemory      dMem <- mkDMemory;
    Cop          cop  <- mkCop;
    
    Bool memReady = iMem.init.done && dMem.init.done;
    
   
        
    endrule
    
    method ActionValue#( Tuple2#( RIndx, Data ) ) cpuToHost;
        let ret <- cop.cpuToHost;
        return ret;
    endmethod
    
    method Action hostToCpu( Bit#( 32 ) startpc ) if ( !cop.started && memReady );
        cop.start;
        pc <= startpc;
    endmethod
    
    interface MemInit iMemInit = iMem.init;
    interface MemInit dMemInit = dMem.init;
    
endmodule

