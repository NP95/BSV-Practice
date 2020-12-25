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
    
    rule doProc( cop.started );
        
        let inst   = iMem.req( pc );
        let dInst  = decode( inst, cop.isUserMode );
        let rVal1  = rf.rd1( validRegValue( dInst.src1 ) );
        let rVal2  = rf.rd2( validRegValue( dInst.src2 ) );
        let copVal = cop.rd( validRegValue( dInst.src1 ) );
        let eInst  = exec( dInst, rVal1, rVal2, pc, ?, copVal, hi, lo );
        if( eInst.iType == Illegal ) begin
            $fwrite( stderr, "Executing illegal instruction at pc: %x. Exiting\n", pc );
            $finish;
        end
        if( eInst.iType == Ld ) eInst.data <- dMem.req( MemReq{ op: Ld, addr: eInst.addr, data: eInst.data } );
        else if( eInst.iType == St ) let d <- dMem.req( MemReq{ op: St, addr: eInst.addr, data: eInst.data } );
        if( eInst.iType == Unsupported ) begin
            cop.causeException( pc, excRI );
            pc <= excHandlerPC;
        end else if( eInst.iType == Syscall ) begin
            cop.causeException( pc, excSys );
            pc <= excHandlerPC;
        end else if( eInst.iType == ERet ) begin
            cop.returnFromException;
            pc <= cop.getEPC;
        end else begin
            if( isValid( eInst.dst ) && validValue( eInst.dst ).regType == Normal ) begin
                rf.wr( validRegValue( eInst.dst ), eInst.data );
            end
            else if( eInst.iType == Mthi ) hi <= eInst.data;
            else if( eInst.iType == Mtlo ) lo <= eInst.data;
            cop.wr( eInst.dst, eInst.data );
            pc <= eInst.brTaken ? eInst.addr : pc + 4;
        end
        
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

