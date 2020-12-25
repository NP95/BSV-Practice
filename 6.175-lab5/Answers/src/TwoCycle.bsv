// TwoCycle.bsv
//
// This is a two cycle implementation of the SMIPS processor.

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

typedef enum {Fetch, Execute} Stage deriving (Bits, Eq, FShow);

(* synthesize *)
module mkProc(Proc);
    
    Reg#(Addr) pc <- mkRegU;
    RFile      rf <- mkRFile;
    DMemory   mem <- mkDMemory;
    Cop       cop <- mkCop;
    
    Bool memReady = mem.init.done();
    
    
    // TODO: Complete the implementation of this processor
    
    Reg#(Stage)       stage <- mkReg(Fetch);
    Reg#(DecodedInst) dInst <- mkRegU;
    
    rule doProc(cop.started);
        
        if(stage == Fetch) begin
            
            Data inst <- mem.req(MemReq{op: Ld, addr: pc, data: ?});
            
            // decode
            dInst <= decode(inst);
            
            // trace - print the instruction
            $display("pc: %h inst: (%h) expanded: ", pc, inst, showInst(inst));
            
            stage <= Execute;
            
        end else begin
            
            // read register values 
            Data rVal1 = rf.rd1(validRegValue(dInst.src1));
            Data rVal2 = rf.rd2(validRegValue(dInst.src2));
            
            // Co-processor read for debugging
            Data copVal = cop.rd(validRegValue(dInst.src1));
            
            // execute
            ExecInst eInst = exec(dInst, rVal1, rVal2, pc, ?, copVal);
            
            // Executing unsupported instruction. Exiting
            if(eInst.iType == Unsupported) begin
                $fwrite(stderr, "Executing unsupported instruction at pc: %x. Exiting\n", pc);
                $finish;
            end
            
            // memory
            if(eInst.iType == Ld) begin
                eInst.data <- mem.req(MemReq{op: Ld, addr: eInst.addr, data: ?});
            end else if(eInst.iType == St) begin
                let d <- mem.req(MemReq{op: St, addr: eInst.addr, data: eInst.data});
            end
            
            // write back
            if(isValid(eInst.dst) && validValue(eInst.dst).regType == Normal) begin
                rf.wr(validRegValue(eInst.dst), eInst.data);
            end

            // update the pc depending on whether the branch is taken or not
            pc <= eInst.brTaken ? eInst.addr : pc + 4;
            
            // Co-processor write for debugging and stats
            cop.wr(eInst.dst, eInst.data);
            
            stage <= Fetch;
            
        end
    
    endrule


    method ActionValue#(Tuple2#(RIndx, Data)) cpuToHost;
        let ret <- cop.cpuToHost;
        return ret;
    endmethod

    method Action hostToCpu(Bit#(32) startpc) if ( !cop.started && memReady );
        cop.start;
        pc <= startpc;
    endmethod

    interface MemInit iMemInit = mem.init;
    interface MemInit dMemInit = mem.init;
endmodule

