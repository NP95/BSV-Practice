import Types::*;
import ProcTypes::*;
import MemTypes::*;
import Btb::*;
import Bht::*;
import RFile::*;
import Scoreboard::*;
import FPGAMemory::*;
import Decode::*;
import Exec::*;
import Cop::*;
import Vector::*;
import Fifo::*;
import Ehr::*;

typedef struct {
    Addr pc;
    Addr nextPc;
    IType iType;
    Bool taken;
    Bool mispredict;
} ExecuteRedirect deriving (Bits, Eq);

typedef struct {
    Addr nextPc;
    Bool eEp;
} DecodeRedirect deriving (Bits, Eq);

typedef struct {
    Addr pc;
    Addr ppc;
    Bool dEp;
    Bool eEp;
} Fetch2Decode deriving (Bits, Eq);

typedef struct {
    Addr pc;
    Addr ppc;
    DecodedInst dInst;
    Bool eEp;
} Decode2RegRead deriving (Bits, Eq);

typedef struct {
    Addr pc;
    Addr ppc;
    DecodedInst dInst;
    Data rVal1;
    Data rVal2;
    Data copVal;
    Bool eEp;
} RegRead2Execute deriving (Bits, Eq);

typedef struct {
    IType            iType;
    Maybe#(FullIndx) dst;
    Data             data;
    Addr             addr;
} Exec2Commit deriving(Bits, Eq);

(* synthesize *)
module mkProc(Proc);
    
    Reg#(Addr)     pc_reg <- mkRegU;
    Btb#(6,8)      btb    <- mkBtb;
    Bht#(8)        bht    <- mkBht;
    RFile          rf     <- mkRFile;
    Scoreboard#(3) sb     <- mkBypassScoreboard;
    FPGAMemory     iMem   <- mkFPGAMemory();
    FPGAMemory     dMem   <- mkFPGAMemory();
    Cop            cop    <- mkCop;
    
    Reg#(Bool) feEp <- mkReg(False);
    Reg#(Bool) fdEp <- mkReg(False);
    Reg#(Bool) dEp  <- mkReg(False);
    Reg#(Bool) deEp <- mkReg(False);
    Reg#(Bool) eEp  <- mkReg(False);
    
    Fifo#(2, DecodeRedirect)  dRedirectFifo <- mkCFFifo;
    Fifo#(2, ExecuteRedirect) eRedirectFifo <- mkCFFifo;
    
    Fifo#(2, Fetch2Decode)    decodeFifo    <- mkCFFifo;
    Fifo#(2, Decode2RegRead)  regReadFifo   <- mkCFFifo;
    Fifo#(2, RegRead2Execute) executeFifo   <- mkCFFifo;
    Fifo#(2, Exec2Commit)     memoryFifo    <- mkCFFifo;
    Fifo#(2, Exec2Commit)     writeBackFifo <- mkCFFifo;
    
    Bool memReady = iMem.init.done() && dMem.init.done();
    
    rule doFetch( cop.started && memReady && decodeFifo.notFull );
        if( eRedirectFifo.notEmpty ) begin
            
            // Retrieve redirect information

            
            // Train BTB & BHT

            
            // Correct misprediction

            
            // Retrieve redirect information

            
            // Correct misprediction
         
            
            // Fetch Instruction
       
            
            // Update PC

            
            // Create and push Fetch2Decode
          
        
        // Retrieve Fetch2Decode & Instruction
        
        
        // Decode Instruction

        
        // Predict Branch
     
            // Create and push Decode2RegRead
        
            // Handle misprediction
         
            
            // Create and push Decode2RegRead
     

            // Handle misprediction
          
            end
            
        end
        
    endrule
    
    rule doRegRead( cop.started && memReady && regReadFifo.notEmpty && executeFifo.notFull );
        
        // Retrieve Decode2RegRead

        
        // Ensure no dependencies
   
            
            // Create and push RegRead2Execute
          
            
            // Update scoreboard
          
        
        // Retrieve RegRead2Execute
   
            // Execute

            
            // Handle misprediction

            
            // Handle branch

            // Push Exec2Commit
   
            
        end
        
    endrule
    
    rule doMemory( cop.started && memReady && memoryFifo.notEmpty && writeBackFifo.notFull );
        
        // Retrieve Exec2Commit

        
        // Memory
    
        
        // Push Exec2Commit

    
    endrule
    
    rule doWriteBack( cop.started && memReady && writeBackFifo.notEmpty );
        
        // Retrieve Exec2Commit

        
        // Update c.data with memory data if applicable

            
        // Write Back
  
    endrule
    
    method ActionValue#(Tuple2#(RIndx, Data)) cpuToHost;
        let ret <- cop.cpuToHost;
        return ret;
    endmethod
    
    method Action hostToCpu(Bit#(32) startpc) if ( !cop.started && memReady );
        cop.start;
        pc_reg <= startpc;
    endmethod
    
    interface MemInit iMemInit = iMem.init;
    interface MemInit dMemInit = dMem.init;
    
endmodule

