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
} TrainRedirect deriving (Bits, Eq);

typedef struct {
    Addr pc;
    Addr ppc;
    Bool dEp;
    Bool rEp;
    Bool eEp;
} Fetch2Decode deriving (Bits, Eq);

typedef struct {
    Addr pc;
    Addr ppc;
    DecodedInst dInst;
    Bool rEp;
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
    
    Ehr#(4, Addr)  pc_reg <- mkEhr(0);
    Btb#(6,8)      btb    <- mkBtb;
    Bht#(8)        bht    <- mkBht;
    RFile          rf     <- mkRFile;
    Scoreboard#(3) sb     <- mkBypassScoreboard;
    FPGAMemory     iMem   <- mkFPGAMemory();
    FPGAMemory     dMem   <- mkFPGAMemory();
    Cop            cop    <- mkCop;
    
    Ehr#(4, Bool) dEp <- mkEhr(False);
    Ehr#(4, Bool) rEp <- mkEhr(False);
    Ehr#(4, Bool) eEp <- mkEhr(False);
    
    Fifo#(2, Fetch2Decode)    decodeFifo    <- mkCFFifo;
    Fifo#(2, Decode2RegRead)  regReadFifo   <- mkCFFifo;
    Fifo#(2, RegRead2Execute) executeFifo   <- mkCFFifo;
    Fifo#(2, Exec2Commit)     memoryFifo    <- mkCFFifo;
    Fifo#(2, Exec2Commit)     writeBackFifo <- mkCFFifo;
    
    Bool memReady = iMem.init.done() && dMem.init.done();
    
    rule doFetch( cop.started && memReady && decodeFifo.notFull );
        
        // Fetch Instruction
    
        
        // Update PC

        
        // Create and push Fetch2Decode
       

    endrule
    
    rule doDecode( cop.started && memReady && decodeFifo.notEmpty && regReadFifo.notFull );
        
        // Retrieve Fetch2Decode & Instruction
      
            
            // Decode Instruction

            
            // Predict Branch
       
            
            // Create and push Decode2RegRead
         
            
            // Handle misprediction
         
        
    endrule
    
    rule doRegRead( cop.started && memReady && regReadFifo.notEmpty && executeFifo.notFull );
        
        // Retrieve Decode2RegRead
 
            
            // Ensure no dependencies
         
                
                // Create and push RegRead2Execute
            
                
                // Update scoreboard

                
                // Handle misprediction
             
                
	    end
            
        end
        
    endrule
    
    rule doExecute( cop.started && memReady && executeFifo.notEmpty && memoryFifo.notFull );
        
        // Retrieve RegRead2Execute

   
            // Execute
          
            
            // Handle misprediction

            // Train BTB & BHT
           
            // Push Exec2Commit
         
        
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
        pc_reg[3] <= startpc;
    endmethod
    
    interface MemInit iMemInit = iMem.init;
    interface MemInit dMemInit = dMem.init;
    
endmodule

