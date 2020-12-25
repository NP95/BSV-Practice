// Reference functions that use Bluespec's '*' operator
function Bit#(TAdd#(n,n)) multiply_unsigned( Bit#(n) a, Bit#(n) b );
    UInt#(n) a_uint = unpack(a);
    UInt#(n) b_uint = unpack(b);
    UInt#(TAdd#(n,n)) product_uint = zeroExtend(a_uint) * zeroExtend(b_uint);
    return pack( product_uint );
endfunction

function Bit#(TAdd#(n,n)) multiply_signed( Bit#(n) a, Bit#(n) b );
    Int#(n) a_int = unpack(a);
    Int#(n) b_int = unpack(b);
    Int#(TAdd#(n,n)) product_int = signExtend(a_int) * signExtend(b_int);
    return pack( product_int );
endfunction

function Bit#(2) fa(Bit#(1) a, Bit#(1) b, Bit#(1) c_in);
    let t = a ^ b;
    let s = t ^ c_in; 
    let c_out = (a & b) | (c_in & t); 
    return {c_out,s}; 
endfunction

function Bit#(TAdd#(n,1)) addN(Bit#(n) x, Bit#(n) y, Bit#(1) c0); 
    Bit#(n) s; Bit#(TAdd#(n,1)) c=0; c[0]=c0;
    for(Integer i=0; i<valueOf(n); i=i+1) begin
        let cs=fa(x[i],y[i],c[i]);
        c[i+1]=cs[1];
        s[i]=cs[0];
    end
    return {c[valueOf(n)],s}; 
endfunction


// Multiplication by repeated addition
function Bit#(TAdd#(n,n)) multiply_by_adding( Bit#(n) a, Bit#(n) b );
    // TODO: Implement this function in Exercise 2
   
endfunction


function Bit#(n) sra(Bit#(n) a, Integer n);
    Int#(n) a_int = unpack(a);
    return pack(a_int>>n);
endfunction

function Bit#(n) sla(Bit#(n) a, Integer n);
    Int#(n) a_int = unpack(a);
    return pack(a_int<<n);
endfunction


// Multiplier Interface
interface Multiplier#( numeric type n );
    method Bool start_ready();
    method Action start( Bit#(n) a, Bit#(n) b );
    method Bool result_ready();
    method ActionValue#(Bit#(TAdd#(n,n))) result();
endinterface



// Folded multiplier by repeated addition
module mkFoldedMultiplier( Multiplier#(n) );

    // You can use these registers or create your own if you want
    Reg#(Bit#(n)) a <- mkRegU();
    Reg#(Bit#(n)) b <- mkRegU();
    Reg#(Bit#(n)) p <- mkRegU();
    Reg#(Bit#(n)) tp <- mkRegU();
    Reg#(Bit#(TAdd#(TLog#(n),1))) i <- mkReg( fromInteger(valueOf(n)+1) );

    rule mulStep(i<fromInteger(valueOf(n)));
        // TODO: Implement this in Exercise 4
        
    endrule

    method Bool start_ready();
        // TODO: Implement this in Exercise 4

    endmethod

    method Action start( Bit#(n) aIn, Bit#(n) bIn );
        // TODO: Implement this in Exercise 4
        
    endmethod

    method Bool result_ready();
        // TODO: Implement this in Exercise 4

    endmethod

    method ActionValue#(Bit#(TAdd#(n,n))) result();
        // TODO: Implement this in Exercise 4
       
        end
    endmethod

endmodule



// Booth Multiplier
module mkBoothMultiplier( Multiplier#(n) );

    Reg#(Bit#(TAdd#(TAdd#(n,n),1))) m_neg <- mkRegU;
    Reg#(Bit#(TAdd#(TAdd#(n,n),1))) m_pos <- mkRegU;
    Reg#(Bit#(TAdd#(TAdd#(n,n),1))) p <- mkRegU;
    Reg#(Bit#(TAdd#(TLog#(n),1))) i <- mkReg( fromInteger(valueOf(n)+1) );

    rule mul_step(i<fromInteger(valueOf(n)));
        // TODO: Implement this in Exercise 6
        Bit#(TAdd#(TAdd#(n,n),1)) _p=p;
        
    endrule

    method Bool start_ready();
        // TODO: Implement this in Exercise 6

    endmethod

    method Action start( Bit#(n) m, Bit#(n) r );
        // TODO: Implement this in Exercise 6

    endmethod

    method Bool result_ready();
        // TODO: Implement this in Exercise 6

    endmethod

    method ActionValue#(Bit#(TAdd#(n,n))) result();
        // TODO: Implement this in Exercise 6

    endmethod

endmodule



// Radix-4 Booth Multiplier
module mkBoothMultiplierRadix4( Multiplier#(n) );

    Reg#(Bit#(TAdd#(TAdd#(n,n),2))) m_neg <- mkRegU;
    Reg#(Bit#(TAdd#(TAdd#(n,n),2))) m_pos <- mkRegU;
    Reg#(Bit#(TAdd#(TAdd#(n,n),2))) p <- mkRegU;
    Reg#(Bit#(TAdd#(TLog#(n),1))) i <- mkReg( fromInteger(valueOf(n)/2+1) );

    rule mul_step(i<fromInteger(valueOf(n)/2));
        // TODO: Implement this in Exercise 8
        Bit#(TAdd#(TAdd#(n,n),2)) _p=p;
        
    endrule

    method Bool start_ready();
        // TODO: Implement this in Exercise 8

    endmethod

    method Action start( Bit#(n) m, Bit#(n) r );
        // TODO: Implement this in Exercise 8

    endmethod

    method Bool result_ready();
        // TODO: Implement this in Exercise 6

    endmethod

    method ActionValue#(Bit#(TAdd#(n,n))) result();
        // TODO: Implement this in Exercise 6

    endmethod

endmodule

