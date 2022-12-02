package SignalProbe;

export mkSignalProbe;

module mkSignalProbe #(Bit #(n) x) (Empty);
  if (genVerilog) vSignalProbe (x);
endmodule

import "BVI" SignalProbe = module vSignalProbe #(Bit #(n) x) (Empty);
  parameter N = valueOf (n);
  port sig = x;
endmodule

endpackage
