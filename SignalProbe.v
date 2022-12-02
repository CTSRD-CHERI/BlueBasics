module SignalProbe (sig);
  parameter N = 1;
  input sig;
  (* preserve_for_debug *) wire [N-1:0] sig;
endmodule
