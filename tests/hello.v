`ifdef behavioral
 `include "groupA_beh.v ";
 `include "groupB_beh.v ";
 `include "ctrl_beh.v ";
`else
 `include "groupA_synth.v ";
 `include "groupB_ synth.v ";
 `include "ctrl_ synth.v ";
`endif
always @(WRITE or READ or STATUS) // test 
 /* hello */  begin
  out = 9;
  end
 // more comment
