module verilog_verilator_error;
   initial begin
      forever begin
         i = $fopen("test.log");
      end
   end
endmodule
