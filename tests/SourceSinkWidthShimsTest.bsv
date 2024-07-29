/*-
 * Copyright (c) 2024 Alexandre Joannou
 * All rights reserved.
 *
 * This material is based upon work supported by the DoD Information Analysis
 * Center Program Management Office (DoD IAC PMO), sponsored by the Defense
 * Technical Information Center (DTIC) under Contract No. FA807518D0004.  Any
 * opinions, findings and conclusions or recommendations expressed in this
 * material are those of the author(s) and do not necessarily reflect the views
 * of the Air Force Installation Contracting Agency (AFICA).
 *
 * @BERI_LICENSE_HEADER_START@
 *
 * Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
 * license agreements.  See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.  BERI licenses this
 * file to you under the BERI Hardware-Software License, Version 1.0 (the
 * "License"); you may not use this file except in compliance with the
 * License.  You may obtain a copy of the License at:
 *
 *   http://www.beri-open-systems.org/legal/license-1-0.txt
 *
 * Unless required by applicable law or agreed to in writing, Work distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations under the License.
 *
 * @BERI_LICENSE_HEADER_END@
 */

package SourceSinkWidthShimsTest;

import FIFOF :: *;
import Connectable :: *;

import BlueBasics :: *;

module top(Empty);

  FIFOF#(Bit#(128)) ff_in <- mkFIFOF;
  FIFOF#(Bit#(32)) ff_mid <- mkFIFOF;
  FIFOF#(Bit#(128)) ff_out <- mkFIFOF;

  FIFOF#(Maybe#(Bit#(128))) ff_stimulus <- mkFIFOF;
  FIFOF#(Maybe#(Bit#(128))) ff_side <- mkFIFOF;

  Source#(Bit#(32)) narrowSrcIn <- toNarrowBitSource(ff_in);
  Sink#(Bit#(32)) narrowSnkOut <- toNarrowBitSink(ff_out);

  let cnt <- mkReg(0);
  rule cnt_up if (cnt < 128);
    if (cnt < 127) begin
      Bit #(128) val = 128'hdeadbeef << cnt;
      ff_stimulus.enq(Valid(val));
      $display("%0t - cnt: %0d, stimulus: ", $time, cnt, fshow(val));
    end else begin
      ff_stimulus.enq(Invalid);
    end
    cnt <= cnt + 1;
  endrule

  rule send;
    let val <- get(ff_stimulus);
    ff_in.enq(val.Valid);
    ff_side.enq(val);
    $display("%0t - sent: ", $time, fshow(val.Valid));
  endrule

  mkConnection(narrowSrcIn, toSink(ff_mid));
  mkConnection(toSource(ff_mid), narrowSnkOut);

  rule receive;
    let mval <- get(ff_side);
    let val <- get(ff_out);
    $display("%0t - received: ", $time, fshow(val));
    if (isValid(mval)) begin
      $display( "%0t - expected ", $time, fshow(mval.Valid)
              , ", received ", fshow(val));
      if(mval.Valid == val) $display("%0t - OK", $time);
      else begin
        $display("%0t - KO", $time);
        $display("Failure");
        $finish(1);
      end
    end
    else begin
      $display("Success");
      $finish();
    end
  endrule

endmodule

endpackage
