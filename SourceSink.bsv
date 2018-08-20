/*-
 * Copyright (c) 2018 Alexandre Joannou
 * All rights reserved.
 *
 * This software was developed by SRI International and the University of
 * Cambridge Computer Laboratory (Department of Computer Science and
 * Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
 * DARPA SSITH research programme.
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

package SourceSink;

import FIFOF :: *;
import SpecialFIFOs :: *;
import GetPut :: *;
import Connectable :: *;

//////////////////////////////
// Source / Sink interfaces //
////////////////////////////////////////////////////////////////////////////////

interface Source#(type t);
   (* always_ready *)
   method Bool canGet();
   method t peek();
   method ActionValue#(t) get();
endinterface

interface Sink#(type t);
   (* always_ready *)
   method Bool canPut();
   method Action put(t val);
endinterface

///////////////////////////////////
// ToSource / ToSink typeclasses //
////////////////////////////////////////////////////////////////////////////////

// ToSource

typeclass ToSource#(type a, type b) dependencies(a determines b);
  function Source#(b) toSource (a val);
endtypeclass

instance ToSource#(Source#(t), t);
  function toSource = id;
endinstance

instance ToSource#(FIFOF#(t), t);
  function toSource (ff) = interface Source#(t);
    method canGet = ff.notEmpty;
    method peek if (ff.notEmpty) = ff.first;
    method get if (ff.notEmpty) = actionvalue
      ff.deq; 
      return ff.first;
    endactionvalue;
  endinterface;
endinstance

// ToSink

typeclass ToSink#(type a, type b) dependencies (a determines b);
    function Sink#(b) toSink (a val);
endtypeclass

instance ToSink#(Sink#(t), t);
    function toSink = id;
endinstance

instance ToSink#(FIFOF#(t), t);
  function toSink (ff) = interface Sink;
    method canPut = ff.notFull;
    method put if (ff.notFull) = ff.enq;
  endinterface;
endinstance

/////////////////////////////
// ToGet / ToPut instances //
////////////////////////////////////////////////////////////////////////////////

// ToGet
instance ToGet#(Source#(t), t);
  function toGet (s) = interface Get;
    method get = s.get;
  endinterface;
endinstance

//ToPut
instance ToPut#(Sink#(t), t);
  function toPut (s) = interface Put;
    method put = s.put;
  endinterface;
endinstance

///////////////////////////
// Connectable instances //
////////////////////////////////////////////////////////////////////////////////

instance Connectable#(Source#(t), Sink#(t));
  module mkConnection#(Source#(t) src, Sink#(t) snk)(Empty);
    mkConnection(toGet(src), toPut(snk));
  endmodule
endinstance

instance Connectable#(Sink#(t), Source#(t));
  module mkConnection#(Sink#(t) snk, Source#(t) src)(Empty);
    mkConnection(toGet(src), toPut(snk));
  endmodule
endinstance

instance Connectable#(Source#(t), Put#(t));
  module mkConnection#(Source#(t) src, Put#(t) put)(Empty);
    mkConnection(toGet(src), put);
  endmodule
endinstance

instance Connectable#(Put#(t), Source#(t));
  module mkConnection#(Put#(t) put, Source#(t) src)(Empty);
    mkConnection(toGet(src), put);
  endmodule
endinstance

instance Connectable#(Sink#(t), Get#(t));
  module mkConnection#(Sink#(t) snk, Get#(t) get)(Empty);
    mkConnection(get, toPut(snk));
  endmodule
endinstance

instance Connectable#(Get#(t), Sink#(t));
  module mkConnection#(Get#(t) get, Sink#(t) snk)(Empty);
    mkConnection(get, toPut(snk));
  endmodule
endinstance

///////////
// Shims //
////////////////////////////////////////////////////////////////////////////////

module mkPutToSink#(Put#(t) put)(Sink#(t)) provisos (Bits#(t, t_sz));
  FIFOF#(t) ff <- mkBypassFIFOF();
  mkConnection(toGet(ff), put);
  return toSink(ff);
endmodule

module mkGetToSource#(Get#(t) get)(Source#(t)) provisos (Bits#(t, t_sz));
  FIFOF#(t) ff <- mkBypassFIFOF();
  mkConnection(get, toPut(ff));
  return toSource(ff);
endmodule

endpackage
