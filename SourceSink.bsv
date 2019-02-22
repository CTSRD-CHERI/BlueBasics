/*-
 * Copyright (c) 2018-2019 Alexandre Joannou
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
   (* always_ready *) method Bool canPeek;
   (* always_ready *) method t peek;
   method Action drop;
endinterface

interface Sink#(type t);
   (* always_ready *) method Bool canPut;
   method Action put(t val);
endinterface

//////////////////////
// HasGet typeclass //
////////////////////////////////////////////////////////////////////////////////

typeclass HasGet#(type a, type b);
  function ActionValue#(b) get(a gettable);
endtypeclass

instance HasGet#(a, b) provisos (ToGet#(a, b));
  function get(x) = toGet(x).get;
endinstance

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
    method canPeek = ff.notEmpty;
    method peek    = ff.first;
    method drop    = ff.deq; 
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
    method put    = ff.enq;
  endinterface;
endinstance

/////////////////////////////////////////////////////
// ToUnguardedSource / ToUnguardedSink typeclasses //
////////////////////////////////////////////////////////////////////////////////

// ToUnguardedSource

typeclass ToUnguardedSource#(type a, type b) dependencies(a determines b);
  module toUnguardedSource#(a val, b dflt)(Source#(b));
endtypeclass

instance ToUnguardedSource#(src_t, t)
  provisos (ToSource#(src_t, t), Bits#(t, _));
  module toUnguardedSource#(src_t s, t dflt)(Source#(t));
    let src = toSource(s);
    let canPeekWire <- mkDWire(False);
    let peekWire    <- mkDWire(dflt);
    let dropWire    <- mkDWire(False);
    rule setCanPeek; canPeekWire <= src.canPeek; endrule
    rule setPeek;
      if (!src.canPeek) begin
        $display("WARNING: peeking from Source that can't be peeked");
        //$finish(0);
      end
      peekWire <= src.peek;
    endrule
    rule doDrop (dropWire);
      if (!src.canPeek) begin
        $display("WARNING: dropping from Source that can't be dropped");
        //$finish(0);
      end
      src.drop;
    endrule
    return interface Source;
      method canPeek = canPeekWire;
      method peek = peekWire;
      method drop = action dropWire <= True; endaction;
    endinterface;
  endmodule
endinstance

// ToUnguardedSink

typeclass ToUnguardedSink#(type a, type b) dependencies(a determines b);
  module toUnguardedSink#(a val)(Sink#(b));
endtypeclass

instance ToUnguardedSink#(snk_t, t)
  provisos (ToSink#(snk_t, t), Bits#(t, _));
  module toUnguardedSink#(snk_t s)(Sink#(t));
    let snk = toSink(s);
    let canPutWire <- mkDWire(False);
    let putWire <- mkRWire;
    rule setCanPut; canPutWire <= snk.canPut; endrule
    rule doPut(isValid(putWire.wget));
      if (!snk.canPut) begin
        $display("WARNING: putting in a Sink that can't be put into");
        //$finish(0);
      end
      snk.put(putWire.wget.Valid);
    endrule
    return interface Sink;
      method canPut = canPutWire;
      method put(x) = action putWire.wset(x); endaction;
    endinterface;
  endmodule
endinstance

/////////////////////////////
// ToGet / ToPut instances //
////////////////////////////////////////////////////////////////////////////////

// ToGet
instance ToGet#(Source#(t), t);
  function toGet (s) = interface Get;
    method get if (s.canPeek) = actionvalue
      s.drop;
      return s.peek;
    endactionvalue;
  endinterface;
endinstance
/* XXX this can't be defined...
instance ToGet#(src_t, t) provisos (ToSource#(src_t, t));
  function toGet (s) = toGet(toSource(s));
endinstance
*/
//ToPut
instance ToPut#(Sink#(t), t);
  function toPut (s) = interface Put;
    method put if (s.canPut) = s.put;
  endinterface;
endinstance
/* XXX this yields a warning...
instance ToPut#(snk_t, t) provisos (ToSink#(snk_t, t));
  function toPut (s) = toPut(toSink(s));
endinstance
*/

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

/////////////////////////////
// helpers and other utils //
////////////////////////////////////////////////////////////////////////////////

function Source#(t) nullSource = interface Source;
  method canPeek = False;
  method peek if (False) = ?;
  method drop if (False) = noAction;
endinterface;

function Sink#(t) nullSink = interface Sink;
  method canPut = True;
  method put(x) = noAction;
endinterface;

endpackage
