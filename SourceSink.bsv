/*-
 * Copyright (c) 2018-2021 Alexandre Joannou
 * Copyright (c) 2019 Peter Rugg
 * Copyright (c) 2019 Jonathan Woodruff
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
   method t peek;
   method Action drop;
endinterface

interface Sink#(type t);
   (* always_ready *) method Bool canPut;
   method Action put(t val);
endinterface

interface SourceSink #(type t);
  interface Source #(t) source;
  interface Sink   #(t) sink;
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

/////////////////////////
// mapSource / mapSink //
////////////////////////////////////////////////////////////////////////////////

// XXX Note: It would be nice to have the Functor class defined...

function Source #(b) mapSource ( function b f (a x)
                               , Source #(a) src) = interface Source;
  method canPeek = src.canPeek;
  method peek    = f (src.peek);
  method drop    = src.drop;
endinterface;

function Sink #(b) mapSink ( function a f (b x)
                           , Sink #(a) snk) = interface Sink;
  method canPut  = snk.canPut;
  method put (x) = snk.put (f (x));
endinterface;

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

instance Connectable#(Source#(t), Sink#(t)) provisos (Bits#(t, t_sz));
  module mkConnection#(Source#(t) src, Sink#(t) snk)(Empty);
    let ug_src <- toUnguardedSource(src, ?);
    let ug_snk <- toUnguardedSink(snk);
    rule connect (ug_src.canPeek && ug_snk.canPut);
      ug_snk.put(ug_src.peek);
      ug_src.drop;
    endrule
  endmodule
endinstance

instance Connectable#(Sink#(t), Source#(t)) provisos (Bits#(t, t_sz));
  module mkConnection#(Sink#(t) snk, Source#(t) src)(Empty);
    mkConnection(src, snk);
  endmodule
endinstance

/*
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
*/

////////////////////////////////////
// toUnguardedSource/Sink modules //
////////////////////////////////////////////////////////////////////////////////

(* always_ready = "canPeek, peek, drop" *)
module toUnguardedSource#(src_t s, t dflt)(Source#(t))
  provisos (ToSource#(src_t, t), Bits#(t, _));
  let src = toSource(s);
  let canPeekWire <- mkDWire(False);
  let peekWire <- mkDWire(dflt);
  let dropWire <- mkPulseWire;
  rule setCanPeek; canPeekWire <= src.canPeek; endrule
  rule setPeek; peekWire <= src.peek; endrule
  rule warnDoDrop (dropWire && !canPeekWire);
    $display("WARNING: %m - dropping from Source that can't be dropped from");
    //$finish(0);
  endrule
  rule doDrop (dropWire && canPeekWire);
    //$display("ALLGOOD: dropping from Source");
    src.drop;
  endrule
  return interface Source;
    method canPeek = canPeekWire;
    method peek    = peekWire;
    method drop    = dropWire.send;
  endinterface;
endmodule

(* always_ready = "canPut, put" *)
module toUnguardedSink#(snk_t s)(Sink#(t))
  provisos (ToSink#(snk_t, t), Bits#(t, _));
  let snk = toSink(s);
  let putWire <- mkRWire;
  rule warnDoPut (isValid(putWire.wget) && !snk.canPut);
    $display("WARNING: %m - putting into a Sink that can't be put into");
    //$finish(0);
  endrule
  rule doPut (isValid(putWire.wget));
    //$display("ALLGOOD: putting in a Sink");
    snk.put(putWire.wget.Valid);
  endrule
  return interface Sink;
    method canPut = snk.canPut;
    method put    = putWire.wset;
  endinterface;
endmodule

////////////////////////////////////
// toGuardedSource/Sink functions //
////////////////////////////////////////////////////////////////////////////////

function Source#(t) toGuardedSource(src_t s) provisos (ToSource#(src_t, t));
  let src = toSource(s);
  return guardSource(src, !src.canPeek);
endfunction

function Sink#(t) toGuardedSink(snk_t s) provisos (ToSink#(snk_t, t));
  let snk = toSink(s);
  return guardSink(snk, !snk.canPut);
endfunction

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

////////////////////////////////////////
// augment source/sink with an action //
////////////////////////////////////////////////////////////////////////////////

function Source#(t) onDrop(src_t s, function Action f (t x))
  provisos (ToSource#(src_t, t));
  let src = toSource(s);
  return interface Source;
    method canPeek = src.canPeek;
    method peek = src.peek;
    method drop = action src.drop; f(src.peek); endaction;
  endinterface;
endfunction

// Note: if a simple action with no argument is desired, consider using a
//       partially applied call to constFn as the second argument to onPut
function Sink#(t) onPut(snk_t s, function Action f (t x))
  provisos (ToSink#(snk_t, t));
  let snk = toSink(s);
  return interface Sink;
    method canPut = snk.canPut;
    method put(x) = action snk.put(x); f(x); endaction;
  endinterface;
endfunction

/////////////////////////////
// helpers and other utils //
////////////////////////////////////////////////////////////////////////////////

// null sources / sinks
function Source#(t) nullSource = interface Source;
  method canPeek = False;
  method peek if (False) = ?;
  method drop if (False) = noAction;
endinterface;

function Sink#(t) nullSink = interface Sink;
  method canPut = True;
  method put(x) = noAction;
endinterface;

// debug wrapping
function Source#(t) debugSource(Source#(t) src, Fmt msg) provisos (FShow#(t)) =
  onDrop(src, constFn($display( msg, " - Source drop method called - canPeek: "
                              , fshow(src.canPeek), " - ", fshow(src.peek))));

function Sink#(t) debugSink(Sink#(t) snk, Fmt msg) provisos (FShow#(t));
  function f (x) = action
    $display( msg, " - Sink put method called - canPut: ", fshow(snk.canPut)
            , " - ", fshow(x));
  endaction;
  return onPut(snk, f);
endfunction

// add a Boolean guard to a Source
function Source#(t) guardSource (Source#(t) raw, Bool block) = interface Source;
  method canPeek = raw.canPeek && !block;
  method peek if (!block) = raw.peek;
  method drop if (!block) = raw.drop;
endinterface;

function Sink#(t) guardSink (Sink#(t) raw, Bool block) = interface Sink;
  method canPut = raw.canPut && !block;
  method put if (!block) = raw.put;
endinterface;

endpackage
