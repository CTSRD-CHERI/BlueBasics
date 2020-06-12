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

package MasterSlave;

import SourceSink :: *;
import Connectable :: *;

///////////////////////////////
// Master / Slave interfaces //
////////////////////////////////////////////////////////////////////////////////

interface Master#(type req, type resp);
  interface Source#(req) source;
  interface Sink#(resp) sink;
endinterface

interface Slave#(type req, type resp);
  interface Source#(resp) source;
  interface Sink#(req) sink;
endinterface

///////////////////////////
// Connectable instances //
////////////////////////////////////////////////////////////////////////////////

instance Connectable#(Master#(req, resp), Slave#(req, resp));
  module mkConnection#(Master#(req, resp) m, Slave#(req, resp) s)(Empty);
    mkConnection(m.source, s.sink);
    mkConnection(s.source, m.sink);
  endmodule
endinstance

instance Connectable#(Slave#(req, resp), Master#(req, resp));
  module mkConnection#(Slave#(req, resp) s, Master#(req, resp) m)(Empty);
    mkConnection(m.source, s.sink);
    mkConnection(s.source, m.sink);
  endmodule
endinstance

///////////
// Debug //
////////////////////////////////////////////////////////////////////////////////

function Master#(req, resp) debugMaster(Master#(req, resp) m, Fmt msg)
  provisos (FShow#(req), FShow#(resp)) =
  interface Master;
    interface source = debugSource(m.source, msg);
    interface sink   = debugSink(m.sink, msg);
  endinterface;

function Slave#(req, resp) debugSlave(Slave#(req, resp) s, Fmt msg)
  provisos (FShow#(req), FShow#(resp)) =
  interface Slave;
    interface source = debugSource(s.source, msg);
    interface sink   = debugSink(s.sink, msg);
  endinterface;

///////////
// Utils //
////////////////////////////////////////////////////////////////////////////////
// Get desired Master / Slave sub-interface
function Source#(a) getMasterSource (Master#(a, b) m) = m.source;
function Sink#(b)   getMasterSink   (Master#(a, b) m) = m.sink;
function Source#(b) getSlaveSource  (Slave#(a, b) s)  = s.source;
function Sink#(a)   getSlaveSink    (Slave#(a, b) s)  = s.sink;

endpackage
