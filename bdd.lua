-- Copyright (c) 2012 Scott Vokes <vokes.s@gmail.com>
-- 
-- Permission to use, copy, modify, and/or distribute this software for
-- any purpose with or without fee is hereby granted, provided that the
-- above copyright notice and this permission notice appear in all
-- copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
-- WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
-- AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
-- DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
-- PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
-- TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
-- PERFORMANCE OF THIS SOFTWARE.
------------------------------------------------------------------------
-- Lua module for binary decision diagrams.
------------------------------------------------------------------------

-- imports
local assert, error, io, ipairs, string, tostring, type = 
   assert, error, io, ipairs, string, tostring, type

local pcall = pcall

module("bdd")

DEBUG = false

local function printf(...)
   io.write(string.format(...))
end

local function log(...)
   if DEBUG then printf(...) end
end

-- Init the data store.
-- T is an array of nodes mapping the node ID to
--    a { var_id, low node ID, high node ID} tuple.
-- 
-- H is a { var, low, high} -> node ID reverse lookup table for T,
--    structured as H[v] -> ( {l, h, node_id} array ).
--
local function init(vs)
   assert(vs)
   local T = {}
   T[0] = { v=vs + 1, f=0, t=0 }     -- always false
   T[1] = { v=vs + 1, f=1, t=1 }     -- always true
   local H = {}
   return {T=T, H=H, vars=vs}
end

-- Prettyprint a BDD.
function pp(D)
   local T = D.T
   -- printf("0: false\n")
   -- printf("1: true\n")
   for i=0,D.start do
      local row = T[i]
      printf("%d: %d -> [f: %d, t: %d]\n",
             i, row.v, row.f, row.t)
   end
end

-- Prettyprint a BDD to graphviz's "dot" language, to generate nice diagrams.
function dotPP(D)
   local function B() printf("\n") end
   for i=D.start,0,-1 do
      local row = D.T[i]
      printf("// %d: v %-5d f: %-5d t: %-5d\n", i, row.v, row.f, row.t)
   end

   printf("digraph {\n")
   printf("    ordering=out;\n")

   for i=#D.T,2,-1 do
      local row = D.T[i]
      printf([[    n%d [label="n%d, v %d"];]], i, i, row.v); B()
      printf([[    n%d -> n%d [style=dashed];]], i, row.f); B()
      printf([[    n%d -> n%d [style=solid];]], i, row.t); B()
   end

   -- I haven't figured out how to force n0 to be on the left of n1...
   printf([[    n0 [shape=box, label="n0, false"];]]); B()
   printf([[    n1 [shape=box, label="n1, true"];]]); B()

   printf("}\n")
end

-- Look up the {v, l, h} -> ID pair in H, or return nil.
local function lookup(H, v, l, h)
   local vt = H[v]
   if not vt then return nil end
end

-- add a new node n_id to T with {v, l, h}
local function add(T, v, l, h)
   local n_id = #T + 1
   T[n_id] = {v=v, f=l, t=h}
   return n_id
end

-- add {v, l, h} -> n_id mapping to H
local function insert(H, v, l, h, n_id)
   local vs = H[v] or {}
   vs[#vs+1] = {l, h, n_id}
end

local function mk(D, v, l, h)
   log("//   --> mk: var %d, low %d, high %d\n", v, l, h)
   -- both answers point at the same node, skip it
   if l == h then return l end

   -- already known, re-use it
   local var_id = lookup(D.H, v, l, h)
   if var_id then return var_id end

   local n_id = add(D.T, v, l, h)
   insert(D.H, v, l, h, n_id)
   return n_id
end

-- take a true/false/nil array (with known max) and convert it to a BDD.
function build(t, vars)
   local D = init(vars)
   local function b(i)
      log("// %d/%d: %s %s\n", i, vars, tostring(t[i]), tostring(t[i]))
      
      local v0, v1
      if i > vars then
         return 1               -- pass, if it hasn't failed along the way
      else
         if t[i] == nil then
            -- don't-care, set both to same - will be pruned later
            v0 = b(i + 1)
            v1 = v0
         elseif type(t[i]) == "boolean" then
            if t[i] then        -- expect true
               v0 = 0
               v1 = b(i + 1)
            else                -- expect false
               v0 = b(i + 1)
               v1 = 0
            end
         else
            error("bad spec: " .. tostring(t[i]))
         end
         return mk(D, i, v0, v1)
      end
   end

   b(1)
   D.start = #D.T
   return D
end

-- Apply operator OP to two BDDs.
function apply(op, bdd1, bdd2)
   local G = {}                 -- cache
   assert(bdd1, "nil bdd #1")
   assert(bdd2, "nil bdd #2")

   local T1, T2 = bdd1.T, bdd2.T
   local vct_1, vct_2 = bdd1.vars, bdd2.vars
   assert(vct_1 == vct_2)
   local D = init(vct_1)

   if op == nil then return nil, "nil operator function" end

   local max_node1, max_node2 = bdd1.start, bdd2.start
   local anypass = false

   -- Apply to node IDs u1 in bdd1 and u2 in bdd2, keep in sync
   -- with which variables are being tested. Use dynamic programming
   -- to avoid redundant re-computation.
   local function app(D, u1, u2)
      local key = string.format("%d:%d", u1, u2)
      log("key is %s\n", key)

      local cached = G[key]
      if cached then return cached end
      
      local res = nil           -- result node ID
      
      if u1 < 2 and u2 < 2 then -- terminal nodes, apply operator
         if op(u1 == 1, u2 == 1) then
            res = 1
            anypass = true
         else
            res = 0
         end
      else          -- non-terminal nodes, recurse and memoize
         -- appropriate rows for each BDD's current nodes
         local r1, r2 = T1[u1], T2[u2]
         local v1, v2 = r1.v, r2.v

         -- variables for each row
         log("\n// u1 row -- node %d, var %d, f: %d, t: %d\n",
            u1, v1, r1.f, r1.t)
         log("// u2 row -- node %d, var %d, f: %d, t: %d\n\n",
            u1, v2, r2.f, r2.t)

         if v1 == v2 then
            res = mk(D, v1,
                     app(D, r1.f, r2.f),
                     app(D, r1.t, r2.t))
         elseif v1 < v2 then
            res = mk(D, v1,
                     app(D, r1.f, u2),
                     app(D, r1.t, u2))
         else -- v1 > v2
            res = mk(D, v2,
                     app(D, u1, r2.f),
                     app(D, u1, r2.t))
         end
      end
      
      G[key] = res
      return res
   end

   app(D, max_node1, max_node2)
   D.start = anypass and #D.T or 0

   return D
end

function restrict(BDD, var, val)
   -- TODO: dynamic programming.

   local T = BDD.T
   local function res(u)
      -- is the pseudocode not bottoming out correctly?
      -- if u <= 1 then return u end

      local row = T[u]
      local rv = row.v
      -- log("u ", u, "var", var, "val", val, "rv", rv)
      if rv > var then
         return u
      elseif rv < var then
         return mk(BDD, rv, res(row.f), res(row.t))
      elseif rv == var then
         if val == 0 then
            return res(row.f)
         else
            return res(row.t)
         end
      end
   end

   return res(BDD.start)
end

function op_or(a, b)
   return a or b
end

function op_and(a, b)
   return a and b
end

function op_xor(a, b)
   return (a or b) and not (a and b)
end

-- Execute a BDD with a given bit array.
-- For example, check {false, true, true, false} against a 4-variable BDD.
function exec(bdd, bits)
   assert(bdd, "nil BDD provided")
   local n_id = bdd.start
   local T = bdd.T

   for i=1,#bits do
      local row = T[n_id]
      if row.v == i then
         local next = (bits[i] and row.t or row.f)
         log("var %d: node %d -> node %d\n", i, n_id, next)
         
         if next < 2 then
            log("reached end result: %d\n", next)
            return next == 1 and 1 or 0
         end
         n_id = next
      else
         log("skipping var %d\n", i)
      end
   end
   if n_id < 2 then
      return T[n_id].t
   end
   return nil, "error"
end

function satcount(bdd, start)
   -- TODO use dynamic programming
   local T = bdd.T
   local function count(u)
      if u <= 1 then
         return u
      else
         local row = T[u]
         assert(row)
         local lu, hu, vu = row.f, row.t, row.v
         local lc, hc = count(lu), count(hu)
         local lrow, hrow = T[lu], T[hu]
         local low = 2^(lrow.v - vu - 1) * lc
         local high = 2^(hrow.v - vu - 1) * hc
         return low + high
      end
   end

   local start = start or bdd.start
   return 2^(T[start].v - 1) * count(start)
end

-- Get any satisfying path.
-- Basic assumption: if any node's low path does not lead directly to 0,
-- it must eventually lead to 1, so take every low that doesn't,
-- otherwise take every 1.
function anysat(bdd)
   local T, res = bdd.T, {}

   local function as(u, i)
      local row = T[u]
      if row.v > i then
         res[i] = false
         return as(u, i + 1)
      elseif u == 0 then
         error("anysat error, should never get here")
      elseif u == 1 then
         return res
      elseif row.f == 0 then
         res[i] = true
         return as(row.t, i + 1)
      else
         res[i] = false
         return as(row.f, i + 1)
      end
   end

   return as(bdd.start, 1)
end

-- call f(sol) on every solution, breaking early if it returns true.
function itersat(bdd, f)
   local T = bdd.T
   local vars = bdd.vars

   local function cp(t)
      local r = {}
      for i,v in ipairs(t) do
         r[i] = v
      end
      return r
   end 

   local function accum(u, bit, acc)
      local row = T[u]
      local rv = row.v

      if rv > bit then          --skipped node; add both branches
         local ca = cp(acc)
         ca[bit] = false
         accum(u, bit + 1, ca)
         ca[bit] = true
         accum(u, bit + 1, ca)
      elseif rv > vars then     --end, save any successes
         if row.t ~= 1 and row.f ~= 1 then
            return
         end
         -- save successful path
         if f(cp(acc)) then error("done") end
      else                      --follow non-dead ends
         local ca = cp(acc)
         bit = bit
         if row.f ~= 0 then
            ca[bit] = false
            accum(row.f, bit + 1, ca)
         end
         if row.t ~= 0 then
            ca[bit] = true
            accum(row.t, bit + 1, ca)
         end
      end
   end

   local row = T[bdd.start]
   local ok, res = pcall(accum, bdd.start, 1, {})
   if not ok then return else return res end
end

function allsat(bdd)
   local res = {}
   itersat(bdd, function (s)
                   res[#res+1] = s
                end)
   return res
end
