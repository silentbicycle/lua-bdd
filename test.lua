require "lunatest"
require "bdd"

-- Test constant trees
local BDD_F = bdd.build({false}, 1)
local BDD_T = bdd.build({true}, 1)

function test_false()
   assert_equal(1, bdd.exec(BDD_F, {false} ))
end

function test_false_fail()
   assert_equal(0, bdd.exec(BDD_F, {true} ))
end

function test_true()
   assert_equal(1, bdd.exec(BDD_T, {true} ))
end

function test_true_fail()
   assert_equal(0, bdd.exec(BDD_T, {false} ))
end

-- apply op to two BDDs, or just return the first if the second is nil.
-- (common pattern, folding e.g. op_or over a list of BDDs)
local function apply(op, new_bdd, acc)
   if acc then
      return bdd.apply(op, new_bdd, acc)
   else
      return new_bdd
   end
end

local function apply_or(n, a) return apply(bdd.op_or, n, a) end
local function apply_and(n, a) return apply(bdd.op_and, n, a) end

-- turn a number into a boolean array
function num2bin(n, bits)
   bits = bits or 8
   local res = {}
   local i = 2^(bits - 1)
   while i >= 1 do
      if n >= i then
         n = n - i
         res[#res+1] = true
      else
         res[#res+1] = false
      end
      i = i / 2
   end
   return res
end

function do_int_check(number, bits)
   local tf = num2bin(number, bits)
   local D = bdd.build(tf, bits)

   assert_equal(1, bdd.exec(D, num2bin(number, bits) ))
   for i=0,(2^bits - 1) do
      if i ~= number then
         local ba = num2bin(i, bits)
         assert_equal(0, bdd.exec(D, ba))
      end
   end
end

function test_5()
   do_int_check(5, 4)
end

function test_int_range()
   for i=0,15 do
      do_int_check(i, 4)
   end
end

function test_1_or_0()
   local bits = 1
   local D0 = bdd.build(num2bin(0, bits), bits)
   local D1 = bdd.build(num2bin(1, bits), bits)

   local n0or1 = assert(bdd.apply(bdd.op_or, D0, D1))

   assert_equal(1, bdd.exec(n0or1, num2bin(0, bits)), "for 0")
   assert_equal(1, bdd.exec(n0or1, num2bin(1, bits)), "for 1")
end

function test_3_or_5()
   local bits = 4
   local D3 = bdd.build(num2bin(3, bits), bits)
   local D5 = bdd.build(num2bin(5, bits), bits)

   local n3or5 = assert(bdd.apply(bdd.op_or, D3, D5))

   for i=0,(2^bits - 1) do
      local exp = (i == 3 or i == 5) and 1 or 0
      assert_equal(exp, bdd.exec(n3or5, num2bin(i, bits)), "for " .. tostring(i))
   end
end

function test_2_or_3()
   local bits = 4
   local D2 = bdd.build(num2bin(2, bits), bits)
   local D3 = bdd.build(num2bin(3, bits), bits)

   local n2or3 = assert(bdd.apply(bdd.op_or, D2, D3))

   for i=0,(2^bits)-1 do
      local exp = (i == 2 or i == 3) and 1 or 0
      assert_equal(exp, bdd.exec(n2or3, num2bin(i, bits)),
                   "for " .. tostring(i))
   end
end

function test_idempotent()
   local bits = 4
   local D3 = bdd.build(num2bin(3, bits), bits)

   local n3or3 = assert(bdd.apply(bdd.op_or, D3, D3))

   for i=0,(2^bits - 1) do
      local exp = (i == 3 and 1 or 0)
      assert_equal(exp, bdd.exec(n3or3, num2bin(i, bits)), "for " .. tostring(i))
   end
end

function check_all_n_bit_ints(bits)
   local prev = nil
   for i=0,(2^bits - 1) do
      local cur = bdd.build(num2bin(i, bits), bits)
      if prev then
         cur = assert(bdd.apply(bdd.op_or, prev, cur))
      end
      prev = cur
   end

   for i=0,(2^bits - 1) do
      local exp = 1
      assert_equal(exp, bdd.exec(prev, num2bin(i, bits)), "for " .. tostring(i))
   end
end

function test_all_4_bits()
   check_all_n_bit_ints(4)
end

function test_all_odd_bits()
   local bits = 6
   local prev = nil
   for i=0,(2^bits - 1), 2 do
      local cur = bdd.build(num2bin(i, bits), bits)
      if prev then
         cur = assert(bdd.apply(bdd.op_or, prev, cur))
      end
      prev = cur
   end

   for i=0,(2^bits - 1) do
      local exp = (i + 1) % 2
      assert_equal(exp,
                   assert(bdd.exec(prev, num2bin(i, bits))),
                   "for " .. tostring(i))
   end
end

-- add a bunch of arbitrary, non-adjacent <= 16-bit ints, verify that they
-- are all matched and that adjacent numbers are not.
function test_several_ints()
   local bits = 16
   local BDD = bdd.build(num2bin(11, bits), bits)
   
   local nums = {16, 9, 41, 516, 87, 29, 4, 881, 1097, 32761, 3333}

   for _, n in ipairs(nums) do
      local new = bdd.build(num2bin(n, bits), bits)
      BDD = assert(bdd.apply(bdd.op_or, BDD, new))
   end

   for _, n in ipairs(nums) do
      assert_equal(0, bdd.exec(BDD, num2bin(n - 1, bits)), "for " .. tostring(n + 1))
      assert_equal(1, bdd.exec(BDD, num2bin(n, bits)), "for " .. tostring(n))
      assert_equal(0, bdd.exec(BDD, num2bin(n + 1, bits)), "for " .. tostring(n + 1))
   end

   --print("\n", bits, "bits -> ", #BDD.T, "nodes\n")
   --bdd.dotPP(BDD)
end

function test_3_or_5_and_5()
   local bits = 4
   local D3 = bdd.build(num2bin(3, bits), bits)
   local D5 = bdd.build(num2bin(5, bits), bits)

   local n3or5 = assert(bdd.apply(bdd.op_or, D3, D5))
   local n3or5and5 = assert(bdd.apply(bdd.op_and, n3or5, D5))

   for i=0,(2^bits - 1) do
      local exp = 0
      if i == 5 then exp = 1 end
      assert_equal(exp, bdd.exec(n3or5and5, num2bin(i, bits)), "for " .. tostring(i))
   end
end

function test_restrict_3_1()
   local bits = 4
   local D3 = bdd.build(num2bin(3, bits), bits)
   D3.start = bdd.restrict(D3, 1, 0)
   assert_equal(1, bdd.exec(D3, {"skip", false, true, true}))
end

function test_restrict_3_1_fail()
   local bits = 4
   local D3 = bdd.build(num2bin(3, bits), bits)
   D3.start = bdd.restrict(D3, 1, 1)
   assert_equal(0, bdd.exec(D3, {"skip", false, true, true}))
end

function test_restrict_3_2()
   local bits = 4
   local D3 = bdd.build(num2bin(3, bits), bits)
   D3.start = bdd.restrict(D3, 2, 0)
   assert_equal(1, bdd.exec(D3, {false, "skip", true, true}))
end

function test_restrict_3_2_fail()
   local bits = 4
   local D3 = bdd.build(num2bin(3, bits), bits)
   D3.start = bdd.restrict(D3, 2, 1)
   assert_equal(0, bdd.exec(D3, {false, "skip", true, true}))
end

function test_restrict_3_4()
   local bits = 4
   local D3 = bdd.build(num2bin(3, bits), bits)
   D3.start = bdd.restrict(D3, 4, 1)
   assert_equal(1, bdd.exec(D3, {false, false, true, "skip"}))
end

function test_restrict_3_4_fail()
   local bits = 4
   local D3 = bdd.build(num2bin(3, bits), bits)
   D3.start = bdd.restrict(D3, 4, 0)
   assert_equal(0, bdd.exec(D3, {false, false, true, "skip"}))
end

function test_satcount_F()
   assert_equal(1, bdd.satcount(BDD_T));
end

function test_satcount_3()
   local bits = 4
   local D3 = bdd.build(num2bin(3, bits), bits)
   assert_equal(1, bdd.satcount(D3))
end

function test_satcount_3_or_5()
   local bits = 4
   local D3 = bdd.build(num2bin(3, bits), bits)
   local D5 = bdd.build(num2bin(5, bits), bits)

   local n3or5 = assert(bdd.apply(bdd.op_or, D3, D5))
   assert_equal(2, bdd.satcount(n3or5))
end

function test_satcount_3_or_5_or_11()
   local bits = 4
   local D3 = bdd.build(num2bin(3, bits), bits)
   local D5 = bdd.build(num2bin(5, bits), bits)
   local D11 = bdd.build(num2bin(11, bits), bits)

   local n3or5 = assert(bdd.apply(bdd.op_or, D3, D5))
   local n3or5or11 = assert(bdd.apply(bdd.op_or, n3or5, D11))
   assert_equal(3, bdd.satcount(n3or5or11))
end

function test_satcount_all_n_bit_ints()
   local bits = 7
   local prev = nil
   for i=0,(2^bits - 1) do
      local cur = bdd.build(num2bin(i, bits), bits)
      if prev then
         cur = assert(bdd.apply(bdd.op_or, prev, cur))
      end
      prev = cur
   end

   assert_equal(2^bits, bdd.satcount(prev))
end

function test_anysat_3()
   local bits = 4
   local D3 = bdd.build(num2bin(3, bits), bits)
   local res = bdd.anysat(D3)
   assert_equal(false, res[1])
   assert_equal(false, res[2])
   assert_equal(true, res[3])
   assert_equal(true, res[4])
end

function test_anysat_sat()
   local bits = 6

   for i=0,(2^bits - 1) do
      local cur = bdd.build(num2bin(i, bits), bits)
      local res = bdd.anysat(cur)
      assert_equal(1, bdd.exec(cur, num2bin(i, bits)))
   end
end

local function cmp_bits(a, b)
   if #a ~= #b then return false end
   for i=1,#a do
      if a[i] ~= b[i] then return false end
   end
   return true
end

function test_allsat_3_or_5_or_11()
   local bits = 4
   local D3 = bdd.build(num2bin(3, bits), bits)
   local D5 = bdd.build(num2bin(5, bits), bits)
   local D11 = bdd.build(num2bin(11, bits), bits)

   local n3or5 = assert(bdd.apply(bdd.op_or, D3, D5))
   local n3or5or11 = assert(bdd.apply(bdd.op_or, n3or5, D11))

   -- bdd.pp(n3or5or11)
   local res = bdd.allsat(n3or5or11)
   assert_equal(3, #res)
   assert_true(cmp_bits(res[1], num2bin(3, bits)));
   assert_true(cmp_bits(res[2], num2bin(5, bits)));
   assert_true(cmp_bits(res[3], num2bin(11, bits)));
end

function check_allsat_either_bit(bit, bits)
   local D0 = bdd.build(num2bin(0, bits), bits)
   local D1 = bdd.build(num2bin(2^(bit-1), bits), bits)

   local D = assert(bdd.apply(bdd.op_or, D0, D1))
   -- print("\n0 and ", (2^(bit-1)))
   -- bdd.pp(D)

   local res = bdd.allsat(D)
   assert_equal(2, #res, "count")
   assert_equal(#res[1], bits, "result bits");
   assert_equal(#res[2], bits, "result bits");
   assert_true(cmp_bits(res[1], num2bin(0, bits)), "first");
   assert_true(cmp_bits(res[2], num2bin(2^(bit-1), bits)), "second");
end

-- Ensure that allsat works properly, even in the presence of
-- nodes that have been skipped. (bit 1 of 4)
function test_allsat_0_or_1()
   check_allsat_either_bit(1, 4)
end

function test_allsat_0_or_2()
   check_allsat_either_bit(2, 4)
end

function test_allsat_0_or_4()
   check_allsat_either_bit(3, 4)
end

function test_allsat_0_or_8()
   check_allsat_either_bit(4, 4)
end

-- Ensure that allsat works properly, even in the presence of
-- nodes that have been skipped. (bit 4 of 4)
function test_allsat_2_or_3()
   local bits = 4
   local D2 = bdd.build(num2bin(2, bits), bits)
   local D3 = bdd.build(num2bin(3, bits), bits)

   local n2or3 = assert(bdd.apply(bdd.op_or, D2, D3))
   -- print""
   -- bdd.pp(n2or3)

   local res = bdd.allsat(n2or3)
   assert_equal(2, #res)
   assert_true(cmp_bits(res[1], num2bin(2, bits)));
   assert_true(cmp_bits(res[2], num2bin(3, bits)));
end

function test_allsat_4_bits()
   local bits = 4
   local prev = nil
   for i=0,(2^bits - 1) do
      local cur = bdd.build(num2bin(i, bits), bits)
      if prev then
         cur = assert(bdd.apply(bdd.op_or, prev, cur))
      end
      prev = cur
   end
   local res = bdd.allsat(prev)
   assert_equal(2^bits, #res)
end

function test_dontcare_x1x1()
   local bits = 4
   -- mask for (x & 0b0101)
   local D = bdd.build({nil, true, nil, true}, bits)

   for i=0,(2^bits - 1) do
      local exp = (i == 5 or i == 7 or i == 13 or i == 15) and 1 or 0
      assert_equal(exp, bdd.exec(D, num2bin(i, bits)), "for " .. tostring(i))
   end
end

-- check with the first and last bits ignored, and both true and false
function test_dontcare_x10x()
   local bits = 4
   -- mask for (x & 0bx10x)
   local D = bdd.build({nil, true, false, nil}, bits)

   for i=0,(2^bits - 1) do
      local exp = (i == 4 or i == 5 or i == 12 or i == 13) and 1 or 0
      assert_equal(exp, bdd.exec(D, num2bin(i, bits)), "for " .. tostring(i))
   end
end

function test_dontcare_combine_and()
   local bits = 4
   -- mask x10x
   local D1 = bdd.build({nil, true, false, nil}, bits)
   -- mask 1xxx
   local D2 = bdd.build({true, nil, nil, nil}, bits)
   
   local D = bdd.apply(bdd.op_and, D1, D2)

   -- only 110x should pass
   for i=0,(2^bits - 1) do
      local exp = (i == 12 or i == 13) and 1 or 0
      assert_equal(exp, bdd.exec(D, num2bin(i, bits)), "for " .. tostring(i))
   end
end

function test_dontcare_combine_or()
   local bits = 4
   -- mask x10x
   local D1 = bdd.build({nil, true, false, nil}, bits)
   -- mask 1xxx
   local D2 = bdd.build({true, nil, nil, nil}, bits)
   
   local D = bdd.apply(bdd.op_or, D1, D2)

   -- 4, 5, x >= 8 should pass
   for i=0,(2^bits - 1) do
      local exp = (i == 4 or i == 5 or i >= 8) and 1 or 0
      assert_equal(exp, bdd.exec(D, num2bin(i, bits)), "for " .. tostring(i))
   end
end

function test_dontcare_combine_or_any()
   local bits = 4
   local D1 = bdd.build({false, nil, nil, nil}, bits)
   local D2 = bdd.build({true, nil, nil, nil}, bits)
   
   local D = bdd.apply(bdd.op_or, D1, D2)

   -- all should pass
   for i=0,(2^bits - 1) do
      assert_equal(1, bdd.exec(D, num2bin(i, bits)), "for " .. tostring(i))
   end
end

function test_and_mutually_exclusive()
   local bits = 2
   local D1 = bdd.build({false, false}, bits)
   local D2 = bdd.build({false, true}, bits)
   
   -- and mutually exclusive patterns -> none should pass
   local D = bdd.apply(bdd.op_and, D1, D2)

   -- none should pass
   for i=0,(2^bits - 1) do
      assert_equal(0, bdd.exec(D, num2bin(i, bits)), "for " .. tostring(i))
   end
end

function print_board(b, sz)
   local cells = sz * sz
   for i=1,cells do
      local cell
      if type(b[i]) == "boolean" then
         cell = b[i] and "Q" or "x"
      else
         cell = "."
      end
      io.write(cell)
      if i % sz == 0 then io.write("\n") end
   end
end

-- Check that anysat handles skipped row variables properly.
function test_dontcare_anysat()
   local D = assert(bdd.build({nil, nil, true, nil}, 4))
   local sols = bdd.allsat(D)

   -- 8 solutions
   assert_equal(8, #sols)

   -- anysat should return first, {f,f,t,f}
   local res = bdd.anysat(D)
   assert_equal(4, #res)
   assert_false(res[1])
   assert_false(res[2])
   assert_true(res[3])
   assert_false(res[4])
end

function test_2x2_rook()
   local bits = 4
   -- each row has 1
   local D1nnn = bdd.build({true, nil, nil, nil}, bits)
   local Dn1nn = bdd.build({nil, true, nil, nil}, bits)
   local either0 = bdd.apply(bdd.op_or, D1nnn, Dn1nn)

   local Dnn1n = bdd.build({nil, nil, true, nil}, bits)
   local Dnnn1 = bdd.build({nil, nil, nil, true}, bits)
   local either1 = bdd.apply(bdd.op_or, Dnn1n, Dnnn1)
   local one_of_each = bdd.apply(bdd.op_and, either0, either1)

   -- no horiz attacks
   local horiz00 = bdd.build({true, false, nil, nil}, bits)
   local horiz01 = bdd.build({false, true, nil, nil}, bits)
   local h0 = bdd.apply(bdd.op_or, horiz00, horiz01)

   local horiz10 = bdd.build({nil, nil, true, false}, bits)
   local horiz11 = bdd.build({nil, nil, false, true}, bits)
   local h1 = bdd.apply(bdd.op_or, horiz10, horiz11)

   local no_h_attacks = bdd.apply(bdd.op_and, h0, h1)

   -- no vert attacks
   local vert00 = bdd.build({true, nil, false, nil}, bits)
   local vert01 = bdd.build({nil, true, nil, false}, bits)
   local v0 = bdd.apply(bdd.op_or, vert00, vert01)

   local vert10 = bdd.build({false, nil, true, nil}, bits)
   local vert11 = bdd.build({nil, false, nil, true}, bits)
   local v1 = bdd.apply(bdd.op_or, vert10, vert11)
   local no_v_attacks = bdd.apply(bdd.op_and, v0, v1)

   local board = bdd.apply(bdd.op_and, no_h_attacks, no_v_attacks)

   function D(d, name)
      if false then
         print(string.format("\n\n%s:", name))
         bdd.pp(d)
      end
   end

   D(D1nnn, "D1nnn")
   D(Dn1nn, "Dn1nn")
   D(either0, "either0")
   D(Dnn1n, "Dnn1n")
   D(Dnnn1, "Dnnn1")
   D(either1, "either1")
   D(one_of_each, "one_of_each")
   D(horiz00, "horiz00")
   D(horiz01, "horiz01")
   D(h0, "h0")
   D(horiz10, "horiz10")
   D(horiz11, "horiz11")
   D(h1, "h1")
   D(no_h_attacks, "no_h_attacks")
   D(vert00, "vert00")
   D(vert01, "vert01")
   D(vert10, "vert10")
   D(vert11, "vert11")
   D(no_v_attacks, "no_v_attacks")
   
   D(board, "board")

   local final = bdd.apply(bdd.op_and, board, one_of_each)
   D(final, "final")
   -- bdd.itersat(final, function(sol) print_board(sol, 2); print "\n" end)

   local res = bdd.allsat(final)
   assert_equal(2, #res)

end

local function pos(sz, col, row) return (row - 1) * sz + col end

local function any_in_row(sz, offset)
   local r
   local cells = sz * sz
   for c=1,sz do
      local b = {}
      b[c + offset] = true
      r = apply_or(bdd.build(b, cells), r)
   end
   return r
end

local function queen_attack_mask(sz, col, row)
   local b = {}
   local cells = sz * sz

   -- row attacks
   for i=1,sz do 
      b[pos(sz, i, row)] = false
   end
   
   -- col attacks
   for i=1,sz do 
      b[pos(sz, col, i)] = false
   end
   
   b[pos(sz, col, row)] = true
   
   -- diagonals
   for drow=1,sz do
      local delta = math.abs(drow - row)
      if delta ~= 0 then
         local dcolL, dcolR = col - delta, col + delta
         if dcolL >= 1 then
            b[pos(sz, dcolL, drow)] = false
         end
         if dcolR <= sz then
            b[pos(sz, dcolR, drow)] = false
         end
      end
   end

   if false then
      print_board(b, sz)
      print ""
   end
   return bdd.build(b, cells)
end

function dump_solutions(d, sz)
   assert(sz)
   bdd.itersat(d, function(sol) print "\n"; print_board(sol, sz) end)
end

function test_contrived_chess_3x3()
   -- on a 3x3 board, place a queen in the top and bottom rows without attacks.
   -- should result in e.g.
   -- Q.. and rotations thereof.
   -- ...
   -- .Q. 

   -- map of all attacks
   local attacks
   for row=1,3,2 do             -- just rows 1 and 3
      local rboard
      for col=1,3 do
         local na = queen_attack_mask(3, col, row)
         if rboard then
            rboard = bdd.apply(bdd.op_or, rboard, na)
         else
            rboard = na
         end
      end

      attacks = apply_and(rboard, attacks)
   end

   local board = attacks
   local sols = bdd.allsat(board)
   assert_equal(4, #sols)
   -- dump_solutions(board, 3)
end

function attack_masks(sz)
   local board = nil
   local cells = sz * sz

   for row=1,sz do
      local row_masks
      for col=1,sz do
         local mask = queen_attack_mask(sz, col, row)

         local debug = false
         row_masks = apply_or(mask, row_masks)

         if debug and board then
            print(string.format("\ncol, row is %d, %d, %d", col, row, nboard.start))
            print_board(b, sz)
            print "===="
            bdd.pp(nboard)
            print_board(bdd.anysat(nboard), sz)
         end
      end
      board = apply_and(row_masks, board)      
   end

   return board
end

function solve_n_queens(sz)
   assert(sz >= 4)
   return attack_masks(sz)
end

function check_nq_sol(n, dump)
   collectgarbage("collect")
   local pre_clock, pre_mem = os.clock(), collectgarbage("count")
   print(string.format("\nFinding all solutions for %d-queens...", n))
   local board = solve_n_queens(n)
   local post_clock, post_mem = os.clock(), collectgarbage("count")
   if true then
      print(string.format("%d queens: %.3f seconds, %.3f kb RAM, %d solutions",
                          n, post_clock - pre_clock, post_mem - pre_mem,
                          bdd.satcount(board), "solutions"))
   end

   -- print the first solution found
   local sol = assert(bdd.anysat(board))
   print_board(sol, n)

   local function verify_solution(sol)
      local queens = 0
      for row=1,n do
         for col=1,n do
            if sol[pos(n, col, row)] then
               queens = queens + 1
               for i=1,n do
                  if i ~= col then assert_false(sol[pos(n, i, row)], "row attack") end
               end
               
               for i=1,n do
                  if i ~= row then assert_false(sol[pos(n, col, i)], "col attack") end
               end
               
               for drow=1,n do
                  local delta = math.abs(drow - row)
                  if delta ~= 0 then
                     local dcolL, dcolR = col - delta, col + delta
                     if dcolL >= 1 then
                        assert_false(sol[pos(n, dcolL, drow)], "diagonal attack")
                     end
                     if dcolR <= n then
                        assert_false(sol[pos(n, dcolR, drow)], "diagonal attack")
                     end
                  end
               end
            end
         end
      end
      assert_equal(n, queens, "not enough queens")
   end
   bdd.itersat(board, verify_solution)

   if dump then dump_solutions(board, n) end
end

function test_nqueens_4()
   check_nq_sol(4)
end

function test_nqueens_5()
   check_nq_sol(5)
end

function test_nqueens_6()
   check_nq_sol(6)
end

function test_nqueens_8()
   check_nq_sol(8)
end

function test_nqueens_9()
   check_nq_sol(9)
end

function test_nqueens_10()
   check_nq_sol(10)
end

lunatest.run()
