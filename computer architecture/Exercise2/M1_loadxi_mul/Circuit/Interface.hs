-- Sigma16 M1: Interface.hs
-- John T. O'Donnell, 2021
-- See Sigma16/README and https://jtod.github.io/home/Sigma16/

{-# LANGUAGE NamedFieldPuns #-}

module Circuit.Interface where
import HDL.Hydra.Core.Lib

------------------------------------------------------------------------
-- Input/Output
------------------------------------------------------------------------

data SysIO a = SysIO
  { io_DMA        :: a
  , io_memStore   :: a
  , io_memFetch   :: a
  , io_regFetch   :: a
  , io_address    :: [a]
  , io_data       :: [a]
  }

------------------------------------------------------------------------
-- Control signals
------------------------------------------------------------------------

{- The behaviour of the datapath is determined by the values of the
control signals.  A naming convention is used for these signals.  Each
control signal has a name with three parts, separated by _.  The first
part is ctl (to indicate that it's a control signal).  The second part
is the name of the subsystem in the datapath which is being
controlled, and the third part is the specific operation being
commanded by the signal. -}

data CtlSig a = CtlSig
  {
-- Controls for system
    cpu,        -- indicates dff for state generates state controls
    condcc,

-- Controls for ALU
   ctl_alu_a,   -- 3-bit alu operation code
   ctl_alu_b,   --   "
   ctl_alu_c,   --   "
   ctl_x_pc,    -- Transmit pc on x (if 0, transmit reg[sa])
   ctl_y_ad,    -- Transmit ad on y (if 0, transmit reg[sb])

-- Controls for register file
   ctl_rf_ld,   -- Load  register file (if 0, remain unchanged)
   ctl_rf_ldcc, -- Load  R15 (if 0, remain unchanged; ld takes priority)
   ctl_rf_pc,   -- Input to register file is pc (if 0, check ctl_rf_alu)
   ctl_rf_alu,  -- Input to register file is ALU output r (if 0, use m)
   ctl_rf_sd,   -- Use ir_d as source a address (if 0, use ir_sa)
   ctl_rf_sa,   -- Used for loadxi to increment index register
-- Controls for system registers
   ctl_ir_ld,   -- Load ir register (if 0, remain unchanged)
   ctl_pc_ld,   -- Load pc register (if 0, remain unchanged)
   ctl_pc_ad,   -- Input to pc is ad (if 0, r)
   ctl_ad_ld,   -- Load ad register (if 0, remain unchanged)
   ctl_ad_alu,  -- Obtain ad input from alu (if 0, from memory data input)
-- Control for multiplication
   ctl_rf_mul,   -- Input register file is prod value
   ctl_mul_start,
-- Controls for memory
   ctl_ma_pc,   -- Transmit pc on memory address bus (if 0, transmit addr)
   ctl_sto      -- Memory store (if 0, fetch)
   :: a         -- all controls are bit signals
  }

data CtlState a = CtlState
  {dff_instr_fet, st_instr_fet,
   dff_dispatch, st_dispatch,
   dff_add, st_add,
   dff_sub, st_sub,
   dff_mul0, st_mul0, dff_mul1, st_mul1, dff_mul2, st_mul2, dff_mul3, st_mul3, dff_mul4, st_mul4, dff_mul5, st_mul5, dff_mul6, st_mul6, dff_mul7, st_mul7, dff_mul8, st_mul8, dff_mul9, st_mul9, dff_mul10, st_mul10, dff_mul11, st_mul11, dff_mul12, st_mul12, dff_mul13, st_mul13, dff_mul14, st_mul14, dff_mul15, st_mul15, dff_mul16, st_mul16,
   dff_div0, st_div0,
   dff_cmp, st_cmp,
   dff_trap0, st_trap0,
   dff_lea0, st_lea0, dff_lea1, st_lea1, dff_lea2, st_lea2,
   dff_load0, st_load0, dff_load1, st_load1, dff_load2, st_load2,
   dff_loadxi0, st_loadxi0, dff_loadxi1, st_loadxi1, dff_loadxi2, st_loadxi2, dff_loadxi3, st_loadxi3,
   dff_store0, st_store0, dff_store1, st_store1, dff_store2, st_store2,
   dff_jump0, st_jump0, dff_jump1, st_jump1, dff_jump2, st_jump2,
   dff_jumpc00, st_jumpc00, dff_jumpc01, st_jumpc01, dff_jumpc02, st_jumpc02,
   dff_jumpc10, st_jumpc10, dff_jumpc11, st_jumpc11, dff_jumpc12, st_jumpc12,
   dff_jal0, st_jal0, dff_jal1, st_jal1, dff_jal2, st_jal2
   :: a         -- all control states are bit signals
  }
  
data DPoutputs a = DPoutputs
     { aluOutputs :: ([a], [a])
     , r :: [a]        -- alu output
     , ccnew :: [a]     -- alu output
     , mul_out :: (a, [a], [a], [a], [a])
     , ready :: a
     , prod :: [a]
     , rx :: [a]
     , ry :: [a]
     , s :: [a]
     , ma :: [a]
     , md :: [a]
     , a :: [a]
     , b :: [a]
     , cc :: [a]
     , ir :: [a]
     , ir_op :: [a]
     , ir_d :: [a]
     , ir_sa :: [a]
     , ir_sb :: [a]
     , pc :: [a]
     , ad :: [a]
     , x :: [a]
     , y :: [a]
     , p :: [a]
     , q :: [a]
     }
