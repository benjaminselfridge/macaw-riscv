{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Macaw.RISCV.ArchTypes where

import Data.Macaw.CFG
import RISCV.Types

import Data.Macaw.RISCV.RISCVReg

data RISCV (a :: RV)

type instance ArchReg (RISCV rv) = RISCVReg rv
