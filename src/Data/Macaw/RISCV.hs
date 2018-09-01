{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Data.Macaw.RISCV where

import RISCV.Types

import Data.Macaw.Architecture.Info

data RISCV (a :: RV)

riscv_info :: RVRepr rv -> ArchitectureInfo (RISCV rv)
riscv_info rvRepr =
  ArchitectureInfo { withArchConstraints = undefined
                   , archAddrWidth = undefined
                   , archEndianness = undefined
                   , disassembleFn = undefined
                   , mkInitialAbsState = undefined
                   , absEvalArchFn = undefined
                   , absEvalArchStmt = undefined
                   , postCallAbsState = undefined
                   , identifyCall = undefined
                   , identifyReturn = undefined
                   , rewriteArchFn = undefined
                   , rewriteArchStmt = undefined
                   , rewriteArchTermStmt = undefined
                   , archDemandContext = undefined
                   , postArchTermStmtAbsState = undefined
                   }
