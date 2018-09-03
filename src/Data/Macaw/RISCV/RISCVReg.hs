{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Data.Macaw.RISCV.RISCVReg where

import Data.BitVector.Sized
import Data.Macaw.CFG (RegAddrWidth, RegisterInfo(..), PrettyF(..))
import Data.Macaw.Types
import Data.Parameterized
import RISCV.Types
import Text.PrettyPrint.ANSI.Leijen as PP hiding ((<$>))

type GPR_ID = BitVector 5
type FPR_ID = BitVector 5

data RISCVReg (rv :: RV) (tp :: Type) where
  RISCV_PC  :: RISCVReg rv (BVType 32)
  RISCV_GPR :: (KnownRVWidth rv, 1 <= RVWidth rv)
            => GPR_ID -> RISCVReg rv (BVType (RVWidth rv))
  RISCV_FPR :: (KnownRVFloatWidth rv, 1 <= RVFloatWidth rv, FExt << rv)
            => FPR_ID -> RISCVReg rv (BVType (RVFloatWidth rv))

instance Show (RISCVReg rv tp) where
  show RISCV_PC = "pc"
  show (RISCV_GPR id) = "x[" ++ show id ++ "]"
  show (RISCV_FPR id) = "f[" ++ show id ++ "]"

instance ShowF (RISCVReg rv) where
  showF = show

instance PrettyF (RISCVReg rv) where
  prettyF = text . show

instance TestEquality (RISCVReg rv) where
  testEquality x y = orderingIsEqual (compareF x y)
    where
      orderingIsEqual :: OrderingF (x :: k) (y :: k) -> Maybe (x :~: y)
      orderingIsEqual o =
        case o of
         LTF -> Nothing
         EQF -> Just Refl
         GTF -> Nothing

instance Eq (RISCVReg rv tp) where
  r == r'
    | Just _ <- testEquality r r' = True
    | otherwise = False

instance OrdF (RISCVReg rv) where
  compareF RISCV_PC RISCV_PC = EQF
  compareF RISCV_PC _        = LTF
  compareF _        RISCV_PC = GTF

  compareF (RISCV_GPR id) (RISCV_GPR id') = fromOrdering (compare id id')
  compareF (RISCV_GPR _ ) _               = LTF
  compareF _              (RISCV_GPR _  ) = GTF

  compareF (RISCV_FPR id) (RISCV_FPR id') = fromOrdering (compare id id')

instance Ord (RISCVReg rv tp) where
  a `compare` b = toOrdering (a `compareF` b)

instance HasRepr (RISCVReg rv) TypeRepr where
  typeRepr r =
    case r of
      RISCV_PC -> knownRepr
      RISCV_GPR _ -> knownRepr
      RISCV_FPR _ -> knownRepr
