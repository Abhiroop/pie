{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs, DataKinds #-}
module Leftpad where

import Data.Maybe (fromJust)
import Data.Kind
import Data.Proxy (Proxy(..))
import GHC.TypeLits (Nat, type (<=?), KnownNat)
import Data.Type.Bool (If)
import GHC.TypeNats
import qualified Data.Vector.Sized as V
import qualified GHC.TypeLits as TL

-- data Nat = Z | S Nat


-- data Vec n a where
--     VNil  :: Vec Z a
--     VCons :: a -> Vec m a -> Vec (S m) a


-- instance Show a => Show (Vec n a) where
--     show VNil = "Nil"
--     show (VCons x xs) = show x ++ "-" ++ show xs


-- l1 = VCons 1 ( VCons 2 ( VNil ) ) :: Vec (S (S Z)) Int
-- l2 = VCons 3 ( VCons 4 ( VNil ) ) :: Vec (S (S Z)) Int
-- l3 = VCons 5 (VNil) :: Vec (S Z) Int



leftPad :: Char -> Int -> String -> String
leftPad c i str
  | i <= length str = str
  | otherwise = replicate (i - length str) c ++ str

foo :: (KnownNat n) => [a] -> Maybe (V.Vector n a)
foo str = V.fromList str



type family Max (a :: Nat) (b :: Nat) :: Nat where
  Max a b = If (a <=? b) b a

type family PadK (pad :: Nat) (n :: Nat) :: Nat where
  PadK pad n = If (pad <=? n) 0 (pad - n)

-- data PaddedVec (k :: Nat) (m :: Nat) (a :: Type) where
--   PaddedVec :: (KnownNat k, KnownNat m)
--             => a
--             -> V.Vector m a
--             -> PaddedVec k m a

-- totalVec :: PaddedVec k m a -> V.Vector (k + m) a
-- totalVec (PaddedVec v str) = undefined -- V.replicate @k v V.++ str -- V.replicate (fromIntegral (natVal (Proxy :: Proxy k))) v V.++ str

leftPad' :: forall pad n maxN padK.
            (KnownNat pad
            , KnownNat n
            , KnownNat maxN
            , KnownNat padK
            , maxN ~ Max n pad  -- Enforce max constraint
            , padK ~ PadK pad n  -- Enforce padding count
            )
         => Char
         -> Proxy pad
         -> V.Vector n Char
         -> V.Vector (Max maxN pad) Char
leftPad' padChar _ inputVec = undefined
-- leftPad' c p str
--   | i' <= V.length str = undefined
--   | otherwise = undefined
--   where
--     i :: Integer
--     i = TL.natVal p
--     i' :: Int
--     i' = fromEnum i

-- leftPad' :: forall pad n. (KnownNat n, KnownNat pad)
--          => Char           -- pad character
--          -> Proxy pad      -- pad length at the type level
--          -> V.Vector n Char
--          -> V.Vector (Max n pad) Char
-- leftPad' = undefined

-- data SNat (n :: Nat) where
--     SZ :: SNat Z
--     SS :: SNat n -> SNat (S n

-- toVecStr :: SNat n -> String -> Vec n Char
-- toVecStr SZ [] = VNil
-- toVecStr (SS n) (x:xs) = VCons x (toVecStr n xs)
