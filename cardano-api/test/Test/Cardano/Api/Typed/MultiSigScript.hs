{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Api.Typed.MultiSigScript
  ( tests
  ) where

import           Cardano.Api.Typed

import           Cardano.Prelude

import           Data.Aeson
import           Hedgehog (Property, discover)
import qualified Hedgehog as H
import           Hedgehog.Internal.Property (failWith)

import           Test.Cardano.Api.Typed.Gen
import           Test.Cardano.Api.Typed.Orphans ()


prop_generateMofNcorrectly :: Property
prop_generateMofNcorrectly = H.property $ do
  RequireMOf req sigs <- H.forAll genMofNRequiredSig
  case length sigs >= req of
    False -> failWith Nothing $ "genMofNRequiredSig: Number of required \
                                \signatures exceed number of available key hashes. \
                                \m: " ++ show req ++ " n: " ++ show (length sigs)
    True -> H.success

prop_roundtrip_MultiSigScript_JSON :: Property
prop_roundtrip_MultiSigScript_JSON =
  H.property $ do
    mss <- H.forAll genMultiSigScript
    H.tripping mss encode eitherDecode


tests :: IO Bool
tests =
  H.checkParallel $$discover
