{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Write.TxSpec where

import Prelude

import Cardano.Api.Gen
    ( genScriptData, shrinkScriptData )
import Cardano.Wallet.Write.Tx
    ( pattern Datum
    , pattern DatumHash
    , datumFromBytes
    , datumFromCardanoScriptData
    , datumHashFromBytes
    , datumHashToBytes
    , datumToBytes
    , datumToCardanoScriptData
    )
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators
    ()
import Test.Hspec
import Test.QuickCheck

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Ledger.Alonzo.Data as Alonzo

spec :: Spec
spec = describe "TxSpec" $ do
    describe "Isomorphisms" $ do
        describe "Datum <-> Cardano.ScriptData" $ do
            it "datumFromCardanoScriptData . datumToCardanoScriptData == id" $
                property $ \x -> do
                    let f = datumFromCardanoScriptData . datumToCardanoScriptData
                    f x === x

            it "datumToCardanoScriptData . datumFromCardanoScriptData == id" $ do
                property $ \x -> do
                    let f = datumToCardanoScriptData . datumFromCardanoScriptData
                    f x === x

        describe "Alonzo.BinaryData <-> Alonzo.Data" $ do
            it "dataToBinaryData . binaryDataToData == id" $ property $ \d -> do
                 let f = Alonzo.dataToBinaryData . Alonzo.binaryDataToData
                 f d === d
            it "binaryDataToData . dataToBinaryData  == id" $ property $ \d -> do
                 let f = Alonzo.binaryDataToData . Alonzo.dataToBinaryData
                 f d === d
        describe "Alonzo.Data <-> Cardano.ScriptData" $ do
            it "Cardano.toAlonzoData . Cardano.fromAlonzoData == id" $ property $ \d -> do
                 let f = Cardano.toAlonzoData . Cardano.fromAlonzoData
                 f d === d

            it "Cardano.fromAlonzoData . Cardano.toAlonzoData == id" $ property $ \d -> do
                 let f = Cardano.toAlonzoData . Cardano.fromAlonzoData
                 f d === d

        describe "Roundtrips" $ do
            it "datumFromBytes . datumToBytes == Right . Datum" $ property $ \d -> do
                 let f = datumFromBytes . datumToBytes
                 -- FIXME: Change function signature
                 f d === Right (Datum d)
            it "datumHashFromBytes . datumHashToBytes == Just . DatumHash" $ property $ \h -> do
                 let f = datumHashFromBytes . datumHashToBytes
                 -- FIXME: Change function signature
                 f h === Just (DatumHash h)

instance Arbitrary Cardano.ScriptData where
     arbitrary = genScriptData
     shrink = shrinkScriptData

