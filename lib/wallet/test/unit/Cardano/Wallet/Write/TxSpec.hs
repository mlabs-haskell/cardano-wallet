{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Write.TxSpec where

import Prelude

import Cardano.Api.Gen
    ( genScriptData, shrinkScriptData )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex )
import Cardano.Wallet.Write.Tx
    ( pattern DatumHash
    , datumFromBytes
    , datumFromCardanoScriptData
    , datumHashFromBytes
    , datumHashToBytes
    , datumToBytes
    , datumToCardanoScriptData
    )
import Plutus.V1.Ledger.Api
    ( Data (..) )
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators
    ()
import Test.Hspec
    ( Spec, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..), property, (===) )

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
            it "datumFromBytes . datumToBytes == pure" $ property $ \d -> do
                 let f = datumFromBytes . datumToBytes
                 f d === pure d
            it "datumHashFromBytes . datumHashToBytes == pure . DatumHash" $ property $ \h -> do
                 let f = datumHashFromBytes . datumHashToBytes
                 -- FIXME: Change function signature
                 f h === Just (DatumHash h)


        let decodePlutusData hex = Alonzo.getPlutusData . Alonzo.binaryDataToData <$> datumFromBytes (unsafeFromHex hex)
        let shouldBeRight a b = a `shouldBe` Right b

        describe "datumFromBytes goldens" $ do
            it "I 42" $
                (decodePlutusData "182a") `shouldBe` Right (I 42)

            it "Constr 0 []" $
                (decodePlutusData "D87980") `shouldBe` Right (Constr 0 [])

            it "Constr 1 [B \"hello\", B ..., Map ...]" $ do
                let longDatum =
                        "D87A834568656C6C6F5820859601DEB772672B933EF30D66609610\
                        \C928BCF116951A52F4B8698F34C1FC80A140A1401A001E8480"
                (decodePlutusData longDatum) `shouldBeRight`
                    Constr 1
                        [ B "hello"
                        , B "\133\150\SOH\222\183rg+\147>\243\rf`\150\DLE\201(\
                            \\188\241\SYN\149\SUBR\244\184i\143\&4\193\252\128"
                        , Map [(B "",Map [(B "",I 2000000)])]
                        ]



instance Arbitrary Cardano.ScriptData where
     arbitrary = genScriptData
     shrink = shrinkScriptData

