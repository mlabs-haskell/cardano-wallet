{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.Primitive.AddressDerivation.MintBurnSpec
    ( spec
    ) where

import Cardano.Wallet.Prelude

import Cardano.Address.Derivation
    ( XPrv, XPub, xprvToBytes )
import Cardano.Address.Script
    ( KeyHash )
import Cardano.Mnemonic
    ( Mnemonic, SomeMnemonic (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , Index (..)
    , Passphrase
    , WalletKey (publicKey)
    , getRawKey
    , hashVerificationKey
    , liftRawKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.MintBurn
    ( derivePolicyKeyAndHash, derivePolicyPrivateKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.AddressDerivationSpec
    ()
import Cardano.Wallet.Unsafe
    ( unsafeBech32Decode, unsafeFromHex, unsafeMkMnemonic, unsafeXPrv )
import GHC.TypeNats
    ( KnownNat )
import Test.Hspec
    ( Expectation, Spec, describe, it, shouldBe )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..), Property, property, vector, (=/=), (===) )
import Test.QuickCheck.Arbitrary
    ( arbitraryBoundedEnum )

import qualified Cardano.Address.Script as CA
import qualified Cardano.Wallet.Primitive.AddressDerivation.Shelley as Shelley
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as T

spec :: Spec
spec = do
    parallel $ describe "Mint/Burn Policy key Address Derivation Properties" $ do
        it "Policy key derivation from master key works for various indexes" $
            property prop_keyDerivationFromXPrv
        it "Policy public key hash matches private key" $
            property prop_keyHashMatchesXPrv
        it "The same index always returns the same private key" $
            property prop_keyDerivationSameIndexSameKey
        it "A different index always returns a different private key" $
            property prop_keyDerivationDiffIndexDiffKey
        it "Using derivePolicyKeyAndHash returns same private key as using derivePolicyPrivateKey" $
            property prop_keyDerivationRelation
        it "Deriving a policy key with cardano-address returns same result as cardano-wallet" $ do
            unit_comparePolicyKeys goldenTestMnemonic (Index 0x80000000)
                "acct_xsk1tqqqnvtppk994fxm05wtlvp6uuu58srue9st8ew29vr5mxxf89tj2ze05pm8qjkyfetpzl58jkgx6dd3s96szhyxfajpc2gxv22xef0ems2cnvh5d5xjemgghg9m8489v6c9rvnt9za2pruyrtkp5y77l5ujxeug"
                -- Child key 1855H/1815H/0H generated by cardano-address CLI
                -- from test mnemonic
            unit_comparePolicyKeyHashes goldenTestMnemonic (Index 0x80000000)
                "6adb501348b99cd38172f355615d7c1d0b9d1e3fc69b565b85a98127"
                -- Hash of child key 1855H/1815H/0H generated by cardano-address
                -- CLI from test mnemonic
            unit_comparePolicyKeys goldenTestMnemonic (Index 0x80000010)
                "acct_xsk1rprds8krzhspy8xhkupk2270hzkexmkdga98zv5ns4x9d9kf89t4qp438a26sn62wsfldthkrvnwe6vnetuehgxyxt4hm46zsw03mknuwef0afcljmultpjjwx8zqm0ftnfkqucwvf5qr4gved6h3gnyeqcsur4p"
                -- Child key 1855H/1815H/16H generated by cardano-address CLI
                -- from test mnemonic
            unit_comparePolicyKeyHashes goldenTestMnemonic (Index 0x80000010)
                "1f07b91d27ca1cbf911ccfba254315733b3c908575ce2fc29d4c6965"
                -- Hash of child key 1855H/1815H/16H generated by
                -- cardano-address CLI from test mnemonic

{-------------------------------------------------------------------------------
                               Properties
-------------------------------------------------------------------------------}

prop_keyDerivationFromXPrv
    :: Passphrase "encryption"
    -> XPrv
    -> Index 'Hardened 'PolicyK
    -> Property
prop_keyDerivationFromXPrv pwd masterkey policyIx =
    rndKey `seq` property () -- NOTE Making sure this doesn't throw
  where
    rndKey :: XPrv
    rndKey = derivePolicyPrivateKey pwd masterkey policyIx

prop_keyHashMatchesXPrv
    :: Passphrase "encryption"
    -> ShelleyKey 'RootK XPrv
    -> Index 'Hardened 'PolicyK
    -> Property
prop_keyHashMatchesXPrv pwd masterkey policyIx =
    hashVerificationKey
      CA.Payment
      (getPublicKey rndKey)
      === keyHash
  where
    rndKey :: ShelleyKey 'PolicyK XPrv
    keyHash :: KeyHash
    (rndKey, keyHash) = derivePolicyKeyAndHash pwd masterkey policyIx

    getPublicKey
        :: ShelleyKey 'PolicyK XPrv
        -> ShelleyKey 'ScriptK XPub
    getPublicKey =
        publicKey . (liftRawKey :: XPrv -> ShelleyKey 'ScriptK XPrv) . getRawKey

prop_keyDerivationSameIndexSameKey
    :: Passphrase "encryption"
    -> XPrv
    -> Index 'Hardened 'PolicyK
    -> Property
prop_keyDerivationSameIndexSameKey pwd masterkey policyIx =
    key1 === key2
  where
    key1 :: XPrv
    key2 :: XPrv
    key1 = derivePolicyPrivateKey pwd masterkey policyIx
    key2 = derivePolicyPrivateKey pwd masterkey policyIx

prop_keyDerivationDiffIndexDiffKey
    :: Passphrase "encryption"
    -> XPrv
    -> Index 'Hardened 'PolicyK
    -> Index 'Hardened 'PolicyK
    -> Property
prop_keyDerivationDiffIndexDiffKey pwd masterkey policyIx1 policyIx2 =
    key1 =/= key2
  where
    key1 :: XPrv
    key2 :: XPrv
    key1 = derivePolicyPrivateKey pwd masterkey policyIx1
    key2 = derivePolicyPrivateKey pwd masterkey policyIx2

prop_keyDerivationRelation
    :: Passphrase "encryption"
    -> XPrv
    -> Index 'Hardened 'PolicyK
    -> Property
prop_keyDerivationRelation pwd masterkey policyIx =
    key1 === key2
  where
    key1 :: XPrv
    key1 = derivePolicyPrivateKey pwd masterkey policyIx

    keyAndHash :: (ShelleyKey 'PolicyK XPrv, KeyHash)
    keyAndHash = derivePolicyKeyAndHash pwd (liftRawKey masterkey) policyIx

    key2 :: XPrv
    key2 = getRawKey $ fst keyAndHash

unit_comparePolicyKeys
    :: KnownNat n
    => Mnemonic n
    -> Index 'Hardened 'PolicyK
    -> Text
    -> Expectation
unit_comparePolicyKeys mnemonic index goldenPolicyKeyBech32 =
    let
        walletRootKey :: XPrv
        walletRootKey =
            Shelley.generateKeyFromSeed (SomeMnemonic mnemonic, Nothing) mempty
            & getRawKey

        walletPolicyKey :: XPrv
        walletPolicyKey =
            derivePolicyPrivateKey (mempty :: Passphrase pwd) walletRootKey index

        walletPolicyKeyBytes :: BS.ByteString
        walletPolicyKeyBytes = xprvToBytes walletPolicyKey

        goldenPolicyKeyBytes :: BS.ByteString
        goldenPolicyKeyBytes =
            BL.toStrict $ unsafeBech32Decode goldenPolicyKeyBech32
    in
        walletPolicyKeyBytes `shouldBe` goldenPolicyKeyBytes

unit_comparePolicyKeyHashes
    :: KnownNat n
    => Mnemonic n
    -> Index 'Hardened 'PolicyK
    -> Text
    -> Expectation
unit_comparePolicyKeyHashes mnemonic index goldenPolicyKeyHashHex =
    let
        walletRootKey :: XPrv
        walletRootKey =
            Shelley.generateKeyFromSeed (SomeMnemonic mnemonic, Nothing) mempty
            & getRawKey

        walletPolicyData :: (ShelleyKey 'PolicyK XPrv, KeyHash)
        walletPolicyData =
            derivePolicyKeyAndHash
              (mempty :: Passphrase pwd) (liftRawKey walletRootKey) index

        walletPolicyKeyHashBytes :: BS.ByteString
        walletPolicyKeyHashBytes = CA.digest $ snd walletPolicyData

        goldenPolicyKeyHashBytes :: BS.ByteString
        goldenPolicyKeyHashBytes =
            unsafeFromHex (T.encodeUtf8 goldenPolicyKeyHashHex)
    in
        walletPolicyKeyHashBytes `shouldBe` goldenPolicyKeyHashBytes

goldenTestMnemonic :: Mnemonic 24
goldenTestMnemonic = unsafeMkMnemonic @24
    [ "history", "stable", "illegal", "holiday"
    , "push", "company", "aisle", "fly"
    , "check", "dog", "earn", "admit"
    , "smart", "rotate", "nation", "goddess"
    , "fix", "wheat", "scissors", "across"
    , "crazy", "actor", "fence", "baby"
    ]

instance Arbitrary XPrv where
    arbitrary = unsafeXPrv . BS.pack <$> vector 128

instance Arbitrary (Index 'Hardened 'PolicyK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum
