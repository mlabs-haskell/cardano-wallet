{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Scenario.CLI.Network
    ( spec
    ) where

import Prelude

import Cardano.CLI
    ( Port (..) )
import Cardano.Wallet.Api.Types
    ( ApiNetworkClock (..)
    , ApiNetworkInformation (..)
    , ApiNetworkParameters
    , NtpSyncingStatus (..)
    , toApiNetworkParameters
    )
import Cardano.Wallet.Primitive.Types
    ( EpochNo (..) )
import Control.Monad
    ( when )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Product.Typed
    ( typed )
import Data.Proxy
    ( Proxy (..) )
import System.Command
    ( Exit (..), Stderr (..), Stdout (..) )
import System.Exit
    ( ExitCode (..) )
import Test.Hspec
    ( SpecWith, it, pendingWith )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain )
import Test.Integration.Framework.DSL
    ( Context (..)
    , KnownCommand
    , cardanoWalletCLI
    , eventually
    , expectCliField
    , expectValidJSON
    )
import Test.Integration.Framework.TestData
    ( cmdOk )
import Test.Utils.Paths
    ( inNixBuild )

spec :: forall t. KnownCommand t => SpecWith (Context t)
spec = do
    it "CLI_NETWORK - cardano-wallet network information" $ \ctx -> do
        info <- getNetworkInfoViaCLI ctx
        let nextEpochNum =
                info ^. (#nextEpoch . #epochNumber . #getApiT)
        nextEpochNum `shouldBe` (currentEpochNo info) + 1

    it "NETWORK_PARAMS - network parameters" $ \ctx -> do
        params <- getNetworkParamsViaCli ctx
        params `shouldBe`
            toApiNetworkParameters (ctx ^. #_blockchainParameters)

    it "CLI_NETWORK - network clock" $ \ctx -> do
        sandboxed <- inNixBuild
        when sandboxed $
            pendingWith "Internet NTP servers unavailable in build sandbox"
        eventually "ntp status = available" $ do
            clock <- getNetworkClockViaCLI ctx
            expectCliField (#ntpStatus . #status)
                (`shouldBe` NtpSyncingStatusAvailable) clock
  where
      getNetworkParamsViaCli
          :: Context t
          -> IO ApiNetworkParameters
      getNetworkParamsViaCli ctx = do
          let port = show (ctx ^. typed @(Port "wallet"))
          (Exit c, Stderr e, Stdout o) <- cardanoWalletCLI @t
              ["network", "parameters", "--port", port ]
          c `shouldBe` ExitSuccess
          e `shouldContain` cmdOk
          expectValidJSON (Proxy @ApiNetworkParameters) o

      getNetworkInfoViaCLI
          :: Context t
          -> IO ApiNetworkInformation
      getNetworkInfoViaCLI ctx = do
          let port = show (ctx ^. typed @(Port "wallet"))
          (Exit c, Stderr e, Stdout o) <- cardanoWalletCLI @t
              ["network", "information", "--port", port ]
          c `shouldBe` ExitSuccess
          e `shouldContain` cmdOk
          expectValidJSON (Proxy @ApiNetworkInformation) o

      getNetworkClockViaCLI
          :: Context t
          -> IO ApiNetworkClock
      getNetworkClockViaCLI ctx = do
          let port = show (ctx ^. typed @(Port "wallet"))
          (Exit c, Stderr e, Stdout o) <- cardanoWalletCLI @t
              ["network", "clock", "--port", port ]
          c `shouldBe` ExitSuccess
          e `shouldContain` cmdOk
          expectValidJSON (Proxy @ApiNetworkClock) o

      currentEpochNo :: ApiNetworkInformation -> EpochNo
      currentEpochNo netInfo =
          netInfo ^. (#networkTip . #epochNumber . #getApiT)
