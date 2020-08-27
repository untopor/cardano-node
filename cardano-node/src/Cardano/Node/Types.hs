{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Node.Types
  ( -- * Configuration
    ConfigError(..)
  , ConfigYamlFilePath(..)
  , DbFile(..)
  , GenesisFile(..)
  , NodeCLI(..)
  , NodeConfiguration(..)
  , ProtocolFilepaths (..)
  , GenesisHash(..)
  , MaxConcurrencyBulkSync(..)
  , MaxConcurrencyDeadline(..)
    -- * Node addresses
  , NodeAddress'(..)
  , NodeIPAddress
  , nodeAddressToSockAddr
  , NodeIPv4Address
  , NodeIPv6Address
  , NodeDnsAddress
  , nodeDnsAddressToDomainAddress
  , NodeHostIPAddress (..)
  , nodeHostIPAddressToSockAddr
  , NodeHostIPv4Address (..)
  , NodeHostIPv6Address (..)
  , nodeHostIPv4AddressToIPAddress
  , nodeHostIPv6AddressToIPAddress
  , NodeHostDnsAddress (..)
  , nodeHostDnsAddressToDomain
  , SocketPath(..)
  , TopologyFile(..)
    -- * Consensus protocol configuration
  , NodeByronProtocolConfiguration(..)
  , NodeHardForkProtocolConfiguration(..)
  , NodeProtocolConfiguration(..)
  , NodeShelleyProtocolConfiguration(..)
  , ViewMode(..)
  , ncProtocol
  , parseNodeConfiguration
  , parseNodeConfigurationFP
  , protocolName
  ) where

import           Cardano.Prelude
import           Prelude (String)

import           Control.Monad (fail)
import           Data.Aeson
import           Data.IP (IP (..), IPv4, IPv6)
import qualified Data.IP as IP
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Yaml (decodeFileThrow)
import           Network.Socket (PortNumber, SockAddr (..))
import qualified Network.DNS as DNS (Domain)
import           System.FilePath (takeDirectory, (</>))
import           System.Posix.Types (Fd)

import           Cardano.Api.Typed (EpochNo)
import qualified Cardano.Chain.Update as Byron
import           Cardano.Crypto (RequiresNetworkMagic (..))
import qualified Cardano.Crypto.Hash as Crypto
import           Cardano.Node.Protocol.Types (Protocol (..))
import           Cardano.Tracing.Config (TraceOptions (..), traceConfigParser)
import           Ouroboros.Network.Block (MaxSlotNo (..))
import           Ouroboros.Network.NodeToNode (DomainAddress (..))

--TODO: things will probably be clearer if we don't use these newtype wrappers and instead
-- use records with named fields in the CLI code.

-- | Errors for the cardano-config module.
newtype ConfigError = ConfigErrorFileNotFound FilePath
    deriving Show

-- | Filepath of the configuration yaml file. This file determines
-- all the configuration settings required for the cardano node
-- (logging, tracing, protocol, slot length etc)
newtype ConfigYamlFilePath = ConfigYamlFilePath
  { unConfigPath :: FilePath }
  deriving newtype (Eq, Show)

newtype DbFile = DbFile
  { unDB :: FilePath }
  deriving newtype Show

newtype GenesisFile = GenesisFile
  { unGenesisFile :: FilePath }
  deriving stock (Eq, Ord)
  deriving newtype (IsString, Show)

instance FromJSON GenesisFile where
  parseJSON (String genFp) = pure . GenesisFile $ Text.unpack genFp
  parseJSON invalid = panic $ "Parsing of GenesisFile failed due to type mismatch. "
                           <> "Encountered: " <> Text.pack (show invalid)

-- Node can be run in two modes.
data ViewMode = LiveView    -- Live mode with TUI
              | SimpleView  -- Simple mode, just output text.
              deriving (Eq, Show)

instance FromJSON ViewMode where
  parseJSON (String str) = case str of
                            "LiveView" -> pure LiveView
                            "SimpleView" -> pure SimpleView
                            view -> panic $ "Parsing of ViewMode: "
                                          <> view <> " failed. "
                                          <> view <> " is not a valid view mode"
  parseJSON invalid = panic $ "Parsing of ViewMode failed due to type mismatch. "
                            <> "Encountered: " <> Text.pack (show invalid)

newtype MaxConcurrencyBulkSync = MaxConcurrencyBulkSync
  { unMaxConcurrencyBulkSync :: Word }
  deriving stock (Eq, Ord)
  deriving newtype (FromJSON, Show)

newtype MaxConcurrencyDeadline = MaxConcurrencyDeadline
  { unMaxConcurrencyDeadline :: Word }
  deriving stock (Eq, Ord)
  deriving newtype (FromJSON, Show)


-- | IPv4 or IPv6 address with a port number.
data NodeAddress' addr = NodeAddress
  { naHostAddress :: !addr
  , naPort :: !PortNumber
  } deriving (Eq, Ord, Show)

type NodeIPAddress   = NodeAddress' NodeHostIPAddress
type NodeIPv4Address = NodeAddress' NodeHostIPv4Address
type NodeIPv6Address = NodeAddress' NodeHostIPv6Address
type NodeDnsAddress  = NodeAddress' NodeHostDnsAddress


instance FromJSON addr => FromJSON (NodeAddress' addr) where
  parseJSON = withObject "NodeAddress" $ \v -> do
    NodeAddress
      <$> v .: "addr"
      <*> ((fromIntegral :: Int -> PortNumber) <$> v .: "port")

instance ToJSON addr => ToJSON (NodeAddress' addr) where
  toJSON na =
    object
      [ "addr" .= toJSON (naHostAddress na)
      , "port" .= (fromIntegral (naPort na) :: Int)
      ]


nodeDnsAddressToDomainAddress :: NodeDnsAddress -> DomainAddress
nodeDnsAddressToDomainAddress NodeAddress { naHostAddress = NodeHostDnsAddress dns, naPort }
  = DomainAddress (Text.encodeUtf8 dns) naPort

nodeAddressToSockAddr :: NodeIPAddress -> SockAddr
nodeAddressToSockAddr (NodeAddress addr port) =
  case unNodeHostIPAddress addr of
    IP.IPv4 ipv4 -> SockAddrInet  port   (IP.toHostAddress ipv4)
    IP.IPv6 ipv6 -> SockAddrInet6 port 0 (IP.toHostAddress6 ipv6) 0

nodeHostIPAddressToSockAddr :: NodeIPAddress -> SockAddr
nodeHostIPAddressToSockAddr NodeAddress { naHostAddress = NodeHostIPAddress ip, naPort } =
    case ip of
      IPv4 ipv4 -> SockAddrInet  (fromIntegral naPort)   (IP.toHostAddress ipv4)
      IPv6 ipv6 -> SockAddrInet6 (fromIntegral naPort) 0 (IP.toHostAddress6 ipv6) 0


newtype NodeHostIPv4Address
  = NodeHostIPv4Address { unNodeHostIPv4Address :: IPv4 }
  deriving newtype Show
  deriving (Eq, Ord)

instance FromJSON NodeHostIPv4Address where
  parseJSON (String ipStr) =
    case readMaybe $ Text.unpack ipStr of
      Just ip -> pure $ NodeHostIPv4Address ip
      Nothing -> panic $ "Parsing of IPv4 failed: " <> ipStr
  parseJSON invalid = panic $ "Parsing of IPv4 failed due to type mismatch. "
                            <> "Encountered: " <> (Text.pack $ show invalid) <> "\n"

instance ToJSON NodeHostIPv4Address where
  toJSON (NodeHostIPv4Address ip) = String (Text.pack $ show ip)


newtype NodeHostIPv6Address
  = NodeHostIPv6Address { unNodeHostIPv6Address :: IPv6 }
  deriving newtype Show
  deriving (Eq, Ord)

instance FromJSON NodeHostIPv6Address where
  parseJSON (String ipStr) =
    case readMaybe $ Text.unpack ipStr of
      Just ip -> pure $ NodeHostIPv6Address ip
      Nothing -> panic $ "Parsing of IPv6 failed: " <> ipStr
  parseJSON invalid = panic $ "Parsing of IPv6 failed due to type mismatch. "
                            <> "Encountered: " <> (Text.pack $ show invalid) <> "\n"
instance ToJSON NodeHostIPv6Address where
  toJSON (NodeHostIPv6Address ip) = String (Text.pack $ show ip)


newtype NodeHostIPAddress
  = NodeHostIPAddress { unNodeHostIPAddress :: IP }
  deriving newtype Show
  deriving (Eq, Ord)

instance FromJSON NodeHostIPAddress where
  parseJSON (String ipStr) =
    case readMaybe $ Text.unpack ipStr of
      Just ip -> pure $ NodeHostIPAddress ip
      Nothing -> panic $ "Parsing of IP failed: " <> ipStr
  parseJSON invalid = panic $ "Parsing of IP failed due to type mismatch. "
                            <> "Encountered: " <> Text.pack (show invalid) <> "\n"

instance ToJSON NodeHostIPAddress where
  toJSON (NodeHostIPAddress ip) = String (Text.pack $ show ip)


nodeHostIPv6AddressToIPAddress :: NodeHostIPv6Address -> NodeHostIPAddress
nodeHostIPv6AddressToIPAddress (NodeHostIPv6Address ip) = NodeHostIPAddress (IPv6 ip)

nodeHostIPv4AddressToIPAddress :: NodeHostIPv4Address -> NodeHostIPAddress
nodeHostIPv4AddressToIPAddress (NodeHostIPv4Address ip) = NodeHostIPAddress (IPv4 ip)


-- | Domain name.
--
newtype NodeHostDnsAddress
  = NodeHostDnsAddress { unNodeHostDnsAddress :: Text }
  deriving newtype Show
  deriving (Eq, Ord)

nodeHostDnsAddressToDomain :: NodeHostDnsAddress -> DNS.Domain
nodeHostDnsAddressToDomain = Text.encodeUtf8 . unNodeHostDnsAddress


data NodeCLI = NodeCLI
  { nodeIPv4Addr    :: !(Maybe NodeIPv4Address)
  , nodeIPv6Addr    :: !(Maybe NodeIPv6Address)
    -- | Filepath of the configuration yaml file. This file determines
    -- all the configuration settings required for the cardano node
    -- (logging, tracing, protocol, slot length etc)
  , configFile      :: !ConfigYamlFilePath
  , topologyFile    :: !TopologyFile
  , databaseFile    :: !DbFile
  , socketFile      :: !(Maybe SocketPath)
  , protocolFiles   :: !ProtocolFilepaths
  , validateDB      :: !Bool
  , shutdownIPC     :: !(Maybe Fd)
  , shutdownOnSlotSynced :: !MaxSlotNo
  }

data NodeConfiguration
  = NodeConfiguration
      { -- Protocol-specific parameters:
         ncProtocolConfig :: NodeProtocolConfiguration

         -- Node parameters, not protocol-specific:
       , ncSocketPath     :: Maybe SocketPath

         -- BlockFetch configuration
       , ncMaxConcurrencyBulkSync :: Maybe MaxConcurrencyBulkSync
       , ncMaxConcurrencyDeadline :: Maybe MaxConcurrencyDeadline

         -- Logging parameters:
       , ncViewMode       :: ViewMode
       , ncLoggingSwitch  :: Bool
       , ncLogMetrics     :: Bool
       , ncTraceConfig    :: TraceOptions
       } deriving Show

class AdjustFilePaths a where
  adjustFilePaths :: (FilePath -> FilePath) -> a -> a

instance AdjustFilePaths NodeConfiguration where
  adjustFilePaths f x@NodeConfiguration {
                        ncProtocolConfig,
                        ncSocketPath
                      } =
    x {
      ncProtocolConfig = adjustFilePaths f ncProtocolConfig,
      ncSocketPath     = adjustFilePaths f ncSocketPath
    }

instance FromJSON NodeConfiguration where
  parseJSON =
    withObject "NodeConfiguration" $ \v -> do

      -- Node parameters, not protocol-specific
      ncSocketPath <- v .:? "SocketPath"

      -- Blockfetch parameters
      ncMaxConcurrencyBulkSync <- v .:? "MaxConcurrencyBulkSync"
      ncMaxConcurrencyDeadline <- v .:? "MaxConcurrencyDeadline"

      -- Logging parameters
      ncViewMode      <- v .:? "ViewMode"         .!= SimpleView
      ncLoggingSwitch <- v .:? "TurnOnLogging"    .!= True
      ncLogMetrics    <- v .:? "TurnOnLogMetrics" .!= True
      ncTraceConfig   <- if ncLoggingSwitch
                           then traceConfigParser v
                           else return TracingOff

      -- Protocol parameters
      protocol <- v .: "Protocol" .!= ByronProtocol
      ncProtocolConfig <-
        case protocol of
          ByronProtocol ->
            NodeProtocolConfigurationByron <$> parseByronProtocol v

          ShelleyProtocol ->
            NodeProtocolConfigurationShelley <$> parseShelleyProtocol v

          CardanoProtocol ->
            NodeProtocolConfigurationCardano <$> parseByronProtocol v
                                             <*> parseShelleyProtocol v
                                             <*> parseHardForkProtocol v
      pure NodeConfiguration {
             ncProtocolConfig
           , ncSocketPath
           , ncMaxConcurrencyBulkSync
           , ncMaxConcurrencyDeadline
           , ncViewMode
           , ncLoggingSwitch
           , ncLogMetrics
           , ncTraceConfig
           }
    where
      parseByronProtocol v = do
        primary   <- v .:? "ByronGenesisFile"
        secondary <- v .:? "GenesisFile"
        npcByronGenesisFile <-
          case (primary, secondary) of
            (Just g, Nothing)  -> return g
            (Nothing, Just g)  -> return g
            (Nothing, Nothing) -> fail $ "Missing required field, either "
                                      ++ "ByronGenesisFile or GenesisFile"
            (Just _, Just _)   -> fail $ "Specify either ByronGenesisFile"
                                      ++ "or GenesisFile, but not both"
        npcByronGenesisFileHash <- v .:? "ByronGenesisHash"

        npcByronReqNetworkMagic     <- v .:? "RequiresNetworkMagic"
                                         .!= RequiresNoMagic
        npcByronPbftSignatureThresh <- v .:? "PBftSignatureThreshold"
        npcByronApplicationName     <- v .:? "ApplicationName"
                                         .!= Byron.ApplicationName "cardano-sl"
        npcByronApplicationVersion  <- v .:? "ApplicationVersion" .!= 1
        protVerMajor                <- v .: "LastKnownBlockVersion-Major"
        protVerMinor                <- v .: "LastKnownBlockVersion-Minor"
        protVerAlt                  <- v .: "LastKnownBlockVersion-Alt" .!= 0

        pure NodeByronProtocolConfiguration {
               npcByronGenesisFile
             , npcByronGenesisFileHash
             , npcByronReqNetworkMagic
             , npcByronPbftSignatureThresh
             , npcByronApplicationName
             , npcByronApplicationVersion
             , npcByronSupportedProtocolVersionMajor = protVerMajor
             , npcByronSupportedProtocolVersionMinor = protVerMinor
             , npcByronSupportedProtocolVersionAlt   = protVerAlt
             }

      parseShelleyProtocol v = do
        primary   <- v .:? "ShelleyGenesisFile"
        secondary <- v .:? "GenesisFile"
        npcShelleyGenesisFile <-
          case (primary, secondary) of
            (Just g, Nothing)  -> return g
            (Nothing, Just g)  -> return g
            (Nothing, Nothing) -> fail $ "Missing required field, either "
                                      ++ "ShelleyGenesisFile or GenesisFile"
            (Just _, Just _)   -> fail $ "Specify either ShelleyGenesisFile"
                                      ++ "or GenesisFile, but not both"
        npcShelleyGenesisFileHash <- v .:? "ShelleyGenesisHash"

        --TODO: these are silly names, allow better aliases:
        protVerMajor    <- v .:  "LastKnownBlockVersion-Major"
        protVerMinor    <- v .:  "LastKnownBlockVersion-Minor"
        protVerMajroMax <- v .:? "MaxKnownMajorProtocolVersion" .!= 1

        pure NodeShelleyProtocolConfiguration {
               npcShelleyGenesisFile
             , npcShelleyGenesisFileHash
             , npcShelleySupportedProtocolVersionMajor = protVerMajor
             , npcShelleySupportedProtocolVersionMinor = protVerMinor
             , npcShelleyMaxSupportedProtocolVersion   = protVerMajroMax
             }

      parseHardForkProtocol v = do
        npcTestShelleyHardForkAtEpoch   <- v .:? "TestShelleyHardForkAtEpoch"
        npcTestShelleyHardForkAtVersion <- v .:? "TestShelleyHardForkAtVersion"
        npcShelleyHardForkNotBeforeEpoch <- v .:? "ShelleyHardForkNotBeforeEpoch"
        pure NodeHardForkProtocolConfiguration {
               npcTestShelleyHardForkAtEpoch,
               npcTestShelleyHardForkAtVersion,
               npcShelleyHardForkNotBeforeEpoch
             }

data ProtocolFilepaths =
     ProtocolFilepaths {
       byronCertFile   :: !(Maybe FilePath)
     , byronKeyFile    :: !(Maybe FilePath)
     , shelleyKESFile  :: !(Maybe FilePath)
     , shelleyVRFFile  :: !(Maybe FilePath)
     , shelleyCertFile :: !(Maybe FilePath)
     }

newtype GenesisHash = GenesisHash (Crypto.Hash Crypto.Blake2b_256 ByteString)
  deriving newtype (Eq, Show, ToJSON, FromJSON)

data NodeProtocolConfiguration =
       NodeProtocolConfigurationByron   NodeByronProtocolConfiguration
     | NodeProtocolConfigurationShelley NodeShelleyProtocolConfiguration
     | NodeProtocolConfigurationCardano NodeByronProtocolConfiguration
                                        NodeShelleyProtocolConfiguration
                                        NodeHardForkProtocolConfiguration
  deriving Show

data NodeShelleyProtocolConfiguration =
     NodeShelleyProtocolConfiguration {
       npcShelleyGenesisFile     :: !GenesisFile
     , npcShelleyGenesisFileHash :: !(Maybe GenesisHash)

       -- | These declare the version of the protocol that the node is prepared
       -- to run. This is usually the version of the protocol in use on the
       -- chain now, but during protocol updates this version will be the one
       -- that we declare that we are ready to move to. This is the endorsement
       -- mechanism for determining when enough block producers are ready to
       -- move to the next version.
       --
     , npcShelleySupportedProtocolVersionMajor :: !Natural
     , npcShelleySupportedProtocolVersionMinor :: !Natural

       -- | The maximum major version of the protocol this node supports.
       -- If the actual version ever goes higher than this then the node
       -- will stop with an appropriate error message.
     , npcShelleyMaxSupportedProtocolVersion :: !Natural
     }
  deriving Show

data NodeByronProtocolConfiguration =
     NodeByronProtocolConfiguration {
       npcByronGenesisFile         :: !GenesisFile
     , npcByronGenesisFileHash     :: !(Maybe GenesisHash)
     , npcByronReqNetworkMagic     :: !RequiresNetworkMagic
     , npcByronPbftSignatureThresh :: !(Maybe Double)

       -- | Update application name.
     , npcByronApplicationName     :: !Byron.ApplicationName

       -- | Application (ie software) version.
     , npcByronApplicationVersion  :: !Byron.NumSoftwareVersion

       -- | These declare the version of the protocol that the node is prepared
       -- to run. This is usually the version of the protocol in use on the
       -- chain now, but during protocol updates this version will be the one
       -- that we declare that we are ready to move to. This is the endorsement
       -- mechanism for determining when enough block producers are ready to
       -- move to the next version.
       --
     , npcByronSupportedProtocolVersionMajor :: !Word16
     , npcByronSupportedProtocolVersionMinor :: !Word16
     , npcByronSupportedProtocolVersionAlt   :: !Word8
     }
  deriving Show

-- | Configuration relating to a hard forks themselves, not the specific eras.
--
data NodeHardForkProtocolConfiguration =
     NodeHardForkProtocolConfiguration {

       -- | If we have knowledge about when the Shelley hard fork is then we
       -- have an opportunity to optimise the bulk sync slightly.
       --
       npcShelleyHardForkNotBeforeEpoch :: Maybe EpochNo

       -- | For testing purposes we support specifying that the hard fork
       -- happens at an exact epoch number (ie the first epoch of the new era).
       --
       -- Obviously if this is used, all the nodes in the test cluster must be
       -- configured the same, or they will disagree.
       --
     , npcTestShelleyHardForkAtEpoch :: Maybe EpochNo

       -- | For testing purposes we support specifying that the hard fork
       -- happens at a given major protocol version. For example this can be
       -- used to cause the Shelley hard fork to occur at the transition from
       -- protocol version 0 to version 1 (rather than the default of from 1 to
       -- 2) which can make the test setup simpler.
       --
       -- Obviously if this is used, all the nodes in the test cluster must be
       -- configured the same, or they will disagree.
       --
     , npcTestShelleyHardForkAtVersion :: Maybe Word
     }
  deriving Show

newtype SocketPath = SocketPath
  { unSocketPath :: FilePath }
  deriving stock (Eq, Ord)
  deriving newtype (FromJSON, IsString, Show)

newtype TopologyFile = TopologyFile
  { unTopology :: FilePath }
  deriving newtype Show

instance AdjustFilePaths NodeProtocolConfiguration where

  adjustFilePaths f (NodeProtocolConfigurationByron pc) =
    NodeProtocolConfigurationByron (adjustFilePaths f pc)

  adjustFilePaths f (NodeProtocolConfigurationShelley pc) =
    NodeProtocolConfigurationShelley (adjustFilePaths f pc)

  adjustFilePaths f (NodeProtocolConfigurationCardano pcb pcs pch) =
    NodeProtocolConfigurationCardano (adjustFilePaths f pcb)
                                     (adjustFilePaths f pcs)
                                     pch

instance AdjustFilePaths NodeByronProtocolConfiguration where
  adjustFilePaths f x@NodeByronProtocolConfiguration {
                        npcByronGenesisFile
                      } =
    x { npcByronGenesisFile = adjustFilePaths f npcByronGenesisFile }

instance AdjustFilePaths NodeShelleyProtocolConfiguration where
  adjustFilePaths f x@NodeShelleyProtocolConfiguration {
                        npcShelleyGenesisFile
                      } =
    x { npcShelleyGenesisFile = adjustFilePaths f npcShelleyGenesisFile }

instance AdjustFilePaths SocketPath where
  adjustFilePaths f (SocketPath p) = SocketPath (f p)

instance AdjustFilePaths GenesisFile where
  adjustFilePaths f (GenesisFile p) = GenesisFile (f p)

instance AdjustFilePaths a => AdjustFilePaths (Maybe a) where
  adjustFilePaths f = fmap (adjustFilePaths f)

ncProtocol :: NodeConfiguration -> Protocol
ncProtocol nc =
    case ncProtocolConfig nc of
      NodeProtocolConfigurationByron{}   -> ByronProtocol
      NodeProtocolConfigurationShelley{} -> ShelleyProtocol
      NodeProtocolConfigurationCardano{} -> CardanoProtocol

parseNodeConfiguration :: NodeCLI -> IO NodeConfiguration
parseNodeConfiguration NodeCLI{configFile} = parseNodeConfigurationFP configFile

parseNodeConfigurationFP :: ConfigYamlFilePath -> IO NodeConfiguration
parseNodeConfigurationFP (ConfigYamlFilePath fp) = do
    nc <- decodeFileThrow fp
    -- Make all the files be relative to the location of the config file.
    pure $ adjustFilePaths (takeDirectory fp </>) nc

-- | A human readable name for the protocol
--
protocolName :: Protocol -> String
protocolName ByronProtocol   = "Byron"
protocolName ShelleyProtocol = "Shelley"
protocolName CardanoProtocol = "Byron; Shelley"
