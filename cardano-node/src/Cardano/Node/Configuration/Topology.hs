{-# LANGUAGE OverloadedStrings #-}

module Cardano.Node.Configuration.Topology
  ( TopologyError(..)
  , NetworkTopology(..)
  , NodeHostIPAddress(..)
  , NodeHostIPv4Address(..)
  , NodeHostIPv6Address(..)
  , NodeSetup(..)
  , RemoteAddress(..)
  , nodeAddressToSockAddr
  , readTopologyFile
  , remoteAddressToNodeAddress
  )
where

import           Cardano.Prelude
import           Prelude (String)

import           Control.Exception (IOException)
import qualified Control.Exception as Exception
import           Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as Text
import           Network.Socket (PortNumber)
import           Text.Read (readMaybe)

import           Cardano.Node.Types

import           Ouroboros.Network.NodeToNode (PeerAdvertise (..))
import           Ouroboros.Consensus.Util.Condense (Condense (..))


-- TODO: remove this type
newtype TopologyError
  = NodeIdNotFoundInToplogyFile FilePath
  deriving Show

-- | Domain name with port number
--
data RemoteAddress = RemoteAddress
  { raAddress   :: !Text
  -- ^ either a dns address or ip address
  , raPort      :: !PortNumber
  -- ^ port number of the destination
  , raAdvertise :: !Bool
  -- ^ advertise the peer through gossip protocol
  } deriving (Eq, Ord, Show)


-- | Parse 'raAddress' field as an IP address; if it parses and the valency is
-- non zero return corresponding NodeAddress.
--
remoteAddressToNodeAddress
  :: RemoteAddress
  -> Either (NodeIPAddress,  PeerAdvertise)
            (NodeDnsAddress, PeerAdvertise)
remoteAddressToNodeAddress (RemoteAddress addrText port advertise) =
    case readMaybe (Text.unpack addrText) of
      Nothing   -> Right (NodeAddress (NodeHostDnsAddress addrText) port, advertisePeer)
      Just addr -> Left  (NodeAddress (NodeHostIPAddress addr) port     , advertisePeer)
  where
    advertisePeer =
      if advertise 
        then DoAdvertisePeer
        else DoNotAdvertisePeer


instance Condense RemoteAddress where
  condense (RemoteAddress addr port adv) =
    concat
      [ Text.unpack addr
      , ":"
      , show port
      , " "
      , if adv then "(advertise)" else "(no-advertise)"
      ]

instance FromJSON RemoteAddress where
  parseJSON = withObject "RemoteAddress" $ \v ->
    RemoteAddress
      <$> v .: "addr"
      <*> ((fromIntegral :: Int -> PortNumber) <$> v .: "port")
      <*> v .: "advertise"

instance ToJSON RemoteAddress where
  toJSON ra =
    object
      [ "addr"      .= raAddress ra
      , "port"      .= (fromIntegral (raPort ra) :: Int)
      , "advertise" .= raAdvertise ra
      ]

data NodeSetup = NodeSetup
  { nodeId :: !Word64
  , nodeIPv4Address :: !(Maybe NodeIPv4Address)
  , nodeIPv6Address :: !(Maybe NodeIPv6Address)
  , producers :: ![RemoteAddress]
  } deriving (Eq, Show)

instance FromJSON NodeSetup where
  parseJSON = withObject "NodeSetup" $ \o ->
                NodeSetup
                  <$> o .: "nodeId"
                  <*> o .: "nodeIPv4Address"
                  <*> o .: "nodeIPv6Address"
                  <*> o .: "producers"

instance ToJSON NodeSetup where
  toJSON ns =
    object
      [ "nodeId" .= nodeId ns
      , "nodeIPv4Address" .= nodeIPv4Address ns
      , "nodeIPv6Address" .= nodeIPv6Address ns
      , "producers" .= producers ns
      ]

data NetworkTopology = MockNodeTopology ![NodeSetup]
                     | RealNodeTopology ![RemoteAddress]
  deriving (Eq, Show)

instance FromJSON NetworkTopology where
  parseJSON = withObject "NetworkTopology" $ \o -> asum
                [ MockNodeTopology <$> o .: "MockProducers"
                , RealNodeTopology <$> o .: "Producers"
                ]

instance ToJSON NetworkTopology where
  toJSON top =
    case top of
      MockNodeTopology nss -> object [ "MockProducers" .= toJSON nss ]
      RealNodeTopology ras -> object [ "Producers" .= toJSON ras ]

-- | Read the `NetworkTopology` configuration from the specified file.
-- While running a real protocol, this gives your node its own address and
-- other remote peers it will attempt to connect to.
readTopologyFile :: NodeCLI -> IO (Either Text NetworkTopology)
readTopologyFile ncli = do
  eBs <- Exception.try $ BS.readFile (unTopology $ topologyFile ncli)

  case eBs of
    Left e -> return . Left $ handler e
    Right bs -> return . first handlerJSON . eitherDecode $ LBS.fromStrict bs

 where
  handler :: IOException -> Text
  handler e = Text.pack $ "Cardano.Node.Configuration.Topology.readTopologyFile: "
                        ++ displayException e
  handlerJSON :: String -> Text
  handlerJSON err = "Is your topology file formatted correctly? \
                    \The port and valency fields should be numerical. " <> Text.pack err
