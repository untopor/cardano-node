{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracing.Config
  ( TraceOptions (..)
  , TraceSelection (..)
  , traceConfigParser
  ) where

import           Cardano.Prelude hiding (show)
import           Prelude (Show (..))

import           Data.Aeson
import           Data.Aeson.Types (Parser)

import           Cardano.BM.Tracing (TracingVerbosity (..))

import           Cardano.Node.Orphans ()


data TraceOptions
  = TracingOff
  | TracingOn TraceSelection
  deriving Show

data TraceSelection
  = TraceSelection
  { traceVerbosity :: !TracingVerbosity

  -- Per-trace toggles, alpha-sorted.
  , traceAcceptPolicy :: !Bool
  , traceBlockchainTime :: !Bool
  , traceBlockFetchClient :: !Bool
  , traceBlockFetchDecisions :: !Bool
  , traceBlockFetchProtocol :: !Bool
  , traceBlockFetchProtocolSerialised :: !Bool
  , traceBlockFetchServer :: !Bool
  , traceKeepAliveClient :: !Bool
  , traceChainDB :: !Bool
  , traceChainSyncClient :: !Bool
  , traceChainSyncBlockServer :: !Bool
  , traceChainSyncHeaderServer :: !Bool
  , traceChainSyncProtocol :: !Bool
  , traceDnsResolver :: !Bool
  , traceForge :: !Bool
  , traceForgeStateInfo :: !Bool
  , traceHandshake :: !Bool
  , traceLocalRootPeers :: !Bool
  , tracePublicRootPeers :: !Bool
  , tracePeerSelection :: !Bool
  , traceDebugPeerSelection :: !Bool
  , tracePeerSelectionActions :: !Bool
  , traceConnectionManager :: !Bool
  , traceServer :: !Bool
  , traceLocalConnectionManager :: !Bool
  , traceLocalServer :: !Bool
  , traceLocalChainSyncProtocol :: !Bool
  , traceLocalHandshake :: !Bool
  , traceLocalTxSubmissionProtocol :: !Bool
  , traceLocalTxSubmissionServer :: !Bool
  , traceLocalStateQueryProtocol :: !Bool
  , traceMempool :: !Bool
  , traceMux :: !Bool
  , traceTxInbound :: !Bool
  , traceTxOutbound :: !Bool
  , traceTxSubmissionProtocol :: !Bool
  } deriving (Eq, Show)


traceConfigParser :: Object -> Parser TraceOptions
traceConfigParser v =
  TracingOn <$> (TraceSelection
    <$> v .:? "TracingVerbosity" .!= NormalVerbosity
    -- Per-trace toggles, alpha-sorted.
    <*> v .:? "TraceAcceptPolicy" .!= False
    <*> v .:? "TraceBlockchainTime" .!= False
    <*> v .:? "TraceBlockFetchClient" .!= False
    <*> v .:? "TraceBlockFetchDecisions" .!= True
    <*> v .:? "TraceBlockFetchProtocol" .!= False
    <*> v .:? "TraceBlockFetchProtocolSerialised" .!= False
    <*> v .:? "TraceBlockFetchServer" .!= False
    <*> v .:? "TraceKeepAliveClient" .!= False
    <*> v .:? "TraceChainDb" .!= True
    <*> v .:? "TraceChainSyncClient" .!= True
    <*> v .:? "TraceChainSyncBlockServer" .!= False
    <*> v .:? "TraceChainSyncHeaderServer" .!= False
    <*> v .:? "TraceChainSyncProtocol" .!= False
    <*> v .:? "TraceDNSResolver" .!= False
    <*> v .:? "TraceForge" .!= True
    <*> v .:? "TraceForgeStateInfo" .!= True
    <*> v .:? "TraceHandshake" .!= False
    <*> v .:? "TraceLocalRootPeers" .!= False
    <*> v .:? "TracePublicRootPeers" .!= False
    <*> v .:? "TracePeerSelection" .!= False
    <*> v .:? "TraceDebugPeerSelection" .!= False
    <*> v .:? "TracePeerSelectionActions" .!= False
    <*> v .:? "TraceConnectionManager" .!= False
    <*> v .:? "TraceServer" .!= False
    <*> v .:? "TraceLocalConnectionManager" .!= False
    <*> v .:? "TraceLocalServer" .!= False
    <*> v .:? "TraceLocalChainSyncProtocol" .!= False
    <*> v .:? "TraceLocalHandshake" .!= False
    <*> v .:? "TraceLocalTxSubmissionProtocol" .!= False
    <*> v .:? "TraceLocalTxSubmissionServer" .!= False
    <*> v .:? "TraceLocalStateQueryProtocol" .!= False
    <*> v .:? "TraceMempool" .!= True
    <*> v .:? "TraceMux" .!= True
    <*> v .:? "TraceTxInbound" .!= False
    <*> v .:? "TraceTxOutbound" .!= False
    <*> v .:? "TraceTxSubmissionProtocol" .!= False)
