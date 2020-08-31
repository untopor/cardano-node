{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Node.Configuration.Socket
  ( gatherConfiguredSockets
  , SocketOrSocketInfo(..)
  , SocketConfigError(..)
  , renderSocketConfigError
  )
where

import           Cardano.Prelude hiding (local)
import           Prelude (String)
import qualified Prelude
import           Data.Functor (($>))

import           Control.Monad.Trans.Except.Extra (handleIOExceptT)
import           Network.Socket (AddrInfo (..), AddrInfoFlag (..), Socket, SocketType (..),
                     defaultHints, getAddrInfo)

import           Cardano.Node.Types

#if defined(mingw32_HOST_OS)
#else
import           System.Directory (removeFile)
import           System.IO.Error (isDoesNotExistError)
#endif

#ifdef SYSTEMD
import           System.Systemd.Daemon (getActivatedSockets)
#endif




-- | Since we support systemd socket activation, we have to handle being
-- given actual already-constructed sockets, or the info needed to make new
-- sockets later.
--
data SocketOrSocketInfo socket info =
       ActualSocket socket
     | SocketInfo   info
  deriving Show

-- | Errors for the current module.
data SocketConfigError
    = NoPublicSocketGiven
    | NoLocalSocketGiven
    | ClashingPublicSocketGiven
    | ClashingLocalSocketGiven
    | LocalSocketError FilePath IOException
    | GetAddrInfoError NodeIPAddress IOException
  deriving Show

instance Exception SocketConfigError where
  displayException = renderSocketConfigError

renderSocketConfigError :: SocketConfigError -> String
renderSocketConfigError NoPublicSocketGiven =
    "No configuration for the node's public socket. Please specify a socket "
 <> "path either in the config file, on the command line or via systemd socket "
 <> "activation."

renderSocketConfigError NoLocalSocketGiven =
    "No configuration for the node's local socket. Please specify a socket "
 <> "path either in the config file, on the command line or via systemd socket "
 <> "activation."

renderSocketConfigError ClashingPublicSocketGiven =
    "Configuration for the node's public socket supplied both by config/cli and "
 <> "via systemd socket activation. Please use one or the other but not both."

renderSocketConfigError ClashingLocalSocketGiven =
    "Configuration for the node's local socket supplied both by config/cli and "
 <> "via systemd socket activation. Please use one or the other but not both."

renderSocketConfigError (LocalSocketError fp ex) =
    "Failure while attempting to remove the stale local socket: "
 <> fp <> " : " <> displayException ex

renderSocketConfigError (GetAddrInfoError addr ex) =
    "Failure while getting address information for the public listening "
 <> "address: " <> show addr <> " : " <> displayException ex


-- | Gather from the various sources of configuration which sockets we will use
-- for the public node-to-node and the local node-to-client IPC.
--
-- We get such configuration from:
--
-- * node config file
-- * node cli
-- * systemd socket activation
--
gatherConfiguredSockets :: NodeConfiguration
                        -> NodeCLI
                        -> ExceptT SocketConfigError IO
                                   (Maybe (SocketOrSocketInfo Socket AddrInfo),
                                    Maybe (SocketOrSocketInfo Socket AddrInfo),
                                           SocketOrSocketInfo Socket SocketPath)
gatherConfiguredSockets config cli = do

    mbAllSocketsFromSystemD          <- liftIO getSystemdSockets

    -- Select the sockets or address for public node-to-node comms
    --
    let mbPublicIPv4SocketsAddrFromConfigOrCLI :: Maybe NodeIPv4Address
        mbPublicIPv4SocketsAddrFromConfigOrCLI = nodeIPv4Addr cli

        mbPublicIPv6SocketsAddrFromConfigOrCLI :: Maybe NodeIPv6Address
        mbPublicIPv6SocketsAddrFromConfigOrCLI = nodeIPv6Addr cli

        --TODO: add config file support
        mbPublicIPv4SocketsFromSystemD         = (\(a, _, _) -> a) <$> mbAllSocketsFromSystemD
        mbPublicIPv6SocketsFromSystemD         = (\(_, a, _) -> a) <$> mbAllSocketsFromSystemD

    ipv4Public <-
      case ( mbPublicIPv4SocketsAddrFromConfigOrCLI,
             mbPublicIPv4SocketsFromSystemD ) of

        (Nothing, Nothing)    -> pure Nothing
        (Nothing, Just [])    -> pure Nothing
        (Just{}, Just{})      -> throwError ClashingPublicSocketGiven

        (_, Just (sock : _)) ->
          return (Just (ActualSocket sock))

        (Just addr, _) ->
              fmap SocketInfo . head
          <$> nodeAddressInfo nodeHostIPv4AddressToIPAddress addr

    ipv6Public <-
      case ( mbPublicIPv6SocketsAddrFromConfigOrCLI,
             mbPublicIPv6SocketsFromSystemD ) of
        (Nothing, Nothing)    -> pure Nothing
        (Nothing, Just [])    -> pure Nothing
        (Just{}, Just{})      -> throwError ClashingPublicSocketGiven

        (_, Just (sock : _)) ->
          return (Just (ActualSocket sock))

        (Just addr, _) ->
                fmap SocketInfo . head
            <$> nodeAddressInfo nodeHostIPv6AddressToIPAddress addr

    case (ipv4Public, ipv6Public) of
      (Nothing, Nothing) -> throwError NoPublicSocketGiven
      _                  -> pure ()


    -- Select the socket or path for local node-to-client comms
    --
    let mbLocalSocketFileConfigOrCLI  = socketFile cli `mplus`
                                        ncSocketPath config
        mbLocalSocketFromSystemD      = (\(_, _, a) -> a) <$> mbAllSocketsFromSystemD

    local  <-
      case ( mbLocalSocketFileConfigOrCLI,
             mbLocalSocketFromSystemD ) of
        (Nothing, Nothing)   -> throwError NoLocalSocketGiven
        (Nothing, Just [])   -> throwError NoLocalSocketGiven
        (Just{}, Just{})     -> throwError ClashingLocalSocketGiven

        (_, Just (sock : _)) ->
          return (ActualSocket sock)

        (Just path, _) ->
          removeStaleLocalSocket path $> SocketInfo path

    return (ipv4Public, ipv6Public, local)


-- | Binding a local unix domain socket always expects to create it, and fails
-- if it exists already. So we delete it first if it exists. But only on unix.
--
removeStaleLocalSocket :: SocketPath -> ExceptT SocketConfigError IO ()
#if defined(mingw32_HOST_OS)
removeStaleLocalSocket _ =
    return ()
#else
removeStaleLocalSocket (SocketPath path) =
    handleIOExceptT (LocalSocketError path) $
      removeFile path `catch` \e ->
        if isDoesNotExistError e then return ()
                                 else throwIO e
#endif

nodeAddressInfo :: Show addr
                => (addr -> NodeHostIPAddress)
                -> NodeAddress' addr
                -> ExceptT SocketConfigError IO [AddrInfo]
nodeAddressInfo mkHostAddr (NodeAddress hostAddr port) =
    handleIOExceptT (GetAddrInfoError $ (NodeAddress (mkHostAddr hostAddr) port)) $
      getAddrInfo (Just hints)
                  (Just $ Prelude.show hostAddr)
                  (Just $ Prelude.show port)
  where
    hints = defaultHints {
              addrFlags = [AI_PASSIVE, AI_ADDRCONFIG]
            , addrSocketType = Stream
            }


-- | Possibly return systemd-activated sockets.  Splits the sockets into three
-- groups:'AF_INET' and 'AF_INET6', 'AF_UNIX'. 
--
getSystemdSockets :: IO (Maybe ([Socket], [Socket], [Socket]))
#ifdef SYSTEMD
getSystemdSockets = do
  sds_m <- getActivatedSockets
  case sds_m of
       Nothing    -> return Nothing
       Just socks ->
         Just <$>
          foldM (\(ipv4s, ipv6s, unixs) sock -> do
                  addr <- getSocketName sock
                  case addr of
                    SockAddrInet {}  -> return (sock : ipv4s,        ipv6s,        unixs)
                    SockAddrInet6 {} -> return (       ipv4s, sock : ipv6s,        unixs)
                    SockAddrUnix {}  -> return (       ipv4s,        ipv6s, sock : unixs))
                ([], [], [])
                socks
#else
getSystemdSockets = return Nothing
#endif
