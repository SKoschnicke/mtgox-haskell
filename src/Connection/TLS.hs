{-# LANGUAGE ScopedTypeVariables #-}
module Connection.TLS (
runTLS
)
where

import Control.Proxy
import qualified Control.Exception as E
import qualified Control.Proxy.Safe as S
import Network.BSD
import Network.Socket (socket, connect, close, Family(..), SocketType(..), SockAddr(..))
import qualified OpenSSL as SSL
import qualified OpenSSL.Session as SSL

ciphers :: String
ciphers = "TLS_RSA_WITH_AES_128_CBC_SHA, TLS_RSA_WITH_AES_256_CBC_SHA, TLS_RSA_WITH_RC4_128_MD5, TLS_RSA_WITH_RC4_128_SHA"

runTLS :: Proxy p => HostName -> PortNumber -> (SSL.SSL -> S.ExceptionP p a' a b' b S.SafeIO r) -> S.ExceptionP p a' a b' b S.SafeIO r
runTLS = runTLS' (S.bracket id)

runTLS' :: (IO SSL.SSL -> (SSL.SSL -> IO ()) -> (a -> b)) -> HostName -> PortNumber -> a -> b
runTLS' b hostname portNumber f = b (SSL.withOpenSSL $ do
    he   <- getHostByName hostname
    sock <- socket AF_INET Stream defaultProtocol
    let sockaddr = SockAddrInet portNumber (head $ hostAddresses he)
    E.catch (connect sock sockaddr)
          (\(e :: E.SomeException) -> close sock >> error ("cannot open socket " ++ show sockaddr ++ " " ++ show e))
    ctx <- SSL.context
    --SSL.contextSetCiphers ctx ciphers
    SSL.contextSetDefaultCiphers ctx
    SSL.connection ctx sock
    )
    (\ssl -> do SSL.shutdown ssl SSL.Unidirectional
                maybe (return ()) close $ SSL.sslSocket ssl)
    f
