{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TLS (
runTLS,
getDefaultParams
)
where

import Network.BSD
import Network.Socket (socket, socketToHandle, Family(..), SocketType(..), sClose, SockAddr(..), connect)
import qualified Crypto.Random.AESCtr as RNG
import Data.CertificateStore
import Data.IORef
import Network.TLS
import Network.TLS.Extra
import qualified Control.Exception as E
import System.IO

ciphers :: [Cipher]
ciphers =
	[ cipher_AES128_SHA1
	, cipher_AES256_SHA1
	, cipher_RC4_128_MD5
	, cipher_RC4_128_SHA1
	]

data SessionRef = SessionRef (IORef (SessionID, SessionData))

instance SessionManager SessionRef where
    sessionEstablish (SessionRef ref) sid sdata = writeIORef ref (sid,sdata)
    sessionResume (SessionRef ref) sid = readIORef ref >>= \(s,d) -> if s == sid then return (Just d) else return Nothing
    sessionInvalidate _ _ = return ()

runTLS :: Params -> String -> PortNumber -> (Context -> IO a) -> IO a
runTLS params hostname portNumber f = E.bracket (do
	rng  <- RNG.makeSystem
	he   <- getHostByName hostname
	sock <- socket AF_INET Stream defaultProtocol
	let sockaddr = SockAddrInet portNumber (head $ hostAddresses he)
	E.catch (connect sock sockaddr)
	      (\(e :: E.SomeException) -> sClose sock >> error ("cannot open socket " ++ show sockaddr ++ " " ++ show e))
	dsth <- socketToHandle sock ReadWriteMode
	ctx <- contextNewOnHandle dsth params rng
	return (dsth, ctx))
	(hClose . fst)
	(f . snd)

getDefaultParams :: CertificateStore -> IORef (SessionID, SessionData) -> Maybe (SessionID, SessionData) -> Params
getDefaultParams store sStorage session =
    updateClientParams setCParams $ setSessionManager (SessionRef sStorage) $ defaultParamsClient
        { pConnectVersion    = tlsConnectVer
        , pAllowedVersions   = [TLS10,TLS11,TLS12]
        , pCiphers           = ciphers
        , pCertificates      = []
        , pLogging           = logging
        , onCertificatesRecv = crecv
        }
    where
		setCParams cparams = cparams { clientWantSessionResume = session }
		logging = defaultLogging
		crecv = certificateVerifyChain store 
		tlsConnectVer = TLS10
