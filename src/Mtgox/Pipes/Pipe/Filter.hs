module Mtgox.Pipes.Pipe.Filter(
    filterDepth
	)
    where 

import Control.Proxy

import Data.Mtgox

filterDepth :: (Monad m, Proxy p) => () -> Pipe p GoxMessage DepthMsg m r
filterDepth = filterD isDepth >-> mapD extractDepth

isDepth :: GoxMessage -> Bool
isDepth (P (PrivateMsg _ _ (D _))) = True
isDepth _ = False

extractDepth :: GoxMessage -> DepthMsg
extractDepth (P (PrivateMsg _ _ (D msg))) = msg
extractDepth _ = error "[*] tried to extract depth from non-depth gox message."
