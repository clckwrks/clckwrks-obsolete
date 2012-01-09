module Main where

import Clckwrks (UserId(..))
import Clckwrks.ProfileData.Acid (GetProfileData(..), AddRole(..))
import Clckwrks.ProfileData.Types (Role(..))
import Data.Acid (query, update)
import Data.Acid.Remote (openRemoteState)
import Network (PortID(UnixSocket))
import System.Environment

-- right now this just connects to the server and makes UserId 1 an administrator
--
-- eventually there should be an actually useful command-line interface
main :: IO ()
main =
    do [socket] <- getArgs
       acid <- openRemoteState "localhost" (UnixSocket socket)
       update acid (AddRole (UserId 1) Administrator)
       pd <- query acid (GetProfileData (UserId 1))
       print pd
       