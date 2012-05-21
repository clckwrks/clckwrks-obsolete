-- |Script to do an incremental backup of an application.  To make it
-- automatic the following steps must be taken, an example can be seen
-- in the creativeprompts package.
-- 
--   1. Add a section to the cabal file to build the backups binary
--   2. Add a section to debian/rules for the backups package.
--   3. Add a fixup to install the binary into /etc/cron.hourly.
--   4. Add a section to debian/control to create a backups deb.
--   5. Add dependencies on the archive package in debian/control
--   5. Add a postinst script in debian/.
module Main where

import Extra.SSH (sshExport)
import System.Archive.Prune (prune)
import System.Archive.UpdateMirror
import System.Environment (getArgs, withArgs)
import System.Exit (ExitCode(ExitSuccess, ExitFailure), exitWith)
import System.IO (hPutStr, hPutStrLn, stderr)

-- * Update List of Targets Here

-- Other than the hostname, these values are based on those passed to
-- the server.  However, we can't really know what those are because
-- the server runs on a remote system.
target = "clckwrks-dot-com-production"
user = "upload"
host = "clckwrks.com"
lock = target ++ "/clckwrks-state.lock"
local = "/srv/backups/" ++ target

-- Should be equivalent to "%Y-%m-%d_%H:%M:%S", but this seems to work
-- better when passed to the prune function and used to parse a date.
format = "%F_%T"

-- |Maximum number of backups to keep.
keep = 50

mytargets :: [Target]
mytargets =
    [ RsyncTarget { prettyName = target
                  , src = [ user ++ "@" ++ host ++ ":/srv/" ++ target
                          ]
                  , dest = local ++ "/"
                  , config = genericConfig target format
                  , options = [Rsync "--progress", Rsync "--stats"]
                  }
    ]

main :: IO ()
main = getArgs >>= return . elem "--initialize" >>= \ init ->
       if init
       then hPutStr stderr ("Authenticating connection with " ++ user ++ "@" ++ host ++ "...") >>
            sshExport (user ++ "@" ++ host) Nothing >>=
            either (\ s -> hPutStrLn stderr ("initialization failed: " ++ s) >> exitWith (ExitFailure 1))
                   (\ () -> hPutStrLn stderr "done." >> exitWith ExitSuccess)
       else withArgs ["--exclude", lock, target] (updateMirrorMain mytargets) >>
            prune format local (target ++ "-") keep
