module Main where

import Distribution.Simple (defaultMainWithHooks, preConf, simpleUserHooks)
import System.Directory (doesFileExist)
import System.Process (callProcess)

-- | GHC compiles `Module.lhs` via markdown-unlit, but the committed source is
-- the kebab-case `.md` file.  Create those symlinks before configure/build.
main :: IO ()
main = defaultMainWithHooks simpleUserHooks
    { preConf = \args flags -> do
          exists <- doesFileExist "link-literate.sh"
          if exists
              then callProcess "bash" ["link-literate.sh"]
              else return ()
          preConf simpleUserHooks args flags
    }
