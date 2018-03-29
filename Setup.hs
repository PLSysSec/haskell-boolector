import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Simple.LocalBuildInfo
import qualified Distribution.PackageDescription as PD
import System.Directory
import Data.Maybe


main = defaultMainWithHooks simpleUserHooks
    { preConf  = boolectorPreConf
    , confHook = boolectorConf
    -- , preTest  = boolectorPreTest 
    }

boolectorPreConf :: Args -> ConfigFlags -> IO PD.HookedBuildInfo
boolectorPreConf args flags = do
  buildInfo <- preConf simpleUserHooks args flags
  rawSystemExit (fromFlag $ configVerbosity flags) "make" ["all"]
  return buildInfo

boolectorConf :: (PD.GenericPackageDescription, PD.HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
boolectorConf (description, buildInfo) flags = do
  localBuildInfo <- confHook simpleUserHooks (description, buildInfo) flags
  let packageDescription = localPkgDescr localBuildInfo
      library = fromJust $ PD.library packageDescription
      libraryBuildInfo = PD.libBuildInfo library
  dir <- getCurrentDirectory
  return localBuildInfo {
    localPkgDescr = packageDescription {
      PD.library = Just $ library {
        PD.libBuildInfo = libraryBuildInfo {
          PD.includeDirs = (dir ++ "/boolector/boolector/src"):PD.includeDirs libraryBuildInfo,
          PD.extraLibDirs = (dir ++ "/boolector/boolector/build"):PD.extraLibDirs libraryBuildInfo
        }
      }
    }
  }

{- From: https://github.com/snakamura/volare/blob/master/Setup.hs

The following license covers this documentation, and the source code, except
where otherwise indicated.

Copyright 2012, snak. All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS "AS IS" AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}
