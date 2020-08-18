import Distribution.Simple
import Distribution.Types.HookedBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Utils (rawSystemExit)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks {
          preBuild = preBuildApron
       }

preBuildApron :: Args -> BuildFlags -> IO HookedBuildInfo
preBuildApron args flags = do
  rawSystemExit (fromFlag $ buildVerbosity flags) "env" ["make", "deps/boolector-dist"]
  preBuild simpleUserHooks args flags
