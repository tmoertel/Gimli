import System.Cmd (system)
import System.Exit (exitWith)
main = system "prove test/*.t" >>= exitWith
