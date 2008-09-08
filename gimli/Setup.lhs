#!/usr/bin/env runhaskell
> import Distribution.Simple
> import System.Cmd (system)
> main = defaultMainWithHooks myHooks
> myHooks = simpleUserHooks { runTests = myRunTests }
> myRunTests args bool pkgDescr localBuildInfo =
>     system "prove test/*.t" >> return ()
