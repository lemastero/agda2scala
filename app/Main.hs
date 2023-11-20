module Main where

import Agda.Compiler.Backend
import Agda.Main ( runAgda )

main :: IO ()
main = runAgda [backend]

backend :: Backend
backend = Backend scalaBackend

-- Backend' opts env menv0 mod0 def0
scalaBackend :: Backend' () () () () ()
scalaBackend = Backend'
  { backendName           = "agda2scala"
  , backendVersion        = Nothing
  , options               = ()
  , commandLineFlags      = []
  , isEnabled             = \ _ -> True
  , preCompile            = \ _ -> return ()
  , postCompile           = \ _ _ _ -> return ()
  , preModule             = \ _ _ _ _ -> return $ Recompile ()
  , postModule            = \ _ _ _ _ _ -> return ()
  , compileDef            = \ _ _ _ _ -> return ()
  , scopeCheckingSuffices = False
  , mayEraseType          = \ _ -> return True
  }
