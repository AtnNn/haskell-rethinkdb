module Debug (
  module Debug.Trace,
  tr, tracePrint
  ) where

import Debug.Trace

tr :: Show a => String -> a -> a
tr s a = trace (s ++ " " ++ show a) a

tracePrint :: Show a => a -> IO ()
tracePrint = traceIO . show