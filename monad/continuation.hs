-- ref : http://www.haskellforall.com/2012/12/the-continuation-monad.html

import Control.Monad

newtype Cont r a = Cont { runCont :: (a -> r) -> r}

-- i.e Cont (IO ()) String
onInput :: (String -> IO ()) -> IO ()
onInput f = forever $ do
  str <- getLine
  f str
