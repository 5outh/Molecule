import Control.Monad.Reader

test (x:xs) = do
  a <- ask
  test xs