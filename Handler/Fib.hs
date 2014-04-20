module Handler.Fib where

import Import
import qualified Prelude (tail)

fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (Prelude.tail fibs)

getFibR :: Int -> Handler Value
getFibR i = return $ object ["value" .= (fibs !! abs i)]

