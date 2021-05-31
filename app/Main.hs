module Main where

import Lib (run)
import ProductDetails (ProductId(ProductId))

main :: IO ()
main = run $ ProductId 399534
