#! /usr/bin/env runhugs +l
--
-- hello.hs
-- Copyright (C) 2017 zhangyule <zyl2336709@gmail.com>
--
-- Distributed under terms of the MIT license.
--

module Main where

import System.Environment

{-
main :: IO ()
main = do args <- getArgs
          putStrLn ("Hello, " ++ args !! 0)
-}
main :: IO ()
main = do line <- getLine
          putStrLn ("Hello, " ++ line)
