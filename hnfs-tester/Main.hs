-- Main module of nfs-tester
--
-- Copyright (C) 2014 Arne Redlich <arne.redlich@googlemail.com>
--
-- Licensed under the LGPL v2.1 - see the LICENSE file for details.

-- CmdArgs wants this
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import qualified System.Console.CmdArgs as CA
import qualified System.Nfs as Nfs

data Args = Args { server :: Nfs.ServerAddress
                 , export :: Nfs.ExportName
                 } deriving (CA.Data, CA.Typeable, Show)

prog :: String
prog = "hnfs-tester"

help :: String
help = "Test program to both exercise and demonstrate the hnfs library"

version :: String
version = "0.0.1"

copyright :: String
copyright = "Copyright (C) 2014 Arne Redlich <arne.redlich@googlemail.com>"

license :: String
license = "Licensed under the LGPL v2.1. See the LICENSE file for details."

args :: CA.Mode (CA.CmdArgs Args)
args = CA.cmdArgsMode $  Args { server = CA.def CA.&= CA.help "NFS server address"
                              , export = CA.def CA.&= CA.help "NFS export"
                              }
       CA.&= CA.help help
       CA.&= CA.summary (prog ++ ", Version " ++ version ++ "\n\n" ++ copyright ++ "\n" ++ license)
       CA.&= CA.program prog

main :: IO ()
main = do
  a <- CA.cmdArgsRun args
  putStrLn $ "Using nfs://" ++ (server a) ++ "/" ++ (export a)


-- Local Variables: **
-- mode: haskell **
-- compile-command: "cd ../.. && cabal install -v" **
-- End: **
