{-|
Module      : Main
Description : hnfs-tester - Nfs (client library) test tool
Copyright   : (c) 2014 Arne Redlich <arne.redlich@googlemail.com>
License     : LGPL v2.1
Maintainer  : Arne Redlich <arne.redlich@googlemail.com>
Stability   : experimental
Portability : POSIX

hnfs-tester main.
-}

{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import qualified Async
import qualified Sync
import Base
import qualified BasicTests as BT
import qualified AdvancedTests as AT
import qualified ConduitTests as CT

import qualified System.Nfs as Nfs

import Control.Monad.Trans.Reader

import Data.Proxy
import Data.Typeable (Typeable)

import Test.Tasty
import Test.Tasty.HUnit as HU
import Test.Tasty.Options

mk_test :: Nfs.ServerAddress ->
           Nfs.ExportName ->
           SyncNfs ->
           (ReaderT TestContext IO (), String) ->
           TestTree
mk_test addr xprt nfs (assertion, desc) =
  HU.testCase desc (withContext $ \ctx ->
                     runReaderT assertion $ TestContext nfs ctx addr xprt)

sync_tests :: Nfs.ServerAddress -> Nfs.ExportName -> TestTree
sync_tests srv xprt = testGroup "Synchronous interface tests" $
             fmap (mk_test srv xprt Sync.nfs) AT.tests

async_tests :: Nfs.ServerAddress -> Nfs.ExportName -> TestTree
async_tests srv xprt = testGroup "Asynchronous interface tests" $
              fmap (mk_test srv xprt Async.nfs) AT.tests

conduit_tests :: Nfs.ServerAddress -> Nfs.ExportName -> TestTree
conduit_tests srv xprt = testGroup "Nfs conduit tests" $
                         fmap (mk_test srv xprt Sync.nfs) CT.tests

-- TODO: make this fail with a nicer error message if server / export are not
-- specified
newtype ServerAddressOpt = ServerAddressOpt Nfs.ServerAddress
                           deriving (Eq, Show, Ord, Typeable)

instance IsOption ServerAddressOpt where
  parseValue s = Just $ ServerAddressOpt s
  optionName = return "server"
  optionHelp = return "NFS server to connect to"

newtype ExportNameOpt = ExportNameOpt Nfs.ExportName
                      deriving (Eq, Show, Ord, Typeable)

instance IsOption ExportNameOpt where
  parseValue s = Just $ ExportNameOpt s
  optionName = return "export"
  optionHelp = return "NFS export to mount"

main :: IO ()
main = let ings = includingOptions [ Option (Proxy :: Proxy ServerAddressOpt)
                                   , Option (Proxy :: Proxy ExportNameOpt)
                                   ] : defaultIngredients
       in
        defaultMainWithIngredients ings $
        askOption $ \(ServerAddressOpt server) ->
        askOption $ \(ExportNameOpt export) ->
        testGroup "HNfs tests" [ BT.tests
                               , sync_tests server export
                               , async_tests server export
                               , conduit_tests server export ]

-- Local Variables: **
-- mode: haskell **
-- compile-command: "cd .. && cabal install -v" **
-- End: **
