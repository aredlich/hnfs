{-|
Module      : BasicTests
Description : hnfs-tester - Nfs (client library) test tool
Copyright   : (c) 2014 Arne Redlich <arne.redlich@googlemail.com>
License     : LGPL v2.1
Maintainer  : Arne Redlich <arne.redlich@googlemail.com>
Stability   : experimental
Portability : POSIX

Basic hnfs tests.
-}

module BasicTests ( tests ) where

import Base

import qualified System.Nfs as Nfs

import Test.Tasty
import Test.Tasty.HUnit as HU

test_init_and_destroy_context :: IO ()
test_init_and_destroy_context = withContext $ \ctx -> do
  err <- Nfs.getError ctx
  HU.assertEqual "initContext failed" Nothing err

-- Does not really verify yet that the context is garbage collected.
test_garbage_collect_context :: IO ()
test_garbage_collect_context = Nfs.initContext >> return ()

test_destroy_context_twice :: IO ()
test_destroy_context_twice = do
  ctx <- Nfs.initContext
  Nfs.destroyContext ctx
  Nfs.destroyContext ctx

test_get_fd :: IO ()
test_get_fd = withContext $ \ctx -> do
  fd <- Nfs.getFd ctx
  HU.assertBool "got an fd without mounting" (fd < 0)

test_queue_length :: IO ()
test_queue_length = withContext $ \ctx -> do
  l <- Nfs.queueLength ctx
  HU.assertEqual "unexpected queue length" 0 l

test_get_read_max :: IO ()
test_get_read_max = withContext $ \ctx -> do
  l <- Nfs.getReadMax ctx
  HU.assertEqual "unexpected read max" 0 l

test_get_write_max :: IO ()
test_get_write_max = withContext $ \ctx -> do
  l <- Nfs.getWriteMax ctx
  HU.assertEqual "unexpected write max" 0 l

tests :: TestTree
tests =
  let l = [ (test_init_and_destroy_context, "init and destroy context")
          , (test_destroy_context_twice, "destroy context twice")
            -- disabled as the test is too weak
            -- , (test_garbage_collect_context, "garbage collect context")
          , (test_get_fd, "get fd from context")
          , (test_queue_length, "get queue length from context")
          , (test_get_read_max, "get read max from context")
          , (test_get_write_max, "get write max from context") ]
  in
   testGroup "Basic tests" $
   fmap (\(assertion, desc) -> HU.testCase desc assertion) l
