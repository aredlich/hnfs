-- NfsTest.hs
--
-- Copyright (C) 2014 Arne Redlich <arne.redlich@googlemail.com>
--
-- Licensed under the LGPL v2.1 - see the LICENSE file.
--
-- A collection of tests that can be run without a NFS Server.
module Main where

import Test.Tasty
import Test.Tasty.HUnit as HU

import System.Nfs as Nfs

test_init_context :: TestTree
test_init_context = let assertion = do
                          ctx <- Nfs.initContext
                          err <- Nfs.getError ctx
                          HU.assertEqual "initContext failed" Nothing err
                    in
                     HU.testCase "test initContext" assertion

test_destroy_context :: TestTree
test_destroy_context = let assertion = do
                             ctx <- Nfs.initContext
                             Nfs.destroyContext ctx
                       in HU.testCase "test destroyContext" assertion

test_destroy_context_twice :: TestTree
test_destroy_context_twice = let assertion = do
                                   ctx <- Nfs.initContext
                                   Nfs.destroyContext ctx
                                   Nfs.destroyContext ctx
                             in HU.testCase "test destroyContext 2x" assertion

test_get_fd :: TestTree
test_get_fd = let assertion = do
                    ctx <- Nfs.initContext
                    fd <- Nfs.getFd ctx
                    HU.assertBool "got an fd without mounting" (fd < 0)
              in
               HU.testCase "test getFd" assertion

test_queue_length :: TestTree
test_queue_length = let assertion = do
                          ctx <- Nfs.initContext
                          l <- Nfs.queueLength ctx
                          HU.assertEqual "unexpected queue length" 0 l
                    in
                     HU.testCase "test queueLength" assertion

test_get_read_max :: TestTree
test_get_read_max = let assertion = do
                          ctx <- Nfs.initContext
                          l <- Nfs.getReadMax ctx
                          HU.assertEqual "unexpected read max" 0 l
                    in
                     HU.testCase "test getReadMax" assertion

test_get_write_max :: TestTree
test_get_write_max = let assertion = do
                           ctx <- Nfs.initContext
                           l <- Nfs.getWriteMax ctx
                           HU.assertEqual "unexpected write max" 0 l
                     in
                      HU.testCase "test getWriteMax" assertion


tests :: TestTree
tests = testGroup "NfsTests" [ test_init_context
                             , test_destroy_context
                             , test_destroy_context_twice
                             , test_get_fd
                             , test_queue_length
                             , test_get_read_max
                             , test_get_write_max
                             ]

main :: IO ()
main = defaultMain tests

-- Local Variables: **
-- mode: haskell **
-- compile-command: "cd .. && cabal test -v" **
-- End: **
