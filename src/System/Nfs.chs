-- System.Nfs
--
-- Copyright (C) 2012 Arne Redlich <arne.redlich@googlemail.com>
--
-- Licensed under the LGPL v2.1 - see the LICENSE file.
--
-- Bindings to Ronny Sahlberg's libnfs - https://github.com/sahlberg/libnfs

{-# LANGUAGE ForeignFunctionInterface #-}

module System.Nfs
       where

-- We need struct timeval - a deficiency in libnfs.h
#include <sys/time.h>
#include <nfsc/libnfs.h>

-- Local Variables: **
-- mode: haskell **
-- compile-command: "cd ../.. && cabal install -v" **
-- End: **
