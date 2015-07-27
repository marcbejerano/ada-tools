#!/bin/bash
cd Properties
mkdir lib 2>/dev/null
gprbuild libAdaProperties.gpr

cd ../Logging
mkdir lib 2>/dev/null
gprbuild libAdaLogging.gpr

cd test
gprbuild LoggerTest.gpr
