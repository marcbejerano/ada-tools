#!/bin/bash
cd Properties
mkdir lib 2>/dev/null
gprclean libAdaProperties.gpr

cd ../Logging
mkdir lib 2>/dev/null
gprclean libAdaLogging.gpr

cd test
gprclean LoggerTest.gpr
rm -f loggertest.log 2>/dev/null

