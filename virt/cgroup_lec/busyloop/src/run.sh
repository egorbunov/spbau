#!/bin/sh
# Workaround bug taking place on my system
# Pthreads don't work in program that has just
# changed its namespaces and execed
/test/busyloop/bin/test
