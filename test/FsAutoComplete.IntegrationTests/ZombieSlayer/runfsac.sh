#!/bin/sh -x
echo Host process: $$

# read _
mono $1 --verbose --hostPID=$$
