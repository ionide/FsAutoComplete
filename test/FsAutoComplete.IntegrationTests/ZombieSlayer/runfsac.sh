#!/bin/sh -x
echo Host process: $$

# read _
$1 --verbose --hostPID=$$
