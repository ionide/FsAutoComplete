#!/bin/sh -x
echo Host process: $$

# read _
if [ "$2" = '--use-dotnet' ]; then

dotnet $1 --verbose --hostPID=$$

else

mono $1 --verbose --hostPID=$$

fi
