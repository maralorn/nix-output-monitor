#!/bin/sh
echo "test" > $out
if [[ "$name" == "local-build2" ]];
then
   echo "local-build2 is coded to fail in the builder"
   exit 42
fi
echo "Finishing build"
