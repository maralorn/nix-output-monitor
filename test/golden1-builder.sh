#!/bin/sh
echo "Hanging if local-build"
if [[ "$name" == "remote-build" ]];
then
while true
do
echo "loop" > /dev/null
done
fi
echo "test" > $out
echo "Finishing build"
