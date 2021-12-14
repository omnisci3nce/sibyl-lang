#!/bin/sh

echo Paperlang compiler test suite
echo =============================

for i in *.pa ; do
  /usr/bin/printf "%.4s... " $i
  ../_build/default/bin/main.exe $i > /dev/null 2>&1
  # $? = 0
  if [ $? -eq 0 ] ; then
    echo ok
  else
    echo fail
  fi
done