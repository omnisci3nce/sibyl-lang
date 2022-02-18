#!/bin/sh

echo "Sibyl compiler test suite"
echo "============================="

echo "Parse Tests"
for i in *.pa ; do
  /usr/bin/printf "%.4s... " $i
  ../_build/default/bin/main.exe -parse $i > /dev/null 2>&1
  # $? = 0
  if [ $? -eq 0 ] ; then
    echo ok
  else
    echo fail
  fi
done