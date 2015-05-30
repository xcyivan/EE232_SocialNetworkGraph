#!/bin/bash
for i in *.circles
  do
   x="count"
   name=$i$x
   echo $name
   cat $i|wc|awk '{split($0,a," ");print a[1]}'>>$name
   done
  
