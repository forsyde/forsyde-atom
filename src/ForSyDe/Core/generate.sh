#!/bin/bash

for i in {2..8}; do
   for ((j=1; j<=$i; j++)); do 
       k=$(expr $i - $j + 1)
       v=$(printf "%0.s_," $(seq 1 $j))
       q=$(printf "%0.s,_" $(seq 1 $k))
       echo "at$i$j (${v:2}x${q::-2}) = x"
   done
done


for i in {2..8}; do    
    v=""
    for ((j=1; j<=$i; j++)); do 
	v="$v, at$i$j<$>x"
    done
    echo "unzip$i x = (${v:2})"
done

for i in {2..8}; do    
    v=""
    for ((j=1; j<=$i; j++)); do 
	v="$v, f (at$i$j<$>x)"
    done
    echo "unzipf$i f x = (${v:2})"
done


for i in {1..8}; do
   for ((j=1; j<=8; j++)); do
       v=""
       q=""
       for ((k=1; k<=$i; k++)); do 
	   v="$v a$k"
	   q="$q <*> a$k"
       done
       echo "psi$i$j f ${v:1} = unzip$j (f <$> ${q:5})"
   done
done
