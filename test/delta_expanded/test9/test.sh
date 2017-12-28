#!/usr/bin/env bash

../../feature_extraction/main.py ../../data/pan_2013/ ./13.txt \
    --word-n-gram 2 3 --word-n-gram-size 20 \
    --normalize false --corpus brown

../../feature_extraction/main.py ../../data/pan_2015/ ./15.txt \
    --word-n-gram 2 3 --word-n-gram-size 20 \
    --normalize false --corpus brown

mkdir 13
mkdir 15

for i in {1..10};
do
    echo $i
    for j in {1..100};
    do
        ../../machine_learning/delta.py 13.txt --with-normalization --opposing-set-size $i
    done | ../Mean > ./13/$i.txt

    for j in {1..100};
    do
        ../../machine_learning/delta.py 15.txt --with-normalization --opposing-set-size $i
    done | ../Mean > ./15/$i.txt
done
