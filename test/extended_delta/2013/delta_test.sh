#!/usr/bin/env bash

if [ ! -f ./13_features_train.txt ]; then
    echo "Extracting features for PAN2013 TRAIN"
    ../../../feature_extraction/main.py ../../../data/pan_2013/ ./13_features_train.txt \
        --corpus brown \
        --normalize false \
        --word-frequencies 300 \
        --char-n-gram 2 3 4 --char-n-gram-size 100
fi

if [ ! -f ./13_features_1.txt ]; then
    echo "Extracting features for PAN2013 TEST 1"
    ../../../feature_extraction/main.py ../../../data/pan_2013_test_1/ ./13_features_1.txt \
        --corpus brown \
        --normalize false \
        --word-frequencies 300 \
        --char-n-gram 2 3 4 --char-n-gram-size 100
fi

if [ ! -f ./13_features_2.txt ]; then
    echo "Extracting features for PAN2013 TEST 2"
    ../../../feature_extraction/main.py ../../../data/pan_2013_test_2/ ./13_features_2.txt \
        --corpus brown \
        --normalize false \
        --word-frequencies 300 \
        --char-n-gram 2 3 4 --char-n-gram-size 100
fi

echo "Using 13 1 features"
for i in {1..100}
do
    ./delta.py ./13_features_train.txt ./13_features_1.txt \
        --with-normalization \
        --opposing-set-size 5
done | ./analyse.hs

echo "Using 13 2 features"
for i in {1..100}
do
    ./delta.py ./13_features_train.txt ./13_features_2.txt \
        --with-normalization \
        --opposing-set-size 2
done | ./analyse.hs
