#!/usr/bin/env bash

if [ ! -f ./13_features_train.txt ]; then
    echo "Extracting features for PAN2013 TRAIN"
    ../../feature_extraction/main.py ../../data/pan_2013/ ./13_features_train.txt \
        --corpus brown \
        --normalize false \
        --word-frequencies 300
fi

if [ ! -f ./15_features_train.txt ]; then
    echo "Extracting features for PAN2015 TRAIN"
    ../../feature_extraction/main.py ../../data/pan_2015/ ./15_features_train.txt \
        --corpus brown \
        --normalize false \
        --word-frequencies 200
fi

if [ ! -f ./13_features_1.txt ]; then
    echo "Extracting features for PAN2013 TEST 1"
    ../../feature_extraction/main.py ../../data/pan_2013_test_1/ ./13_features_1.txt \
        --corpus brown \
        --normalize false \
        --word-frequencies 300
fi

if [ ! -f ./13_features_2.txt ]; then
    echo "Extracting features for PAN2013 TEST 2"
    ../../feature_extraction/main.py ../../data/pan_2013_test_2/ ./13_features_2.txt \
        --corpus brown \
        --normalize false \
        --word-frequencies 300
fi

if [ ! -f ./15_features.txt ]; then
    echo "Extracting features for PAN2015 TEST"
    ../../feature_extraction/main.py ../../data/pan_2015/ ./15_features.txt \
        --corpus brown \
        --normalize false \
        --word-frequencies 200
fi

echo "Using 13 1 features"
for j in {1..100}
do
    ./delta.py ./13_features_train.txt ./13_features_1.txt \
        --with-normalization \
        --opposing-set-size 4 \
        --with-PN
done | ./analyse.hs

echo "Using 13 2 features"
for j in {1..100}
do
    ./delta.py ./13_features_train.txt ./13_features_2.txt \
        --with-normalization \
        --opposing-set-size 4 \
        --with-PN
done | ./analyse.hs

echo "Using 15 features"
for j in {1..100}
do
    ./delta.py ./15_features_train.txt ./15_features.txt \
        --with-normalization \
        --opposing-set-size 1 \
        --with-PN
done | ./analyse.hs

echo "For generating AUROC graph"
for i in {1..100}
do
    for j in {1..100}
    do
        ./delta.py ./15_features_train.txt ./15_features.txt \
            --with-normalization \
            --opposing-set-size $i \
            --with-PN
    done | ./analyse.hs
done
