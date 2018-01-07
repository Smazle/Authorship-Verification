#!/usr/bin/env bash

if [ ! -f ./15_features_train.txt ]; then
    echo "Extracting features for PAN2015 TRAIN"
    ../../../feature_extraction/main.py ../../../data/pan_2015/ ./15_features_train.txt \
        --corpus brown \
        --normalize false \
        --special-n-gram 1 2 3 --special-n-gram-size 20
fi

if [ ! -f ./15_features.txt ]; then
    echo "Extracting features for PAN2015 TEST"
    ../../../feature_extraction/main.py ../../../data/pan_2015/ ./15_features.txt \
        --corpus brown \
        --normalize false \
        --special-n-gram 1 2 3 --special-n-gram-size 20
fi

echo "Using 15 features"
for i in {0..20}
do
    for j in {1..100}
    do
        ./delta.py ./15_features_train.txt ./15_features.txt \
            --with-normalization \
            --opposing-set-size $i \
            --with-PN
    done | ./analyse.hs
done
