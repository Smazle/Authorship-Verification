#!/usr/bin/env bash

if [ ! -f ./13_features_1.txt ]; then
    echo "Extracting test features for PAN2013 1"
    ../../feature_extraction/main.py ../../data/pan_2013_test_1/ ./13_features_1.txt \
        --corpus brown \
        --word-frequencies 300
fi

if [ ! -f ./13_features_2.txt ]; then
    echo "Extracting test features for PAN2013 2"
    ../../feature_extraction/main.py ../../data/pan_2013_test_2/ ./13_features_2.txt \
        --corpus brown \
        --word-frequencies 300
fi

if [ ! -f ./15_features.txt ]; then
    echo "Extracting test features for PAN2015"
    ../../feature_extraction/main.py ../../data/pan_2015/ ./15_features.txt \
        --corpus brown \
        --word-frequencies 300
fi

#echo "Using 13 1 features"
#for i in {0..20}
#do
    #for j in {1..100}
    #do
        #../../machine_learning/delta.py ./13_features_1.txt \
            #--with-normalization \
            #--opposing-set-size $i \
            #--with-PN
    #done | ./analyse.hs
#done

echo "Using 13 2 features"
for i in {0..20}
do
    for j in {1..100}
    do
        ../../machine_learning/delta.py ./13_features_2.txt \
            --with-normalization \
            --opposing-set-size $i \
            --with-PN
    done | ./analyse.hs
done

echo "Using 15 features"
for i in {0..20}
do
    for j in {1..100}
    do
        ../../machine_learning/delta.py ./15_features.txt \
            --with-normalization \
            --opposing-set-size $i \
            --with-PN
    done | ./analyse.hs
done
