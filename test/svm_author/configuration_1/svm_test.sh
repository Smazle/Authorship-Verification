#!/usr/bin/env bash

if [ ! -f ./13_train.txt ]; then
    echo "Generating features for PAN 2013 TRAIN"
    ../../../feature_extraction/main.py ../../../data/pan_2013/ ./13_train.txt \
        --normalize false \
        --corpus brown \
        --char-n-gram 3 4 5 --char-n-gram-size 500 \
        --word-n-gram 3 4 --word-n-gram-size 100 \
        --postag-n-gram 2 3 4 --postag-n-gram-size 20
fi

if [ ! -f ./13_test_1.txt ]; then
    echo "Generating features for PAN 2013 TEST 1"
    ../../../feature_extraction/main.py ../../../data/pan_2013_test_1/ ./13_test_1.txt \
        --normalize false \
        --corpus brown \
        --char-n-gram 3 4 5 --char-n-gram-size 500 \
        --word-n-gram 3 4 --word-n-gram-size 100 \
        --postag-n-gram 2 3 4 --postag-n-gram-size 20
fi

if [ ! -f ./13_test_2.txt ]; then
    echo "Generating features for PAN 2013 TEST 2"
    ../../../feature_extraction/main.py ../../../data/pan_2013_test_2/ ./13_test_2.txt \
        --normalize false \
        --corpus brown \
        --char-n-gram 3 4 5 --char-n-gram-size 500 \
        --word-n-gram 3 4 --word-n-gram-size 100 \
        --postag-n-gram 2 3 4 --postag-n-gram-size 20
fi

echo "TESTING PAN2013 1"
for i in {1..100}
do
    ./svm_author.py ./13_train.txt ./13_test_1.txt \
        --with-normalization \
        --c 100 \
        --gamma 0.00001
done | ./analyse.hs

echo "TESTING PAN2013 2"
for i in {1..100}
do
    ./svm_author.py ./13_train.txt ./13_test_2.txt \
        --with-normalization \
        --c 100 \
        --gamma 0.00001
done | ./analyse.hs
