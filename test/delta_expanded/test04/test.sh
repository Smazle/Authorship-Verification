#!/usr/bin/env bash

if [ ! -f ./13.txt ]; then
    echo "13 not found, generating..."
    ../../../feature_extraction/main.py ../../../data/pan_2013/ ./13.txt \
        --char-n-gram 2 3 4 --char-n-gram-size 100 \
        --special-n-gram 2 3 4 --special-n-gram-size 20 \
        --postag-n-gram 1 2 3 --postag-n-gram-size 15 \
        --normalize false --corpus brown
fi

if [ ! -f ./15.txt ]; then
    echo "15 not found, generating..."
    ../../../feature_extraction/main.py ../../../data/pan_2015/ ./15.txt \
        --char-n-gram 2 3 4 --char-n-gram-size 100 \
        --special-n-gram 2 3 4 --special-n-gram-size 20 \
        --postag-n-gram 1 2 3 --postag-n-gram-size 15 \
        --normalize false --corpus brown
fi

for dataset in 13 15
do
    for metric in 1 2
    do
        echo "PAN 20$dataset $metric (opposing authors, mean accuracy 100 runs)"
        for i in {1..10};
        do
            echo -n $i
            echo -n " "
            for j in {1..100};
            do
                ../../../machine_learning/delta.py $dataset.txt\
                    --with-normalization \
                    --opposing-set-size $i \
                    --metric $metric
            done | ../Mean
        done | sort -k2 -n --reverse | head -n 1
    done
done
