#!/bin/bash

hamsrc=~/School/ml/MachineLearning/project/data/datasets/enron
hamsrc+=/preprocessed/enron1/ham

spamsrc=~/School/ml/MachineLearning/project/data/datasets/enron
spamsrc+=/preprocessed/enron1/spam

hamdst=~/School/ml/MachineLearning/project/data/preprocessed/ham
spamdst=~/School/ml/MachineLearning/project/data/preprocessed/spam

numvecdst=~/School/ml/MachineLearning/project/data/

if [$1 == "pre"]; then 
  node code/parseEnron.js $hamsrc $hamdst 
  node code/parseEnron.js $spamsrc $spamdst
  echo 'done preprocessing'
fi

runhaskell code/stringToNumVec.hs $hamdst $numvecdst

echo 'done extracting hams'

runhaskell code/stringToNumVec.hs $spamdst $numvecdst

echo 'done extracting spams'
echo 'Done.'
