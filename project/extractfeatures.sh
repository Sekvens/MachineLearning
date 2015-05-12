#!/bin/bash

hamsrc=~/School/ml/MachineLearning/project/data/datasets/enron
hamsrc+=/preprocessed/enron1/ham

spamsrc=~/School/ml/MachineLearning/project/data/datasets/enron
spamsrc+=/preprocessed/enron1/spam

prehamdst=~/School/ml/MachineLearning/project/data/preprocessed/ham
prespamdst=~/School/ml/MachineLearning/project/data/preprocessed/spam

finalhamdst=~/School/ml/MachineLearning/project/data/numvecs/ham
finalspamdst=~/School/ml/MachineLearning/project/data/numvecs/spam

if [ "$1" = "pre" ]; then 
  node code/parseEnron.js $hamsrc $prehamdst 
  node code/parseEnron.js $spamsrc $prespamdst
  echo 'done preprocessing'
fi

runhaskell code/stringToNumVec.hs $prehamdst $finalhamdst

echo 'done extracting hams'

runhaskell code/stringToNumVec.hs $prespamdst $finalspamdst

echo 'done extracting spams'
echo 'Done.'
