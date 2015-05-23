#!/bin/bash

hamsrc=data/datasets/enron/preprocessed/enron1/ham
spamsrc=data/datasets/enron/preprocessed/enron1/spam

prehamdst=data/preprocessed/ham
prespamdst=data/preprocessed/spam

finalhamdst=data/numvecs/ham
finalspamdst=data/numvecs/spam

node code/parseEnron.js $hamsrc $prehamdst 
node code/parseEnron.js $spamsrc $prespamdst
echo 'done preprocessing'

runhaskell code/stringToNumVec.hs $prehamdst $finalhamdst

echo 'done extracting hams'

runhaskell code/stringToNumVec.hs $prespamdst $finalspamdst

echo 'done extracting spams'
echo 'Done.'
