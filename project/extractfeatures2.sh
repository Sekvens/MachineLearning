#!/bin/bash

hamsrc=data/datasets/enron/preprocessed/enron2/ham
spamsrc=data/datasets/enron/preprocessed/enron2/spam

prehamdst=data/preprocessed/ham2
prespamdst=data/preprocessed/spam2

finalhamdst=data/numvecs/ham2
finalspamdst=data/numvecs/spam2

node code/parseEnron.js $hamsrc $prehamdst 
node code/parseEnron.js $spamsrc $prespamdst
echo 'done preprocessing'

runhaskell code/stringToNumVec.hs $prehamdst $finalhamdst

echo 'done extracting hams'

runhaskell code/stringToNumVec.hs $prespamdst $finalspamdst

echo 'done extracting spams'
echo 'Done.'
