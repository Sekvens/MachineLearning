
readdata = 1;

%train_algorithm = 'traingd';
train_algorithm = 'trainrp';

lr            = 2;
epochs        = 1000;
nhidden       = 1;
spampercent   = 0.1;
trainratio    = 0.7;

if (readdata) 
  hamSrc = '../../data/numvecs/ham/';
  spamSrc = '../../data/numvecs/spam/';

  hamListing = dir(hamSrc);
  spamListing = dir(spamSrc);

  hams = getNumVecs(hamListing(3:end), hamSrc);
  [hdimx, hdimy] = size(hams);

  n = floor(hdimy * spampercent);
  [lstdimx, lstdimy] = size(spamListing(3:end));
  if (n > lstdimx)
    disp('Error: Percentage too high, not enough spams');
  else
    spams = getNumVecs(spamListing(3:n), spamSrc);
    [sdimx, sdimy] = size(spams);

    hamtargets = zeros(1, hdimy);
    spamtargets = ones(1, sdimy);

    patterns  = horzcat(hams, spams);
    targets   = horzcat(hamtargets, spamtargets);

    sequence = randperm(length(targets));

    [seqdimx, seqdimy] = size(sequence);

    randpatterns = zeros(29, seqdimy);
    randtargets = zeros(1, seqdimy);

    for idx = 1:numel(sequence)
      index = sequence(idx);
      pattern = patterns(:,index);
      target  = targets(:,index);
      randpatterns(:,idx) = pattern;
      randtargets(:,idx) = target;
    end
  end
end

n = floor(seqdimy * trainratio);

trainpatterns = randpatterns(:, 1:n);
traintargets = randtargets(:, 1:n);

testpatterns = randpatterns(:, (n+1):end);
testtargets = randtargets(:, (n+1):end);

[testdimx, testdimy] = size(testpatterns);

normTrainPatterns = mapminmax(trainpatterns);
normTestPatterns = mapminmax(testpatterns);

net = newff(normTrainPatterns, traintargets, [nhidden], ...
  {'tansig' 'logsig'}, train_algorithm, '', 'mse', {}, {}, '');

if (strcmp(train_algorithm, 'traingd'))
  net.trainParam.lr     = lr;
  net.trainParam.epochs = epochs;
  net.trainParam.goal = 0;
elseif (strcmp(train_algorithm, 'trainrp'))
  net.trainParam.epochs = epochs;
  net.trainParam.goal = 0;
  net.trainParam.max_fail = 1000;
  net.trainParam.min_grad = 0;
end

net = init(net); 

[trained_net, stats] = train(net, normTrainPatterns, traintargets);

testoutput = sim(trained_net, normTestPatterns);

score = 0;

for idx = 1:numel(testoutput)
  element = testoutput(idx);
  if (element > 0.5)
    if (testtargets(idx) > 0.5)
      score = score + 1;
    end
  else
    if (testtargets(idx) <= 0.5)
      score = score + 1;
    end
  end
end

disp('Score: ');
disp(score / testdimy);

