
readdata  = 1;
cli       = 1;

%train_algorithm = 'traingd';
train_algorithm = 'trainrp';

lr            = 2;
epochs        = 1000;
nhidden       = 1;
spampercent   = 0.2;
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

    randpatterns = zeros(19, seqdimy);
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

if (cli) 
  net.trainParam.showCommandLine = true;  
end


net = init(net); 

[trained_net, stats] = train(net, normTrainPatterns, traintargets);

testoutput = sim(trained_net, normTestPatterns);

% spam -> spam
scoreA = 0;

% ham -> ham
scoreB = 0;

% spam -> ham
scoreC = 0;

% ham -> spam
scoreD = 0;

% any -> correct
scoreE = 0;

% any -> false
scoreF = 0;

nhams = 0;
nspams = 0;

for idx = 1:numel(testoutput)
  element = testoutput(idx);
  if (element > 0.5)
    % network thinks pattern is spam
    if (testtargets(idx) > 0.5)
      % pattern is actually spam
      scoreA = scoreA + 1;
      scoreE = scoreE + 1;
      nspams = nspams + 1;
    else 
      % pattern is actually ham 
      scoreF = scoreF + 1;
      scoreD = scoreD + 1;
      nhams = nhams + 1;
    end
  else
    % network thinks pattern is ham
    if (testtargets(idx) <= 0.5)
      % pattern is actually ham 
      scoreB = scoreB + 1;
      scoreE = scoreE + 1;
      nhams = nhams + 1;
    else 
      % pattern is actually spam
      scoreC = scoreC + 1;
      scoreF = scoreF + 1;
      nspams = nspams + 1;
    end
  end
end

spamSuccessRatio = (scoreA / nspams);
hamSuccessRatio = (scoreB / nhams);
spamFailRatio = 1 - spamSuccessRatio;
hamFailRatio = 1 - hamSuccessRatio;
anySuccessRatio = (scoreE / testdimy);
anyFailRatio = 1 - anySuccessRatio;

fprintf('spam -> spam: %d (%2.2f %%)\n', scoreA, spamSuccessRatio * 100);
fprintf('ham -> ham: %d (%2.2f %%)\n', scoreB, hamSuccessRatio * 100);
fprintf('spam -> ham: %d (%2.2f %%)\n', scoreC, spamFailRatio * 100);
fprintf('ham -> spam: %d (%2.2f %%)\n', scoreD, hamFailRatio * 100);
fprintf('any -> success: %d (%2.2f %%)\n', scoreE, anySuccessRatio * 100);
fprintf('any -> fail: %d (%2.2f %%)\n', scoreF, anyFailRatio * 100);

fprintf('total no ham patterns: %d\n', nhams);
fprintf('total no spam patters: %d\n', nspams);
fprintf('total no test patterns: %d\n', testdimy);

