
readdata  = 0;

trainratio = 0.7;

if (readdata) 
  % Get numeric vectors from files
  readData
end

n = floor(seqdimy * trainratio);

% Divide train and test patterns
trainpatterns = randpatterns(:, 1:n);
traintargets = randtargets(:, 1:n);

testpatterns = randpatterns(:, (n+1):end);
testtargets = randtargets(:, (n+1):end);

[testdimx, testdimy] = size(testpatterns);

normTrainPatterns = mapminmax(trainpatterns);
normTestPatterns = mapminmax(testpatterns);

train_algorithm_vals    = cell(2, 1);
train_algorithm_vals{1} = 'traingd';
train_algorithm_vals{2} = 'trainrp';

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% COMMON CONFIGS %%%%%%%%%%%%%%%%%%%%%%%
epoch_start             = 100;
epoch_interval          = 100;
epoch_end               = 1000;

nhidden_start           = 1;
nhidden_interval        = 1;
nhidden_end             = 20; 

% GD %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lr_start                = 0.1;
lr_interval             = 0.1;
lr_end                  = 10;

% RP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delta0_start            = 0;
delta0_interval         = 0.01;
delta0_end              = 2;
delta_inc_start         = 1;
delta_inc_interval      = 0.1;
delta_inc_end           = 5;
delta_dec_start         = 0.1;
delta_dec_interval      = 0.1;
delta_dec_end           = 0.9;
trainmax_vals           = [10:10:300];

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

delta_dec_vals          = [delta_dec_start:delta_dec_interval:delta_dec_end];
delta_inc_vals          = [delta_inc_start:delta_inc_interval:delta_inc_end];
delta0_vals             = [delta0_start:delta0_interval:delta0_end];
lr_vals                 = [lr_start:lr_interval:lr_end];
epochs_vals             = [epoch_start:epoch_interval:epoch_end];
nhidden_vals            = [nhidden_start:nhidden_interval:nhidden_end];
rp_dyn = [0.04, 1.2, 0.5, 10];

train_algorithm = 'traingd';

bestAnyRatio  = 0.0;
spamRatio     = 0.0;
hamRatio      = 0.0;
bestepochs    = -1;
bestnhidden   = -1;
bestlr        = -1;

% Main trial and error loop

for i = 1:numel(epochs_vals)
  for j = 1:numel(nhidden_vals)
    for k = 1:numel(lr_vals)

      epochs = epochs_vals(i);
      nhidden = nhidden_vals(j);
      lr      = lr_vals(k);
      gd_dyn = [lr];

      [net, anySuccessRatio, hamSuccessRatio, ...
      spamSuccessRatio, scoreA, scoreB, scoreE] = ...
      createNet( train_algorithm, epochs, nhidden, gd_dyn, ...
        normTrainPatterns, traintargets, normTestPatterns, testtargets, ...
        testdimy);
      if (anySuccessRatio > bestAnyRatio) 
        bestAnyRatio  = anySuccessRatio;
        bestepochs    = epochs;
        bestnhidden   = nhidden;
        bestlr        = lr;
        spamRatio     = spamSuccessRatio;
        hamRatio      = hamSuccessRatio;
      end

    end
  end
end

% Write Score to file
fid = fopen('../../data/results/results.txt', 'w');

fprintf(fid, 'testing epochs from %d interval %d to %d\n', ...
  epoch_start, epoch_interval, epoch_end);
fprintf(fid, 'testing hidden nodes from %d interval %d to %d\n', ...
  nhidden_start, nhidden_interval, nhidden_end);
fprintf(fid, 'testing learning rates from %d interval %d to %d\n\n', ...
  lr_start, lr_interval, lr_end);

fprintf(fid, 'number of epochs: %d\n', bestepochs);
fprintf(fid, 'number of nodes in hidden layer: %d\n', bestnhidden);

if (strcmp(train_algorithm, 'traingd'))
  fprintf(fid, 'train_algoritm: traingd\n');
  fprintf(fid, 'learning rate: %f\n', bestlr);
elseif (strcmp(train_algorithm, 'trainrp'))
  %fprintf(fid, 'train_algoritm: trainrp\n');
  %fprintf(fid, 'delta0: %f\n', net.trainParam.delta0);
  %fprintf(fid, 'delta_inc: %f\n', net.trainParam.delt_inc);
  %fprintf(fid, 'delta_dec: %f\n', net.trainParam.delt_dec);
  %fprintf(fid, 'trainmax: %d\n', net.trainParam.deltamax);
end

fprintf(fid, 'total no ham patterns: %d\n', nhams);
fprintf(fid, 'total no spam patters: %d\n', nspams);
fprintf(fid, 'total no test patterns: %d\n\n', testdimy);

fprintf(fid, 'any -> success: %d (%2.2f %%)\n', scoreE, bestratio * 100);
fprintf(fid, 'spam -> spam: %d (%2.2f %%)\n', scoreA, spamSuccessRatio * 100);
fprintf(fid, 'ham -> ham: %d (%2.2f %%)\n\n', scoreB, hamSuccessRatio * 100);

%fprintf(fid, 'spam -> ham: %d (%2.2f %%)\n', scoreC, spamFailRatio * 100);
%fprintf(fid, 'ham -> spam: %d (%2.2f %%)\n', scoreD, hamFailRatio * 100);
%fprintf(fid, 'any -> fail: %d (%2.2f %%)\n', scoreF, anyFailRatio * 100);

fclose(fid);

