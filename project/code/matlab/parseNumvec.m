
tic

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
epoch_start             = 900;
epoch_interval          = 100;
epoch_end               = 900;

nhidden_start           = 1;
nhidden_interval        = 4;
nhidden_end             = 22; 

% GD %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lr_start                = 0.1;
lr_interval             = 0.5;
lr_end                  = 3;

% RP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delta0_start            = 0.01;
delta0_interval         = 0.03;
delta0_end              = 0.1;

delta_inc_start         = 1;
delta_inc_interval      = 0.2;
delta_inc_end           = 2;

delta_dec_start         = 0.1;
delta_dec_interval      = 0.3;
delta_dec_end           = 0.9;
trainmax_vals           = [50:10:50];

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

delta0_vals             = [delta0_start:delta0_interval:delta0_end];
delta_inc_vals          = [delta_inc_start:delta_inc_interval:delta_inc_end];
delta_dec_vals          = [delta_dec_start:delta_dec_interval:delta_dec_end];

lr_vals                 = [lr_start:lr_interval:lr_end];
epochs_vals             = [epoch_start:epoch_interval:epoch_end];
nhidden_vals            = [nhidden_start:nhidden_interval:nhidden_end];


bestAnyRatio  = 0.0;
spamRatio     = 0.0;
hamRatio      = 0.0;
bestepochs    = -1;
bestnhidden   = -1;
bestlr        = -1;
bestdelta0    = -1;
bestdeltainc  = -1;
bestdeltadec  = -1;

% Main trial and error loop

for i = 1:numel(epochs_vals)
  for j = 1:numel(nhidden_vals)
    for k = 1:numel(lr_vals)

      epochs = epochs_vals(i);
      nhidden = nhidden_vals(j);
      lr      = lr_vals(k);
      gd_dyn = [lr];

      [net, ...
      anySuccessRatio, ...
      hamSuccessRatio, ...
      spamSuccessRatio, ...
      scoreA, ...
      scoreB, ...
      scoreE, ...
      nhams, ...
      nspams] = ...
      createNet(train_algorithm_vals{1}, ...
                epochs, ...
                nhidden, ...
                gd_dyn, ...
                normTrainPatterns, ...
                traintargets, ...
                normTestPatterns, ...
                testtargets, ...
                testdimy);
      disp(anySuccessRatio);
      if (anySuccessRatio > bestAnyRatio) 
        train_algorithm = train_algorithm_vals{1};
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


for i = 1:numel(epochs_vals)
  for j = 1:numel(nhidden_vals)
    for k = 1:numel(delta0_vals)
      for l = 1:numel(delta_inc_vals)
        for m =1:numel(delta_dec_vals)

          rp_dyn = [delta0_vals(k), delta_inc_vals(l), delta_dec_vals(m), 30];

          epochs = epochs_vals(i);
          nhidden = nhidden_vals(j);
          lr      = lr_vals(k);

          [net, ...
          anySuccessRatio, ...
          hamSuccessRatio, ...
          spamSuccessRatio, ...
          scoreA, ...
          scoreB, ...
          scoreE, ...
          nhams, ...
          nspams] = ...
          createNet(train_algorithm_vals{2}, ...
                    epochs, ...
                    nhidden, ...
                    rp_dyn, ...
                    normTrainPatterns, ...
                    traintargets, ...
                    normTestPatterns, ...
                    testtargets, ...
                    testdimy);
          disp(anySuccessRatio);
          if (anySuccessRatio > bestAnyRatio) 
            train_algorithm = train_algorithm_vals{2};
            bestAnyRatio  = anySuccessRatio;
            bestepochs    = epochs;
            bestnhidden   = nhidden;
            bestdelta0    = rp_dyn(1);
            bestdeltainc  = rp_dyn(2);
            bestdeltadec  = rp_dyn(3);
            bestdeltamax  = rp_dyn(4);
            spamRatio     = spamSuccessRatio;
            hamRatio      = hamSuccessRatio;
          end

        end
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
fprintf(fid, 'testing learning rates from %.2f interval %.2f to %.2f\n\n', ...
  lr_start, lr_interval, lr_end);

fprintf(fid, 'number of epochs: %d\n', bestepochs);
fprintf(fid, 'number of nodes in hidden layer: %d\n', bestnhidden);

if (strcmp(train_algorithm, 'traingd'))
  fprintf(fid, 'train_algoritm: traingd\n');
  fprintf(fid, 'learning rate: %f\n', bestlr);
elseif (strcmp(train_algorithm, 'trainrp'))
  fprintf(fid, 'train_algoritm: trainrp\n');
  fprintf(fid, 'delta0: %.2f\n', bestdelta0);
  fprintf(fid, 'delta_inc: %.2f\n', bestdeltainc);
  fprintf(fid, 'delta_dec: %.2f\n', bestdeltadec);
  fprintf(fid, 'trainmax: %d\n', bestdeltamax);
end

fprintf(fid, 'total no ham patterns: %d\n', nhams);
fprintf(fid, 'total no spam patters: %d\n', nspams);
fprintf(fid, 'total no test patterns: %d\n\n', testdimy);

fprintf(fid, 'any -> success: %d (%2.2f %%)\n', scoreE, bestAnyRatio * 100);
fprintf(fid, 'spam -> spam: %d (%2.2f %%)\n', scoreA, spamSuccessRatio * 100);
fprintf(fid, 'ham -> ham: %d (%2.2f %%)\n\n', scoreB, hamSuccessRatio * 100);
fprintf(fid, 'Total runtime: %.2f secs\n\n', toc);

%fprintf(fid, 'spam -> ham: %d (%2.2f %%)\n', scoreC, spamFailRatio * 100);
%fprintf(fid, 'ham -> spam: %d (%2.2f %%)\n', scoreD, hamFailRatio * 100);
%fprintf(fid, 'any -> fail: %d (%2.2f %%)\n', scoreF, anyFailRatio * 100);

fclose(fid);

