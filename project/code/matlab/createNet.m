
function [net, anySuccessRatio, hamSuccessRatio, ...
  spamSuccessRatio, scoreA,scoreB, scoreE] = ...
  getNumVecs( train_algorithm, epochs, nhidden, dynamics, ...
  normTrainPatterns, traintargets, normTestPatterns, testtargets, testdimy);

  net = newff(normTrainPatterns, traintargets, [nhidden], ...
    {'tansig' 'logsig'}, train_algorithm, '', 'mse', {}, {}, '');
   
  % common parameters 
  net.trainParam.showCommandLine = true;  
  net.trainParam.show            = 100;
  net.trainParam.epochs          = epochs;
  net.trainParam.goal            = 0;
  net.trainParam.min_grad        = 0;
  net.trainParam.max_fail        = 1000;

  if (strcmp(train_algorithm, 'traingd'))
    % traingd parameters
    net.trainParam.lr = dynamics(1);
  elseif (strcmp(train_algorithm, 'trainrp'))
    % trainrp parameters
    net.trainParam.delta0   = dynamics(1);
    net.trainParam.delt_inc = dynamics(2); 
    net.trainParam.delt_dec = dynamics(3);  
    net.trainParam.deltamax = dynamics(4); 
  end

  net = init(net); 

  [trained_net, stats] = train(net, normTrainPatterns, traintargets);

  testoutput = sim(trained_net, normTestPatterns);

  % Calculate score
  calcScore
end

