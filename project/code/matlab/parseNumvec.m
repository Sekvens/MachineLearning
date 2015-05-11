
fid = fopen('../../data/numvecs.txt', 'r');
arr = fscanf(fid, '%f');

train_algorithm = 'traingd';
%train_algorithm = 'trainrp';

lr            = 2;
epochs        = 1000;
nhiddenlayers = 2;

p = splitarr(arr);
%disp(p);

%[ydim, xdim] = size(p);

%t = ones(xdim);

%net = newff(p, t, [nhiddenlayers], {'tansig' 'logsig'}, train_algorithm, ...
  %'', 'mse', {}, {}, '');

%if (strcmp(train_algorithm, 'traingd'))
  %net.trainParam.lr     = lr;
  %net.trainParam.epochs = 1000;
%end

%net = init(net); 
%[trained_net, stats] = train(net, p, t);

