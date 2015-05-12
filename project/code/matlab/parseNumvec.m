
sourceDir = '../../data/numvecs/ham/';
listing = dir(sourceDir);

[dimx, dimy] = size(listing);

res = zeros(29, dimx);
for idx = 1:numel(listing)
  path = strcat(sourceDir, listing(idx).name);
  if (strcmp(strcat(sourceDir, '.'), path) == 0 && ...
      strcmp(strcat(sourceDir,'..'), path) == 0)
    fid = fopen(path, 'r');
    if (fid == -1)
      disp(strcat('ERROR: invalid file path: ', path));
    else 
      arr = fscanf(fid, '%f');
      [arrdimx, arrdimy] = size(arr);
      if (arrdimx ~= 29)
        disp('ERROR: arrays does not match');
      else
        res(:,idx) = arr;
        fclose(fid);
      end
    end
  end
end


train_algorithm = 'traingd';
%train_algorithm = 'trainrp';

lr            = 2;
epochs        = 1000;
nhiddenlayers = 2;

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

