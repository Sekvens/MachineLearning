addpath('psopt');
addpath('gtop');
options = pso_gtop_lab4('init');

LB = options.PopInitRange(1,:);
UB = options.PopInitRange(2,:);
[x,fval] = pso(@pso_gtop_lab4,22,[],[],[],[],LB,UB,[],options);

