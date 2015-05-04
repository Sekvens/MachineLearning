function [best, fit, stat] = run(gens, populationSize)
    load('gasolver/lab4.mat');

%     best score : 16.06
%    [ -748.3891 3.2140 0.4767 0.2927 167.3969 403.4941 279.3988 610.4546 1935.8055 0.0415 0.3821 0.1399 0.1489 0.0107 1.4439 1.6135 2.6655 201.4489 -1.2844 2.7223 -1.4451 -1.5027 ]
%     generations 200
%     population size 60
	
    GAparams.crossover.func = 'blend';
    GAparams.crossover.pressure = 1.8;
    GAparams.crossover.prob = 0.9;

    GAparams.mutate.prob = 0.005;
    GAparams.mutate.decay = 'linear';
    GAparams.mutate.proportional = 0;
    
    GAparams.select.pressure = 1.8;
    GAparams.select.func = 'rank';
     
    GAparams.replace.elitist = 1;
    GAparams.replace.comparative = 1;    
    GAparams.replace.func = 'all';
        
    addpath('gasolver');
    addpath('gtop');
    mgapar = GAparams;
    mgapar.stop.direction = 'min';
    load('gtop_lab4.mat');
    mgapar.objParams.problem = MGADSMproblem;
    [best, fit, stat] = GAsolver(22, PopInitRange', 'mgadsm', populationSize, gens, mgapar);
    ga_plot_diversity(stat);
end