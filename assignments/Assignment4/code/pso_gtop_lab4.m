function f = pso_gtop_lab4(x)
    persistent prob;
    if isempty(prob)
        prob = coder.load('gtop_lab4.mat');
    end
    if strcmp(x,'init')
        f.PopInitRange = prob.PopInitRange;
        f.ConstrBoundary = 'reflect';
        f.Vectorized = 'off';
        f.Display =  'final';
        f.DemoMode = 'pretty';
        f.PlotFcns = {@psoplotbestf, @psoplotvelocity};
        f.SocialAttraction = 1.25;
        f.CognitiveAttraction = 0.5;
        f.VelocityLimit = 500;
        f.Generations = 300;
        f.IntertiaWeight = [0.7;0.3];
        f.PopulationSize = 40;
    else
        f = mga_dsm(x,prob.MGADSMproblem);
    end
   
    
%      CognitiveAttraction: 0.5000
%          ConstrBoundary: 'reflect'
%         AccelerationFcn: @psoiterate
%                DemoMode: 'pretty'
%                 Display: 'final'
%            FitnessLimit: -Inf
%             Generations: 300
%               HybridFcn: []
%           InertiaWeight: [0.7;0.3]
%       InitialPopulation: []
%       InitialVelocities: []
%                KnownMin: []
%              OutputFcns: {}
%                PlotFcns: {@psoplotbestf, @psoplotvelocity}
%            PlotInterval: 1
%            PopInitRange: [2x22 double]
%          PopulationSize: 40
%          PopulationType: [1x12 char]
%        SocialAttraction: 1.2500
%           StallGenLimit: 50
%               TimeLimit: Inf
%                  TolCon: 1.0000e-06
%                  TolFun: 1.0000e-06
%              Vectorized: 'off'
%           VelocityLimit: 500

%Final best point: [-795.52 3.1445 0.55709 0.7583 262.59 459.04 263.4 569.88 2012.1 0.52941 0.8668 0.030247 0.02972 0.037367 5.3748 1.6183 1.1506 277.75 -3.1369 -0.86963 -1.7364 -0.86793]
%best 18.68
%41.6 s