
numvecdim = 17;
spampercent     = 0.30;

hamSrc = '../../data/numvecs/ham2/';
spamSrc = '../../data/numvecs/spam2/';

hamListing = dir(hamSrc);
spamListing = dir(spamSrc);


hams = getNumVecs(hamListing(3:end), hamSrc, numvecdim);
[hdimx, hdimy] = size(hams);

n = floor(hdimy * spampercent);
[lstdimx, lstdimy] = size(spamListing(3:end));
if (n > lstdimx)
  disp('Error: Percentage too high, not enough spams');
else
  spams = getNumVecs(spamListing(3:n), spamSrc, numvecdim);

  spams(2,:) = [];
  hams(2,:)  = [];
  spams(1,:) = [];
  hams(1,:)  = [];
  spams(4,:) = [];
  hams(4,:)  = [];
  spams(5,:) = [];
  hams(5,:)  = [];
  spams(10,:) = [];
  hams(10,:)  = [];
  spams(3,:) = [];
  hams(3,:)  = [];
  spams(5,:) = [];
  hams(5,:)  = [];
  spams(3,:) = [];
  hams(3,:)  = [];
  spams(8,:) = [];
  hams(8,:)  = [];
  spams(7,:) = [];
  hams(7,:)  = [];
  spams(5,:) = [];
  hams(5,:)  = [];
  spams(5,:) = [];
  hams(5,:)  = [];

  [hdimx, hdimy] = size(hams);
  [sdimx, sdimy] = size(spams);
  numvecdim = 5;


  hamtargets = zeros(1, hdimy);
  spamtargets = ones(1, sdimy);

  patterns  = horzcat(hams, spams);
  targets   = horzcat(hamtargets, spamtargets);

  sequence = randperm(length(targets));

  [seqdimx, seqdimy] = size(sequence);

  randpatterns = zeros(numvecdim, seqdimy);
  randtargets = zeros(1, seqdimy);

  for idx = 1:numel(sequence)
    index = sequence(idx);
    pattern = patterns(:,index);
    target  = targets(:,index);
    randpatterns(:,idx) = pattern;
    randtargets(:,idx) = target;
  end
end

% 1 total length
% 2 short words
% 3 avg word len 
% 4 avg sentence in chars 
% 5 avg sentence in words 
% 6 alpharatio 
% 7 digitratio 
% 8 punctratio 
% 9 whiteratio 
% 10 noccuringwords len 1 
% 11 noccuringwords len 2 
% 12 simpsons 
% 13 spamwords 
% 14 linkwords 
% 15 wordenfreq 1 
% 15 wordenfreq 2 
% 15 wordenfreq 3 

% 3 avg word len 
% 4 avg sentence in chars 
% 10 noccuringwords len 1 
% 12 simpsons 

