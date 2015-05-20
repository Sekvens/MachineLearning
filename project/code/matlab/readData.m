
spampercent     = 0.3;

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
