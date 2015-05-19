
% spam -> spam
scoreA = 0;

% ham -> ham
scoreB = 0;

% spam -> ham
scoreC = 0;

% ham -> spam
scoreD = 0;

% any -> correct
scoreE = 0;

% any -> false
scoreF = 0;

nhams = 0;
nspams = 0;

for idx = 1:numel(testoutput)
  element = testoutput(idx);
  if (element > 0.5)
    % network thinks pattern is spam
    if (testtargets(idx) > 0.5)
      % pattern is actually spam
      scoreA = scoreA + 1;
      scoreE = scoreE + 1;
      nspams = nspams + 1;
    else 
      % pattern is actually ham 
      scoreF = scoreF + 1;
      scoreD = scoreD + 1;
      nhams = nhams + 1;
    end
  else
    % network thinks pattern is ham
    if (testtargets(idx) <= 0.5)
      % pattern is actually ham 
      scoreB = scoreB + 1;
      scoreE = scoreE + 1;
      nhams = nhams + 1;
    else 
      % pattern is actually spam
      scoreC = scoreC + 1;
      scoreF = scoreF + 1;
      nspams = nspams + 1;
    end
  end
end

spamSuccessRatio = (scoreA / nspams);
hamSuccessRatio = (scoreB / nhams);
spamFailRatio = 1 - spamSuccessRatio;
hamFailRatio = 1 - hamSuccessRatio;
anySuccessRatio = (scoreE / testdimy);
anyFailRatio = 1 - anySuccessRatio;
