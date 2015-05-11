
fid = fopen('../../data/numvecs.txt', 'r');
arr = fscanf(fid, '%f');

r = splitarr(arr);

disp(r);
