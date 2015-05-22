
% Extract numvectors from all files in a listing

function [ result ] = getNumVecs(listing, src, numvecdim)

  [ldimx, ldimy] = size(listing);
  result = zeros(numvecdim, ldimx);

  for idx = 1:numel(listing)
    path = strcat(src, listing(idx).name);
    if (strcmp(strcat(src, '.'), path) == 0 && ...
        strcmp(strcat(src,'..'), path) == 0)
      fid = fopen(path, 'r');
      if (fid == -1)
        disp(strcat('ERROR: invalid file path: ', path));
      else 
        numvec = fscanf(fid, '%f');
        [vdimx, vdimy] = size(numvec);
        if (vdimx ~= numvecdim)
          disp('ERROR: arrays does not match');
        else
          if (any(isnan(numvec(:))))
            disp(numvec);
            disp(path);
          end
          result(:,idx) = numvec;
          fclose(fid);
        end
      end
    end
  end
end

