
% Extract numvectors from all files in a listing

function [ result ] = getNumVecs(listing, src)

  [ldimx, ldimy] = size(listing);
  result = zeros(19, ldimx);

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
        if (vdimx ~= 19)
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

