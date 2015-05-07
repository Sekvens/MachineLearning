
// Reads sourcedir, processes the files and copies them to destdir. destdir
// must exist
// Usage: parseenron.js [sourcedir] [destdir]


if (process.argv.length !== 4) {
  console.log('Usage: parseEnron [sourcedir] [destdir]');
} else {

  var sourcedir = process.argv[2];
  var destdir   = process.argv[3];
  var fs = require('fs');
  var async = require('async');
  var re = /^(to :|cc :|subject :|Subject:)|[0-9]{2} : [0-9]{2} (am|pm)|- - -/;
  var options = { encoding: 'utf-8' };

  var processString = function (str) {
    var lines = str.split("\n");
    var filtered = lines.filter(function (line) {
      return !line.match(re);
    });
    return filtered.join("\n");
  };

  fs.readdir(sourcedir, function (err, files) {
    if (err) console.log(err);
    else {
      var i = 0;
      async.whilst(
        function () { return i < files.length },
        function (callback) {
          var file = files[i]
          fs.readFile(sourcedir + '/' + file, options, function (err, data) {
            if (err) stop = true;
            else {
              var processed = processString(data);
              fs.writeFile(destdir + '/' + file, processed, options, 
                function (err, data) {
                  if (err) callback(err); 
                  else {
                    console.log('File: new' + file + ' has been written!');
                    i += 1;
                    callback();
                  }
                }
              );
            }
          });
        },
        function (err) {
          if (err) console.log(err);
        }
      );
    }
  });
}
