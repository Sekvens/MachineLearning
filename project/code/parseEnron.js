

var fs = require('fs');

var re = 
    "^(to :|cc :|subject :|Subject:)" +
    "|[0-9]{2} : [0-9]{2} (am|pm)" +
    "|- - - -";


var options = { encoding: 'utf-8' };

fs.readFile('samples/file1.txt', options, function (err, data) {
  if (err) console.log(err);
  else {
    var processed = processString(data);
    fs.writeFile('samples/newfile1.txt', processed, options, 
      function (err, data) {
        if (err) console.log(err);
        else console.log('File saved!');
      }
    );
  }
});

var processString = function (str) {
  var lines = str.split("\n");
  var filtered = lines.filter(function (line) {
    return !line.match(re);
  });
  return filtered.join("\n");
};

