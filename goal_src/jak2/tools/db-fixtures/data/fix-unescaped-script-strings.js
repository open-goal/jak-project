const fs = require('fs');
let data = fs.readFileSync('./fixture-regions.json', 'utf8');

data = data.replace(/"((?:"[^"]*"|[^"])*?)"(?=[:},])(?=(?:"[^"]*"|[^"])*$)/gm, function (m, group) {
  return '"' + group.replace(/"/g, '\\"') + '"'
})

fs.writeFileSync('./fixture-regions-clean.json', data, 'utf8');
