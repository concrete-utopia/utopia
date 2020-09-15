module.exports = diags

var objToYaml = require('./obj-to-yaml.js')

function diags (extra) {
  var y = objToYaml(extra)
  if (y)
    y = '\n' + y

  return y
}
