module.exports = objToYaml

var cleanYamlObject = require('./clean-yaml-object.js')
var yaml = require('js-yaml')

function objToYaml (obj) {
  obj = cleanYamlObject(obj)
  var y = ''
  if (obj && typeof obj === 'object' && Object.keys(obj).length) {
    y = yaml.safeDump(obj).split('\n').map(function (l) {
      return l.trim() ? '  ' + l : l.trim()
    }).join('\n')
    y = '  ---\n' + y + '  ...\n'
  }

  return y
}
