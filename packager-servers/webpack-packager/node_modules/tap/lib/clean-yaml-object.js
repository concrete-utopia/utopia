var cleanYamlObject = require('clean-yaml-object')
var path = require('path')
var Module = require('module')
var fs = require('fs')
var binpath = path.resolve(__dirname, '../bin')
var stack = require('./stack.js')
var Domain = require('domain').Domain

function hasOwn (obj, key) {
  return Object.prototype.hasOwnProperty.call(obj, key)
}

module.exports = cleanTapYamlObject

function cleanTapYamlObject (object) {
  if (hasOwn(object, 'stack') && !hasOwn(object, 'at'))
    object.at = stack.parseLine(object.stack.split('\n')[0])

  var file = object.at && object.at.file && path.resolve(object.at.file)
  if (file && (file.indexOf(__dirname) === 0 || file.indexOf(binpath) === 0))
    delete object.at

  if (object.at && object.at.file && object.at.line && !object.source) {
    var content
    file = path.resolve(object.at.file)
    try {
      content = Module.wrap(fs.readFileSync(file))
    } catch (er) {}
    if (content) {
      content = (content.split('\n')[object.at.line - 1] || '').trim()
      if (content)
        object.source = content + '\n'
    }
  }

  return cleanYamlObject(object, yamlFilter)
}

function yamlFilter (propertyName, isRoot, source, target) {
  if (source instanceof Domain)
    return false

  if (!isRoot)
    return true

  if (propertyName === 'stack') {
    if (source.stack)
      target.stack = source.stack
    return false
  }

  return !(propertyName === 'todo' ||
  propertyName === 'time' ||
  /^_?tapChild/.test(propertyName) ||
  /^tapStream/.test(propertyName) ||
  /^tapMochaTest/.test(propertyName) ||
  propertyName === 'cb' ||
  propertyName === 'name' ||
  propertyName === 'indent' ||
  propertyName === 'skip' ||
  propertyName === 'bail' ||
  propertyName === 'diagnostic' ||
  propertyName === 'buffered' ||
  propertyName === 'parent' ||
  propertyName === 'domainEmitter' ||
  propertyName === 'domainThrew' ||
  propertyName === 'domain' ||
  (propertyName === 'at' && !source.at))
}
