var sourceMap = require('source-map'),
    SourceMapConsumer = sourceMap.SourceMapConsumer,
    SourceMapGenerator = sourceMap.SourceMapGenerator

module.exports = merge

/**
 * Merge old source map and new source map and return merged.
 * If old or new source map value is falsy, return another one as it is.
 *
 * @param {object|undefined} oldMap old source map object
 * @param {object|undefined} newmap new source map object
 * @return {object|undefined} merged source map object, or undefined when both old and new source map are undefined
 */
function merge(oldMap, newMap) {

  if (!oldMap)
    return newMap
  if (!newMap)
    return oldMap

  var oldMapConsumer = new SourceMapConsumer(oldMap),
      newMapConsumer = new SourceMapConsumer(newMap),
      mergedMapGenerator = new SourceMapGenerator()

  // iterate on new map and overwrite original position of new map with one of old map
  newMapConsumer.eachMapping(function(m) {

    // pass when `originalLine` is null.
    // It occurs in case that the node does not have origin in original code.
    if (m.originalLine == null)
      return

    var origPosInOldMap = oldMapConsumer.originalPositionFor({line: m.originalLine, column: m.originalColumn})
    if (origPosInOldMap.source == null)
      return

    mergedMapGenerator.addMapping({
      original: {
        line: origPosInOldMap.line,
        column: origPosInOldMap.column
      },
      generated: {
        line: m.generatedLine,
        column: m.generatedColumn
      },
      source: m.source,
      name: m.name
    })
  })

  var mergedMap = JSON.parse(mergedMapGenerator.toString())
  mergedMap.sources = oldMap.sources
  mergedMap.sourcesContent = oldMap.sourcesContent
  mergedMap.sourceRoot = oldMap.sourceRoot

  return mergedMap
}
