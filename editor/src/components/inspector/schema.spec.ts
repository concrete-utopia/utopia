import * as R from 'ramda'
import { JsonSchema } from '../../missing-types/json-schema'
import Utils from '../../utils/utils'
import { BaseWhitelist } from './integration-base-schema'
import { getDefaultValueForDefinition, getDefaultValueForPath, MainSchema } from './schema-stuff'

function makeSureAllLeavesHaveDefault(schema: JsonSchema) {
  return Utils.compileDefaultForSchema(schema, 'base', 'root', true, schema)
}

/**
 *
 * this is a util for us to export the schema for design/review purposes
 */
// eslint-disable-next-line @typescript-eslint/no-unused-vars
function generateSchemaTree(o: any, prefix?: string) {
  // KILLME? Kill this whole file?
  // eslint-disable-next-line no-param-reassign
  prefix = prefix || ''

  if (prefix === '.initial') return

  const path = {
    propertyElements: prefix.slice(1).split('.'),
  }

  let description = ''
  description = description.replace(/\n/g, ' ')

  switch (typeof o) {
    case 'object':
      if (Array.isArray(o)) return prefix + '\n'

      var output = prefix + '\t' + description + '\n'
      for (var k in o) {
        if (o.hasOwnProperty(k)) output += generateSchemaTree(o[k], prefix + '.' + k)
      }
      return output
    case 'function':
      return ''
    default:
      return prefix + '\t' + description + '\n'
  }
}

describe('Json Schema Types', () => {
  it('make sure all leafs have default values', async (done) => {
    try {
      expect(MainSchema).not.toBeNull()
      makeSureAllLeavesHaveDefault(MainSchema)
      done()
    } catch (error) {
      done(error)
    }
  })

  // it('make sure Definitions have no refs', async (done) => {
  //   try {
  //     makeSureAllLeavesHaveDefault(JustTheDefinitions)
  //     const definitionSchemaTree = generateSchemaTree(JustTheDefinitions)
  //     expect(definitionSchemaTree).not.toBeNull
  //     // TODO eventually roll in refs at compile time
  //     // expect(definitionSchemaTree).not.toContain('$ref')
  //     done()
  //   } catch (error) {
  //     done(error)
  //   }
  // })

  it('getDefaultValueForPath works', () => {
    const defaultPerspective = getDefaultValueForPath('transforms.perspective', 'base')
    expect(defaultPerspective).toEqual(600)
  })

  it('getDefaultValueForDefinition works', () => {
    const defaultPerspective = getDefaultValueForDefinition('Body.friction', 'base')
    expect(defaultPerspective).toEqual(0.1)
  })

  it('make sure each whitelist prop path have a corresponding schema', () => {
    const allRootPaths = R.uniq(R.flatten(R.values(BaseWhitelist)))
    allRootPaths.forEach((path) => {
      const pathArray = path.split('.')
      const foundSchema = Utils.traverseJsonSchema(pathArray, MainSchema)
      Utils.assert(`No JSON Schema found for '${path}'`, foundSchema != null)
    })
  })
})
