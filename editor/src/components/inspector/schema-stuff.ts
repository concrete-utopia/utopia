import * as Ajv from 'ajv'
import * as ObjectPath from 'object-path'
import * as R from 'ramda'
import { AnyJson } from '../../missing-types/json'
import { JsonSchema, PropSchema } from '../../missing-types/json-schema'
import Utils from '../../utils/utils'
import {
  BaseTemplateName,
  PackageType,
  PropertyPath,
  SvgTemplateName,
} from '../../core/shared/project-file-types'
import * as PP from '../../core/shared/property-path'
import { BaseDefinitions, BaseProperties, BaseWhitelist } from './integration-base-schema'

export type SchemaPropertyType = JsonSchema['type'] | string | null

const definitions = BaseDefinitions

const properties: PropSchema = BaseProperties

export const MainSchema: JsonSchema = {
  definitions: definitions,
  type: 'object',
  properties: properties,
}

export const JustTheDefinitions: JsonSchema = {
  properties: definitions,
}

export function getDefaultValueForPath(path: string, type: PackageType): any {
  return getValueInner(path, type, false)
}

export const lazyGetDefaultValueForPath = (path: string) => () => {
  return getDefaultValueForPath(path, 'base')
}

export const getValueOrDefaultFallbackForPath = (path: string, props: any): any => {
  const potentialValue = ObjectPath.get(props, path)
  return Utils.defaultIfNullLazy(potentialValue, () => {
    return getDefaultValueForPath(path, 'base')
  })
}

export function getDefaultValueForDefinition(path: string, type: PackageType): any {
  return getValueInner(path, type, true)
}

const GetValueInnerCache: { [key: string]: any } = {}

function getValueInner(path: string, type: PackageType, definition: boolean): any {
  const cacheKey = `${type}:${path}:${definition}`
  if (cacheKey in GetValueInnerCache) {
    return GetValueInnerCache[cacheKey]
  } else {
    const propertyPath = PP.fromString(path)
    const pathArray = propertyPath.propertyElements
    const baseSchema = definition ? JustTheDefinitions : MainSchema
    const foundSchema = Utils.traverseJsonSchema(pathArray, baseSchema)
    // TODO if the found schema has no defaults but it's children has, assemble a default on the fly
    if (foundSchema != null) {
      const schemaDefault = Utils.compileDefaultForSchema(
        foundSchema,
        type,
        PP.toString(propertyPath),
        false,
        MainSchema,
      )
      GetValueInnerCache[cacheKey] = schemaDefault
      return schemaDefault
    } else {
      throw new Error('Did not find Schema for ' + path)
    }
  }
}

export function getSchemaForPathWithSchema(
  propertyPath: PropertyPath,
  schema: JsonSchema,
): JsonSchema | null {
  const pathArray = PP.getElements(propertyPath)
  return Utils.traverseJsonSchema(pathArray, schema)
}

export function getSchemaForPath(propertyPath: PropertyPath): JsonSchema | null {
  return getSchemaForPathWithSchema(propertyPath, MainSchema)
}

export function getTypeForPathWithSchema(
  propertyPath: PropertyPath,
  schema: JsonSchema,
): SchemaPropertyType {
  const foundSchema = getSchemaForPathWithSchema(propertyPath, schema)
  if (foundSchema == null) {
    return null
  } else {
    if (foundSchema.$ref == null) {
      return foundSchema.type
    } else {
      return foundSchema.$ref
    }
  }
}

export function getTypeForPath(propertyPath: PropertyPath): SchemaPropertyType {
  return getTypeForPathWithSchema(propertyPath, MainSchema)
}

const ajv = new Ajv({ useDefaults: false })
ajv.addSchema(MainSchema, 'main')

export function verifyPropertyValueForPath(path: string, value: AnyJson): boolean {
  const pathArray = PP.fromString(path).propertyElements
  const foundSchema = Utils.traverseJsonSchema(pathArray, MainSchema)
  if (foundSchema != null) {
    const valid = ajv.validate(foundSchema, value)
    if (valid) {
      return true
    } else {
      console.error(`Validation errors for ${path} with value ${JSON.stringify(value)}`, ajv.errors)
    }
  }
  return false
}

export function getAllPossiblePropertyPathsForTemplate(
  baseType: PackageType,
  templateType: BaseTemplateName | SvgTemplateName,
): string[] {
  const propertiesForTemplate = BaseWhitelist[templateType]
  if (propertiesForTemplate != null) {
    const objectWithAllLegalDefaults = propertiesForTemplate.reduce((working, propertyPath) => {
      const path = PP.fromString(propertyPath).propertyElements
      const defaultForPath = getDefaultValueForPath(propertyPath, baseType)
      return R.assocPath(path, defaultForPath, working)
    }, {})
    return Utils.getAllObjectPaths(objectWithAllLegalDefaults).map((path) => path.join('.'))
  } else {
    console.error('Found an illegal base template type', templateType, baseType)
    return []
  }
}
