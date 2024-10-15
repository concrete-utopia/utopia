import chroma from 'chroma-js'
import { v4 as UUID } from 'uuid'
import type { PackageType } from '../core/shared/project-file-types'
import type { AnyJson, JsonMap } from '../missing-types/json'
import type { JsonSchema, PropSchema } from '../missing-types/json-schema'
import type { ControlStyles } from '../components/inspector/common/control-styles'
import type { NormalisedFrame } from 'utopia-api/core'
import { fastForEach, NO_OP } from '../core/shared/utils'
import type {
  CanvasRectangle,
  SimpleRectangle,
  CoordinateMarker,
  Rectangle,
  LocalRectangle,
} from '../core/shared/math-utils'
import {
  roundTo,
  normalizeDegrees,
  degreesToRadians,
  radiansToDegrees,
  scalePoint,
  scaleVector,
  rotateVector,
  zeroPoint,
  zeroSize,
  zeroRectangle,
  zeroRectangleAtPoint,
  shiftToOrigin,
  rectOrigin,
  rectContainsPoint,
  circleContainsPoint,
  ellipseContainsPoint,
  negate,
  magnitude,
  product,
  distance,
  vectorFromPoints,
  interpolateAt,
  offsetPoint,
  normalizeRect,
  getLocalRectangleInNewParentContext,
  getLocalPointInNewParentContext,
  getCanvasRectangleWithCanvasOffset,
  getCanvasPointWithCanvasOffset,
  getCanvasVectorFromWindowVector,
  asLocal,
  asGlobal,
  rectangleDifference,
  rectangleIntersection,
  pointDifference,
  vectorDifference,
  offsetRect,
  combineRectangles,
  stretchRect,
  scaleSize,
  scaleRect,
  getRectCenter,
  setRectLeftX,
  setRectCenterX,
  setRectRightX,
  setRectTopY,
  setRectCenterY,
  setRectBottomY,
  setRectWidth,
  setRectHeight,
  rectFromPointVector,
  rectSizeToVector,
  transformFrameUsingBoundingBox,
  closestPointOnLine,
  lineIntersection,
  percentToNumber,
  numberToPercent,
  rectangleToPoints,
  boundingRectangle,
  boundingRectangleArray,
  angleOfPointFromVertical,
  pointsEqual,
  rectanglesEqual,
  proportion,
  rect,
  point,
  sizesEqual,
  forceNotNaN,
  nanToZero,
  safeParseInt,
} from '../core/shared/math-utils'
import {
  optionalMap,
  optionalFlatMap,
  maybeToArray,
  arrayToMaybe,
  defaultIfNull,
  defaultIfNullLazy,
  forceNotNull,
} from '../core/shared/optional-utils'
import {
  propOrNull,
  objectMap,
  mapValues,
  get,
  propOr,
  copyObjectWithoutFunctions,
  getAllObjectPaths,
  modifyKey,
  mergeObjects,
  objectValues,
  isEmptyObject,
  objectFlattenKeys,
} from '../core/shared/object-utils'
import {
  stripNulls,
  filterDuplicates,
  flatMapArray,
  pluck,
  traverseArray,
  arrayToObject,
  mapArrayToDictionary,
  uniq,
  dropLast,
  last,
  removeIndexFromArray,
  flattenArray,
  addToMapOfArraysUnique,
  addUniquely,
  addAllUniquely,
  findLastIndex,
  drop,
  insert,
  insertMultiple,
} from '../core/shared/array-utils'
import {
  shallowEqual,
  oneLevelNestedEquals,
  keepReferenceIfShallowEqual,
} from '../core/shared/equality-utils'
import {
  safeFunction,
  SafeFunctionCurriedErrorHandler,
  processErrorWithSourceMap,
} from '../core/shared/code-exec-utils'
import type { ValueType, OptionsType, OptionTypeBase } from 'react-select'
import { emptySet } from '../core/shared/set-utils'
import * as ObjectPath from 'object-path'
// TODO Remove re-exported functions

export type FilteredFields<Base, T> = {
  [K in keyof Base]: Base[K] extends T ? K : never
}[keyof Base]

export interface Front {
  type: 'front'
}

export function front(): Front {
  return {
    type: 'front',
  }
}

export interface Back {
  type: 'back'
}

export function back(): Back {
  return {
    type: 'back',
  }
}

export interface Absolute {
  type: 'absolute'
  index: number
}

export function absolute(index: number): Absolute {
  return {
    type: 'absolute',
    index: index,
  }
}

export interface After {
  type: 'after'
  index: number
}

export function after(index: number): After {
  return {
    type: 'after',
    index: index,
  }
}

export interface Before {
  type: 'before'
  index: number
}

export function before(index: number): Before {
  return {
    type: 'before',
    index: index,
  }
}

export type IndexPosition = Front | Back | Absolute | After | Before

export function shiftIndexPositionForRemovedElement(
  indexPosition: IndexPosition,
  removedElementIndex: number,
): IndexPosition {
  switch (indexPosition.type) {
    case 'front':
    case 'back':
    case 'absolute':
      return indexPosition
    case 'before':
      if (removedElementIndex < indexPosition.index) {
        return before(indexPosition.index - 1)
      } else {
        return indexPosition
      }
    case 'after':
      if (removedElementIndex < indexPosition.index) {
        return after(indexPosition.index - 1)
      } else {
        return indexPosition
      }
    default:
      const _exhaustiveCheck: never = indexPosition
      throw new Error(`Unhandled index position ${JSON.stringify(indexPosition)}`)
  }
}

export type Axis = 'x' | 'y'

export type DiagonalAxis = 'TopLeftToBottomRight' | 'BottomLeftToTopRight'

export interface HSLA {
  h: number
  s: number
  l: number
  a: number
}

export type Color = { red: number; green: number; blue: number; alpha: number }

export type ColorProperty = {
  enabled: boolean
  color: Color
}

export function normalisedFrameToCanvasFrame(frame: NormalisedFrame): CanvasRectangle {
  const { left: x, top: y, width, height } = frame
  return { x, y, width, height } as CanvasRectangle
}

export type ObtainChildren<T> = (elem: T, parents: Array<T>) => Array<T>

export type GetFrame<T> = (elem: T, parents: Array<T>) => SimpleRectangle

export type HitTester<T> = {
  elem: T
  parents: Array<T>
  frame: SimpleRectangle
}

export type Clock = {
  // returns the current timestamp
  now: () => number
}

export const getChainSegmentEdge = (controlStyles: ControlStyles) => ({
  top: `0 1px ${controlStyles.borderColor} inset`,
  bottom: `0 -1px ${controlStyles.borderColor} inset`,
  left: `1px 0 ${controlStyles.borderColor} inset`,
  right: `-1px 0 ${controlStyles.borderColor} inset`,
})

export function generateUUID(): string {
  return UUID().replace(/-/g, '_')
}

function isColor(color: any): color is Color {
  if (color == null) {
    return false
  } else {
    return (
      (color as Color).red !== undefined &&
      (color as Color).blue !== undefined &&
      (color as Color).green !== undefined &&
      (color as Color).alpha !== undefined
    )
  }
}

function safeColor(color: Color): Color {
  return {
    red: propOrNull('red', color) == null ? 0 : color.red,
    green: propOrNull('green', color) == null ? 0 : color.green,
    blue: propOrNull('blue', color) == null ? 0 : color.blue,
    alpha: propOrNull('alpha', color) == null ? 1 : color.alpha,
  }
}

function colorToRGBA(unsafe: Color): string {
  const color = safeColor(unsafe)
  return (
    'rgba(' +
    Math.round(color.red) +
    ',' +
    Math.round(color.green) +
    ',' +
    Math.round(color.blue) +
    ',' +
    color.alpha +
    ')'
  )
}

function colorToHex(unsafe: Color): string {
  const color = safeColor(unsafe)
  return chroma(color.red, color.green, color.blue).hex().toUpperCase()
}

function colorToRGBAWithoutOpacity(unsafe: Color): string {
  const color = safeColor(unsafe)
  return (
    'rgba(' +
    Math.round(color.red) +
    ',' +
    Math.round(color.green) +
    ',' +
    Math.round(color.blue) +
    ',1)'
  )
}

function colorToReactNativeColor(color: Color | null, defaultToBlack: boolean = false): string {
  if (color == null || (color.red == null && color.blue == null && color.green == null)) {
    return defaultToBlack ? 'rgba(0,0,0,1)' : 'transparent'
  }

  return colorToRGBA(color)
}

function nullIfTransparent(color: Color | null): Color | null {
  if (color == null) {
    return null
  }
  if (color.alpha == 0) {
    return null
  }
  return color
}

const TRANSPARENT_IMAGE_SRC =
  'data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw=='

function isJsonStringValid(jsonString: string): boolean {
  try {
    JSON.parse(jsonString)
  } catch (e) {
    return false
  }
  return true
}

function jsonParseOrNull(jsonString: string): any | null {
  try {
    return JSON.parse(jsonString)
  } catch (e) {
    return null
  }
}

function safeCompare<T>(left: T | null | undefined, right: T, fn: (l: T, r: T) => T): T {
  if (left === null || left === undefined) {
    return right
  } else {
    return fn(left, right)
  }
}

function removeFirst<T>(array: Array<T>): Array<T> {
  return drop(1, array)
}

function resolveRef($ref: string, completeSchema: JsonSchema): JsonSchema {
  if ($ref.startsWith('main#/definitions/')) {
    // this ONLY works with top level definitions
    // TODO make it work with deep refs
    const definitionName = $ref.slice(18)
    return completeSchema.definitions![definitionName]!
  } else {
    throw new Error(
      'using a $ref which does not point to main#/definitions/ is not yet allowed: ' + $ref,
    )
  }
}

function traverseJsonSchema(
  schemaPath: Array<string | number>,
  schema: JsonSchema,
  completeSchema?: JsonSchema,
): JsonSchema | null {
  const schemaToUse = completeSchema ?? schema

  const firstSchemaPath = schemaPath[0]
  if (firstSchemaPath === undefined) {
    return schema
  }
  if (schema.type === 'array' && schema.items != null) {
    return traverseJsonSchema(removeFirst(schemaPath), schema.items, schemaToUse)
  }
  if (schema.properties != null) {
    const prop = schema.properties[firstSchemaPath]
    if (prop == null) {
      return null
    } else {
      return traverseJsonSchema(removeFirst(schemaPath), prop, schemaToUse)
    }
  }
  if (schema.$ref != null) {
    return traverseJsonSchema(schemaPath, resolveRef(schema.$ref, schemaToUse), schemaToUse)
  }

  return null
}

function extractDefaultForType(schema: JsonSchema, type: PackageType): AnyJson | undefined {
  if (type === 'svg' && schema.defaultSpecialForSvg != null) {
    return schema.defaultSpecialForSvg
  } else {
    return schema.default
  }
}

function compileDefaultForSchema(
  schema: JsonSchema,
  type: PackageType,
  debugKey: string | number,
  preferLeafDefault: boolean,
  completeSchema: JsonSchema,
): AnyJson {
  const defaultForNode = extractDefaultForType(schema, type)
  if (!preferLeafDefault && defaultForNode !== undefined) {
    return defaultForNode
  }
  if (schema.type === 'array' && schema.items != null) {
    /**
     * The requested property path points to an array,
     * which in our implementation is an object with numbers as keys.
     * So an array of one element is of the shape { 0: ... }
     */
    return {
      0: compileDefaultForSchema(schema.items, type, debugKey, preferLeafDefault, completeSchema),
    }
  }

  if (schema.allOf != null) {
    let result: JsonMap = {}
    fastForEach(schema.allOf, (elementOfAll) => {
      const elementSchema = compileDefaultForSchema(
        elementOfAll,
        type,
        debugKey,
        preferLeafDefault,
        completeSchema,
      )
      if (
        typeof elementSchema === 'object' &&
        elementSchema != null &&
        !Array.isArray(elementSchema)
      ) {
        result = {
          ...result,
          ...elementSchema,
        }
      } else {
        throw new Error(`Impossible to merge schema element ${elementSchema} into object.`)
      }
    })
    return result
  }

  if (schema.properties != null) {
    let result: { [property: string]: AnyJson } = {}
    const properties = schema.properties
    for (const [key, property] of Object.entries(properties)) {
      result[key] = compileDefaultForSchema(
        property,
        type,
        `${debugKey}.${key}`,
        preferLeafDefault,
        completeSchema,
      )
    }
    return result
  }

  if (schema.$ref != null) {
    return compileDefaultForSchema(
      resolveRef(schema.$ref, completeSchema),
      type,
      debugKey,
      preferLeafDefault,
      completeSchema,
    )
  }

  if (preferLeafDefault && defaultForNode !== undefined) {
    return defaultForNode
  }

  // oh no, we don't have a default for this!
  throw new Error(`Schema Error: couldn't find default value for type '${debugKey}'`)
}

function compileDefaultForPropSchema(propSchema: PropSchema): any {
  return objectMap((schema, key) => {
    try {
      return compileDefaultForSchema(schema, 'base', key, false, schema)
    } catch (e) {
      // we couldn't find a default
      return null
    }
  }, propSchema)
}

export function eventTargetIsTextArea(event: KeyboardEvent): boolean {
  return /textarea/i.test((event.target as any).tagName)
}
export function eventTargetIsTextAreaOrInput(event: KeyboardEvent) {
  return /input|textarea/i.test((event.target as any).tagName)
}

function getBaseAndIndex(name: string, insertSpace: boolean): { base: string; index: number } {
  let tokens = name.split(' ')
  let lastToken: string = ''
  let baseName = name
  if (insertSpace) {
    lastToken = forceNotNull('Token should exist.', tokens[tokens.length - 1])
    baseName = dropLast(tokens).join(' ')
  } else {
    const regexMatch = name.match(/\d+$/)
    if (regexMatch != null) {
      lastToken = forceNotNull('First regex match should exist.', regexMatch[0])
      baseName = name.slice(0, regexMatch.index)
      tokens = [baseName, lastToken]
    }
  }

  const indexToken = parseInt(lastToken)

  if (tokens.length == 0 || isNaN(indexToken)) {
    return {
      base: name,
      index: 1,
    }
  } else {
    return {
      base: baseName,
      index: indexToken,
    }
  }
}

export function nextName(
  candidate: string,
  namesTaken: Array<string>,
  insertSpace: boolean = true,
): string {
  const { base } = getBaseAndIndex(candidate, insertSpace)
  const maxIndex = namesTaken
    .filter((name) => {
      return name.indexOf(base) >= 0
    })
    .reduce((acc, name) => {
      const { index } = getBaseAndIndex(name, insertSpace)
      return index > acc ? index : acc
    }, 0)
  return `${base}${insertSpace ? ' ' : ''}${maxIndex + 1}`
}

// Treats "front" as if everything in the array was in a stack from back to front.
function indexToInsertAt<T>(array: Array<T>, indexPosition: IndexPosition): number {
  switch (indexPosition.type) {
    case 'front':
      return array.length
    case 'back':
      return 0
    case 'absolute':
      return indexPosition.index
    case 'after':
      return Math.min(indexPosition.index + 1, array.length)
    case 'before':
      return Math.max(indexPosition.index, 0)
  }
}

function addToArrayWithFill<T>(
  element: T,
  array: Array<T>,
  atPosition: IndexPosition,
  fillValue: () => T,
): Array<T> {
  const index = indexToInsertAt(array, atPosition)
  let midResult: Array<T> = [...array]
  for (let fillIndex = array.length; fillIndex < index; fillIndex++) {
    midResult.push(fillValue())
  }
  return insert(index, element, midResult)
}

export function addToArrayAtIndexPosition<T>(
  element: T,
  array: Array<T>,
  atPosition: IndexPosition,
): Array<T> {
  const index = indexToInsertAt(array, atPosition)
  return insert(index, element, array)
}

export function addElementsToArrayAtIndexPosition<T>(
  elements: Array<T>,
  array: Array<T>,
  atPosition: IndexPosition,
): Array<T> {
  const index = indexToInsertAt(array, atPosition)
  return insertMultiple(index, elements, array)
}

function assert(errorMessage: string, predicate: boolean | (() => boolean)): void {
  if (typeof predicate === 'function' && predicate()) {
    return
  }
  if (typeof predicate === 'boolean' && predicate) {
    return
  }
  throw new Error(`Assert failed: ${errorMessage}`)
}

export interface Annotations {
  _signature: string
  _description: string
  _properties: { [key: string]: any & Annotations }
}

function withDoc<T>(
  value: T,
  signature: string,
  doc: string,
  properties: { [key: string]: any & Annotations } = {},
): T & Annotations {
  var annotated: any = value
  annotated._signature = signature
  annotated._description = doc
  annotated._properties = properties
  return annotated
}

function shallowClone(value: any): any {
  switch (typeof value) {
    case 'object':
      if (Array.isArray(value)) {
        return [...value]
      } else {
        return { ...value }
      }
    default:
      return value
  }
}

function proxyValue(
  valueToProxy: any,
  recordAssignment: (p: Array<string>, value: any) => any,
): any {
  function wrapInProxy(toWrap: any, objPath: Array<string>): any {
    if (typeof toWrap == 'object' && toWrap != null) {
      let withWrappedChildren: any
      switch (typeof toWrap) {
        case 'object':
          if (Array.isArray(toWrap)) {
            withWrappedChildren = toWrap.map((element: any, index: number) =>
              wrapInProxy(element, objPath.concat([`${index}`])),
            )
          } else {
            withWrappedChildren = mapValues(
              (element: any, key: string) => wrapInProxy(element, objPath.concat([key])),
              toWrap,
            )
          }
          break
        default:
          withWrappedChildren = toWrap
      }

      return new Proxy(withWrappedChildren, {
        set: function (target: any, property: any, value: any, receiver: any): boolean {
          if (typeof value === 'symbol') {
            target[property] = value
          } else {
            const innerPath = objPath.concat([property])
            const innerValue = wrapInProxy(value, innerPath)
            target[property] = innerValue
            recordAssignment(innerPath, value)
          }
          return true
        },
      })
    } else {
      return toWrap
    }
  }

  return wrapInProxy(valueToProxy, [])
}

const createSimpleClock = function (): Clock {
  return {
    now: () => {
      return Date.now()
    },
  }
}

function stepInArray<T>(
  eq: (first: T, second: T) => boolean,
  step: number,
  array: Array<T>,
  stepFrom: T,
): T | null {
  let workingIndex = 0
  let foundIndex: number | null = null
  for (const element of array) {
    // Check if this is the element we're looking for.
    if (eq(element, stepFrom)) {
      foundIndex = workingIndex
      // Might as well bail out early.
      break
    }
    workingIndex++
  }
  if (foundIndex === null) {
    return null
  } else {
    let index: number = foundIndex + step
    function tryNextStep(): void {
      if (index < 0 || index >= array.length) {
        index = step >= 0 ? index - array.length : index + array.length
        tryNextStep()
      }
    }
    tryNextStep()
    return forceNotNull(`No element at index ${index}`, array[index])
  }
}

function increaseScale(scale: number): number {
  return scale * 2
}

function decreaseScale(scale: number): number {
  return scale / 2
}

function createThrottler(
  timeout: number,
): (functionToCall: (...args: any[]) => void, ...args: any[]) => void {
  let canCallFunction = true
  return (functionToCall: (...args: any[]) => void, ...args: any[]) => {
    if (canCallFunction) {
      // eslint-disable-next-line prefer-spread
      functionToCall.apply(null, args)
      canCallFunction = false
      setTimeout(() => {
        canCallFunction = true
      }, timeout)
    }
  }
}

function path<T>(
  objPath: Array<string | number>,
  obj: Record<string | number, any> | undefined | null,
): T | undefined {
  if (obj == null) {
    return undefined
  } else {
    return ObjectPath.get(obj, objPath)
  }
}

// eslint-disable-next-line @typescript-eslint/ban-types
function pathOr<T, U = T>(defaultValue: T, objPath: Array<string | number>, obj: {}): T | U {
  return ObjectPath.get(obj, objPath, defaultValue)
}

function immutableUpdateField(valueToUpdate: any, field: string | number, valueToSet: any): any {
  const fieldParsedAsNumber: number = typeof field === 'number' ? field : parseInt(field)
  if (isNaN(fieldParsedAsNumber)) {
    // Object field update.
    return {
      ...defaultIfNull({}, valueToUpdate),
      [field]: valueToSet,
    }
  } else {
    let result: Array<any> = valueToUpdate == null ? [] : [...valueToUpdate]
    result[fieldParsedAsNumber] = valueToSet
    return result
  }
}

function immutableUpdate(
  valueToUpdate: any,
  objPath: Array<string | number>,
  valueToSet: any,
): any {
  const first = objPath[0]
  if (first === undefined) {
    // No path, so we're just replacing the whole value at this point.
    return valueToSet
  } else if (objPath.length === 1) {
    // Last part of the path, setting the `valueToSet` where the final part specifies.
    return immutableUpdateField(valueToUpdate, first, valueToSet)
  } else {
    // 2 or more path elements, need to step down path part to recursively invoke this on the remainder.
    const remainder = objPath.slice(1)
    const fieldParsedAsNumber: number = typeof first === 'number' ? first : parseInt(first)
    const isArrayUpdate = typeof first === 'number' || !isNaN(fieldParsedAsNumber)
    if (isArrayUpdate) {
      // Arrays.
      const defaultedArray: Array<any> = defaultIfNull([], valueToUpdate)
      let result: Array<any> = [...defaultedArray]
      result[fieldParsedAsNumber] = immutableUpdate(
        defaultedArray[fieldParsedAsNumber],
        remainder,
        valueToSet,
      )
      return result
    } else {
      // Objects.
      const defaultedObject: { [key: string]: any } = defaultIfNull({}, valueToUpdate)
      return {
        ...defaultedObject,
        [first]: immutableUpdate(defaultedObject[first], remainder, valueToSet),
      }
    }
  }
}

function isLocalRectangle(rectangle: any): rectangle is LocalRectangle {
  if (typeof rectangle === 'object') {
    return (
      rectangle.x != null &&
      typeof rectangle.x === 'number' &&
      rectangle.y != null &&
      typeof rectangle.y === 'number' &&
      rectangle.width != null &&
      typeof rectangle.width === 'number' &&
      rectangle.height != null &&
      typeof rectangle.height === 'number'
    )
  } else {
    return false
  }
}

function update<T>(index: number, newValue: T, array: Array<T>): Array<T> {
  let result = [...array]
  result.splice(index, 1, newValue)
  return result
}

export type Defer<T> = Promise<T> & {
  resolve: (value?: T) => void
  reject: (reason?: any) => void
}

export function defer<T>(): Defer<T> {
  var res, rej

  var promise = new Promise<T>((resolve, reject) => {
    res = resolve
    rej = reject
  })
  // eslint-disable-next-line @typescript-eslint/no-floating-promises
  Object.defineProperty(promise, 'resolve', { value: res })
  // eslint-disable-next-line @typescript-eslint/no-floating-promises
  Object.defineProperty(promise, 'reject', { value: rej })

  return promise as any
}

function timeLimitPromise<T>(promise: Promise<T>, limitms: number, message: string): Promise<T> {
  const timeoutPromise: Promise<any> = new Promise((resolve, reject) => {
    const timeoutID = setTimeout(() => {
      clearTimeout(timeoutID)
      reject(message)
    }, limitms)
  })
  return Promise.race([promise, timeoutPromise])
}

/**
 * A type guard for React Select's onChange values, which can either be a value
 * or an array of values.
 */
export function isOptionType<T extends OptionTypeBase>(value: ValueType<T>): value is T {
  return value != null && !Array.isArray(value)
}

/**
 * A type guard for React Select's onChange values, which can either be a value
 * or an array of values.
 */
export function isOptionsType<T extends OptionTypeBase>(
  value: ValueType<T>,
): value is OptionsType<T> {
  return Array.isArray(value)
}

function deduplicateBy<T>(key: (t: T) => string, ts: Array<T>): Array<T> {
  const seen = new Set<string>()
  const results: Array<T> = []
  for (const t of ts) {
    const k = key(t)
    if (!seen.has(k)) {
      results.push(t)
      seen.add(k)
    }
  }
  return results
}

// https://github.com/gregberge/react-merge-refs/blob/main/src/index.tsx
export function mergeRefs<T = any>(
  refs: Array<React.MutableRefObject<T> | React.LegacyRef<T> | undefined | null>,
): React.RefCallback<T> {
  return (value) => {
    refs.forEach((ref) => {
      if (typeof ref === 'function') {
        ref(value)
      } else if (ref != null) {
        ;(ref as React.MutableRefObject<T | null>).current = value
      }
    })
  }
}

export default {
  generateUUID: generateUUID,
  assert: assert,
  roundTo: roundTo,
  normalizeDegrees: normalizeDegrees,
  degreesToRadians: degreesToRadians,
  radiansToDegrees: radiansToDegrees,
  scalePoint: scalePoint,
  scaleVector: scaleVector,
  rotateVector: rotateVector,
  zeroPoint: zeroPoint,
  zeroSize: zeroSize,
  zeroRectangle: zeroRectangle,
  zeroRectangleAtPoint: zeroRectangleAtPoint,
  shiftToOrigin: shiftToOrigin,
  rectOrigin: rectOrigin,
  rectContainsPoint: rectContainsPoint,
  circleContainsPoint: circleContainsPoint,
  ellipseContainsPoint: ellipseContainsPoint,
  negate: negate,
  magnitude: magnitude,
  product: product,
  distance: distance,
  vectorFromPoints: vectorFromPoints,
  interpolateAt: interpolateAt,
  offsetPoint: offsetPoint,
  addVectors: offsetPoint,
  normalizeRect: normalizeRect,
  getLocalRectangleInNewParentContext: getLocalRectangleInNewParentContext,
  getLocalPointInNewParentContext: getLocalPointInNewParentContext,
  getCanvasRectangleWithCanvasOffset: getCanvasRectangleWithCanvasOffset,
  getCanvasPointWithCanvasOffset: getCanvasPointWithCanvasOffset,
  getCanvasVectorFromWindowVector: getCanvasVectorFromWindowVector,
  asLocal: asLocal,
  asGlobal: asGlobal,
  rectangleDifference: rectangleDifference,
  rectangleIntersection: rectangleIntersection,
  pointDifference: pointDifference,
  vectorDifference: vectorDifference,
  offsetRect: offsetRect,
  combineRectangles: combineRectangles,
  stretchRect: stretchRect,
  scaleSize: scaleSize,
  scaleRect: scaleRect,
  getRectCenter: getRectCenter,
  setRectLeftX: setRectLeftX,
  setRectCenterX: setRectCenterX,
  setRectRightX: setRectRightX,
  setRectTopY: setRectTopY,
  setRectCenterY: setRectCenterY,
  setRectBottomY: setRectBottomY,
  setRectWidth: setRectWidth,
  setRectHeight: setRectHeight,
  rectFromPointVector: rectFromPointVector,
  rectSizeToVector: rectSizeToVector,
  transformFrameUsingBoundingBox: transformFrameUsingBoundingBox,
  closestPointOnLine: closestPointOnLine,
  lineIntersection: lineIntersection,
  isColor: isColor,
  safeColor: safeColor,
  colorToRGBA: colorToRGBA,
  colorToHex: colorToHex,
  colorToRGBAWithoutOpacity: colorToRGBAWithoutOpacity,
  colorToReactNativeColor: colorToReactNativeColor,
  nullIfTransparent: nullIfTransparent,
  SafeFunction: safeFunction,
  SafeFunctionCurriedErrorHandler: SafeFunctionCurriedErrorHandler,
  TRANSPARENT_IMAGE_SRC: TRANSPARENT_IMAGE_SRC,
  get: get,
  isJsonStringValid: isJsonStringValid,
  safeCompare: safeCompare,
  optionalMap: optionalMap,
  optionalFlatMap: optionalFlatMap,
  stripNulls: stripNulls,
  filterDuplicates: filterDuplicates,
  propOr: propOr,
  propOrNull: propOrNull,
  objectMap: objectMap,
  removeFirst: removeFirst,
  resolveRef: resolveRef,
  traverseJsonSchema: traverseJsonSchema,
  compileDefaultForSchema: compileDefaultForSchema,
  compileDefaultForPropSchema: compileDefaultForPropSchema,
  eventTargetIsTextAreaOrInput: eventTargetIsTextAreaOrInput,
  copyObjectWithoutFunctions: copyObjectWithoutFunctions,
  forceNotNull: forceNotNull,
  eventTargetIsTextArea: eventTargetIsTextArea,
  nextName: nextName,
  indexToInsertAt: indexToInsertAt,
  addToArrayWithFill: addToArrayWithFill,
  percentToNumber: percentToNumber,
  numberToPercent: numberToPercent,
  getAllObjectPaths: getAllObjectPaths,
  mapValues: mapValues,
  withDoc: withDoc,
  shallowClone: shallowClone,
  proxyValue: proxyValue,
  createSimpleClock: createSimpleClock,
  shallowEqual: shallowEqual,
  oneLevelNestedEquals: oneLevelNestedEquals,
  keepReferenceIfShallowEqual: keepReferenceIfShallowEqual,
  maybeToArray: maybeToArray,
  arrayToMaybe: arrayToMaybe,
  rectangleToPoints: rectangleToPoints,
  flatMapArray: flatMapArray,
  boundingRectangle: boundingRectangle,
  boundingRectangleArray: boundingRectangleArray,
  pluck: pluck,
  traverseArray: traverseArray,
  NO_OP: NO_OP,
  stepInArray: stepInArray,
  arrayToObject: arrayToObject,
  angleOfPointFromVertical: angleOfPointFromVertical,
  pointsEqual: pointsEqual,
  rectanglesEqual: rectanglesEqual,
  proportion: proportion,
  increaseScale: increaseScale,
  decreaseScale: decreaseScale,
  createThrottler: createThrottler,
  mapArrayToDictionary: mapArrayToDictionary,
  modifyKey: modifyKey,
  uniq: uniq,
  fastForEach: fastForEach,
  forceNotNaN: forceNotNaN,
  nanToZero: nanToZero,
  safeParseInt: safeParseInt,
  defaultIfNull: defaultIfNull,
  defaultIfNullLazy: defaultIfNullLazy,
  emptySet: emptySet,
  path: path,
  pathOr: pathOr,
  mergeObjects: mergeObjects,
  objectValues: objectValues,
  rect: rect,
  point: point,
  dropLast: dropLast,
  last: last,
  immutableUpdate: immutableUpdate,
  isEmptyObject: isEmptyObject,
  sizesEqual: sizesEqual,
  objectFlattenKeys: objectFlattenKeys,
  jsonParseOrNull: jsonParseOrNull,
  isLocalRectangle: isLocalRectangle,
  removeIndexFromArray: removeIndexFromArray,
  flattenArray: flattenArray,
  update: update,
  addToMapOfArraysUnique: addToMapOfArraysUnique,
  addUniquely: addUniquely,
  addAllUniquely: addAllUniquely,
  defer: defer,
  processErrorWithSourceMap: processErrorWithSourceMap,
  findLastIndex: findLastIndex,
  timeLimitPromise: timeLimitPromise,
  deduplicateBy: deduplicateBy,
}
