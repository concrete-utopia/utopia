import { defaultEither } from '../shared/either'
import type { JSXAttributes } from '../shared/element-template'
import { unsetJSXValuesAtPaths } from '../shared/jsx-attributes'
import type { PropertyPath } from '../shared/project-file-types'
import * as PP from '../shared/property-path'

export const MarginPropertyPaths: Array<PropertyPath> = [
  PP.create('style', 'margin'),
  PP.create('style', 'margin-block'),
  PP.create('style', 'margin-block-end'),
  PP.create('style', 'margin-block-start'),
  PP.create('style', 'margin-bottom'),
  PP.create('style', 'margin-inline'),
  PP.create('style', 'margin-inline-end'),
  PP.create('style', 'margin-inline-start'),
  PP.create('style', 'margin-left'),
  PP.create('style', 'margin-right'),
  PP.create('style', 'margin-top'),
  PP.create('style', 'margin-trim'),
]

export const PaddingPropertyPaths: Array<PropertyPath> = [
  PP.create('style', 'padding'),
  PP.create('style', 'padding-block'),
  PP.create('style', 'padding-block-end'),
  PP.create('style', 'padding-block-start'),
  PP.create('style', 'padding-bottom'),
  PP.create('style', 'padding-inline'),
  PP.create('style', 'padding-inline-end'),
  PP.create('style', 'padding-inline-start'),
  PP.create('style', 'padding-left'),
  PP.create('style', 'padding-right'),
  PP.create('style', 'padding-top'),
]

export function removePaddingProperties(jsxAttributes: JSXAttributes): JSXAttributes {
  return defaultEither(jsxAttributes, unsetJSXValuesAtPaths(jsxAttributes, PaddingPropertyPaths))
}

export function removeMarginProperties(jsxAttributes: JSXAttributes): JSXAttributes {
  return defaultEither(jsxAttributes, unsetJSXValuesAtPaths(jsxAttributes, MarginPropertyPaths))
}
