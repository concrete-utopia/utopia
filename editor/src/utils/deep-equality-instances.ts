import {
  arrayDeepEquality,
  combine2EqualityCalls,
  createCallFromEqualsFunction,
  createCallWithTripleEquals,
  KeepDeepEqualityCall,
} from './deep-equality'
import * as TP from '../core/shared/template-path'
import * as PP from '../core/shared/property-path'
import { HigherOrderControl } from '../components/canvas/canvas-types'
import { JSXElementName } from '../core/shared/element-template'
import { TemplatePath, PropertyPath } from '../core/shared/project-file-types'
import { createCallFromIntrospectiveKeepDeep } from './react-performance'

export const TemplatePathKeepDeepEquality: KeepDeepEqualityCall<TemplatePath> = createCallFromEqualsFunction(
  (oldPath: TemplatePath, newPath: TemplatePath) => {
    return TP.pathsEqual(oldPath, newPath)
  },
)

export const TemplatePathArrayKeepDeepEquality: KeepDeepEqualityCall<Array<
  TemplatePath
>> = arrayDeepEquality(TemplatePathKeepDeepEquality)

export const PropertyPathKeepDeepEquality: KeepDeepEqualityCall<PropertyPath> = createCallFromEqualsFunction(
  (oldPath: PropertyPath, newPath: PropertyPath) => {
    return PP.pathsEqual(oldPath, newPath)
  },
)

export const HigherOrderControlArrayKeepDeepEquality: KeepDeepEqualityCall<Array<
  HigherOrderControl
>> = arrayDeepEquality(createCallFromIntrospectiveKeepDeep())

export function JSXElementNameKeepDeepEqualityCall(): KeepDeepEqualityCall<JSXElementName> {
  return combine2EqualityCalls(
    (name) => name.baseVariable,
    createCallWithTripleEquals(),
    (name) => name.propertyPath,
    PropertyPathKeepDeepEquality,
    (baseVariable, propertyPath) => {
      return {
        baseVariable: baseVariable,
        propertyPath: propertyPath,
      }
    },
  )
}
