import {
  arrayDeepEquality,
  createCallFromEqualsFunction,
  KeepDeepEqualityCall,
  keepDeepEqualityResult,
  KeepDeepEqualityResult,
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

export const PropertyPathKeepDeepEquality: KeepDeepEqualityCall<PropertyPath> = createCallFromEqualsFunction(
  (oldPath: PropertyPath, newPath: PropertyPath) => {
    return PP.pathsEqual(oldPath, newPath)
  },
)

export function JSXElementNameKeepDeepEqualityCall(
  oldName: JSXElementName,
  newName: JSXElementName,
): KeepDeepEqualityResult<JSXElementName> {
  let areEqual: boolean = true

  let baseVariable: string
  if (oldName.baseVariable === newName.baseVariable) {
    baseVariable = oldName.baseVariable
  } else {
    areEqual = false
    baseVariable = newName.baseVariable
  }

  const propertyPathResult = PropertyPathKeepDeepEquality(
    oldName.propertyPath,
    newName.propertyPath,
  )
  const propertyPath = propertyPathResult.value
  areEqual = areEqual && propertyPathResult.areEqual

  if (areEqual) {
    return keepDeepEqualityResult(oldName, true)
  } else {
    const elementName: JSXElementName = {
      baseVariable: baseVariable,
      propertyPath: propertyPath,
    }
    return keepDeepEqualityResult(elementName, false)
  }
}

export const TemplatePathArrayKeepDeepEquality: KeepDeepEqualityCall<Array<
  TemplatePath
>> = arrayDeepEquality(TemplatePathKeepDeepEquality)

export const HigherOrderControlArrayKeepDeepEquality: KeepDeepEqualityCall<Array<
  HigherOrderControl
>> = arrayDeepEquality(createCallFromIntrospectiveKeepDeep())
