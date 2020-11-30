import {
  arrayDeepEquality,
  combine2EqualityCalls,
  createCallFromEqualsFunction,
  createCallWithTripleEquals,
  KeepDeepEqualityCall,
  KeepDeepEqualityResult,
  keepDeepEqualityResult,
  mapKeepDeepEqualityResult,
} from './deep-equality'
import * as TP from '../core/shared/template-path'
import * as PP from '../core/shared/property-path'
import { HigherOrderControl } from '../components/canvas/canvas-types'
import { JSXElementName } from '../core/shared/element-template'
import {
  TemplatePath,
  PropertyPath,
  InstancePath,
  ScenePath,
} from '../core/shared/project-file-types'
import { createCallFromIntrospectiveKeepDeep } from './react-performance'
import { Either, foldEither, isLeft, left, right } from '../core/shared/either'

export const TemplatePathKeepDeepEquality: KeepDeepEqualityCall<TemplatePath> = createCallFromEqualsFunction(
  (oldPath: TemplatePath, newPath: TemplatePath) => {
    return TP.pathsEqual(oldPath, newPath)
  },
)

export const TemplatePathArrayKeepDeepEquality: KeepDeepEqualityCall<Array<
  TemplatePath
>> = arrayDeepEquality(TemplatePathKeepDeepEquality)

export const InstancePathKeepDeepEquality: KeepDeepEqualityCall<InstancePath> = createCallFromEqualsFunction(
  (oldPath: InstancePath, newPath: InstancePath) => {
    return TP.pathsEqual(oldPath, newPath)
  },
)

export const InstancePathArrayKeepDeepEquality: KeepDeepEqualityCall<Array<
  InstancePath
>> = arrayDeepEquality(InstancePathKeepDeepEquality)

export const ScenePathKeepDeepEquality: KeepDeepEqualityCall<ScenePath> = createCallFromEqualsFunction(
  TP.scenePathsEqual,
)

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

export function EitherKeepDeepEquality<L, R>(
  leftDeep: KeepDeepEqualityCall<L>,
  rightDeep: KeepDeepEqualityCall<R>,
): KeepDeepEqualityCall<Either<L, R>> {
  type Result = KeepDeepEqualityResult<Either<L, R>>
  return (oldEither: Either<L, R>, newEither: Either<L, R>) => {
    return foldEither<L, R, Result>(
      (oldLeftValue) => {
        return foldEither<L, R, Result>(
          (newLeftValue) => {
            const leftDeepResult = leftDeep(oldLeftValue, newLeftValue)
            return mapKeepDeepEqualityResult<L, Either<L, R>>(left, leftDeepResult)
          },
          (_) => {
            return keepDeepEqualityResult(newEither, false)
          },
          newEither,
        )
      },
      (oldRightValue) => {
        return foldEither<L, R, Result>(
          (_) => {
            return keepDeepEqualityResult(newEither, false)
          },
          (newRightValue) => {
            const rightDeepResult = rightDeep(oldRightValue, newRightValue)
            return mapKeepDeepEqualityResult<R, Either<L, R>>(right, rightDeepResult)
          },
          newEither,
        )
      },
      oldEither,
    )
  }
}
