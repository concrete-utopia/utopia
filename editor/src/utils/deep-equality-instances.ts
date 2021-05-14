import {
  arrayDeepEquality,
  combine2EqualityCalls,
  combine4EqualityCalls,
  createCallFromEqualsFunction,
  createCallWithShallowEquals,
  createCallWithTripleEquals,
  KeepDeepEqualityCall,
  KeepDeepEqualityResult,
  keepDeepEqualityResult,
  mapKeepDeepEqualityResult,
} from './deep-equality'
import * as EP from '../core/shared/element-path'
import * as PP from '../core/shared/property-path'
import { HigherOrderControl } from '../components/canvas/canvas-types'
import { JSXElementName } from '../core/shared/element-template'
import { ElementPath, PropertyPath } from '../core/shared/project-file-types'
import { createCallFromIntrospectiveKeepDeep } from './react-performance'
import { Either, foldEither, isLeft, left, right } from '../core/shared/either'
import { NameAndIconResult } from '../components/inspector/common/name-and-icon-hook'

export const ElementPathKeepDeepEquality: KeepDeepEqualityCall<ElementPath> = createCallFromEqualsFunction(
  (oldPath: ElementPath, newPath: ElementPath) => {
    return EP.pathsEqual(oldPath, newPath)
  },
)

export const ElementPathArrayKeepDeepEquality: KeepDeepEqualityCall<Array<
  ElementPath
>> = arrayDeepEquality(ElementPathKeepDeepEquality)

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

export const NameAndIconResultKeepDeepEquality: KeepDeepEqualityCall<NameAndIconResult> = combine4EqualityCalls(
  (result) => result.path,
  ElementPathKeepDeepEquality,
  (result) => result.name,
  createCallWithTripleEquals(),
  (result) => result.label,
  createCallWithTripleEquals(),
  (result) => result.iconProps,
  createCallWithShallowEquals(),
  (path, name, label, iconProps) => {
    return {
      path: path,
      name: name,
      label: label,
      iconProps: iconProps,
    }
  },
)

export const NameAndIconResultArrayKeepDeepEquality: KeepDeepEqualityCall<Array<
  NameAndIconResult
>> = arrayDeepEquality(NameAndIconResultKeepDeepEquality)
