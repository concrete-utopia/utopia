import type { KeepDeepEqualityCall, KeepDeepEqualityResult } from './deep-equality'
import {
  arrayDeepEquality,
  combine2EqualityCalls,
  combine3EqualityCalls,
  combine4EqualityCalls,
  combine5EqualityCalls,
  combine6EqualityCalls,
  createCallFromEqualsFunction,
  createCallWithShallowEquals,
  createCallWithTripleEquals,
  keepDeepEqualityResult,
  mapKeepDeepEqualityResult,
  nullableDeepEquality,
  StringKeepDeepEquality,
  unionDeepEquality,
} from './deep-equality'
import * as EP from '../core/shared/element-path'
import * as PP from '../core/shared/property-path'
import type { HigherOrderControl } from '../components/canvas/canvas-types'
import type { JSXElementName } from '../core/shared/element-template'
import type {
  ElementPath,
  ElementPropertyPath,
  PropertyPath,
  StaticElementPath,
} from '../core/shared/project-file-types'
import {
  createCallFromIntrospectiveKeepDeep,
  getIntrospectiveKeepDeepResult,
} from './react-performance'
import type { Either } from '../core/shared/either'
import { foldEither, isLeft, left, right } from '../core/shared/either'
import type { NameAndIconResult } from '../components/inspector/common/name-and-icon-hook'
import type { ElementsToRerender, ElementWarnings } from '../components/editor/store/editor-state'
import {
  DropTargetHint,
  elementWarnings,
  NavigatorState,
} from '../components/editor/store/editor-state'
import type { LayoutTargetableProp } from '../core/layout/layout-helpers-new'
import type { CanvasPoint, WindowPoint } from '../core/shared/math-utils'
import { canvasPoint, windowPoint } from '../core/shared/math-utils'
import * as EPP from '../components/template-property-path'

export const ElementPathKeepDeepEquality: KeepDeepEqualityCall<ElementPath> =
  createCallFromEqualsFunction((oldPath: ElementPath, newPath: ElementPath) => {
    return EP.pathsEqual(oldPath, newPath)
  })

export const StaticElementPathKeepDeepEquality: KeepDeepEqualityCall<StaticElementPath> =
  createCallFromEqualsFunction((oldPath: StaticElementPath, newPath: StaticElementPath) => {
    return EP.pathsEqual(oldPath, newPath)
  })

export const ElementPathArrayKeepDeepEquality: KeepDeepEqualityCall<Array<ElementPath>> =
  arrayDeepEquality(ElementPathKeepDeepEquality)

export function PropertyPathKeepDeepEquality(): KeepDeepEqualityCall<PropertyPath> {
  return createCallFromEqualsFunction((oldPath: PropertyPath, newPath: PropertyPath) => {
    return PP.pathsEqual(oldPath, newPath)
  })
}

export function ElementPropertyPathKeepDeepEquality(): KeepDeepEqualityCall<ElementPropertyPath> {
  return combine2EqualityCalls(
    (e) => e.elementPath,
    ElementPathKeepDeepEquality,
    (e) => e.propertyPath,
    PropertyPathKeepDeepEquality(),
    (e, p) => EPP.create(e, p),
  )
}

export function HigherOrderControlKeepDeepEquality(
  oldValue: HigherOrderControl,
  newValue: HigherOrderControl,
): KeepDeepEqualityResult<HigherOrderControl> {
  return getIntrospectiveKeepDeepResult<HigherOrderControl>(oldValue, newValue)
}

export const HigherOrderControlArrayKeepDeepEquality: KeepDeepEqualityCall<
  Array<HigherOrderControl>
> = arrayDeepEquality(HigherOrderControlKeepDeepEquality)

export function JSXElementNameKeepDeepEqualityCall(
  oldValue: JSXElementName,
  newValue: JSXElementName,
): KeepDeepEqualityResult<JSXElementName> {
  return combine2EqualityCalls(
    (name: JSXElementName) => name.baseVariable,
    StringKeepDeepEquality,
    (name: JSXElementName) => name.propertyPath,
    PropertyPathKeepDeepEquality(),
    (baseVariable, propertyPath) => {
      return {
        baseVariable: baseVariable,
        propertyPath: propertyPath,
      }
    },
  )(oldValue, newValue)
}

export function EitherKeepDeepEquality<L, R>(
  leftDeep: KeepDeepEqualityCall<L>,
  rightDeep: KeepDeepEqualityCall<R>,
): KeepDeepEqualityCall<Either<L, R>> {
  type Result = KeepDeepEqualityResult<Either<L, R>>
  return (oldEither: Either<L, R>, newEither: Either<L, R>) => {
    const result = foldEither<L, R, Result>(
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

    return result.areEqual ? keepDeepEqualityResult(oldEither, true) : result
  }
}

export const NameAndIconResultKeepDeepEquality: KeepDeepEqualityCall<NameAndIconResult> =
  combine4EqualityCalls(
    (result) => result.path,
    ElementPathKeepDeepEquality,
    (result) => result.name,
    nullableDeepEquality(JSXElementNameKeepDeepEqualityCall),
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

export const NameAndIconResultArrayKeepDeepEquality: KeepDeepEqualityCall<
  Array<NameAndIconResult>
> = arrayDeepEquality(NameAndIconResultKeepDeepEquality)

export const LayoutTargetablePropArrayKeepDeepEquality: KeepDeepEqualityCall<
  Array<LayoutTargetableProp>
> = arrayDeepEquality(createCallWithTripleEquals())

export const ElementWarningsKeepDeepEquality: KeepDeepEqualityCall<ElementWarnings> =
  combine5EqualityCalls(
    (warnings) => warnings.widthOrHeightZero,
    createCallWithTripleEquals(),
    (warnings) => warnings.absoluteWithUnpositionedParent,
    createCallWithTripleEquals(),
    (warnings) => warnings.dynamicSceneChildWidthHeightPercentage,
    createCallWithTripleEquals(),
    (warnings) => warnings.invalidGroup,
    createCallWithTripleEquals(),
    (warnings) => warnings.invalidGroupChild,
    createCallWithTripleEquals(),
    elementWarnings,
  )

export const WindowPointKeepDeepEquality: KeepDeepEqualityCall<WindowPoint> = combine2EqualityCalls(
  (point) => point.x,
  createCallWithTripleEquals(),
  (point) => point.y,
  createCallWithTripleEquals(),
  (x, y) => windowPoint({ x: x, y: y }),
)

export const CanvasPointKeepDeepEquality: KeepDeepEqualityCall<CanvasPoint> = combine2EqualityCalls(
  (point) => point.x,
  createCallWithTripleEquals(),
  (point) => point.y,
  createCallWithTripleEquals(),
  (x, y) => canvasPoint({ x: x, y: y }),
)

export const ElementsToRerenderKeepDeepEquality: KeepDeepEqualityCall<ElementsToRerender> =
  unionDeepEquality(
    createCallWithTripleEquals<'rerender-all-elements'>(),
    ElementPathArrayKeepDeepEquality,
    (p): p is 'rerender-all-elements' => p === 'rerender-all-elements',
    (p): p is Array<ElementPath> => Array.isArray(p),
  )
