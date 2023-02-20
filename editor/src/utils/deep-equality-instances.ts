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
  KeepDeepEqualityCall,
  KeepDeepEqualityResult,
  keepDeepEqualityResult,
  mapKeepDeepEqualityResult,
  nullableDeepEquality,
  StringKeepDeepEquality,
  unionDeepEquality,
} from './deep-equality'
import * as EP from '../core/shared/element-path'
import * as PP from '../core/shared/property-path'
import { HigherOrderControl } from '../components/canvas/canvas-types'
import { JSXElementName } from '../core/shared/element-template'
import { ElementPath, PropertyPath, StaticElementPath } from '../core/shared/project-file-types'
import { createCallFromIntrospectiveKeepDeep } from './react-performance'
import { Either, foldEither, isLeft, left, right } from '../core/shared/either'
import { NameAndIconResult } from '../components/inspector/common/name-and-icon-hook'
import {
  DropTargetHint,
  ElementsToRerender,
  elementWarnings,
  ElementWarnings,
  NavigatorState,
} from '../components/editor/store/editor-state'
import { LayoutTargetableProp } from '../core/layout/layout-helpers-new'
import { CanvasPoint, canvasPoint, windowPoint, WindowPoint } from '../core/shared/math-utils'

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

export function HigherOrderControlKeepDeepEquality(
  oldValue: HigherOrderControl,
  newValue: HigherOrderControl,
): KeepDeepEqualityResult<HigherOrderControl> {
  return createCallFromIntrospectiveKeepDeep<HigherOrderControl>()(oldValue, newValue)
}

export const HigherOrderControlArrayKeepDeepEquality: KeepDeepEqualityCall<
  Array<HigherOrderControl>
> = arrayDeepEquality(HigherOrderControlKeepDeepEquality)

export const JSXElementNameKeepDeepEqualityCall: KeepDeepEqualityCall<JSXElementName> =
  combine2EqualityCalls(
    (name) => name.baseVariable,
    StringKeepDeepEquality,
    (name) => name.propertyPath,
    PropertyPathKeepDeepEquality(),
    (baseVariable, propertyPath) => {
      return {
        baseVariable: baseVariable,
        propertyPath: propertyPath,
      }
    },
  )

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

export const DropTargetHintKeepDeepEquality: KeepDeepEqualityCall<DropTargetHint> =
  combine3EqualityCalls(
    (hint) => hint.displayAtElementPath,
    nullableDeepEquality(ElementPathKeepDeepEquality),
    (hint) => hint.moveToElementPath,
    nullableDeepEquality(ElementPathKeepDeepEquality),
    (hint) => hint.type,
    createCallWithTripleEquals(),
    (displayAtElementPath, moveToElementPath, type) => {
      return {
        displayAtElementPath: displayAtElementPath,
        moveToElementPath: moveToElementPath,
        type: type,
      }
    },
  )

export const NavigatorStateKeepDeepEquality: KeepDeepEqualityCall<NavigatorState> =
  combine6EqualityCalls(
    (state) => state.minimised,
    createCallWithTripleEquals(),
    (state) => state.dropTargetHint,
    DropTargetHintKeepDeepEquality,
    (state) => state.collapsedViews,
    ElementPathArrayKeepDeepEquality,
    (state) => state.renamingTarget,
    nullableDeepEquality(ElementPathKeepDeepEquality),
    (state) => state.highlightedTargets,
    ElementPathArrayKeepDeepEquality,
    (state) => state.hiddenInNavigator,
    ElementPathArrayKeepDeepEquality,
    (
      minimised,
      dropTargetHint,
      collapsedViews,
      renamingTarget,
      highlightedTargets,
      hiddenInNavigator,
    ) => {
      return {
        minimised: minimised,
        dropTargetHint: dropTargetHint,
        collapsedViews: collapsedViews,
        renamingTarget: renamingTarget,
        highlightedTargets: highlightedTargets,
        hiddenInNavigator: hiddenInNavigator,
      }
    },
  )

export const LayoutTargetablePropArrayKeepDeepEquality: KeepDeepEqualityCall<
  Array<LayoutTargetableProp>
> = arrayDeepEquality(createCallWithTripleEquals())

export const ElementWarningsKeepDeepEquality: KeepDeepEqualityCall<ElementWarnings> =
  combine3EqualityCalls(
    (warnings) => warnings.widthOrHeightZero,
    createCallWithTripleEquals(),
    (warnings) => warnings.absoluteWithUnpositionedParent,
    createCallWithTripleEquals(),
    (warnings) => warnings.dynamicSceneChildWidthHeightPercentage,
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
