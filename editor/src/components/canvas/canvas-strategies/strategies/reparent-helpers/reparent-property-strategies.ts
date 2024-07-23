import { getLayoutProperty } from '../../../../../core/layout/getLayoutProperty'
import type { LayoutPinnedProp } from '../../../../../core/layout/layout-helpers-new'
import type { Either } from '../../../../../core/shared/either'
import { foldEither, isLeft, isRight, left, right } from '../../../../../core/shared/either'
import type {
  ElementInstanceMetadataMap,
  JSXElement,
} from '../../../../../core/shared/element-template'
import { isJSXElement } from '../../../../../core/shared/element-template'
import type { ElementPath } from '../../../../../core/shared/project-file-types'
import { styleStringInArray } from '../../../../../utils/common-constants'
import type { CanvasCommand } from '../../../commands/commands'
import { MetadataUtils } from '../../../../../core/model/element-metadata-utils'
import {
  nukeSizingPropsForAxisCommand,
  sizeToVisualDimensionsAlongAxisInstance,
} from '../../../../inspector/inspector-common'
import { deleteProperties } from '../../../commands/delete-properties-command'
import * as PP from '../../../../../core/shared/property-path'
import { setCssLengthProperty, setExplicitCssValue } from '../../../commands/set-css-length-command'
import { cssNumber } from '../../../../inspector/common/css-utils'
import { mapDropNulls } from '../../../../../core/shared/array-utils'
import * as EP from '../../../../../core/shared/element-path'
import type { ElementPathTrees } from '../../../../../core/shared/element-path-tree'
import {
  getStaticReparentPropertyChanges,
  getAbsoluteReparentPropertyChanges,
} from './reparent-property-changes'
import {
  replaceFragmentLikePathsWithTheirChildrenRecursive,
  treatElementAsFragmentLike,
} from '../fragment-like-helpers'
import type { AllElementProps } from '../../../../editor/store/editor-state'
import type { ReparentStrategy } from './reparent-strategy-helpers'
import type { ProjectContentTreeRoot } from '../../../../assets'
import { singleAxisAutoLayoutContainerDirections } from '../flow-reorder-helpers'
import type { OldPathToNewPathMapping } from '../../post-action-options/post-action-paste'

type ReparentPropertyStrategyUnapplicableReason = string

type ReparentPropertyStrategy = () => Either<
  ReparentPropertyStrategyUnapplicableReason,
  Array<CanvasCommand>
>

const hasPin = (pin: LayoutPinnedProp, element: JSXElement) => {
  const rawPin = getLayoutProperty(pin, right(element.props), styleStringInArray)
  return isRight(rawPin) && rawPin.value != null
}

export interface ElementPathSnapshots {
  oldPath: ElementPath
  newPath: ElementPath
}

export interface MetadataSnapshots {
  originalTargetMetadata: ElementInstanceMetadataMap
  originalPathTrees: ElementPathTrees
  originalDomReconstructedMetadata: ElementInstanceMetadataMap
  currentMetadata: ElementInstanceMetadataMap
  currentPathTrees: ElementPathTrees
  currentDomReconstructedMetadata: ElementInstanceMetadataMap
}

export const stripPinsConvertToVisualSize =
  (
    elementToReparent: ElementPathSnapshots,
    metadata: MetadataSnapshots,
  ): ReparentPropertyStrategy =>
  () => {
    const instance = MetadataUtils.findElementByElementPath(
      metadata.originalTargetMetadata,
      elementToReparent.oldPath,
    )
    if (instance == null) {
      return left('Cannot find metadata for reparented element')
    }

    if (isLeft(instance.element) || !isJSXElement(instance.element.value)) {
      return left('Reparented element is not a JSX element')
    }

    const hasBothHorizontalPins =
      hasPin('left', instance.element.value) && hasPin('right', instance.element.value)
    const hasBothVerticalPins =
      hasPin('top', instance.element.value) && hasPin('bottom', instance.element.value)

    if (!hasBothHorizontalPins && !hasBothVerticalPins) {
      return left('Element does not have both pins set on either axis')
    }

    const adjustVerticalPinsCommands = hasBothVerticalPins
      ? [
          ...sizeToVisualDimensionsAlongAxisInstance(
            'vertical',
            instance,
          )(elementToReparent.newPath),
          deleteProperties('always', elementToReparent.newPath, [
            PP.create('style', 'top'),
            PP.create('style', 'bottom'),
          ]),
        ]
      : []

    const adjustHorizontalPinsCommands = hasBothHorizontalPins
      ? [
          ...sizeToVisualDimensionsAlongAxisInstance(
            'horizontal',
            instance,
          )(elementToReparent.newPath),
          deleteProperties('always', elementToReparent.newPath, [
            PP.create('style', 'right'),
            PP.create('style', 'left'),
          ]),
        ]
      : []

    return right([...adjustHorizontalPinsCommands, ...adjustVerticalPinsCommands])
  }

export const convertRelativeSizingToVisualSize =
  (
    elementToReparent: ElementPathSnapshots,
    metadata: MetadataSnapshots,
  ): ReparentPropertyStrategy =>
  () => {
    const instance = MetadataUtils.findElementByElementPath(
      metadata.originalTargetMetadata,
      elementToReparent.oldPath,
    )
    if (instance == null) {
      return left('Cannot find metadata for reparented element')
    }

    if (isLeft(instance.element) || !isJSXElement(instance.element.value)) {
      return left('Reparented element is not a JSX element')
    }

    const isWidthRelative =
      foldEither(
        () => null,
        (e) => e?.unit,
        getLayoutProperty('width', right(instance.element.value.props), styleStringInArray),
      ) === '%'

    const isHeightRelative =
      foldEither(
        () => null,
        (e) => e?.unit,
        getLayoutProperty('height', right(instance.element.value.props), styleStringInArray),
      ) === '%'

    if (!isWidthRelative && !isHeightRelative) {
      return left('Neither width nor height is set to %')
    }

    const adjustVerticalPinsCommands = isHeightRelative
      ? sizeToVisualDimensionsAlongAxisInstance('vertical', instance)(elementToReparent.newPath)
      : []

    const adjustHorizontalPinsCommands = isWidthRelative
      ? sizeToVisualDimensionsAlongAxisInstance('horizontal', instance)(elementToReparent.newPath)
      : []

    return right([...adjustHorizontalPinsCommands, ...adjustVerticalPinsCommands])
  }

export const convertSizingToVisualSizeWhenPastingFromFlexToFlex =
  (
    elementToReparent: ElementPathSnapshots,
    metadata: MetadataSnapshots,
    targetParent: ElementPath,
  ): ReparentPropertyStrategy =>
  () => {
    const targetParentInstance = MetadataUtils.findElementByElementPath(
      metadata.currentMetadata,
      targetParent,
    )
    if (targetParentInstance == null) {
      return left('Target parent has no metadata')
    }

    if (!MetadataUtils.isFlexLayoutedContainer(targetParentInstance)) {
      return left('Target parent is not a flex layouted container')
    }

    const elementToReparentInstance = MetadataUtils.findElementByElementPath(
      metadata.originalTargetMetadata,
      elementToReparent.oldPath,
    )

    if (elementToReparentInstance == null) {
      return left('Element to reparent has no metadata')
    }

    const isTargetParentFlex = MetadataUtils.isFlexLayoutedContainer(targetParentInstance)
    const isOriginalParentFlex =
      elementToReparentInstance.specialSizeMeasurements.parentFlexDirection != null

    if (!isTargetParentFlex || !isOriginalParentFlex) {
      return left('Strategy is only applicable when pasting from flex to flex')
    }

    return right([
      nukeSizingPropsForAxisCommand('horizontal', elementToReparent.newPath),
      ...sizeToVisualDimensionsAlongAxisInstance(
        'horizontal',
        elementToReparentInstance,
      )(elementToReparent.newPath),
      nukeSizingPropsForAxisCommand('vertical', elementToReparent.newPath),
      ...sizeToVisualDimensionsAlongAxisInstance(
        'vertical',
        elementToReparentInstance,
      )(elementToReparent.newPath),
    ])
  }

const getZIndex = (element: JSXElement): number | null => {
  const zIndex = getLayoutProperty('zIndex', right(element.props), styleStringInArray)
  return foldEither(
    () => null,
    (z) => z?.value ?? null,
    zIndex,
  )
}

export const setZIndexOnPastedElement =
  (
    elementToReparent: ElementPathSnapshots,
    targetParent: ElementPath,
    metadata: MetadataSnapshots,
  ): ReparentPropertyStrategy =>
  () => {
    const siblings = MetadataUtils.getChildrenOrdered(
      metadata.currentMetadata,
      metadata.currentPathTrees,
      targetParent,
    )
    const maximumZIndexOfOverlappingElements = mapDropNulls((sibling) => {
      return foldEither(
        () => null,
        (e) => (isJSXElement(e) ? getZIndex(e) : null),
        sibling.element,
      )
    }, siblings)
      .sort()
      .reverse()
      .at(0)

    if (maximumZIndexOfOverlappingElements == null) {
      return left('No siblings have z-index applied')
    }

    return right([
      setCssLengthProperty(
        'always',
        elementToReparent.newPath,
        PP.create('style', 'zIndex'),
        setExplicitCssValue(cssNumber(maximumZIndexOfOverlappingElements, null)),
        null,
      ),
    ])
  }

export const convertFragmentLikeChildrenToVisualSize =
  (
    elementToReparent: ElementPathSnapshots,
    metadata: MetadataSnapshots,
    oldAllElementProps: AllElementProps,
    childPathLookup: OldPathToNewPathMapping,
    newParent: ElementPath,
    reparentStrategy: ReparentStrategy,
    projectContents: ProjectContentTreeRoot,
    propertyStrategies: Array<
      (
        elementToReparent: ElementPathSnapshots,
        metadata: MetadataSnapshots,
        newParent: ElementPath,
      ) => ReparentPropertyStrategy
    >,
  ): ReparentPropertyStrategy =>
  () => {
    const isElementFragmentLike = treatElementAsFragmentLike(
      metadata.originalTargetMetadata,
      metadata.originalDomReconstructedMetadata,
      oldAllElementProps,
      metadata.originalPathTrees,
      elementToReparent.oldPath,
    )
    if (!isElementFragmentLike) {
      return left('Element is not fragment-like')
    }

    const childPaths = replaceFragmentLikePathsWithTheirChildrenRecursive(
      metadata.originalTargetMetadata,
      metadata.originalDomReconstructedMetadata,
      oldAllElementProps,
      metadata.originalPathTrees,
      [elementToReparent.oldPath],
    )

    const commands = childPaths.flatMap((originalPath) => {
      const instance = MetadataUtils.findElementByElementPath(
        metadata.originalTargetMetadata,
        originalPath,
      )
      const newPath = childPathLookup[EP.toString(originalPath)]
      if (instance == null || newPath == null) {
        return []
      }
      let baseLayoutConversionCommands: Array<CanvasCommand> = []
      if (reparentStrategy === 'REPARENT_AS_ABSOLUTE') {
        baseLayoutConversionCommands = getAbsoluteReparentPropertyChanges(
          newPath,
          newParent,
          metadata.originalTargetMetadata,
          metadata.currentMetadata,
          projectContents,
          'force-pins',
        )
      } else {
        const directions = singleAxisAutoLayoutContainerDirections(
          newParent,
          metadata.currentMetadata,
          metadata.currentPathTrees,
        )

        const convertDisplayInline =
          directions === 'non-single-axis-autolayout' || directions.flexOrFlow === 'flex'
            ? 'do-not-convert'
            : directions.direction
        baseLayoutConversionCommands = getStaticReparentPropertyChanges(
          newPath,
          instance.specialSizeMeasurements.position,
          instance.specialSizeMeasurements.display,
          convertDisplayInline,
        )
      }
      return [
        ...baseLayoutConversionCommands,
        ...runReparentPropertyStrategies(
          propertyStrategies.map((strategy) =>
            strategy({ oldPath: originalPath, newPath: newPath }, metadata, newParent),
          ),
        ),
      ]
    })

    return right(commands)
  }

export function runReparentPropertyStrategies(
  edgeCases: Array<ReparentPropertyStrategy>,
): Array<CanvasCommand> {
  return edgeCases.reduce(
    (commands, edgeCase) =>
      foldEither(
        () => commands,
        (newCommands) => [...commands, ...newCommands],
        edgeCase(),
      ),
    [] as Array<CanvasCommand>,
  )
}
