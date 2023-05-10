import { getLayoutProperty } from '../../../../../core/layout/getLayoutProperty'
import { LayoutPinnedProp } from '../../../../../core/layout/layout-helpers-new'
import { Either, foldEither, isLeft, isRight, left, right } from '../../../../../core/shared/either'
import {
  ElementInstanceMetadataMap,
  JSXElement,
  isJSXElement,
} from '../../../../../core/shared/element-template'
import { ElementPath } from '../../../../../core/shared/project-file-types'
import { styleStringInArray } from '../../../../../utils/common-constants'
import { CanvasCommand } from '../../../commands/commands'
import { MetadataUtils } from '../../../../../core/model/element-metadata-utils'
import {
  nukeSizingPropsForAxisCommand,
  sizeToVisualDimensionsAlongAxisInstance,
} from '../../../../inspector/inspector-common'
import { deleteProperties } from '../../../commands/delete-properties-command'
import * as PP from '../../../../../core/shared/property-path'

type ReparentPropertyStrategyUnapplicableReason = string

type ReparentPropertyStrategy = () => Either<
  ReparentPropertyStrategyUnapplicableReason,
  Array<CanvasCommand>
>

type MkReparentPropertyStrategy = (
  elementToReparent: { oldPath: ElementPath; newPath: ElementPath },
  targetParent: ElementPath,
  metadata: ElementInstanceMetadataMap,
) => ReparentPropertyStrategy

const hasPin = (pin: LayoutPinnedProp, element: JSXElement) => {
  const rawPin = getLayoutProperty(pin, right(element.props), styleStringInArray)
  return isRight(rawPin) && rawPin.value != null
}

export const stripPinsConvertToVisualSize: MkReparentPropertyStrategy =
  (
    elementToReparent: { oldPath: ElementPath; newPath: ElementPath },
    targetParent: ElementPath,
    metadata: ElementInstanceMetadataMap,
  ) =>
  () => {
    const isTargetParentFlex = MetadataUtils.isFlexLayoutedContainer(
      MetadataUtils.findElementByElementPath(metadata, targetParent),
    )
    if (!isTargetParentFlex) {
      return left('Target parent is not a flex container')
    }

    const instance = MetadataUtils.findElementByElementPath(metadata, elementToReparent.oldPath)
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
    elementToReparent: { oldPath: ElementPath; newPath: ElementPath },
    metadata: ElementInstanceMetadataMap,
  ): ReparentPropertyStrategy =>
  () => {
    const instance = MetadataUtils.findElementByElementPath(metadata, elementToReparent.oldPath)
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
    elementToReparent: { oldPath: ElementPath; newPath: ElementPath },
    targetParent: ElementPath,
    metadata: ElementInstanceMetadataMap,
  ): ReparentPropertyStrategy =>
  () => {
    const targetParentInstance = MetadataUtils.findElementByElementPath(metadata, targetParent)
    if (targetParentInstance == null) {
      return left('Target parent has no metadata')
    }

    if (!MetadataUtils.isFlexLayoutedContainer(targetParentInstance)) {
      return left('Target parent is not a flex layouted container')
    }

    const elementToReparentInstance = MetadataUtils.findElementByElementPath(
      metadata,
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
