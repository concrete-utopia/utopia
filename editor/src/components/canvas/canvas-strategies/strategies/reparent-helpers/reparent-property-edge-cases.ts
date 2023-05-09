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
import { sizeToVisualDimensionsAlongAxisInstance } from '../../../../inspector/inspector-common'
import { deleteProperties } from '../../../commands/delete-properties-command'
import * as PP from '../../../../../core/shared/property-path'

type ReparentPropertyEdgeCaseUnapplicableReason = string

type ReparentPropertyEdgeCase = () => Either<
  ReparentPropertyEdgeCaseUnapplicableReason,
  Array<CanvasCommand>
>

type MkReparentPropertyEdgeCase = (
  elementToReparent: { oldPath: ElementPath; newPath: ElementPath },
  targetParent: ElementPath,
  metadata: ElementInstanceMetadataMap,
) => ReparentPropertyEdgeCase

const hasPin = (pin: LayoutPinnedProp, element: JSXElement) => {
  const rawPin = getLayoutProperty(pin, right(element.props), styleStringInArray)
  return isRight(rawPin) && rawPin.value != null
}

export const stripPinsConvertToVisualSize: MkReparentPropertyEdgeCase =
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

export function runReparentPropertyEdgeCases(
  edgeCases: Array<ReparentPropertyEdgeCase>,
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
