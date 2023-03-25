import { styleStringInArray } from '../../../../../utils/common-constants'
import { isHorizontalPoint } from 'utopia-api/core'
import { getLayoutProperty } from '../../../../../core/layout/getLayoutProperty'
import {
  framePointForPinnedProp,
  LayoutPinnedProp,
} from '../../../../../core/layout/layout-helpers-new'
import { MetadataUtils } from '../../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../../core/shared/array-utils'
import { isRight, right } from '../../../../../core/shared/either'
import * as EP from '../../../../../core/shared/element-path'
import { ElementInstanceMetadataMap, JSXElement } from '../../../../../core/shared/element-template'
import {
  canvasPoint,
  nullIfInfinity,
  pointDifference,
  roundPointToNearestHalf,
  zeroCanvasRect,
} from '../../../../../core/shared/math-utils'
import { ElementPath, PropertyPath } from '../../../../../core/shared/project-file-types'
import * as PP from '../../../../../core/shared/property-path'
import { ProjectContentTreeRoot } from '../../../../assets'
import { getElementFromProjectContents } from '../../../../editor/store/editor-state'
import { CSSPosition, Direction, FlexDirection } from '../../../../inspector/common/css-utils'
import { stylePropPathMappingFn } from '../../../../inspector/common/property-path-hooks'
import {
  AdjustCssLengthProperty,
  adjustCssLengthProperty,
} from '../../../commands/adjust-css-length-command'
import { CanvasCommand } from '../../../commands/commands'
import {
  ConvertCssPercentToPx,
  convertCssPercentToPx,
} from '../../../commands/convert-css-percent-to-px-command'
import { deleteProperties } from '../../../commands/delete-properties-command'
import { setProperty } from '../../../commands/set-property-command'
import {
  getOptionalCommandToConvertDisplayInlineBlock,
  singleAxisAutoLayoutContainerDirections,
} from '../flow-reorder-helpers'
import { ReparentStrategy } from './reparent-strategy-helpers'

const propertiesToRemove: Array<PropertyPath> = [
  PP.create('style', 'left'),
  PP.create('style', 'top'),
  PP.create('style', 'right'),
  PP.create('style', 'bottom'),
]

export function getAbsoluteReparentPropertyChanges(
  target: ElementPath,
  newParent: ElementPath,
  targetStartingMetadata: ElementInstanceMetadataMap,
  newParentStartingMetadata: ElementInstanceMetadataMap,
  projectContents: ProjectContentTreeRoot,
  openFile: string | null | undefined,
): Array<AdjustCssLengthProperty | ConvertCssPercentToPx> {
  const element: JSXElement | null = getElementFromProjectContents(
    target,
    projectContents,
    openFile,
  )

  if (element == null) {
    return []
  }

  const currentParentContentBox =
    MetadataUtils.findElementByElementPath(targetStartingMetadata, target)?.specialSizeMeasurements
      .offsetParentContentBox ?? zeroCanvasRect

  const newParentContentBox = MetadataUtils.getParentCoordinateSystemBounds(
    newParent,
    newParentStartingMetadata,
  )

  const offsetTL = roundPointToNearestHalf(
    pointDifference(newParentContentBox, currentParentContentBox),
  )
  const offsetBR = roundPointToNearestHalf(
    pointDifference(
      canvasPoint({
        x: currentParentContentBox.x + currentParentContentBox.width,
        y: currentParentContentBox.y + currentParentContentBox.height,
      }),
      canvasPoint({
        x: newParentContentBox.x + newParentContentBox.width,
        y: newParentContentBox.y + newParentContentBox.height,
      }),
    ),
  )

  const createAdjustCssLengthProperty = (
    pin: LayoutPinnedProp,
    newValue: number,
    parentDimension: number | undefined,
    elementParentFlexDirection: FlexDirection | null,
  ): AdjustCssLengthProperty | null => {
    const value = getLayoutProperty(pin, right(element.props), styleStringInArray)
    if (isRight(value) && value.value != null) {
      // TODO what to do about missing properties?
      return adjustCssLengthProperty(
        'always',
        target,
        stylePropPathMappingFn(pin, styleStringInArray),
        newValue,
        parentDimension,
        elementParentFlexDirection,
        'create-if-not-existing',
      )
    } else {
      return null
    }
  }

  const createConvertCssPercentToPx = (pin: LayoutPinnedProp): ConvertCssPercentToPx | null => {
    const value = getLayoutProperty(pin, right(element.props), styleStringInArray)
    if (isRight(value) && value.value != null && value.value.unit === '%') {
      return convertCssPercentToPx(
        'always',
        target,
        stylePropPathMappingFn(pin, styleStringInArray),
        isHorizontalPoint(framePointForPinnedProp(pin))
          ? currentParentContentBox.width
          : currentParentContentBox.height,
      )
    } else {
      return null
    }
  }

  const newParentFrame = nullIfInfinity(
    MetadataUtils.getFrameInCanvasCoords(newParent, newParentStartingMetadata),
  )
  const newParentFlexDirection =
    MetadataUtils.findElementByElementPath(newParentStartingMetadata, newParent)
      ?.specialSizeMeasurements.flexDirection ?? null

  const topLeftCommands = mapDropNulls(
    (pin) => {
      const horizontal = isHorizontalPoint(framePointForPinnedProp(pin))
      return createAdjustCssLengthProperty(
        pin,
        horizontal ? offsetTL.x : offsetTL.y,
        horizontal ? newParentFrame?.width : newParentFrame?.height,
        newParentFlexDirection,
      )
    },
    ['top', 'left'] as const,
  )

  const bottomRightCommands = mapDropNulls(
    (pin) => {
      const horizontal = isHorizontalPoint(framePointForPinnedProp(pin))
      return createAdjustCssLengthProperty(
        pin,
        horizontal ? offsetBR.x : offsetBR.y,
        horizontal ? newParentFrame?.width : newParentFrame?.height,
        newParentFlexDirection,
      )
    },
    ['bottom', 'right'] as const,
  )

  const widthHeightCommands = mapDropNulls((pin) => createConvertCssPercentToPx(pin), [
    'width',
    'height',
  ] as const)

  return [...topLeftCommands, ...bottomRightCommands, ...widthHeightCommands]
}

export function getStaticReparentPropertyChanges(
  newPath: ElementPath,
  targetOriginalStylePosition: CSSPosition | null,
  targetOriginalDisplayProp: string | null,
  convertToInline: Direction | 'do-not-convert',
): Array<CanvasCommand> {
  const optionalInlineConversionCommand = getOptionalCommandToConvertDisplayInlineBlock(
    newPath,
    targetOriginalDisplayProp,
    convertToInline,
  )

  if (targetOriginalStylePosition !== 'absolute' && targetOriginalStylePosition !== 'relative') {
    return [
      ...optionalInlineConversionCommand,
      deleteProperties('always', newPath, propertiesToRemove),
    ]
  }

  return [
    ...optionalInlineConversionCommand,
    deleteProperties('always', newPath, [...propertiesToRemove, PP.create('style', 'position')]),
    setProperty('always', newPath, PP.create('style', 'contain'), 'layout'),
  ]
}

export function getReparentPropertyChanges(
  reparentStrategy: ReparentStrategy,
  target: ElementPath,
  newParent: ElementPath,
  targetStartingMetadata: ElementInstanceMetadataMap,
  newParentStartingMetadata: ElementInstanceMetadataMap,
  projectContents: ProjectContentTreeRoot,
  openFile: string | null | undefined,
  targetOriginalStylePosition: CSSPosition | null,
  targetOriginalDisplayProp: string | null,
): Array<CanvasCommand> {
  switch (reparentStrategy) {
    case 'REPARENT_AS_ABSOLUTE':
      return getAbsoluteReparentPropertyChanges(
        target,
        newParent,
        targetStartingMetadata,
        newParentStartingMetadata,
        projectContents,
        openFile,
      )
    case 'REPARENT_AS_STATIC':
      const newPath = EP.appendToPath(newParent, EP.toUid(target))
      const directions = singleAxisAutoLayoutContainerDirections(
        newParent,
        newParentStartingMetadata,
      )

      const convertDisplayInline =
        directions === 'non-single-axis-autolayout' || directions.flexOrFlow === 'flex'
          ? 'do-not-convert'
          : directions.direction

      return getStaticReparentPropertyChanges(
        newPath,
        targetOriginalStylePosition,
        targetOriginalDisplayProp,
        convertDisplayInline,
      )
  }
}
