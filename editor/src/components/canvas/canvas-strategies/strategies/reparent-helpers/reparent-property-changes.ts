import { styleStringInArray } from '../../../../../utils/common-constants'
import { isHorizontalPoint } from 'utopia-api/core'
import { getLayoutProperty } from '../../../../../core/layout/getLayoutProperty'
import {
  framePointForPinnedProp,
  LayoutPinnedProp,
} from '../../../../../core/layout/layout-helpers-new'
import { MetadataUtils } from '../../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../../core/shared/array-utils'
import { eitherToMaybe, isRight, right } from '../../../../../core/shared/either'
import * as EP from '../../../../../core/shared/element-path'
import { ElementInstanceMetadataMap, JSXElement } from '../../../../../core/shared/element-template'
import {
  CanvasPoint,
  canvasPoint,
  CanvasVector,
  isInfinityRectangle,
  nullIfInfinity,
  pointDifference,
  roundPointToNearestHalf,
} from '../../../../../core/shared/math-utils'
import { ElementPath, PropertyPath } from '../../../../../core/shared/project-file-types'
import * as PP from '../../../../../core/shared/property-path'
import { ProjectContentTreeRoot } from '../../../../assets'
import {
  AllElementProps,
  getElementFromProjectContents,
} from '../../../../editor/store/editor-state'
import { cssNumber, CSSPosition, Direction } from '../../../../inspector/common/css-utils'
import { stylePropPathMappingFn } from '../../../../inspector/common/property-path-hooks'
import {
  AdjustCssLengthProperties,
  adjustCssLengthProperties,
  CreateIfNotExistant,
  lengthPropertyToAdjust,
  LengthPropertyToAdjust,
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
import {
  convertRelativeSizingToVisualSize,
  convertSizingToVisualSizeWhenPastingFromFlexToFlex,
  ElementPathSnapshots,
  MetadataSnapshots,
  runReparentPropertyStrategies,
  setZIndexOnPastedElement,
  stripPinsConvertToVisualSize,
} from './reparent-property-strategies'
import { assertNever } from '../../../../../core/shared/utils'
import { flexChildProps, pruneFlexPropsCommands } from '../../../../inspector/inspector-common'
import { setCssLengthProperty } from '../../../commands/set-css-length-command'
import { ElementPathTrees } from '../../../../../core/shared/element-path-tree'
import {
  replaceFragmentLikePathsWithTheirChildrenRecursive,
  treatElementAsFragmentLike,
} from '../fragment-like-helpers'

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
): Array<AdjustCssLengthProperties | ConvertCssPercentToPx> {
  const element: JSXElement | null = getElementFromProjectContents(
    target,
    projectContents,
    openFile,
  )

  const originalParentInstance = MetadataUtils.findElementByElementPath(
    targetStartingMetadata,
    EP.parentPath(target),
  )
  const newParentInstance = MetadataUtils.findElementByElementPath(
    newParentStartingMetadata,
    newParent,
  )

  if (element == null || originalParentInstance == null || newParentInstance == null) {
    return []
  }

  const currentParentContentBox =
    MetadataUtils.getGlobalContentBoxForChildren(originalParentInstance)
  const newParentContentBox = MetadataUtils.getGlobalContentBoxForChildren(newParentInstance)

  if (currentParentContentBox == null || newParentContentBox == null) {
    return []
  }

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

  const newParentFrame = nullIfInfinity(
    MetadataUtils.getFrameInCanvasCoords(newParent, newParentStartingMetadata),
  )
  const newParentFlexDirection =
    MetadataUtils.findElementByElementPath(newParentStartingMetadata, newParent)
      ?.specialSizeMeasurements.flexDirection ?? null

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

  // We need at least one position prop offset in each dimension
  const hasPin = (pin: LayoutPinnedProp) => {
    const rawPin = getLayoutProperty(pin, right(element.props), styleStringInArray)
    return isRight(rawPin) && rawPin.value != null
  }

  const needsLeftPin = !hasPin('left') && !hasPin('right')
  const needsTopPin = !hasPin('top') && !hasPin('bottom')

  let edgePropertiesToAdjust: Array<LengthPropertyToAdjust> = []

  for (const pin of ['top', 'left'] as const) {
    const horizontal = isHorizontalPoint(framePointForPinnedProp(pin))
    const needsPin = (pin === 'left' && needsLeftPin) || (pin === 'top' && needsTopPin)
    edgePropertiesToAdjust.push(
      lengthPropertyToAdjust(
        stylePropPathMappingFn(pin, styleStringInArray),
        horizontal ? offsetTL.x : offsetTL.y,
        horizontal ? newParentFrame?.width : newParentFrame?.height,
        needsPin ? 'create-if-not-existing' : 'do-not-create-if-doesnt-exist',
      ),
    )
  }

  for (const pin of ['bottom', 'right'] as const) {
    const horizontal = isHorizontalPoint(framePointForPinnedProp(pin))
    edgePropertiesToAdjust.push(
      lengthPropertyToAdjust(
        stylePropPathMappingFn(pin, styleStringInArray),
        horizontal ? offsetBR.x : offsetBR.y,
        horizontal ? newParentFrame?.width : newParentFrame?.height,
        'do-not-create-if-doesnt-exist',
      ),
    )
  }

  const widthHeightCommands = mapDropNulls((pin) => createConvertCssPercentToPx(pin), [
    'width',
    'height',
  ] as const)

  return [
    adjustCssLengthProperties('always', target, newParentFlexDirection, edgePropertiesToAdjust),
    ...widthHeightCommands,
  ]
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

export type ElementPathLookup = {
  [oldUid: string]: /* new element path */ ElementPath
}

export function positionElementToCoordinatesCommands(
  elementToReparent: ElementPathSnapshots,
  oldAllElementProps: AllElementProps,
  metadata: MetadataSnapshots,
  desiredTopLeft: CanvasPoint,
  pathLookup: ElementPathLookup,
): CanvasCommand[] {
  const basicCommands = [
    ...pruneFlexPropsCommands(flexChildProps, elementToReparent.newPath),
    setCssLengthProperty(
      'always',
      elementToReparent.newPath,
      PP.create('style', 'top'),
      { type: 'EXPLICIT_CSS_NUMBER', value: cssNumber(desiredTopLeft.y, null) },
      null,
    ),
    setCssLengthProperty(
      'always',
      elementToReparent.newPath,
      PP.create('style', 'left'),
      { type: 'EXPLICIT_CSS_NUMBER', value: cssNumber(desiredTopLeft.x, null) },
      null,
    ),
    setProperty('always', elementToReparent.newPath, PP.create('style', 'position'), 'absolute'),
  ]

  const elementIsFragmentLike = treatElementAsFragmentLike(
    metadata.originalTargetMetadata,
    oldAllElementProps,
    metadata.originalPathTrees,
    elementToReparent.oldPath,
  )

  if (!elementIsFragmentLike) {
    return basicCommands
  }

  const children = replaceFragmentLikePathsWithTheirChildrenRecursive(
    metadata.originalTargetMetadata,
    oldAllElementProps,
    metadata.originalPathTrees,
    [elementToReparent.oldPath],
  )

  const containerBounds = MetadataUtils.getFrameInCanvasCoords(
    elementToReparent.oldPath,
    metadata.originalTargetMetadata,
  )

  if (containerBounds == null || isInfinityRectangle(containerBounds)) {
    return basicCommands
  }

  const childrenPositioningCommands = children.flatMap((child) => {
    const childBounds = MetadataUtils.getFrameInCanvasCoords(child, metadata.originalTargetMetadata)
    if (childBounds == null || isInfinityRectangle(childBounds)) {
      return []
    }
    const adjustedTop = desiredTopLeft.y + childBounds.y - containerBounds.y
    const adjustedLeft = desiredTopLeft.x + childBounds.x - containerBounds.x

    const targetPath = pathLookup[EP.toUid(child)]

    return [
      ...pruneFlexPropsCommands(flexChildProps, elementToReparent.newPath),
      setProperty('always', elementToReparent.newPath, PP.create('style', 'position'), 'absolute'),
      setCssLengthProperty(
        'always',
        targetPath,
        PP.create('style', 'top'),
        { type: 'EXPLICIT_CSS_NUMBER', value: cssNumber(adjustedTop, null) },
        null,
      ),
      setCssLengthProperty(
        'always',
        targetPath,
        PP.create('style', 'left'),
        { type: 'EXPLICIT_CSS_NUMBER', value: cssNumber(adjustedLeft, null) },
        null,
      ),
    ]
  })

  return [...basicCommands, ...childrenPositioningCommands]
}

export function getReparentPropertyChanges(
  reparentStrategy: ReparentStrategy,
  originalElementPath: ElementPath,
  target: ElementPath,
  newParent: ElementPath,
  originalContextMetadata: ElementInstanceMetadataMap,
  originalPathTrees: ElementPathTrees,
  currentMetadata: ElementInstanceMetadataMap,
  currentPathTrees: ElementPathTrees,
  projectContents: ProjectContentTreeRoot,
  openFile: string | null | undefined,
  targetOriginalStylePosition: CSSPosition | null,
  targetOriginalDisplayProp: string | null,
): Array<CanvasCommand> {
  const newPath = EP.appendToPath(newParent, EP.toUid(target))
  switch (reparentStrategy) {
    case 'REPARENT_AS_ABSOLUTE': {
      const basicCommads = getAbsoluteReparentPropertyChanges(
        target,
        newParent,
        originalContextMetadata,
        currentMetadata,
        projectContents,
        openFile,
      )

      const strategyCommands = runReparentPropertyStrategies([
        stripPinsConvertToVisualSize(
          { oldPath: originalElementPath, newPath: newPath },
          {
            originalTargetMetadata: originalContextMetadata,
            currentMetadata: currentMetadata,
            originalPathTrees: originalPathTrees,
            currentPathTrees: currentPathTrees,
          },
        ),
        convertRelativeSizingToVisualSize(
          { oldPath: originalElementPath, newPath: newPath },
          {
            originalTargetMetadata: originalContextMetadata,
            currentMetadata: currentMetadata,
            originalPathTrees: originalPathTrees,
            currentPathTrees: currentPathTrees,
          },
        ),
        setZIndexOnPastedElement({ oldPath: originalElementPath, newPath: newPath }, newParent, {
          originalTargetMetadata: originalContextMetadata,
          currentMetadata: currentMetadata,
          originalPathTrees: originalPathTrees,
          currentPathTrees: currentPathTrees,
        }),
      ])

      return [...basicCommads, ...strategyCommands]
    }
    case 'REPARENT_AS_STATIC': {
      const directions = singleAxisAutoLayoutContainerDirections(
        newParent,
        currentMetadata,
        currentPathTrees,
      )

      const convertDisplayInline =
        directions === 'non-single-axis-autolayout' || directions.flexOrFlow === 'flex'
          ? 'do-not-convert'
          : directions.direction

      const basicCommads = getStaticReparentPropertyChanges(
        newPath,
        targetOriginalStylePosition,
        targetOriginalDisplayProp,
        convertDisplayInline,
      )
      const strategyCommands = runReparentPropertyStrategies([
        stripPinsConvertToVisualSize(
          { oldPath: originalElementPath, newPath: newPath },
          {
            originalTargetMetadata: originalContextMetadata,
            currentMetadata: currentMetadata,
            originalPathTrees: originalPathTrees,
            currentPathTrees: currentPathTrees,
          },
        ),
        convertRelativeSizingToVisualSize(
          { oldPath: originalElementPath, newPath: newPath },
          {
            originalTargetMetadata: originalContextMetadata,
            currentMetadata: currentMetadata,
            originalPathTrees: originalPathTrees,
            currentPathTrees: currentPathTrees,
          },
        ),
        convertSizingToVisualSizeWhenPastingFromFlexToFlex(
          { oldPath: originalElementPath, newPath: newPath },
          newParent,
          {
            originalTargetMetadata: originalContextMetadata,
            currentMetadata: currentMetadata,
            originalPathTrees: originalPathTrees,
            currentPathTrees: currentPathTrees,
          },
        ),
      ])

      return [...basicCommads, ...strategyCommands]
    }
    default:
      assertNever(reparentStrategy)
  }
}
