import { styleStringInArray } from '../../../../../utils/common-constants'
import { isHorizontalPoint } from 'utopia-api/core'
import { getLayoutProperty } from '../../../../../core/layout/getLayoutProperty'
import type { LayoutPinnedProp } from '../../../../../core/layout/layout-helpers-new'
import { framePointForPinnedProp } from '../../../../../core/layout/layout-helpers-new'
import { MetadataUtils } from '../../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../../core/shared/array-utils'
import { isRight, right } from '../../../../../core/shared/either'
import * as EP from '../../../../../core/shared/element-path'
import type {
  ElementInstanceMetadataMap,
  JSXElement,
} from '../../../../../core/shared/element-template'
import type { CanvasPoint } from '../../../../../core/shared/math-utils'
import {
  canvasPoint,
  isInfinityRectangle,
  nullIfInfinity,
  pointDifference,
  roundPointToNearestWhole,
} from '../../../../../core/shared/math-utils'
import type { ElementPath, PropertyPath } from '../../../../../core/shared/project-file-types'
import * as PP from '../../../../../core/shared/property-path'
import type { ProjectContentTreeRoot } from '../../../../assets'
import type { AllElementProps } from '../../../../editor/store/editor-state'
import { getJSXElementFromProjectContents } from '../../../../editor/store/editor-state'
import type { CSSPosition, Direction } from '../../../../inspector/common/css-utils'
import { cssNumber } from '../../../../inspector/common/css-utils'
import { stylePropPathMappingFn } from '../../../../inspector/common/property-path-hooks'
import type {
  AdjustCssLengthProperties,
  LengthPropertyToAdjust,
} from '../../../commands/adjust-css-length-command'
import {
  adjustCssLengthProperties,
  lengthPropertyToAdjust,
} from '../../../commands/adjust-css-length-command'
import type { CanvasCommand } from '../../../commands/commands'
import type { ConvertCssPercentToPx } from '../../../commands/convert-css-percent-to-px-command'
import { convertCssPercentToPx } from '../../../commands/convert-css-percent-to-px-command'
import { deleteProperties } from '../../../commands/delete-properties-command'
import { setProperty } from '../../../commands/set-property-command'
import {
  getOptionalCommandToConvertDisplayInlineBlock,
  singleAxisAutoLayoutContainerDirections,
} from '../flow-reorder-helpers'
import type { ReparentStrategy } from './reparent-strategy-helpers'
import type { ElementPathSnapshots, MetadataSnapshots } from './reparent-property-strategies'
import {
  convertFragmentLikeChildrenToVisualSize,
  convertRelativeSizingToVisualSize,
  convertSizingToVisualSizeWhenPastingFromFlexToFlex,
  runReparentPropertyStrategies,
  setZIndexOnPastedElement,
  stripPinsConvertToVisualSize,
} from './reparent-property-strategies'
import { assertNever } from '../../../../../core/shared/utils'
import { flexChildProps, prunePropsCommands } from '../../../../inspector/inspector-common'
import { setCssLengthProperty } from '../../../commands/set-css-length-command'
import type { ElementPathTrees } from '../../../../../core/shared/element-path-tree'
import {
  replaceFragmentLikePathsWithTheirChildrenRecursive,
  treatElementAsFragmentLike,
} from '../fragment-like-helpers'
import type { OldPathToNewPathMapping } from '../../post-action-options/post-action-paste'

const propertiesToRemove: Array<PropertyPath> = [
  PP.create('style', 'left'),
  PP.create('style', 'top'),
  PP.create('style', 'right'),
  PP.create('style', 'bottom'),
]

export type ForcePins = 'force-pins' | 'do-not-force-pins'

export function getAbsoluteReparentPropertyChanges(
  target: ElementPath,
  newParent: ElementPath,
  targetStartingMetadata: ElementInstanceMetadataMap,
  newParentStartingMetadata: ElementInstanceMetadataMap,
  projectContents: ProjectContentTreeRoot,
  forcePins: ForcePins,
): Array<AdjustCssLengthProperties | ConvertCssPercentToPx> {
  const element: JSXElement | null = getJSXElementFromProjectContents(target, projectContents)

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

  const offsetTL = roundPointToNearestWhole(
    pointDifference(newParentContentBox, currentParentContentBox),
  )
  const offsetBR = roundPointToNearestWhole(
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

  const needsLeftPin = !hasPin('left') && !hasPin('right') && forcePins === 'force-pins'
  const needsTopPin = !hasPin('top') && !hasPin('bottom') && forcePins === 'force-pins'

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

export function positionElementToCoordinatesCommands(
  elementToReparent: ElementPathSnapshots,
  oldAllElementProps: AllElementProps,
  metadata: MetadataSnapshots,
  desiredTopLeft: CanvasPoint,
  pathLookup: OldPathToNewPathMapping,
): CanvasCommand[] {
  const basicCommands = [
    ...prunePropsCommands(flexChildProps, elementToReparent.newPath),
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
    metadata.originalDomReconstructedMetadata,
    oldAllElementProps,
    metadata.originalPathTrees,
    elementToReparent.oldPath,
  )

  if (!elementIsFragmentLike) {
    return basicCommands
  }

  const children = replaceFragmentLikePathsWithTheirChildrenRecursive(
    metadata.originalTargetMetadata,
    metadata.originalDomReconstructedMetadata,
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

    const targetPath = pathLookup[EP.toString(child)]
    if (targetPath == null) {
      return basicCommands
    }

    return [
      ...prunePropsCommands(flexChildProps, targetPath),
      setProperty('always', targetPath, PP.create('style', 'position'), 'absolute'),
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
  originalDomReconstructedMetadata: ElementInstanceMetadataMap,
  originalPathTrees: ElementPathTrees,
  currentMetadata: ElementInstanceMetadataMap,
  currentDomReconstructedMetadata: ElementInstanceMetadataMap,
  currentPathTrees: ElementPathTrees,
  projectContents: ProjectContentTreeRoot,
  targetOriginalStylePosition: CSSPosition | null,
  targetOriginalDisplayProp: string | null,
  oldAllElementProps: AllElementProps,
  childPathLookup: OldPathToNewPathMapping,
): Array<CanvasCommand> {
  const newPath = EP.appendToPath(newParent, EP.toUid(target))

  const elementPathSnapshot = { oldPath: originalElementPath, newPath: newPath }
  const metadataSnapshot: MetadataSnapshots = {
    originalTargetMetadata: originalContextMetadata,
    originalPathTrees: originalPathTrees,
    originalDomReconstructedMetadata: originalDomReconstructedMetadata,
    currentMetadata: currentMetadata,
    currentPathTrees: currentPathTrees,
    currentDomReconstructedMetadata: currentDomReconstructedMetadata,
  }

  switch (reparentStrategy) {
    case 'REPARENT_AS_ABSOLUTE': {
      const basicCommads = getAbsoluteReparentPropertyChanges(
        target,
        newParent,
        originalContextMetadata,
        currentMetadata,
        projectContents,
        'force-pins',
      )

      const strategyCommands = runReparentPropertyStrategies([
        stripPinsConvertToVisualSize(elementPathSnapshot, metadataSnapshot),
        convertRelativeSizingToVisualSize(elementPathSnapshot, metadataSnapshot),
        setZIndexOnPastedElement(elementPathSnapshot, newParent, metadataSnapshot),
        convertFragmentLikeChildrenToVisualSize(
          elementPathSnapshot,
          metadataSnapshot,
          oldAllElementProps,
          childPathLookup,
          newParent,
          reparentStrategy,
          projectContents,
          [stripPinsConvertToVisualSize, convertRelativeSizingToVisualSize],
        ),
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
        stripPinsConvertToVisualSize(elementPathSnapshot, metadataSnapshot),
        convertRelativeSizingToVisualSize(elementPathSnapshot, metadataSnapshot),
        convertSizingToVisualSizeWhenPastingFromFlexToFlex(
          elementPathSnapshot,
          metadataSnapshot,
          newParent,
        ),
        convertFragmentLikeChildrenToVisualSize(
          elementPathSnapshot,
          metadataSnapshot,
          oldAllElementProps,
          childPathLookup,
          newParent,
          reparentStrategy,
          projectContents,
          [
            stripPinsConvertToVisualSize,
            convertRelativeSizingToVisualSize,
            convertSizingToVisualSizeWhenPastingFromFlexToFlex,
          ],
        ),
      ])

      return [...basicCommads, ...strategyCommands]
    }
    default:
      assertNever(reparentStrategy)
  }
}
