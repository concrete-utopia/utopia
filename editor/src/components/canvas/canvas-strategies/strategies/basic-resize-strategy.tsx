import { styleStringInArray } from '../../../../utils/common-constants'
import { getLayoutProperty } from '../../../../core/layout/getLayoutProperty'
import { MetadataUtils, PropsOrJSXAttributes } from '../../../../core/model/element-metadata-utils'
import { Either, foldEither, isLeft, isRight, right } from '../../../../core/shared/either'
import { ElementInstanceMetadata, isJSXElement } from '../../../../core/shared/element-template'
import {
  CanvasPoint,
  canvasRectangle,
  CanvasRectangle,
  isInfinityRectangle,
  offsetPoint,
} from '../../../../core/shared/math-utils'
import { stylePropPathMappingFn } from '../../../inspector/common/property-path-hooks'
import { EdgePosition, oppositeEdgePosition } from '../../canvas-types'
import {
  isEdgePositionACorner,
  isEdgePositionAHorizontalEdge,
  pickPointOnRect,
} from '../../canvas-utils'
import {
  adjustCssLengthProperties,
  lengthPropertyToAdjust,
  LengthPropertyToAdjust,
} from '../../commands/adjust-css-length-command'
import { setCursorCommand } from '../../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { updateHighlightedViews } from '../../commands/update-highlighted-views-command'
import { ImmediateParentBounds } from '../../controls/parent-bounds'
import { ImmediateParentOutlines } from '../../controls/parent-outlines'
import { AbsoluteResizeControl } from '../../controls/select-mode/absolute-resize-control'
import { ZeroSizeResizeControlWrapper } from '../../controls/zero-sized-element-controls'
import {
  CanvasStrategy,
  controlWithProps,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  InteractionLifecycle,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'
import {
  elementToTargetToUpdateProp,
  getLockedAspectRatio,
  isAnySelectedElementAspectRatioLocked,
  pickCursorFromEdgePosition,
  resizeBoundingBox,
  ToUpdatePropResult,
} from './resize-helpers'
import { CSSNumber } from '../../../../components/inspector/common/css-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { CanvasCommand } from '../../commands/commands'
import * as EP from '../../../../core/shared/element-path'

export const BASIC_RESIZE_STRATEGY_ID = 'BASIC_RESIZE'

export function basicResizeStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): CanvasStrategy | null {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)

  if (selectedElements.length !== 1) {
    return null
  }
  const targetedElement = selectedElements[0]

  const widthToTarget = elementToTargetToUpdateProp(
    canvasState.projectContents,
    canvasState.startingMetadata,
    targetedElement,
    'width',
  )
  const heightToTarget = elementToTargetToUpdateProp(
    canvasState.projectContents,
    canvasState.startingMetadata,
    targetedElement,
    'height',
  )

  const metadata = MetadataUtils.findElementByElementPath(
    canvasState.startingMetadata,
    targetedElement,
  )
  const elementDimensions = getElementDimensions(
    widthToTarget.propertyValue,
    heightToTarget.propertyValue,
  )

  const hasDimensions =
    elementDimensions != null &&
    (elementDimensions.width != null || elementDimensions.height != null)

  const elementParentBounds = metadata?.specialSizeMeasurements.immediateParentBounds ?? null
  const hasSizedParent =
    elementParentBounds != null &&
    (elementParentBounds.width !== 0 || elementParentBounds.height !== 0)

  return {
    id: BASIC_RESIZE_STRATEGY_ID,
    name: 'Resize (Basic)',
    controlsToRender: [
      controlWithProps({
        control: AbsoluteResizeControl,
        props: { targets: selectedElements },
        key: 'absolute-resize-control',
        show: 'always-visible',
      }),
      controlWithProps({
        control: ZeroSizeResizeControlWrapper,
        props: { targets: selectedElements },
        key: 'zero-size-resize-control',
        show: 'always-visible',
      }),
      controlWithProps({
        control: ImmediateParentOutlines,
        props: { targets: selectedElements },
        key: 'parent-outlines-control',
        show: 'visible-only-while-active',
      }),
      controlWithProps({
        control: ImmediateParentBounds,
        props: { targets: selectedElements },
        key: 'parent-bounds-control',
        show: 'visible-only-while-active',
      }),
    ],
    fitness:
      interactionSession != null &&
      interactionSession.interactionData.type === 'DRAG' &&
      interactionSession.activeControl.type === 'RESIZE_HANDLE' &&
      (hasDimensions || !hasSizedParent)
        ? 1
        : 0,
    apply: (_strategyLifecycle: InteractionLifecycle) => {
      if (
        interactionSession != null &&
        interactionSession.interactionData.type === 'DRAG' &&
        interactionSession.activeControl.type === 'RESIZE_HANDLE'
      ) {
        // no multiselection support yet
        const selectedElement = selectedElements[0]
        const edgePosition = interactionSession.activeControl.edgePosition
        if (interactionSession.interactionData.drag != null) {
          const drag = interactionSession.interactionData.drag
          const originalBounds = MetadataUtils.getFrameInCanvasCoords(
            selectedElement,
            canvasState.startingMetadata,
          )

          if (originalBounds == null || isInfinityRectangle(originalBounds)) {
            return emptyStrategyApplicationResult
          }

          if (metadata == null) {
            return emptyStrategyApplicationResult
          }

          const anySelectedElementAspectRatioLocked = isAnySelectedElementAspectRatioLocked(
            canvasState.startingMetadata,
            [selectedElement],
          )

          const resizedBounds = resizeBoundingBox(
            originalBounds,
            drag,
            edgePosition,
            getLockedAspectRatio(
              interactionSession,
              interactionSession.interactionData.modifiers,
              originalBounds,
              anySelectedElementAspectRatioLocked,
            ),
            'non-center-based',
          )

          let resizePropertiesByTarget: Array<{
            target: ElementPath
            properties: Array<LengthPropertyToAdjust>
          }> = []
          function addResizeProperty(
            name: 'width' | 'height',
            propResult: ToUpdatePropResult,
            original: number,
            resized: number,
            parent: number | undefined,
          ): void {
            if (propResult.element == null) {
              return
            }
            const propertyTargetElement = propResult.element
            const propValue = propResult.propertyValue
            if (
              isRight(propValue) &&
              propValue.value == null &&
              (original === resized || hasSizedParent)
            ) {
              return
            }
            let currentForTargetProperties: Array<LengthPropertyToAdjust>
            const entryForTarget = resizePropertiesByTarget.find((property) => {
              return EP.pathsEqual(property.target, propertyTargetElement)
            })
            if (entryForTarget == null) {
              currentForTargetProperties = []
              resizePropertiesByTarget.push({
                target: propertyTargetElement,
                properties: currentForTargetProperties,
              })
            } else {
              currentForTargetProperties = entryForTarget.properties
            }
            currentForTargetProperties.push(
              lengthPropertyToAdjust(
                stylePropPathMappingFn(name, styleStringInArray),
                isRight(propValue) && propValue != null ? resized - original : resized,
                parent,
                'create-if-not-existing',
              ),
            )
          }

          addResizeProperty(
            'width',
            widthToTarget,
            originalBounds.width,
            resizedBounds.width,
            elementParentBounds?.width,
          )
          addResizeProperty(
            'height',
            heightToTarget,
            originalBounds.height,
            resizedBounds.height,
            elementParentBounds?.height,
          )
          let commands: Array<CanvasCommand> = []
          for (const resizePropertiesForTarget of resizePropertiesByTarget) {
            commands.push(
              adjustCssLengthProperties(
                'always',
                resizePropertiesForTarget.target,
                null,
                resizePropertiesForTarget.properties,
              ),
            )
          }
          commands.push(updateHighlightedViews('mid-interaction', []))
          commands.push(setCursorCommand(pickCursorFromEdgePosition(edgePosition)))
          commands.push(setElementsToRerenderCommand(selectedElements))

          return strategyApplicationResult(commands)
        } else {
          return strategyApplicationResult([
            updateHighlightedViews('mid-interaction', []),
            setCursorCommand(pickCursorFromEdgePosition(edgePosition)),
          ])
        }
      }
      // Fallback for when the checks above are not satisfied.
      return emptyStrategyApplicationResult
    },
  }
}

export function resizeWidthHeight(
  boundingBox: CanvasRectangle,
  drag: CanvasPoint,
  edgePosition: EdgePosition,
): CanvasRectangle {
  if (isEdgePositionACorner(edgePosition)) {
    const oppositeCornerPosition = {
      x: 1 - edgePosition.x,
      y: 1 - edgePosition.y,
    } as EdgePosition

    let oppositeCorner = pickPointOnRect(boundingBox, oppositeCornerPosition)
    const draggedCorner = pickPointOnRect(boundingBox, edgePosition)
    const newCorner = offsetPoint(draggedCorner, drag)

    const newWidth = Math.abs(oppositeCorner.x - newCorner.x)
    const newHeight = Math.abs(oppositeCorner.y - newCorner.y)

    return canvasRectangle({
      x: boundingBox.x,
      y: boundingBox.y,
      width: newWidth,
      height: newHeight,
    })
  } else {
    const isEdgeHorizontalSide = isEdgePositionAHorizontalEdge(edgePosition)

    const oppositeSideCenterPosition = oppositeEdgePosition(edgePosition)

    const oppositeSideCenter = pickPointOnRect(boundingBox, oppositeSideCenterPosition)
    const draggedSideCenter = pickPointOnRect(boundingBox, edgePosition)

    if (isEdgeHorizontalSide) {
      const newHeight = Math.abs(oppositeSideCenter.y - (draggedSideCenter.y + drag.y))
      return canvasRectangle({
        x: boundingBox.x,
        y: boundingBox.y,
        width: boundingBox.width,
        height: newHeight,
      })
    } else {
      const newWidth = Math.abs(oppositeSideCenter.x - (draggedSideCenter.x + drag.x))
      return canvasRectangle({
        x: boundingBox.x,
        y: boundingBox.y,
        width: newWidth,
        height: boundingBox.height,
      })
    }
  }
}

type ElementDimensions = {
  width: number | null
  height: number | null
} | null

type DimensionProp = Either<string, CSSNumber | undefined>

function getDimensionPropValue(prop: DimensionProp): number | null {
  return foldEither(
    (_) => null,
    (v) => v?.value ?? null,
    prop,
  )
}

function getElementDimensions(
  widthProp: DimensionProp,
  heightProp: DimensionProp,
): ElementDimensions {
  return {
    width: getDimensionPropValue(widthProp),
    height: getDimensionPropValue(heightProp),
  }
}
