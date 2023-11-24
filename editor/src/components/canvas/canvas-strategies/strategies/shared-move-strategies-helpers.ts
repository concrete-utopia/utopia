import { styleStringInArray } from '../../../../utils/common-constants'
import { isHorizontalPoint } from 'utopia-api/core'
import { getLayoutProperty } from '../../../../core/layout/getLayoutProperty'
import { framePointForPinnedProp } from '../../../../core/layout/layout-helpers-new'
import type { PropsOrJSXAttributes } from '../../../../core/model/element-metadata-utils'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { isRight, right } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import type {
  ElementInstanceMetadataMap,
  JSXElement,
} from '../../../../core/shared/element-template'
import type { CanvasPoint, CanvasRectangle, CanvasVector } from '../../../../core/shared/math-utils'
import {
  boundingRectangleArray,
  zeroRectIfNullOrInfinity,
  canvasRectangleToLocalRectangle,
  canvasVector,
  nullIfInfinity,
  offsetRect,
  roundRectangleToNearestWhole,
  zeroCanvasPoint,
  zeroCanvasRect,
} from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'

import { getJSXElementFromProjectContents } from '../../../editor/store/editor-state'
import { stylePropPathMappingFn } from '../../../inspector/common/property-path-hooks'
import { determineConstrainedDragAxis } from '../../canvas-controls-frame'
import type { CanvasFrameAndTarget } from '../../canvas-types'
import { CSSCursor } from '../../canvas-types'
import type { AdjustCssLengthProperties } from '../../commands/adjust-css-length-command'
import {
  adjustCssLengthProperties,
  lengthPropertyToAdjust,
} from '../../commands/adjust-css-length-command'
import type { CanvasCommand } from '../../commands/commands'
import { pushIntendedBoundsAndUpdateGroups } from '../../commands/push-intended-bounds-and-update-groups-command'
import { setCursorCommand } from '../../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { setSnappingGuidelines } from '../../commands/set-snapping-guidelines-command'
import { updateHighlightedViews } from '../../commands/update-highlighted-views-command'
import {
  collectParentAndSiblingGuidelines,
  gatherParentAndSiblingTargets,
  runLegacyAbsoluteMoveSnapping,
} from '../../controls/guideline-helpers'
import type {
  ConstrainedDragAxis,
  GuidelineWithRelevantPoints,
  GuidelineWithSnappingVectorAndPointsOfRelevance,
} from '../../guideline'
import type { InteractionCanvasState, StrategyApplicationResult } from '../canvas-strategy-types'
import { emptyStrategyApplicationResult, strategyApplicationResult } from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import type { AbsolutePin } from './resize-helpers'
import type { FlexDirection } from '../../../inspector/common/css-utils'
import { memoize } from '../../../../core/shared/memoize'
import { is } from '../../../../core/shared/equality-utils'
import type { ProjectContentTreeRoot } from '../../../../components/assets'
import type { InspectorStrategy } from '../../../../components/inspector/inspector-strategies/inspector-strategy'
import { rectangleToSixFramePoints } from '../../commands/utils/group-resize-utils'
import invariant from '../../../../third-party/remix/invariant'
import type { SetCssLengthProperty } from '../../commands/set-css-length-command'
import {
  setCssLengthProperty,
  setValueKeepingOriginalUnit,
} from '../../commands/set-css-length-command'
import type { ActiveFrame, ActiveFrameAction } from '../../commands/set-active-frames-command'
import { activeFrameTargetRect, setActiveFrames } from '../../commands/set-active-frames-command'

export interface MoveCommandsOptions {
  ignoreLocalFrame?: boolean
}

export const getAdjustMoveCommands =
  (
    targets: Array<ElementPath>,
    canvasState: InteractionCanvasState,
    interactionSession: InteractionSession,
    options?: MoveCommandsOptions,
  ) =>
  (
    snappedDragVector: CanvasPoint,
  ): {
    commands: Array<SetCssLengthProperty | AdjustCssLengthProperties>
    intendedBounds: Array<CanvasFrameAndTarget>
  } => {
    const filteredSelectedElements = flattenSelection(targets)
    let commands: Array<SetCssLengthProperty | AdjustCssLengthProperties> = []
    let intendedBounds: Array<CanvasFrameAndTarget> = []
    filteredSelectedElements.forEach((selectedElement) => {
      const elementResult = getInteractionMoveCommandsForSelectedElement(
        selectedElement,
        snappedDragVector,
        canvasState,
        interactionSession,
        options,
      )
      commands.push(...elementResult.commands)
      intendedBounds.push(...elementResult.intendedBounds)
    })
    return { commands, intendedBounds }
  }

export function applyMoveCommon(
  originalTargets: Array<ElementPath>,
  targets: Array<ElementPath>,
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession,
  getMoveCommands: (snappedDragVector: CanvasPoint) => {
    commands: Array<CanvasCommand>
    intendedBounds: Array<CanvasFrameAndTarget>
  },
  action: ActiveFrameAction,
): StrategyApplicationResult {
  if (
    interactionSession.interactionData.type === 'DRAG' &&
    interactionSession.interactionData.drag != null
  ) {
    const drag = interactionSession.interactionData.drag
    const shiftKeyPressed = interactionSession.interactionData.modifiers.shift
    const cmdKeyPressed = interactionSession.interactionData.modifiers.cmd

    function getActiveFrames(intendedBounds: CanvasFrameAndTarget[]): ActiveFrame[] {
      return intendedBounds.map((b) => {
        const originalTarget = targets.find((t) => EP.toUid(t) === EP.toUid(b.target))
        const source =
          originalTarget != null
            ? zeroRectIfNullOrInfinity(
                MetadataUtils.getFrameInCanvasCoords(originalTarget, canvasState.startingMetadata),
              )
            : zeroCanvasRect
        return {
          action: action,
          target: activeFrameTargetRect(b.frame),
          source: source,
        }
      })
    }

    if (cmdKeyPressed) {
      const commandsForSelectedElements = getMoveCommands(drag)

      return strategyApplicationResult([
        ...commandsForSelectedElements.commands,
        pushIntendedBoundsAndUpdateGroups(
          commandsForSelectedElements.intendedBounds,
          'starting-metadata',
        ),
        updateHighlightedViews('mid-interaction', []),
        setElementsToRerenderCommand(targets),
        setCursorCommand(CSSCursor.Select),
        setActiveFrames(getActiveFrames(commandsForSelectedElements.intendedBounds)),
      ])
    } else {
      const constrainedDragAxis =
        shiftKeyPressed && drag != null ? determineConstrainedDragAxis(drag) : null
      const targetsForSnapping = originalTargets.map(
        (path) => interactionSession.updatedTargetPaths[EP.toString(path)] ?? path,
      )
      const snapTargets: ElementPath[] = gatherParentAndSiblingTargets(
        canvasState.startingMetadata,
        canvasState.startingAllElementProps,
        canvasState.startingElementPathTree,
        targetsForSnapping,
      )
      const moveGuidelines = collectParentAndSiblingGuidelines(
        snapTargets,
        canvasState.startingMetadata,
      )

      const { snappedDragVector, guidelinesWithSnappingVector } = snapDrag(
        drag,
        constrainedDragAxis,
        canvasState.startingMetadata,
        targets,
        moveGuidelines,
        canvasState.scale,
      )
      const commandsForSelectedElements = getMoveCommands(snappedDragVector)
      return strategyApplicationResult([
        ...commandsForSelectedElements.commands,
        updateHighlightedViews('mid-interaction', []),
        setSnappingGuidelines('mid-interaction', guidelinesWithSnappingVector),
        pushIntendedBoundsAndUpdateGroups(
          commandsForSelectedElements.intendedBounds,
          'starting-metadata',
        ),
        setElementsToRerenderCommand([...targets, ...targetsForSnapping]),
        setCursorCommand(CSSCursor.Select),
        setActiveFrames(getActiveFrames(commandsForSelectedElements.intendedBounds)),
      ])
    }
  } else {
    // Fallback for when the checks above are not satisfied.
    return emptyStrategyApplicationResult
  }
}

function getAppropriateLocalFrame(
  options: MoveCommandsOptions | undefined,
  selectedElement: ElementPath,
  startingMetadata: ElementInstanceMetadataMap,
) {
  return options?.ignoreLocalFrame
    ? null
    : MetadataUtils.getLocalFrameFromSpecialSizeMeasurements(selectedElement, startingMetadata)
}

export function getDirectMoveCommandsForSelectedElement(
  projectContents: ProjectContentTreeRoot,
  startingMetadata: ElementInstanceMetadataMap,
  selectedElement: ElementPath,
  mappedPath: ElementPath,
  leftOrTop: 'left' | 'top',
  newPixelValue: number,
  options?: MoveCommandsOptions,
): {
  commands: Array<SetCssLengthProperty | AdjustCssLengthProperties>
  intendedBounds: Array<CanvasFrameAndTarget>
} {
  const localFrame = getAppropriateLocalFrame(options, selectedElement, startingMetadata)

  const drag = canvasVector({
    x: leftOrTop === 'left' ? newPixelValue - (localFrame?.x ?? 0) : 0,
    y: leftOrTop === 'top' ? newPixelValue - (localFrame?.y ?? 0) : 0,
  })

  return getMoveCommandsForSelectedElement(
    projectContents,
    startingMetadata,
    selectedElement,
    mappedPath,
    drag,
  )
}

export function getMoveCommandsForSelectedElement(
  projectContents: ProjectContentTreeRoot,
  startingMetadata: ElementInstanceMetadataMap,
  selectedElement: ElementPath,
  mappedPath: ElementPath,
  drag: CanvasVector,
  options?: MoveCommandsOptions,
): {
  commands: Array<SetCssLengthProperty | AdjustCssLengthProperties>
  intendedBounds: Array<CanvasFrameAndTarget>
} {
  const element: JSXElement | null = getJSXElementFromProjectContents(
    selectedElement,
    projectContents,
  )

  const elementMetadata = MetadataUtils.findElementByElementPath(
    startingMetadata, // TODO should this be using the current metadata?
    selectedElement,
  )

  const elementParentBounds =
    elementMetadata?.specialSizeMeasurements.coordinateSystemBounds ?? null

  const globalFrame = nullIfInfinity(
    MetadataUtils.getFrameInCanvasCoords(selectedElement, startingMetadata),
  )

  invariant(
    globalFrame != null,
    `Error in changeBounds: the ${EP.toString(
      selectedElement,
    )} element's global frame was null or infinity`,
  )
  invariant(
    elementParentBounds != null,
    `Error in changeBounds: the ${EP.toString(
      selectedElement,
    )} element's coordinateSystemBounds was null`,
  )

  if (element == null) {
    return { commands: [], intendedBounds: [] }
  }

  if (options?.ignoreLocalFrame === true) {
    return createMoveCommandsForElementPositionRelative(
      element,
      selectedElement,
      mappedPath,
      drag,
      globalFrame,
      elementParentBounds,
      elementMetadata?.specialSizeMeasurements.parentFlexDirection ?? null,
    )
  }

  return createMoveCommandsForElementCreatingMissingPins(
    element,
    selectedElement,
    mappedPath,
    drag,
    globalFrame,
    elementParentBounds,
    elementMetadata?.specialSizeMeasurements.parentFlexDirection ?? null,
  )
}

export function getInteractionMoveCommandsForSelectedElement(
  selectedElement: ElementPath,
  drag: CanvasVector,
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession,
  options?: MoveCommandsOptions,
): {
  commands: Array<SetCssLengthProperty | AdjustCssLengthProperties>
  intendedBounds: Array<CanvasFrameAndTarget>
} {
  const mappedPath =
    interactionSession.updatedTargetPaths[EP.toString(selectedElement)] ?? selectedElement

  return getMoveCommandsForSelectedElement(
    canvasState.projectContents,
    canvasState.startingMetadata,
    selectedElement,
    mappedPath,
    drag,
    options,
  )
}

export function moveInspectorStrategy(
  metadata: ElementInstanceMetadataMap,
  selectedElementPaths: ElementPath[],
  projectContents: ProjectContentTreeRoot,
  movement: CanvasVector,
): InspectorStrategy {
  return {
    name: 'Move by pixels',
    strategy: () => {
      let commands: Array<CanvasCommand> = []
      let intendedBounds: Array<CanvasFrameAndTarget> = []
      for (const selectedPath of selectedElementPaths) {
        const moveCommandsResult = getMoveCommandsForSelectedElement(
          projectContents,
          metadata,
          selectedPath,
          selectedPath,
          movement,
        )
        commands.push(...moveCommandsResult.commands)
        intendedBounds.push(...moveCommandsResult.intendedBounds)
      }
      commands.push(pushIntendedBoundsAndUpdateGroups(intendedBounds, 'live-metadata'))
      commands.push(setElementsToRerenderCommand(selectedElementPaths))
      return commands
    },
  }
}

export function directMoveInspectorStrategy(
  metadata: ElementInstanceMetadataMap,
  selectedElementPaths: ElementPath[],
  projectContents: ProjectContentTreeRoot,
  leftOrTop: 'left' | 'top',
  newPixelValue: number,
): InspectorStrategy {
  return {
    name: 'Move to a pixel position',
    strategy: () => {
      let commands: Array<CanvasCommand> = []
      let intendedBounds: Array<CanvasFrameAndTarget> = []
      for (const selectedPath of selectedElementPaths) {
        const moveCommandsResult = getDirectMoveCommandsForSelectedElement(
          projectContents,
          metadata,
          selectedPath,
          selectedPath,
          leftOrTop,
          newPixelValue,
        )
        commands.push(...moveCommandsResult.commands)
        intendedBounds.push(...moveCommandsResult.intendedBounds)
      }
      commands.push(pushIntendedBoundsAndUpdateGroups(intendedBounds, 'live-metadata'))
      commands.push(setElementsToRerenderCommand(selectedElementPaths))
      return commands
    },
  }
}

export function createMoveCommandsForElementPositionRelative(
  element: JSXElement,
  selectedElement: ElementPath,
  mappedPath: ElementPath,
  drag: CanvasVector,
  globalFrame: CanvasRectangle | null,
  elementParentBounds: CanvasRectangle | null,
  elementParentFlexDirection: FlexDirection | null,
): {
  commands: Array<AdjustCssLengthProperties>
  intendedBounds: Array<CanvasFrameAndTarget>
} {
  const { existingPins, extendedPins } = ensureAtLeastOnePinPerDimension(right(element.props))

  const adjustPinProperties = extendedPins.map((pin) => {
    const horizontal = isHorizontalPoint(
      // TODO avoid using the loaded FramePoint enum
      framePointForPinnedProp(pin),
    )
    const negative = pin === 'right' || pin === 'bottom'

    const updatedPropValue = (horizontal ? drag.x : drag.y) * (negative ? -1 : 1)
    const parentDimension = horizontal ? elementParentBounds?.width : elementParentBounds?.height

    return lengthPropertyToAdjust(
      stylePropPathMappingFn(pin, styleStringInArray),
      updatedPropValue,
      parentDimension,
      'create-if-not-existing',
    )
  }, extendedPins)

  const adjustPinCommand = adjustCssLengthProperties(
    'always',
    selectedElement,
    elementParentFlexDirection,
    adjustPinProperties,
  )
  const intendedBounds = (() => {
    if (globalFrame == null) {
      return []
    } else {
      const intendedGlobalFrame = roundRectangleToNearestWhole(offsetRect(globalFrame, drag))
      return [{ target: mappedPath, frame: intendedGlobalFrame }]
    }
  })()

  return { commands: [adjustPinCommand], intendedBounds: intendedBounds }
}

export function createMoveCommandsForElementCreatingMissingPins(
  element: JSXElement,
  selectedElement: ElementPath,
  mappedPath: ElementPath,
  drag: CanvasVector,
  globalFrame: CanvasRectangle,
  elementParentBounds: CanvasRectangle,
  elementParentFlexDirection: FlexDirection | null,
): {
  commands: Array<SetCssLengthProperty>
  intendedBounds: Array<CanvasFrameAndTarget>
} {
  const { extendedPins } = ensureAtLeastOnePinPerDimension(right(element.props))
  const pinsOnlyForDimensionThatChanged = (() => {
    const filteredPins: Array<AbsolutePin> = []
    if (drag.x !== 0) {
      // only vertical pins
      filteredPins.push(...extendedPins.filter((p) => horizontalPins.includes(p)))
    }
    if (drag.y !== 0) {
      // only vertical pins
      filteredPins.push(...extendedPins.filter((p) => verticalPins.includes(p)))
    }
    return filteredPins
  })()

  const intendedGlobalFrame = roundRectangleToNearestWhole(offsetRect(globalFrame, drag))

  const intendedLocalFullFrame = rectangleToSixFramePoints(
    canvasRectangleToLocalRectangle(intendedGlobalFrame, elementParentBounds),
    elementParentBounds,
  )

  const setCssLengthPropertyCommands = mapDropNulls((pin) => {
    const horizontal = isHorizontalPoint(
      // TODO avoid using the loaded FramePoint enum
      framePointForPinnedProp(pin),
    )

    const adjustedValue = intendedLocalFullFrame[pin]

    return setCssLengthProperty(
      'always',
      selectedElement,
      stylePropPathMappingFn(pin, styleStringInArray),
      setValueKeepingOriginalUnit(
        adjustedValue,
        horizontal ? elementParentBounds?.width : elementParentBounds?.height,
      ),
      elementParentFlexDirection,

      'create-if-not-existing',
      'warn-about-replacement',
    )
  }, pinsOnlyForDimensionThatChanged)

  const intendedBounds = (() => {
    if (globalFrame == null) {
      return []
    } else {
      return [{ target: mappedPath, frame: intendedGlobalFrame }]
    }
  })()

  return { commands: setCssLengthPropertyCommands, intendedBounds: intendedBounds }
}

export function getMultiselectBounds(
  jsxMetadata: ElementInstanceMetadataMap,
  selectedElements: Array<ElementPath>,
): CanvasRectangle | null {
  const frames = mapDropNulls((element) => {
    return nullIfInfinity(MetadataUtils.getFrameInCanvasCoords(element, jsxMetadata))
  }, selectedElements)

  return boundingRectangleArray(frames)
}

export const flattenSelection = memoize(flattenSelectionInner, {
  maxSize: 1,
  matchesArg: is,
})

// No need to include descendants in multiselection when dragging
// Note: this maybe slow when there are lot of selected views
function flattenSelectionInner(selectedViews: Array<ElementPath>): Array<ElementPath> {
  const filteredTargets = selectedViews.filter((view) =>
    selectedViews.every((otherView) => !EP.isDescendantOf(view, otherView)),
  )

  return filteredTargets.length === selectedViews.length ? selectedViews : filteredTargets
}

export function snapDrag(
  drag: CanvasPoint | null,
  constrainedDragAxis: ConstrainedDragAxis | null,
  jsxMetadata: ElementInstanceMetadataMap,
  selectedElements: Array<ElementPath>,
  moveGuidelines: Array<GuidelineWithRelevantPoints>,
  canvasScale: number,
): {
  snappedDragVector: CanvasPoint
  guidelinesWithSnappingVector: Array<GuidelineWithSnappingVectorAndPointsOfRelevance>
} {
  if (drag == null) {
    return {
      snappedDragVector: zeroCanvasPoint,
      guidelinesWithSnappingVector: [],
    }
  }

  const multiselectBounds = getMultiselectBounds(jsxMetadata, selectedElements)

  // This is the entry point to extend the list of snapping strategies, if we want to add more

  const { snappedDragVector, guidelinesWithSnappingVector } = runLegacyAbsoluteMoveSnapping(
    drag,
    constrainedDragAxis,
    moveGuidelines,
    canvasScale,
    multiselectBounds,
  )

  return { snappedDragVector, guidelinesWithSnappingVector }
}

const horizontalPins: Array<AbsolutePin> = ['left', 'right']
const verticalPins: Array<AbsolutePin> = ['top', 'bottom']

function ensureAtLeastOnePinPerDimension(props: PropsOrJSXAttributes): {
  existingPins: Array<AbsolutePin>
  extendedPins: Array<AbsolutePin>
} {
  const existingHorizontalPins = horizontalPins.filter((p) => {
    const prop = getLayoutProperty(p, props, styleStringInArray)
    return isRight(prop) && prop.value != null
  })
  const existingVerticalPins = verticalPins.filter((p) => {
    const prop = getLayoutProperty(p, props, styleStringInArray)
    return isRight(prop) && prop.value != null
  })

  const horizontalPinsToAdd: Array<AbsolutePin> = [...existingHorizontalPins]
  if (existingHorizontalPins.length === 0) {
    horizontalPinsToAdd.push('left')
  }

  const verticalPinsToAdd: Array<AbsolutePin> = [...existingVerticalPins]
  if (existingVerticalPins.length === 0) {
    verticalPinsToAdd.push('top')
  }

  return {
    existingPins: [...existingHorizontalPins, ...existingVerticalPins],
    extendedPins: [...horizontalPinsToAdd, ...verticalPinsToAdd],
  }
}

export function areAllSelectedElementsNonAbsolute(
  selectedElements: Array<ElementPath>,
  metadata: ElementInstanceMetadataMap,
): boolean {
  if (selectedElements.length > 0) {
    return selectedElements.every((element) => {
      const elementMetadata = MetadataUtils.findElementByElementPath(metadata, element)
      return !MetadataUtils.isPositionAbsolute(elementMetadata)
    })
  } else {
    return false
  }
}
