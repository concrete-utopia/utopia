import type { CanvasVector } from '../../../../core/shared/math-utils'
import {
  canvasRectangle,
  canvasVector,
  offsetPoint,
  scaleVector,
  zeroRectangle,
} from '../../../../core/shared/math-utils'
import type { KeyCharacter } from '../../../../utils/keyboard'
import Keyboard from '../../../../utils/keyboard'
import type { CanvasFrameAndTarget, EdgePosition } from '../../canvas-types'
import { EdgePositionBottom, EdgePositionRight } from '../../canvas-types'
import type { CanvasCommand } from '../../commands/commands'
import { pushIntendedBoundsAndUpdateGroups } from '../../commands/push-intended-bounds-and-update-groups-command'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { setSnappingGuidelines } from '../../commands/set-snapping-guidelines-command'
import { AbsoluteResizeControl } from '../../controls/select-mode/absolute-resize-control'
import type { CanvasStrategy, InteractionCanvasState } from '../canvas-strategy-types'
import {
  controlWithProps,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import { resizeBoundingBox, supportsAbsoluteResize } from './resize-helpers'
import { childrenBoundsToSnapTo } from './shared-absolute-resize-strategy-helpers'
import { changeBounds } from './shared-absolute-resize-strategy-helpers'
import type { AccumulatedPresses } from './shared-keyboard-strategy-helpers'
import {
  accumulatePresses,
  getKeyboardStrategyGuidelines,
  getLastKeyPressState,
  getMovementDeltaFromKey,
} from './shared-keyboard-strategy-helpers'
import { getMultiselectBounds } from './shared-move-strategies-helpers'
import { retargetStrategyToChildrenOfFragmentLikeElements } from './fragment-like-helpers'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import type { ElementPathTrees } from '../../../../core/shared/element-path-tree'
import { gatherParentAndSiblingTargets } from '../../controls/guideline-helpers'
import { uniqBy } from '../../../../core/shared/array-utils'
import * as EP from '../../../../core/shared/element-path'
import type { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import type { AllElementProps } from '../../../editor/store/editor-state'
import { getDescriptiveStrategyLabelWithRetargetedPaths } from '../canvas-strategies'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'

interface VectorAndEdge {
  movement: CanvasVector
  edge: EdgePosition
}

function pressesToVectorAndEdges(
  accumulatedPresses: Array<AccumulatedPresses>,
  maxNegativeMovement: CanvasVector,
): Array<VectorAndEdge> {
  let result: Array<VectorAndEdge> = []

  accumulatedPresses.forEach((accumulatedPress) => {
    accumulatedPress.keysPressed.forEach((key) => {
      const keyPressMovement = scaleVector(
        getMovementDeltaFromKey(key, accumulatedPress.modifiers),
        accumulatedPress.count,
      )
      const edgePosition = getEdgePositionFromKey(key)
      if (edgePosition != null) {
        let foundVectorAndEdge: VectorAndEdge | null = null
        for (let vectorAndEdge of result) {
          if (vectorAndEdge.edge.x === edgePosition.x && vectorAndEdge.edge.y === edgePosition.y) {
            foundVectorAndEdge = vectorAndEdge
          }
        }
        if (foundVectorAndEdge == null) {
          result.push({
            movement: getLimitedMovement(keyPressMovement, maxNegativeMovement),
            edge: edgePosition,
          })
        } else {
          const accumulatedMovement = offsetPoint(foundVectorAndEdge.movement, keyPressMovement)
          foundVectorAndEdge.movement = getLimitedMovement(accumulatedMovement, maxNegativeMovement)
        }
      }
    })
  })

  return result
}

function getLimitedMovement(
  movement: CanvasVector,
  maxNegativeMovement: CanvasVector,
): CanvasVector {
  return {
    x: Math.max(movement.x, maxNegativeMovement.x),
    y: Math.max(movement.y, maxNegativeMovement.y),
  } as CanvasVector
}

function getFitness(interactionSession: InteractionSession | null): number {
  if (interactionSession != null && interactionSession.interactionData.type === 'KEYBOARD') {
    const lastKeyState = getLastKeyPressState(
      interactionSession.interactionData.keyStates,
      Keyboard.keyIsArrow,
    )
    if (lastKeyState != null) {
      const cmdAndOptionallyShiftModifier =
        lastKeyState.modifiers.cmd && !lastKeyState.modifiers.alt && !lastKeyState.modifiers.ctrl

      if (cmdAndOptionallyShiftModifier) {
        return 1
      }
    }
  }

  return 0
}

export const ResizeMinimumValue = 1

export function keyboardAbsoluteResizeStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): CanvasStrategy | null {
  const { pathsWereReplaced, paths: selectedElements } =
    retargetStrategyToChildrenOfFragmentLikeElements(canvasState)

  if (
    selectedElements.length === 0 ||
    !selectedElements.every((element) => {
      return supportsAbsoluteResize(canvasState.startingMetadata, element, canvasState)
    }) ||
    selectedElements.some((t) =>
      MetadataUtils.isGridLayoutedContainer(
        MetadataUtils.findElementByElementPath(canvasState.startingMetadata, EP.parentPath(t)),
      ),
    )
  ) {
    return null
  }

  return {
    id: 'KEYBOARD_ABSOLUTE_RESIZE',
    name: 'Resize',
    descriptiveLabel: getDescriptiveStrategyLabelWithRetargetedPaths(
      'Resizing Elements',
      pathsWereReplaced,
    ),
    icon: {
      category: 'modalities',
      type: 'moveabs-large',
    },
    controlsToRender: [
      controlWithProps({
        control: AbsoluteResizeControl,
        props: { targets: selectedElements, pathsWereReplaced: pathsWereReplaced },
        key: 'absolute-resize-control',
        show: 'visible-except-when-other-strategy-is-active',
      }),
    ],
    fitness: getFitness(interactionSession),
    apply: () => {
      if (interactionSession != null && interactionSession.interactionData.type === 'KEYBOARD') {
        const originalFrame =
          getMultiselectBounds(canvasState.startingMetadata, selectedElements) ??
          canvasRectangle(zeroRectangle)
        const accumulatedPresses = accumulatePresses(interactionSession.interactionData.keyStates)
        const maxNegativeMovement = canvasVector({
          x: -originalFrame.width + ResizeMinimumValue,
          y: -originalFrame.height + ResizeMinimumValue,
        })
        const movementsWithEdges = pressesToVectorAndEdges(accumulatedPresses, maxNegativeMovement)

        // Start with the frame as it is at the start of the interaction.
        let newFrame = originalFrame

        let commands: Array<CanvasCommand> = []
        let intendedBounds: Array<CanvasFrameAndTarget> = []

        movementsWithEdges.forEach((movementWithEdge) => {
          newFrame = resizeBoundingBox(
            newFrame,
            movementWithEdge.movement,
            movementWithEdge.edge,
            null,
            'non-center-based',
          )

          const changeBoundsResult = changeBounds(
            canvasState.projectContents,
            canvasState.startingMetadata,
            selectedElements,
            originalFrame,
            intendedBounds,
            movementWithEdge.edge,
            movementWithEdge.movement,
          )
          intendedBounds = changeBoundsResult.intendedBounds
          commands.push(...changeBoundsResult.commands)
        })
        const parentsAndSiblings: ElementPath[] = gatherParentAndSiblingTargets(
          canvasState.startingMetadata,
          canvasState.startingAllElementProps,
          canvasState.startingElementPathTree,
          getTargetPathsFromInteractionTarget(canvasState.interactionTarget),
        )
        const children = getChildrenToSnapTo(
          deduplicateEdges(movementsWithEdges.map((m) => m.edge)),
          selectedElements,
          canvasState.startingMetadata,
          canvasState.startingAllElementProps,
          canvasState.startingElementPathTree,
        )
        const snapTargets = [...children, ...parentsAndSiblings]
        const guidelines = getKeyboardStrategyGuidelines(snapTargets, interactionSession, newFrame)
        commands.push(setSnappingGuidelines('mid-interaction', guidelines))
        commands.push(pushIntendedBoundsAndUpdateGroups(intendedBounds, 'starting-metadata'))
        commands.push(setElementsToRerenderCommand(selectedElements))
        return strategyApplicationResult(commands)
      } else {
        return emptyStrategyApplicationResult
      }
    },
  }
}

function getEdgePositionFromKey(key: KeyCharacter): EdgePosition | null {
  switch (key) {
    case 'left':
    case 'right':
      return EdgePositionRight
    case 'up':
    case 'down':
      return EdgePositionBottom
    default:
      return null
  }
}

function deduplicateEdges(edges: EdgePosition[]): EdgePosition[] {
  return uniqBy(edges, (l, r) => l.x === r.x && l.y === r.y)
}

function getChildrenToSnapTo(
  edgePositions: EdgePosition[],
  targets: Array<ElementPath>,
  componentMetadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
) {
  return uniqBy(
    edgePositions.flatMap((edge) =>
      childrenBoundsToSnapTo(edge, targets, componentMetadata, allElementProps, pathTrees),
    ),
    (l, r) => EP.toString(l) === EP.toString(r),
  )
}
