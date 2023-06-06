import { getAllUniqueUids } from '../../../../core/model/get-unique-ids'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { fixUtopiaElement } from '../../../../core/shared/uid-utils'
import { assertNever } from '../../../../core/shared/utils'
import { ReparentTargetForPaste, getTargetParentForPaste } from '../../../../utils/clipboard'
import { front } from '../../../../utils/utils'
import {
  absolutePositionForPaste,
  insertWithReparentStrategies,
} from '../../../editor/actions/actions'
import { CanvasCommand } from '../../commands/commands'
import { updateSelectedViews } from '../../commands/update-selected-views-command'
import {
  CanvasStrategy,
  InteractionCanvasState,
  emptyStrategyApplicationResult,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'
import {
  StaticReparentTarget,
  reparentStrategyForPaste,
} from './reparent-helpers/reparent-strategy-helpers'
import { elementToReparent } from './reparent-utils'

const PasteModes = ['replace', 'preserve'] as const
type PasteMode = typeof PasteModes[number]

export const PasteWithPropertiesReplacedStrategyId = 'PasteWithPropertiesReplacedStrategy'
export const PasteWithPropertiesPreservedStrategyId = 'PasteWithPropertiesPreservedStrategy'

export const pasteStrategy =
  (mode: PasteMode) =>
  (
    canvasState: InteractionCanvasState,
    interactionSession: InteractionSession | null,
  ): CanvasStrategy | null => {
    if (interactionSession?.interactionData.type !== 'STATIC_REPARENT') {
      return null
    }

    const strategyId =
      mode === 'preserve'
        ? PasteWithPropertiesPreservedStrategyId
        : mode === 'replace'
        ? PasteWithPropertiesReplacedStrategyId
        : assertNever(mode)

    const name =
      mode === 'preserve'
        ? 'Paste with properties preserved'
        : mode === 'replace'
        ? 'Paste with properties replaced'
        : assertNever(mode)

    const fitness = mode === 'preserve' ? 1 : mode === 'replace' ? 2 : assertNever(mode)

    return {
      id: strategyId,
      name: name,
      fitness: fitness,
      controlsToRender: [],
      apply: () => {
        if (
          interactionSession?.interactionData.type !== 'STATIC_REPARENT' ||
          canvasState.interactionTarget.type !== 'TARGET_PATHS'
        ) {
          return emptyStrategyApplicationResult
        }

        const elements =
          mode === 'preserve'
            ? interactionSession.interactionData.elementsWithPropsPreserved
            : mode === 'replace'
            ? interactionSession.interactionData.elementsWithPropsReplaced
            : assertNever(mode)

        const target = getTargetParentForPaste(
          canvasState.projectContents,
          canvasState.interactionTarget.elements,
          canvasState.nodeModules,
          canvasState.openFile,
          canvasState.startingMetadata,
          [], // TODO
          {
            elements: elements,
            originalContextMetadata:
              interactionSession.interactionData.targetOriginalContextMetadata,
            originalContextElementPathTrees:
              interactionSession.interactionData.targetOriginalPathTrees,
          },
        )
        if (target == null) {
          return emptyStrategyApplicationResult
        }

        // when targeting a conditional, wrap multiple elements into a fragment
        // TODO

        const strategy = reparentStrategyForPaste(
          canvasState.startingMetadata,
          canvasState.startingAllElementProps,
          canvasState.startingElementPathTree,
          target.parentPath.intendedParentPath,
        )

        const currentValue = elements[0] // TODO

        const existingIDs = getAllUniqueUids(canvasState.projectContents).allIDs
        const elementWithUniqueUID = fixUtopiaElement(
          currentValue.element,
          new Set(existingIDs),
        ).value

        const reparentTarget: StaticReparentTarget =
          strategy === 'REPARENT_AS_ABSOLUTE'
            ? {
                strategy: strategy,
                insertionPath: target.parentPath,
                intendedCoordinates: absolutePositionForPaste(
                  target,
                  currentValue.originalElementPath,
                  {
                    originalTargetMetadata:
                      interactionSession.interactionData.targetOriginalContextMetadata,
                    currentMetadata: canvasState.startingMetadata,
                    originalPathTrees: interactionSession.interactionData.targetOriginalPathTrees,
                    currentPathTrees: canvasState.startingElementPathTree,
                  },
                  interactionSession.interactionData.canvasViewportCenter,
                ),
              }
            : { strategy: strategy, insertionPath: target.parentPath }

        const result = insertWithReparentStrategies(
          {
            jsxMetadata: canvasState.startingMetadata,
            elementPathTrees: canvasState.startingElementPathTree,
            projectContents: canvasState.projectContents,
            nodeModules: canvasState.nodeModules,
            openFileName: canvasState.openFile ?? null,
          },
          interactionSession.interactionData.targetOriginalContextMetadata,
          interactionSession.interactionData.targetOriginalPathTrees,
          reparentTarget,
          {
            elementPath: currentValue.originalElementPath,
            pathToReparent: elementToReparent(elementWithUniqueUID, currentValue.importsToAdd),
          },
          front(),
          canvasState.builtInDependencies,
        )

        if (result == null) {
          return emptyStrategyApplicationResult
        }

        return strategyApplicationResult([
          ...result.commands,
          updateSelectedViews('always', [result.newPath]),
        ])
      },
    }
  }
