import * as EP from '../../../../core/shared/element-path'
import { generateUidWithExistingComponents } from '../../../../core/model/element-template-utils'
import { getAllUniqueUids } from '../../../../core/model/get-unique-ids'
import { jsxFragment } from '../../../../core/shared/element-template'
import { fixUtopiaElement } from '../../../../core/shared/uid-utils'
import { assertNever } from '../../../../core/shared/utils'
import { ElementPasteWithMetadata, getTargetParentForPaste } from '../../../../utils/clipboard'
import { absolute, front } from '../../../../utils/utils'
import { ProjectContentTreeRoot } from '../../../assets'
import { ElementPaste } from '../../../editor/action-types'
import {
  absolutePositionForPaste,
  insertWithReparentStrategies,
} from '../../../editor/actions/actions'
import {
  InsertionPath,
  isConditionalClauseInsertionPath,
} from '../../../editor/store/insertion-path'
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
import { updateFunctionCommand } from '../../commands/update-function-command'
import { foldAndApplyCommandsInner } from '../../commands/commands'
import { updateSelectedViews } from '../../commands/update-selected-views-command'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { isLeft } from '../../../../core/shared/either'
import { showToastCommand } from '../../commands/show-toast-command'

const PasteModes = ['replace', 'preserve'] as const
type PasteMode = typeof PasteModes[number]

export const PasteWithPropertiesReplacedStrategyId = 'PasteWithPropertiesReplacedStrategy'
export const PasteWithPropertiesPreservedStrategyId = 'PasteWithPropertiesPreservedStrategy'

export const pasteWithPropsReplacedStrategy = pasteMetaStrategy('replace')
export const pasteWithPropsPreservedStrategy = pasteMetaStrategy('preserve')

type StrategySlice = Pick<CanvasStrategy, 'fitness' | 'id' | 'name'>

export function pasteStrategy(
  interactionSession: InteractionSession,
  canvasState: InteractionCanvasState,
  elementPasteWithMetadata: ElementPasteWithMetadata,
  strategyProps: StrategySlice,
): CanvasStrategy {
  return {
    id: strategyProps.id,
    name: strategyProps.name,
    fitness: strategyProps.fitness,
    controlsToRender: [],
    apply: () => {
      if (
        interactionSession?.interactionData.type !== 'DISCRETE_REPARENT' ||
        canvasState.interactionTarget.type !== 'TARGET_PATHS'
      ) {
        return emptyStrategyApplicationResult
      }

      const target = getTargetParentForPaste(
        canvasState.projectContents,
        canvasState.interactionTarget.elements,
        canvasState.nodeModules,
        canvasState.openFile,
        canvasState.startingMetadata,
        interactionSession.interactionData.pasteTargetsToIgnore,
        {
          elementPaste: elementPasteWithMetadata.elements,
          originalContextMetadata: elementPasteWithMetadata.targetOriginalContextMetadata,
          originalContextElementPathTrees:
            interactionSession.interactionData.targetOriginalPathTrees,
        },
        canvasState.startingElementPathTree,
      )
      if (isLeft(target)) {
        return strategyApplicationResult([
          showToastCommand(target.value, 'ERROR', `${strategyProps.id}-get-target-parent-failure`),
        ])
      }

      const elements = getElementsFromPaste(
        elementPasteWithMetadata.elements,
        target.value.parentPath,
        canvasState.projectContents,
      )

      const strategy = reparentStrategyForPaste(
        canvasState.startingMetadata,
        canvasState.startingAllElementProps,
        canvasState.startingElementPathTree,
        target.value.parentPath.intendedParentPath,
      )

      const commands = elements.flatMap((currentValue) => {
        return [
          updateFunctionCommand('always', (editor, commandLifecycle) => {
            if (interactionSession.interactionData.type !== 'DISCRETE_REPARENT') {
              // This is here to appease the TS type checker edge case
              return []
            }

            const existingIDs = getAllUniqueUids(editor.projectContents).allIDs
            const elementWithUniqueUID = fixUtopiaElement(
              currentValue.element,
              new Set(existingIDs),
            ).value

            const reparentTarget: StaticReparentTarget =
              strategy === 'REPARENT_AS_ABSOLUTE'
                ? {
                    type: strategy,
                    insertionPath: target.value.parentPath,
                    intendedCoordinates: absolutePositionForPaste(
                      target.value,
                      currentValue.originalElementPath,
                      elements.map((element) => element.originalElementPath),
                      {
                        originalTargetMetadata:
                          elementPasteWithMetadata.targetOriginalContextMetadata,
                        currentMetadata: canvasState.startingMetadata,
                        originalPathTrees:
                          interactionSession.interactionData.targetOriginalPathTrees,
                        currentPathTrees: canvasState.startingElementPathTree,
                      },
                      interactionSession.interactionData.canvasViewportCenter,
                    ),
                  }
                : { type: strategy, insertionPath: target.value.parentPath }

            const indexPosition =
              target.value.type === 'sibling'
                ? absolute(
                    MetadataUtils.getIndexInParent(
                      editor.jsxMetadata,
                      editor.elementPathTree,
                      target.value.siblingPath,
                    ) + 1,
                  )
                : front()

            const result = insertWithReparentStrategies(
              editor,
              elementPasteWithMetadata.targetOriginalContextMetadata,
              interactionSession.interactionData.targetOriginalPathTrees,
              reparentTarget,
              {
                elementPath: currentValue.originalElementPath,
                pathToReparent: elementToReparent(elementWithUniqueUID, currentValue.importsToAdd),
              },
              indexPosition,
              canvasState.builtInDependencies,
            )

            if (result == null) {
              return []
            }

            return foldAndApplyCommandsInner(
              editor,
              [],
              [
                ...result.commands,
                updateSelectedViews('always', [...editor.selectedViews, result.newPath]),
              ],
              commandLifecycle,
            ).statePatches
          }),
        ]
      })

      if (commands.length === 0) {
        return emptyStrategyApplicationResult
      }

      return strategyApplicationResult([updateSelectedViews('always', []), ...commands])
    },
  }
}

function pasteMetaStrategy(mode: PasteMode) {
  return (
    canvasState: InteractionCanvasState,
    interactionSession: InteractionSession | null,
  ): CanvasStrategy | null => {
    if (interactionSession?.interactionData.type !== 'DISCRETE_REPARENT') {
      return null
    }

    switch (mode) {
      case 'replace':
        return pasteStrategy(
          interactionSession,
          canvasState,
          interactionSession.interactionData.dataWithPropsReplaced,
          { name: 'Paste', id: PasteWithPropertiesReplacedStrategyId, fitness: 2 },
        )
      case 'preserve':
        return pasteStrategy(
          interactionSession,
          canvasState,
          interactionSession.interactionData.dataWithPropsPreserved,
          {
            name: 'Paste with props preserved',
            id: PasteWithPropertiesPreservedStrategyId,
            fitness: 1,
          },
        )
      default:
        assertNever(mode)
    }
  }
}

function getElementsFromPaste(
  elements: ElementPaste[],
  targetPath: InsertionPath,
  projectContents: ProjectContentTreeRoot,
): ElementPaste[] {
  if (elements.length > 1 && isConditionalClauseInsertionPath(targetPath)) {
    /**
     * FIXME: the wrapper here won't have a corresponding entry in `targetOriginalContextMetadata`,
     * and a lot of code down the line relies on this
     */
    const fragmentUID = generateUidWithExistingComponents(projectContents)
    const mergedImportsFromElements = elements
      .map((e) => e.importsToAdd)
      .reduce((merged, imports) => ({ ...merged, ...imports }), {})
    const mergedImportsWithReactImport = {
      ...mergedImportsFromElements,
      react: {
        importedAs: 'React',
        importedFromWithin: [],
        importedWithName: null,
      },
    }
    const fragment = jsxFragment(
      fragmentUID,
      elements.map((e) => e.element),
      true,
    )
    return [
      {
        element: fragment,
        importsToAdd: mergedImportsWithReactImport,
        originalElementPath: EP.fromString(fragmentUID),
      },
    ]
  }

  return elements
}
