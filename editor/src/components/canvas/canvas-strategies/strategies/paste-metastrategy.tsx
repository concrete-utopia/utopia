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
  ControlWithProps,
  CustomStrategyState,
  InteractionCanvasState,
  controlWithProps,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import { InteractionSession, boundingArea, createInteractionViaMouse } from '../interaction-state'
import {
  StaticReparentTarget,
  reparentStrategyForPaste,
} from './reparent-helpers/reparent-strategy-helpers'
import { elementToReparent } from './reparent-utils'
import { updateFunctionCommand } from '../../commands/update-function-command'
import { foldAndApplyCommandsInner } from '../../commands/commands'
import { updateSelectedViews } from '../../commands/update-selected-views-command'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { flattenSelection } from './shared-move-strategies-helpers'
import { AbsoluteResizeControl } from '../../controls/select-mode/absolute-resize-control'
import { RegisteredCanvasStrategies, getApplicableStrategies } from '../canvas-strategies'
import { canvasPoint } from '../../../../core/shared/math-utils'
import { emptyModifiers } from '../../../../utils/modifiers'
import { uniqBy } from '../../../../core/shared/array-utils'

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
  customStrategyState: CustomStrategyState,
  elementPasteWithMetadata: ElementPasteWithMetadata,
  strategyProps: StrategySlice,
): CanvasStrategy {
  const fakeInteractionData = createInteractionViaMouse(
    canvasPoint({ x: 0, y: 0 }),
    emptyModifiers,
    boundingArea(),
    'zero-drag-not-permitted',
  )

  const patchedInteractionSession: InteractionSession = {
    ...interactionSession,
    ...fakeInteractionData,
  }

  const controls = uniqBy(
    getApplicableStrategies(
      RegisteredCanvasStrategies,
      canvasState,
      patchedInteractionSession,
      customStrategyState,
    ).flatMap((s) =>
      s.controlsToRender.map(
        (c: ControlWithProps<any>): ControlWithProps<any> => ({ ...c, key: c.key + '-for-paste' }),
      ),
    ),
    (a, b) => a.key === b.key,
  )

  return {
    id: strategyProps.id,
    name: strategyProps.name,
    fitness: strategyProps.fitness,
    controlsToRender: controls,
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
      if (target == null) {
        return emptyStrategyApplicationResult
      }

      const elements = getElementsFromPaste(
        elementPasteWithMetadata.elements,
        target.parentPath,
        canvasState.projectContents,
      )

      const strategy = reparentStrategyForPaste(
        canvasState.startingMetadata,
        canvasState.startingAllElementProps,
        canvasState.startingElementPathTree,
        target.parentPath.intendedParentPath,
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
                    insertionPath: target.parentPath,
                    intendedCoordinates: absolutePositionForPaste(
                      target,
                      currentValue.originalElementPath,
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
                : { type: strategy, insertionPath: target.parentPath }

            const indexPosition =
              target.type === 'sibling'
                ? absolute(
                    MetadataUtils.getIndexInParent(
                      editor.jsxMetadata,
                      editor.elementPathTree,
                      target.siblingPath,
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
    customStrategyState: CustomStrategyState,
  ): CanvasStrategy | null => {
    if (interactionSession?.interactionData.type !== 'DISCRETE_REPARENT') {
      return null
    }

    switch (mode) {
      case 'replace':
        return pasteStrategy(
          interactionSession,
          canvasState,
          customStrategyState,
          interactionSession.interactionData.dataWithPropsReplaced,
          { name: 'Paste', id: PasteWithPropertiesReplacedStrategyId, fitness: 2 },
        )
      case 'preserve':
        return pasteStrategy(
          interactionSession,
          canvasState,
          customStrategyState,
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
