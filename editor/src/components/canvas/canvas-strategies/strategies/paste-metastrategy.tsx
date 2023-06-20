import * as EP from '../../../../core/shared/element-path'
import { generateUidWithExistingComponents } from '../../../../core/model/element-template-utils'
import { getAllUniqueUids } from '../../../../core/model/get-unique-ids'
import { ElementInstanceMetadataMap, jsxFragment } from '../../../../core/shared/element-template'
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
import { CanvasCommand, foldAndApplyCommandsInner } from '../../commands/commands'
import { updateSelectedViews } from '../../commands/update-selected-views-command'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { ElementPathTrees } from '../../../../core/shared/element-path-tree'
import { ElementPath, NodeModules } from '../../../../core/shared/project-file-types'
import { AllElementProps } from '../../../editor/store/editor-state'
import { BuiltInDependencies } from '../../../../core/es-modules/package-manager/built-in-dependencies-list'
import { CanvasPoint } from '../../../../core/shared/math-utils'

const PasteModes = ['replace', 'preserve'] as const
type PasteMode = typeof PasteModes[number]

export const PasteWithPropertiesReplacedStrategyId = 'PasteWithPropertiesReplacedStrategy'
export const PasteWithPropertiesPreservedStrategyId = 'PasteWithPropertiesPreservedStrategy'

export const pasteWithPropsReplacedStrategy = pasteMetaStrategy('replace')
export const pasteWithPropsPreservedStrategy = pasteMetaStrategy('preserve')

type StrategySlice = Pick<CanvasStrategy, 'fitness' | 'id' | 'name'>

interface EditorStateContext {
  projectContents: ProjectContentTreeRoot
  nodeModules: NodeModules
  openFile: string | null
  pasteTargetsToIgnore: Array<ElementPath>
  builtInDependencies: BuiltInDependencies
  startingMetadata: ElementInstanceMetadataMap
  startingElementPathTrees: ElementPathTrees
  startingAllElementProps: AllElementProps
}

interface PasteContext {
  selectedViews: ElementPath[]
  elementPasteWithMetadata: ElementPasteWithMetadata
  targetOriginalPathTrees: ElementPathTrees
  canvasViewportCenter: CanvasPoint
}

export function pasteStrategyApply(
  editorStateContext: EditorStateContext,
  pasteContext: PasteContext,
): Array<CanvasCommand> | null {
  const target = getTargetParentForPaste(
    editorStateContext.projectContents,
    pasteContext.selectedViews,
    editorStateContext.nodeModules,
    editorStateContext.openFile,
    editorStateContext.startingMetadata,
    editorStateContext.pasteTargetsToIgnore,
    {
      elementPaste: pasteContext.elementPasteWithMetadata.elements,
      originalContextMetadata: pasteContext.elementPasteWithMetadata.targetOriginalContextMetadata,
      originalContextElementPathTrees: pasteContext.targetOriginalPathTrees,
    },
    editorStateContext.startingElementPathTrees,
  )

  if (target == null) {
    return null
  }

  const elements = getElementsFromPaste(
    pasteContext.elementPasteWithMetadata.elements,
    target.parentPath,
    editorStateContext.projectContents,
  )

  const strategy = reparentStrategyForPaste(
    editorStateContext.startingMetadata,
    editorStateContext.startingAllElementProps,
    editorStateContext.startingElementPathTrees,
    target.parentPath.intendedParentPath,
  )

  return elements.flatMap((currentValue) => {
    return [
      updateFunctionCommand('always', (editor, commandLifecycle) => {
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
                      pasteContext.elementPasteWithMetadata.targetOriginalContextMetadata,
                    currentMetadata: editorStateContext.startingMetadata,
                    originalPathTrees: pasteContext.targetOriginalPathTrees,
                    currentPathTrees: editorStateContext.startingElementPathTrees,
                  },
                  pasteContext.canvasViewportCenter,
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
          pasteContext.elementPasteWithMetadata.targetOriginalContextMetadata,
          pasteContext.targetOriginalPathTrees,
          reparentTarget,
          {
            elementPath: currentValue.originalElementPath,
            pathToReparent: elementToReparent(elementWithUniqueUID, currentValue.importsToAdd),
          },
          indexPosition,
          editorStateContext.builtInDependencies,
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
}

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

      const commands = pasteStrategyApply(
        {
          projectContents: canvasState.projectContents,
          nodeModules: canvasState.nodeModules,
          openFile: canvasState.openFile ?? null,
          pasteTargetsToIgnore: interactionSession.interactionData.pasteTargetsToIgnore,
          builtInDependencies: canvasState.builtInDependencies,
          startingMetadata: canvasState.startingMetadata,
          startingAllElementProps: canvasState.startingAllElementProps,
          startingElementPathTrees: canvasState.startingElementPathTree,
        },
        {
          selectedViews: canvasState.interactionTarget.elements,
          elementPasteWithMetadata: elementPasteWithMetadata,
          targetOriginalPathTrees: interactionSession.interactionData.targetOriginalPathTrees,
          canvasViewportCenter: interactionSession.interactionData.canvasViewportCenter,
        },
      )

      if (commands == null || commands.length === 0) {
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
