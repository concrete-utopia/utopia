import { sortBy } from '../../../core/shared/array-utils'
import { foldEither } from '../../../core/shared/either'
import {
  JSXElement,
  jsxAttributeValue,
  emptyComments,
  ElementInstanceMetadata,
} from '../../../core/shared/element-template'
import { setJSXValueAtPath } from '../../../core/shared/jsx-attributes'
import { magnitude, canvasPoint } from '../../../core/shared/math-utils'
import * as EP from '../../../core/shared/element-path'
import * as PP from '../../../core/shared/property-path'
import {
  EditorState,
  forUnderlyingTargetFromEditorState,
  modifyUnderlyingForOpenFile,
  TransientCanvasState,
  TransientFilesState,
} from '../../editor/store/editor-state'
import {
  CanvasStrategy,
  CanvasStrategyUpdateFn,
  FlexAlignControlRectProps,
  SelectModeCanvasSession,
} from '../canvas-types'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'

function translateStrategy(
  editorState: EditorState,
  activeSession: SelectModeCanvasSession,
  previousTransientState: TransientCanvasState | null,
): TransientCanvasState {
  // only apply after a certain treshold IF we hadn't already passed that treshold once
  if (
    !previousTransientState?.sessionStatePatch.dragDeltaMinimumPassed &&
    magnitude(activeSession.drag ?? canvasPoint({ x: 0, y: 0 })) < 15
  ) {
    return {
      highlightedViews: [],
      selectedViews: editorState.selectedViews,
      filesState: {},
      toastsToApply: [],
      sessionStatePatch: {},
      editorStatePatch: {},
    }
  }

  // only do something if you hadn't moved the mouse for a second
  // if (activeSession.globalTime - activeSession.lastTimeMouseMoved < 1000) {
  //   return {
  //     highlightedViews: [],
  //     selectedViews: editorState.selectedViews,
  //     filesState: previousTransientState?.filesState ?? {},
  //     toastsToApply: [],
  //     sessionStatePatch: previousTransientState?.sessionStatePatch ?? {},
  //   }
  // }

  const elementsToTarget = editorState.selectedViews

  let transientFilesState: TransientFilesState = {}
  let workingEditorState = editorState

  for (const elementToTarget of elementsToTarget) {
    workingEditorState = modifyUnderlyingForOpenFile(
      elementToTarget,
      editorState,
      (element: JSXElement) => {
        return foldEither(
          () => {
            return element
          },
          (updatedProps) => {
            return {
              ...element,
              props: updatedProps,
            }
          },
          setJSXValueAtPath(
            element.props,
            PP.create(['style', 'transform']),
            jsxAttributeValue(
              `translateX(${activeSession.drag?.x}px) translateY(${activeSession.drag?.y}px)`,
              emptyComments,
            ),
          ),
        )
      },
    )

    forUnderlyingTargetFromEditorState(
      elementToTarget,
      workingEditorState,
      (success, underlyingElement, underlyingTarget, underlyingFilePath) => {
        transientFilesState[underlyingFilePath] = {
          topLevelElementsIncludingScenes: success.topLevelElements,
          imports: success.imports,
        }
        return success
      },
    )
  }

  return {
    highlightedViews: [],
    selectedViews: editorState.selectedViews,
    filesState: transientFilesState,
    toastsToApply: [],
    sessionStatePatch: {
      dragDeltaMinimumPassed: true,
    },
    editorStatePatch: {},
  }
}

function calcualteFlexAlignIndicatorBoxes(
  targetMetadata: ElementInstanceMetadata | null,
): Array<FlexAlignControlRectProps> {
  const BoxHeight = 10
  if (targetMetadata?.globalFrame == null) {
    return []
  }
  return [
    {
      x: targetMetadata.globalFrame?.x,
      y: targetMetadata.globalFrame?.y,
      width: targetMetadata.globalFrame?.width,
      height: BoxHeight,
    },
    {
      x: targetMetadata.globalFrame?.x,
      y: targetMetadata.globalFrame?.y + targetMetadata.globalFrame.height - BoxHeight,
      width: targetMetadata.globalFrame?.width,
      height: BoxHeight,
    },
  ]
}

const flexAlignParentStrategy: CanvasStrategyUpdateFn = (
  editorState: EditorState,
  activeSession: SelectModeCanvasSession,
  previousTransientState: TransientCanvasState | null,
): TransientCanvasState => {
  // only apply after a certain treshold IF we hadn't already passed that treshold once
  if (
    !previousTransientState?.sessionStatePatch.dragDeltaMinimumPassed &&
    magnitude(activeSession.drag ?? canvasPoint({ x: 0, y: 0 })) < 15
  ) {
    return {
      highlightedViews: [],
      selectedViews: editorState.selectedViews,
      filesState: {},
      toastsToApply: [],
      sessionStatePatch: {},
      editorStatePatch: {},
    }
  }

  const draggedElement = editorState.selectedViews[0]
  const targetParent = MetadataUtils.getParent(editorState.jsxMetadata, draggedElement)
  const indicatorBoxes = calcualteFlexAlignIndicatorBoxes(targetParent)

  return {
    highlightedViews: [],
    selectedViews: editorState.selectedViews,
    filesState: {},
    toastsToApply: [],
    sessionStatePatch: {
      dragDeltaMinimumPassed: true,
    },
    editorStatePatch: {
      canvas: {
        controls: {
          flexAlignDropTargets: indicatorBoxes,
        },
      },
    },
  }
}

const RegisteredCanvasStrategies: Array<CanvasStrategy> = [
  {
    name: "Change Parent's Flex Align and Justify",
    fitnessFn: (editor, currentSession, previousTransientState) => {
      if (editor.selectedViews.length === 1) {
        const selectedView = editor.selectedViews[0]

        const isFlexLayouted = MetadataUtils.isParentYogaLayoutedContainerForElementAndElementParticipatesInLayout(
          MetadataUtils.findElementByElementPath(editor.jsxMetadata, selectedView),
        )
        const hasNoSiblings =
          MetadataUtils.getSiblings(editor.jsxMetadata, selectedView).length === 1

        if (isFlexLayouted && hasNoSiblings) {
          return 10 // fit!
        }
      }
      return 0 // not fit
    },
    updateFn: flexAlignParentStrategy,
  },
]

export function pickDefaultCanvasStrategy(
  editorState: EditorState,
  currentSession: SelectModeCanvasSession,
  previousTransientState: TransientCanvasState | null,
): CanvasStrategyUpdateFn | null {
  sortBy(RegisteredCanvasStrategies, (l, r) => {
    // sort by fitness, descending
    return (
      r.fitnessFn(editorState, currentSession, previousTransientState) -
      l.fitnessFn(editorState, currentSession, previousTransientState)
    )
  })
  return RegisteredCanvasStrategies[0]?.updateFn ?? null
}

export function applyCanvasStrategy(
  editorState: EditorState,
  canvasSession: SelectModeCanvasSession,
  previousTransientState: TransientCanvasState | null,
): TransientCanvasState | null {
  const strategy = pickDefaultCanvasStrategy(editorState, canvasSession, previousTransientState)
  return strategy?.(editorState, canvasSession, previousTransientState) ?? null
}
