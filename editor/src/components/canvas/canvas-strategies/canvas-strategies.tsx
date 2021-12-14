import { foldEither } from '../../../core/shared/either'
import { JSXElement, jsxAttributeValue, emptyComments } from '../../../core/shared/element-template'
import { setJSXValueAtPath } from '../../../core/shared/jsx-attributes'
import { magnitude, canvasPoint } from '../../../core/shared/math-utils'
import * as PP from '../../../core/shared/property-path'
import {
  EditorState,
  forUnderlyingTargetFromEditorState,
  modifyUnderlyingForOpenFile,
  TransientCanvasState,
  TransientFilesState,
} from '../../editor/store/editor-state'
import { CanvasStrategy, SelectModeCanvasSession } from '../canvas-types'

function moveTransformTranslate(
  editorState: EditorState,
  activeSession: SelectModeCanvasSession,
  previousTransientState: TransientCanvasState | null,
): TransientCanvasState {
  // only apply after a certain treshold IF we hadn't already passed that treshold once
  if (
    !previousTransientState?.sessionStatePatch?.dragDeltaMinimumPassed &&
    magnitude(activeSession.drag ?? canvasPoint({ x: 0, y: 0 })) < 15
  ) {
    return {
      highlightedViews: [],
      selectedViews: editorState.selectedViews,
      filesState: {},
      toastsToApply: [],
      sessionStatePatch: {},
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
  }
}

export function pickDefaultCanvasStrategy(): CanvasStrategy | null {
  return moveTransformTranslate
}

export function applyCanvasStrategy(
  editorState: EditorState,
  canvasSession: SelectModeCanvasSession,
  previousTransientState: TransientCanvasState | null,
): TransientCanvasState | null {
  const strategy = canvasSession.activeStrategy
  return strategy?.(editorState, canvasSession, previousTransientState) ?? null
}
