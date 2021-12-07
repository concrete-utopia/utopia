import { foldEither } from '../../../core/shared/either'
import { JSXElement, jsxAttributeValue, emptyComments } from '../../../core/shared/element-template'
import { setJSXValueAtPath } from '../../../core/shared/jsx-attributes'
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
): TransientCanvasState {
  // TODO only apply after a certain treshold

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
  }
}

export function pickDefaultCanvasStrategy(): CanvasStrategy | null {
  return moveTransformTranslate
}

export function applyCanvasStrategy(
  editorState: EditorState,
  canvasSession: SelectModeCanvasSession,
): TransientCanvasState | null {
  const strategy = canvasSession.activeStrategy
  return strategy?.(editorState, canvasSession) ?? null
}
