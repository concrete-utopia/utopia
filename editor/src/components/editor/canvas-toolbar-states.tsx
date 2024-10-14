import { useGetDragStrategyIndicatorFlags } from '../canvas/controls/select-mode/strategy-indicator'
import type { EditorState } from './store/editor-state'
import { RightMenuTab } from './store/editor-state'
import { Substores, useEditorState } from './store/store-hook'
import type { SelectModeToolbarMode } from './editor-modes'
import { isSelectMode } from './editor-modes'
import type { Optic } from '../../core/shared/optics/optics'
import { fromField, fromTypeGuard } from '../../core/shared/optics/optic-creators'
import { anyBy, set } from '../../core/shared/optics/optic-utilities'
import type { EditorAction } from './action-types'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { getJSXAttributesAtPath } from '../../core/shared/jsx-attribute-utils'
import { create } from '../../core/shared/property-path'

// This is the data structure that governs the Canvas Toolbar's submenus and active buttons
type ToolbarMode =
  | { primary: 'edit'; secondary: 'nothing-selected' | 'selected' | 'strategy-active' }
  | { primary: 'text'; secondary: 'target' | 'inserting' | 'write' }
  | {
      primary: 'insert'
      secondary: {
        type: 'insert-options'
        divInsertionActive: boolean
        imageInsertionActive: boolean
        buttonInsertionActive: boolean
        conditionalInsertionActive: boolean
        gridInsertionActive: boolean
        insertSidebarOpen: boolean
      }
    }
  | { primary: 'play' }
  | { primary: 'comment' }
  | { primary: 'zoom' }

export function useToolbarMode(): ToolbarMode {
  const editorMode = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.mode,
    'useGetToolbarMode editorMode',
  )

  const dragStrategyFlags = useGetDragStrategyIndicatorFlags()

  const rightMenuTab = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.rightMenu.selectedTab,
    'useGetToolbarMode rightMenuTab',
  )

  const keysPressed = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.keysPressed,
    'useGetToolbarMode keysPressed',
  )

  const nothingSelected = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews.length === 0,
    'useGetToolbarMode nothingSelected',
  )

  // If Z is held, the EditorCanvas's zoom behavior takes precendence, sort of like a secret Editor Mode
  if (keysPressed['z']) {
    return { primary: 'zoom' }
  }

  // Live Mode
  if (editorMode.type === 'live') {
    return { primary: 'play' }
  }

  // Comment Mode
  if (editorMode.type === 'comment') {
    return { primary: 'comment' }
  }

  // Text Edit Mode (info partially stored in editor.mode InsertMode)
  if (
    editorMode.type === 'insert' &&
    editorMode.subjects.length === 1 &&
    editorMode.subjects[0].textEdit
  ) {
    if (dragStrategyFlags?.dragStarted) {
      // while you’re drawing to insert a (text element) and after you’re done you edit
      return { primary: 'text', secondary: 'inserting' }
    }
    // when the text cursor shows up and you can highlight text instances on the canvas
    return { primary: 'text', secondary: 'target' }
  }
  if (editorMode.type === 'textEdit') {
    return { primary: 'text', secondary: 'write' }
  }

  // Insert Mode (sans text insertion)
  if (
    (editorMode.type === 'select' && editorMode.toolbarMode === 'pseudo-insert') ||
    editorMode.type === 'insert' ||
    rightMenuTab === RightMenuTab.Insert
  ) {
    const insertionTargetDiv =
      editorMode.type === 'insert' &&
      editorMode.subjects.some(
        (subject) =>
          subject.element.name.baseVariable === 'div' &&
          // if the insertionSubjectWrapper is not null, this is either a conditional or a fragment insertion
          subject.insertionSubjectWrapper == null,
      )
    const insertionTargetImage =
      editorMode.type === 'insert' &&
      editorMode.subjects.some((subject) => subject.element.name.baseVariable === 'img')
    const insertionTargetButton =
      editorMode.type === 'insert' &&
      editorMode.subjects.some((subject) => subject.element.name.baseVariable === 'button')
    const insertionTargetConditional =
      editorMode.type === 'insert' &&
      editorMode.subjects.some((subject) => subject.insertionSubjectWrapper === 'conditional')
    const insertionTargetGrid =
      editorMode.type === 'insert' &&
      editorMode.subjects.some((subject) => {
        if (subject.element.name.baseVariable !== 'div') {
          return false
        }

        const style = subject.element.props.find(
          (p) => p.type === 'JSX_ATTRIBUTES_ENTRY' && p.key === 'style',
        )
        if (style == null) {
          return false
        }

        const display = getJSXAttributesAtPath(subject.element.props, create('style', 'display'))
        return (
          style.type === 'JSX_ATTRIBUTES_ENTRY' &&
          style.value.type === 'ATTRIBUTE_VALUE' &&
          display.attribute.type === 'PART_OF_ATTRIBUTE_VALUE' &&
          display.attribute.value === 'grid'
        )
      })

    return {
      primary: 'insert',
      secondary: {
        type: 'insert-options',
        divInsertionActive: insertionTargetDiv,
        imageInsertionActive: insertionTargetImage,
        buttonInsertionActive: insertionTargetButton,
        conditionalInsertionActive: insertionTargetConditional,
        gridInsertionActive: insertionTargetGrid,
        insertSidebarOpen: rightMenuTab === RightMenuTab.Insert,
      },
    }
  }

  // Edit Mode
  if (nothingSelected) {
    return { primary: 'edit', secondary: 'nothing-selected' }
  }

  if (dragStrategyFlags?.dragStarted) {
    return { primary: 'edit', secondary: 'strategy-active' }
  }

  if (editorMode.type === 'select') {
    return { primary: 'edit', secondary: 'selected' }
  }

  return { primary: 'edit', secondary: 'nothing-selected' } // fallback - for now
}

export const editorStateToolbarModeOptic: Optic<EditorState, SelectModeToolbarMode> = fromField<
  EditorState,
  'mode'
>('mode')
  .compose(fromTypeGuard(isSelectMode))
  .compose(fromField('toolbarMode'))

export function maybeClearPseudoInsertMode(
  editorStateBefore: EditorState,
  editorStateAfter: EditorState,
  action: EditorAction,
): EditorState {
  // Check the psuedo insert mode is currently enabled as there's no need to do anything otherwise.
  if (anyBy(editorStateToolbarModeOptic, (mode) => mode === 'pseudo-insert', editorStateAfter)) {
    function clearPseudoInsertMode(): EditorState {
      return set<EditorState, SelectModeToolbarMode>(
        editorStateToolbarModeOptic,
        'none',
        editorStateAfter,
      )
    }

    // If the project contents have changed at all and the change didn't come from a worker, then clear the pseudo-insert mode.
    if (
      editorStateBefore.projectContents !== editorStateAfter.projectContents &&
      action.action !== 'UPDATE_FROM_WORKER'
    ) {
      return clearPseudoInsertMode()
    }

    // If the user has started an interaction, clear the psuedo mode.
    if (action.action === 'CREATE_INTERACTION_SESSION') {
      return clearPseudoInsertMode()
    }

    // If the user focuses another panel.
    if (editorStateAfter.focusedPanel !== 'canvas') {
      return clearPseudoInsertMode()
    }
  }

  // Default to the original value in all other cases.
  return editorStateAfter
}
