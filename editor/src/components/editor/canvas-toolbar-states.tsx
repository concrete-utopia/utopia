import { assertNever } from '../../core/shared/utils'
import { useGetDragStrategyIndicatorFlags } from '../canvas/controls/select-mode/strategy-indicator'
import { RightMenuTab } from './store/editor-state'
import { Substores, useEditorState } from './store/store-hook'

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
        floatingInsertMenuOpen: boolean
        insertSidebarOpen: boolean
      }
    }
  | { primary: 'play' }
  | { primary: 'zoom' }

export function useToolbarMode(toolbarInsertMode: boolean): ToolbarMode {
  const editorMode = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.mode,
    'useGetToolbarMode editorMode',
  )
  const floatingInsertMenu = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.floatingInsertMenu.insertMenuMode,
    'useGetToolbarMode floatingInsertMenu',
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
    toolbarInsertMode ||
    editorMode.type === 'insert' ||
    floatingInsertMenu === 'insert' ||
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

    return {
      primary: 'insert',
      secondary: {
        type: 'insert-options',
        divInsertionActive: insertionTargetDiv,
        imageInsertionActive: insertionTargetImage,
        buttonInsertionActive: insertionTargetButton,
        conditionalInsertionActive: insertionTargetConditional,
        floatingInsertMenuOpen: floatingInsertMenu === 'insert',
        insertSidebarOpen: rightMenuTab === RightMenuTab.Insert,
      },
    }
  }

  // Edit Mode
  if (nothingSelected) {
    return { primary: 'edit', secondary: 'nothing-selected' }
  }

  if (floatingInsertMenu === 'convert' || floatingInsertMenu === 'wrap') {
    return { primary: 'edit', secondary: 'selected' }
  }

  if (dragStrategyFlags?.dragStarted) {
    return { primary: 'edit', secondary: 'strategy-active' }
  }

  if (editorMode.type === 'select') {
    return { primary: 'edit', secondary: 'selected' }
  }

  return { primary: 'edit', secondary: 'nothing-selected' } // fallback - for now
}
