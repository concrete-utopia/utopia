import { assertNever } from '../../core/shared/utils'
import { useGetDragStrategyIndicatorFlags } from '../canvas/controls/select-mode/strategy-indicator'
import { RightMenuTab } from './store/editor-state'
import { Substores, useEditorState } from './store/store-hook'

type ToolbarMode =
  | { primary: 'edit'; secondary: 'move' | 'select' | 'other-strategy' }
  | { primary: 'text'; secondary: 'target' | 'inserting' | 'write' }
  | { primary: 'insert'; secondary: 'floating-menu' | 'insert-sidebar' | 'target' | 'inserting' }
  | { primary: 'play' }
  | { primary: 'zoom' }

export function useGetToolbarMode(): ToolbarMode {
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

  // If Z is held, the EditorCanvas's zoom behavior takes precendence, sort of like a secret Editor Mode
  if (keysPressed['z']) {
    return { primary: 'zoom' }
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
  if (editorMode.type === 'insert') {
    if (dragStrategyFlags?.dragStarted) {
      return { primary: 'insert', secondary: 'inserting' }
    }
    return { primary: 'insert', secondary: 'target' }
  }
  if (floatingInsertMenu === 'insert') {
    return { primary: 'insert', secondary: 'floating-menu' }
  }
  if (rightMenuTab === RightMenuTab.Insert) {
    return { primary: 'insert', secondary: 'insert-sidebar' }
  }

  // Live Mode
  if (editorMode.type === 'live') {
    return { primary: 'play' }
  }

  // Edit Mode
  if (floatingInsertMenu === 'convert' || floatingInsertMenu === 'wrap') {
    return { primary: 'edit', secondary: 'select' }
  }

  if (dragStrategyFlags?.dragStarted) {
    if (dragStrategyFlags.indicatorFlags.showIndicator) {
      return { primary: 'edit', secondary: 'move' }
    }
    return { primary: 'edit', secondary: 'other-strategy' }
  }

  if (editorMode.type === 'select') {
    return { primary: 'edit', secondary: 'select' }
  }

  return { primary: 'edit', secondary: 'select' } // fallback - for now
}
