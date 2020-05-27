import { KeyCharacter, modifiersForEvent, strictCheckModifiers } from '../../utils/keyboard'
import { toggleTextFormatting } from '../text-utils'
import { EditorAction, EditorDispatch } from './action-types'
import * as EditorActions from './actions/actions'
import { DerivedState, EditorState } from './store/editor-state'
import { isLiveMode, EditorModes, isInsertMode } from './editor-modes'

export function handleGlobalKeyDown(
  event: KeyboardEvent,
  key: KeyCharacter,
  editor: EditorState,
  dispatch: EditorDispatch,
): Array<EditorAction> {
  const modifiers = modifiersForEvent(event)

  const cmd = strictCheckModifiers(modifiers, ['cmd'])
  const shiftCmd = strictCheckModifiers(modifiers, ['shift', 'cmd'])
  const altCmd = strictCheckModifiers(modifiers, ['alt', 'cmd'])

  switch (key) {
    case 'left':
    case 'right':
    case 'up':
    case 'down': {
      if (cmd) {
        event.preventDefault()
      }
      return []
    }

    case 'c': {
      if (altCmd) {
        // copy style, prevent console from opening
        event.preventDefault()
      }
      return []
    }

    case 'plus':
    case 'minus': {
      if (cmd) {
        event.preventDefault()
      }
      return []
    }
    case 'i': {
      if (cmd) {
        event.preventDefault()
        return toggleTextFormatting(editor, dispatch, 'italic')
      } else {
        return []
      }
    }
    case 'u': {
      if (cmd) {
        event.preventDefault()
        return toggleTextFormatting(editor, dispatch, 'underline')
      } else {
        return []
      }
    }
    case 'b': {
      if (cmd) {
        event.preventDefault()
        return toggleTextFormatting(editor, dispatch, 'bold')
      } else {
        return []
      }
    }
    case 'd': {
      if (cmd) {
        event.preventDefault()
      }
      return []
    }
    case 'f': {
      if (cmd) {
        event.preventDefault()
      }
      return []
    }
    case 'g': {
      if (cmd) {
        event.preventDefault()
      } else if (shiftCmd) {
        event.preventDefault()
      }
      return []
    }
    case 'h': {
      if (cmd) {
        event.preventDefault()
      } else if (shiftCmd) {
        event.preventDefault()
      }
      return []
    }
    case 'p': {
      if (cmd) {
        event.preventDefault()
        return [EditorActions.togglePanel('preview')]
      } else if (shiftCmd) {
        event.preventDefault()
        return [EditorActions.toggleCanvasIsLive()]
      }
      return []
    }
    case 'r': {
      if (cmd) {
        event.preventDefault()
      }
      return []
    }
    case 's': {
      if (cmd) {
        event.preventDefault()
        return [EditorActions.saveCurrentFile()]
      }
      return []
    }
    case 'z': {
      if (cmd || shiftCmd) {
        event.preventDefault()
        if (shiftCmd) {
          return [EditorActions.redo()]
        } else {
          return [EditorActions.undo()]
        }
      } else {
        return []
      }
    }
    case '1': {
      if (altCmd) {
        return [EditorActions.togglePanel('leftmenu')]
      } else {
        return []
      }
    }
    case '2': {
      if (altCmd) {
        return [EditorActions.togglePanel('inspector')]
      } else {
        return []
      }
    }
    case 'y': {
      if (cmd) {
        event.preventDefault()
        return [EditorActions.toggleInterfaceDesignerAdditionalControls()]
      } else {
        return []
      }
    }
    case 'period': {
      if (cmd) {
        return [EditorActions.toggleInterfaceDesignerCodeEditor()]
      } else {
        return []
      }
    }
    case 'backslash': {
      if (cmd) {
        return [EditorActions.togglePanel('inspector'), EditorActions.togglePanel('leftmenu')]
      } else {
        return []
      }
    }
    case 'comma': {
      if (cmd) {
        // prevent opening new tab
        event.preventDefault()
        return [EditorActions.toggleInterfaceDesignerLayoutReversed()]
      } else {
        return []
      }
    }
    case 'esc': {
      if (isLiveMode(editor.mode)) {
        return [EditorActions.updateEditorMode(EditorModes.selectMode(editor.mode.controlId))]
      } else if (isInsertMode(editor.mode)) {
        return [EditorActions.updateEditorMode(EditorModes.selectMode())]
      } else {
        return []
      }
    }
    case 'enter': {
      if (cmd) {
        // Refer to the passthrough hack in monaco-wrapper in case this shortcut needs be changed
        return [EditorActions.toggleCanvasIsLive()]
      } else {
        return []
      }
    }
    default:
      return []
  }
}
