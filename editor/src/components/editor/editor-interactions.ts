import { Keyboard, KeyCharacter, KeysPressed, StoredKeyCharacters } from '../../utils/keyboard'
import { Modifier, Modifiers } from '../../utils/modifiers'
import Canvas from '../canvas/canvas'
import { DispatchPriority, EditorAction, EditorDispatch } from './action-types'
import * as EditorActions from './actions/actions'
import { handleGlobalKeyDown } from './global-shortcuts'
import { insertImage } from './image-insert'
import { DerivedState, EditorState } from './store/editor-state'

function shouldOnlyUpdateModifiers(event: KeyboardEvent, editor: EditorState): boolean {
  return isEventFromInput(event.target) || editor.modal !== null
}

export function handleKeyDown(
  event: KeyboardEvent,
  editor: EditorState,
  derived: DerivedState,
  dispatch: EditorDispatch,
) {
  const key = Keyboard.keyCharacterForCode(event.keyCode)
  const onlyUpdateModifiers = shouldOnlyUpdateModifiers(event, editor)
  let updatedKeysPressed: KeysPressed

  if (onlyUpdateModifiers) {
    updatedKeysPressed = updateModifiers(
      editor.keysPressed,
      Modifier.modifiersForKeyboardEvent(event),
    )
  } else {
    updatedKeysPressed = updateKeysPressed(
      editor.keysPressed,
      key,
      true,
      Modifier.modifiersForKeyboardEvent(event),
    )
  }
  const updateKeysAction = EditorActions.updateKeys(updatedKeysPressed)

  // In the inspector tab between elements driven by the browser, everywhere else handle it ourselves.
  if (
    key === 'tab' &&
    editor.focusedPanel !== 'inspector' &&
    editor.focusedPanel !== 'dependencylist'
  ) {
    event.preventDefault()
  }

  const globalKeyActions = handleGlobalKeyDown(event, key, editor, dispatch)

  if (globalKeyActions.length > 0) {
    // early return here, global shortcuts take precedence over lesser shortcuts
    return dispatch([updateKeysAction, ...globalKeyActions], 'everyone')
  }
  let paneSpecificActions: Array<EditorAction> = []
  let priority: DispatchPriority = 'everyone'
  if (
    !onlyUpdateModifiers &&
    (inspectorFocused(editor) || canvasFocused(editor) || navigatorFocused(editor))
  ) {
    // Handle the key internally, unless the inspector is focused and the key is a tab,
    // as we want the browser to handle it.
    if (!(inspectorFocused(editor) && key === 'tab')) {
      paneSpecificActions = Canvas.handleKeyDown(event, editor, derived, dispatch, () => {
        insertImage(dispatch)
      })
      priority = 'everyone'
    }
  }

  dispatch([updateKeysAction, ...paneSpecificActions], priority)
}

export function handleKeyUp(
  event: KeyboardEvent,
  editor: EditorState,
  derived: DerivedState,
  dispatch: EditorDispatch,
) {
  const key = Keyboard.keyCharacterForCode(event.keyCode)
  const onlyUpdateModifiers = shouldOnlyUpdateModifiers(event, editor)
  let updatedKeysPressed: KeysPressed

  if (onlyUpdateModifiers) {
    updatedKeysPressed = updateModifiers(
      editor.keysPressed,
      Modifier.modifiersForKeyboardEvent(event),
    )
  } else {
    updatedKeysPressed = updateKeysPressed(
      editor.keysPressed,
      key,
      false,
      Modifier.modifiersForKeyboardEvent(event),
    )
  }

  const updateKeysAction = EditorActions.updateKeys(updatedKeysPressed)

  // In the inspector tab between elements driven by the browser, everywhere else handle it ourselves.
  if (key === 'tab' && editor.focusedPanel !== 'inspector') {
    event.preventDefault()
  }

  let paneSpecificActions: Array<EditorAction> = []
  let priority: DispatchPriority = 'everyone'
  if (
    !onlyUpdateModifiers &&
    (inspectorFocused(editor) || canvasFocused(editor) || navigatorFocused(editor))
  ) {
    // Handle the key internally, unless the inspector is focused and the key is a tab,
    // as we want the browser to handle it.
    if (!(inspectorFocused(editor) && key === 'tab')) {
      paneSpecificActions = Canvas.handleKeyUp(key, editor, derived)
      priority = 'everyone'
    }
  }

  dispatch([updateKeysAction, ...paneSpecificActions], priority)
}

function canvasFocused(editor: EditorState): boolean {
  return editor.focusedPanel === 'canvas'
}

function inspectorFocused(editor: EditorState): boolean {
  return editor.focusedPanel === 'inspector'
}

function navigatorFocused(editor: EditorState): boolean {
  return editor.focusedPanel === 'navigator'
}

function updateKeysPressed(
  keysPressed: KeysPressed,
  updatedCharacter: KeyCharacter,
  isKeyDown: boolean,
  modifiers: Modifiers,
): KeysPressed {
  let originalKeysPressed = { ...keysPressed }
  let updatedCharacterMap: KeysPressed = {}
  if (StoredKeyCharacters.indexOf(updatedCharacter) > -1) {
    if (updatedCharacter === 'cmd') {
      // chrome causes no keyup events to arrive when cmd is pressed
      originalKeysPressed = {}
    }
    updatedCharacterMap = { [updatedCharacter]: isKeyDown }
  }

  return {
    ...originalKeysPressed,
    alt: modifiers.alt,
    cmd: modifiers.cmd,
    ctrl: modifiers.ctrl,
    shift: modifiers.shift,
    ...updatedCharacterMap,
  }
}

function updateModifiers(keysPressed: KeysPressed, modifiers: Modifiers): KeysPressed {
  return {
    ...keysPressed,
    alt: modifiers.alt,
    cmd: modifiers.cmd,
    ctrl: modifiers.ctrl,
    shift: modifiers.shift,
  }
}

export function isEventFromInput(target: any) {
  return target.tagName?.toLowerCase() === 'input' || target.tagName?.toLowerCase() === 'textarea'
}
