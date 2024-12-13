import { assertNever } from '../../core/shared/utils'
import type { Key } from '../../utils/keyboard'
import { Keyboard } from '../../utils/keyboard'
import type { ComplexMap } from '../../utils/map'
import {
  emptyComplexMap,
  addToComplexMap,
  getKeysFromComplexMap,
  getValueFromComplexMap,
} from '../../utils/map'

const key = Keyboard.key

export interface Shortcut {
  description: string
  shortcutKeys: Array<Key>
}

export function shortcut(description: string, shortcutKeys: Key | Array<Key>): Shortcut {
  return {
    description: description,
    shortcutKeys: Array.isArray(shortcutKeys) ? shortcutKeys : [shortcutKeys],
  }
}

export const DELETE_SELECTED_SHORTCUT = 'delete-selected'
export const RESET_CANVAS_ZOOM_SHORTCUT = 'reset-zoom'
export const ZOOM_UI_IN_SHORTCUT = 'zoom-ui-in'
export const ZOOM_CANVAS_IN_SHORTCUT = 'zoom-canvas-in'
export const ZOOM_UI_OUT_SHORTCUT = 'zoom-ui-out'
export const ZOOM_CANVAS_OUT_SHORTCUT = 'zoom-canvas-out'
export const FIRST_CHILD_OR_EDIT_TEXT_SHORTCUT = 'first-child-or-edit-text'
export const COMMENT_SHORTCUT = 'comment'
export const JUMP_TO_PARENT_SHORTCUT = 'jump-to-parent'
export const JUMP_TO_PARENT_SHORTCUT_BACKSLASH = 'jump-to-parent-backslash'
export const CANCEL_EVERYTHING_SHORTCUT = 'cancel-everything'
export const CYCLE_HIERACHY_TARGETS_SHORTCUT = 'cycle-hierachy-targets'
export const CYCLE_FORWARD_SIBLING_TARGETS_SHORTCUT = 'cycle-forward-sibling-targets'
export const CYCLE_BACKWARD_SIBLING_TARGETS_SHORTCUT = 'cycle-backward-sibling-targets'

export const SELECT_ALL_SIBLINGS_SHORTCUT = 'select-all-siblings'
export const TOGGLE_BORDER_SHORTCUT = 'toggle-border'
export const COPY_SELECTION_SHORTCUT = 'copy-selection'
export const DUPLICATE_SELECTION_SHORTCUT = 'duplicate-selection'
export const TOGGLE_BACKGROUND_SHORTCUT = 'toggle-background'
export const UNWRAP_ELEMENT_SHORTCUT = 'unwrap-element'
export const WRAP_ELEMENT_PICKER_SHORTCUT = 'wrap-element-picker'
export const GROUP_ELEMENT_DEFAULT_SHORTCUT = 'group-element-default'
export const TOGGLE_HIDDEN_SHORTCUT = 'toggle-hidden'
export const INSERT_IMAGE_SHORTCUT = 'insert-image'
export const TOGGLE_LIVE_CANVAS_SHORTCUT = 'toggle-live-canvas'
export const START_RENAMING_SHORTCUT = 'start-renaming'
export const INSERT_RECTANGLE_SHORTCUT = 'insert-rectangle'
export const INSERT_ELLIPSE_SHORTCUT = 'insert-ellipse'
export const ADD_ELEMENT_SHORTCUT = 'add-element'
export const SAVE_CURRENT_FILE_SHORTCUT = 'save-current-file'
export const TOGGLE_SHADOW_SHORTCUT = 'toggle-shadow'
export const INSERT_DIV_SHORTCUT = 'insert-div'
export const CUT_SELECTION_SHORTCUT = 'cut-selection'
export const UNDO_CHANGES_SHORTCUT = 'undo-changes'
export const REDO_CHANGES_SHORTCUT = 'redo-changes'
export const MOVE_ELEMENT_FORWARD_SHORTCUT = 'move-element-forward'
export const MOVE_ELEMENT_TO_FRONT_SHORTCUT = 'move-element-to-front'
export const MOVE_ELEMENT_BACKWARD_SHORTCUT = 'move-element-backward'
export const MOVE_ELEMENT_TO_BACK_SHORTCUT = 'move-element-to-back'
export const FOCUS_CLASS_NAME_INPUT = 'focus-inspector-class-name-input'
export const TOGGLE_FOCUSED_OMNIBOX_TAB = 'toggle-focused-omnibox-tab'
export const TOGGLE_NAVIGATOR = 'toggle-navigator'
export const TOGGLE_INSPECTOR = 'toggle-inspector'
export const TOGGLE_DESIGNER_ADDITIONAL_CONTROLS_SHORTCUT = 'toggle-designer-additional-controls'
export const TOGGLE_CODE_EDITOR_SHORTCUT = 'toggle-code-editor'
export const TOGGLE_INSPECTOR_AND_NAVIGATOR_SHORTCUT = 'toggle-inspector-and-navigator'
export const TEXT_EDIT_MODE = 'text-edit-mode'
export const TOGGLE_TEXT_BOLD = 'toggle-text-bold'
export const TOGGLE_TEXT_ITALIC = 'toggle-text-italic'
export const TOGGLE_TEXT_UNDERLINE = 'toggle-text-underline'
export const TOGGLE_TEXT_STRIKE_THROUGH = 'toggle-text-strike-through'
export const PASTE_TO_REPLACE = 'paste-to-replace'
export const PASTE_STYLE_PROPERTIES = 'paste-style-properties'
export const COPY_STYLE_PROPERTIES = 'copy-style-properties'

export const OPEN_EYEDROPPER = 'open-eyedropper'
export const CONVERT_TO_FLEX_CONTAINER = 'convert-to-flex-container'
export const REMOVE_ABSOLUTE_POSITIONING = 'remove-absolute-positioning'
export const RESIZE_TO_FIT = 'resize-to-fit'
export const OPEN_INSERT_MENU = 'open-insert-menu'
export const WRAP_IN_DIV = 'wrap-in-div'
export const CONVERT_TO_GRID_CONTAINER = 'convert-to-grid-container'

export type ShortcutDetails = { [key: string]: Shortcut }

export const shortcutDetailsWithDefaults: ShortcutDetails = {
  [DELETE_SELECTED_SHORTCUT]: shortcut('Delete the selected element.', [
    key('delete', []),
    key('backspace', []),
  ]),
  [RESET_CANVAS_ZOOM_SHORTCUT]: shortcut('Resets the zoom to 100%.', key('0', 'cmd')),
  [ZOOM_UI_IN_SHORTCUT]: shortcut('Zoom UI in.', key('plus', ['alt', 'cmd'])),
  [ZOOM_CANVAS_IN_SHORTCUT]: shortcut('Zoom canvas in.', key('plus', 'cmd')),
  [ZOOM_UI_OUT_SHORTCUT]: shortcut('Zoom UI out.', key('minus', ['alt', 'cmd'])),
  [ZOOM_CANVAS_OUT_SHORTCUT]: shortcut('Zoom canvas out.', key('minus', 'cmd')),
  [FIRST_CHILD_OR_EDIT_TEXT_SHORTCUT]: shortcut(
    'Select the first child of or edit the currently selected text element.',
    key('enter', []),
  ),
  [COMMENT_SHORTCUT]: shortcut('Activate comment mode.', key('c', [])),
  [JUMP_TO_PARENT_SHORTCUT]: shortcut('Jump to parent element.', key('enter', 'shift')),
  [JUMP_TO_PARENT_SHORTCUT_BACKSLASH]: shortcut(
    'Jump to parent element, with backslash.',
    key('backslash', []),
  ),
  [CANCEL_EVERYTHING_SHORTCUT]: shortcut(
    'Exit insert mode, dragging or anything else back to default.',
    key('esc', []),
  ),
  [CYCLE_HIERACHY_TARGETS_SHORTCUT]: shortcut(
    'Cycle up and down the hierarchy of the currently selected element.',
    key('q', []),
  ),
  [CYCLE_FORWARD_SIBLING_TARGETS_SHORTCUT]: shortcut(
    'Cycle forward between siblings of the currently selected element.',
    key('tab', []),
  ),
  [CYCLE_BACKWARD_SIBLING_TARGETS_SHORTCUT]: shortcut(
    'Cycle backward between siblings of the currently selected element.',
    key('tab', 'shift'),
  ),
  [SELECT_ALL_SIBLINGS_SHORTCUT]: shortcut(
    'Select all siblings of currently selected element.',
    key('a', 'cmd'),
  ),
  [TOGGLE_BORDER_SHORTCUT]: shortcut(
    'Toggle the border of the currently selected text element.',
    key('forwardslash', ['shift']),
  ),
  [COPY_SELECTION_SHORTCUT]: shortcut(
    'Copy the current selection to the clipboard.',
    key('c', 'cmd'),
  ),
  [DUPLICATE_SELECTION_SHORTCUT]: shortcut(
    'Duplicate the current selected elements.',
    key('d', 'cmd'),
  ),
  [TOGGLE_BACKGROUND_SHORTCUT]: shortcut(
    'Toggle the background layers of the currently selected element.',
    key('forwardslash', ['alt']),
  ),
  [UNWRAP_ELEMENT_SHORTCUT]: shortcut(
    'Unwrap children of an element into their grandparent element.',
    key('g', ['cmd', 'shift']),
  ),
  [WRAP_ELEMENT_PICKER_SHORTCUT]: shortcut('Wrap elements with a selected element.', key('w', [])),
  [GROUP_ELEMENT_DEFAULT_SHORTCUT]: shortcut('Group elements with a div.', key('g', 'cmd')),
  [TOGGLE_HIDDEN_SHORTCUT]: shortcut('Toggle element as hidden.', key('h', ['cmd', 'shift'])),
  [INSERT_IMAGE_SHORTCUT]: shortcut('Insert an image.', key('i', [])),
  [TOGGLE_LIVE_CANVAS_SHORTCUT]: shortcut(
    'Toggle the canvas between live and edit mode.',
    key('p', ['cmd', 'shift']),
  ),
  [START_RENAMING_SHORTCUT]: shortcut(
    'Start renaming the currently selected element.',
    key('r', 'cmd'),
  ),
  [INSERT_RECTANGLE_SHORTCUT]: shortcut('Insert a rectangle.', key('r', [])),
  [INSERT_ELLIPSE_SHORTCUT]: shortcut('Insert an ellipse.', key('e', [])),
  [SAVE_CURRENT_FILE_SHORTCUT]: shortcut('Save the current file.', key('s', 'cmd')),
  [INSERT_DIV_SHORTCUT]: shortcut('Insert a div.', [key('d', []), key('f', [])]),
  [CUT_SELECTION_SHORTCUT]: shortcut(
    'Cut the current selection to the clipboard.',
    key('x', 'cmd'),
  ),
  [UNDO_CHANGES_SHORTCUT]: shortcut('Undo the most recent change.', key('z', 'cmd')),
  [REDO_CHANGES_SHORTCUT]: shortcut(
    'Redo the most recently undone change.',
    key('z', ['cmd', 'shift']),
  ),
  [MOVE_ELEMENT_FORWARD_SHORTCUT]: shortcut(
    'Move element forward in z-index related to its siblings.',
    key(']', 'cmd'),
  ),
  [MOVE_ELEMENT_TO_FRONT_SHORTCUT]: shortcut(
    'Move element to the front in relation to its siblings.',
    key(']', ['alt', 'cmd']),
  ),
  [MOVE_ELEMENT_BACKWARD_SHORTCUT]: shortcut(
    'Move element backward in z-index related to its siblings.',
    key('[', 'cmd'),
  ),
  [MOVE_ELEMENT_TO_BACK_SHORTCUT]: shortcut(
    'Move element to the back in relation to its siblings.',
    key('[', ['alt', 'cmd']),
  ),
  [FOCUS_CLASS_NAME_INPUT]: shortcut(
    'Focus the classname input field in the inspector.',
    key('forwardslash', ['alt', 'cmd']),
  ),
  [TOGGLE_FOCUSED_OMNIBOX_TAB]: shortcut(
    'Focus the omnibox or toggle its current tab.',
    key('forwardslash', 'cmd'),
  ),
  [TOGGLE_NAVIGATOR]: shortcut('Toggle the navigator.', key('1', ['alt', 'cmd'])),
  [TOGGLE_INSPECTOR]: shortcut('Toggle the inspector.', key('2', ['alt', 'cmd'])),
  [TOGGLE_DESIGNER_ADDITIONAL_CONTROLS_SHORTCUT]: shortcut(
    'Toggle additional controls in the designer.',
    key('y', 'cmd'),
  ),
  [TOGGLE_CODE_EDITOR_SHORTCUT]: shortcut('Toggle the code editor.', key('period', 'cmd')),
  // FIXME: Is this needed as well as TOGGLE_LEFT_MENU_SHORTCUT and TOGGLE_RIGHT_MENU_SHORTCUT?
  [TOGGLE_INSPECTOR_AND_NAVIGATOR_SHORTCUT]: shortcut(
    'Toggle the inspector and the navigator.',
    key('backslash', 'cmd'),
  ),
  [ADD_ELEMENT_SHORTCUT]: shortcut('Add element...', key('a', [])),
  [OPEN_EYEDROPPER]: shortcut('Open the eyedropper', key('c', 'ctrl')),
  [TEXT_EDIT_MODE]: shortcut('Activate text edit mode', key('t', [])),
  [TOGGLE_TEXT_BOLD]: shortcut(
    'Toggle font-weight to bold of the currently selected text element.',
    key('b', ['cmd']),
  ),
  [TOGGLE_TEXT_ITALIC]: shortcut(
    'Toggle font-style to italic of the currently selected text element.',
    key('i', ['cmd']),
  ),
  [TOGGLE_TEXT_UNDERLINE]: shortcut(
    'Toggle text-decoration to underline of the currently selected text element.',
    key('u', ['cmd']),
  ),
  [TOGGLE_TEXT_STRIKE_THROUGH]: shortcut(
    'Toggle text-decoration to line-through of the currently selected text element.',
    key('x', ['cmd', 'shift']),
  ),
  [CONVERT_TO_FLEX_CONTAINER]: shortcut(
    'Convert selected elements to flex containers',
    key('a', ['shift']),
  ),
  [CONVERT_TO_GRID_CONTAINER]: shortcut(
    'Convert selected elements to grid containers',
    key('a', ['shift', 'alt']),
  ),
  [REMOVE_ABSOLUTE_POSITIONING]: shortcut(`Strip absolute sizing props props`, key('x', [])),
  [COPY_STYLE_PROPERTIES]: shortcut('Copy style properties', key('c', ['alt', 'cmd'])),
  [PASTE_STYLE_PROPERTIES]: shortcut('Paste style properties', key('v', ['alt', 'cmd'])),
  [PASTE_TO_REPLACE]: shortcut('Paste to replace', key('v', ['shift', 'cmd'])),
  [RESIZE_TO_FIT]: shortcut('Resize selected elements to fit', key('r', ['alt', 'cmd', 'shift'])),
  [OPEN_INSERT_MENU]: shortcut('Open insert menu', key('k', ['cmd'])),
  [WRAP_IN_DIV]: shortcut('Wrap the selected elements into a div', key('enter', ['cmd'])),
}

export type ShortcutConfiguration = { [key: string]: Array<Key> }

export type ShortcutNamesByKey = ComplexMap<Key, string>

export function applyShortcutConfigurationToDefaults(
  config: ShortcutConfiguration | null,
): ShortcutNamesByKey {
  const defaultedConfig: ShortcutConfiguration = config ?? {}
  let keyToShortcut: ComplexMap<Key, string> = emptyComplexMap<Key, string>()
  // Add the defaults indexed by the key combination, so as to eliminate multiple settings for
  // the same key combination.
  for (const defaultsShortcut of Object.keys(shortcutDetailsWithDefaults) as Array<string>) {
    // If this is configured, don't set the defaults.
    if (!(defaultsShortcut in defaultedConfig)) {
      const defaultsKeys: Shortcut = shortcutDetailsWithDefaults[defaultsShortcut]
      for (const defaultsKey of defaultsKeys.shortcutKeys) {
        keyToShortcut = addToComplexMap(
          Keyboard.keyToString,
          keyToShortcut,
          defaultsKey,
          defaultsShortcut,
        )
      }
    }
  }
  // Add the configuration indexed by the key combination, overriding those set by the defaults.
  for (const configShortcut of Object.keys(defaultedConfig) as Array<string>) {
    const configKeys = defaultedConfig[configShortcut]
    for (const configKey of configKeys) {
      keyToShortcut = addToComplexMap(
        Keyboard.keyToString,
        keyToShortcut,
        configKey,
        configShortcut,
      )
    }
  }

  return keyToShortcut
}

export function getShortcutDetails(config: ShortcutConfiguration | null): ShortcutDetails {
  let result: ShortcutDetails = {}
  // Ensure any configuration has been applied.
  const namesByKey = applyShortcutConfigurationToDefaults(config)
  for (const shortcutKey of getKeysFromComplexMap(namesByKey)) {
    const shortcutName = getValueFromComplexMap(Keyboard.keyToString, namesByKey, shortcutKey)
    if (shortcutName != null) {
      const currentShortcut = result[shortcutName]
      if (currentShortcut === undefined) {
        const shortcutDefault = shortcutDetailsWithDefaults[shortcutName]
        result[shortcutName] = shortcut(shortcutDefault.description, [shortcutKey])
      } else {
        result[shortcutName] = shortcut(currentShortcut.description, [
          ...currentShortcut.shortcutKeys,
          shortcutKey,
        ])
      }
    }
  }

  // Add in any which aren't included already, as those are ones without bindings.
  for (const defaultsShortcut of Object.keys(shortcutDetailsWithDefaults) as Array<string>) {
    if (!(defaultsShortcut in result)) {
      const shortcutDefault = shortcutDetailsWithDefaults[defaultsShortcut]
      result[defaultsShortcut] = shortcut(shortcutDefault.description, [])
    }
  }

  return result
}

export type ShortcutCallbacks<T> = { [key: string]: () => T }

export function handleShortcuts<T>(
  namesByKey: ShortcutNamesByKey,
  event: KeyboardEvent,
  defaultValue: T,
  callbacks: ShortcutCallbacks<T>,
): T {
  const eventKey = Keyboard.keyFromEvent(event)
  const shortcutName = getValueFromComplexMap(Keyboard.keyToString, namesByKey, eventKey)
  if (shortcutName == null) {
    return defaultValue
  } else {
    const callback = callbacks[shortcutName]
    if (callback == null) {
      return defaultValue
    } else {
      // we found a shortcut for this key combo, let's prevent default on the event
      event.preventDefault()
      return callback()
    }
  }
}

export function keyToString(shortcutKey: Key): string {
  const mods = shortcutKey.modifiers.map((m) => {
    switch (m) {
      case 'alt':
        return '⌥'
      case 'cmd':
        return '⌘'
      case 'ctrl':
        return '^'
      case 'shift':
        return '⬆'
      default:
        assertNever(m)
    }
  })

  const char = () => {
    switch (shortcutKey.character) {
      case 'period':
        return '.'
      default:
        return shortcutKey.character
    }
  }

  return [...mods, char()].join('')
}
