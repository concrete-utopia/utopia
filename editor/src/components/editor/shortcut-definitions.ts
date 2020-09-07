import { Key, Keyboard } from '../../utils/keyboard'
import { objectMap } from '../../core/shared/object-utils'
import {
  ComplexMap,
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
export const JUMP_TO_PARENT_SHORTCUT = 'jump-to-parent'
export const CANCEL_EVERYTHING_SHORTCUT = 'cancel-everything'
export const CYCLE_HIERACHY_TARGETS_SHORTCUT = 'cycle-hierachy-targets'
export const CYCLE_FORWARD_SIBLING_TARGETS_SHORTCUT = 'cycle-forward-sibling-targets'
export const CYCLE_BACKWARD_SIBLING_TARGETS_SHORTCUT = 'cycle-backward-sibling-targets'

export const RESIZE_ELEMENT_UP_SHORTCUT = 'resize-element-up'
export const RESIZE_ELEMENT_UP_MORE_SHORTCUT = 'resize-element-up-more'
export const MOVE_ELEMENT_UP_SHORTCUT = 'move-element-up'
export const MOVE_ELEMENT_UP_MORE_SHORTCUT = 'move-element-up-more'
export const RESIZE_ELEMENT_DOWN_SHORTCUT = 'resize-element-down'
export const RESIZE_ELEMENT_DOWN_MORE_SHORTCUT = 'resize-element-down-more'
export const MOVE_ELEMENT_DOWN_SHORTCUT = 'move-element-down'
export const MOVE_ELEMENT_DOWN_MORE_SHORTCUT = 'move-element-down-more'
export const RESIZE_ELEMENT_LEFT_SHORTCUT = 'resize-element-left'
export const RESIZE_ELEMENT_LEFT_MORE_SHORTCUT = 'resize-element-left-more'
export const MOVE_ELEMENT_LEFT_SHORTCUT = 'move-element-left'
export const MOVE_ELEMENT_LEFT_MORE_SHORTCUT = 'move-element-left-more'
export const RESIZE_ELEMENT_RIGHT_SHORTCUT = 'resize-element-right'
export const RESIZE_ELEMENT_RIGHT_MORE_SHORTCUT = 'resize-element-right-more'
export const MOVE_ELEMENT_RIGHT_SHORTCUT = 'move-element-right'
export const MOVE_ELEMENT_RIGHT_MORE_SHORTCUT = 'move-element-right-more'

export const SELECT_ALL_SIBLINGS_SHORTCUT = 'select-all-siblings'
export const TOGGLE_TEXT_BOLD_SHORTCUT = 'toggle-text-bold'
export const TOGGLE_BORDER_SHORTCUT = 'toggle-border'
export const COPY_SELECTION_SHORTCUT = 'copy-selection'
export const DUPLICATE_SELECTION_SHORTCUT = 'duplicate-selection'
export const TOGGLE_BACKGROUND_SHORTCUT = 'toggle-background'
export const UNWRAP_ELEMENT_SHORTCUT = 'unwrap-element'
export const WRAP_ELEMENT_SHORTCUT = 'wrap-element'
export const TOGGLE_HIDDEN_SHORTCUT = 'toggle-hidden'
export const TOGGLE_TEXT_ITALIC_SHORTCUT = 'toggle-text-italic'
export const INSERT_IMAGE_SHORTCUT = 'insert-image'
export const TOGGLE_PREVIEW_SHORTCUT = 'toggle-preview'
export const TOGGLE_LIVE_CANVAS_SHORTCUT = 'toggle-live-canvas'
export const START_RENAMING_SHORTCUT = 'start-renaming'
export const INSERT_RECTANGLE_SHORTCUT = 'insert-rectangle'
export const INSERT_ELLIPSE_SHORTCUT = 'insert-ellipse'
export const SAVE_CURRENT_FILE_SHORTCUT = 'save-current-file'
export const TOGGLE_SHADOW_SHORTCUT = 'toggle-shadow'
export const INSERT_TEXT_SHORTCUT = 'insert-text'
export const INSERT_VIEW_SHORTCUT = 'insert-view'
export const CUT_SELECTION_SHORTCUT = 'cut-selection'
export const UNDO_CHANGES_SHORTCUT = 'undo-changes'
export const REDO_CHANGES_SHORTCUT = 'redo-changes'
export const HIDE_HIGHLIGHTS_SHORTCUT = 'hide-highlights'
export const SHOW_HIGHLIGHTS_SHORTCUT = 'show-highlights'
export const MOVE_ELEMENT_FORWARD_SHORTCUT = 'move-element-forward'
export const MOVE_ELEMENT_TO_FRONT_SHORTCUT = 'move-element-to-front'
export const MOVE_ELEMENT_BACKWARD_SHORTCUT = 'move-element-backward'
export const MOVE_ELEMENT_TO_BACK_SHORTCUT = 'move-element-to-back'
export const TOGGLE_TEXT_UNDERLINE_SHORTCUT = 'toggle-text-underline'
export const TOGGLE_LEFT_MENU_SHORTCUT = 'toggle-left-menu'
export const TOGGLE_RIGHT_MENU_SHORTCUT = 'toggle-right-menu'
export const TOGGLE_DESIGNER_ADDITIONAL_CONTROLS_SHORTCUT = 'toggle-designer-additional-controls'
export const TOGGLE_CODE_EDITOR_SHORTCUT = 'toggle-code-editor'
export const TOGGLE_INSPECTOR_AND_LEFT_MENU_SHORTCUT = 'toggle-inspector-and-left-menu'
export const TOGGLE_DESIGNER_LAYOUT_REVERSED = 'toggle-designer-layout-reversed'

export type ShortcutDetails = { [key: string]: Shortcut }

const shortcutDetailsWithDefaults: ShortcutDetails = {
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
  [JUMP_TO_PARENT_SHORTCUT]: shortcut('Jump to parent element.', key('enter', 'shift')),
  [CANCEL_EVERYTHING_SHORTCUT]: shortcut(
    'Exit insert mode, dragging or anything else back to default.',
    key('esc', []),
  ),
  [CYCLE_HIERACHY_TARGETS_SHORTCUT]: shortcut(
    'Cycle up and down the hierarchy of the currently selected element.',
    key('space', []),
  ),
  [CYCLE_FORWARD_SIBLING_TARGETS_SHORTCUT]: shortcut(
    'Cycle forward between siblings of the currently selected element.',
    key('tab', []),
  ),
  [CYCLE_BACKWARD_SIBLING_TARGETS_SHORTCUT]: shortcut(
    'Cycle backward between siblings of the currently selected element.',
    key('tab', 'shift'),
  ),
  [RESIZE_ELEMENT_UP_SHORTCUT]: shortcut('Resize top edge of selected elements.', key('up', 'cmd')),
  [MOVE_ELEMENT_UP_SHORTCUT]: shortcut('Move element up in the y-axis.', key('up', [])),
  [RESIZE_ELEMENT_UP_MORE_SHORTCUT]: shortcut(
    'Fast resize top edge of selected elements.',
    key('up', ['cmd', 'shift']),
  ),
  [MOVE_ELEMENT_UP_MORE_SHORTCUT]: shortcut(
    'Fast move element up in the y-axis.',
    key('up', ['shift']),
  ),
  [RESIZE_ELEMENT_DOWN_SHORTCUT]: shortcut(
    'Resize bottom edge of selected elements.',
    key('down', 'cmd'),
  ),
  [MOVE_ELEMENT_DOWN_SHORTCUT]: shortcut('Move element down in the y-axis.', key('down', [])),
  [RESIZE_ELEMENT_DOWN_MORE_SHORTCUT]: shortcut(
    'Fast resize bottom edge of selected elements.',
    key('down', ['cmd', 'shift']),
  ),
  [MOVE_ELEMENT_DOWN_MORE_SHORTCUT]: shortcut(
    'Fast move element down in the y-axis.',
    key('down', ['shift']),
  ),
  [RESIZE_ELEMENT_LEFT_SHORTCUT]: shortcut(
    'Resize left edge of selected elements.',
    key('left', 'cmd'),
  ),
  [MOVE_ELEMENT_LEFT_SHORTCUT]: shortcut('Move element left in the x-axis.', key('left', [])),
  [RESIZE_ELEMENT_LEFT_MORE_SHORTCUT]: shortcut(
    'Fast resize left edge of selected elements.',
    key('left', ['cmd', 'shift']),
  ),
  [MOVE_ELEMENT_LEFT_MORE_SHORTCUT]: shortcut(
    'Fast move element left in the x-axis.',
    key('left', ['shift']),
  ),
  [RESIZE_ELEMENT_RIGHT_SHORTCUT]: shortcut(
    'Resize right edge of selected elements.',
    key('right', 'cmd'),
  ),
  [MOVE_ELEMENT_RIGHT_SHORTCUT]: shortcut('Move element right in the x-axis.', key('right', [])),
  [RESIZE_ELEMENT_RIGHT_MORE_SHORTCUT]: shortcut(
    'Fast resize right edge of selected elements.',
    key('right', ['cmd', 'shift']),
  ),
  [MOVE_ELEMENT_RIGHT_MORE_SHORTCUT]: shortcut(
    'Fast move element right in the x-axis.',
    key('right', ['shift']),
  ),
  [SELECT_ALL_SIBLINGS_SHORTCUT]: shortcut(
    'Select all siblings of currently selected element.',
    key('a', 'cmd'),
  ),
  [TOGGLE_TEXT_BOLD_SHORTCUT]: shortcut(
    'Toggle the bold attribute of the current text element.',
    key('b', 'cmd'),
  ),
  [TOGGLE_BORDER_SHORTCUT]: shortcut(
    'Toggle the border of the currently selected text element.',
    key('b', []),
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
    key('f', []),
  ),
  [UNWRAP_ELEMENT_SHORTCUT]: shortcut(
    'Unwrap children of an element into their grandparent element.',
    key('g', ['cmd', 'shift']),
  ),
  [WRAP_ELEMENT_SHORTCUT]: shortcut('Wrap elements with a group.', key('g', 'cmd')),
  [TOGGLE_HIDDEN_SHORTCUT]: shortcut('Toggle element as hidden.', key('h', ['cmd', 'shift'])),
  [TOGGLE_TEXT_ITALIC_SHORTCUT]: shortcut(
    'Toggle the italic attribute of the current text element.',
    key('i', 'cmd'),
  ),
  [INSERT_IMAGE_SHORTCUT]: shortcut('Insert an image.', key('i', [])),
  [TOGGLE_PREVIEW_SHORTCUT]: shortcut('Toggle the preview panel.', key('p', 'cmd')),
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
  [TOGGLE_SHADOW_SHORTCUT]: shortcut(
    'Toggle the shadow of the currently selected element.',
    key('s', []),
  ),
  [INSERT_TEXT_SHORTCUT]: shortcut('Insert a text element.', key('t', [])),
  [INSERT_VIEW_SHORTCUT]: shortcut('Insert a view.', key('v', [])),
  [CUT_SELECTION_SHORTCUT]: shortcut(
    'Cut the current selection to the clipboard.',
    key('x', 'cmd'),
  ),
  [UNDO_CHANGES_SHORTCUT]: shortcut('Undo the most recent change.', key('z', 'cmd')),
  [REDO_CHANGES_SHORTCUT]: shortcut(
    'Redo the most recently undone change.',
    key('z', ['cmd', 'shift']),
  ),
  // FIXME: Do these shortcuts need to specify keyup/keydown state?
  [HIDE_HIGHLIGHTS_SHORTCUT]: shortcut('Hide the highlights.', key('z', [], 'keydown')),
  [SHOW_HIGHLIGHTS_SHORTCUT]: shortcut('Show the highlights.', key('z', [], 'keyup')),
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
  [TOGGLE_TEXT_UNDERLINE_SHORTCUT]: shortcut(
    'Toggle the underline attribute of the current text element.',
    key('u', 'cmd'),
  ),
  [TOGGLE_LEFT_MENU_SHORTCUT]: shortcut(
    'Toggle the left hand menu panel.',
    key('1', ['alt', 'cmd']),
  ),
  [TOGGLE_RIGHT_MENU_SHORTCUT]: shortcut(
    'Toggle the right hand menu panel.',
    key('2', ['alt', 'cmd']),
  ),
  [TOGGLE_DESIGNER_ADDITIONAL_CONTROLS_SHORTCUT]: shortcut(
    'Toggle additional controls in the designer.',
    key('y', 'cmd'),
  ),
  [TOGGLE_CODE_EDITOR_SHORTCUT]: shortcut('Toggle the code editor.', key('period', 'cmd')),
  // FIXME: Is this needed as well as TOGGLE_LEFT_MENU_SHORTCUT and TOGGLE_RIGHT_MENU_SHORTCUT?
  [TOGGLE_INSPECTOR_AND_LEFT_MENU_SHORTCUT]: shortcut(
    'Toggle the inspector and the left menu.',
    key('backslash', 'cmd'),
  ),
  [TOGGLE_DESIGNER_LAYOUT_REVERSED]: shortcut(
    'Toggle designer layout reversed vertically.',
    key('comma', 'cmd'),
  ),
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
      return callback()
    }
  }
}
