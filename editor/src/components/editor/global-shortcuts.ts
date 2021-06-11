import { findElementAtPath, MetadataUtils } from '../../core/model/element-metadata-utils'
import { generateUidWithExistingComponents } from '../../core/model/element-template-utils'
import {
  isUtopiaAPITextElement,
  isUtopiaAPITextElementFromMetadata,
} from '../../core/model/project-file-utils'
import { importAlias, importDetails, ElementPath } from '../../core/shared/project-file-types'
import * as PP from '../../core/shared/property-path'
import * as EP from '../../core/shared/element-path'
import { emptyComments } from '../../core/workers/parser-printer/parser-printer-comments'
import Keyboard, {
  KeyCharacter,
  KeysPressed,
  modifiersForEvent,
  StoredKeyCharacters,
  strictCheckModifiers,
} from '../../utils/keyboard'
import { Modifier, Modifiers } from '../../utils/modifiers'
import Utils from '../../utils/utils'
import Canvas, { TargetSearchType } from '../canvas/canvas'
import CanvasActions from '../canvas/canvas-actions'
import { adjustAllSelectedFrames } from '../canvas/controls/select-mode/move-utils'
import { getAllTargetsAtPoint } from '../canvas/dom-lookup'
import {
  toggleBackgroundLayers,
  toggleBorder,
  toggleShadow,
  toggleStylePropPath,
  toggleStylePropPaths,
} from '../inspector/common/css-utils'
import { toggleTextFormatting } from '../text-utils'
import { EditorAction, EditorDispatch } from './action-types'
import * as EditorActions from './actions/action-creators'
import {
  defaultEllipseElement,
  defaultRectangleElement,
  defaultTextElement,
  defaultViewElement,
} from './defaults'
import {
  EditorModes,
  isInsertMode,
  isLiveMode,
  isSelectLiteMode,
  isSelectMode,
} from './editor-modes'
import { insertImage } from './image-insert'
import {
  CANCEL_EVERYTHING_SHORTCUT,
  COPY_SELECTION_SHORTCUT,
  CUT_SELECTION_SHORTCUT,
  CYCLE_BACKWARD_SIBLING_TARGETS_SHORTCUT,
  CYCLE_FORWARD_SIBLING_TARGETS_SHORTCUT,
  CYCLE_HIERACHY_TARGETS_SHORTCUT,
  DELETE_SELECTED_SHORTCUT,
  DUPLICATE_SELECTION_SHORTCUT,
  FIRST_CHILD_OR_EDIT_TEXT_SHORTCUT,
  handleShortcuts,
  HIDE_HIGHLIGHTS_SHORTCUT,
  INSERT_ELLIPSE_SHORTCUT,
  INSERT_IMAGE_SHORTCUT,
  INSERT_RECTANGLE_SHORTCUT,
  INSERT_TEXT_SHORTCUT,
  INSERT_VIEW_SHORTCUT,
  JUMP_TO_PARENT_SHORTCUT,
  MOVE_ELEMENT_BACKWARD_SHORTCUT,
  MOVE_ELEMENT_DOWN_MORE_SHORTCUT,
  MOVE_ELEMENT_DOWN_SHORTCUT,
  MOVE_ELEMENT_FORWARD_SHORTCUT,
  MOVE_ELEMENT_LEFT_MORE_SHORTCUT,
  MOVE_ELEMENT_LEFT_SHORTCUT,
  MOVE_ELEMENT_RIGHT_MORE_SHORTCUT,
  MOVE_ELEMENT_RIGHT_SHORTCUT,
  MOVE_ELEMENT_TO_BACK_SHORTCUT,
  MOVE_ELEMENT_TO_FRONT_SHORTCUT,
  MOVE_ELEMENT_UP_MORE_SHORTCUT,
  MOVE_ELEMENT_UP_SHORTCUT,
  REDO_CHANGES_SHORTCUT,
  RESET_CANVAS_ZOOM_SHORTCUT,
  RESIZE_ELEMENT_DOWN_MORE_SHORTCUT,
  RESIZE_ELEMENT_DOWN_SHORTCUT,
  RESIZE_ELEMENT_LEFT_MORE_SHORTCUT,
  RESIZE_ELEMENT_LEFT_SHORTCUT,
  RESIZE_ELEMENT_RIGHT_MORE_SHORTCUT,
  RESIZE_ELEMENT_RIGHT_SHORTCUT,
  RESIZE_ELEMENT_UP_MORE_SHORTCUT,
  RESIZE_ELEMENT_UP_SHORTCUT,
  SAVE_CURRENT_FILE_SHORTCUT,
  SELECT_ALL_SIBLINGS_SHORTCUT,
  SHOW_HIGHLIGHTS_SHORTCUT,
  START_RENAMING_SHORTCUT,
  TOGGLE_BACKGROUND_SHORTCUT,
  TOGGLE_BORDER_SHORTCUT,
  TOGGLE_CODE_EDITOR_SHORTCUT,
  TOGGLE_DESIGNER_ADDITIONAL_CONTROLS_SHORTCUT,
  TOGGLE_HIDDEN_SHORTCUT,
  TOGGLE_INSPECTOR_AND_LEFT_MENU_SHORTCUT,
  TOGGLE_LEFT_MENU_SHORTCUT,
  TOGGLE_LIVE_CANVAS_SHORTCUT,
  TOGGLE_PREVIEW_SHORTCUT,
  TOGGLE_RIGHT_MENU_SHORTCUT,
  TOGGLE_SHADOW_SHORTCUT,
  TOGGLE_TEXT_BOLD_SHORTCUT,
  TOGGLE_TEXT_ITALIC_SHORTCUT,
  TOGGLE_TEXT_UNDERLINE_SHORTCUT,
  UNDO_CHANGES_SHORTCUT,
  UNWRAP_ELEMENT_SHORTCUT,
  WRAP_ELEMENT_SHORTCUT,
  ZOOM_CANVAS_IN_SHORTCUT,
  ZOOM_CANVAS_OUT_SHORTCUT,
  ZOOM_UI_IN_SHORTCUT,
  ZOOM_UI_OUT_SHORTCUT,
  ShortcutNamesByKey,
} from './shortcut-definitions'
import { DerivedState, EditorState, getOpenFile } from './store/editor-state'
import { CanvasMousePositionRaw, WindowMousePositionRaw } from '../../utils/global-positions'

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

function isEventFromInput(target: any) {
  return target.tagName?.toLowerCase() === 'input' || target.tagName?.toLowerCase() === 'textarea'
}

function editorIsTarget(event: KeyboardEvent, editor: EditorState): boolean {
  return !isEventFromInput(event.target) && editor.modal == null
}

function jumpToParentActions(selectedViews: Array<ElementPath>): Array<EditorAction> {
  const jumpResult = Canvas.jumpToParent(selectedViews)
  switch (jumpResult) {
    case null:
      return []
    case 'CLEAR':
      return [EditorActions.clearSelection()]
    default:
      return [EditorActions.selectComponents([jumpResult], false)]
  }
}

function getTextEditorTarget(editor: EditorState, derived: DerivedState): ElementPath | null {
  if (editor.canvas.dragState != null || editor.selectedViews.length !== 1) {
    return null
  } else {
    const target = editor.selectedViews[0]
    const element = MetadataUtils.findElementByElementPath(editor.jsxMetadata, target)
    if (element != null && isUtopiaAPITextElementFromMetadata(element)) {
      return target
    } else {
      return null
    }
  }
}

function shouldTabBeHandledByBrowser(editor: EditorState): boolean {
  return editor.focusedPanel === 'inspector' || editor.focusedPanel === 'dependencylist'
}

export function preventBrowserShortcuts(editor: EditorState, event: KeyboardEvent): void {
  const key = Keyboard.keyCharacterForCode(event.keyCode)
  const modifiers = modifiersForEvent(event)
  const cmd = strictCheckModifiers(modifiers, ['cmd'])
  const altCmd = strictCheckModifiers(modifiers, ['alt', 'cmd'])
  const shiftCmd = strictCheckModifiers(modifiers, ['shift', 'cmd'])

  switch (key) {
    case 'tab':
      if (!shouldTabBeHandledByBrowser(editor)) {
        event.preventDefault()
      }
      break
    case 'plus':
      if (cmd) {
        event.preventDefault()
      }
      break
    case 'minus':
      if (cmd) {
        event.preventDefault()
      }
      break
    case 'up':
    case 'down':
      if (cmd) {
        event.preventDefault()
      }
      break
    case 'left':
    case 'right':
      if (cmd) {
        event.preventDefault()
      }
      break
    case 'b':
      if (cmd) {
        event.preventDefault()
      }
      break
    case 'c':
      if (altCmd) {
        // copy style, prevent console from opening
        event.preventDefault()
      }
      break
    case 'd':
      if (cmd) {
        event.preventDefault()
      }
      break
    case 'f':
      if (cmd) {
        event.preventDefault()
      }
      break
    case 'g':
      if (cmd) {
        event.preventDefault()
      } else if (shiftCmd) {
        event.preventDefault()
      }
      break
    case 'h':
      if (cmd) {
        event.preventDefault()
      } else if (shiftCmd) {
        event.preventDefault()
      }
      break
    case 'i':
      if (cmd) {
        event.preventDefault()
      }
      break
    case 'p':
      if (cmd) {
        event.preventDefault()
      } else if (shiftCmd) {
        event.preventDefault()
      }
      break
    case 'r':
      if (cmd) {
        event.preventDefault()
      }
      break
    case 's':
      if (cmd) {
        event.preventDefault()
      }
      break
    case 'z':
      if (cmd || shiftCmd) {
        event.preventDefault()
      }
      break
    case '[':
      if (cmd) {
        event.preventDefault()
      }
      break
    case ']':
      if (cmd) {
        event.preventDefault()
      }
      break
    case 'u': {
      if (cmd) {
        event.preventDefault()
      }
      break
    }
    case 'y': {
      if (cmd) {
        event.preventDefault()
      }
      break
    }
    case 'comma': {
      if (cmd) {
        // prevent opening new tab
        event.preventDefault()
      }
      break
    }
  }
}

export function handleKeyDown(
  event: KeyboardEvent,
  editor: EditorState,
  derived: DerivedState,
  namesByKey: ShortcutNamesByKey,
  dispatch: EditorDispatch,
): void {
  // Stop the browser from firing things like save dialogs.
  preventBrowserShortcuts(editor, event)

  // Ensure that any key presses are appropriately recorded.
  const key = Keyboard.keyCharacterForCode(event.keyCode)
  const editorTargeted = editorIsTarget(event, editor)
  let updatedKeysPressed: KeysPressed
  if (editorTargeted) {
    updatedKeysPressed = updateKeysPressed(
      editor.keysPressed,
      key,
      true,
      Modifier.modifiersForKeyboardEvent(event),
    )
  } else {
    updatedKeysPressed = updateModifiers(
      editor.keysPressed,
      Modifier.modifiersForKeyboardEvent(event),
    )
  }
  const updateKeysAction = EditorActions.updateKeys(updatedKeysPressed)

  function cycleSiblings(forwards: boolean): Array<EditorAction> {
    if (isSelectMode(editor.mode) || isSelectLiteMode(editor.mode)) {
      const tabbedTo = Canvas.jumpToSibling(editor.selectedViews, editor.jsxMetadata, forwards)
      if (tabbedTo != null) {
        return [EditorActions.selectComponents([tabbedTo], false)]
      }
    }
    return []
  }

  function adjustFrames(
    isResizing: boolean,
    direction: 'vertical' | 'horizontal',
    directionModifier: -1 | 1,
    adjustment: 1 | 10,
  ): Array<EditorAction> {
    const adjustmentActions = adjustAllSelectedFrames(
      editor,
      dispatch,
      false,
      isResizing,
      directionModifier,
      direction,
      adjustment,
    )
    return [EditorActions.transientActions(adjustmentActions)]
  }

  function getUIFileActions(): Array<EditorAction> {
    if (key === 'tab' && shouldTabBeHandledByBrowser(editor)) {
      return []
    }
    return handleShortcuts<Array<EditorAction>>(namesByKey, event, [], {
      [DELETE_SELECTED_SHORTCUT]: () => {
        return isSelectMode(editor.mode) ? [EditorActions.deleteSelected()] : []
      },
      [RESET_CANVAS_ZOOM_SHORTCUT]: () => {
        return [CanvasActions.zoom(1)]
      },
      [ZOOM_UI_IN_SHORTCUT]: () => {
        return [CanvasActions.zoomUI(true)]
      },
      [ZOOM_CANVAS_IN_SHORTCUT]: () => {
        return [CanvasActions.zoom(Utils.increaseScale(editor.canvas.scale))]
      },
      [ZOOM_UI_OUT_SHORTCUT]: () => {
        return [CanvasActions.zoomUI(false)]
      },
      [ZOOM_CANVAS_OUT_SHORTCUT]: () => {
        return [CanvasActions.zoom(Utils.decreaseScale(editor.canvas.scale))]
      },
      [FIRST_CHILD_OR_EDIT_TEXT_SHORTCUT]: () => {
        if (isSelectMode(editor.mode) || isSelectLiteMode(editor.mode)) {
          const textTarget = getTextEditorTarget(editor, derived)
          if (textTarget != null && isSelectMode(editor.mode)) {
            return [EditorActions.openTextEditor(textTarget, null)]
          } else {
            const childToSelect = Canvas.getFirstChild(editor.selectedViews, editor.jsxMetadata)
            if (childToSelect != null) {
              return [EditorActions.selectComponents([childToSelect], false)]
            }
          }
        }
        return []
      },
      [JUMP_TO_PARENT_SHORTCUT]: () => {
        if (isSelectMode(editor.mode) || isSelectLiteMode(editor.mode)) {
          return jumpToParentActions(editor.selectedViews)
        } else {
          return []
        }
      },
      [CANCEL_EVERYTHING_SHORTCUT]: () => {
        if (isInsertMode(editor.mode)) {
          return [
            EditorActions.switchEditorMode(EditorModes.selectMode()),
            CanvasActions.clearDragState(false),
            EditorActions.clearHighlightedViews(),
          ]
        } else if (editor.canvas.dragState != null && editor.canvas.dragState.start != null) {
          return [CanvasActions.clearDragState(false)]
        } else if (isSelectMode(editor.mode) || isSelectLiteMode(editor.mode)) {
          return jumpToParentActions(editor.selectedViews)
        }

        // TODO: Move this around.
        if (isLiveMode(editor.mode)) {
          return [EditorActions.updateEditorMode(EditorModes.selectMode(editor.mode.controlId))]
        } else if (isInsertMode(editor.mode)) {
          return [EditorActions.updateEditorMode(EditorModes.selectMode())]
        }
        return []
      },
      [CYCLE_HIERACHY_TARGETS_SHORTCUT]: () => {
        if (isSelectMode(editor.mode) || isSelectLiteMode(editor.mode)) {
          if (CanvasMousePositionRaw == null) {
            return [EditorActions.clearSelection()]
          }
          const targetStack = getAllTargetsAtPoint(
            editor.jsxMetadata,
            editor.selectedViews,
            editor.hiddenInstances,
            'no-filter',
            WindowMousePositionRaw,
            editor.canvas.scale,
            editor.canvas.realCanvasOffset,
          )
          const nextTarget = Canvas.getNextTarget(editor.selectedViews, targetStack)
          if (targetStack.length === 0 || nextTarget === null) {
            return [EditorActions.clearSelection()]
          } else {
            return [EditorActions.selectComponents([nextTarget], false)]
          }
        }
        return []
      },
      [CYCLE_FORWARD_SIBLING_TARGETS_SHORTCUT]: () => {
        return cycleSiblings(true)
      },
      [CYCLE_BACKWARD_SIBLING_TARGETS_SHORTCUT]: () => {
        return cycleSiblings(false)
      },
      [RESIZE_ELEMENT_UP_SHORTCUT]: () => {
        return isSelectMode(editor.mode) ? adjustFrames(true, 'vertical', -1, 1) : []
      },
      [RESIZE_ELEMENT_UP_MORE_SHORTCUT]: () => {
        return isSelectMode(editor.mode) ? adjustFrames(true, 'vertical', -1, 10) : []
      },
      [MOVE_ELEMENT_UP_SHORTCUT]: () => {
        return isSelectMode(editor.mode) ? adjustFrames(false, 'vertical', -1, 1) : []
      },
      [MOVE_ELEMENT_UP_MORE_SHORTCUT]: () => {
        return isSelectMode(editor.mode) ? adjustFrames(false, 'vertical', -1, 10) : []
      },
      [RESIZE_ELEMENT_DOWN_SHORTCUT]: () => {
        return isSelectMode(editor.mode) ? adjustFrames(true, 'vertical', 1, 1) : []
      },
      [RESIZE_ELEMENT_DOWN_MORE_SHORTCUT]: () => {
        return isSelectMode(editor.mode) ? adjustFrames(true, 'vertical', 1, 10) : []
      },
      [MOVE_ELEMENT_DOWN_SHORTCUT]: () => {
        return isSelectMode(editor.mode) ? adjustFrames(false, 'vertical', 1, 1) : []
      },
      [MOVE_ELEMENT_DOWN_MORE_SHORTCUT]: () => {
        return isSelectMode(editor.mode) ? adjustFrames(false, 'vertical', 1, 10) : []
      },
      [RESIZE_ELEMENT_LEFT_SHORTCUT]: () => {
        return isSelectMode(editor.mode) ? adjustFrames(true, 'horizontal', -1, 1) : []
      },
      [RESIZE_ELEMENT_LEFT_MORE_SHORTCUT]: () => {
        return isSelectMode(editor.mode) ? adjustFrames(true, 'horizontal', -1, 10) : []
      },
      [MOVE_ELEMENT_LEFT_SHORTCUT]: () => {
        return isSelectMode(editor.mode) ? adjustFrames(false, 'horizontal', -1, 1) : []
      },
      [MOVE_ELEMENT_LEFT_MORE_SHORTCUT]: () => {
        return isSelectMode(editor.mode) ? adjustFrames(false, 'horizontal', -1, 10) : []
      },
      [RESIZE_ELEMENT_RIGHT_SHORTCUT]: () => {
        return isSelectMode(editor.mode) ? adjustFrames(true, 'horizontal', 1, 1) : []
      },
      [RESIZE_ELEMENT_RIGHT_MORE_SHORTCUT]: () => {
        return isSelectMode(editor.mode) ? adjustFrames(true, 'horizontal', 1, 10) : []
      },
      [MOVE_ELEMENT_RIGHT_SHORTCUT]: () => {
        return isSelectMode(editor.mode) ? adjustFrames(false, 'horizontal', 1, 1) : []
      },
      [MOVE_ELEMENT_RIGHT_MORE_SHORTCUT]: () => {
        return isSelectMode(editor.mode) ? adjustFrames(false, 'horizontal', 1, 10) : []
      },
      [SELECT_ALL_SIBLINGS_SHORTCUT]: () => {
        return [EditorActions.selectAllSiblings()]
      },
      [TOGGLE_TEXT_BOLD_SHORTCUT]: () => {
        return toggleTextFormatting(editor, dispatch, 'bold')
      },
      [TOGGLE_BORDER_SHORTCUT]: () => {
        return isSelectMode(editor.mode)
          ? editor.selectedViews.map((target) =>
              EditorActions.toggleProperty(
                target,
                toggleStylePropPath(PP.create(['style', 'border']), toggleBorder),
              ),
            )
          : []
      },
      [COPY_SELECTION_SHORTCUT]: () => {
        return isSelectMode(editor.mode) ? [EditorActions.copySelectionToClipboard()] : []
      },
      [DUPLICATE_SELECTION_SHORTCUT]: () => {
        return isSelectMode(editor.mode) ? [EditorActions.duplicateSelected()] : []
      },
      [TOGGLE_BACKGROUND_SHORTCUT]: () => {
        return isSelectMode(editor.mode)
          ? editor.selectedViews.map((target) =>
              EditorActions.toggleProperty(target, toggleStylePropPaths(toggleBackgroundLayers)),
            )
          : []
      },
      [UNWRAP_ELEMENT_SHORTCUT]: () => {
        return isSelectMode(editor.mode)
          ? editor.selectedViews.map((target) => EditorActions.unwrapGroupOrView(target))
          : []
      },
      [WRAP_ELEMENT_SHORTCUT]: () => {
        return isSelectMode(editor.mode) ? [EditorActions.wrapInGroup(editor.selectedViews)] : []
      },
      [TOGGLE_HIDDEN_SHORTCUT]: () => {
        return [EditorActions.toggleHidden()]
      },
      [TOGGLE_TEXT_ITALIC_SHORTCUT]: () => {
        return isSelectMode(editor.mode) ? toggleTextFormatting(editor, dispatch, 'italic') : []
      },
      [INSERT_IMAGE_SHORTCUT]: () => {
        if (isSelectMode(editor.mode) || isInsertMode(editor.mode)) {
          // FIXME: Side effects.
          insertImage(dispatch)
        }
        return []
      },
      [TOGGLE_PREVIEW_SHORTCUT]: () => {
        return [EditorActions.togglePanel('preview')]
      },
      [TOGGLE_LIVE_CANVAS_SHORTCUT]: () => {
        return [EditorActions.toggleCanvasIsLive()]
      },
      [START_RENAMING_SHORTCUT]: () => {
        const exitInsertModeActions = [
          EditorActions.switchEditorMode(EditorModes.selectMode()),
          CanvasActions.clearDragState(false),
          EditorActions.clearHighlightedViews(),
        ]
        if (editor.selectedViews.length === 1) {
          const target = editor.selectedViews[0]
          return [EditorActions.setNavigatorRenamingTarget(target), ...exitInsertModeActions]
        } else {
          return exitInsertModeActions
        }
      },
      [INSERT_RECTANGLE_SHORTCUT]: () => {
        if (isSelectMode(editor.mode) || isInsertMode(editor.mode)) {
          const newUID = generateUidWithExistingComponents(editor.projectContents)
          return [
            EditorActions.enableInsertModeForJSXElement(
              defaultRectangleElement(newUID),
              newUID,
              {
                'utopia-api': importDetails(null, [importAlias('Rectangle')], null),
              },
              null,
            ),
          ]
        } else {
          return []
        }
      },
      [INSERT_ELLIPSE_SHORTCUT]: () => {
        if (isSelectMode(editor.mode) || isInsertMode(editor.mode)) {
          const newUID = generateUidWithExistingComponents(editor.projectContents)
          return [
            EditorActions.enableInsertModeForJSXElement(
              defaultEllipseElement(newUID),
              newUID,
              { 'utopia-api': importDetails(null, [importAlias('Ellipse')], null) },
              null,
            ),
          ]
        } else {
          return []
        }
      },
      [SAVE_CURRENT_FILE_SHORTCUT]: () => {
        return [EditorActions.saveCurrentFile()]
      },
      [TOGGLE_SHADOW_SHORTCUT]: () => {
        return editor.selectedViews.map((target) =>
          EditorActions.toggleProperty(
            target,
            toggleStylePropPath(PP.create(['style', 'boxShadow']), toggleShadow),
          ),
        )
      },
      [INSERT_TEXT_SHORTCUT]: () => {
        if (isSelectMode(editor.mode) || isInsertMode(editor.mode)) {
          const newUID = generateUidWithExistingComponents(editor.projectContents)
          return [
            EditorActions.enableInsertModeForJSXElement(
              defaultTextElement(newUID),
              newUID,
              { 'utopia-api': importDetails(null, [importAlias('Text')], null) },
              null,
            ),
          ]
        } else {
          return []
        }
      },
      [INSERT_VIEW_SHORTCUT]: () => {
        if (isSelectMode(editor.mode) || isInsertMode(editor.mode)) {
          const newUID = generateUidWithExistingComponents(editor.projectContents)
          return [
            EditorActions.enableInsertModeForJSXElement(
              defaultViewElement(newUID),
              newUID,
              { 'utopia-api': importDetails(null, [importAlias('View')], null) },
              null,
            ),
          ]
        } else {
          return []
        }
      },
      [CUT_SELECTION_SHORTCUT]: () => {
        return isSelectMode(editor.mode)
          ? [EditorActions.copySelectionToClipboard(), EditorActions.deleteSelected()]
          : []
      },
      [UNDO_CHANGES_SHORTCUT]: () => {
        return [EditorActions.undo()]
      },
      [REDO_CHANGES_SHORTCUT]: () => {
        return [EditorActions.redo()]
      },
      [HIDE_HIGHLIGHTS_SHORTCUT]: () => {
        return [EditorActions.setHighlightsEnabled(false), EditorActions.clearHighlightedViews()]
      },
      [MOVE_ELEMENT_BACKWARD_SHORTCUT]: () => {
        return isSelectMode(editor.mode) ? [EditorActions.moveSelectedBackward()] : []
      },
      [MOVE_ELEMENT_TO_BACK_SHORTCUT]: () => {
        return isSelectMode(editor.mode) ? [EditorActions.moveSelectedToBack()] : []
      },
      [MOVE_ELEMENT_FORWARD_SHORTCUT]: () => {
        return isSelectMode(editor.mode) ? [EditorActions.moveSelectedForward()] : []
      },
      [MOVE_ELEMENT_TO_FRONT_SHORTCUT]: () => {
        return isSelectMode(editor.mode) ? [EditorActions.moveSelectedToFront()] : []
      },
      [TOGGLE_TEXT_UNDERLINE_SHORTCUT]: () => {
        return isSelectMode(editor.mode) ? toggleTextFormatting(editor, dispatch, 'underline') : []
      },
      [TOGGLE_LEFT_MENU_SHORTCUT]: () => {
        return [EditorActions.togglePanel('leftmenu')]
      },
      [TOGGLE_RIGHT_MENU_SHORTCUT]: () => {
        return [EditorActions.togglePanel('rightmenu')]
      },
      [TOGGLE_DESIGNER_ADDITIONAL_CONTROLS_SHORTCUT]: () => {
        return [EditorActions.toggleInterfaceDesignerAdditionalControls()]
      },
      [TOGGLE_CODE_EDITOR_SHORTCUT]: () => {
        return [EditorActions.toggleInterfaceDesignerCodeEditor()]
      },
      [TOGGLE_INSPECTOR_AND_LEFT_MENU_SHORTCUT]: () => {
        return [EditorActions.togglePanel('inspector'), EditorActions.togglePanel('leftmenu')]
      },
    })
  }

  function getShortcutActions(): Array<EditorAction> {
    const openFile = getOpenFile(editor)
    if (openFile == null) {
      return []
    } else {
      switch (openFile.type) {
        case 'TEXT_FILE':
          return getUIFileActions()
        default:
          return []
      }
    }
  }

  // Build the actions to dispatch.
  let actions: Array<EditorAction> = [updateKeysAction]
  if (editorTargeted) {
    actions.push(...getShortcutActions())
  }

  dispatch(actions, 'everyone')
}

export function handleKeyUp(
  event: KeyboardEvent,
  editor: EditorState,
  namesByKey: ShortcutNamesByKey,
  dispatch: EditorDispatch,
): void {
  // Stop the browser from firing things like save dialogs.
  preventBrowserShortcuts(editor, event)

  // Ensure that any key presses are appropriately recorded.
  const key = Keyboard.keyCharacterForCode(event.keyCode)
  const editorTargeted = editorIsTarget(event, editor)
  let updatedKeysPressed: KeysPressed
  if (editorTargeted) {
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

  function getUIFileActions(): Array<EditorAction> {
    return handleShortcuts<Array<EditorAction>>(namesByKey, event, [], {
      [SHOW_HIGHLIGHTS_SHORTCUT]: () => {
        return [EditorActions.setHighlightsEnabled(true)]
      },
    })
  }

  function getShortcutActions(): Array<EditorAction> {
    const openFile = getOpenFile(editor)
    if (openFile == null) {
      return []
    } else {
      switch (openFile.type) {
        case 'TEXT_FILE':
          return getUIFileActions()
        default:
          return []
      }
    }
  }

  // Build the actions to dispatch.
  let actions: Array<EditorAction> = [updateKeysAction]
  if (editorTargeted) {
    actions.push(...getShortcutActions())
  }

  dispatch(actions, 'everyone')
}
