import { findElementAtPath } from '../../core/model/element-metadata-utils'
import { isUtopiaAPITextElement } from '../../core/model/project-file-utils'
import {
  InstancePath,
  TemplatePath,
  importDetails,
  importAlias,
} from '../../core/shared/project-file-types'
import * as TP from '../../core/shared/template-path'
import * as PP from '../../core/shared/property-path'
import Keyboard, {
  KeyCharacter,
  modifiersForEvent,
  strictCheckModifiers,
  looseCheckModifier,
  KeysPressed,
  StoredKeyCharacters,
} from '../../utils/keyboard'
import Utils from '../../utils/utils'
import Canvas, { TargetSearchType } from '../canvas/canvas'
import CanvasActions from '../canvas/canvas-actions'
import { toggleTextFormatting } from '../text-utils'
import { EditorAction, EditorDispatch } from './action-types'
import * as EditorActions from './actions/actions'
import { EditorModes, isInsertMode, isLiveMode } from './editor-modes'
import {
  EditorState,
  getOpenImportsFromState,
  getOpenUtopiaJSXComponentsFromState,
  getOpenFile,
} from './store/editor-state'

import { LeftMenuTab } from '../navigator/left-pane'

import { CanvasMousePositionRaw } from '../../templates/editor-canvas'
import { adjustAllSelectedFrames } from '../canvas/controls/select-mode/move-utils'
import {
  toggleStylePropPath,
  toggleBorder,
  toggleStylePropPaths,
  toggleBackgroundLayers,
  toggleShadow,
} from '../inspector/common/css-utils'
import { generateUidWithExistingComponents } from '../../core/model/element-template-utils'
import {
  defaultRectangleElement,
  defaultEllipseElement,
  defaultTextElement,
  defaultViewElement,
} from './defaults'
import { insertImage } from './image-insert'
import { Modifiers, Modifier } from '../../utils/modifiers'

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

function jumpToParentActions(selectedViews: Array<TemplatePath>): Array<EditorAction> {
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

function getTextEditorTarget(editor: EditorState): InstancePath | null {
  if (editor.canvas.dragState != null || editor.selectedViews.length !== 1) {
    return null
  } else {
    const target = editor.selectedViews[0]
    if (TP.isScenePath(target)) {
      return null
    }

    const components = getOpenUtopiaJSXComponentsFromState(editor)
    const imports = getOpenImportsFromState(editor)
    const element = findElementAtPath(target, components, editor.jsxMetadataKILLME)
    if (element != null && isUtopiaAPITextElement(element, imports)) {
      return target
    } else {
      return null
    }
  }
}

function frameAdjustment(shiftPressed: boolean): 10 | 1 {
  return shiftPressed ? 10 : 1
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
      true,
      Modifier.modifiersForKeyboardEvent(event),
    )
  }
  const updateKeysAction = EditorActions.updateKeys(updatedKeysPressed)

  // Modifiers and the like used throughout various conditions.
  const modifiers = modifiersForEvent(event)
  const cmd = strictCheckModifiers(modifiers, ['cmd'])
  const shift = strictCheckModifiers(modifiers, ['shift'])
  const altCmd = strictCheckModifiers(modifiers, ['alt', 'cmd'])
  const shiftCmd = strictCheckModifiers(modifiers, ['shift', 'cmd'])
  const noModifier = modifiers.length === 0
  const modeType = editor.mode.type
  const adjustment = frameAdjustment(looseCheckModifier(modifiers, 'shift'))

  function getUIFileActions(): Array<EditorAction> {
    switch (key) {
      case 'backspace':
      case 'delete':
        if (editor.mode.type === 'select') {
          if (cmd) {
            return editor.selectedViews.map((target) => EditorActions.unwrapGroupOrView(target))
          }
        }
        return [EditorActions.deleteSelected()]
      case '0':
        if (cmd) {
          return [CanvasActions.zoom(1)]
        }
        break
      case 'plus':
        if (altCmd) {
          return [CanvasActions.zoomUI(true)]
        } else if (cmd) {
          return [CanvasActions.zoom(Utils.increaseScale(editor.canvas.scale))]
        }
        break
      case 'minus':
        if (altCmd) {
          return [CanvasActions.zoomUI(false)]
        } else if (cmd) {
          return [CanvasActions.zoom(Utils.decreaseScale(editor.canvas.scale))]
        }
        break
      case 'enter':
        if (noModifier && modeType === 'select') {
          const textTarget = getTextEditorTarget(editor)
          if (textTarget != null) {
            return [EditorActions.openTextEditor(textTarget, null)]
          } else {
            const childToSelect = Canvas.getFirstChild(
              editor.selectedViews,
              editor.jsxMetadataKILLME,
            )
            if (childToSelect != null) {
              return [EditorActions.selectComponents([childToSelect], false)]
            }
          }
        } else if (shift && modeType === 'select') {
          return jumpToParentActions(editor.selectedViews)
        } else if (cmd) {
          // Refer to the passthrough hack in monaco-wrapper in case this shortcut needs be changed
          return [EditorActions.toggleCanvasIsLive()]
        }
        break
      case 'esc':
        if (noModifier) {
          if (modeType === 'insert') {
            return [
              EditorActions.switchEditorMode(EditorModes.selectMode()),
              CanvasActions.clearDragState(false),
              EditorActions.clearHighlightedViews(),
              EditorActions.setLeftMenuTab(LeftMenuTab.UINavigate),
            ]
          } else if (editor.canvas.dragState != null && editor.canvas.dragState.start != null) {
            return [CanvasActions.clearDragState(false)]
          } else if (modeType === 'select') {
            return jumpToParentActions(editor.selectedViews)
          }
        }

        // TODO: Move this around.
        if (isLiveMode(editor.mode)) {
          return [EditorActions.updateEditorMode(EditorModes.selectMode(editor.mode.controlId))]
        } else if (isInsertMode(editor.mode)) {
          return [EditorActions.updateEditorMode(EditorModes.selectMode())]
        }
        break
      case 'space':
        if (noModifier && modeType === 'select') {
          if (CanvasMousePositionRaw == null) {
            return [EditorActions.clearSelection()]
          }
          const targetStack = Canvas.getAllTargetsAtPoint(
            editor,
            CanvasMousePositionRaw,
            [TargetSearchType.All],
            true,
            'strict', // _IF_ we want to enable loose targeting for selection, it means we also need to change component-area-control
          )
          const nextTarget = Canvas.getNextTarget(editor.selectedViews, targetStack)
          if (targetStack.length === 0 || nextTarget === null) {
            return [EditorActions.clearSelection()]
          } else {
            return [EditorActions.selectComponents([nextTarget], false)]
          }
        }
        break
      case 'tab':
        if (!shouldTabBeHandledByBrowser(editor)) {
          if ((shift || noModifier) && modeType === 'select') {
            const forwards = !shift
            const tabbedTo = Canvas.jumpToSibling(
              editor.selectedViews,
              editor.jsxMetadataKILLME,
              forwards,
            )
            if (tabbedTo != null) {
              return [EditorActions.selectComponents([tabbedTo], false)]
            }
          }
        }
        break
      case 'up':
      case 'down':
        if (altCmd) {
          return key === 'up'
            ? [EditorActions.moveSelectedForward()]
            : [EditorActions.moveSelectedBackward()]
        } else {
          const isResizing = looseCheckModifier(modifiers, 'cmd')
          const verticalAdjustmentActions = adjustAllSelectedFrames(
            editor,
            dispatch,
            false,
            isResizing,
            key === 'up' ? -1 : 1,
            'vertical',
            adjustment,
          )
          return [EditorActions.transientActions(verticalAdjustmentActions)]
        }
      case 'left':
      case 'right':
        const isResizing = looseCheckModifier(modifiers, 'cmd')
        const horizontalAdjustmentActions = adjustAllSelectedFrames(
          editor,
          dispatch,
          false,
          isResizing,
          key === 'left' ? -1 : 1,
          'horizontal',
          adjustment,
        )
        return [EditorActions.transientActions(horizontalAdjustmentActions)]
      case 'a':
        if (cmd) {
          return [EditorActions.selectAllSiblings()]
        }
        break
      case 'b':
        if (cmd) {
          return toggleTextFormatting(editor, dispatch, 'bold')
        } else if (noModifier && editor.selectedViews.length > 0) {
          return TP.filterScenes(editor.selectedViews).map((target) =>
            EditorActions.toggleProperty(
              target,
              toggleStylePropPath(PP.create(['style', 'border']), toggleBorder),
            ),
          )
        }
        break
      case 'c':
        if (cmd) {
          return [EditorActions.copySelectionToClipboard()]
        }
        break
      case 'd':
        if (cmd) {
          if (editor.selectedViews.length > 0) {
            return [EditorActions.duplicateSelected()]
          }
        }
        break
      case 'f':
        if (noModifier && editor.selectedViews.length > 0) {
          return TP.filterScenes(editor.selectedViews).map((target) =>
            EditorActions.toggleProperty(target, toggleStylePropPaths(toggleBackgroundLayers)),
          )
        }
        break
      case 'g':
        if (editor.selectedViews.length > 0) {
          if (shiftCmd) {
            return editor.selectedViews.map((target) => EditorActions.unwrapGroupOrView(target))
          } else if (cmd) {
            return [EditorActions.wrapInGroup(editor.selectedViews)]
          }
        }
        break
      case 'h':
        if (shiftCmd) {
          if (editor.selectedViews.length > 0) {
            return [EditorActions.toggleHidden()]
          }
        }
        break
      case 'i':
        if (cmd) {
          return toggleTextFormatting(editor, dispatch, 'italic')
        } else if (noModifier && (modeType === 'select' || modeType === 'insert')) {
          insertImage(dispatch)
          return []
        }
        break
      case 'p':
        if (cmd) {
          return [EditorActions.togglePanel('preview')]
        } else if (shiftCmd) {
          return [EditorActions.toggleCanvasIsLive()]
        }
        break
      case 'r':
        if (cmd) {
          const exitInsertModeActions = [
            EditorActions.switchEditorMode(EditorModes.selectMode()),
            CanvasActions.clearDragState(false),
            EditorActions.clearHighlightedViews(),
            EditorActions.setLeftMenuTab(LeftMenuTab.UINavigate),
          ]
          if (editor.selectedViews.length === 1) {
            const target = editor.selectedViews[0]
            return [EditorActions.setNavigatorRenamingTarget(target), ...exitInsertModeActions]
          } else {
            return exitInsertModeActions
          }
        }
        if (noModifier && (modeType === 'select' || modeType === 'insert')) {
          const utopiaComponents = getOpenUtopiaJSXComponentsFromState(editor)
          const newUID = generateUidWithExistingComponents(utopiaComponents)
          return [
            EditorActions.enableInsertModeForJSXElement(
              defaultRectangleElement(newUID),
              newUID,
              { 'utopia-api': importDetails(null, [importAlias('Rectangle')], null) },
              null,
            ),
          ]
        }
        break
      case 'e':
        if (noModifier && (modeType === 'select' || modeType === 'insert')) {
          const utopiaComponents = getOpenUtopiaJSXComponentsFromState(editor)
          const newUID = generateUidWithExistingComponents(utopiaComponents)
          return [
            EditorActions.enableInsertModeForJSXElement(
              defaultEllipseElement(newUID),
              newUID,
              { 'utopia-api': importDetails(null, [importAlias('Ellipse')], null) },
              null,
            ),
          ]
        }
        break
      case 's':
        if (cmd) {
          return [EditorActions.saveCurrentFile()]
        } else if (noModifier && modeType === 'select' && editor.selectedViews.length > 0) {
          return TP.filterScenes(editor.selectedViews).map((target) =>
            EditorActions.toggleProperty(
              target,
              toggleStylePropPath(PP.create(['style', 'boxShadow']), toggleShadow),
            ),
          )
        }
        break
      case 't':
        if (noModifier && (modeType === 'select' || modeType === 'insert')) {
          const utopiaComponents = getOpenUtopiaJSXComponentsFromState(editor)
          const newUID = generateUidWithExistingComponents(utopiaComponents)
          return [
            EditorActions.enableInsertModeForJSXElement(
              defaultTextElement(newUID),
              newUID,
              { 'utopia-api': importDetails(null, [importAlias('Text')], null) },
              null,
            ),
          ]
        }
        break
      case 'v':
        if (noModifier && (modeType === 'select' || modeType === 'insert')) {
          const utopiaComponents = getOpenUtopiaJSXComponentsFromState(editor)
          const newUID = generateUidWithExistingComponents(utopiaComponents)
          return [
            EditorActions.enableInsertModeForJSXElement(
              defaultViewElement(newUID),
              newUID,
              { 'utopia-api': importDetails(null, [importAlias('View')], null) },
              null,
            ),
          ]
        }
        break
      case 'x':
        if (cmd) {
          return [EditorActions.copySelectionToClipboard(), EditorActions.deleteSelected()]
        }
        break
      case 'z':
        if (cmd || shiftCmd) {
          if (shiftCmd) {
            return [EditorActions.redo()]
          } else {
            return [EditorActions.undo()]
          }
        } else if (noModifier) {
          return [EditorActions.setHighlightsEnabled(false), EditorActions.clearHighlightedViews()]
        }
        break
      case '[':
        if (cmd) {
          return [EditorActions.moveSelectedBackward()]
        } else if (altCmd) {
          return [EditorActions.moveSelectedToBack()]
        }
        break
      case ']':
        if (cmd) {
          return [EditorActions.moveSelectedForward()]
        } else if (altCmd) {
          return [EditorActions.moveSelectedToFront()]
        }
        break
      case 'u': {
        if (cmd) {
          return toggleTextFormatting(editor, dispatch, 'underline')
        }
        break
      }
      case '1': {
        if (altCmd) {
          return [EditorActions.togglePanel('leftmenu')]
        }
        break
      }
      case '2': {
        if (altCmd) {
          return [EditorActions.togglePanel('rightmenu')]
        }
        break
      }
      case 'y': {
        if (cmd) {
          return [EditorActions.toggleInterfaceDesignerAdditionalControls()]
        }
        break
      }
      case 'period': {
        if (cmd) {
          return [EditorActions.toggleInterfaceDesignerCodeEditor()]
        }
        break
      }
      case 'backslash': {
        if (cmd) {
          return [EditorActions.togglePanel('inspector'), EditorActions.togglePanel('leftmenu')]
        }
        break
      }
      case 'comma': {
        if (cmd) {
          // prevent opening new tab
          return [EditorActions.toggleInterfaceDesignerLayoutReversed()]
        }
        break
      }
    }
    return []
  }

  function getShortcutActions(): Array<EditorAction> {
    const openFile = getOpenFile(editor)
    if (openFile == null) {
      return []
    } else {
      switch (openFile.type) {
        case 'UI_JS_FILE':
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
      true,
      Modifier.modifiersForKeyboardEvent(event),
    )
  }
  const updateKeysAction = EditorActions.updateKeys(updatedKeysPressed)

  // Modifiers and the like used throughout various conditions.
  const modifiers = modifiersForEvent(event)
  const noModifier = modifiers.length === 0

  function getUIFileActions(): Array<EditorAction> {
    switch (key) {
      case 'z':
        if (noModifier) {
          return [EditorActions.setHighlightsEnabled(true)]
        }
        break
    }
    return []
  }

  function getShortcutActions(): Array<EditorAction> {
    const openFile = getOpenFile(editor)
    if (openFile == null) {
      return []
    } else {
      switch (openFile.type) {
        case 'UI_JS_FILE':
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
