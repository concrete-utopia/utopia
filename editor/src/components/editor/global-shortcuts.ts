import { findElementAtPath, MetadataUtils } from '../../core/model/element-metadata-utils'
import { generateUidWithExistingComponents } from '../../core/model/element-template-utils'
import { importAlias, importDetails, ElementPath } from '../../core/shared/project-file-types'
import * as PP from '../../core/shared/property-path'
import * as EP from '../../core/shared/element-path'
import Keyboard, {
  KeyCharacter,
  KeysPressed,
  modifiersForEvent,
  StoredKeyCharacters,
  strictCheckModifiers,
} from '../../utils/keyboard'
import { emptyModifiers, Modifier, Modifiers } from '../../utils/modifiers'
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
import { EditorAction, EditorDispatch, SwitchEditorMode } from './action-types'
import * as EditorActions from './actions/action-creators'
import * as MetaActions from './actions/meta-actions'
import {
  defaultDivElement,
  defaultEllipseElement,
  defaultRectangleElement,
  defaultViewElement,
} from './defaults'
import { EditorModes, isInsertMode, isLiveMode, isSelectMode } from './editor-modes'
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
  INSERT_ELLIPSE_SHORTCUT,
  INSERT_IMAGE_SHORTCUT,
  INSERT_RECTANGLE_SHORTCUT,
  INSERT_VIEW_SHORTCUT,
  JUMP_TO_PARENT_SHORTCUT,
  MOVE_ELEMENT_BACKWARD_SHORTCUT,
  MOVE_ELEMENT_FORWARD_SHORTCUT,
  MOVE_ELEMENT_TO_BACK_SHORTCUT,
  MOVE_ELEMENT_TO_FRONT_SHORTCUT,
  REDO_CHANGES_SHORTCUT,
  RESET_CANVAS_ZOOM_SHORTCUT,
  SAVE_CURRENT_FILE_SHORTCUT,
  SELECT_ALL_SIBLINGS_SHORTCUT,
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
  UNDO_CHANGES_SHORTCUT,
  UNWRAP_ELEMENT_SHORTCUT,
  WRAP_ELEMENT_DEFAULT_SHORTCUT,
  WRAP_ELEMENT_PICKER_SHORTCUT,
  ZOOM_CANVAS_IN_SHORTCUT,
  ZOOM_CANVAS_OUT_SHORTCUT,
  ZOOM_UI_IN_SHORTCUT,
  ZOOM_UI_OUT_SHORTCUT,
  ShortcutNamesByKey,
  CONVERT_ELEMENT_SHORTCUT,
  ADD_ELEMENT_SHORTCUT,
  GROUP_ELEMENT_PICKER_SHORTCUT,
  GROUP_ELEMENT_DEFAULT_SHORTCUT,
  TOGGLE_FOCUSED_OMNIBOX_TAB,
  FOCUS_CLASS_NAME_INPUT,
  INSERT_DIV_SHORTCUT,
  OPEN_EYEDROPPPER,
} from './shortcut-definitions'
import { DerivedState, EditorState, getOpenFile, RightMenuTab } from './store/editor-state'
import { CanvasMousePositionRaw, WindowMousePositionRaw } from '../../utils/global-positions'
import { getDragStateStart } from '../canvas/canvas-utils'
import { isFeatureEnabled } from '../../utils/feature-switches'
import {
  boundingArea,
  createHoverInteractionViaMouse,
} from '../canvas/canvas-strategies/interaction-state'
import { emptyComments, jsxAttributeValue } from '../../core/shared/element-template'

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

export function editorIsTarget(event: KeyboardEvent, editor: EditorState): boolean {
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
      return MetaActions.selectComponents([jumpResult], false)
  }
}

function getTextEditorTarget(editor: EditorState, derived: DerivedState): ElementPath | null {
  if (editor.canvas.dragState != null || editor.selectedViews.length !== 1) {
    return null
  } else {
    const target = editor.selectedViews[0]
    const element = MetadataUtils.findElementByElementPath(editor.jsxMetadata, target)
    if (element != null && MetadataUtils.getTextContentOfElement(element) != null) {
      return target
    } else {
      return null
    }
  }
}

function shouldTabBeHandledByBrowser(editor: EditorState): boolean {
  return (
    editor.focusedPanel === 'inspector' ||
    editor.focusedPanel === 'dependencylist' ||
    editor.floatingInsertMenu.insertMenuMode !== 'closed'
  )
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

  const modifiers = Modifier.modifiersForKeyboardEvent(event)

  let updatedKeysPressed: KeysPressed
  if (editorTargeted) {
    updatedKeysPressed = updateKeysPressed(editor.keysPressed, key, true, modifiers)
  } else {
    updatedKeysPressed = updateModifiers(editor.keysPressed, modifiers)
  }
  const updateKeysAction = EditorActions.updateKeys(updatedKeysPressed)

  function cycleSiblings(forwards: boolean): Array<EditorAction> {
    if (isSelectMode(editor.mode)) {
      const tabbedTo = Canvas.jumpToSibling(editor.selectedViews, editor.jsxMetadata, forwards)
      if (tabbedTo != null) {
        return MetaActions.selectComponents([tabbedTo], false)
      }
    }
    return []
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
        if (isSelectMode(editor.mode)) {
          const textTarget = getTextEditorTarget(editor, derived)
          if (textTarget != null && isSelectMode(editor.mode)) {
            return [EditorActions.focusFormulaBar()]
          } else {
            const childToSelect = Canvas.getFirstChild(editor.selectedViews, editor.jsxMetadata)
            if (childToSelect != null) {
              return MetaActions.selectComponents([childToSelect], false)
            }
          }
        }
        return []
      },
      [JUMP_TO_PARENT_SHORTCUT]: () => {
        if (isSelectMode(editor.mode)) {
          return jumpToParentActions(editor.selectedViews)
        } else {
          return []
        }
      },
      [CANCEL_EVERYTHING_SHORTCUT]: () => {
        if (isInsertMode(editor.mode) || editor.rightMenu.selectedTab === RightMenuTab.Insert) {
          return MetaActions.cancelInsertModeActions('do-not-apply-changes')
        } else if (
          editor.canvas.dragState != null &&
          getDragStateStart(editor.canvas.dragState, editor.canvas.resizeOptions) != null
        ) {
          return [CanvasActions.clearDragState(false)]
        } else if (editor.canvas.interactionSession != null) {
          return [CanvasActions.clearInteractionSession(false)]
        } else if (isSelectMode(editor.mode)) {
          return jumpToParentActions(editor.selectedViews)
        }

        // TODO: Move this around.
        if (isLiveMode(editor.mode)) {
          return [EditorActions.updateEditorMode(EditorModes.selectMode(editor.mode.controlId))]
        }
        return []
      },
      [CYCLE_HIERACHY_TARGETS_SHORTCUT]: () => {
        if (isSelectMode(editor.mode)) {
          if (CanvasMousePositionRaw == null) {
            return [EditorActions.clearSelection()]
          }
          const targetStack = getAllTargetsAtPoint('no-filter', WindowMousePositionRaw)
          const nextTarget = Canvas.getNextTarget(editor.selectedViews, targetStack)
          if (targetStack.length === 0 || nextTarget === null) {
            return [EditorActions.clearSelection()]
          } else {
            return MetaActions.selectComponents([nextTarget], false)
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
      [SELECT_ALL_SIBLINGS_SHORTCUT]: () => {
        return [EditorActions.selectAllSiblings()]
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
      [WRAP_ELEMENT_DEFAULT_SHORTCUT]: () => {
        return isSelectMode(editor.mode)
          ? [EditorActions.wrapInView(editor.selectedViews, 'default-empty-div')]
          : []
      },
      [WRAP_ELEMENT_PICKER_SHORTCUT]: () => {
        return isSelectMode(editor.mode)
          ? [EditorActions.openFloatingInsertMenu({ insertMenuMode: 'wrap' })]
          : []
      },
      // For now, the "Group / G" shortcuts do the same as the Wrap Element shortcuts – until we have Grouping working again
      [GROUP_ELEMENT_DEFAULT_SHORTCUT]: () => {
        return isSelectMode(editor.mode)
          ? [EditorActions.wrapInView(editor.selectedViews, 'default-empty-div')]
          : []
      },
      [GROUP_ELEMENT_PICKER_SHORTCUT]: () => {
        return isSelectMode(editor.mode)
          ? [EditorActions.openFloatingInsertMenu({ insertMenuMode: 'wrap' })]
          : []
      },
      [TOGGLE_HIDDEN_SHORTCUT]: () => {
        return [EditorActions.toggleHidden()]
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
          CanvasActions.clearInteractionSession(false),
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
          return addCreateHoverInteractionActionToSwitchModeAction(
            EditorActions.enableInsertModeForJSXElement(
              defaultRectangleElement(newUID),
              newUID,
              {
                'utopia-api': importDetails(null, [importAlias('Rectangle')], null),
              },
              null,
            ),
            modifiers,
          )
        } else {
          return []
        }
      },
      [INSERT_ELLIPSE_SHORTCUT]: () => {
        if (isSelectMode(editor.mode) || isInsertMode(editor.mode)) {
          const newUID = generateUidWithExistingComponents(editor.projectContents)
          return addCreateHoverInteractionActionToSwitchModeAction(
            EditorActions.enableInsertModeForJSXElement(
              defaultEllipseElement(newUID),
              newUID,
              { 'utopia-api': importDetails(null, [importAlias('Ellipse')], null) },
              null,
            ),
            modifiers,
          )
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
      [INSERT_VIEW_SHORTCUT]: () => {
        if (isSelectMode(editor.mode) || isInsertMode(editor.mode)) {
          const newUID = generateUidWithExistingComponents(editor.projectContents)
          return addCreateHoverInteractionActionToSwitchModeAction(
            EditorActions.enableInsertModeForJSXElement(
              defaultViewElement(newUID),
              newUID,
              { 'utopia-api': importDetails(null, [importAlias('View')], null) },
              null,
            ),
            modifiers,
          )
        } else {
          return []
        }
      },
      [INSERT_DIV_SHORTCUT]: () => {
        if (!isSelectMode(editor.mode) && !isInsertMode(editor.mode)) {
          return []
        }

        const newUID = generateUidWithExistingComponents(editor.projectContents)
        return addCreateHoverInteractionActionToSwitchModeAction(
          EditorActions.enableInsertModeForJSXElement(defaultDivElement(newUID), newUID, {}, null),
          modifiers,
        )
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
      [FOCUS_CLASS_NAME_INPUT]: () => {
        return [EditorActions.focusClassNameInput()]
      },
      [TOGGLE_FOCUSED_OMNIBOX_TAB]: () => {
        return [EditorActions.focusFormulaBar()]
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
      [CONVERT_ELEMENT_SHORTCUT]: () => {
        if (isSelectMode(editor.mode)) {
          return [EditorActions.openFloatingInsertMenu({ insertMenuMode: 'convert' })]
        } else {
          return []
        }
      },
      [ADD_ELEMENT_SHORTCUT]: () => {
        if (isSelectMode(editor.mode)) {
          return [
            EditorActions.openFloatingInsertMenu({
              insertMenuMode: 'insert',
              parentPath: null,
              indexPosition: null,
            }),
          ]
        } else {
          return []
        }
      },
      [OPEN_EYEDROPPPER]: () => {
        const selectedElement = editor.selectedViews.at(0)
        if (selectedElement == null) {
          return []
        }
        if ((window as any).EyeDropper == null) {
          return []
        }

        ;(new (window as any).EyeDropper() as any).open().then((result: any) => {
          dispatch([
            EditorActions.setProperty(
              selectedElement,
              PP.create(['style', 'backgroundColor']),
              jsxAttributeValue(result.sRGBHex as string, emptyComments),
            ),
          ])
        })
        return []
      },
    })
  }

  function getCanvasShortcutActions(): Array<EditorAction> {
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

  function getGlobalShortcutActions(): Array<EditorAction> {
    const openFile = getOpenFile(editor)
    if (openFile != null && openFile.type === 'TEXT_FILE') {
      return handleShortcuts<Array<EditorAction>>(namesByKey, event, [], {
        [UNDO_CHANGES_SHORTCUT]: () => {
          return [EditorActions.undo()]
        },
        [REDO_CHANGES_SHORTCUT]: () => {
          return [EditorActions.redo()]
        },
      })
    } else {
      return []
    }
  }

  // Build the actions to dispatch.
  let actions: Array<EditorAction> = [updateKeysAction]
  let shortCutActions: EditorAction[]
  if (editorTargeted) {
    shortCutActions = getCanvasShortcutActions()
  } else {
    shortCutActions = getGlobalShortcutActions()
  }
  if (shortCutActions.length > 0) {
    if (editor.canvas.interactionSession?.interactionData.type === 'KEYBOARD') {
      // if we are in a keyboard interaction session, we want keyboard shortcuts to finish the current interaction session,
      // so the effect of the shortcut is not combined into the undo of the interaction
      dispatch([CanvasActions.clearInteractionSession(true)])
    }
    actions.push(...shortCutActions)
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
  const updatedKeysPressed = updateKeysPressed(
    editor.keysPressed,
    key,
    false,
    Modifier.modifiersForKeyboardEvent(event),
  )

  const updateKeysAction = EditorActions.updateKeys(updatedKeysPressed)

  function getUIFileActions(): Array<EditorAction> {
    return handleShortcuts<Array<EditorAction>>(namesByKey, event, [], {
      // no shortcuts at the moment
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

function addCreateHoverInteractionActionToSwitchModeAction(
  switchModeAction: SwitchEditorMode,
  modifiers: Modifiers,
) {
  return CanvasMousePositionRaw != null
    ? [
        switchModeAction,
        CanvasActions.createInteractionSession(
          createHoverInteractionViaMouse(CanvasMousePositionRaw, modifiers, boundingArea()),
        ),
      ]
    : [switchModeAction]
}
