import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { generateUidWithExistingComponents } from '../../core/model/element-template-utils'
import type { ElementPath } from '../../core/shared/project-file-types'
import { importAlias, importDetails } from '../../core/shared/project-file-types'
import * as PP from '../../core/shared/property-path'
import type { KeyCharacter, KeysPressed } from '../../utils/keyboard'
import Keyboard, {
  modifiersForEvent,
  StoredKeyCharacters,
  strictCheckModifiers,
} from '../../utils/keyboard'
import type { Modifiers } from '../../utils/modifiers'
import { Modifier } from '../../utils/modifiers'
import Utils from '../../utils/utils'
import Canvas from '../canvas/canvas'
import CanvasActions from '../canvas/canvas-actions'
import { getAllTargetsAtPoint } from '../canvas/dom-lookup'
import {
  toggleBackgroundLayers,
  toggleBorder,
  toggleShadow,
  toggleStylePropPath,
  toggleStylePropPaths,
} from '../inspector/common/css-utils'
import type { EditorAction, EditorDispatch, LoginState, SwitchEditorMode } from './action-types'
import * as EditorActions from './actions/action-creators'
import * as MetaActions from './actions/meta-actions'
import {
  defaultDivElement,
  defaultEllipseElement,
  defaultRectangleElement,
  defaultSpanElement,
} from './defaults'
import {
  EditorModes,
  isCommentMode,
  isFollowMode,
  isInsertMode,
  isLiveMode,
  isSelectMode,
  isTextEditMode,
} from './editor-modes'
import { insertImage } from './image-insert'
import type { ShortcutNamesByKey } from './shortcut-definitions'
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
  TOGGLE_INSPECTOR_AND_NAVIGATOR_SHORTCUT,
  TOGGLE_NAVIGATOR,
  TOGGLE_LIVE_CANVAS_SHORTCUT,
  TOGGLE_PREVIEW_SHORTCUT,
  TOGGLE_INSPECTOR,
  TOGGLE_SHADOW_SHORTCUT,
  UNDO_CHANGES_SHORTCUT,
  UNWRAP_ELEMENT_SHORTCUT,
  WRAP_ELEMENT_PICKER_SHORTCUT,
  ZOOM_CANVAS_IN_SHORTCUT,
  ZOOM_CANVAS_OUT_SHORTCUT,
  ZOOM_UI_IN_SHORTCUT,
  ZOOM_UI_OUT_SHORTCUT,
  ADD_ELEMENT_SHORTCUT,
  GROUP_ELEMENT_DEFAULT_SHORTCUT,
  TOGGLE_FOCUSED_OMNIBOX_TAB,
  FOCUS_CLASS_NAME_INPUT,
  INSERT_DIV_SHORTCUT,
  OPEN_EYEDROPPER,
  TEXT_EDIT_MODE,
  TOGGLE_TEXT_BOLD,
  TOGGLE_TEXT_ITALIC,
  TOGGLE_TEXT_UNDERLINE,
  TOGGLE_TEXT_STRIKE_THROUGH,
  PASTE_STYLE_PROPERTIES,
  COPY_STYLE_PROPERTIES,
  CONVERT_TO_FLEX_CONTAINER,
  REMOVE_ABSOLUTE_POSITIONING,
  RESIZE_TO_FIT,
  JUMP_TO_PARENT_SHORTCUT_BACKSLASH,
  OPEN_INSERT_MENU,
  PASTE_TO_REPLACE,
  WRAP_IN_DIV,
  COMMENT_SHORTCUT,
} from './shortcut-definitions'
import type { EditorState, LockedElements, NavigatorEntry, UserState } from './store/editor-state'
import { getOpenFile, RightMenuTab } from './store/editor-state'
import { CanvasMousePositionRaw, WindowMousePositionRaw } from '../../utils/global-positions'
import { pickColorWithEyeDropper } from '../canvas/canvas-utils'
import {
  boundingArea,
  createHoverInteractionViaMouse,
} from '../canvas/canvas-strategies/interaction-state'
import type { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import {
  emptyComments,
  jsExpressionValue,
  isJSXElementLike,
} from '../../core/shared/element-template'
import {
  toggleTextBold,
  toggleTextItalic,
  toggleTextStrikeThrough,
  toggleTextUnderline,
} from '../text-editor/text-editor-shortcut-helpers'
import { commandsForFirstApplicableStrategy } from '../inspector/inspector-strategies/inspector-strategy'
import {
  addFlexLayoutStrategies,
  removeFlexLayoutStrategies,
} from '../inspector/inspector-strategies/inspector-strategies'
import {
  detectAreElementsFlexContainers,
  toggleResizeToFitSetToFixed,
  toggleAbsolutePositioningCommands,
} from '../inspector/inspector-common'
import { zeroCanvasPoint } from '../../core/shared/math-utils'
import * as EP from '../../core/shared/element-path'
import { createWrapInGroupActions } from '../canvas/canvas-strategies/strategies/group-conversion-helpers'
import { isRight } from '../../core/shared/either'
import type { ElementPathTrees } from '../../core/shared/element-path-tree'
import { createPasteToReplacePostActionActions } from '../canvas/canvas-strategies/post-action-options/post-action-options'
import { wrapInDivStrategy } from './wrap-in-callbacks'
import { type ProjectServerState } from './store/project-server-state'
import { allowedToEditProject } from './store/collaborative-editing'
import { hasCommentPermission } from './store/permissions'
import { type ShowComponentPickerContextMenuCallback } from '../navigator/navigator-item/component-picker-context-menu'
import { showReplaceComponentPicker } from '../context-menu-items'

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

function jumpToParentActions(
  selectedViews: Array<ElementPath>,
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  lockedElements: LockedElements,
): Array<EditorAction> {
  const jumpResult = Canvas.jumpToParent(selectedViews, metadata, pathTrees, lockedElements)
  switch (jumpResult) {
    case null:
      return []
    case 'CLEAR':
      return [EditorActions.clearSelection()]
    default:
      return MetaActions.selectComponents([jumpResult], false)
  }
}

function activeElementIsAnInput(): boolean {
  const activeElement = document.activeElement
  if (activeElement != null) {
    const activeElementTag = activeElement.tagName.toLowerCase()
    return activeElementTag === 'input' || activeElementTag === 'textarea'
  }

  return false
}

const activeElementIsNotAnInput = () => !activeElementIsAnInput()

export function preventBrowserShortcuts(editor: EditorState, event: KeyboardEvent): void {
  const key = Keyboard.keyCharacterForCode(event.keyCode)
  const modifiers = modifiersForEvent(event)
  const cmd = strictCheckModifiers(modifiers, ['cmd'])
  const altCmd = strictCheckModifiers(modifiers, ['alt', 'cmd'])
  const shiftCmd = strictCheckModifiers(modifiers, ['shift', 'cmd'])

  switch (key) {
    case 'tab':
      if (activeElementIsNotAnInput()) {
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
        if (activeElementIsNotAnInput()) {
          event.preventDefault()
        }
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
  loginState: LoginState,
  projectServerState: ProjectServerState,
  metadataRef: { current: ElementInstanceMetadataMap },
  navigatorTargetsRef: { current: Array<NavigatorEntry> },
  namesByKey: ShortcutNamesByKey,
  dispatch: EditorDispatch,
  showComponentPicker: ShowComponentPickerContextMenuCallback,
): Array<EditorAction> {
  // Stop the browser from firing things like save dialogs.
  preventBrowserShortcuts(editor, event)

  const allowedToEdit = allowedToEditProject(loginState, projectServerState)
  const canComment = hasCommentPermission(loginState)

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
      const tabbedTo = Canvas.jumpToSibling(
        editor.selectedViews,
        editor.jsxMetadata,
        editor.elementPathTree,
        forwards,
      )
      if (tabbedTo != null) {
        return MetaActions.selectComponents([tabbedTo], false)
      }
    }
    return []
  }

  function getUIFileActions(): Array<EditorAction> {
    if (key === 'tab' && activeElementIsAnInput()) {
      return []
    }
    return handleShortcuts<Array<EditorAction>>(namesByKey, event, [], {
      [DELETE_SELECTED_SHORTCUT]: () => {
        return isSelectMode(editor.mode) ? [EditorActions.deleteSelected()] : []
      },
      [RESET_CANVAS_ZOOM_SHORTCUT]: () => {
        return !isFollowMode(editor.mode) ? [CanvasActions.zoom(1)] : []
      },
      [ZOOM_UI_IN_SHORTCUT]: () => {
        return [CanvasActions.zoomUI(true)]
      },
      [ZOOM_CANVAS_IN_SHORTCUT]: () => {
        return !isFollowMode(editor.mode)
          ? [CanvasActions.zoom(Utils.increaseScale(editor.canvas.scale))]
          : []
      },
      [ZOOM_UI_OUT_SHORTCUT]: () => {
        return [CanvasActions.zoomUI(false)]
      },
      [ZOOM_CANVAS_OUT_SHORTCUT]: () => {
        return !isFollowMode(editor.mode)
          ? [CanvasActions.zoom(Utils.decreaseScale(editor.canvas.scale))]
          : []
      },
      [FIRST_CHILD_OR_EDIT_TEXT_SHORTCUT]: () => {
        if (isSelectMode(editor.mode)) {
          const firstTextEditableView = editor.selectedViews.find((v) =>
            MetadataUtils.targetTextEditable(editor.jsxMetadata, editor.elementPathTree, v),
          )
          if (firstTextEditableView != null) {
            return [
              EditorActions.switchEditorMode(
                EditorModes.textEditMode(
                  firstTextEditableView,
                  null,
                  'existing',
                  'select-all-on-focus',
                ),
              ),
            ]
          }

          const childToSelect = Canvas.getFirstChild(
            editor.selectedViews,
            editor.jsxMetadata,
            editor.elementPathTree,
          )
          if (childToSelect != null) {
            return MetaActions.selectComponents([childToSelect], false)
          }
        }
        return []
      },
      [COMMENT_SHORTCUT]: () => {
        if (canComment) {
          return [
            EditorActions.switchEditorMode(EditorModes.commentMode(null, 'not-dragging')),
            EditorActions.setRightMenuTab(RightMenuTab.Comments),
          ]
        }
        return []
      },
      [JUMP_TO_PARENT_SHORTCUT]: () => {
        if (isSelectMode(editor.mode)) {
          return jumpToParentActions(
            editor.selectedViews,
            editor.jsxMetadata,
            editor.elementPathTree,
            editor.lockedElements,
          )
        } else {
          return []
        }
      },
      [JUMP_TO_PARENT_SHORTCUT_BACKSLASH]: () => {
        if (isSelectMode(editor.mode)) {
          return jumpToParentActions(
            editor.selectedViews,
            editor.jsxMetadata,
            editor.elementPathTree,
            editor.lockedElements,
          )
        } else {
          return []
        }
      },
      [CANCEL_EVERYTHING_SHORTCUT]: () => {
        if (isInsertMode(editor.mode) || editor.rightMenu.selectedTab === RightMenuTab.Insert) {
          return MetaActions.cancelInsertModeActions('do-not-apply-changes')
        } else if (editor.canvas.interactionSession != null) {
          return [CanvasActions.clearInteractionSession(false)]
        } else if (isSelectMode(editor.mode)) {
          const focusedElementSelected =
            editor.selectedViews.length === 1 &&
            EP.pathsEqual(editor.selectedViews[0], editor.focusedElementPath)
          if (editor.selectedViews.length === 0 || focusedElementSelected) {
            return [EditorActions.setFocusedElement(null)]
          } else {
            return jumpToParentActions(
              editor.selectedViews,
              editor.jsxMetadata,
              editor.elementPathTree,
              editor.lockedElements,
            )
          }
        }

        // TODO: Move this around.
        if (isLiveMode(editor.mode)) {
          return [
            EditorActions.updateEditorMode(
              EditorModes.selectMode(editor.mode.controlId, false, 'none'),
            ),
          ]
        }
        if (isTextEditMode(editor.mode)) {
          return [EditorActions.updateEditorMode(EditorModes.selectMode(null, false, 'none'))]
        }
        if (isCommentMode(editor.mode)) {
          return [EditorActions.updateEditorMode(EditorModes.selectMode(null, false, 'none'))]
        }
        return []
      },
      [CYCLE_HIERACHY_TARGETS_SHORTCUT]: () => {
        if (isSelectMode(editor.mode)) {
          if (CanvasMousePositionRaw == null) {
            return [EditorActions.clearSelection()]
          }
          const targetStack = getAllTargetsAtPoint(
            'no-filter',
            WindowMousePositionRaw,
            editor.canvas.scale,
            editor.canvas.realCanvasOffset,
            editor.jsxMetadata,
          )
          const nextTarget = Canvas.getNextTarget(
            editor.jsxMetadata,
            editor.elementPathTree,
            editor.lockedElements,
            editor.selectedViews,
            targetStack,
          )
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
                toggleStylePropPath(PP.create('style', 'border'), toggleBorder),
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
        return isSelectMode(editor.mode) ? [EditorActions.unwrapElements(editor.selectedViews)] : []
      },
      [WRAP_ELEMENT_PICKER_SHORTCUT]: () => {
        if (allowedToEdit) {
          if (isSelectMode(editor.mode)) {
            if (editor.selectedViews.length === 1) {
              const mousePoint = WindowMousePositionRaw ?? zeroCanvasPoint
              showComponentPicker(editor.selectedViews, EditorActions.wrapTarget)(event, {
                position: mousePoint,
              })
              return []
            }
          }
        }
        return []
      },
      [GROUP_ELEMENT_DEFAULT_SHORTCUT]: () => {
        return isSelectMode(editor.mode) && editor.selectedViews.length > 0
          ? [
              createWrapInGroupActions(
                editor.selectedViews,
                editor.projectContents,
                editor.jsxMetadata,
                editor.allElementProps,
                editor.elementPathTree,
                navigatorTargetsRef.current,
              ),
            ]
          : []
      },
      [TOGGLE_HIDDEN_SHORTCUT]: () => {
        return [EditorActions.toggleHidden()]
      },
      [INSERT_IMAGE_SHORTCUT]: () => {
        if (allowedToEdit) {
          if (isSelectMode(editor.mode) || isInsertMode(editor.mode)) {
            // Side effects.
            insertImage(dispatch)
          }
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
          EditorActions.switchEditorMode(EditorModes.selectMode(null, false, 'none')),
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
            toggleStylePropPath(PP.create('style', 'boxShadow'), toggleShadow),
          ),
        )
      },
      [INSERT_DIV_SHORTCUT]: () => {
        if (!allowedToEdit) {
          return []
        }
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
        return isSelectMode(editor.mode) ? [EditorActions.cutSelectionToClipboard()] : []
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
      [TOGGLE_NAVIGATOR]: () => {
        return [EditorActions.togglePanel('leftmenu')]
      },
      [TOGGLE_INSPECTOR]: () => {
        return [EditorActions.togglePanel('rightmenu')]
      },
      [TOGGLE_DESIGNER_ADDITIONAL_CONTROLS_SHORTCUT]: () => {
        return [EditorActions.toggleInterfaceDesignerAdditionalControls()]
      },
      [TOGGLE_CODE_EDITOR_SHORTCUT]: () => {
        return [EditorActions.togglePanel('codeEditor')]
      },
      [TOGGLE_INSPECTOR_AND_NAVIGATOR_SHORTCUT]: () => {
        return [EditorActions.togglePanel('rightmenu'), EditorActions.togglePanel('leftmenu')]
      },
      [ADD_ELEMENT_SHORTCUT]: () => {
        if (allowedToEdit) {
          if (isSelectMode(editor.mode)) {
            const mousePoint = WindowMousePositionRaw ?? zeroCanvasPoint
            showComponentPicker(editor.selectedViews, EditorActions.insertAsChildTarget())(event, {
              position: mousePoint,
            })
            return []
          }
        }
        return []
      },
      [OPEN_EYEDROPPER]: () => {
        const selectedViews = editor.selectedViews
        if (selectedViews.length === 0) {
          return []
        }
        void pickColorWithEyeDropper()
          .then(({ sRGBHex }) =>
            dispatch(
              selectedViews.map((view) =>
                EditorActions.setProp_UNSAFE(
                  view,
                  PP.create('style', 'backgroundColor'),
                  jsExpressionValue(sRGBHex, emptyComments),
                ),
              ),
            ),
          )
          .catch((e) => console.error(e))
        return []
      },
      [TEXT_EDIT_MODE]: () => {
        if (allowedToEdit) {
          const newUID = generateUidWithExistingComponents(editor.projectContents)

          actions.push(
            EditorActions.enableInsertModeForJSXElement(
              defaultSpanElement(newUID),
              newUID,
              {},
              null,
              {
                textEdit: true,
              },
            ),
            CanvasActions.createInteractionSession(
              createHoverInteractionViaMouse(
                CanvasMousePositionRaw!,
                modifiers,
                boundingArea(),
                'zero-drag-permitted',
              ),
            ),
          )
        }
        return actions
      },
      [TOGGLE_TEXT_BOLD]: () => {
        if (isSelectMode(editor.mode)) {
          editor.selectedViews.forEach((target, i) => {
            const element = MetadataUtils.findElementByElementPath(editor.jsxMetadata, target)
            toggleTextBold(
              target,
              element?.specialSizeMeasurements.fontWeight ?? null,
              dispatch,
              metadataRef,
              i === 0 ? 'separate-undo-step' : 'merge-with-previous',
            )
          })
        }
        return []
      },
      [TOGGLE_TEXT_ITALIC]: () => {
        if (isSelectMode(editor.mode)) {
          editor.selectedViews.forEach((target, i) => {
            const element = MetadataUtils.findElementByElementPath(editor.jsxMetadata, target)
            toggleTextItalic(
              target,
              element?.specialSizeMeasurements.fontStyle ?? null,
              dispatch,
              metadataRef,
              i === 0 ? 'separate-undo-step' : 'merge-with-previous',
            )
          })
        }
        return []
      },
      [TOGGLE_TEXT_UNDERLINE]: () => {
        if (isSelectMode(editor.mode)) {
          editor.selectedViews.forEach((target, i) => {
            const element = MetadataUtils.findElementByElementPath(editor.jsxMetadata, target)
            toggleTextUnderline(
              target,
              element?.specialSizeMeasurements.textDecorationLine ?? null,
              dispatch,
              metadataRef,
              i === 0 ? 'separate-undo-step' : 'merge-with-previous',
            )
          })
        }
        return []
      },
      [TOGGLE_TEXT_STRIKE_THROUGH]: () => {
        if (isSelectMode(editor.mode)) {
          editor.selectedViews.forEach((target, i) => {
            const element = MetadataUtils.findElementByElementPath(editor.jsxMetadata, target)
            toggleTextStrikeThrough(
              target,
              element?.specialSizeMeasurements.textDecorationLine ?? null,
              dispatch,
              metadataRef,
              i === 0 ? 'separate-undo-step' : 'merge-with-previous',
            )
          })
        }
        return []
      },
      [PASTE_TO_REPLACE]: () => {
        if (isSelectMode(editor.mode)) {
          const actions = createPasteToReplacePostActionActions(
            editor.selectedViews,
            editor.internalClipboard,
          )
          if (actions != null) {
            return actions
          }
        }
        return []
      },
      [PASTE_STYLE_PROPERTIES]: () => {
        return isSelectMode(editor.mode)
          ? editor.selectedViews.map((target) => {
              return EditorActions.pasteProperties('style')
            })
          : []
      },
      [COPY_STYLE_PROPERTIES]: () => {
        return isSelectMode(editor.mode)
          ? editor.selectedViews.map((target) => {
              return EditorActions.copyProperties()
            })
          : []
      },
      [CONVERT_TO_FLEX_CONTAINER]: () => {
        if (!isSelectMode(editor.mode)) {
          return []
        }
        const elementsConsideredForFlexConversion = editor.selectedViews.filter(
          (elementPath) =>
            MetadataUtils.getJSXElementFromMetadata(editor.jsxMetadata, elementPath) != null,
        )
        const selectedElementsFlexContainers = detectAreElementsFlexContainers(
          editor.jsxMetadata,
          elementsConsideredForFlexConversion,
        )
        const commands = commandsForFirstApplicableStrategy(
          selectedElementsFlexContainers
            ? removeFlexLayoutStrategies(
                editor.jsxMetadata,
                elementsConsideredForFlexConversion,
                editor.elementPathTree,
              )
            : addFlexLayoutStrategies(
                editor.jsxMetadata,
                elementsConsideredForFlexConversion,
                editor.elementPathTree,
                editor.allElementProps,
              ),
        )
        if (commands == null) {
          return []
        }
        return [EditorActions.applyCommandsAction(commands)]
      },
      [REMOVE_ABSOLUTE_POSITIONING]: () => {
        if (!isSelectMode(editor.mode)) {
          return []
        }

        const commands = toggleAbsolutePositioningCommands(
          editor.jsxMetadata,
          editor.allElementProps,
          editor.elementPathTree,
          editor.selectedViews,
        )

        if (commands.length === 0) {
          return []
        }

        return [EditorActions.applyCommandsAction(commands)]
      },
      [RESIZE_TO_FIT]: () => {
        if (!isSelectMode(editor.mode)) {
          return []
        }
        const commands = toggleResizeToFitSetToFixed(
          editor.jsxMetadata,
          editor.selectedViews,
          editor.elementPathTree,
          editor.allElementProps,
        )
        if (commands.length === 0) {
          return []
        }
        return [EditorActions.applyCommandsAction(commands)]
      },
      [OPEN_INSERT_MENU]: () => {
        if (allowedToEdit) {
          return [
            EditorActions.setPanelVisibility('rightmenu', true),
            EditorActions.setRightMenuTab(RightMenuTab.Insert),
          ]
        } else {
          return []
        }
      },
      [WRAP_IN_DIV]: () => {
        if (!isSelectMode(editor.mode)) {
          return []
        }
        const commands = commandsForFirstApplicableStrategy([
          wrapInDivStrategy(
            editor.jsxMetadata,
            editor.selectedViews,
            editor.elementPathTree,
            editor.allElementProps,
            editor.projectContents,
            editor.propertyControlsInfo,
          ),
        ])
        if (commands == null) {
          return []
        }
        return [EditorActions.applyCommandsAction(commands)]
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

  return actions
}

export function handleKeyUp(
  event: KeyboardEvent,
  editor: EditorState,
  namesByKey: ShortcutNamesByKey,
): Array<EditorAction> {
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
  return actions
}

function addCreateHoverInteractionActionToSwitchModeAction(
  switchModeAction: SwitchEditorMode,
  modifiers: Modifiers,
) {
  const mousePoint = CanvasMousePositionRaw ?? zeroCanvasPoint
  return [
    switchModeAction,
    CanvasActions.createInteractionSession(
      createHoverInteractionViaMouse(mousePoint, modifiers, boundingArea(), 'zero-drag-permitted'),
    ),
  ]
}
