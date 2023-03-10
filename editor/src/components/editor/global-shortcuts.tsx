import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { generateUidWithExistingComponents } from '../../core/model/element-template-utils'
import {
  importAlias,
  importDetails,
  ElementPath,
  Imports,
} from '../../core/shared/project-file-types'
import * as PP from '../../core/shared/property-path'
import Keyboard, {
  KeyCharacter,
  KeysPressed,
  modifiersForEvent,
  StoredKeyCharacters,
  strictCheckModifiers,
} from '../../utils/keyboard'
import { Modifier, Modifiers } from '../../utils/modifiers'
import Utils, { getChainSegmentEdge } from '../../utils/utils'
import Canvas from '../canvas/canvas'
import CanvasActions from '../canvas/canvas-actions'
import { getAllTargetsAtPoint } from '../canvas/dom-lookup'
import {
  cssPixelLength,
  toggleBackgroundLayers,
  toggleBorder,
  toggleShadow,
  toggleStylePropPath,
  toggleStylePropPaths,
} from '../inspector/common/css-utils'
import { EditorAction, EditorDispatch, SwitchEditorMode, WrapInView } from './action-types'
import * as EditorActions from './actions/action-creators'
import * as MetaActions from './actions/meta-actions'
import {
  defaultDivElement,
  defaultEllipseElement,
  defaultRectangleElement,
  defaultSpanElement,
  defaultTransparentViewElement,
  defaultUnstyledDivElement,
  defaultViewElement,
} from './defaults'
import { EditorModes, isInsertMode, isLiveMode, isSelectMode, isTextEditMode } from './editor-modes'
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
  TOGGLE_INSPECTOR_AND_NAVIGATOR_SHORTCUT,
  TOGGLE_NAVIGATOR,
  TOGGLE_LIVE_CANVAS_SHORTCUT,
  TOGGLE_PREVIEW_SHORTCUT,
  TOGGLE_INSPECTOR,
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
  OPEN_EYEDROPPPER as OPEN_EYEDROPPER,
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
} from './shortcut-definitions'
import { DerivedState, EditorState, getOpenFile, RightMenuTab } from './store/editor-state'
import { CanvasMousePositionRaw, WindowMousePositionRaw } from '../../utils/global-positions'
import { getDragStateStart, pickColorWithEyeDropper } from '../canvas/canvas-utils'
import {
  boundingArea,
  createHoverInteractionViaMouse,
} from '../canvas/canvas-strategies/interaction-state'
import {
  ElementInstanceMetadataMap,
  emptyComments,
  jsxAttributesFromMap,
  jsxAttributeValue,
  JSXElement,
  jsxElement,
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
  nukeAllAbsolutePositioningPropsCommands,
  addPositionAbsoluteTopLeft,
  sizeToVisualDimensions,
  toggleResizeToFitSetToFixed,
  isIntrinsicallyInlineElement,
} from '../inspector/inspector-common'
import { CSSProperties } from 'react'
import { setProperty } from '../canvas/commands/set-property-command'
import { getElementContentAffectingType } from '../canvas/canvas-strategies/strategies/group-like-helpers'
import {
  setCssLengthProperty,
  setExplicitCssValue,
} from '../canvas/commands/set-css-length-command'
import { isInfinityRectangle, zeroCanvasPoint } from '../../core/shared/math-utils'

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
): Array<EditorAction> {
  const jumpResult = Canvas.jumpToParent(selectedViews, metadata)
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
  metadataRef: { current: ElementInstanceMetadataMap },
  namesByKey: ShortcutNamesByKey,
  dispatch: EditorDispatch,
): Array<EditorAction> {
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
    if (key === 'tab' && activeElementIsAnInput()) {
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
          const firstTextEditableView = editor.selectedViews.find((v) =>
            MetadataUtils.targetTextEditable(editor.jsxMetadata, v),
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

          const childToSelect = Canvas.getFirstChild(editor.selectedViews, editor.jsxMetadata)
          if (childToSelect != null) {
            return MetaActions.selectComponents([childToSelect], false)
          }
        }
        return []
      },
      [JUMP_TO_PARENT_SHORTCUT]: () => {
        if (isSelectMode(editor.mode)) {
          return jumpToParentActions(editor.selectedViews, editor.jsxMetadata)
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
          return jumpToParentActions(editor.selectedViews, editor.jsxMetadata)
        }

        // TODO: Move this around.
        if (isLiveMode(editor.mode)) {
          return [EditorActions.updateEditorMode(EditorModes.selectMode(editor.mode.controlId))]
        }
        if (isTextEditMode(editor.mode)) {
          return [EditorActions.updateEditorMode(EditorModes.selectMode())]
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
        return isSelectMode(editor.mode)
          ? editor.selectedViews.map((target) => EditorActions.unwrapGroupOrView(target))
          : []
      },
      [WRAP_ELEMENT_DEFAULT_SHORTCUT]: () => {
        return isSelectMode(editor.mode) && editor.selectedViews.length > 0
          ? [
              EditorActions.wrapInView(
                editor.selectedViews,
                detectBestWrapperElement(editor.jsxMetadata, editor.selectedViews[0], () =>
                  generateUidWithExistingComponents(editor.projectContents),
                ),
              ),
            ]
          : []
      },
      [WRAP_ELEMENT_PICKER_SHORTCUT]: () => {
        return isSelectMode(editor.mode)
          ? [EditorActions.openFloatingInsertMenu({ insertMenuMode: 'wrap' })]
          : []
      },
      // For now, the "Group / G" shortcuts do the same as the Wrap Element shortcuts – until we have Grouping working again
      [GROUP_ELEMENT_DEFAULT_SHORTCUT]: () => {
        return isSelectMode(editor.mode) && editor.selectedViews.length > 0
          ? [
              EditorActions.wrapInElement(
                editor.selectedViews,
                detectBestWrapperElement(editor.jsxMetadata, editor.selectedViews[0], () =>
                  generateUidWithExistingComponents(editor.projectContents),
                ),
              ),
            ]
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
            toggleStylePropPath(PP.create('style', 'boxShadow'), toggleShadow),
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
      [TOGGLE_NAVIGATOR]: () => {
        return [EditorActions.togglePanel('navigator')]
      },
      [TOGGLE_INSPECTOR]: () => {
        return [EditorActions.togglePanel('rightmenu')]
      },
      [TOGGLE_DESIGNER_ADDITIONAL_CONTROLS_SHORTCUT]: () => {
        return [EditorActions.toggleInterfaceDesignerAdditionalControls()]
      },
      [TOGGLE_CODE_EDITOR_SHORTCUT]: () => {
        return [EditorActions.toggleInterfaceDesignerCodeEditor()]
      },
      [TOGGLE_INSPECTOR_AND_NAVIGATOR_SHORTCUT]: () => {
        return [EditorActions.togglePanel('rightmenu'), EditorActions.togglePanel('navigator')]
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
      [OPEN_EYEDROPPER]: () => {
        const selectedViews = editor.selectedViews
        if (selectedViews.length === 0) {
          return []
        }
        void pickColorWithEyeDropper()
          .then(({ sRGBHex }) =>
            dispatch(
              selectedViews.map((view) =>
                EditorActions.setProperty(
                  view,
                  PP.create('style', 'backgroundColor'),
                  jsxAttributeValue(sRGBHex, emptyComments),
                ),
              ),
            ),
          )
          .catch((e) => console.error(e))
        return []
      },
      [TEXT_EDIT_MODE]: () => {
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
        const selectedElementsFlexContainers = detectAreElementsFlexContainers(
          editor.jsxMetadata,
          editor.selectedViews,
        )
        const commands = commandsForFirstApplicableStrategy(
          editor.jsxMetadata,
          editor.selectedViews,
          selectedElementsFlexContainers ? removeFlexLayoutStrategies : addFlexLayoutStrategies,
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

        const commands = editor.selectedViews.flatMap((elementPath) => {
          const element = MetadataUtils.findElementByElementPath(editor.jsxMetadata, elementPath)
          if (element == null) {
            return []
          }

          const contentAffectingType = getElementContentAffectingType(
            editor.jsxMetadata,
            editor.allElementProps,
            elementPath,
          )

          if (contentAffectingType === 'fragment') {
            return []
          }

          if (contentAffectingType === 'sizeless-div') {
            const childrenBoundingFrame = MetadataUtils.getFrameInCanvasCoords(
              elementPath,
              editor.jsxMetadata,
            )
            if (childrenBoundingFrame == null || isInfinityRectangle(childrenBoundingFrame)) {
              return []
            }

            return [
              setCssLengthProperty(
                'always',
                elementPath,
                PP.create('style', 'width'),
                setExplicitCssValue(cssPixelLength(childrenBoundingFrame.width)),
                element.specialSizeMeasurements.parentFlexDirection ?? null,
              ),
              setCssLengthProperty(
                'always',
                elementPath,
                PP.create('style', 'height'),
                setExplicitCssValue(cssPixelLength(childrenBoundingFrame.height)),
                element.specialSizeMeasurements.parentFlexDirection ?? null,
              ),
            ]
          }

          if (MetadataUtils.isPositionAbsolute(element)) {
            return [
              ...nukeAllAbsolutePositioningPropsCommands(elementPath),
              ...(isIntrinsicallyInlineElement(element)
                ? [
                    ...sizeToVisualDimensions(editor.jsxMetadata, elementPath),
                    setProperty(
                      'always',
                      elementPath,
                      PP.create('style', 'display'),
                      'inline-block',
                    ),
                  ]
                : []),
            ]
          } else {
            return [
              ...sizeToVisualDimensions(editor.jsxMetadata, elementPath),
              ...addPositionAbsoluteTopLeft(editor.jsxMetadata, elementPath),
            ]
          }
        })

        if (commands.length === 0) {
          return []
        }

        return [EditorActions.applyCommandsAction(commands)]
      },
      [RESIZE_TO_FIT]: () => {
        if (!isSelectMode(editor.mode)) {
          return []
        }
        const commands = toggleResizeToFitSetToFixed(editor.jsxMetadata, editor.selectedViews)
        if (commands.length === 0) {
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

function detectBestWrapperElement(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
  makeUid: () => string,
): { element: JSXElement; importsToAdd: Imports } {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  const uid = makeUid()
  if (
    element == null ||
    element.specialSizeMeasurements.parentFlexDirection == null ||
    element.specialSizeMeasurements.parentLayoutSystem !== 'flex'
  ) {
    return { element: defaultUnstyledDivElement(uid), importsToAdd: {} }
  }

  const style: CSSProperties = {
    display: 'flex',
    flexDirection: element.specialSizeMeasurements.parentFlexDirection,
    contain: 'layout',
  }

  if (
    element.specialSizeMeasurements.parentFlexGap != null &&
    element.specialSizeMeasurements.parentFlexGap !== 0
  ) {
    style.gap = element.specialSizeMeasurements.parentFlexGap
  }

  const props = jsxAttributesFromMap({
    'data-uid': jsxAttributeValue(uid, emptyComments),
    style: jsxAttributeValue(style, emptyComments),
  })

  return {
    element: jsxElement('div', uid, props, []),
    importsToAdd: {},
  }
}
