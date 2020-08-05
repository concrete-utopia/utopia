import * as R from 'ramda'
import { findElementAtPath, MetadataUtils } from '../../core/model/element-metadata-utils'
import { ComponentMetadata, ElementInstanceMetadata } from '../../core/shared/element-template'
import { generateUidWithExistingComponents } from '../../core/model/element-template-utils'
import { isUtopiaAPITextElement } from '../../core/model/project-file-utils'
import {
  InstancePath,
  TemplatePath,
  importDetails,
  importAlias,
} from '../../core/shared/project-file-types'
import { CanvasMousePositionRaw } from '../../templates/editor-canvas'
import Keyboard, {
  KeyCharacter,
  KeysPressed,
  looseCheckModifier,
  modifiersForEvent,
  strictCheckModifiers,
} from '../../utils/keyboard'
import Utils, { normalisedFrameToCanvasFrame } from '../../utils/utils'
import {
  CanvasPoint,
  CanvasRectangle,
  rectangleIntersection,
  canvasRectangle,
} from '../../core/shared/math-utils'
import { EditorAction, EditorDispatch } from '../editor/action-types'
import * as EditorActions from '../editor/actions/actions'
import {
  defaultEllipseElement,
  defaultRectangleElement,
  defaultTextElement,
  defaultViewElement,
} from '../editor/defaults'
import { EditorModes, Mode } from '../editor/editor-modes'
import {
  DerivedState,
  EditorState,
  getOpenImportsFromState,
  getOpenUtopiaJSXComponentsFromState,
} from '../editor/store/editor-state'
import {
  toggleBorder,
  toggleShadow,
  toggleStylePropPath,
  toggleBackgroundLayers,
  toggleStylePropPaths,
} from '../inspector/common/css-utils'
import { LeftMenuTab } from '../navigator/left-pane'
import * as PP from '../../core/shared/property-path'
import * as TP from '../../core/shared/template-path'
import CanvasActions from './canvas-actions'
import { adjustAllSelectedFrames } from './controls/select-mode/move-utils'
import { flatMapArray } from '../../core/shared/array-utils'

export const enum TargetSearchType {
  ParentsOfSelected = 'ParentsOfSelected',
  SiblingsOfSelected = 'SiblingsOfSelected',
  ChildrenOfSelected = 'ChildrenOfSelected',
  SelectedElements = 'SelectedElements',
  TopLevelElements = 'TopLevelElements',
  All = 'all',
}

type FrameWithPath = {
  path: TemplatePath
  frame: CanvasRectangle
}

const Canvas = {
  parentsAndSiblings: [
    TargetSearchType.SelectedElements,
    TargetSearchType.SiblingsOfSelected,
    TargetSearchType.ParentsOfSelected,
  ],
  parentsSiblingsAndChildren: [
    TargetSearchType.ChildrenOfSelected,
    TargetSearchType.SelectedElements,
    TargetSearchType.SiblingsOfSelected,
    TargetSearchType.ParentsOfSelected,
  ],
  getFramesInCanvasContext(
    components: ComponentMetadata[],
    useBoundingFrames: boolean,
  ): Array<FrameWithPath> {
    function recurseChildren(
      offset: CanvasPoint,
      component: ElementInstanceMetadata,
    ): { boundingRect: CanvasRectangle | null; frames: Array<FrameWithPath> } {
      const componentFrame = component.localFrame

      if (componentFrame == null) {
        return {
          boundingRect: null,
          frames: [],
        }
      }

      const offsetFrame: CanvasRectangle = Utils.getCanvasRectangleWithCanvasOffset(
        offset,
        componentFrame,
      )
      const overflows = MetadataUtils.overflows(component)
      const includeClippedNext = useBoundingFrames && overflows
      const childFrames = component.children.map((child) => {
        const recurseResults = recurseChildren(offsetFrame, child)
        const rectToBoundWith = includeClippedNext ? recurseResults.boundingRect : offsetFrame
        return { boundingRect: rectToBoundWith, frames: recurseResults.frames }
      })
      const allChildrenBounds = Utils.boundingRectangleArray(
        Utils.pluck(childFrames, 'boundingRect'),
      )
      if (childFrames.length > 0 && allChildrenBounds != null) {
        const allChildrenFrames = Utils.pluck(childFrames, 'frames').flat()
        const boundingRect = Utils.boundingRectangle(offsetFrame, allChildrenBounds)
        const toAppend: FrameWithPath = { path: component.templatePath, frame: boundingRect }
        return {
          boundingRect: boundingRect,
          frames: [toAppend].concat(allChildrenFrames),
        }
      } else {
        const boundingRect = offsetFrame
        const toAppend = { path: component.templatePath, frame: boundingRect }
        return { boundingRect: boundingRect, frames: [toAppend] }
      }
    }

    return Utils.flatMapArray((rootComponent) => {
      if (rootComponent.globalFrame == null) {
        return []
      } else {
        const nonNullGlobalFrame = rootComponent.globalFrame
        return flatMapArray(
          (root) => recurseChildren(nonNullGlobalFrame, root).frames,
          rootComponent.rootElements,
        )
      }
    }, components)
  },
  jumpToParent(selectedViews: Array<TemplatePath>): TemplatePath | 'CLEAR' | null {
    switch (selectedViews.length) {
      case 0:
        // Nothing is selected, so do nothing.
        return null
      case 1:
        // Only a single element is selected...
        const parentPath = TP.parentPath(selectedViews[0])
        if (parentPath == null) {
          // ...the selected element is a top level one, so deselect.
          return 'CLEAR'
        } else {
          // ...the selected element has a parent, so select that.
          return parentPath
        }
      default:
        // Multiple elements are selected so select the topmost element amongst them.
        const newSelection: TemplatePath | null = selectedViews.reduce(
          (working: TemplatePath | null, selectedView) => {
            if (working == null) {
              return selectedView
            } else {
              return R.minBy(TP.depth, selectedView, working)
            }
          },
          null,
        )
        return Utils.forceNotNull('Internal Error.', newSelection)
    }
  },
  jumpToSibling(
    selectedViews: Array<TemplatePath>,
    components: ComponentMetadata[],
    forwards: boolean,
  ): TemplatePath | null {
    switch (selectedViews.length) {
      case 0:
        return null
      case 1:
        const singleSelectedElement = selectedViews[0]
        const siblings = MetadataUtils.getSiblings(components, singleSelectedElement)
        const pathsToStep = siblings.map((s) => s.templatePath)
        return Utils.stepInArray(TP.pathsEqual, forwards ? 1 : -1)(
          pathsToStep,
          singleSelectedElement,
        )
      default:
        // Multiple elements are selected so select the topmost element amongst them.
        const newSelection: TemplatePath | null = selectedViews.reduce(
          (working: TemplatePath | null, selectedView) => {
            if (working == null) {
              return selectedView
            } else {
              return R.minBy(TP.depth, selectedView, working)
            }
          },
          null,
        )
        return Utils.forceNotNull('Internal Error.', newSelection)
    }
  },
  getFirstChild(
    selectedViews: Array<TemplatePath>,
    components: Array<ComponentMetadata>,
  ): TemplatePath | null {
    if (selectedViews.length !== 1) {
      return null
    } else {
      const children = MetadataUtils.getImmediateChildren(components, selectedViews[0])
      return children.length > 0 ? children[0].templatePath : null
    }
  },
  targetFilter(
    selectedViews: Array<TemplatePath>,
    searchTypes: Array<TargetSearchType>,
  ): Array<(path: TemplatePath) => boolean> {
    return searchTypes.map((searchType) => {
      switch (searchType) {
        case TargetSearchType.ParentsOfSelected:
          return (path: TemplatePath) => {
            for (const selectedView of selectedViews) {
              if (TP.isAncestorOf(selectedView, path)) {
                return true
              }
            }
            return false
          }
        case TargetSearchType.ChildrenOfSelected:
          return (path: TemplatePath) => {
            for (const selectedView of selectedViews) {
              if (TP.isChildOf(path, selectedView)) {
                return true
              }
            }
            return false
          }
        case TargetSearchType.SiblingsOfSelected:
          return (path: TemplatePath) => {
            for (const selectedView of selectedViews) {
              if (TP.isSiblingOf(selectedView, path) && !TP.containsPath(path, selectedViews)) {
                return true
              }
            }
            return false
          }
        case TargetSearchType.TopLevelElements:
          return (path: TemplatePath) => {
            // TODO Scene Implementation
            if (TP.depth(path) === 2) {
              return true
            }
            return false
          }
        case TargetSearchType.SelectedElements:
          return (path: TemplatePath) => {
            if (TP.containsPath(path, selectedViews)) {
              return true
            }
            return false
          }
        case TargetSearchType.All:
          return (path: TemplatePath) => {
            return true
          }
        default:
          const _exhaustiveCheck: never = searchType
          throw new Error(`Unknown search type ${JSON.stringify(searchType)}`)
      }
    })
  },
  getAllTargetsAtPoint(
    editor: EditorState,
    canvasPosition: CanvasPoint,
    searchTypes: Array<TargetSearchType>,
    useBoundingFrames: boolean,
    looseTargetingForZeroSizedElements: 'strict' | 'loose',
  ): Array<TemplatePath> {
    const looseReparentThreshold = 5
    const targetFilters = Canvas.targetFilter(editor.selectedViews, searchTypes)
    const framesWithPaths = Canvas.getFramesInCanvasContext(
      editor.jsxMetadataKILLME,
      useBoundingFrames,
    )
    const filteredFrames = framesWithPaths.filter((frameWithPath) => {
      const shouldUseLooseTargeting =
        looseTargetingForZeroSizedElements &&
        (frameWithPath.frame.width <= 0 || frameWithPath.frame.height <= 0)

      return targetFilters.some((filter) => filter(frameWithPath.path)) &&
        !editor.hiddenInstances.some((hidden) =>
          TP.isAncestorOf(frameWithPath.path, hidden, true),
        ) &&
        shouldUseLooseTargeting
        ? rectangleIntersection(
            canvasRectangle({
              x: frameWithPath.frame.x,
              y: frameWithPath.frame.y,
              width: frameWithPath.frame.width || 1,
              height: frameWithPath.frame.height || 1,
            }),
            canvasRectangle({
              x: canvasPosition.x - looseReparentThreshold,
              y: canvasPosition.y - looseReparentThreshold,
              width: 2 * looseReparentThreshold,
              height: 2 * looseReparentThreshold,
            }),
          ) != null
        : Utils.rectContainsPoint(frameWithPath.frame, canvasPosition)
    })
    filteredFrames.reverse()

    const targets = filteredFrames.map((filteredFrame) => filteredFrame.path)
    return targets
  },
  getNextTarget(
    current: Array<TemplatePath>,
    targetStack: Array<TemplatePath>,
  ): TemplatePath | null {
    if (current.length <= 1) {
      const currentIndex =
        current.length === 0
          ? -1
          : R.findIndex((target) => TP.pathsEqual(target, current[0]), targetStack)
      const endOrNotFound = currentIndex === -1 || currentIndex === targetStack.length - 1
      if (endOrNotFound) {
        return targetStack[0]
      } else {
        return targetStack[currentIndex + 1]
      }
    } else {
      return null
    }
  },
  handleKeyDown(
    event: KeyboardEvent,
    editor: EditorState,
    derived: DerivedState,
    dispatch: EditorDispatch,
    insertImageFn: () => void,
  ): Array<EditorAction> {
    const key = Keyboard.keyCharacterForCode(event.keyCode)

    const modifiers = modifiersForEvent(event)

    const cmd = strictCheckModifiers(modifiers, ['cmd'])
    const shift = strictCheckModifiers(modifiers, ['shift'])
    const altCmd = strictCheckModifiers(modifiers, ['alt', 'cmd'])
    const shiftCmd = strictCheckModifiers(modifiers, ['shift', 'cmd'])
    const ctrl = strictCheckModifiers(modifiers, ['ctrl'])
    const shiftCtrl = strictCheckModifiers(modifiers, ['shift', 'ctrl'])
    const noModifier = modifiers.length === 0

    const modeType = editor.mode.type
    const adjustment = frameAdjustment(looseCheckModifier(modifiers, 'shift'))

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
        } else {
          return []
        }
      case 'plus':
        if (altCmd) {
          return [CanvasActions.zoomUI(true)]
        } else if (cmd) {
          return [CanvasActions.zoom(Utils.increaseScale(editor.canvas.scale))]
        } else {
          return []
        }
      case 'minus':
        if (altCmd) {
          return [CanvasActions.zoomUI(false)]
        } else if (cmd) {
          return [CanvasActions.zoom(Utils.decreaseScale(editor.canvas.scale))]
        } else {
          return []
        }
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
            if (childToSelect == null) {
              return []
            } else {
              return [EditorActions.selectComponents([childToSelect], false)]
            }
          }
        } else if (shift && modeType === 'select') {
          return jumpToParentActions(editor.selectedViews)
        } else if (cmd && modeType === 'select') {
          return [EditorActions.wrapInView(editor.selectedViews)]
        } else {
          return []
        }
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
        return []
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
        } else {
          return []
        }
      case 'tab':
        if ((shift || noModifier) && modeType === 'select') {
          const forwards = !shift
          const tabbedTo = Canvas.jumpToSibling(
            editor.selectedViews,
            editor.jsxMetadataKILLME,
            forwards,
          )
          if (tabbedTo == null) {
            return []
          } else {
            return [EditorActions.selectComponents([tabbedTo], false)]
          }
        } else {
          return []
        }
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
        } else {
          return []
        }
      case 'b':
        if (noModifier && editor.selectedViews.length > 0) {
          return TP.filterScenes(editor.selectedViews).map((target) =>
            EditorActions.toggleProperty(
              target,
              toggleStylePropPath(PP.create(['style', 'border']), toggleBorder),
            ),
          )
        } else {
          return []
        }
      case 'c':
        if (cmd) {
          return [EditorActions.copySelectionToClipboard()]
        } else if (editor.selectedViews.length > 0) {
          // TODO Toggle Clipping
          return []
        } else {
          return []
        }
      case 'd':
        if (cmd && editor.selectedViews.length > 0) {
          return [EditorActions.duplicateSelected()]
        } else if (cmd) {
          return []
        }
        return []
      case 'f':
        if (noModifier && editor.selectedViews.length > 0) {
          return TP.filterScenes(editor.selectedViews).map((target) =>
            EditorActions.toggleProperty(target, toggleStylePropPaths(toggleBackgroundLayers)),
          )
        } else {
          return []
        }
      case 'g':
        if (editor.selectedViews.length > 0) {
          if (shiftCmd) {
            return editor.selectedViews.map((target) => EditorActions.unwrapGroupOrView(target))
          } else if (cmd) {
            return [EditorActions.wrapInGroup(editor.selectedViews)]
          }
        }
        return []
      case 'h':
        if (shiftCmd && editor.selectedViews.length > 0) {
          return [EditorActions.toggleHidden()]
        } else {
          return []
        }
      case 'i':
        if (noModifier && (modeType === 'select' || modeType === 'insert')) {
          insertImageFn()
          return []
        } else {
          return []
        }
      case 'p':
        if (shiftCmd && editor.selectedViews.length > 0) {
          return [] // TODO Nodegraph insertConnectedNodeActions('GetProperty', editor.selectedViews[0])
        } else if (noModifier && modeType === 'select') {
          return [] // sorry but path is too broken for now [enableInsertMode({package: 'svg', template: 'path'}, defaultPathCommands())]
        } else {
          return []
        }
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
        return []
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

        return []
      case 's':
        if (noModifier && modeType === 'select' && editor.selectedViews.length > 0) {
          return TP.filterScenes(editor.selectedViews).map((target) =>
            EditorActions.toggleProperty(
              target,
              toggleStylePropPath(PP.create(['style', 'boxShadow']), toggleShadow),
            ),
          )
        } else if (shiftCmd && editor.selectedViews.length > 0) {
          return [] // TODO Nodegraph insertConnectedNodeActions('SetProperty', editor.selectedViews[0])
        } else {
          return []
        }
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
        } else if (shiftCmd && editor.selectedViews.length > 0) {
          return [] // TODO Nodegraph insertConnectedNodeActions('TouchInteraction', editor.selectedViews[0])
        } else {
          return []
        }
      case 'v':
        if (altCmd) {
          if (editor.selectedViews.length > 0) {
            // TODO Paste Style
            return []
          } else {
            return []
          }
        } else if (noModifier && (modeType === 'select' || modeType === 'insert')) {
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
        } else {
          return []
        }
      case 'x':
        if (cmd) {
          return [EditorActions.copySelectionToClipboard(), EditorActions.deleteSelected()]
        } else {
          return []
        }
      case 'z':
        return [EditorActions.setHighlightsEnabled(false), EditorActions.clearHighlightedViews()]
      case '[':
        if (cmd) {
          event.preventDefault()
          return [EditorActions.moveSelectedBackward()]
        } else if (altCmd) {
          return [EditorActions.moveSelectedToBack()]
        } else {
          return []
        }
      case ']':
        if (cmd) {
          event.preventDefault()
          return [EditorActions.moveSelectedForward()]
        } else if (altCmd) {
          return [EditorActions.moveSelectedToFront()]
        } else {
          return []
        }
      default:
        return []
    }
  },
  handleKeyUp(key: KeyCharacter, editor: EditorState, derived: DerivedState): Array<EditorAction> {
    switch (key) {
      case 'z':
        return [EditorActions.setHighlightsEnabled(true)]
      default:
        return []
    }
  },
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

export default Canvas
