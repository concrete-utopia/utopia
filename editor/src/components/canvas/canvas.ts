import * as R from 'ramda'
import { findElementAtPath, MetadataUtils } from '../../core/model/element-metadata-utils'
import {
  ComponentMetadata,
  ElementInstanceMetadata,
  JSXMetadata,
} from '../../core/shared/element-template'
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
import * as EditorActions from '../editor/actions/action-creators'
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
    metadata: JSXMetadata,
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
      const children = MetadataUtils.getImmediateChildren(metadata, component.templatePath)
      const childFrames = children.map((child) => {
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
        const rootElements = MetadataUtils.getImmediateChildren(metadata, rootComponent.scenePath)
        return flatMapArray(
          (root) => recurseChildren(nonNullGlobalFrame, root).frames,
          rootElements,
        )
      }
    }, metadata.components)
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
    components: JSXMetadata,
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
  getFirstChild(selectedViews: Array<TemplatePath>, components: JSXMetadata): TemplatePath | null {
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
    looseTargetingForZeroSizedElements: 'strict' | 'loose' | 'around-the-target',
  ): Array<TemplatePath> {
    const usesExperiment = looseTargetingForZeroSizedElements === 'around-the-target'
    const looseReparentThreshold = usesExperiment ? 5 : 35
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
        (shouldUseLooseTargeting || usesExperiment)
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
  handleKeyUp(key: KeyCharacter, editor: EditorState, derived: DerivedState): Array<EditorAction> {
    switch (key) {
      case 'z':
        return [EditorActions.setHighlightsEnabled(true)]
      default:
        return []
    }
  },
}

export default Canvas
