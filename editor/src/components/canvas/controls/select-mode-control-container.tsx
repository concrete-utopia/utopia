import * as R from 'ramda'
import * as React from 'react'
import { KeysPressed } from '../../../utils/keyboard'
import Utils from '../../../utils/utils'
import { CanvasPoint, CanvasRectangle, CanvasVector } from '../../../core/shared/math-utils'
import { TemplatePath, ScenePath } from '../../../core/shared/project-file-types'
import { EditorAction } from '../../editor/action-types'
import * as EditorActions from '../../editor/actions/action-creators'
import { DuplicationState } from '../../editor/store/editor-state'
import * as TP from '../../../core/shared/template-path'
import { CanvasPositions, MoveDragState, ResizeDragState, moveDragState } from '../canvas-types'
import { Guidelines, Guideline } from '../guideline'
import { ConstraintsControls } from './constraints-control'
import { DistanceGuideline } from './distance-guideline'
import { GuidelineControl } from './guideline-control'
import { collectParentAndSiblingGuidelines, getSnappedGuidelines } from './guideline-helpers'
import { ControlProps } from './new-canvas-controls'
import { ComponentLabelControl } from './component-area-control'
import { YogaControls } from './yoga-control'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { keepDeepReferenceEqualityIfPossible } from '../../../utils/react-performance'
import { OutlineControls } from './outline-control'
import { RepositionableControl } from './repositionable-control'
import { LeftMenuTab } from '../../navigator/left-pane'
import CanvasActions from '../canvas-actions'
import { getOriginalCanvasFrames, createDuplicationNewUIDs } from '../canvas-utils'
import { areYogaChildren } from './select-mode/yoga-utils'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { BoundingMarks } from './parent-bounding-marks'
import { RightMenuTab } from '../right-menu'
import { getSelectableViews } from './select-mode/select-mode-hooks'
import { getAllTargetsAtPoint } from '../dom-lookup'
import { WindowMousePositionRaw } from '../../../templates/editor-canvas'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { isSceneAgainstImports } from '../../../core/model/project-file-utils'
import { isRight } from '../../../core/shared/either'

export const SnappingThreshold = 5

function getDistanceGuidelines(
  highlightedView: TemplatePath,
  componentMetadata: ElementInstanceMetadataMap,
): Array<Guideline> {
  const frame = MetadataUtils.getFrameInCanvasCoords(highlightedView, componentMetadata)
  if (frame == null) {
    return []
  } else {
    return Guidelines.guidelinesForFrame(frame, false)
  }
}

interface SelectModeControlContainerProps extends ControlProps {
  startDragStateAfterDragExceedsThreshold: (
    nativeEvent: MouseEvent,
    foundTarget: TemplatePath,
  ) => void
  setSelectedViewsLocally: (newSelectedViews: Array<TemplatePath>) => void
  keysPressed: KeysPressed
  windowToCanvasPosition: (event: MouseEvent) => CanvasPositions
  isDragging: boolean // set only when user already moves a cursor a little after a mousedown
  isResizing: boolean
  selectionEnabled: boolean
  maybeHighlightOnHover: (target: TemplatePath) => void
  maybeClearHighlightsOnHoverEnd: () => void
  duplicationState: DuplicationState | null
  dragState: MoveDragState | ResizeDragState | null
}

interface SelectModeControlContainerState {
  moveGuidelines: Array<Guideline>
  lastHovered: TemplatePath | null
}

// SelectModeControlContainer is a single React component containing all the individual
// SelectModeControl components (one for every utopia component).
export class SelectModeControlContainer extends React.Component<
  SelectModeControlContainerProps,
  SelectModeControlContainerState
> {
  // put local state that does not affect the render here instead of setState!
  hasReparented: boolean = false
  furthestDragDelta: CanvasVector | null = null
  dragDelta: CanvasVector = Utils.zeroPoint as CanvasVector
  previousDragPosition: CanvasPositions | null = null

  currentlyReparenting: boolean = false

  constructor(props: SelectModeControlContainerProps) {
    super(props)
    this.state = {
      moveGuidelines: [],
      lastHovered: null,
    }
  }

  static getDerivedStateFromProps(
    props: SelectModeControlContainerProps,
    previousState: SelectModeControlContainerState,
  ) {
    const guidelines = collectParentAndSiblingGuidelines(
      props.componentMetadata,
      props.selectedViews,
    )
    return {
      moveGuidelines: keepDeepReferenceEqualityIfPossible(previousState.moveGuidelines, guidelines),
    }
  }

  selectComponent = (target: TemplatePath, isMultiselect: boolean): Array<TemplatePath> => {
    // TODO BALAZS Remove this and unify with select-mode-hooks
    if (this.props.selectedViews.some((view) => TP.pathsEqual(target, view))) {
      return this.props.selectedViews
    } else {
      let updatedSelection = [target]
      if (isMultiselect) {
        updatedSelection = TP.addPathIfMissing(target, this.props.selectedViews)
      }

      this.props.setSelectedViewsLocally(updatedSelection)

      /**
       * As of November 2020, we need two nested requestAnimationFrames here,
       * the first one is called almost immediately (before vsync),
       * the second one is properly called in the next frame
       */
      requestAnimationFrame(() => {
        requestAnimationFrame(() => {
          let selectActions = [
            EditorActions.clearHighlightedViews(),
            EditorActions.selectComponents(updatedSelection, false),
            EditorActions.setRightMenuTab(RightMenuTab.Inspector),
          ]
          this.props.dispatch(selectActions, 'canvas')
        })
      })

      return updatedSelection
    }
  }

  onControlMouseDown = (
    selectedViews: Array<TemplatePath>,
    target: TemplatePath,
    start: CanvasPoint,
    originalEvent: React.MouseEvent<HTMLDivElement>,
  ) => {
    this.props.startDragStateAfterDragExceedsThreshold(originalEvent.nativeEvent, target)
  }

  onContextMenu = (event: React.MouseEvent<HTMLDivElement>) => {
    event.stopPropagation()
    event.preventDefault()
    const elementsUnderCursor = getAllTargetsAtPoint(
      this.props.componentMetadata,
      this.props.selectedViews,
      this.props.hiddenInstances,
      this.props.focusedElementPath,
      'no-filter',
      WindowMousePositionRaw,
      this.props.scale,
      this.props.canvasOffset,
    )
    this.props.dispatch(
      [
        EditorActions.showContextMenu('context-menu-canvas', event.nativeEvent, {
          elementsUnderCursor,
        }),
      ],
      'canvas',
    )
  }

  getTargetViews(): Array<TemplatePath> {
    return this.props.duplicationState == null
      ? this.props.selectedViews
      : this.props.duplicationState.duplicateRoots.map((r) => r.currentTP)
  }

  // Highlights the parent of the actual selection. It does not highlight anything if not all the
  // components in the selection have the same parent.
  highlightSelectionParent(): EditorAction[] {
    if (this.props.selectedViews.length > 0) {
      const firstParent = TP.parentPath(this.props.selectedViews[0])
      if (firstParent != null) {
        const allSelectedViewsHasSameParent = this.props.selectedViews.every((et) =>
          TP.pathsEqual(TP.parentPath(et), firstParent),
        )
        if (allSelectedViewsHasSameParent) {
          return [EditorActions.setHighlightedView(firstParent)]
        }
      }
    }
    return []
  }

  resetState = () => {
    this.furthestDragDelta = null
    this.dragDelta = Utils.zeroPoint as CanvasVector
    this.previousDragPosition = null
    this.currentlyReparenting = false
  }

  isHighlighted = (path: TemplatePath) => {
    return this.props.highlightedViews.some((highlighted) => TP.pathsEqual(path, highlighted))
  }

  getClippedArea = (target: TemplatePath): CanvasRectangle | null => {
    const targetFrame = MetadataUtils.getFrameInCanvasCoords(target, this.props.componentMetadata)

    return TP.getAncestors(target).reduce(
      (frameIntersect: CanvasRectangle | null, current: TemplatePath) => {
        if (TP.isScenePath(current)) {
          // TODO Scene Implementation - should scenes act like they clip?
          return frameIntersect
        }

        const currentInstance = MetadataUtils.getElementByInstancePathMaybe(
          this.props.componentMetadata,
          current,
        )
        if (currentInstance == null) {
          return frameIntersect
        } else {
          if (MetadataUtils.overflows(currentInstance)) {
            return frameIntersect
          } else {
            const currentFrame = currentInstance.globalFrame
            if (frameIntersect != null && currentFrame != null) {
              return Utils.rectangleIntersection(frameIntersect, currentFrame)
            } else {
              return null
            }
          }
        }
      },
      targetFrame,
    )
  }

  renderLabel = (target: TemplatePath, hoverEnabled: boolean): JSX.Element | null => {
    const frame = MetadataUtils.getFrameInCanvasCoords(target, this.props.componentMetadata)
    if (frame == null) {
      return null
    }
    return (
      <ComponentLabelControl
        key={`${TP.toComponentId(target)}-label`}
        testID={`label-control-${TP.toComponentId(target)}`}
        mouseEnabled={true}
        componentMetadata={this.props.componentMetadata}
        target={target}
        frame={frame}
        scale={this.props.scale}
        highlighted={this.isHighlighted(target)}
        selectedComponents={this.props.selectedViews}
        dispatch={this.props.dispatch}
        canvasOffset={this.props.canvasOffset}
        hoverEffectEnabled={hoverEnabled}
        doubleClickToSelect={false}
        selectComponent={this.selectComponent}
        onMouseDown={this.onControlMouseDown}
        onHover={this.onHover}
        onHoverEnd={this.onHoverEnd}
        keysPressed={this.props.keysPressed}
        windowToCanvasPosition={this.props.windowToCanvasPosition}
        selectedViews={this.props.selectedViews}
        imports={this.props.imports}
        showAdditionalControls={this.props.showAdditionalControls}
      />
    )
  }

  clearSelection = () => {
    if (this.props.selectionEnabled) {
      this.props.dispatch([EditorActions.clearSelection()], 'canvas')
    }
  }

  getSelectedBoundingBox = (views: TemplatePath[]) => {
    let boundingRectangles: Array<CanvasRectangle> = []
    Utils.fastForEach(views, (view) => {
      const frame = MetadataUtils.getFrameInCanvasCoords(view, this.props.componentMetadata)
      if (frame != null) {
        boundingRectangles.push(frame)
      }
    })
    return Utils.boundingRectangleArray(boundingRectangles)
  }

  getMoveGuidelines = () => {
    if (
      this.props.selectedViews.length > 0 &&
      this.props.isDragging &&
      !areYogaChildren(this.props.componentMetadata, this.props.selectedViews) &&
      !this.props.keysPressed['cmd']
    ) {
      const draggedElements =
        this.props.dragState?.type === 'MOVE_DRAG_STATE' ? this.props.dragState.draggedElements : []
      const boundingBox = this.getSelectedBoundingBox(draggedElements)

      if (boundingBox == null) {
        return []
      }

      const closestGuideLines = getSnappedGuidelines(
        this.state.moveGuidelines,
        null,
        boundingBox,
        this.props.scale,
      )
      return closestGuideLines.map((g, i) => {
        return (
          <GuidelineControl
            key={
              (g.guideline.type === 'XAxisGuideline' ? 'x' + g.guideline.x : 'y' + g.guideline.y) +
              i
            }
            guidelineWithSnapping={g}
            targetFrame={boundingBox}
            canvasOffset={this.props.canvasOffset}
            scale={this.props.scale}
          />
        )
      })
    } else {
      return []
    }
  }

  onHover = (target: TemplatePath) => {
    if (this.inSelection(target)) {
      this.props.maybeClearHighlightsOnHoverEnd()
    } else {
      this.props.maybeHighlightOnHover(target)
    }
    this.setState({
      lastHovered: target,
    })
  }

  onHoverEnd = (target: TemplatePath) => {
    if (this.props.selectionEnabled && !this.props.isDragging) {
      this.props.dispatch([EditorActions.clearHighlightedViews()], 'canvas')
    }
    this.setState({
      lastHovered: null,
    })
  }

  getDistanceGuidelines = () => {
    if (
      this.props.selectedViews.length > 0 &&
      !this.props.isDragging &&
      this.props.keysPressed['alt']
    ) {
      let boundingBoxes: (CanvasRectangle | null)[] = []
      if (TP.areAllElementsInSameScene(this.props.selectedViews)) {
        boundingBoxes = [this.getSelectedBoundingBox(this.props.selectedViews)]
      } else {
        boundingBoxes = this.props.selectedViews.map((view) => this.getSelectedBoundingBox([view]))
      }

      if (boundingBoxes.length < 1) {
        return []
      }
      let hoveredSelectedItem: TemplatePath | null = null
      Utils.fastForEach(this.props.selectedViews, (selectedView) => {
        if (TP.pathsEqual(selectedView, this.state.lastHovered)) {
          if (
            hoveredSelectedItem == null ||
            TP.depth(hoveredSelectedItem) < TP.depth(selectedView)
          ) {
            hoveredSelectedItem = selectedView
          }
        }
      })

      let guideLineElements: any[] = []
      Utils.fastForEach(boundingBoxes, (boundingBox, index) => {
        if (boundingBox != null) {
          let distanceGuidelines: Array<Guideline> = []
          if (hoveredSelectedItem == null) {
            distanceGuidelines = Utils.flatMapArray((highlightedView) => {
              const highlightedViewIsSelected = this.props.selectedViews.some((selectedView) =>
                TP.pathsEqual(selectedView, highlightedView),
              )
              if (highlightedViewIsSelected) {
                return []
              } else {
                if (TP.isFromSameSceneAs(highlightedView, this.props.selectedViews[index])) {
                  return getDistanceGuidelines(highlightedView, this.props.componentMetadata)
                } else {
                  return []
                }
              }
            }, this.props.highlightedViews)
          } else {
            const parentPath = TP.parentPath(hoveredSelectedItem)
            if (parentPath != null) {
              if (TP.isFromSameSceneAs(parentPath, this.props.selectedViews[index])) {
                distanceGuidelines = getDistanceGuidelines(parentPath, this.props.componentMetadata)
              }
            }
          }
          guideLineElements.push(
            <DistanceGuideline
              key={`${TP.toComponentId(this.props.selectedViews[index])}-distance-guidelines`}
              canvasOffset={this.props.canvasOffset}
              scale={this.props.scale}
              guidelines={distanceGuidelines}
              selectedViews={this.props.selectedViews}
              highlightedViews={this.props.highlightedViews}
              boundingBox={boundingBox}
            />,
          )
        }
      })

      return guideLineElements
    } else {
      return []
    }
  }

  getBoundingMarks = (): Array<JSX.Element> => {
    let boundingMarks: Array<JSX.Element> = []
    if (this.props.isDragging || this.props.keysPressed['alt']) {
      const targets = TP.filterScenes(this.props.selectedViews)
      if (targets.length > 0) {
        const targetInstance = MetadataUtils.getElementByInstancePathMaybe(
          this.props.componentMetadata,
          targets[0],
        )
        if (targetInstance != null) {
          const { specialSizeMeasurements } = targetInstance

          const immediateParentBounds = specialSizeMeasurements.immediateParentBounds
          if (
            !specialSizeMeasurements.immediateParentProvidesLayout &&
            immediateParentBounds != null
          ) {
            boundingMarks.push(
              <BoundingMarks
                key={`${TP.toComponentId(targets[0])}-bounding-marks-immediate-parent`}
                canvasOffset={this.props.canvasOffset}
                scale={this.props.scale}
                rect={immediateParentBounds}
                boundsType='immediateParent'
              />,
            )
          }

          const coordinateSystemBounds = MetadataUtils.isPositionAbsolute(targetInstance)
            ? specialSizeMeasurements.coordinateSystemBounds
            : specialSizeMeasurements.immediateParentBounds
          if (coordinateSystemBounds != null) {
            boundingMarks.push(
              <BoundingMarks
                key={`${TP.toComponentId(targets[0])}-bounding-marks-coordinate-system`}
                canvasOffset={this.props.canvasOffset}
                scale={this.props.scale}
                rect={coordinateSystemBounds}
                boundsType='coordinateSystem'
              />,
            )
          }
        }
      }
    }
    return boundingMarks
  }

  inSelection(tp: TemplatePath) {
    return this.props.selectedViews.some((et) => TP.pathsEqual(et, tp))
  }

  canResizeElements(): boolean {
    return this.props.selectedViews.every((target) => {
      if (TP.isScenePath(target)) {
        const scene = MetadataUtils.findElementByTemplatePath(this.props.componentMetadata, target)
        let rootHasStyleProp = false
        if (scene != null) {
          rootHasStyleProp = scene.rootElements.some((rootElement) => {
            return this.props.elementsThatRespectLayout.some((path) => {
              return TP.pathsEqual(path, rootElement)
            })
          })
        }
        return MetadataUtils.isSceneTreatedAsGroup(scene) || rootHasStyleProp
      } else {
        return this.props.elementsThatRespectLayout.some((path) => TP.pathsEqual(path, target))
      }
    })
  }

  render() {
    const cmdPressed = this.props.keysPressed['cmd'] || false
    const allElementsDirectlySelectable = cmdPressed && !this.props.isDragging
    const storyboardChildren = MetadataUtils.getAllStoryboardChildren(this.props.componentMetadata)
    const roots = mapDropNulls(
      (child) =>
        MetadataUtils.elementIsOldStyleScene(child) ||
        (isRight(child.element) && isSceneAgainstImports(child.element.value, this.props.imports))
          ? child.templatePath
          : null,
      storyboardChildren,
    )
    let labelDirectlySelectable = this.props.highlightsEnabled

    // TODO future span element should be included here
    let repositionOnly = false
    if (this.props.selectedViews.length === 1 && !TP.isScenePath(this.props.selectedViews[0])) {
      const path = this.props.selectedViews[0]
      const element = MetadataUtils.getElementByInstancePathMaybe(
        this.props.componentMetadata,
        path,
      )
      repositionOnly =
        element != null && MetadataUtils.isAutoSizingText(this.props.imports, element)
    }

    return (
      <div
        data-testid='select-mode-control-container-root'
        style={{
          pointerEvents: 'initial',
          position: 'absolute',
          left: 0,
          top: 0,
          right: 0,
          bottom: 0,
        }}
        onContextMenu={this.onContextMenu}
      >
        {roots.map((root) => {
          return (
            <React.Fragment key={`${TP.toComponentId(root)}}-root-controls`}>
              {this.renderLabel(root, allElementsDirectlySelectable || labelDirectlySelectable)}
            </React.Fragment>
          )
        })}
        {this.props.selectionEnabled ? (
          <>
            <OutlineControls {...this.props} />
            {this.canResizeElements() ? (
              repositionOnly ? (
                <RepositionableControl {...this.props} />
              ) : (
                <>
                  <ConstraintsControls {...this.props} />
                  <YogaControls
                    {...this.props}
                    dragState={
                      this.props.dragState != null &&
                      this.props.dragState.type === 'RESIZE_DRAG_STATE'
                        ? this.props.dragState
                        : null
                    }
                  />
                </>
              )
            ) : null}
          </>
        ) : null}
        {...this.getMoveGuidelines()}
        {this.getDistanceGuidelines()}
        {this.getBoundingMarks()}
      </div>
    )
  }
}
