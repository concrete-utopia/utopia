import * as R from 'ramda'
import * as React from 'react'
import { KeysPressed } from '../../../utils/keyboard'
import Utils from '../../../utils/utils'
import {
  CanvasPoint,
  CanvasRectangle,
  CanvasVector,
  offsetRect,
} from '../../../core/shared/math-utils'
import { TemplatePath } from '../../../core/shared/project-file-types'
import { EditorAction } from '../../editor/action-types'
import * as EditorActions from '../../editor/actions/actions'
import { DuplicationState } from '../../editor/store/editor-state'
import * as TP from '../../../core/shared/template-path'
import { CanvasPositions, MoveDragState, ResizeDragState, moveDragState } from '../canvas-types'
import { Guidelines, Guideline } from '../guideline'
import { ConstraintsControls } from './constraints-control'
import { DistanceGuideline } from './distance-guideline'
import { GuidelineControl } from './guideline-control'
import { collectParentAndSiblingGuidelines, getSnappedGuidelines } from './guideline-helpers'
import { ControlProps } from './new-canvas-controls'
import { ComponentAreaControl, ComponentLabelControl } from './component-area-control'
import { YogaControls } from './yoga-control'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { keepDeepReferenceEqualityIfPossible } from '../../../utils/react-performance'
import { OutlineControls } from './outline-control'
import { RepositionableControl } from './repositionable-control'
import { LeftMenuTab } from '../../navigator/left-pane'
import CanvasActions from '../canvas-actions'
import { getOriginalCanvasFrames, createDuplicationNewUIDs } from '../canvas-utils'
import { areYogaChildren } from './select-mode/yoga-utils'
import { ComponentMetadata } from '../../../core/shared/element-template'
import { BoundingMarks } from './parent-bounding-marks'
import { RightMenuTab } from '../right-menu'

export const SnappingThreshold = 5

function getDistanceGuidelines(
  highlightedView: TemplatePath,
  componentMetadata: ComponentMetadata[],
): Array<Guideline> {
  const frame = MetadataUtils.getFrameInCanvasCoords(highlightedView, componentMetadata)
  if (frame == null) {
    return []
  } else {
    return Guidelines.guidelinesForFrame(frame, false)
  }
}

interface SelectModeControlContainerProps extends ControlProps {
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
    if (this.props.selectedViews.some((view) => TP.pathsEqual(target, view))) {
      return this.props.selectedViews
    } else {
      let updatedSelection = [target]
      if (isMultiselect) {
        updatedSelection = TP.addPathIfMissing(target, this.props.selectedViews)
      }
      let selectActions = [
        EditorActions.clearHighlightedViews(),
        EditorActions.selectComponents(updatedSelection, false),
        EditorActions.setLeftMenuTab(LeftMenuTab.UINavigate),
        EditorActions.setRightMenuTab(RightMenuTab.Inspector),
      ]
      this.props.dispatch(selectActions, 'canvas')
      return updatedSelection
    }
  }

  onControlMouseDown = (
    selectedViews: Array<TemplatePath>,
    target: TemplatePath,
    start: CanvasPoint,
    originalEvent: React.MouseEvent<HTMLDivElement>,
  ) => {
    if (
      // Only on left mouse down.
      originalEvent.buttons === 1 &&
      !MetadataUtils.anyUnknownOrGeneratedElements(
        this.props.rootComponents,
        this.props.componentMetadata,
        selectedViews,
      )
    ) {
      const moveTargets = TP.areAllElementsInSameScene(selectedViews) ? selectedViews : [target]
      // setting original frames
      let originalFrames = getOriginalCanvasFrames(moveTargets, this.props.componentMetadata)
      originalFrames = originalFrames.filter((f) => f.frame != null)
      const selectionArea = Utils.boundingRectangleArray(
        selectedViews.map((view) => {
          return MetadataUtils.getFrameInCanvasCoords(view, this.props.componentMetadata)
        }),
      )

      const duplicate = originalEvent.altKey
      const duplicateNewUIDs = duplicate
        ? createDuplicationNewUIDs(
            this.props.selectedViews,
            this.props.componentMetadata,
            this.props.rootComponents,
          )
        : null

      this.props.dispatch([
        CanvasActions.createDragState(
          moveDragState(
            start,
            null,
            null,
            originalFrames,
            selectionArea,
            !originalEvent.metaKey,
            originalEvent.shiftKey,
            duplicate,
            originalEvent.metaKey,
            duplicateNewUIDs,
            start,
            this.props.componentMetadata,
            moveTargets,
          ),
        ),
      ])
    }
  }

  onContextMenu = (event: React.MouseEvent<HTMLDivElement>) => {
    event.stopPropagation()
    event.preventDefault()
    this.props.dispatch(
      [EditorActions.showContextMenu('context-menu-canvas', event.nativeEvent)],
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

  filterHiddenInstances = (paths: Array<TemplatePath>): Array<TemplatePath> => {
    return paths.filter((path) =>
      this.props.hiddenInstances.every((hidden) => !TP.pathsEqual(path, hidden)),
    )
  }

  isHighlighted = (path: TemplatePath) => {
    return this.props.highlightedViews.some((highlighted) => TP.pathsEqual(path, highlighted))
  }

  getSelectableViews(allElementsDirectlySelectable: boolean): TemplatePath[] {
    let candidateViews: Array<TemplatePath>

    if (allElementsDirectlySelectable) {
      candidateViews = MetadataUtils.getAllPaths(this.props.componentMetadata)
    } else {
      const allRoots = MetadataUtils.getAllCanvasRootPaths(this.props.componentMetadata)
      let siblings: Array<TemplatePath> = []
      Utils.fastForEach(this.props.selectedViews, (view) => {
        Utils.fastForEach(TP.allPaths(view), (ancestor) => {
          const ancestorChildren = MetadataUtils.getImmediateChildren(
            this.props.componentMetadata,
            ancestor,
          )

          siblings.push(...ancestorChildren.map((child) => child.templatePath))
        })
      })

      const selectableViews = [...allRoots, ...siblings]
      const uniqueSelectableViews = R.uniqWith<TemplatePath>(TP.pathsEqual, selectableViews)

      const selectableViewsFiltered = uniqueSelectableViews.filter((view) => {
        // I kept the group-like behavior here that the user can't single-click select the parent group, even though it is a view now
        const isGroup = MetadataUtils.isAutoSizingViewFromComponents(
          this.props.componentMetadata,
          view,
        )
        const isAncestorOfSelected = this.props.selectedViews.some((selectedView) =>
          TP.isAncestorOf(selectedView, view, false),
        )
        if (isGroup && isAncestorOfSelected) {
          return false
        } else {
          return true
        }
      })
      candidateViews = selectableViewsFiltered
    }

    return this.filterHiddenInstances(candidateViews)
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

  renderControl = (target: TemplatePath, index: number, isChild: boolean): JSX.Element | null => {
    const frame = this.getClippedArea(target)
    if (frame != null) {
      return (
        <ComponentAreaControl
          key={`${TP.toComponentId(target)}-${index}-control`}
          testID={`component-area-control-${TP.toComponentId(target)}-${index}`}
          componentMetadata={this.props.componentMetadata}
          target={target}
          frame={frame}
          scale={this.props.scale}
          highlighted={this.isHighlighted(target)}
          selectedComponents={this.props.selectedViews}
          dispatch={this.props.dispatch}
          canvasOffset={this.props.canvasOffset}
          hoverEffectEnabled={!isChild}
          doubleClickToSelect={isChild}
          selectComponent={this.selectComponent}
          onMouseDown={this.onControlMouseDown}
          onHover={this.onHover}
          onHoverEnd={this.onHoverEnd}
          keysPressed={this.props.keysPressed}
          windowToCanvasPosition={this.props.windowToCanvasPosition}
          selectedViews={this.props.selectedViews}
          imports={this.props.imports}
        />
      )
    } else {
      return null
    }
  }

  renderLabel = (target: TemplatePath, hoverEnabled: boolean): JSX.Element | null => {
    const frame = MetadataUtils.getFrameInCanvasCoords(target, this.props.componentMetadata)
    if (frame == null) {
      return null
    }
    return (
      <ComponentLabelControl
        key={`${TP.toComponentId(target)}-label`}
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
      />
    )
  }

  clearSelection = () => {
    if (this.props.selectionEnabled) {
      this.props.dispatch([EditorActions.clearSelection()], 'canvas')
    }
  }

  renderNonSelectableControl = (target: TemplatePath): JSX.Element | null => {
    const frame = MetadataUtils.getFrameInCanvasCoords(target, this.props.componentMetadata)
    if (frame == null) {
      return null
    }
    return (
      <ComponentAreaControl
        key={`${TP.toComponentId(target)}-non-selectable`}
        componentMetadata={this.props.componentMetadata}
        target={target}
        frame={frame}
        scale={this.props.scale}
        highlighted={this.isHighlighted(target)}
        selectedComponents={this.props.selectedViews}
        dispatch={this.props.dispatch}
        canvasOffset={this.props.canvasOffset}
        hoverEffectEnabled={true}
        doubleClickToSelect={false}
        onMouseDown={this.clearSelection}
        onHover={Utils.NO_OP}
        onHoverEnd={Utils.NO_OP}
        keysPressed={this.props.keysPressed}
        windowToCanvasPosition={this.props.windowToCanvasPosition}
        selectedViews={this.props.selectedViews}
        imports={this.props.imports}
      />
    )
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

  render() {
    const cmdPressed = this.props.keysPressed['cmd'] || false
    const allElementsDirectlySelectable = cmdPressed && !this.props.isDragging
    const roots = MetadataUtils.getAllScenePaths(this.props.componentMetadata)
    let labelDirectlySelectable = true
    let draggableViews = this.getSelectableViews(allElementsDirectlySelectable)
    if (!this.props.highlightsEnabled) {
      draggableViews = []
      labelDirectlySelectable = false
    }

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
        style={{
          pointerEvents: 'initial',
        }}
        onContextMenu={this.onContextMenu}
      >
        {roots.map((root) => {
          return (
            <React.Fragment key={TP.toComponentId(root)}>
              {this.renderLabel(root, allElementsDirectlySelectable || labelDirectlySelectable)}
              {this.renderNonSelectableControl(root)}
            </React.Fragment>
          )
        })}
        {this.props.selectionEnabled
          ? draggableViews.map((draggableView, index) => {
              if (
                !allElementsDirectlySelectable &&
                this.props.selectedViews.some((view) =>
                  TP.pathsEqual(TP.parentPath(draggableView), view),
                )
              ) {
                // only double clickable to select and drag
                return this.renderControl(draggableView, index, true)
              } else {
                // directly draggable
                return this.renderControl(draggableView, index, false)
              }
            })
          : null}
        {this.props.selectionEnabled ? (
          <>
            <OutlineControls {...this.props} />
            {repositionOnly ? (
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
            )}
          </>
        ) : null}
        {...this.getMoveGuidelines()}
        {this.getDistanceGuidelines()}
        {this.getBoundingMarks()}
      </div>
    )
  }
}
