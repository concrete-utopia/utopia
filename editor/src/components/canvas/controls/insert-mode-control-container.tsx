import React from 'react'
import { FramePoint } from 'utopia-api/core'
import { LayoutHelpers, TopLeftWidthHeight } from '../../../core/layout/layout-helpers'
import {
  getScenePropsOrElementAttributes,
  MetadataUtils,
} from '../../../core/model/element-metadata-utils'
import {
  ElementInstanceMetadataMap,
  emptyComments,
  isJSXElement,
  JSXAttributes,
  jsxAttributeValue,
  JSXElement,
  JSXElementChild,
} from '../../../core/shared/element-template'
import { setJSXValueAtPath } from '../../../core/shared/jsx-attributes'
import { ElementPath, Imports } from '../../../core/shared/project-file-types'
import { Either, eitherToMaybe, isLeft, isRight, right } from '../../../core/shared/either'
import { KeysPressed } from '../../../utils/keyboard'
import Utils from '../../../utils/utils'
import {
  canvasPoint,
  CanvasPoint,
  canvasRectangle,
  CanvasRectangle,
  CanvasVector,
  localRectangle,
} from '../../../core/shared/math-utils'
import { setFocus } from '../../common/actions'
import { EditorAction } from '../../editor/action-types'
import * as EditorActions from '../../editor/actions/action-creators'
import {
  EditorModes,
  elementInsertionSubject,
  ElementInsertionSubject,
  insertionParent,
  insertionSubjectIsJSXElement,
  insertionSubjectIsScene,
  InsertMode,
} from '../../editor/editor-modes'
import * as PP from '../../../core/shared/property-path'
import * as EP from '../../../core/shared/element-path'
import CanvasActions from '../canvas-actions'
import { InsertDragState, insertDragState } from '../canvas-types'
import { GuidelineWithSnappingVectorAndPointsOfRelevance } from '../guideline'
import { ComponentLabelControl } from './component-area-control'
import { GuidelineControl } from './guideline-control'
import {
  applySnappingToPoint,
  collectSelfAndChildrenGuidelines,
  getSnappedGuidelinesForPoint,
} from './guideline-helpers'
import { InsertionControls } from './insertion-control'
import { CanvasControlsContainerID, ControlProps } from './new-canvas-controls'
import { getLayoutPropertyOr } from '../../../core/layout/getLayoutProperty'
import { mapDropNulls, safeIndex } from '../../../core/shared/array-utils'
import { getStoryboardElementPath } from '../../../core/model/scene-utils'
import { isSceneFromMetadata } from '../../../core/model/project-file-utils'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { cancelInsertModeActions } from '../../../components/editor/actions/meta-actions'
import { createInteractionViaMouse } from '../canvas-strategies/interaction-state'
import { emptyModifiers } from '../../../utils/modifiers'
import { CanvasMousePositionRaw } from '../../../utils/global-positions'

const DefaultWidth = 100
const DefaultHeight = 100

interface InsertModeControlContainerProps extends ControlProps {
  mode: InsertMode
  keysPressed: KeysPressed
  dragState: InsertDragState | null
  canvasOffset: CanvasVector
  scale: number
}

interface InsertModeControlContainerState {
  guidelines: Array<GuidelineWithSnappingVectorAndPointsOfRelevance>
  dragFrame: CanvasRectangle | null
  mousePoint: CanvasPoint | null
  aspectRatio: number | null
}

function focusPoint(rect: CanvasRectangle): CanvasPoint {
  return canvasPoint({ x: rect.x + rect.width, y: rect.y + rect.height })
}

function parentIsFlex(
  parentPath: ElementPath | null | undefined,
  componentMetadata: ElementInstanceMetadataMap,
): boolean {
  const parentInstance = MetadataUtils.findElementByElementPath(
    componentMetadata,
    parentPath ?? null,
  )
  return MetadataUtils.isFlexLayoutedContainer(parentInstance)
}

function pathInFlexParent(dragState: InsertDragState, mode: InsertMode): ElementPath | null {
  if (!insertionSubjectIsJSXElement(mode.subject)) {
    return null
  }

  const { parent, uid } = mode.subject
  if (parent == null) {
    return null
  }

  if (!parentIsFlex(parent.target, dragState.metadata)) {
    return null
  }

  return EP.appendToPath(parent.target, uid)
}

type MkDragFrameForFlex = (drag: CanvasVector, isCentered: boolean) => CanvasRectangle

function dragFrameForFlexParent(
  dragState: InsertDragState,
  mode: InsertMode,
): MkDragFrameForFlex | null {
  const path = pathInFlexParent(dragState, mode)
  if (path == null) {
    return null
  }

  const pathString = EP.toString(path)
  const frame = dragState.metadata[pathString]?.globalFrame ?? null
  if (frame == null) {
    return null
  }

  const { x, y } = frame
  const start = canvasPoint({ x, y })
  return (drag, isCentered) => Utils.rectFromPointVector(start, drag, isCentered)
}

function getDragFrame(props: InsertModeControlContainerProps): CanvasRectangle | null {
  if (
    props.mode.insertionStarted &&
    props.dragState != null &&
    props.dragState.type === 'INSERT_DRAG_STATE' &&
    props.dragState.start != null &&
    props.dragState.drag != null
  ) {
    const drag = Utils.forceNotNull('Drag should not be null', props.dragState.drag)
    const isCentered = props.keysPressed['alt'] || false

    const dragFrameForFlex = dragFrameForFlexParent(props.dragState, props.mode)
    if (dragFrameForFlex != null) {
      return dragFrameForFlex(drag, isCentered)
    }

    const start = Utils.forceNotNull('Drag start should not be null', props.dragState.start)
    return Utils.rectFromPointVector(start, drag, isCentered)
  } else {
    return null
  }
}

export class InsertModeControlContainer extends React.Component<
  InsertModeControlContainerProps,
  InsertModeControlContainerState
> {
  constructor(props: InsertModeControlContainerProps) {
    super(props)
    this.state = {
      guidelines: [],
      dragFrame: null,
      mousePoint: null,
      aspectRatio: null,
    }
  }

  static getDerivedStateFromProps(
    props: InsertModeControlContainerProps,
  ): Partial<InsertModeControlContainerState> {
    return {
      dragFrame: getDragFrame(props),
    }
  }

  getParentAttributes = (
    parentPath: ElementPath | null | undefined,
  ): Either<any, JSXAttributes> | null => {
    if (parentPath == null) {
      return null
    } else {
      return getScenePropsOrElementAttributes(parentPath, this.props.componentMetadata)
    }
  }

  onHover = (target: ElementPath) => {
    // The goal of highlight in insert mode is to show which component will be the parent of the
    // newly inserted component. If the user already started to draw the new component, then we should
    // not change the highlights at all, because the parent will remain the same regardless of the
    // actual mouse position
    if (!this.props.mode.insertionStarted) {
      this.props.dispatch([EditorActions.setHighlightedView(target)], 'canvas')
    }
  }

  onHoverEnd = (target: ElementPath) => {
    // See comment in the `onHover` function: we should not change the highlight during insertion
    if (!this.props.mode.insertionStarted) {
      this.props.dispatch([EditorActions.clearHighlightedViews()], 'canvas')
    }
  }

  isHighlighted = (path: ElementPath) => {
    return this.props.highlightedViews.some((highlighted) => EP.pathsEqual(path, highlighted))
  }

  isUtopiaAPIInsertion(
    element: JSXElementChild,
    importsToAdd: Imports,
    elementType: string,
  ): boolean {
    if (importsToAdd['utopia-api'] != null) {
      if (isJSXElement(element)) {
        if (PP.depth(element.name.propertyPath) === 0) {
          return element.name.baseVariable === elementType
        } else {
          return PP.lastPart(element.name.propertyPath) === elementType
        }
      } else {
        return false
      }
    } else {
      return false
    }
  }

  isTextInsertion(element: JSXElementChild, importsToAdd: Imports): boolean {
    return this.isUtopiaAPIInsertion(element, importsToAdd, 'Text')
  }

  isImageInsertion(element: JSXElementChild, importsToAdd: Imports): boolean {
    return this.isUtopiaAPIInsertion(element, importsToAdd, 'Image')
  }

  isViewInsertion(element: JSXElementChild, importsToAdd: Imports): boolean {
    return this.isUtopiaAPIInsertion(element, importsToAdd, 'View')
  }

  renderLabel = (target: ElementPath): JSX.Element | null => {
    const frame = MetadataUtils.getFrameInCanvasCoords(target, this.props.componentMetadata)

    if (frame == null) {
      return null
    }

    return (
      <ComponentLabelControl
        key={EP.toComponentId(target)}
        mouseEnabled={true}
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
        onMouseDown={Utils.NO_OP}
        onHover={this.onHover}
        onHoverEnd={this.onHoverEnd}
        keysPressed={this.props.keysPressed}
        windowToCanvasPosition={this.props.windowToCanvasPosition}
        selectedViews={this.props.selectedViews}
        showAdditionalControls={this.props.showAdditionalControls}
        allElementProps={this.props.allElementProps}
      />
    )
  }

  renderInsertionOutline = (dragFrame: CanvasRectangle) => {
    return (
      <InsertionControls
        frame={dragFrame}
        canvasOffset={this.props.canvasOffset}
        scale={this.props.scale}
      />
    )
  }

  getInsertedElementFrameProps(
    parentPath: ElementPath | null,
    dragFrame: CanvasRectangle,
    allowZeroSize: 'allow-zero-size' | 'set-zero-size-to-default',
  ): TopLeftWidthHeight {
    const parentFrame =
      parentPath == null
        ? null
        : MetadataUtils.getFrameInCanvasCoords(parentPath, this.props.componentMetadata)
    const parentOrigin: CanvasPoint = Utils.defaultIfNull(
      { x: 0, y: 0 } as CanvasPoint,
      parentFrame,
    )
    const localFrame = Utils.getLocalRectangleInNewParentContext(parentOrigin, dragFrame)

    const frameIsZero = localFrame.width === 0 && localFrame.height === 0

    const nonZeroLocalFrame =
      frameIsZero && allowZeroSize === 'set-zero-size-to-default'
        ? localRectangle({
            x: localFrame.x - DefaultWidth / 2,
            y: localFrame.y - DefaultHeight / 2,
            width: DefaultWidth,
            height: DefaultHeight,
          })
        : localFrame

    return {
      [FramePoint.Left]: nonZeroLocalFrame.x,
      [FramePoint.Top]: nonZeroLocalFrame.y,
      [FramePoint.Width]: nonZeroLocalFrame.width,
      [FramePoint.Height]: nonZeroLocalFrame.height,
    }
  }

  elementWithDragFrame = (
    insertionSubject: JSXElement,
    allowZeroSize: 'allow-zero-size' | 'set-zero-size-to-default',
  ): JSXElement => {
    if (this.props.mode.insertionStarted && this.state.dragFrame != null) {
      const attributes = insertionSubject.props
      const frame = this.getInsertedElementFrameProps(
        this.props.highlightedViews[0],
        this.state.dragFrame,
        allowZeroSize,
      )

      const parentAttributes = this.getParentAttributes(this.props.highlightedViews[0])
      const updatedAttributes = LayoutHelpers.updateLayoutPropsWithFrame(
        parentIsFlex(this.props.highlightedViews[0], this.props.componentMetadata),
        parentAttributes,
        attributes,
        frame,
        ['style'],
      )

      if (isLeft(updatedAttributes)) {
        throw new Error(`Problem setting drag frame on an element we just created.`)
      }

      return {
        ...insertionSubject,
        props: updatedAttributes.value,
      }
    } else {
      return insertionSubject
    }
  }

  getImageElementWithSize = (
    allowZeroSize: 'allow-zero-size' | 'set-zero-size-to-default',
  ): JSXElement => {
    if (insertionSubjectIsJSXElement(this.props.mode.subject)) {
      const element = this.props.mode.subject.element
      if (this.props.mode.insertionStarted && this.state.dragFrame != null) {
        const width = getLayoutPropertyOr(0, 'width', right(element.props), ['style'])
        const height = getLayoutPropertyOr(0, 'height', right(element.props), ['style'])
        const canvasFrame = {
          x: this.state.dragFrame.x - width / 2,
          y: this.state.dragFrame.y - height / 2,
          width: width,
          height: height,
        } as CanvasRectangle

        const frame = this.getInsertedElementFrameProps(
          this.props.highlightedViews[0],
          canvasFrame,
          allowZeroSize,
        )

        const attributes = element.props
        const parentAttributes = this.getParentAttributes(this.props.highlightedViews[0])
        const updatedAttributes = LayoutHelpers.updateLayoutPropsWithFrame(
          parentIsFlex(this.props.highlightedViews[0], this.props.componentMetadata),
          parentAttributes,
          attributes,
          frame,
          ['style'],
        )

        if (isLeft(updatedAttributes)) {
          throw new Error(`Problem setting frame on an image we just created.`)
        }

        return {
          ...element,
          props: updatedAttributes.value,
        }
      } else {
        return element
      }
    } else {
      throw new Error(`resize image missing JSXElement`)
    }
  }

  onMouseDown = (e: MouseEvent) => {
    const mousePoint = this.props.windowToCanvasPosition(e).canvasPositionRounded

    const setFocusAction = setFocus('canvas')
    if (
      insertionSubjectIsJSXElement(this.props.mode.subject) &&
      !this.props.mode.insertionStarted
    ) {
      const createInteractionSession = CanvasActions.createInteractionSession(
        createInteractionViaMouse(mousePoint, emptyModifiers, {
          type: 'RESIZE_HANDLE',
          edgePosition: { x: 1, y: 1 },
        }),
      )

      this.props.dispatch([createInteractionSession, setFocusAction], 'everyone')
    }
  }

  renderGuidelines() {
    if (this.state.mousePoint != null) {
      const rectToUse =
        this.state.dragFrame == null
          ? Utils.rectFromPointVector(this.state.mousePoint, { x: 0, y: 0 } as CanvasVector, false)
          : this.state.dragFrame

      return this.state.guidelines.map((g, i) => {
        return (
          <GuidelineControl
            key={
              (g.guideline.type === 'XAxisGuideline' ? 'x' + g.guideline.x : 'y' + g.guideline.y) +
              i
            }
            guidelineWithSnapping={g}
            targetFrame={rectToUse}
            canvasOffset={this.props.canvasOffset}
            scale={this.props.scale}
          />
        )
      })
    } else {
      return null
    }
  }

  componentDidMount() {
    const canvasContainer = document.getElementById(CanvasControlsContainerID)
    if (canvasContainer != null) {
      canvasContainer.addEventListener('mousedown', this.onMouseDown)
    }
  }

  componentWillUnmount() {
    const canvasContainer = document.getElementById(CanvasControlsContainerID)
    if (canvasContainer != null) {
      canvasContainer.removeEventListener('mousedown', this.onMouseDown)
    }
  }

  render() {
    const storyboardChildren = MetadataUtils.getAllStoryboardChildren(this.props.componentMetadata)
    const roots = mapDropNulls((child) => {
      if (isRight(child.element)) {
        const isScene = isSceneFromMetadata(child)
        if (isScene) {
          return child.elementPath
        } else {
          return null
        }
      } else {
        return null
      }
    }, storyboardChildren)
    const dragFrame = this.state.dragFrame

    return (
      <div
        style={{
          pointerEvents: 'none',
          position: 'absolute',
          top: 0,
          left: 0,
          width: '100%',
          height: '100%',
        }}
        data-testid='insert-mode-mouse-catcher'
      >
        {roots.map((root) => this.renderLabel(root))}
        {dragFrame == null ? null : this.renderInsertionOutline(dragFrame)}
        {this.renderGuidelines()}
      </div>
    )
  }
}

function getDefaultAspectRatio(subject: ElementInsertionSubject) {
  if (subject.size != null) {
    return subject.size.width / subject.size.height
  }
  return 1
}

function correctToAspectRatio(
  start: CanvasPoint,
  mouse: CanvasPoint,
  aspectRatio: number,
): CanvasPoint {
  const deltaX = mouse.x - start.x
  const deltaY = mouse.y - start.y
  const signDeltaX = deltaX >= 0 ? 1 : -1
  const signDeltaY = deltaY >= 0 ? 1 : -1
  const maxAbsDelta = Math.max(Math.abs(deltaX), Math.abs(deltaY))

  if (aspectRatio <= 1) {
    return {
      x: start.x + signDeltaX * maxAbsDelta * aspectRatio,
      y: start.y + signDeltaY * maxAbsDelta,
    } as CanvasPoint
  } else {
    return {
      x: start.x + signDeltaX * maxAbsDelta,
      y: start.y + (signDeltaY * maxAbsDelta) / aspectRatio,
    } as CanvasPoint
  }
}
