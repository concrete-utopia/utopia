import * as React from 'react'
import { FramePoint } from 'utopia-api'
import { LayoutHelpers, TopLeftWidthHeight } from '../../../core/layout/layout-helpers'
import {
  getScenePropsOrElementAttributes,
  MetadataUtils,
} from '../../../core/model/element-metadata-utils'
import {
  isJSXElement,
  JSXAttribute,
  JSXAttributes,
  jsxAttributeValue,
  JSXElement,
  JSXElementChild,
} from '../../../core/shared/element-template'
import { setJSXValueAtPath } from '../../../core/shared/jsx-attributes'
import { PropertyPath, TemplatePath, Imports } from '../../../core/shared/project-file-types'
import { Either, eitherToMaybe, isLeft, isRight, right } from '../../../core/shared/either'
import { KeysPressed } from '../../../utils/keyboard'
import Utils from '../../../utils/utils'
import { CanvasPoint, CanvasRectangle, CanvasVector } from '../../../core/shared/math-utils'
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
  insertionSubjectIsDragAndDrop,
} from '../../editor/editor-modes'
import { LeftMenuTab } from '../../navigator/left-pane'
import * as PP from '../../../core/shared/property-path'
import * as TP from '../../../core/shared/template-path'
import CanvasActions from '../canvas-actions'
import { CanvasContainerID, InsertDragState, insertDragState } from '../canvas-types'
import { GuidelineWithSnappingVector } from '../guideline'
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
import { RightMenuTab } from '../right-menu'
import { mapDropNulls, safeIndex } from '../../../core/shared/array-utils'
import { createLayoutPropertyPath } from '../../../core/layout/layout-helpers-new'
import { getStoryboardTemplatePath } from '../../../core/model/scene-utils'
import { emptyComments } from '../../../core/workers/parser-printer/parser-printer-comments'
import { isSceneAgainstImports } from '../../../core/model/project-file-utils'

// I feel comfortable having this function confined to this file only, since we absolutely shouldn't be trying
// to set values that would fail whilst inserting elements. If that ever changes, this function should be binned
// and we should handle those failures explicitly
function forceSetValueAtPath(
  attributes: JSXAttributes,
  path: PropertyPath,
  value: JSXAttribute,
): JSXAttributes {
  const result = setJSXValueAtPath(attributes, path, value)
  if (isLeft(result)) {
    throw new Error(`Failed to set value at path ${PP.toString(path)}: ${result.value}`)
  } else {
    return result.value
  }
}

interface InsertModeControlContainerProps extends ControlProps {
  mode: InsertMode
  keysPressed: KeysPressed
  projectId: string | null
  dragState: InsertDragState | null
  canvasOffset: CanvasVector
  scale: number
}

interface InsertModeControlContainerState {
  guidelines: Array<GuidelineWithSnappingVector>
  dragFrame: CanvasRectangle | null
  mousePoint: CanvasPoint | null
  aspectRatio: number | null
}

function getDragFrame(props: InsertModeControlContainerProps): CanvasRectangle | null {
  if (
    props.mode.insertionStarted &&
    props.dragState != null &&
    props.dragState.type === 'INSERT_DRAG_STATE' &&
    props.dragState.start != null &&
    props.dragState.drag != null
  ) {
    const start = Utils.forceNotNull('Drag start should not be null', props.dragState.start)
    const drag = Utils.forceNotNull('Drag should not be null', props.dragState.drag)
    return Utils.rectFromPointVector(start, drag, props.keysPressed['alt'] || false)
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

  static getDerivedStateFromProps(props: InsertModeControlContainerProps) {
    return {
      dragFrame: getDragFrame(props),
    }
  }

  getParentAttributes = (
    parentPath: TemplatePath | null | undefined,
  ): Either<any, JSXAttributes> | null => {
    if (parentPath == null) {
      return null
    } else {
      return getScenePropsOrElementAttributes(parentPath, this.props.componentMetadata)
    }
  }

  parentIsFlex = (parentPath: TemplatePath | null | undefined): boolean => {
    const parentInstance =
      parentPath != null && TP.isInstancePath(parentPath)
        ? MetadataUtils.getElementByInstancePathMaybe(this.props.componentMetadata, parentPath)
        : null
    return MetadataUtils.isFlexLayoutedContainer(parentInstance)
  }

  onHover = (target: TemplatePath) => {
    // The goal of highlight in insert mode is to show which component will be the parent of the
    // newly inserted component. If the user already started to draw the new component, then we should
    // not change the highlights at all, because the parent will remain the same regardless of the
    // actual mouse position
    if (!this.props.mode.insertionStarted) {
      this.props.dispatch([EditorActions.setHighlightedView(target)], 'canvas')
    }
  }

  onHoverEnd = (target: TemplatePath) => {
    // See comment in the `onHover` function: we should not change the highlight during insertion
    if (!this.props.mode.insertionStarted) {
      this.props.dispatch([EditorActions.clearHighlightedViews()], 'canvas')
    }
  }

  isHighlighted = (path: TemplatePath) => {
    return this.props.highlightedViews.some((highlighted) => TP.pathsEqual(path, highlighted))
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

  renderLabel = (target: TemplatePath): JSX.Element | null => {
    const frame = MetadataUtils.getFrameInCanvasCoords(target, this.props.componentMetadata)

    if (frame == null) {
      return null
    }

    return (
      <ComponentLabelControl
        key={TP.toComponentId(target)}
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
        imports={this.props.imports}
        showAdditionalControls={this.props.showAdditionalControls}
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
    parentPath: TemplatePath | null,
    dragFrame: CanvasRectangle,
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

    return {
      [FramePoint.Left]: localFrame.x,
      [FramePoint.Top]: localFrame.y,
      [FramePoint.Width]: localFrame.width,
      [FramePoint.Height]: localFrame.height,
    }
  }

  elementWithDragFrame = (insertionSubject: JSXElement): JSXElement => {
    if (this.props.mode.insertionStarted && this.state.dragFrame != null) {
      const attributes = insertionSubject.props
      const frame = this.getInsertedElementFrameProps(
        this.props.highlightedViews[0],
        this.state.dragFrame,
      )

      const parentAttributes = this.getParentAttributes(this.props.highlightedViews[0])
      const updatedAttributes = LayoutHelpers.updateLayoutPropsWithFrame(
        this.parentIsFlex(this.props.highlightedViews[0]),
        parentAttributes,
        attributes,
        frame,
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

  getImageElementWithSize = (): JSXElement => {
    if (insertionSubjectIsJSXElement(this.props.mode.subject)) {
      const element = this.props.mode.subject.element
      if (this.props.mode.insertionStarted && this.state.dragFrame != null) {
        const width = getLayoutPropertyOr(0, 'Width', right(element.props))
        const height = getLayoutPropertyOr(0, 'Height', right(element.props))
        const canvasFrame = {
          x: this.state.dragFrame.x - width / 2,
          y: this.state.dragFrame.y - height / 2,
          width: width,
          height: height,
        } as CanvasRectangle

        const frame = this.getInsertedElementFrameProps(this.props.highlightedViews[0], canvasFrame)

        const attributes = element.props
        const parentAttributes = this.getParentAttributes(this.props.highlightedViews[0])
        const updatedAttributes = LayoutHelpers.updateLayoutPropsWithFrame(
          this.parentIsFlex(this.props.highlightedViews[0]),
          parentAttributes,
          attributes,
          frame,
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

  setTextElementFixedSize = (element: JSXElement): JSXElement => {
    const attributes = element.props
    const updatedAttributes = forceSetValueAtPath(
      attributes,
      PP.create(['textSizing']),
      jsxAttributeValue('fixed', emptyComments),
    )

    return {
      ...element,
      props: updatedAttributes,
    }
  }

  getTextElementAutoSizeWithPosition(
    insertionSubject: JSXElement,
    mousePosition: CanvasPoint,
  ): JSXElement {
    const attributes = insertionSubject.props
    const globalFrameAsRectangle = {
      x: mousePosition.x,
      y: mousePosition.y,
      width: NaN,
      height: 16,
    } as CanvasRectangle
    const frame = this.getInsertedElementFrameProps(
      this.props.highlightedViews[0],
      globalFrameAsRectangle,
    )

    const parentAttributes = this.getParentAttributes(this.props.highlightedViews[0])
    const updatedAttributes = LayoutHelpers.updateLayoutPropsWithFrame(
      this.parentIsFlex(this.props.highlightedViews[0]),
      parentAttributes,
      attributes,
      frame,
    )

    if (isLeft(updatedAttributes)) {
      throw new Error(`Problem setting frame on an element we just created.`)
    }

    const updatedAttributes2 = forceSetValueAtPath(
      updatedAttributes.value,
      PP.create(['textSizing']),
      jsxAttributeValue('auto', emptyComments),
    )

    return {
      ...insertionSubject,
      props: updatedAttributes2,
    }
  }

  onMouseDown = (e: MouseEvent) => {
    const mousePoint = this.props.windowToCanvasPosition(e).canvasPositionRounded
    const snappedMousePoint = applySnappingToPoint(mousePoint, this.state.guidelines)
    const updateDragStateAction = CanvasActions.createDragState(
      insertDragState(
        snappedMousePoint,
        { x: 0, y: 0 } as CanvasVector,
        this.props.componentMetadata,
      ),
    )
    const setFocusAction = setFocus('canvas')

    if (
      insertionSubjectIsJSXElement(this.props.mode.subject) &&
      !this.props.mode.insertionStarted
    ) {
      const insertionSubject = this.props.mode.subject
      const parent = safeIndex(this.props.highlightedViews, 0) ?? null
      const staticParent = MetadataUtils.templatePathToStaticTemplatePath(parent)

      let { element } = this.props.mode.subject
      if (this.isTextInsertion(element, insertionSubject.importsToAdd)) {
        element = this.getTextElementAutoSizeWithPosition(element, snappedMousePoint)
      } else if (this.parentIsFlex(this.props.highlightedViews[0])) {
        element = {
          ...element,
          props:
            eitherToMaybe(
              setJSXValueAtPath(
                element.props,
                createLayoutPropertyPath('position'),
                jsxAttributeValue('relative', emptyComments),
              ),
            ) ?? element.props,
        }
      }
      this.props.dispatch(
        [
          EditorActions.updateEditorMode(
            EditorModes.insertMode(
              true,
              elementInsertionSubject(
                insertionSubject.uid,
                element,
                insertionSubject.size,
                insertionSubject.importsToAdd,
                insertionParent(parent, staticParent),
              ),
            ),
          ),
          updateDragStateAction,
          setFocusAction,
        ],
        'everyone',
      )
    } else {
      this.props.dispatch(
        [
          EditorActions.updateEditorMode({
            ...this.props.mode,
            insertionStarted: true,
          }),
          updateDragStateAction,
          setFocusAction,
        ],
        'everyone',
      )
    }
  }

  onMouseUp = (event: MouseEvent) => {
    if (!this.props.mode.insertionStarted) {
      return
    }

    const baseActions: EditorAction[] = [
      EditorActions.updateEditorMode(EditorModes.selectMode()),
      EditorActions.setRightMenuTab(RightMenuTab.Inspector),
      EditorActions.clearHighlightedViews(),
      CanvasActions.clearDragState(false),
    ]

    if (insertionSubjectIsJSXElement(this.props.mode.subject)) {
      const insertionSubject = this.props.mode.subject
      const insertionElement = insertionSubject.element
      let element = null
      const parentPath =
        safeIndex(this.props.highlightedViews, 0) ??
        getStoryboardTemplatePath(this.props.rootComponents)
      let extraActions: EditorAction[] = []

      if (
        this.props.dragState != null &&
        this.props.dragState.drag != null &&
        Utils.distance(this.props.dragState.drag, Utils.zeroPoint as CanvasPoint) === 0
      ) {
        // image and text insertion with single click
        if (
          this.isImageInsertion(insertionElement, insertionSubject.importsToAdd) &&
          this.state.dragFrame != null
        ) {
          element = this.getImageElementWithSize()
        } else if (this.isTextInsertion(insertionElement, insertionSubject.importsToAdd)) {
          element = insertionElement
        }
      } else {
        // TODO Hidden Instances
        element = this.elementWithDragFrame(insertionElement)
      }

      if (this.isTextInsertion(insertionElement, insertionSubject.importsToAdd)) {
        if (parentPath != null) {
          const path = TP.appendToPath(parentPath, insertionSubject.uid)
          extraActions.push(EditorActions.openTextEditor(path, null))
        }
      }

      if (element == null) {
        this.props.dispatch(baseActions, 'everyone')
      } else {
        this.props.dispatch(
          [
            EditorActions.insertJSXElement(
              element,
              parentPath ?? null,
              insertionSubject.importsToAdd,
            ),
            ...baseActions,
            ...extraActions,
          ],
          'everyone',
        )
      }
    } else if (insertionSubjectIsScene(this.props.mode.subject)) {
      const actions =
        this.state.dragFrame == null
          ? baseActions
          : baseActions.concat(EditorActions.insertScene(this.state.dragFrame))
      this.props.dispatch(actions, 'everyone')
    } else {
      this.props.dispatch(baseActions, 'everyone')
    }
  }

  onMouseMove = (e: MouseEvent) => {
    const mousePoint = this.props.windowToCanvasPosition(e).canvasPositionRounded
    const keepAspectRatio = this.props.keysPressed['shift'] || false

    if (insertionSubjectIsJSXElement(this.props.mode.subject)) {
      const insertionSubject = this.props.mode.subject
      const guidelines = collectSelfAndChildrenGuidelines(
        this.props.componentMetadata,
        this.props.highlightedViews,
        this.props.mode.subject.uid,
      )

      const closestGuidelines = getSnappedGuidelinesForPoint(
        guidelines,
        null,
        mousePoint,
        this.props.scale,
      )

      if (
        this.props.mode.insertionStarted &&
        this.props.dragState != null &&
        this.props.dragState.start != null
      ) {
        const parent = this.props.highlightedViews[0]
        const staticParent = MetadataUtils.templatePathToStaticTemplatePath(parent)
        let element = this.elementWithDragFrame(insertionSubject.element)
        if (this.isTextInsertion(element, insertionSubject.importsToAdd)) {
          element = this.setTextElementFixedSize(element)
        }

        const aspectRatioCorrectedMousePoint = this.state.aspectRatio
          ? correctToAspectRatio(this.props.dragState.start, mousePoint, this.state.aspectRatio)
          : mousePoint
        const snappedMousePoint = applySnappingToPoint(
          aspectRatioCorrectedMousePoint,
          closestGuidelines,
        )
        const dragVector = Utils.vectorFromPoints(this.props.dragState.start, snappedMousePoint)
        this.props.dispatch(
          [
            EditorActions.updateEditorMode(
              EditorModes.insertMode(
                true,
                elementInsertionSubject(
                  insertionSubject.uid,
                  element,
                  insertionSubject.size,
                  insertionSubject.importsToAdd,
                  insertionParent(parent, staticParent),
                ),
              ),
            ),
            CanvasActions.createDragState(
              insertDragState(this.props.dragState.start, dragVector, this.props.componentMetadata),
            ),
          ],
          'everyone',
        )
      }

      this.setState({
        guidelines: closestGuidelines,
        mousePoint: mousePoint,
        aspectRatio: keepAspectRatio ? getDefaultAspectRatio(this.props.mode.subject) : null,
      })
    } else if (
      this.props.mode.insertionStarted &&
      this.props.dragState != null &&
      this.props.dragState.start != null
    ) {
      const dragVector = Utils.vectorFromPoints(this.props.dragState.start, mousePoint)
      this.props.dispatch(
        [
          CanvasActions.createDragState(
            insertDragState(this.props.dragState.start, dragVector, this.props.componentMetadata),
          ),
        ],
        'everyone',
      )

      this.setState({
        mousePoint: mousePoint,
      })
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
      canvasContainer.addEventListener('mousemove', this.onMouseMove)
      canvasContainer.addEventListener('mouseup', this.onMouseUp)
    }
  }

  componentWillUnmount() {
    const canvasContainer = document.getElementById(CanvasControlsContainerID)
    if (canvasContainer != null) {
      canvasContainer.removeEventListener('mousedown', this.onMouseDown)
      canvasContainer.removeEventListener('mousemove', this.onMouseMove)
      canvasContainer.removeEventListener('mouseup', this.onMouseUp)
    }
  }

  render() {
    const storyboardChildren = MetadataUtils.getAllStoryboardChildren(this.props.componentMetadata)
    const roots = mapDropNulls(
      (child) =>
        MetadataUtils.elementIsOldStyleScene(child) ||
        (isRight(child.element) && isSceneAgainstImports(child.element.value, this.props.imports))
          ? child.templatePath
          : null,
      storyboardChildren,
    )
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
