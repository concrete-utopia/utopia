/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import Utils from '../../../utils/utils'
import { CanvasPoint, CanvasRectangle, point, windowPoint } from '../../../core/shared/math-utils'
import { EditorDispatch } from '../../editor/action-types'
import { setResizeOptionsTargetOptions } from '../../editor/actions/action-creators'
import { ControlFontSize } from '../canvas-controls-frame'
import {
  CSSCursor,
  ResizeDragState,
  resizeDragState,
  CanvasPositions,
  EdgePosition,
  EnabledDirection,
  DirectionVertical,
  DirectionHorizontal,
  DirectionAll,
  DragState,
  updateResizeDragState,
} from '../canvas-types'
import { ResizeStatus } from './new-canvas-controls'
import { ElementPath } from '../../../core/shared/project-file-types'
import CanvasActions from '../canvas-actions'
import {
  ElementProps,
  OriginalCanvasAndLocalFrame,
  ResizeOptions,
} from '../../editor/store/editor-state'
import {
  DetectedLayoutSystem,
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../../core/shared/element-template'
import { isFeatureEnabled } from '../../../utils/feature-switches'
//TODO: switch to functional component and make use of 'useColorTheme':
import { colorTheme as fixmeColorTheme, useColorTheme } from '../../../uuiui'
import { LayoutTargetableProp } from '../../../core/layout/layout-helpers-new'
import { PropertyTargetSelector } from './property-target-selector'
import { unless, when } from '../../../utils/react-conditionals'
import { isZeroSizedElement } from './outline-utils'
import { ZeroSizeResizeControl } from './zero-sized-element-controls'
import {
  anyDragStarted,
  getDragStateStart,
  getResizeOptions,
  isTargetPropertyHorizontal,
} from '../canvas-utils'
import { safeIndex } from '../../../core/shared/array-utils'
import { CSSPosition } from '../../inspector/common/css-utils'
import * as EP from '../../../core/shared/element-path'
import { createInteractionViaMouse } from '../canvas-strategies/interaction-state'
import { Modifier } from '../../../utils/modifiers'
import { windowToCanvasCoordinates } from '../dom-lookup'

interface ResizeControlProps extends ResizeRectangleProps {
  cursor: CSSCursor
  position: EdgePosition
  enabledDirection: EnabledDirection
  selectedViews: Array<ElementPath>
  dragState: ResizeDragState | null
  propertyTargetSelectedIndex: number
}

function layoutSystemForPositionOrFlex(
  position: CSSPosition | null | undefined,
  flexDirection: 'horizontal' | 'vertical' | null,
): 'flex-horizontal' | 'flex-vertical' | 'absolute' | null {
  if (position === 'absolute') {
    return 'absolute'
  } else if (flexDirection === 'horizontal') {
    return 'flex-horizontal'
  } else if (flexDirection === 'vertical') {
    return 'flex-vertical'
  } else {
    return null
  }
}

class ResizeControl extends React.Component<ResizeControlProps> {
  reference = React.createRef<HTMLDivElement>()
  constructor(props: ResizeControlProps) {
    super(props)
  }

  onMouseDown = (event: React.MouseEvent<HTMLDivElement>) => {
    event.stopPropagation()
    if (event.buttons === 1) {
      const beforeOrAfter =
        this.props.position.y === 0.5 ? this.props.position.x : this.props.position.y
      const edge = beforeOrAfter === 0 ? 'before' : 'after'
      const centerBasedResize = event.altKey
      const keepAspectRatio = event.shiftKey || this.props.elementAspectRatioLocked
      const enableSnapping = !event.metaKey
      const canvasPositions = this.props.windowToCanvasPosition(event.nativeEvent)
      const start: CanvasPoint = canvasPositions.canvasPositionRaw
      const originalFrames = this.props.getOriginalFrames()
      const isMultiSelect = this.props.selectedViews.length !== 1
      const enabledDirection = this.props.enabledDirection
      let propertyTargetOptions: Array<LayoutTargetableProp> = []
      const layoutSystem = layoutSystemForPositionOrFlex(
        this.props.targetComponentMetadata?.specialSizeMeasurements.position,
        this.props.flexDirection,
      )
      if (enabledDirection.x === 1 && enabledDirection.y === 0) {
        // Left to right resize.
        propertyTargetOptions = getResizeOptions(layoutSystem, 'vertical', edge)
      } else if (enabledDirection.x === 0 && enabledDirection.y === 1) {
        // Up to down resize.
        propertyTargetOptions = getResizeOptions(layoutSystem, 'horizontal', edge)
      } else {
        // Diagonal resize of some kind.
      }
      const targetProperty = safeIndex(
        propertyTargetOptions,
        this.props.propertyTargetSelectedIndex,
      )
      if (isFeatureEnabled('Canvas Strategies')) {
        const startPoint = windowToCanvasCoordinates(
          this.props.scale,
          this.props.canvasOffset,
          windowPoint(point(event.clientX, event.clientY)),
        ).canvasPositionRounded

        if (event.button !== 2) {
          this.props.dispatch(
            [
              CanvasActions.createInteractionSession(
                createInteractionViaMouse(startPoint, Modifier.modifiersForEvent(event), {
                  type: 'RESIZE_HANDLE',
                  edgePosition: this.props.position,
                }),
              ),
            ],
            'everyone',
          )
        }
      } else {
        const newDragState = updateResizeDragState(
          resizeDragState(
            this.props.measureSize,
            originalFrames,
            this.props.position,
            this.props.enabledDirection,
            this.props.metadata,
            this.props.selectedViews,
            isMultiSelect,
            [],
          ),
          start,
          null,
          targetProperty,
          enableSnapping,
          centerBasedResize,
          keepAspectRatio,
        )

        this.props.dispatch(
          [
            CanvasActions.createDragState(newDragState),
            setResizeOptionsTargetOptions(
              propertyTargetOptions,
              this.props.propertyTargetSelectedIndex,
            ),
          ],
          'canvas',
        )
      }
      this.props.onResizeStart(this.props.measureSize, this.props.position)
    }
  }

  onMouseMove = (event: React.MouseEvent<any>) => {
    this.props.maybeClearHighlightsOnHoverEnd()
    event.stopPropagation()
  }

  render() {
    return (
      <React.Fragment>
        {this.props.resizeStatus === 'enabled' ? (
          <div onMouseDown={this.onMouseDown} onMouseMove={this.onMouseMove}>
            {this.props.children}
          </div>
        ) : (
          this.props.children
        )}
      </React.Fragment>
    )
  }
}

interface ResizeEdgeProps {
  dispatch: EditorDispatch
  cursor: CSSCursor
  direction: 'horizontal' | 'vertical'
  canvasOffset: CanvasPoint
  visualSize: CanvasRectangle
  scale: number
  position: EdgePosition
  resizeStatus: ResizeStatus
  targetComponentMetadata: ElementInstanceMetadata | null
  dragState: DragState | null
  targetProps: ElementProps | null
}

interface ResizeEdgeState {}

function allowsInteractiveResize(layoutSystem: DetectedLayoutSystem): boolean {
  switch (layoutSystem) {
    case 'flex':
    case 'flow':
      return true
    case 'grid':
    case 'none':
      return false
    default:
      const _exhaustiveCheck: never = layoutSystem
      throw new Error(`Unhandled layoutSystem ${JSON.stringify(layoutSystem)}`)
  }
}

function shiftPropertyTargetSelectorAxis(
  primaryDirection: 'horizontal' | 'vertical',
  actualDirection: 'horizontal' | 'vertical',
  edge: 'before' | 'after',
): number {
  if (actualDirection === primaryDirection) {
    if (edge === 'before') {
      return 25
    } else {
      return 10
    }
  } else {
    return 10
  }
}

class ResizeEdge extends React.Component<ResizeEdgeProps, ResizeEdgeState> {
  constructor(props: ResizeEdgeProps) {
    super(props)
  }
  reference = React.createRef<HTMLDivElement>()

  render() {
    if (this.props.resizeStatus != 'enabled') {
      return null
    }
    const beforeOrAfter =
      this.props.position.y === 0.5 ? this.props.position.x : this.props.position.y
    const edge = beforeOrAfter === 0 ? 'before' : 'after'
    const baseLeft =
      this.props.canvasOffset.x +
      this.props.visualSize.x +
      this.props.position.x * this.props.visualSize.width
    const baseTop =
      this.props.canvasOffset.y +
      this.props.visualSize.y +
      this.props.position.y * this.props.visualSize.height

    const lineSize = 10 / this.props.scale
    const width = this.props.direction === 'horizontal' ? this.props.visualSize.width : lineSize
    const height = this.props.direction === 'vertical' ? this.props.visualSize.height : lineSize
    const left =
      baseLeft +
      (this.props.direction === 'horizontal' ? -this.props.visualSize.width / 2 : -lineSize / 2)
    const top =
      baseTop +
      (this.props.direction === 'vertical' ? -this.props.visualSize.height / 2 : -lineSize / 2)

    const displayResizeSelector =
      this.props.dragState != null &&
      this.props.dragState.type === 'RESIZE_DRAG_STATE' &&
      anyDragStarted(this.props.dragState) &&
      this.props.dragState.edgePosition.x === this.props.position.x &&
      this.props.dragState.edgePosition.y === this.props.position.y

    const interactiveResize =
      this.props.targetComponentMetadata != null &&
      allowsInteractiveResize(
        this.props.targetComponentMetadata.specialSizeMeasurements.parentLayoutSystem,
      )

    const layoutSystem = layoutSystemForPositionOrFlex(
      this.props.targetComponentMetadata?.specialSizeMeasurements.position,
      null,
    )

    return (
      <React.Fragment>
        <div
          ref={this.reference}
          style={{
            pointerEvents: 'initial',
            position: 'absolute',
            left: left,
            top: top,
            width: width,
            height: height,
            boxSizing: 'border-box',
            backgroundColor: 'transparent',
            cursor: this.props.resizeStatus === 'enabled' ? this.props.cursor : undefined,
          }}
        />
        {when(
          displayResizeSelector && interactiveResize,
          <PropertyTargetSelector
            top={top + shiftPropertyTargetSelectorAxis('horizontal', this.props.direction, edge)}
            left={left + shiftPropertyTargetSelectorAxis('vertical', this.props.direction, edge)}
            options={getResizeOptions(layoutSystem, this.props.direction, edge)}
            targetComponentMetadata={this.props.targetComponentMetadata}
            targetProps={this.props.targetProps}
            key={
              this.props.targetComponentMetadata != null
                ? EP.toString(this.props.targetComponentMetadata.elementPath)
                : `${this.props.direction}-${this.props.position.x}-${this.props.position.y}`
            }
          />,
        )}
      </React.Fragment>
    )
  }
}

interface ResizeLinesProps {
  targetComponentMetadata: ElementInstanceMetadata | null
  cursor: CSSCursor
  direction: 'horizontal' | 'vertical'
  canvasOffset: CanvasPoint
  visualSize: CanvasRectangle
  scale: number
  position: EdgePosition
  resizeStatus: ResizeStatus
  dragState: DragState | null
  color?: string
  flexDirection: 'horizontal' | 'vertical' | null
  targetProps: ElementProps | null
}

const LineOffset = 6
const ResizeLines = React.memo((props: ResizeLinesProps) => {
  const reference = React.createRef<HTMLDivElement>()
  const LineSVGComponent =
    props.position.y === 0.5 ? DimensionableControlVertical : DimensionableControlHorizontal

  const beforeOrAfter = props.position.y === 0.5 ? props.position.x : props.position.y
  const edge = beforeOrAfter === 0 ? 'before' : 'after'

  const displayResizeSelector =
    props.dragState != null &&
    props.dragState.type === 'RESIZE_DRAG_STATE' &&
    anyDragStarted(props.dragState) &&
    props.dragState.edgePosition.x === props.position.x &&
    props.dragState.edgePosition.y === props.position.y

  const left = props.canvasOffset.x + props.visualSize.x + props.position.x * props.visualSize.width
  const top = props.canvasOffset.y + props.visualSize.y + props.position.y * props.visualSize.height

  const catchmentSize = 12 / props.scale

  const mouseCatcher =
    props.resizeStatus !== 'enabled' ? null : (
      <div
        ref={reference}
        style={{
          pointerEvents: 'initial',
          position: 'absolute',
          width: catchmentSize,
          height: catchmentSize,
          top: top - catchmentSize / 2,
          left: left - catchmentSize / 2,
          backgroundColor: 'transparent',
          cursor: props.cursor,
        }}
      />
    )

  const interactiveResize = React.useMemo(() => {
    return (
      props.targetComponentMetadata != null &&
      allowsInteractiveResize(
        props.targetComponentMetadata.specialSizeMeasurements.parentLayoutSystem,
      )
    )
  }, [props.targetComponentMetadata])

  const layoutSystem = layoutSystemForPositionOrFlex(
    props.targetComponentMetadata?.specialSizeMeasurements.position,
    props.flexDirection,
  )

  return (
    <React.Fragment>
      <LineSVGComponent
        scale={props.scale}
        centerX={left}
        centerY={top}
        edge={edge}
        color={props.color}
      />
      {when(
        displayResizeSelector && interactiveResize,
        <PropertyTargetSelector
          top={top + shiftPropertyTargetSelectorAxis('horizontal', props.direction, edge)}
          left={left + shiftPropertyTargetSelectorAxis('vertical', props.direction, edge)}
          options={getResizeOptions(layoutSystem, props.direction, edge)}
          targetComponentMetadata={props.targetComponentMetadata}
          targetProps={props.targetProps}
          key={
            props.targetComponentMetadata != null
              ? EP.toString(props.targetComponentMetadata.elementPath)
              : `${props.direction}-${props.position.x}-${props.position.y}`
          }
        />,
      )}
      {mouseCatcher}
    </React.Fragment>
  )
})

interface DimensionableControlProps {
  centerX: number
  centerY: number
  edge: 'before' | 'after'
  color?: string
  scale: number
}

const ControlSideShort = 3
const ControlSideLong = 8

const DimensionableControlVertical = (props: DimensionableControlProps) => {
  const colorTheme = useColorTheme()
  const controlLength = 15
  const controlWidth = ControlSideShort
  const scaledControlLength = controlLength / props.scale
  const scaledControlOffsetTop = -(scaledControlLength / 2)

  return (
    <div
      className='label-dimensionableControlVertical'
      style={{
        position: 'absolute',
        backgroundColor: colorTheme.canvasElementBackground.value,
        borderRadius: `${5 / props.scale}px`,
        // These just about work. I can clean them up afterwards
        boxShadow: `0px 0px 0px ${0.3 / props.scale}px ${colorTheme.primary.value}, 0px ${
          1 / props.scale
        }px ${3 / props.scale}px rgba(140,140,140,.9)`,
        height: controlLength / props.scale,
        width: controlWidth / props.scale,
        left: props.centerX - 1,
        top: props.centerY + scaledControlOffsetTop,
      }}
    />
  )
}

const DimensionableControlHorizontal = (props: DimensionableControlProps) => {
  const colorTheme = useColorTheme()
  const controlLength = ControlSideShort
  const controlWidth = 15
  const scaledControlWidth = controlWidth / props.scale
  const scaledControlOffsetLeft = -(scaledControlWidth / 2)

  return (
    <div
      className='label-dimensionableControlVertical'
      style={{
        position: 'absolute',
        backgroundColor: colorTheme.canvasElementBackground.value,
        borderRadius: `${5 / props.scale}px`,
        // These just about work. I can clean them up afterwards
        boxShadow: `0px 0px 0px ${0.3 / props.scale}px ${colorTheme.primary.value}, 0px ${
          1 / props.scale
        }px ${3 / props.scale}px rgba(140,140,140,.9)`,
        height: controlLength / props.scale,
        width: controlWidth / props.scale,
        left: props.centerX + scaledControlOffsetLeft,
        top: props.centerY - 1,
      }}
    />
  )
}

interface ResizePointProps {
  dispatch: EditorDispatch
  cursor: CSSCursor
  canvasOffset: CanvasPoint
  visualSize: CanvasRectangle
  scale: number
  position: EdgePosition
  resizeStatus: ResizeStatus
  extraStyle?: React.CSSProperties
  testID: string
}

class ResizePoint extends React.Component<ResizePointProps> {
  reference = React.createRef<HTMLDivElement>()

  render() {
    const left =
      this.props.canvasOffset.x +
      this.props.visualSize.x +
      this.props.position.x * this.props.visualSize.width
    const top =
      this.props.canvasOffset.y +
      this.props.visualSize.y +
      this.props.position.y * this.props.visualSize.height

    const size = 6 / this.props.scale
    const catchmentSize = 12 / this.props.scale

    const mouseCatcher =
      this.props.resizeStatus !== 'enabled' ? null : (
        <div
          ref={this.reference}
          style={{
            position: 'absolute',
            width: catchmentSize,
            height: catchmentSize,
            top: top - catchmentSize / 2,
            left: left - catchmentSize / 2,
            backgroundColor: 'transparent',
            cursor: this.props.cursor,
          }}
          data-testid={`${this.props.testID}-${this.props.position.x}-${this.props.position.y}`}
        />
      )
    return (
      <React.Fragment>
        <div
          style={{
            pointerEvents: 'initial',
            position: 'absolute',
            width: size,
            height: size,
            top: top - size / 2,
            left: left - size / 2,
            borderWidth: 1 / this.props.scale,
            boxSizing: 'border-box',
            backgroundColor: fixmeColorTheme.canvasControlsSizeBoxBackground.value,
            borderRadius: '10%',
            borderStyle: 'none',
            borderColor: 'transparent',
            boxShadow: `${fixmeColorTheme.canvasControlsSizeBoxShadowColor50.value} 0px 0px ${
              1 / this.props.scale
            }px, ${fixmeColorTheme.canvasControlsSizeBoxShadowColor21.value} 0px ${
              1 / this.props.scale
            }px ${2 / this.props.scale}px ${1 / this.props.scale}px `,
            ...this.props.extraStyle,
          }}
        />
        {mouseCatcher}
      </React.Fragment>
    )
  }
}

interface ResizeRectangleProps {
  children?: React.ReactNode
  targetComponentMetadata: ElementInstanceMetadata | null
  dispatch: EditorDispatch
  scale: number
  canvasOffset: CanvasPoint
  measureSize: CanvasRectangle // this is the size we want to adjust when the user drags
  visualSize: CanvasRectangle // this is the canvas size of the selection (might not be the same as measureSize in case of Yoga)
  resizeStatus: ResizeStatus
  selectedViews: Array<ElementPath>
  elementAspectRatioLocked: boolean
  imageMultiplier: number | null
  sideResizer: boolean
  color?: string
  dragState: ResizeDragState | null
  windowToCanvasPosition: (event: MouseEvent) => CanvasPositions
  getOriginalFrames: () => Array<OriginalCanvasAndLocalFrame>
  metadata: ElementInstanceMetadataMap
  onResizeStart: (originalSize: CanvasRectangle, draggedPoint: EdgePosition) => void
  testID: string
  maybeClearHighlightsOnHoverEnd: () => void
  flexDirection: 'horizontal' | 'vertical' | null
  propertyTargetSelectedIndex: number
  targetProps: ElementProps | null
}

export class ResizeRectangle extends React.Component<ResizeRectangleProps> {
  render() {
    const controlProps = this.props

    if (isZeroSizedElement(this.props.measureSize)) {
      return (
        <ZeroSizeResizeControl
          frame={this.props.measureSize}
          scale={this.props.scale}
          color={null}
          dispatch={this.props.dispatch}
          element={this.props.targetComponentMetadata}
          maybeClearHighlightsOnHoverEnd={this.props.maybeClearHighlightsOnHoverEnd}
        />
      )
    } else if (
      this.props.resizeStatus === 'enabled' ||
      this.props.resizeStatus === 'noninteractive'
    ) {
      const pointControls = unless(
        this.props.sideResizer,
        <React.Fragment>
          <ResizeControl
            {...controlProps}
            key={'resize-control-v-0.5-0.0'}
            cursor={CSSCursor.ResizeNS}
            position={{ x: 0.5, y: 0 }}
            enabledDirection={DirectionVertical}
          >
            <ResizeEdge
              {...controlProps}
              cursor={CSSCursor.ResizeNS}
              direction='horizontal'
              position={{ x: 0.5, y: 0 }}
            />
          </ResizeControl>
          <ResizeControl
            {...controlProps}
            key={'resize-control-v-0.5-1.0'}
            cursor={CSSCursor.ResizeNS}
            position={{ x: 0.5, y: 1 }}
            enabledDirection={DirectionVertical}
          >
            <ResizeEdge
              {...controlProps}
              cursor={CSSCursor.ResizeNS}
              direction='horizontal'
              position={{ x: 0.5, y: 1 }}
            />
          </ResizeControl>
          <ResizeControl
            {...controlProps}
            key={'resize-control-h-0.0-0.5'}
            cursor={CSSCursor.ResizeEW}
            position={{ x: 0, y: 0.5 }}
            enabledDirection={DirectionHorizontal}
          >
            <ResizeEdge
              {...controlProps}
              cursor={CSSCursor.ResizeEW}
              direction='vertical'
              position={{ x: 0, y: 0.5 }}
            />
          </ResizeControl>
          <ResizeControl
            {...controlProps}
            key={'resize-control-h-1.0-0.5'}
            cursor={CSSCursor.ResizeEW}
            position={{ x: 1, y: 0.5 }}
            enabledDirection={DirectionHorizontal}
          >
            <ResizeEdge
              {...controlProps}
              cursor={CSSCursor.ResizeEW}
              direction='vertical'
              position={{ x: 1, y: 0.5 }}
            />
          </ResizeControl>
          <ResizeControl
            {...controlProps}
            key={'resize-control-a-0.0-0.0'}
            cursor={CSSCursor.ResizeNWSE}
            position={{ x: 0, y: 0 }}
            enabledDirection={DirectionAll}
          >
            <ResizePoint
              {...controlProps}
              cursor={CSSCursor.ResizeNWSE}
              position={{ x: 0, y: 0 }}
            />
          </ResizeControl>
          <ResizeControl
            {...controlProps}
            key={'resize-control-a-1.0-0.0'}
            cursor={CSSCursor.ResizeNESW}
            position={{ x: 1, y: 0 }}
            enabledDirection={DirectionAll}
          >
            <ResizePoint
              {...controlProps}
              cursor={CSSCursor.ResizeNESW}
              position={{ x: 1, y: 0 }}
            />
          </ResizeControl>
          <ResizeControl
            {...controlProps}
            key={'resize-control-a-0.0-1.0'}
            cursor={CSSCursor.ResizeNESW}
            position={{ x: 0, y: 1 }}
            enabledDirection={DirectionAll}
          >
            <ResizePoint
              {...controlProps}
              cursor={CSSCursor.ResizeNESW}
              position={{ x: 0, y: 1 }}
            />
          </ResizeControl>
          <ResizeControl
            {...controlProps}
            key={'resize-control-a-1.0-1.0'}
            cursor={CSSCursor.ResizeNWSE}
            position={{ x: 1, y: 1 }}
            enabledDirection={DirectionAll}
          >
            <ResizePoint
              {...controlProps}
              cursor={CSSCursor.ResizeNWSE}
              position={{ x: 1, y: 1 }}
            />
          </ResizeControl>
        </React.Fragment>,
      )

      const resizeLines = when(
        this.props.sideResizer,
        <React.Fragment>
          <ResizeControl
            {...controlProps}
            key={'resize-control-v-0.5-1.0'}
            cursor={CSSCursor.ResizeNS}
            position={{ x: 0.5, y: 1 }}
            enabledDirection={DirectionVertical}
          >
            <ResizeLines
              {...controlProps}
              cursor={CSSCursor.ResizeNS}
              direction='horizontal'
              position={{ x: 0.5, y: 1 }}
            />
          </ResizeControl>
          <ResizeControl
            {...controlProps}
            key={'resize-control-h-1.0-0.5'}
            cursor={CSSCursor.ResizeEW}
            position={{ x: 1, y: 0.5 }}
            enabledDirection={DirectionHorizontal}
          >
            <ResizeLines
              {...controlProps}
              cursor={CSSCursor.ResizeEW}
              direction='vertical'
              position={{ x: 1, y: 0.5 }}
            />
          </ResizeControl>
        </React.Fragment>,
      )

      return (
        <React.Fragment>
          {resizeLines}
          {pointControls}
        </React.Fragment>
      )
    } else {
      return null
    }
  }
}
