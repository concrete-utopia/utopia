/** @jsx jsx */
import { jsx } from '@emotion/react'
import * as React from 'react'
import Utils from '../../../utils/utils'
import { CanvasPoint, CanvasRectangle } from '../../../core/shared/math-utils'
import { EditorDispatch } from '../../editor/action-types'
import { setCanvasAnimationsEnabled } from '../../editor/actions/action-creators'
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
} from '../canvas-types'
import { ResizeStatus } from './new-canvas-controls'
import { ElementPath } from '../../../core/shared/project-file-types'
import CanvasActions from '../canvas-actions'
import { OriginalCanvasAndLocalFrame, ResizeOptions } from '../../editor/store/editor-state'
import {
  DetectedLayoutSystem,
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../../core/shared/element-template'
import { calculateExtraSizeForZeroSizedElement } from './outline-utils'
import { isFeatureEnabled } from '../../../utils/feature-switches'
import { SizeBoxLabel } from './size-box-label'
//TODO: switch to functional component and make use of 'useColorTheme':
import { colorTheme as fixmeColorTheme, useColorTheme } from '../../../uuiui'
import { LayoutTargetableProp } from '../../../core/layout/layout-helpers-new'
import { PropertyTargetSelector } from './property-target-selector'
import { unless, when } from '../../../utils/react-conditionals'
import { betterReactMemo } from '../../../uuiui-deps'

interface ResizeControlProps extends ResizeRectangleProps {
  cursor: CSSCursor
  position: EdgePosition
  enabledDirection: EnabledDirection
  selectedViews: Array<ElementPath>
  dragState: ResizeDragState | null
}

class ResizeControl extends React.Component<ResizeControlProps> {
  reference = React.createRef<HTMLDivElement>()
  constructor(props: ResizeControlProps) {
    super(props)
  }

  componentWillUnmount() {
    this.props.dispatch([setCanvasAnimationsEnabled(true)], 'canvas')
  }

  onMouseDown = (event: React.MouseEvent<HTMLDivElement>) => {
    event.stopPropagation()
    if (event.buttons === 1) {
      const centerBasedResize = event.altKey
      const keepAspectRatio = event.shiftKey || this.props.elementAspectRatioLocked
      const enableSnapping = !event.metaKey
      const canvasPositions = this.props.windowToCanvasPosition(event.nativeEvent)
      const start: CanvasPoint = canvasPositions.canvasPositionRaw
      const originalFrames = this.props.getOriginalFrames()
      const isMultiSelect = this.props.selectedViews.length !== 1
      const newDragState = resizeDragState(
        start,
        null,
        enableSnapping,
        centerBasedResize,
        keepAspectRatio,
        this.props.measureSize,
        originalFrames,
        this.props.position,
        this.props.enabledDirection,
        this.props.metadata,
        this.props.selectedViews,
        isMultiSelect,
        this.props.resizeOptions.propertyTargetOptions[
          this.props.resizeOptions.propertyTargetSelectedIndex
        ],
      )

      this.props.dispatch(
        [CanvasActions.createDragState(newDragState), setCanvasAnimationsEnabled(false)],
        'canvas',
      )
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
  labels: {
    vertical: string
    horizontal: string
  }
  dragState: DragState | null
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

    const isEdgeDragged =
      this.props.dragState != null &&
      this.props.dragState.type === 'RESIZE_DRAG_STATE' &&
      this.props.dragState.start != null &&
      this.props.dragState.edgePosition.x === this.props.position.x &&
      this.props.dragState.edgePosition.y === this.props.position.y

    const interactiveResize =
      this.props.targetComponentMetadata != null &&
      allowsInteractiveResize(
        this.props.targetComponentMetadata.specialSizeMeasurements.parentLayoutSystem,
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
          isEdgeDragged && interactiveResize,
          <PropertyTargetSelector
            top={top + shiftPropertyTargetSelectorAxis('horizontal', this.props.direction, edge)}
            left={left + shiftPropertyTargetSelectorAxis('vertical', this.props.direction, edge)}
            options={
              this.props.direction === 'horizontal'
                ? ['Height', 'marginTop', 'marginBottom', 'minHeight', 'maxHeight']
                : ['Width', 'marginLeft', 'marginRight', 'minWidth', 'maxWidth']
            }
            targetComponentMetadata={this.props.targetComponentMetadata}
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
  labels: {
    vertical: LayoutTargetableProp
    horizontal: LayoutTargetableProp
  }
}

const LineOffset = 6
const ResizeLines = betterReactMemo('ResizeLines', (props: ResizeLinesProps) => {
  const reference = React.createRef<HTMLDivElement>()
  const LineSVGComponent =
    props.position.y === 0.5 ? DimensionableControlVertical : DimensionableControlHorizontal

  const beforeOrAfter = props.position.y === 0.5 ? props.position.x : props.position.y
  const edge = beforeOrAfter === 0 ? 'before' : 'after'

  const isEdgeDragged =
    props.dragState != null &&
    props.dragState.type === 'RESIZE_DRAG_STATE' &&
    props.dragState.start != null &&
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
        isEdgeDragged && interactiveResize,
        <PropertyTargetSelector
          top={top + shiftPropertyTargetSelectorAxis('horizontal', props.direction, edge)}
          left={left + shiftPropertyTargetSelectorAxis('vertical', props.direction, edge)}
          options={
            props.direction === 'vertical'
              ? [props.labels.vertical, 'minWidth', 'maxWidth']
              : [props.labels.horizontal, 'minHeight', 'maxHeight']
          }
          targetComponentMetadata={props.targetComponentMetadata}
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
        backgroundColor: colorTheme.primary.shade(10).value,
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
        backgroundColor: colorTheme.primary.shade(10).value,
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
            boxShadow: `${fixmeColorTheme.canvasControlsSizeBoxShadowColor.o(50).value} 0px 0px ${
              1 / this.props.scale
            }px, ${fixmeColorTheme.canvasControlsSizeBoxShadowColor.o(21).value} 0px ${
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
  labels: {
    vertical: LayoutTargetableProp
    horizontal: LayoutTargetableProp
  }
  resizeOptions: ResizeOptions
}

export class ResizeRectangle extends React.Component<ResizeRectangleProps> {
  render() {
    const controlProps = this.props

    const { showingInvisibleElement, dimension } = calculateExtraSizeForZeroSizedElement(
      this.props.measureSize,
    )

    if (showingInvisibleElement && isFeatureEnabled('Invisible Element Controls')) {
      // is it a one dimensional element?
      const verticalResizeControls =
        dimension === 'vertical' ? (
          <React.Fragment>
            <ResizeControl
              {...controlProps}
              key={'resize-control-v-0.5-0.0'}
              cursor={CSSCursor.ResizeNS}
              position={{ x: 0.5, y: 0 }}
              enabledDirection={DirectionVertical}
            >
              <ResizePoint
                {...controlProps}
                cursor={CSSCursor.ResizeNS}
                position={{ x: 0.5, y: 0 }}
                extraStyle={{
                  opacity: 0,
                }}
              />
            </ResizeControl>
            <ResizeControl
              {...controlProps}
              key={'resize-control-v-0.5-1.0'}
              cursor={CSSCursor.ResizeNS}
              position={{ x: 0.5, y: 1 }}
              enabledDirection={DirectionVertical}
            >
              <ResizePoint
                {...controlProps}
                cursor={CSSCursor.ResizeNS}
                position={{ x: 0.5, y: 1 }}
                extraStyle={{
                  opacity: 0,
                }}
              />
            </ResizeControl>
          </React.Fragment>
        ) : null

      const horizontalResizeControls =
        dimension === 'horizontal' ? (
          <React.Fragment>
            <ResizeControl
              {...controlProps}
              key={'resize-control-h-0.0-0.5'}
              cursor={CSSCursor.ResizeEW}
              position={{ x: 0, y: 0.5 }}
              enabledDirection={DirectionHorizontal}
            >
              <ResizePoint
                {...controlProps}
                cursor={CSSCursor.ResizeEW}
                position={{ x: 0, y: 0.5 }}
                extraStyle={{
                  opacity: 0,
                }}
              />
            </ResizeControl>
            <ResizeControl
              {...controlProps}
              key={'resize-control-h-1.0-0.5'}
              cursor={CSSCursor.ResizeEW}
              position={{ x: 1, y: 0.5 }}
              enabledDirection={DirectionHorizontal}
            >
              <ResizePoint
                {...controlProps}
                cursor={CSSCursor.ResizeEW}
                position={{ x: 1, y: 0.5 }}
                extraStyle={{
                  opacity: 0,
                }}
              />
            </ResizeControl>
          </React.Fragment>
        ) : null

      // TODO  double click sets an arbitrary size in the missing dimensions
      return [verticalResizeControls, horizontalResizeControls]
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
