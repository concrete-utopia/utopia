import * as React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { Imports, ElementPath } from '../../../core/shared/project-file-types'
import { KeysPressed } from '../../../utils/keyboard'
import utils from '../../../utils/utils'
import { CanvasPoint, CanvasRectangle, WindowPoint } from '../../../core/shared/math-utils'
import { EditorDispatch } from '../../editor/action-types'
import * as EditorActions from '../../editor/actions/action-creators'
import * as EP from '../../../core/shared/element-path'
import { ControlFontSize } from '../canvas-controls-frame'
import { CanvasPositions } from '../canvas-types'
import { calculateExtraSizeForZeroSizedElement } from './outline-utils'
//TODO: switch to functional component and make use of 'useColorTheme':
import { UtopiaTheme, colorTheme } from '../../../uuiui'

interface ComponentAreaControlProps {
  mouseEnabled: boolean
  target: ElementPath
  frame: CanvasRectangle
  highlighted: boolean
  canvasOffset: CanvasPoint
  scale: number
  dispatch: EditorDispatch
  hoverEffectEnabled: boolean
  doubleClickToSelect: boolean
  selectedComponents: Array<ElementPath>
  selectComponent?: (target: ElementPath, isMultiselect: boolean) => Array<ElementPath>
  onMouseDown?: (
    views: Array<ElementPath>,
    target: ElementPath,
    dragStart: CanvasPoint,
    originalEvent: React.MouseEvent<HTMLDivElement>,
  ) => void
  componentMetadata: ElementInstanceMetadataMap
  onHover: (target: ElementPath) => void
  onHoverEnd: (target: ElementPath) => void
  keysPressed: KeysPressed
  windowToCanvasPosition: (event: MouseEvent) => CanvasPositions
  selectedViews: ElementPath[]
  showAdditionalControls: boolean
  testID?: string
}

// SelectModeControl is a transparent react component sitting on top of a utopia component.
// It catches and handles mouse event for basic canvas interaction: selection, dragging, highlight
class ComponentAreaControlInner extends React.Component<ComponentAreaControlProps> {
  mouseOver: boolean = false

  constructor(props: ComponentAreaControlProps) {
    super(props)
  }

  mouseEnabled = () => !this.props.keysPressed.z

  onMouseDown = (event: React.MouseEvent<HTMLDivElement>) => {
    if (this.mouseEnabled()) {
      if (!this.props.doubleClickToSelect) {
        const isMultiselect = event.shiftKey
        const newSelection =
          this.props.selectComponent == null
            ? []
            : this.props.selectComponent(this.props.target, isMultiselect)
        this.handleDragStart(newSelection, event)
      } else {
        this.handleDragStart(this.props.selectedComponents, event)
      }
    }
  }

  onMouseOver = () => {
    this.mouseOver = true
    if (this.mouseEnabled()) {
      if (this.props.hoverEffectEnabled) {
        this.onHoverBegin()
      }
    }
  }

  componentWillUnmount() {
    if (this.mouseOver) {
      this.onHoverEnd()
    }
  }

  onMouseLeave = () => {
    this.mouseOver = false
    this.onHoverEnd()
  }

  onHoverBegin = () => {
    if (!this.props.highlighted) {
      this.props.onHover(this.props.target)
    }
  }

  onDragEnter = () => {
    this.onHoverBegin()
  }

  onHoverEnd = () => {
    if (this.props.highlighted) {
      this.props.onHoverEnd(this.props.target)
    }
  }

  onDragLeave = () => {
    this.onHoverEnd()
  }

  handleDragStart(
    selectedViews: Array<ElementPath>,
    event: React.MouseEvent<HTMLDivElement>,
  ): void {
    const cursorPosition = this.props.windowToCanvasPosition(event.nativeEvent)
    if (this.props.mouseEnabled) {
      if (this.props.onMouseDown == null) {
        throw new Error('onMouseDown must be provided for mouse enabled controls')
      }

      event.stopPropagation()

      this.props.onMouseDown(
        selectedViews,
        this.props.target,
        cursorPosition.canvasPositionRaw,
        event,
      )
    }
  }

  getComponentLabelControl = () => {
    const label = MetadataUtils.getElementLabel(this.props.target, this.props.componentMetadata)
    const scaledFontSize = ControlFontSize / this.props.scale
    const offsetY = -(scaledFontSize * 1.5)
    const offsetX = 3 / this.props.scale

    const isInternalComponent = false // TODO MISSING FEATURE: we need to determine if something is an instance of an internal component
    const isOutputComponent = false // TODO MISSING FEATURE: we need a way to tell the output component from the rest
    const labelSelectable = this.mouseEnabled()

    return (
      <React.Fragment>
        <div
          onMouseOver={labelSelectable ? this.onMouseOver : utils.NO_OP}
          onMouseOut={labelSelectable ? this.onMouseLeave : utils.NO_OP}
          onMouseDown={labelSelectable ? this.onMouseDown : utils.NO_OP}
          onDragEnter={labelSelectable ? this.onDragEnter : utils.NO_OP}
          onDragLeave={labelSelectable ? this.onDragLeave : utils.NO_OP}
          data-testid={this.props.mouseEnabled && labelSelectable ? this.props.testID : undefined}
          className='roleComponentName'
          style={{
            pointerEvents: this.props.mouseEnabled ? 'initial' : 'none',
            color: isInternalComponent
              ? colorTheme.component.value
              : colorTheme.subduedForeground.value,
            position: 'absolute',
            fontWeight: isOutputComponent ? 600 : 500,
            left: this.props.canvasOffset.x + this.props.frame.x + offsetX,
            top: this.props.canvasOffset.y + this.props.frame.y + offsetY,
            maxWidth: this.props.frame.width,
            paddingBottom: '0px',
            fontFamily:
              '-apple-system, BlinkMacSystemFont, Helvetica, "Segoe UI", Roboto,  Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol"',
            fontSize: scaledFontSize,
            whiteSpace: 'nowrap',
            overflow: 'hidden',
            textOverflow: 'ellipsis',
            textDecoration:
              this.isTargetSelected() || this.props.highlighted ? 'underline' : undefined,
          }}
        >
          {label}
          {/* TODO MISSING FEATURE: states are MIA for the moment */}
          {/* {component.stateGroups.length > 0 ? (
            <span className=' ml4 br2 ph2' style={{ fontSize: scaledFontSize }}>
              {component.stateGroups.length}
            </span>
          ) : null} */}
        </div>
      </React.Fragment>
    )
  }

  isTargetSelected = () => {
    return this.props.selectedComponents.some((tp: ElementPath) =>
      EP.pathsEqual(this.props.target, tp),
    )
  }
}
export class ComponentLabelControl extends ComponentAreaControlInner {
  render() {
    return this.getComponentLabelControl()
  }
}
