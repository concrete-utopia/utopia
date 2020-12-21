import * as React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { JSXMetadata } from '../../../core/shared/element-template'
import { Imports, TemplatePath } from '../../../core/shared/project-file-types'
import { KeysPressed } from '../../../utils/keyboard'
import utils from '../../../utils/utils'
import { CanvasPoint, CanvasRectangle, WindowPoint } from '../../../core/shared/math-utils'
import { EditorDispatch } from '../../editor/action-types'
import * as EditorActions from '../../editor/actions/action-creators'
import * as TP from '../../../core/shared/template-path'
import { ControlFontSize } from '../canvas-controls-frame'
import { CanvasPositions } from '../canvas-types'
import { calculateExtraSizeForZeroSizedElement } from './outline-utils'
import { UtopiaTheme, colorTheme } from '../../../uuiui'

interface ComponentAreaControlProps {
  mouseEnabled: boolean
  target: TemplatePath
  frame: CanvasRectangle
  highlighted: boolean
  canvasOffset: CanvasPoint
  scale: number
  dispatch: EditorDispatch
  hoverEffectEnabled: boolean
  doubleClickToSelect: boolean
  selectedComponents: Array<TemplatePath>
  selectComponent?: (target: TemplatePath, isMultiselect: boolean) => Array<TemplatePath>
  onMouseDown: (
    views: Array<TemplatePath>,
    target: TemplatePath,
    dragStart: CanvasPoint,
    originalEvent: React.MouseEvent<HTMLDivElement>,
  ) => void
  componentMetadata: JSXMetadata
  onHover: (target: TemplatePath) => void
  onHoverEnd: (target: TemplatePath) => void
  keysPressed: KeysPressed
  windowToCanvasPosition: (event: MouseEvent) => CanvasPositions
  selectedViews: TemplatePath[]
  imports: Imports
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

  onClick = (event: React.MouseEvent<HTMLDivElement>) => {
    if (this.mouseEnabled()) {
      if (event.nativeEvent.detail > 1) {
        // we interpret this as a double click so that the user can keep on clicking and we keep firing double clicks
        this.onDoubleClick(event)
      }
    }
  }

  onDoubleClick = (event: React.MouseEvent<HTMLDivElement>) => {
    const { showingInvisibleElement } = calculateExtraSizeForZeroSizedElement(this.props.frame)
    if (this.mouseEnabled()) {
      if (this.props.doubleClickToSelect && this.props.selectComponent != null) {
        const isMultiselect = event.shiftKey
        this.props.selectComponent(this.props.target, isMultiselect)
      }
      if (TP.isInstancePath(this.props.target)) {
        const instance = MetadataUtils.getElementByInstancePathMaybe(
          this.props.componentMetadata.elements,
          this.props.target,
        )
        if (
          this.props.selectedComponents.length === 1 &&
          TP.pathsEqual(this.props.selectedComponents[0], this.props.target)
        ) {
          if (MetadataUtils.isTextAgainstImports(this.props.imports, instance)) {
            // if target is a text and it is the one and only selected component, then activate the text editor
            const mousePosition = {
              x: event.clientX,
              y: event.clientY,
            } as WindowPoint
            this.props.dispatch(
              [EditorActions.openTextEditor(this.props.target, mousePosition)],
              'canvas',
            )
          } else if (showingInvisibleElement) {
            // double clicking on an invisible element should give it an arbitrary size
            this.props.dispatch([
              EditorActions.addMissingDimensions(this.props.target, this.props.frame),
            ])
          }
        }
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
    selectedViews: Array<TemplatePath>,
    event: React.MouseEvent<HTMLDivElement>,
  ): void {
    const cursorPosition = this.props.windowToCanvasPosition(event.nativeEvent)
    this.props.onMouseDown(
      selectedViews,
      this.props.target,
      cursorPosition.canvasPositionRaw,
      event,
    )
  }

  getComponentAreaControl = (canShowInvisibleIndicator: boolean) => {
    const {
      extraWidth,
      extraHeight,
      showingInvisibleElement,
      borderRadius,
    } = calculateExtraSizeForZeroSizedElement(this.props.frame)
    const showInvisibleIndicator = canShowInvisibleIndicator && showingInvisibleElement

    return (
      <React.Fragment>
        <div
          className='role-component-selection-highlight'
          onMouseOver={this.onMouseOver}
          onMouseLeave={this.onMouseLeave}
          onMouseDown={this.onMouseDown}
          onClick={this.onClick}
          onDragEnter={this.onDragEnter}
          onDragLeave={this.onDragLeave}
          style={{
            pointerEvents: this.props.mouseEnabled ? 'initial' : 'none',
            position: 'absolute',
            left: this.props.canvasOffset.x + this.props.frame.x - extraWidth / 2,
            top: this.props.canvasOffset.y + this.props.frame.y - extraHeight / 2,
            width: this.props.frame.width + extraWidth,
            height: this.props.frame.height + extraHeight,
            borderColor: UtopiaTheme.color.primary.o(50).value,
            borderStyle: showInvisibleIndicator ? 'solid' : undefined,
            borderWidth: 0.5 / this.props.scale,
            borderRadius: showInvisibleIndicator ? borderRadius : 0,
          }}
          data-testid={this.props.testID}
        />
      </React.Fragment>
    )
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
    return this.props.selectedComponents.some((tp: TemplatePath) =>
      TP.pathsEqual(this.props.target, tp),
    )
  }
}

export class ComponentAreaControl extends ComponentAreaControlInner {
  componentDidUpdate(prevProps: ComponentAreaControlProps) {
    if (prevProps.hoverEffectEnabled !== this.props.hoverEffectEnabled) {
      if (this.mouseOver && !this.isTargetSelected()) {
        if (this.props.hoverEffectEnabled) {
          this.onHoverBegin()
        } else {
          this.onHoverEnd()
        }
      }
    }
  }

  render() {
    const isParentSelected = this.props.selectedComponents.some((tp: TemplatePath) =>
      TP.pathsEqual(TP.parentPath(this.props.target), tp),
    )
    return this.getComponentAreaControl(isParentSelected || this.props.showAdditionalControls)
  }
}

export class ComponentLabelControl extends ComponentAreaControlInner {
  render() {
    return this.getComponentLabelControl()
  }
}
