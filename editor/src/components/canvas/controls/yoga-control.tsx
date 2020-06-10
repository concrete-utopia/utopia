import * as React from 'react'
import { FlexStretch, Sides } from 'utopia-api'
import { colorTheme } from 'uuiui'
import { LayoutHelpers } from '../../../core/layout/layout-helpers'
import {
  getSceneMetadataOrElementInstanceMetadata,
  MetadataUtils,
} from '../../../core/model/element-metadata-utils'
import {
  ComponentMetadata,
  ElementInstanceMetadata,
  UtopiaJSXComponent,
} from '../../../core/shared/element-template'
import { InstancePath } from '../../../core/shared/project-file-types'
import { defaultEither, mapEither } from '../../../core/shared/either'
import Utils from '../../../utils/utils'
import { CanvasPoint, CanvasRectangle, canvasRectangle } from '../../../core/shared/math-utils'
import { EditorDispatch } from '../../editor/action-types'
import { OriginalFrame } from '../../editor/store/editor-state'
import * as TP from '../../../core/shared/template-path'
import { ResizeDragState } from '../canvas-types'
import { getOriginalFrames } from '../canvas-utils'
import { ControlProps } from './new-canvas-controls'
import { getSelectionColor } from './outline-control'
import { ResizeRectangle } from './size-box'

interface YogaControlProps {
  dispatch: EditorDispatch
  rootComponents: Array<UtopiaJSXComponent>
  componentMetadata: ComponentMetadata[]
  canvasOffset: CanvasPoint
  scale: number
  target: InstancePath
}

interface YogaControlState {
  originalFrames: Array<OriginalFrame>
}
class YogaControl extends React.Component<YogaControlProps, YogaControlState> {
  constructor(props: YogaControlProps) {
    super(props)
    this.state = {
      originalFrames: [],
    }
  }

  getElementsForPadding(padding: Partial<Sides> | null, frame: CanvasRectangle) {
    if (padding == null) {
      return []
    } else {
      const leftElement =
        padding.left != null && padding.left !== 0 ? (
          <div
            className=' roleYogaPaddingControl f10  fw5 tc overflow-visible'
            style={{
              background: colorTheme.paddingFillTranslucent.value,
              color: colorTheme.paddingForeground.value,
              position: 'absolute',
              left: frame.x + this.props.canvasOffset.x,
              top: frame.y + this.props.canvasOffset.y,
              width: padding.left,
              height: frame.height,
              lineHeight: frame.height + 'px',
            }}
          >
            {padding.left}
          </div>
        ) : null
      const topElement =
        padding.top != null && padding.top !== 0 ? (
          <div
            className=' f10  fw4 tc overflow-visible'
            style={{
              background: colorTheme.paddingFillTranslucent.value,
              color: colorTheme.paddingForeground.value,
              position: 'absolute',
              left: frame.x + this.props.canvasOffset.x + (padding.left || 0),
              top: frame.y + this.props.canvasOffset.y,
              width: frame.width - (padding.left || 0) - (padding.right || 0),
              height: padding.top,
              lineHeight: padding.top + 'px',
            }}
          >
            {padding.top}
          </div>
        ) : null
      const rightElement =
        padding.right != null && padding.right !== 0 ? (
          <div
            className=' f10  fw4 tc overflow-visible'
            style={{
              background: colorTheme.paddingFillTranslucent.value,
              color: colorTheme.paddingForeground.value,
              position: 'absolute',
              left: frame.x + this.props.canvasOffset.x + frame.width - padding.right,
              top: frame.y + this.props.canvasOffset.y,
              width: padding.right,
              height: frame.height,
              lineHeight: frame.height + 'px',
            }}
          >
            {padding.right}
          </div>
        ) : null
      const bottomElement =
        padding.bottom != null && padding.bottom !== 0 ? (
          <div
            className=' f10  fw4 tc overflow-visible'
            style={{
              background: colorTheme.paddingFillTranslucent.value,
              color: colorTheme.paddingForeground.value,
              position: 'absolute',
              left: frame.x + this.props.canvasOffset.x + (padding.left || 0),
              top: frame.y + this.props.canvasOffset.y + frame.height - padding.bottom,
              width: frame.width - (padding.left || 0) - (padding.right || 0),
              height: padding.bottom,
              lineHeight: padding.bottom + 'px',
            }}
          >
            {padding.bottom}
          </div>
        ) : null
      const innerDiv =
        leftElement == null &&
        topElement == null &&
        rightElement == null &&
        bottomElement == null ? null : (
          <div
            className='ba bgtransparent'
            style={{
              position: 'absolute',
              pointerEvents: 'none',
              left: frame.x + this.props.canvasOffset.x + (padding.left || 0),
              top: frame.y + this.props.canvasOffset.y + (padding.top || 0),
              width: frame.width - (padding.left || 0) - (padding.right || 0),
              height: frame.height - (padding.top || 0) - (padding.bottom || 0),
            }}
          />
        )
      return [leftElement, topElement, rightElement, bottomElement, innerDiv]
    }
  }

  render() {
    const selectedView = MetadataUtils.getElementByInstancePathMaybe(
      this.props.componentMetadata,
      this.props.target,
    )
    if (selectedView == null) {
      return null
    }
    if (
      !MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
        this.props.target,
        this.props.componentMetadata,
      )
    ) {
      return null
    }
    const selectedViewFrame = selectedView.globalFrame
    const padding = MetadataUtils.getYogaPadding(selectedView)
    if (selectedViewFrame == null) {
      return null
    }
    return <React.Fragment>{this.getElementsForPadding(padding, selectedViewFrame)}</React.Fragment>
  }
}

interface YogaResizeControlProps extends ControlProps {
  targetElement: ElementInstanceMetadata
  target: InstancePath
  color: string
  dragState: ResizeDragState | null
}

class YogaResizeControl extends React.Component<YogaResizeControlProps> {
  getTargetStretch = (): FlexStretch => {
    const target = this.props.targetElement
    const parentPath = TP.parentPath(this.props.target)
    const sceneMetadataOrElementMetadata = getSceneMetadataOrElementInstanceMetadata(
      parentPath,
      this.props.componentMetadata,
    )
    const defaultStretch = 'none'
    if (sceneMetadataOrElementMetadata == null) {
      return defaultStretch
    } else {
      return defaultEither(
        defaultStretch,
        LayoutHelpers.getFlexStretchForChild(sceneMetadataOrElementMetadata, target),
      )
    }
  }

  getYogaSize = (visualSize: CanvasRectangle): CanvasRectangle => {
    const childStretch = this.getTargetStretch()
    const yogaSize = MetadataUtils.getYogaSizeProps(
      this.props.target,
      this.props.componentMetadata,
      this.props.rootComponents,
    )

    return canvasRectangle({
      x: visualSize.x,
      y: visualSize.y,
      width:
        childStretch === 'horizontal' || yogaSize.width == null ? visualSize.width : yogaSize.width, // warning, this only works so long as 'scale' is not a first class citizen
      height:
        childStretch === 'vertical' || yogaSize.height == null
          ? visualSize.height
          : yogaSize.height,
    })
  }

  obtainOriginalFrames = () => {
    return getOriginalFrames(this.props.selectedViews, this.props.componentMetadata)
  }

  render() {
    const visualSize = this.props.targetElement.globalFrame
    if (visualSize == null) {
      // TODO check me
      return null
    }

    const yogaSize = this.getYogaSize(visualSize)

    return (
      <ResizeRectangle
        dispatch={this.props.dispatch}
        scale={this.props.scale}
        canvasOffset={this.props.canvasOffset}
        measureSize={yogaSize}
        visualSize={visualSize}
        resizeStatus={this.props.resizeStatus}
        selectedViews={this.props.selectedViews}
        elementAspectRatioLocked={this.props.elementAspectRatioLocked}
        imageMultiplier={this.props.imageMultiplier}
        sideResizer={true}
        color={this.props.color}
        dragState={this.props.dragState}
        windowToCanvasPosition={this.props.windowToCanvasPosition}
        getOriginalFrames={this.obtainOriginalFrames}
        metadata={this.props.componentMetadata}
        onResizeStart={Utils.NO_OP}
      />
    )
  }
}

export interface YogaControlsProps extends ControlProps {
  dragState: ResizeDragState | null
}

export class YogaControls extends React.Component<YogaControlsProps> {
  render() {
    const targets = TP.filterScenes(this.props.selectedViews)
    const everyThingIsYogaLayouted = targets.every((selectedView) => {
      return MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
        selectedView,
        this.props.componentMetadata,
      )
    })
    const unknownElementsSelected = MetadataUtils.anyUnknownOrGeneratedElements(
      this.props.rootComponents,
      this.props.componentMetadata,
      this.props.selectedViews,
    )
    const showResizeControl =
      targets.length === 1 && everyThingIsYogaLayouted && !unknownElementsSelected

    const yogaControls = targets.map((selectedView) => {
      return (
        <YogaControl
          {...this.props}
          key={'yoga-' + TP.toComponentId(selectedView)}
          target={selectedView}
        />
      )
    })

    let color: string = ''
    if (showResizeControl) {
      const selectedView = targets[0]
      const instance = TP.isScenePath(selectedView)
        ? null
        : MetadataUtils.getElementByInstancePathMaybe(this.props.componentMetadata, selectedView)
      const createsYogaLayout = MetadataUtils.isFlexLayoutedContainer(instance)
      color = getSelectionColor(
        selectedView,
        this.props.rootComponents,
        this.props.componentMetadata,
        this.props.imports,
        createsYogaLayout,
        true,
      )
    }

    return (
      <React.Fragment>
        {...yogaControls}
        {!showResizeControl ? null : (
          <YogaResizeControl
            {...this.props}
            target={targets[0]}
            targetElement={
              MetadataUtils.getElementByInstancePathMaybe(this.props.componentMetadata, targets[0])!
            }
            color={color}
          />
        )}
      </React.Fragment>
    )
  }
}
