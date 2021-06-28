import * as React from 'react'
import { FlexStretch, Sides } from 'utopia-api'
import { LayoutHelpers } from '../../../core/layout/layout-helpers'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ElementInstanceMetadata } from '../../../core/shared/element-template'
import { ElementPath } from '../../../core/shared/project-file-types'
import { defaultEither, mapEither } from '../../../core/shared/either'
import Utils from '../../../utils/utils'
import { CanvasRectangle, canvasRectangle } from '../../../core/shared/math-utils'
import * as EP from '../../../core/shared/element-path'
import { ResizeDragState } from '../canvas-types'
import { getOriginalFrames } from '../canvas-utils'
import { ControlProps } from './new-canvas-controls'
import { getSelectionColor } from './outline-control'
import { ResizeRectangle } from './size-box'
import { betterReactMemo } from '../../../uuiui-deps'
import { useColorTheme } from '../../../uuiui'
interface YogaResizeControlProps extends ControlProps {
  targetElement: ElementInstanceMetadata
  target: ElementPath
  color: string
  dragState: ResizeDragState | null
}

class YogaResizeControl extends React.Component<YogaResizeControlProps> {
  getTargetStretch = (): FlexStretch => {
    const target = this.props.targetElement
    const parentPath = EP.parentPath(this.props.target)
    const sceneMetadataOrElementMetadata = MetadataUtils.findElementByElementPath(
      this.props.componentMetadata,
      parentPath,
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
    const yogaSize = MetadataUtils.getYogaSizeProps(this.props.target, this.props.componentMetadata)

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
        testID={`component-resize-control-${EP.toComponentId(this.props.target)}-0`}
        maybeClearHighlightsOnHoverEnd={this.props.maybeClearHighlightsOnHoverEnd}
      />
    )
  }
}

export interface YogaControlsProps extends ControlProps {
  dragState: ResizeDragState | null
}

export const YogaControls = betterReactMemo('YogaControls', (props: YogaControlsProps) => {
  const colorTheme = useColorTheme()
  const targets = props.selectedViews
  const everyThingIsYogaLayouted = targets.every((selectedView) => {
    return MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
      selectedView,
      props.componentMetadata,
    )
  })

  const unknownElementsSelected = MetadataUtils.anyUnknownOrGeneratedElements(
    props.projectContents,
    props.nodeModules,
    props.openFile,
    props.selectedViews,
  )

  const showResizeControl =
    targets.length === 1 && everyThingIsYogaLayouted && !unknownElementsSelected

  let color: string = ''
  if (showResizeControl && targets.length > 0) {
    const selectedView = targets[0]
    color = getSelectionColor(
      selectedView,
      props.componentMetadata,
      props.focusedElementPath,
      colorTheme,
    )
  }

  return (
    <React.Fragment>
      {!showResizeControl ? null : (
        <YogaResizeControl
          {...props}
          target={targets[0]}
          targetElement={
            MetadataUtils.findElementByElementPath(props.componentMetadata, targets[0])!
          }
          color={color}
        />
      )}
    </React.Fragment>
  )
})
