import * as React from 'react'
import { FlexStretch, Sides } from 'utopia-api'
import { LayoutHelpers } from '../../../core/layout/layout-helpers'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ElementInstanceMetadata } from '../../../core/shared/element-template'
import { TemplatePath } from '../../../core/shared/project-file-types'
import { defaultEither, mapEither } from '../../../core/shared/either'
import Utils from '../../../utils/utils'
import { CanvasRectangle, canvasRectangle } from '../../../core/shared/math-utils'
import * as TP from '../../../core/shared/template-path'
import { ResizeDragState } from '../canvas-types'
import { getOriginalFrames } from '../canvas-utils'
import { ControlProps } from './new-canvas-controls'
import { getSelectionColor } from './outline-control'
import { ResizeRectangle } from './size-box'
import {
  getJSXComponentsAndImportsForPathInnerComponent,
  withUnderlyingTarget,
} from '../../editor/store/editor-state'
import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
interface YogaResizeControlProps extends ControlProps {
  targetElement: ElementInstanceMetadata
  target: TemplatePath
  color: string
  dragState: ResizeDragState | null
}

class YogaResizeControl extends React.Component<YogaResizeControlProps> {
  getTargetStretch = (): FlexStretch => {
    const target = this.props.targetElement
    const parentPath = TP.parentPath(this.props.target)
    const sceneMetadataOrElementMetadata = MetadataUtils.findElementByTemplatePath(
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
    const { components } = getJSXComponentsAndImportsForPathInnerComponent(
      this.props.target,
      this.props.openFile,
      this.props.projectContents,
      this.props.nodeModules,
      this.props.transientState.filesState,
      this.props.resolve,
    )
    const yogaSize = MetadataUtils.getYogaSizeProps(
      this.props.target,
      this.props.componentMetadata,
      components,
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
        testID={`component-resize-control-${TP.toComponentId(this.props.target)}-0`}
        maybeClearHighlightsOnHoverEnd={this.props.maybeClearHighlightsOnHoverEnd}
      />
    )
  }
}

export interface YogaControlsProps extends ControlProps {
  dragState: ResizeDragState | null
}

export class YogaControls extends React.Component<YogaControlsProps> {
  render() {
    const targets = this.props.selectedViews
    const everyThingIsYogaLayouted = targets.every((selectedView) => {
      return MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
        selectedView,
        this.props.componentMetadata,
      )
    })

    const unknownElementsSelected = MetadataUtils.anyUnknownOrGeneratedElements(
      this.props.projectContents,
      this.props.nodeModules,
      this.props.openFile,
      this.props.selectedViews,
    )

    const showResizeControl =
      targets.length === 1 && everyThingIsYogaLayouted && !unknownElementsSelected

    let color: string = ''
    if (showResizeControl && targets.length > 0) {
      const selectedView = targets[0]
      color = withUnderlyingTarget<string>(
        selectedView,
        this.props.projectContents,
        this.props.nodeModules,
        this.props.openFile,
        '',
        (success, element, underlyingTarget, underlyingFilePath) => {
          return getSelectionColor(
            underlyingTarget,
            getUtopiaJSXComponentsFromSuccess(success),
            this.props.componentMetadata,
            success.imports,
            this.props.focusedElementPath,
          )
        },
      )
    }

    return (
      <React.Fragment>
        {!showResizeControl ? null : (
          <YogaResizeControl
            {...this.props}
            target={targets[0]}
            targetElement={
              MetadataUtils.findElementByTemplatePath(this.props.componentMetadata, targets[0])!
            }
            color={color}
          />
        )}
      </React.Fragment>
    )
  }
}
