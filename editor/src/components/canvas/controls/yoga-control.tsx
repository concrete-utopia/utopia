import * as React from 'react'
import { FlexStretch, Sides } from 'utopia-api'
import { colorTheme } from 'uuiui'
import { FlexLayoutHelpers, LayoutHelpers } from '../../../core/layout/layout-helpers'
import {
  findJSXElementAtPath,
  getSceneMetadataOrElementInstanceMetadata,
  MetadataUtils,
} from '../../../core/model/element-metadata-utils'
import { ElementInstanceMetadata } from '../../../core/shared/element-template'
import { InstancePath } from '../../../core/shared/project-file-types'
import { defaultEither, eitherToMaybe, mapEither, right } from '../../../core/shared/either'
import Utils from '../../../utils/utils'
import { CanvasRectangle, canvasRectangle } from '../../../core/shared/math-utils'
import { EditorDispatch } from '../../editor/action-types'
import * as TP from '../../../core/shared/template-path'
import { ResizeDragState } from '../canvas-types'
import { getOriginalFrames } from '../canvas-utils'
import { ControlProps } from './new-canvas-controls'
import { getSelectionColor } from './outline-control'
import { ResizeRectangle } from './size-box'
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

    let labels: {
      vertical: 'FlexFlexBasis' | 'FlexCrossBasis'
      horizontal: 'FlexFlexBasis' | 'FlexCrossBasis'
    } = {
      vertical: 'FlexFlexBasis',
      horizontal: 'FlexCrossBasis',
    }
    const parentPath = TP.parentPath(this.props.target)
    const parentElement = findJSXElementAtPath(
      parentPath,
      this.props.rootComponents,
      this.props.componentMetadata,
    )
    if (parentElement != null) {
      const flexDirection = eitherToMaybe(FlexLayoutHelpers.getMainAxis(right(parentElement.props)))
      if (flexDirection === 'vertical') {
        // column, column-reverse
        labels = {
          horizontal: 'FlexCrossBasis',
          vertical: 'FlexFlexBasis',
        }
      } else {
        labels = {
          vertical: 'FlexFlexBasis',
          horizontal: 'FlexCrossBasis',
        }
      }
    }
    const yogaSize = this.getYogaSize(visualSize)

    return (
      <ResizeRectangle
        targetComponentMetadata={this.props.targetElement}
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
        labels={labels}
        propertyTargetOptions={this.props.propertyTargetOptions}
        propertyTargetSelectedIndex={this.props.propertyTargetSelectedIndex}
        setTargetOptionsArray={this.props.setTargetOptionsArray}
      />
    )
  }
}

export interface YogaControlsProps extends ControlProps {
  dragState: ResizeDragState | null
}

export class YogaControls extends React.Component<YogaControlsProps> {
  render() {
    if (this.props.showYogaChildControls) {
      const targets = Utils.flatMapArray((view) => {
        const elementInstance = MetadataUtils.getElementByInstancePathMaybe(
          this.props.componentMetadata,
          view,
        )
        if (MetadataUtils.isFlexLayoutedContainer(elementInstance)) {
          return elementInstance?.children.map((child) => child.templatePath) ?? []
        } else {
          return []
        }
      }, TP.filterScenes(this.props.selectedViews))

      return targets.map((target) => {
        const instance = MetadataUtils.getElementByInstancePathMaybe(
          this.props.componentMetadata,
          target,
        )
        const createsYogaLayout = MetadataUtils.isFlexLayoutedContainer(instance)
        const color = getSelectionColor(
          target,
          this.props.rootComponents,
          this.props.componentMetadata,
          this.props.imports,
          createsYogaLayout,
          true,
        )

        if (instance == null) {
          return null
        }

        return (
          <YogaResizeControl
            {...this.props}
            key={TP.toString(target)}
            target={target}
            targetElement={instance}
            color={color}
          />
        )
      })
    } else {
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
          {!showResizeControl ? null : (
            <YogaResizeControl
              {...this.props}
              target={targets[0]}
              targetElement={
                MetadataUtils.getElementByInstancePathMaybe(
                  this.props.componentMetadata,
                  targets[0],
                )!
              }
              color={color}
            />
          )}
        </React.Fragment>
      )
    }
  }
}
