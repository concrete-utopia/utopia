import * as React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import Utils from '../../../utils/utils'
import * as TP from '../../../core/shared/template-path'
import { ControlProps } from './new-canvas-controls'
import { anyInstanceYogaLayouted } from './select-mode/yoga-utils'
import { getSelectionColor } from './outline-control'

const Size = 6

export class RepositionableControl extends React.Component<ControlProps> {
  render() {
    const anySelectedElementIsYogaLayouted = anyInstanceYogaLayouted(
      this.props.componentMetadata,
      this.props.selectedViews,
    )
    const outlineOffset = 0.5 / this.props.scale

    let indicators: JSX.Element[] = []
    Utils.fastForEach(this.props.selectedViews, (selectedView) => {
      const frame = MetadataUtils.getFrameInCanvasCoords(selectedView, this.props.componentMetadata)
      if (frame == null) {
        return
      }

      const instance = TP.isScenePath(selectedView)
        ? null
        : MetadataUtils.getElementByInstancePathMaybe(
            this.props.componentMetadata.elements,
            selectedView,
          )
      const createsYogaLayout = MetadataUtils.isFlexLayoutedContainer(instance)
      const isPositionRelative = instance?.specialSizeMeasurements.position === 'relative'
      const isFlow = MetadataUtils.isFlowElement(instance)
      const selectionColor = getSelectionColor(
        selectedView,
        this.props.rootComponents,
        this.props.componentMetadata,
        this.props.imports,
        createsYogaLayout,
        anySelectedElementIsYogaLayouted,
        isPositionRelative,
        isFlow,
      )

      indicators.push(
        <div
          key={TP.toComponentId(selectedView)}
          className='role-outline'
          style={{
            position: 'absolute',
            left: this.props.canvasOffset.x + frame.x - Size / 2 + outlineOffset,
            top: this.props.canvasOffset.y + frame.y - Size / 2 + outlineOffset,
            borderRadius: '50%',
            width: Size,
            height: Size,
            backgroundColor: selectionColor,
            pointerEvents: 'none',
          }}
        />,
      )
    })

    return <>{...indicators}</>
  }
}
