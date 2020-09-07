import * as React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { DragState } from '../canvas-types'
import { MultiselectResizeControl } from './multiselect-resize-control'
import { ControlProps } from './new-canvas-controls'
import { anyInstanceYogaLayouted } from './select-mode/yoga-utils'
import { isScenePath } from '../../../core/shared/template-path'

export interface ConstraintsControlProps extends ControlProps {
  dragState: DragState | null
  sideResizeOnly: boolean
}

export class ConstraintsControls extends React.Component<ConstraintsControlProps> {
  render() {
    const anySelectedElementIsYogaLayouted = anyInstanceYogaLayouted(
      this.props.componentMetadata,
      this.props.selectedViews,
    )
    const anyUnknownElements = MetadataUtils.anyUnknownOrGeneratedElements(
      this.props.rootComponents,
      this.props.componentMetadata,
      this.props.selectedViews,
    )
    const validResizeDragState =
      this.props.dragState == null || this.props.dragState.type === 'RESIZE_DRAG_STATE'

    return (
      <>
        {anySelectedElementIsYogaLayouted || anyUnknownElements || !validResizeDragState ? null : (
          <MultiselectResizeControl
            {...this.props}
            dragState={
              this.props.dragState != null && this.props.dragState.type === 'RESIZE_DRAG_STATE'
                ? this.props.dragState
                : null
            }
            sideResizeOnly={this.props.sideResizeOnly}
          />
        )}
      </>
    )
  }
}
