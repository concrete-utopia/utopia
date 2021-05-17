import * as React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
import {
  ElementOriginType,
  isUnknownOrGeneratedElement,
} from '../../../core/shared/project-file-types'
import { DragState } from '../canvas-types'
import { MultiselectResizeControl } from './multiselect-resize-control'
import { ControlProps } from './new-canvas-controls'
import { anyInstanceYogaLayouted } from './select-mode/yoga-utils'

export interface ConstraintsControlProps extends ControlProps {
  dragState: DragState | null
}

export class ConstraintsControls extends React.Component<ConstraintsControlProps> {
  render() {
    const anySelectedElementIsYogaLayouted = anyInstanceYogaLayouted(
      this.props.componentMetadata,
      this.props.selectedViews,
    )
    const anyUnknownElements = MetadataUtils.anyUnknownOrGeneratedElements(
      this.props.projectContents,
      this.props.nodeModules,
      this.props.openFile,
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
          />
        )}
      </>
    )
  }
}
