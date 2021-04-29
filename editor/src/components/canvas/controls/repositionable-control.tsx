import * as React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import Utils from '../../../utils/utils'
import * as TP from '../../../core/shared/template-path'
import { ControlProps } from './new-canvas-controls'
import { getSelectionColor } from './outline-control'
import { getJSXComponentsAndImportsForPathInnerComponent } from '../../editor/store/editor-state'

const Size = 6

export class RepositionableControl extends React.Component<ControlProps> {
  render() {
    const outlineOffset = 0.5 / this.props.scale

    let indicators: JSX.Element[] = []
    Utils.fastForEach(this.props.selectedViews, (selectedView) => {
      const frame = MetadataUtils.getFrameInCanvasCoords(selectedView, this.props.componentMetadata)
      if (frame == null) {
        return
      }

      const { components, imports } = getJSXComponentsAndImportsForPathInnerComponent(
        selectedView,
        this.props.openFile,
        this.props.projectContents,
        this.props.nodeModules,
        this.props.transientState.filesState,
        this.props.resolve,
      )
      const selectionColor = getSelectionColor(
        selectedView,
        components,
        this.props.componentMetadata,
        imports,
        this.props.focusedElementPath,
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
