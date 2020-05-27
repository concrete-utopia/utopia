import * as React from 'react'
import * as EditorActions from '../../editor/actions/actions'
import { Mode } from '../../editor/editor-modes'
import { EditorDispatch } from '../../editor/action-types'

interface DeselectControlProps {
  mode: Mode
  dispatch: EditorDispatch
}

export class DeselectControl extends React.Component<DeselectControlProps> {
  onMouseDown = () => {
    if (this.props.mode.type !== 'insert') {
      this.props.dispatch([EditorActions.clearSelection()], 'canvas')
    }
  }

  render() {
    return (
      <div
        className='deselect-control'
        style={{
          position: 'absolute',
          top: 0,
          left: 0,
          width: '100%',
          height: '100%',
        }}
        onMouseDown={this.onMouseDown}
      />
    )
  }
}
