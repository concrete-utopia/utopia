import { useEditorState } from '../../../editor/store/store-hook'
import CanvasActions from '../../canvas-actions'
import * as React from 'react'

const ApplyOnMouseUp_ = () => {
  const dispatch = useEditorState((store) => store.dispatch, 'ApplyOnMouseUp dispatch')

  const onMouseUp = React.useCallback(() => {
    dispatch([CanvasActions.clearInteractionState(true)], 'everyone')
  }, [dispatch])

  return (
    <div
      key={'apply-on-mouse-up'}
      style={{
        position: 'absolute',
        left: 0,
        top: 0,
        width: '100%',
        height: '100%',
      }}
      onMouseUp={onMouseUp}
    />
  )
}
ApplyOnMouseUp_.displayName = 'ApplyOnMouseUp'
export const ApplyOnMouseUp = React.memo(ApplyOnMouseUp_)
