import { useEditorState } from '../../../editor/store/store-hook'
import CanvasActions from '../../canvas-actions'
import * as React from 'react'

const ApplyOnMouseUp_ = () => {
  const dispatch = useEditorState((store) => store.dispatch, 'ApplyOnMouseUp dispatch')

  const onMouseUp = React.useCallback(() => {
    dispatch([CanvasActions.clearInteractionState(true)], 'everyone')
  }, [dispatch])

  React.useEffect(() => {
    window.addEventListener('mouseup', onMouseUp, true)
    return function cleanup() {
      window.removeEventListener('mouseup', onMouseUp)
    }
  }, [onMouseUp])

  return null
}
ApplyOnMouseUp_.displayName = 'ApplyOnMouseUp'
export const ApplyOnMouseUp = React.memo(ApplyOnMouseUp_)
