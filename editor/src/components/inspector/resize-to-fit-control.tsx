import React from 'react'
import { FlexRow, Icn, Tooltip } from '../../uuiui'
import { applyCommandsAction } from '../editor/actions/action-creators'
import { useDispatch } from '../editor/store/dispatch-context'
import { useRefEditorState } from '../editor/store/store-hook'
import { resizeToFitCommands } from './inspector-common'

export const ResizeToFitControlTestId = 'ResizeToFitControlTestId'

interface ResizeToFitControlProps {}

export const ResizeToFitControl = React.memo<ResizeToFitControlProps>(() => {
  const dispatch = useDispatch()
  const selectedViewsRef = useRefEditorState((store) => store.editor.selectedViews)

  const metadataRef = useRefEditorState((store) => store.editor.jsxMetadata)

  const onMouseDown = React.useCallback(() => {
    const commands = resizeToFitCommands(metadataRef.current, selectedViewsRef.current)
    if (commands.length > 0) {
      dispatch([applyCommandsAction(commands)])
    }
  }, [dispatch, metadataRef, selectedViewsRef])

  return (
    <Tooltip title={'Resize to Fit'}>
      <FlexRow
        data-testid={ResizeToFitControlTestId}
        onMouseDown={onMouseDown}
        style={{ cursor: 'pointer' }}
      >
        <Icn type='fitToChildren' color='main' category='layout/commands' width={18} height={18} />
      </FlexRow>
    </Tooltip>
  )
})
