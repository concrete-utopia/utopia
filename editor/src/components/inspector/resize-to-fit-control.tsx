import React from 'react'
import { FlexRow, Icn, Tooltip } from '../../uuiui'
import { EditorAction, EditorDispatch } from '../editor/action-types'
import { useDispatch } from '../editor/store/dispatch-context'
import { Substores, useEditorState } from '../editor/store/store-hook'
import { setPropHugStrategies } from './inspector-strategies/inspector-strategies'
import { executeFirstApplicableStrategy } from './inspector-strategies/inspector-strategy'

export const ResizeToFitControlTestId = 'ResizeToFitControlTestId'

interface ResizeToFitControlProps {}

export const ResizeToFitControl = React.memo<ResizeToFitControlProps>(() => {
  const dispatch = useDispatch()
  const selectedViews = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews,
    'ResizeToFitControl selectedViews',
  )

  const metadata = useEditorState(
    Substores.metadata,
    (store) => store.editor.jsxMetadata,
    'ResizeToFitControl metadata',
  )

  const onMouseDown = React.useCallback(() => {
    let batchedActions: Array<EditorAction> = []
    const batchedDispatch: EditorDispatch = (actions) => {
      batchedActions.push(...actions)
    }
    executeFirstApplicableStrategy(
      batchedDispatch,
      metadata,
      selectedViews,
      setPropHugStrategies('horizontal'),
    )
    executeFirstApplicableStrategy(
      batchedDispatch,
      metadata,
      selectedViews,
      setPropHugStrategies('vertical'),
    )
    if (batchedActions.length > 0) {
      dispatch(batchedActions)
    }
  }, [dispatch, metadata, selectedViews])

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
