import React from 'react'
import { createSelector } from 'reselect'
import { FlexRow, Icn, Tooltip } from '../../uuiui'
import { applyCommandsAction } from '../editor/actions/action-creators'
import { useDispatch } from '../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { metadataSelector, selectedViewsSelector } from './inpector-selectors'
import { notFixedSizeOnBothAxes, toggleResizeToFitSetToFixed } from './inspector-common'

export const ResizeToFitControlTestId = 'ResizeToFitControlTestId'

const notFixedSizeOnEitherAxisSelector = createSelector(
  metadataSelector,
  selectedViewsSelector,
  notFixedSizeOnBothAxes,
)

interface ResizeToFitControlProps {}

export const ResizeToFitControl = React.memo<ResizeToFitControlProps>(() => {
  const dispatch = useDispatch()
  const selectedViewsRef = useRefEditorState((store) => store.editor.selectedViews)

  const metadataRef = useRefEditorState((store) => store.editor.jsxMetadata)

  const notFixedSizeOnEitherAxisState = useEditorState(
    Substores.metadata,
    notFixedSizeOnEitherAxisSelector,
    'ResizeToFitControl notFixedSizeOnEitherAxisState',
  )

  const onMouseDown = React.useCallback(() => {
    const commands = toggleResizeToFitSetToFixed(metadataRef.current, selectedViewsRef.current)
    if (commands.length > 0) {
      dispatch([applyCommandsAction(commands)])
    }
  }, [dispatch, metadataRef, selectedViewsRef])

  const toolTipText = notFixedSizeOnEitherAxisState ? 'Set to Fixed Size' : 'Resize to Fit'

  return (
    <Tooltip title={toolTipText}>
      <FlexRow
        data-testid={ResizeToFitControlTestId}
        onMouseDown={onMouseDown}
        style={{ cursor: 'pointer' }}
      >
        {notFixedSizeOnEitherAxisState ? (
          <Icn type='growToParent' color='main' category='layout/commands' width={18} height={18} />
        ) : (
          <Icn
            type='fitToChildren'
            color='main'
            category='layout/commands'
            width={18}
            height={18}
          />
        )}
      </FlexRow>
    </Tooltip>
  )
})
