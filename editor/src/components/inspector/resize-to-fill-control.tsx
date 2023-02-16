import React from 'react'
import { createSelector } from 'reselect'
import { FlexRow, Icn, Tooltip } from '../../uuiui'
import { applyCommandsAction } from '../editor/actions/action-creators'
import { useDispatch } from '../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { metadataSelector, selectedViewsSelector } from './inpector-selectors'
import {
  getFillFixedHugOptions,
  notFixedSizeOnBothAxes,
  toggleResizeToFillSetToFixed,
} from './inspector-common'

export const ResizeToFitControlTestId = 'ResizeToFitControlTestId'

const notFixedSizeOnEitherAxisSelector = createSelector(
  metadataSelector,
  selectedViewsSelector,
  notFixedSizeOnBothAxes,
)

interface ResizeToFillControlProps {}

const optionsSelector = createSelector(
  metadataSelector,
  selectedViewsSelector,
  (metadata, selectedViews) => getFillFixedHugOptions(metadata, selectedViews).includes('fill'),
)

export const ResizeToFillControl = React.memo<ResizeToFillControlProps>(() => {
  const dispatch = useDispatch()
  const selectedViewsRef = useRefEditorState((store) => store.editor.selectedViews)

  const metadataRef = useRefEditorState((store) => store.editor.jsxMetadata)

  const isFillApplicable = useEditorState(
    Substores.metadata,
    optionsSelector,
    'ResizeToFitControl isFillApplicable',
  )

  const notFixedSizeOnEitherAxisState = useEditorState(
    Substores.metadata,
    notFixedSizeOnEitherAxisSelector,
    'ResizeToFitControl notFixedSizeOnEitherAxisState',
  )

  const onMouseDown = React.useCallback(() => {
    if (!isFillApplicable) {
      return
    }

    const commands = toggleResizeToFillSetToFixed(metadataRef.current, selectedViewsRef.current)
    if (commands.length > 0) {
      dispatch([applyCommandsAction(commands)])
    }
  }, [dispatch, isFillApplicable, metadataRef, selectedViewsRef])

  const isFillMode = notFixedSizeOnEitherAxisState && isFillApplicable
  const toolTipText = notFixedSizeOnEitherAxisState ? 'Set to Fixed Size' : 'Resize to Fill'

  return (
    <Tooltip title={toolTipText}>
      <FlexRow
        data-testid={ResizeToFitControlTestId}
        onMouseDown={onMouseDown}
        style={{
          cursor: isFillMode ? 'pointer' : 'auto',
          opacity: isFillApplicable ? 1 : 0.5,
        }}
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
