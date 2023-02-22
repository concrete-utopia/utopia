import React, { CSSProperties } from 'react'
import { createSelector } from 'reselect'
import { FlexRow, Icn, Tooltip } from '../../uuiui'
import { applyCommandsAction } from '../editor/actions/action-creators'
import { useDispatch } from '../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { metadataSelector, selectedViewsSelector } from './inpector-selectors'
import {
  notFixedSizeOnBothAxes,
  resizeToFillCommands,
  resizeToFitCommands,
  sizeToVisualDimensions,
  toggleResizeToFillSetToFixed,
  toggleResizeToFitSetToFixed,
} from './inspector-common'

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

  const onResizeToFit = React.useCallback(() => {
    const commands = resizeToFitCommands(metadataRef.current, selectedViewsRef.current)
    if (commands.length > 0) {
      dispatch([applyCommandsAction(commands)])
    }
  }, [dispatch, metadataRef, selectedViewsRef])

  const onResizeToFill = React.useCallback(() => {
    const commands = resizeToFillCommands(metadataRef.current, selectedViewsRef.current)
    if (commands.length > 0) {
      dispatch([applyCommandsAction(commands)])
    }
  }, [dispatch, metadataRef, selectedViewsRef])

  const onSetToFixedSize = React.useCallback(() => {
    const commands = selectedViewsRef.current.flatMap((e) =>
      sizeToVisualDimensions(metadataRef.current, e),
    )
    if (commands.length > 0) {
      dispatch([applyCommandsAction(commands)])
    }
  }, [dispatch, metadataRef, selectedViewsRef])

  const disabledStyles: CSSProperties = notFixedSizeOnEitherAxisState
    ? {
        cursor: 'pointer',
        opacity: 0.5,
        pointerEvents: 'none',
      }
    : { cursor: 'pointer' }

  return (
    <FlexRow style={{ gap: 12 }}>
      <Tooltip title={'Resize to Fit'}>
        <div
          data-testid={ResizeToFitControlTestId}
          onMouseDown={onResizeToFit}
          style={disabledStyles}
        >
          <Icn
            type='fitToChildren'
            color='main'
            category='layout/commands'
            width={18}
            height={18}
          />
        </div>
      </Tooltip>
      <Tooltip title={'Resize to Fill'}>
        <div onMouseDown={onResizeToFill} style={disabledStyles}>
          <Icn type='growToParent' color='main' category='layout/commands' width={18} height={18} />
        </div>
      </Tooltip>
      <Tooltip title={'Fixed size'}>
        <div onMouseDown={onSetToFixedSize} style={disabledStyles}>
          <Icn type='fixed' color='main' category='layout/commands' width={16} height={16} />
        </div>
      </Tooltip>
    </FlexRow>
  )
})
