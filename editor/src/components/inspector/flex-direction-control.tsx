import React from 'react'
import { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { ElementPath } from '../../core/shared/project-file-types'
import { Icons, useColorTheme } from '../../uuiui'
import { EditorDispatch } from '../editor/action-types'
import { useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { FlexDirection } from './common/css-utils'
import { metadataSelector, selectedViewsSelector } from './inpector-selectors'
import { filterKeepFlexContainers } from './inspector-common'
import {
  removeFlexDirectionStrategies,
  runStrategies,
  updateFlexDirectionStrategies,
} from './inspector-strategies'

interface FlexDirectionToggleProps {
  flexDirection: FlexDirection | null
}

export const FlexDirectionToggle = React.memo<FlexDirectionToggleProps>(({ flexDirection }) => {
  const dispatch = useEditorState('restOfStore')(
    (store) => store.dispatch,
    'FlexDirectionToggle dispatch',
  )
  const metadataRef = useRefEditorState('metadata')(metadataSelector)
  const selectedViewsRef = useRefEditorState('selectedViews')(selectedViewsSelector)
  const nFlexContainers = useEditorState('metadata')(
    (store) =>
      filterKeepFlexContainers(metadataSelector(store), selectedViewsSelector(store)).length,
    'FlexDirectionToggle, nFlexContainers',
  )

  const colorTheme = useColorTheme()

  const handleColumnClick = React.useCallback(
    (e: React.MouseEvent) =>
      maybeSetFlexDirection(
        dispatch,
        metadataRef.current,
        selectedViewsRef.current,
        e.button === 0 ? 'column' : null,
      ),
    [dispatch, metadataRef, selectedViewsRef],
  )

  const handleRowClick = React.useCallback(
    (e: React.MouseEvent) =>
      maybeSetFlexDirection(
        dispatch,
        metadataRef.current,
        selectedViewsRef.current,
        e.button === 0 ? 'row' : null,
      ),
    [dispatch, metadataRef, selectedViewsRef],
  )

  if (nFlexContainers === 0) {
    return null
  }

  return (
    <div
      style={{
        display: 'grid',
        gridTemplateRows: '1fr',
        gridTemplateColumns: '1fr 1fr',
        aspectRatio: '2',
        width: 50,
      }}
    >
      <div
        onMouseDown={handleRowClick}
        style={{
          aspectRatio: '1',
          backgroundColor: (flexDirection === 'row' ? colorTheme.fg8 : colorTheme.fg9).value,
          cursor: 'pointer',
          display: 'flex',
          justifyContent: 'center',
          alignItems: 'center',
          padding: 3,
        }}
      >
        <Icons.FlexRow />
      </div>
      <div
        onMouseDown={handleColumnClick}
        style={{
          aspectRatio: '1',
          backgroundColor: (flexDirection === 'column' ? colorTheme.fg8 : colorTheme.fg9).value,
          cursor: 'pointer',
          display: 'flex',
          justifyContent: 'center',
          alignItems: 'center',
          padding: 3,
        }}
      >
        <Icons.FlexColumn />
      </div>
    </div>
  )
})

function maybeSetFlexDirection(
  dispatch: EditorDispatch,
  metadata: ElementInstanceMetadataMap,
  selectedViews: ElementPath[],
  desiredFlexDirection: FlexDirection | null,
) {
  const strategies =
    desiredFlexDirection == null
      ? removeFlexDirectionStrategies()
      : updateFlexDirectionStrategies(desiredFlexDirection)
  runStrategies(dispatch, metadata, selectedViews, strategies)
}
