import React from 'react'
import { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { ElementPath } from '../../core/shared/project-file-types'
import { Icons, useColorTheme } from '../../uuiui'
import { EditorDispatch } from '../editor/action-types'
import { useEditorState } from '../editor/store/store-hook'
import { FlexDirection } from './common/css-utils'
import { filterKeepFlexContainers } from './inspector-common'
import {
  removeFlexDirectionStrategies,
  runStrategies,
  updateFlexDirectionStrategies,
} from './inspector-strategies'

interface FlexDirectionToggleProps {
  flexDirection: FlexDirection
}

export const FlexDirectionToggle = React.memo<FlexDirectionToggleProps>(({ flexDirection }) => {
  const dispatch = useEditorState((store) => store.dispatch, 'FlexDirectionToggle dispatch')
  const metadata = useEditorState(
    (store) => store.editor.jsxMetadata,
    'FlexDirectionToggle metadata',
  )
  const selectedViews = useEditorState(
    (store) => store.editor.selectedViews,
    'FlexDirectionToggle selectedViews',
  )

  const colorTheme = useColorTheme()

  const handleColumnClick = React.useCallback(
    (e: React.MouseEvent) =>
      maybeSetFlexDirection(dispatch, metadata, selectedViews, e.button === 2 ? null : 'column'),
    [dispatch, metadata, selectedViews],
  )

  const handleRowClick = React.useCallback(
    (e: React.MouseEvent) =>
      maybeSetFlexDirection(dispatch, metadata, selectedViews, e.button === 2 ? null : 'row'),
    [dispatch, metadata, selectedViews],
  )

  if (filterKeepFlexContainers(metadata, selectedViews).length === 0) {
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
