import React from 'react'
import { createSelector } from 'reselect'
import { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { ElementPath } from '../../core/shared/project-file-types'
import { Icons, useColorTheme } from '../../uuiui'
import { EditorDispatch } from '../editor/action-types'
import { useDispatch } from '../editor/store/dispatch-context'
import { useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { FlexDirection } from './common/css-utils'
import {
  flexDirectionSelector,
  metadataSelector,
  selectedViewsSelector,
} from './inpector-selectors'
import { numberOfFlexContainers } from './inspector-common'
import {
  removeFlexDirectionStrategies,
  runStrategies,
  updateFlexDirectionStrategies,
} from './inspector-strategies'

const nFlexContainersSelector = createSelector(
  metadataSelector,
  selectedViewsSelector,
  numberOfFlexContainers,
)
export const FlexDirectionToggleTestId = (direction: FlexDirection): string =>
  `FlexDirectionToggle-${direction}`

export const FlexDirectionToggle = React.memo(() => {
  const dispatch = useDispatch()

  const flexDirection = useEditorState(flexDirectionSelector, 'FlexDirectionToggle flexDirection')

  const metadataRef = useRefEditorState(metadataSelector)
  const selectedViewsRef = useRefEditorState(selectedViewsSelector)

  const nFlexContainers = useEditorState(
    nFlexContainersSelector,
    'FlexDirectionToggle nFlexContainers',
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
        data-testid={FlexDirectionToggleTestId('row')}
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
        data-testid={FlexDirectionToggleTestId('column')}
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
