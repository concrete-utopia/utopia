import React from 'react'
import { createSelector } from 'reselect'
import { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { ElementPath } from '../../core/shared/project-file-types'
import { Icons, useColorTheme } from '../../uuiui'
import { useSetHoveredControlsHandlers } from '../canvas/controls/select-mode/select-mode-hooks'
import {
  SubduedPaddingControlProps,
  SubduedPaddingControl,
} from '../canvas/controls/select-mode/subdued-padding-control'
import { EdgePieces } from '../canvas/padding-utils'
import { EditorDispatch } from '../editor/action-types'
import { useDispatch } from '../editor/store/dispatch-context'
import { AllElementProps } from '../editor/store/editor-state'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { FlexDirection } from './common/css-utils'
import { CanvasControlWithProps } from './common/inspector-atoms'
import {
  flexDirectionSelector,
  metadataSelector,
  selectedViewsSelector,
} from './inpector-selectors'
import { numberOfFlexContainers } from './inspector-common'
import {
  removeFlexDirectionStrategies,
  updateFlexDirectionStrategies,
} from './inspector-strategies/inspector-strategies'
import { executeFirstApplicableStrategy } from './inspector-strategies/inspector-strategy'

const nFlexContainersSelector = createSelector(
  metadataSelector,
  selectedViewsSelector,
  numberOfFlexContainers,
)

export const FlexDirectionControlTestId = 'FlexDirectionControlTestId'

export const FlexDirectionToggleTestId = (direction: FlexDirection): string =>
  `FlexDirectionToggle-${direction}`

export const FlexDirectionToggle = React.memo(() => {
  const dispatch = useDispatch()

  const flexDirection = useEditorState(
    Substores.metadata,
    flexDirectionSelector,
    'FlexDirectionToggle flexDirection',
  )

  const metadataRef = useRefEditorState(metadataSelector)
  const selectedViewsRef = useRefEditorState(selectedViewsSelector)
  const allElementPropsRef = useRefEditorState((store) => store.editor.allElementProps)

  const nFlexContainers = useEditorState(
    Substores.metadata,
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
        allElementPropsRef.current,
        e.button === 0 ? 'column' : null,
      ),
    [allElementPropsRef, dispatch, metadataRef, selectedViewsRef],
  )

  const handleRowClick = React.useCallback(
    (e: React.MouseEvent) =>
      maybeSetFlexDirection(
        dispatch,
        metadataRef.current,
        selectedViewsRef.current,
        allElementPropsRef.current,
        e.button === 0 ? 'row' : null,
      ),
    [allElementPropsRef, dispatch, metadataRef, selectedViewsRef],
  )

  const paddingControlsForHover: Array<CanvasControlWithProps<SubduedPaddingControlProps>> =
    React.useMemo(
      () =>
        EdgePieces.map((side) => ({
          control: SubduedPaddingControl,
          props: {
            side: side,
            hoveredOrFocused: 'hovered',
          },
          key: `subdued-padding-control-hovered-${side}`,
        })),
      [],
    )

  const { onMouseEnter, onMouseLeave } = useSetHoveredControlsHandlers<SubduedPaddingControlProps>()
  const onMouseEnterWithPaddingControls = React.useCallback(
    () => onMouseEnter(paddingControlsForHover),
    [onMouseEnter, paddingControlsForHover],
  )

  if (nFlexContainers === 0) {
    return null
  }

  return (
    <div
      data-testid={FlexDirectionControlTestId}
      onMouseEnter={onMouseEnterWithPaddingControls}
      onMouseLeave={onMouseLeave}
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
  allElementProps: AllElementProps,
  desiredFlexDirection: FlexDirection | null,
) {
  const strategies =
    desiredFlexDirection == null
      ? removeFlexDirectionStrategies()
      : updateFlexDirectionStrategies(desiredFlexDirection)
  executeFirstApplicableStrategy(dispatch, metadata, selectedViews, allElementProps, strategies)
}
