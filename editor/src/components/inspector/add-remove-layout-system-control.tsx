/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { jsx } from '@emotion/react'
import {
  Icons,
  FlexRow,
  SquareButton,
  FunctionIcons,
  InspectorSectionHeader,
  useColorTheme,
} from '../../uuiui'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { metadataSelector, selectedViewsSelector } from './inpector-selectors'
import {
  addFlexLayoutStrategies,
  removeFlexLayoutStrategies,
} from './inspector-strategies/inspector-strategies'
import { detectAreElementsFlexContainers } from './inspector-common'
import { executeFirstApplicableStrategy } from './inspector-strategies/inspector-strategy'
import { useDispatch } from '../editor/store/dispatch-context'

export const AddRemoveLayouSystemControlTestId = (): string => 'AddRemoveLayouSystemControlTestId'

interface AddRemoveLayoutSystemControlProps {}

export const AddRemoveLayouSystemControl = React.memo<AddRemoveLayoutSystemControlProps>(() => {
  const isFlexLayoutedContainer = useEditorState(
    Substores.metadata,
    (store) =>
      detectAreElementsFlexContainers(metadataSelector(store), selectedViewsSelector(store)),
    'AddRemoveLayouSystemControl, isFlexLayoutedContainer',
  )

  const dispatch = useDispatch()
  const elementMetadataRef = useRefEditorState(metadataSelector)
  const selectedViewsRef = useRefEditorState(selectedViewsSelector)

  const addLayoutSystem = React.useCallback(
    () =>
      executeFirstApplicableStrategy(
        dispatch,
        elementMetadataRef.current,
        selectedViewsRef.current,
        addFlexLayoutStrategies,
      ),
    [dispatch, elementMetadataRef, selectedViewsRef],
  )

  const removeLayoutSystem = React.useCallback(
    () =>
      executeFirstApplicableStrategy(
        dispatch,
        elementMetadataRef.current,
        selectedViewsRef.current,
        removeFlexLayoutStrategies,
      ),
    [dispatch, elementMetadataRef, selectedViewsRef],
  )

  const colorTheme = useColorTheme()

  return (
    <InspectorSectionHeader
      css={{
        transition: 'color .1s ease-in-out',
        color: colorTheme.fg1.value,
        '--buttonContentOpacity': 0.3,
        '&:hover': {
          color: colorTheme.fg1.value,
          '--buttonContentOpacity': 1,
        },
      }}
    >
      <FlexRow
        style={{
          flexGrow: 1,
          alignSelf: 'stretch',
          gap: 8,
        }}
      >
        <span style={{ textTransform: 'capitalize', fontSize: '11px' }}>Layout System</span>
      </FlexRow>
      {isFlexLayoutedContainer ? (
        <SquareButton
          data-testid={AddRemoveLayouSystemControlTestId()}
          highlight
          onClick={removeLayoutSystem}
        >
          <FunctionIcons.Remove />
        </SquareButton>
      ) : (
        <SquareButton
          data-testid={AddRemoveLayouSystemControlTestId()}
          highlight
          onClick={addLayoutSystem}
        >
          <Icons.Plus style={{ opacity: 'var(--buttonContentOpacity)' }} />
        </SquareButton>
      )}
    </InspectorSectionHeader>
  )
})
