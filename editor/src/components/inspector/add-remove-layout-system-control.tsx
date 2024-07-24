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
  Icn,
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

export const AddRemoveLayoutSystemControlTestId = (): string => 'AddRemoveLayoutSystemControlTestId'

interface AddRemoveLayoutSystemControlProps {}

export const AddRemoveLayoutSystemControl = React.memo<AddRemoveLayoutSystemControlProps>(() => {
  const isFlexLayoutedContainer = useEditorState(
    Substores.metadata,
    (store) =>
      detectAreElementsFlexContainers(metadataSelector(store), selectedViewsSelector(store)),
    'AddRemoveLayoutSystemControl isFlexLayoutedContainer',
  )

  const dispatch = useDispatch()
  const elementMetadataRef = useRefEditorState(metadataSelector)
  const selectedViewsRef = useRefEditorState(selectedViewsSelector)
  const elementPathTreeRef = useRefEditorState((store) => store.editor.elementPathTree)
  const allElementPropsRef = useRefEditorState((store) => store.editor.allElementProps)

  const addLayoutSystem = React.useCallback(
    () =>
      executeFirstApplicableStrategy(
        dispatch,
        addFlexLayoutStrategies(
          elementMetadataRef.current,
          selectedViewsRef.current,
          elementPathTreeRef.current,
          allElementPropsRef.current,
        ),
      ),
    [allElementPropsRef, dispatch, elementMetadataRef, elementPathTreeRef, selectedViewsRef],
  )

  const removeLayoutSystem = React.useCallback(
    () =>
      executeFirstApplicableStrategy(
        dispatch,
        removeFlexLayoutStrategies(
          elementMetadataRef.current,
          selectedViewsRef.current,
          elementPathTreeRef.current,
        ),
      ),
    [dispatch, elementMetadataRef, elementPathTreeRef, selectedViewsRef],
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
          data-testid={AddRemoveLayoutSystemControlTestId()}
          highlight
          onClick={removeLayoutSystem}
        >
          <Icn category='semantic' type='cross' width={12} height={12} />
        </SquareButton>
      ) : (
        <SquareButton
          data-testid={AddRemoveLayoutSystemControlTestId()}
          highlight
          onClick={addLayoutSystem}
        >
          <Icn category='semantic' type='plus' width={12} height={12} />
        </SquareButton>
      )}
    </InspectorSectionHeader>
  )
})
