/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { jsx } from '@emotion/react'
import {
  InspectorSectionIcons,
  Icons,
  FlexRow,
  SquareButton,
  FunctionIcons,
  InspectorSectionHeader,
  useColorTheme,
} from '../../uuiui'
import { useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { metadataSelector, selectedViewsSelector } from './inpector-selectors'
import {
  addFlexLayoutStrategies,
  removeFlexLayoutStrategies,
  runStrategies,
} from './inspector-strategies'
import { detectAreElementsFlexContainers } from './inspector-common'

interface AddRemoveLayoutSystemControlProps {}

export const AddRemoveLayouSystemControl = React.memo<AddRemoveLayoutSystemControlProps>(() => {
  const isFlexLayoutedContainer = useEditorState('metadata')(
    (store) =>
      detectAreElementsFlexContainers(metadataSelector(store), selectedViewsSelector(store)),
    'AddRemoveLayouSystemControl, isFlexLayoutedContainer',
  )

  const dispatch = useEditorState('restOfStore')(
    (store) => store.dispatch,
    'AddRemoveLayouSystemControl dispatch',
  )
  const elementMetadataRef = useRefEditorState('metadata')(metadataSelector)
  const selectedViewsRef = useRefEditorState('selectedHighlightedViews')(selectedViewsSelector)

  const addLayoutSystem = React.useCallback(
    () =>
      runStrategies(
        dispatch,
        elementMetadataRef.current,
        selectedViewsRef.current,
        addFlexLayoutStrategies,
      ),
    [dispatch, elementMetadataRef, selectedViewsRef],
  )

  const removeLayoutSystem = React.useCallback(
    () =>
      runStrategies(
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
        marginTop: 8,
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
        <InspectorSectionIcons.LayoutSystem />
        <span>New Layout System</span>
      </FlexRow>
      {isFlexLayoutedContainer ? (
        <SquareButton highlight onClick={removeLayoutSystem}>
          <FunctionIcons.Remove />
        </SquareButton>
      ) : (
        <SquareButton highlight onClick={addLayoutSystem}>
          <Icons.Plus style={{ opacity: 'var(--buttonContentOpacity)' }} />
        </SquareButton>
      )}
    </InspectorSectionHeader>
  )
})
