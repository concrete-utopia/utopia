import React from 'react'
import { when } from '../../utils/react-conditionals'
import { useEditorState } from '../editor/store/store-hook'
import { AddRemoveLayouSystemControl } from './add-remove-layout-system-control'
import { FlexDirectionToggle } from './flex-direction-control'
import { selectedViewsSelector, metadataSelector } from './inpector-selectors'
import { detectAreElementsInFlexLayout, detectFlexDirection } from './inspector-common'
import { NineBlockControl } from './nine-block-controls'

export const FlexSection = React.memo(() => {
  const flexDirection = useEditorState(
    (store) =>
      selectedViewsSelector(store).length === 0
        ? 'row'
        : detectFlexDirection(metadataSelector(store), selectedViewsSelector(store)),
    'FlexSection flexDirection',
  )

  const allElementsInFlexLayout = useEditorState(
    (store) => detectAreElementsInFlexLayout(metadataSelector(store), selectedViewsSelector(store)),
    'FlexSection areAllElementsInFlexLayout',
  )

  return (
    <>
      {when(
        allElementsInFlexLayout,
        <div>
          <AddRemoveLayouSystemControl />
          <FlexDirectionToggle flexDirection={flexDirection} />
          <NineBlockControl flexDirection={flexDirection} />
        </div>,
      )}
    </>
  )
})
