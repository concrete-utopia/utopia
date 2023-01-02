import React from 'react'
import { when } from '../../utils/react-conditionals'
import { useEditorState } from '../editor/store/store-hook'
import { AddRemoveLayouSystemControl } from './add-remove-layout-system-control'
import { FlexDirectionToggle } from './flex-direction-control'
import { selectedViewsSelector, metadataSelector } from './inpector-selectors'
import { detectAreElementsFlexContainers, detectFlexDirection } from './inspector-common'
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
    (store) =>
      detectAreElementsFlexContainers(metadataSelector(store), selectedViewsSelector(store)),
    'FlexSection areAllElementsInFlexLayout',
  )

  return (
    <div>
      <AddRemoveLayouSystemControl />
      {when(
        allElementsInFlexLayout,
        <>
          <FlexDirectionToggle flexDirection={flexDirection} />
          <NineBlockControl flexDirection={flexDirection} />
        </>,
      )}
    </div>
  )
})
