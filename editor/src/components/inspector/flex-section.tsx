import React from 'react'
import { createSelector } from 'reselect'
import { when } from '../../utils/react-conditionals'
import { Substores, useEditorState } from '../editor/store/store-hook'
import { AddRemoveLayoutSystemControl } from './add-remove-layout-system-control'
import { FlexDirectionToggle } from './flex-direction-control'
import { selectedViewsSelector, metadataSelector } from './inpector-selectors'
import { detectAreElementsFlexContainers } from './inspector-common'
import { NineBlockControl } from './nine-block-controls'
import { UIGridRow } from './widgets/ui-grid-row'
import {
  DisabledFlexGroupPicker,
  PaddingRow,
} from '../../components/inspector/sections/layout-section/layout-system-subsection/layout-system-controls'
import { SpacedPackedControl } from './spaced-packed-control'
import { ThreeBarControl } from './three-bar-control'
import { FlexGapControl } from './sections/layout-section/flex-container-subsection/flex-container-controls'
import { FlexContainerControls } from './sections/layout-section/flex-container-subsection/flex-container-subsection'
import { FlexCol } from 'utopia-api'

export const areElementsFlexContainersSelector = createSelector(
  metadataSelector,
  selectedViewsSelector,
  detectAreElementsFlexContainers,
)

export const FlexSection = React.memo(() => {
  const allElementsInFlexLayout = useEditorState(
    Substores.metadata,
    areElementsFlexContainersSelector,
    'FlexSection areAllElementsInFlexLayout',
  )

  return (
    <div>
      <AddRemoveLayoutSystemControl />
      {when(
        allElementsInFlexLayout,
        <FlexCol css={{ gap: 10 }}>
          <UIGridRow padded={true} variant='<-------------1fr------------->'>
            <DisabledFlexGroupPicker
              layoutSystem={null}
              providesCoordinateSystemForChildren={false}
            />
          </UIGridRow>
          <UIGridRow padded variant='<--1fr--><--1fr-->'>
            <UIGridRow padded={false} variant='<-------------1fr------------->'>
              <NineBlockControl />
              <ThreeBarControl />
            </UIGridRow>
            <FlexCol css={{ gap: 10 }}>
              <FlexDirectionToggle />
              <FlexContainerControls seeMoreVisible={true} />
              <FlexGapControl />
            </FlexCol>
          </UIGridRow>
          <UIGridRow padded={false} variant='<-------------1fr------------->'>
            <SpacedPackedControl />
          </UIGridRow>
        </FlexCol>,
      )}
    </div>
  )
})
