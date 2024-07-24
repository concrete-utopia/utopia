import React from 'react'
import { createSelector } from 'reselect'
import { when } from '../../utils/react-conditionals'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { AddRemoveLayoutSystemControl } from './add-remove-layout-system-control'
import { FlexDirectionToggle } from './flex-direction-control'
import { selectedViewsSelector, metadataSelector } from './inpector-selectors'
import { NineBlockControl } from './nine-block-controls'
import { UIGridRow } from './widgets/ui-grid-row'
import { LayoutSystemControl } from '../../components/inspector/sections/layout-section/layout-system-subsection/layout-system-controls'
import { SpacedPackedControl } from './spaced-packed-control'
import { ThreeBarControl } from './three-bar-control'
import { FlexGapControl } from './sections/layout-section/flex-container-subsection/flex-container-controls'
import { FlexContainerControls } from './sections/layout-section/flex-container-subsection/flex-container-subsection'
import { FlexCol } from 'utopia-api'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { strictEvery } from '../../core/shared/array-utils'
import { useDispatch } from '../editor/store/dispatch-context'
import {
  addFlexLayoutStrategies,
  addGridLayoutStrategies,
} from './inspector-strategies/inspector-strategies'
import { executeFirstApplicableStrategy } from './inspector-strategies/inspector-strategy'
import type { DetectedLayoutSystem } from 'utopia-shared/src/types'
import { assertNever } from '../../core/shared/utils'
import { Subdued } from '../../uuiui'

export const layoutSystemSelector = createSelector(
  metadataSelector,
  selectedViewsSelector,
  (metadata, selectedViews) => {
    const detectedLayoutSystems = selectedViews.map(
      (path) =>
        MetadataUtils.findElementByElementPath(metadata, path)?.specialSizeMeasurements
          .layoutSystemForChildren ?? null,
    )

    const allLayoutSystemsTheSame = strictEvery(
      detectedLayoutSystems,
      (e) => e === detectedLayoutSystems[0],
    )

    if (allLayoutSystemsTheSame) {
      return detectedLayoutSystems[0]
    }

    return null
  },
)

export const FlexSection = React.memo(() => {
  const layoutSystem = useEditorState(
    Substores.metadata,
    layoutSystemSelector,
    'FlexSection areAllElementsInFlexLayout',
  )

  const dispatch = useDispatch()
  const elementMetadataRef = useRefEditorState(metadataSelector)
  const selectedViewsRef = useRefEditorState(selectedViewsSelector)
  const elementPathTreeRef = useRefEditorState((store) => store.editor.elementPathTree)
  const allElementPropsRef = useRefEditorState((store) => store.editor.allElementProps)

  const addFlexLayoutSystem = React.useCallback(
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

  const addGridLayoutSystem = React.useCallback(
    () =>
      executeFirstApplicableStrategy(
        dispatch,
        addGridLayoutStrategies(
          elementMetadataRef.current,
          selectedViewsRef.current,
          elementPathTreeRef.current,
          allElementPropsRef.current,
        ),
      ),
    [allElementPropsRef, dispatch, elementMetadataRef, elementPathTreeRef, selectedViewsRef],
  )

  const onLayoutSystemChange = React.useCallback(
    (value: DetectedLayoutSystem) => {
      switch (value) {
        case 'flex':
          return addFlexLayoutSystem()
        case 'grid':
          return addGridLayoutSystem()
        case 'flow':
        case 'none':
          return
        default:
          assertNever(value)
      }
    },
    [addFlexLayoutSystem, addGridLayoutSystem],
  )

  return (
    <div>
      <AddRemoveLayoutSystemControl />
      <FlexCol css={{ gap: 10 }}>
        {when(
          layoutSystem === 'grid' || layoutSystem === 'flex',
          <UIGridRow padded={true} variant='<-------------1fr------------->'>
            <LayoutSystemControl
              layoutSystem={layoutSystem}
              providesCoordinateSystemForChildren={false}
              onChange={onLayoutSystemChange}
            />
          </UIGridRow>,
        )}
        {when(
          layoutSystem === 'grid',
          <UIGridRow padded tall={false} variant={'<-------------1fr------------->'}>
            <div>
              <Subdued>Grid inspector coming soon...</Subdued>
            </div>
          </UIGridRow>,
        )}
        {when(
          layoutSystem === 'flex',
          <>
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
          </>,
        )}
      </FlexCol>
      ,
    </div>
  )
})
