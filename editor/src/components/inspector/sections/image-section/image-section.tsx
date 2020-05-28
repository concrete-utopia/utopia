import * as React from 'react'
import { colorTheme, InspectorSectionHeader } from 'uuiui'
import { betterReactMemo } from 'uuiui-deps'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { emptySpecialSizeMeasurements } from '../../../../core/shared/element-template'
import * as PP from '../../../../core/shared/property-path'
import { filterScenes } from '../../../../core/shared/template-path'
import utils from '../../../../utils/utils'
import { NewInspectorContextMenuWrapper } from '../../../context-menu-wrapper'
import { useEditorState } from '../../../editor/store/store-hook'
import { StringControl } from '../../controls/string-control'
import { addOnUnsetValues } from '../../common/context-menu-items'
import { useInspectorMetadataInfo, useIsSubSectionVisible } from '../../common/property-path-hooks'
import { GridRow } from '../../widgets/grid-row'
import { PropertyLabel } from '../../widgets/property-label'
import { ImageDensityControl } from './image-density-control'

const imgSrcProp = [PP.create(['src'])]
const imgAltProp = [PP.create(['alt'])]

export const ImgSection = betterReactMemo('ImgSection', () => {
  const { dispatch, selectedViews, zerothElementInstanceMetadata } = useEditorState((store) => {
    const selectedPaths = filterScenes(store.editor.selectedViews)
    return {
      dispatch: store.dispatch,
      selectedViews: selectedPaths,
      zerothElementInstanceMetadata: MetadataUtils.getElementByInstancePathMaybe(
        store.editor.jsxMetadataKILLME,
        selectedPaths[0],
      ),
    }
  })
  const { naturalWidth, naturalHeight, clientWidth, clientHeight } =
    zerothElementInstanceMetadata?.specialSizeMeasurements ?? emptySpecialSizeMeasurements
  const {
    value: srcValue,
    controlStyles: srcControlStyles,
    controlStatus: srcControlStatus,
    onSubmitValue: srcOnSubmitValue,
    onUnsetValues: srcOnUnsetValues,
  } = useInspectorMetadataInfo('src')

  const {
    value: altValue,
    controlStyles: altControlStyles,
    controlStatus: altControlStatus,
    onSubmitValue: altOnSubmitValue,
    onUnsetValues: altOnUnsetValues,
  } = useInspectorMetadataInfo('alt')

  const isVisible = useIsSubSectionVisible('img')

  const srcContextMenuItems = utils.stripNulls([
    srcValue != null ? addOnUnsetValues(['srcValue'], srcOnUnsetValues) : null,
  ])
  const altContextMenuItems = utils.stripNulls([
    srcValue != null ? addOnUnsetValues(['altValue'], altOnUnsetValues) : null,
  ])

  let naturalDimensionsNode: React.ReactNode
  if (naturalWidth != null && naturalHeight != null && naturalWidth !== 0 && naturalHeight !== 0) {
    naturalDimensionsNode = (
      <span style={{ marginLeft: 4, color: colorTheme.tertiaryForeground.value }}>
        ({naturalWidth} × {naturalHeight})
      </span>
    )
  }

  if (!isVisible) {
    return null
  }

  return (
    <>
      <InspectorSectionHeader>Image {naturalDimensionsNode}</InspectorSectionHeader>
      <NewInspectorContextMenuWrapper
        id='image-section-src-context-menu'
        items={srcContextMenuItems}
        style={{ gridColumn: '1 / span 4' }}
        data={null}
      >
        <GridRow padded={true} type='<---1fr--->|------172px-------|'>
          <PropertyLabel target={imgSrcProp}>Source</PropertyLabel>
          <StringControl
            id='image-src'
            key='image-src'
            value={srcValue}
            onSubmitValue={srcOnSubmitValue}
            controlStyles={srcControlStyles}
            controlStatus={srcControlStatus}
          />
        </GridRow>
      </NewInspectorContextMenuWrapper>
      <NewInspectorContextMenuWrapper
        id='image-section-alt-context-menu'
        items={altContextMenuItems}
        style={{ gridColumn: '1 / span 4' }}
        data={null}
      >
        <GridRow padded={true} type='<---1fr--->|------172px-------|'>
          <PropertyLabel target={imgAltProp}>Alt Text</PropertyLabel>
          <StringControl
            id='image-alt'
            key='image-alt'
            value={altValue}
            onSubmitValue={altOnSubmitValue}
            controlStyles={altControlStyles}
            controlStatus={altControlStatus}
          />
        </GridRow>
      </NewInspectorContextMenuWrapper>
      <GridRow padded type='<---1fr--->|------172px-------|'>
        <PropertyLabel target={imgAltProp}>Density</PropertyLabel>
        <ImageDensityControl
          dispatch={dispatch}
          selectedViews={selectedViews}
          naturalWidth={naturalWidth}
          naturalHeight={naturalHeight}
          clientWidth={clientWidth}
          clientHeight={clientHeight}
        />
      </GridRow>
    </>
  )
})
ImgSection.displayName = 'ImageSection'
