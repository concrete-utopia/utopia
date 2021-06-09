import * as React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { emptySpecialSizeMeasurements } from '../../../../core/shared/element-template'
import * as PP from '../../../../core/shared/property-path'
import utils from '../../../../utils/utils'
import { InspectorContextMenuWrapper } from '../../../context-menu-wrapper'
import { useEditorState } from '../../../editor/store/store-hook'
import { StringControl } from '../../controls/string-control'
import { addOnUnsetValues } from '../../common/context-menu-items'
import {
  useInspectorElementInfo,
  useIsSubSectionVisible,
  useSelectedViews,
} from '../../common/property-path-hooks'
import { UIGridRow } from '../../widgets/ui-grid-row'
import { PropertyLabel } from '../../widgets/property-label'
import { ImageDensityControl } from './image-density-control'
import { colorTheme, InspectorSectionHeader } from '../../../../uuiui'
import { betterReactMemo } from '../../../../uuiui-deps'

const imgSrcProp = [PP.create(['src'])]
const imgAltProp = [PP.create(['alt'])]

export const ImgSection = betterReactMemo('ImgSection', () => {
  const selectedViews = useSelectedViews()

  const { dispatch, zerothElementInstanceMetadata } = useEditorState((store) => {
    return {
      dispatch: store.dispatch,
      zerothElementInstanceMetadata: MetadataUtils.findElementByElementPath(
        store.editor.jsxMetadata,
        selectedViews[0],
      ),
    }
  }, 'ImgSection')
  const { naturalWidth, naturalHeight, clientWidth, clientHeight } =
    zerothElementInstanceMetadata?.specialSizeMeasurements ?? emptySpecialSizeMeasurements
  const {
    value: srcValue,
    controlStyles: srcControlStyles,
    controlStatus: srcControlStatus,
    onSubmitValue: srcOnSubmitValue,
    onUnsetValues: srcOnUnsetValues,
  } = useInspectorElementInfo('src')

  const {
    value: altValue,
    controlStyles: altControlStyles,
    controlStatus: altControlStatus,
    onSubmitValue: altOnSubmitValue,
    onUnsetValues: altOnUnsetValues,
  } = useInspectorElementInfo('alt')

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
      <span style={{ marginLeft: 4, color: colorTheme.subduedForeground.value }}>
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
      <InspectorContextMenuWrapper
        id='image-section-src-context-menu'
        items={srcContextMenuItems}
        style={{ gridColumn: '1 / span 4' }}
        data={null}
      >
        <UIGridRow padded={true} variant='<---1fr--->|------172px-------|'>
          <PropertyLabel target={imgSrcProp}>Source</PropertyLabel>
          <StringControl
            id='image-src'
            key='image-src'
            testId='image-src'
            value={srcValue}
            onSubmitValue={srcOnSubmitValue}
            controlStyles={srcControlStyles}
            controlStatus={srcControlStatus}
          />
        </UIGridRow>
      </InspectorContextMenuWrapper>
      <InspectorContextMenuWrapper
        id='image-section-alt-context-menu'
        items={altContextMenuItems}
        style={{ gridColumn: '1 / span 4' }}
        data={null}
      >
        <UIGridRow padded={true} variant='<---1fr--->|------172px-------|'>
          <PropertyLabel target={imgAltProp}>Alt Text</PropertyLabel>
          <StringControl
            id='image-alt'
            key='image-alt'
            testId='image-alt'
            value={altValue}
            onSubmitValue={altOnSubmitValue}
            controlStyles={altControlStyles}
            controlStatus={altControlStatus}
          />
        </UIGridRow>
      </InspectorContextMenuWrapper>
      <UIGridRow padded variant='<---1fr--->|------172px-------|'>
        <PropertyLabel target={imgAltProp}>Density</PropertyLabel>
        <ImageDensityControl
          dispatch={dispatch}
          selectedViews={selectedViews}
          naturalWidth={naturalWidth}
          naturalHeight={naturalHeight}
          clientWidth={clientWidth}
          clientHeight={clientHeight}
        />
      </UIGridRow>
    </>
  )
})
ImgSection.displayName = 'ImageSection'
