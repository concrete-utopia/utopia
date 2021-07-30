import * as React from 'react'
import { LayoutSystemSubsection } from './layout-system-subsection/layout-system-subsection'
import { SelfLayoutSubsection } from './self-layout-subsection/self-layout-subsection-old'
import { emptySpecialSizeMeasurements } from '../../../../core/shared/element-template'
import { betterReactMemo } from '../../../../uuiui-deps'
import { useEditorState } from '../../../editor/store/store-hook'
import { fastForEach } from '../../../../core/shared/utils'
import * as EP from '../../../../core/shared/element-path'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { SpecialSizeMeasurementsKeepDeepEquality } from '../../../editor/store/store-deep-equality-instances'
import { isFeatureEnabled } from '../../../../utils/feature-switches'
import { LayoutSubsection } from './self-layout-subsection/self-layout-subsection'
import { setInspectorLayoutSectionHovered } from '../../../editor/actions/action-creators'

interface LayoutSectionProps {
  hasNonDefaultPositionAttributes: boolean
  aspectRatioLocked: boolean
  toggleAspectRatioLock: () => void
}

export const LayoutSection = betterReactMemo('LayoutSection', (props: LayoutSectionProps) => {
  const specialSizeMeasurements = useEditorState(
    (state) => {
      let foundSpecialSizeMeasurements = emptySpecialSizeMeasurements
      fastForEach(state.editor.selectedViews, (path) => {
        // TODO multiselect
        const jsxMetadata = state.editor.jsxMetadata
        const elementMetadata = MetadataUtils.findElementByElementPath(jsxMetadata, path)
        if (elementMetadata != null) {
          foundSpecialSizeMeasurements = elementMetadata.specialSizeMeasurements
        }
      })
      return foundSpecialSizeMeasurements
    },
    'LayoutSection specialSizeMeasurements',
    (old, next) => SpecialSizeMeasurementsKeepDeepEquality()(old, next).areEqual,
  )

  const dispatch = useEditorState((store) => store.dispatch, 'LayoutSection dispatch')

  const selfLayoutSection = isFeatureEnabled('Layout Section Experimental') ? (
    <LayoutSubsection
      position={specialSizeMeasurements.position}
      parentLayoutSystem={specialSizeMeasurements.parentLayoutSystem}
      parentFlexDirection={specialSizeMeasurements.parentFlexDirection}
      aspectRatioLocked={props.aspectRatioLocked}
      toggleAspectRatioLock={props.toggleAspectRatioLock}
    />
  ) : (
    <SelfLayoutSubsection
      position={specialSizeMeasurements.position}
      isChildOfFlexComponent={specialSizeMeasurements.parentLayoutSystem === 'flex'}
      parentFlexDirection={specialSizeMeasurements.parentFlexDirection}
      aspectRatioLocked={props.aspectRatioLocked}
      toggleAspectRatioLock={props.toggleAspectRatioLock}
    />
  )

  return (
    <div
      onMouseOver={() => {
        dispatch([setInspectorLayoutSectionHovered(true)], 'everyone')
      }}
      onMouseOut={() => {
        dispatch([setInspectorLayoutSectionHovered(false)], 'everyone')
      }}
    >
      {selfLayoutSection}
      <LayoutSystemSubsection specialSizeMeasurements={specialSizeMeasurements} />
    </div>
  )
})
