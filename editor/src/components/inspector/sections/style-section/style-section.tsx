import React from 'react'
import type { InspectorPartProps } from '../../inspector'
import { BackgroundSubsection } from './background-subsection/background-subsection'
import { BorderSubsection } from './border-subsection/border-subsection'
import { ContainerSubsection } from './container-subsection/container-subsection'
import { ShadowSubsection } from './shadow-subsection/shadow-subsection'
import { TextShadowSubsection } from './text-subsection/text-shadow-subsection'
import { TextSubsection } from './text-subsection/text-subsection'
import { TransformSubsection } from './transform-subsection/transform-subsection'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { getInspectorPreferencesForTargets } from '../../../../core/property-controls/property-controls-utils'
import { when } from '../../../../utils/react-conditionals'

export enum StyleSubsection {
  Layer = 'Layer',
  Opacity = 'Opacity',
  Text = 'Text',
  Image = 'Image',
  Background = 'Background',
  Border = 'Border',
  Shadow = 'Shadow',
  Filters = 'Filters',
  TextShadow = 'Text Shadow',
}

export interface StyleSectionProps extends InspectorPartProps<React.CSSProperties> {}

export const StyleSection = React.memo(() => {
  const inspectorPreferences = useEditorState(
    Substores.propertyControlsInfo,
    (store) =>
      getInspectorPreferencesForTargets(
        store.editor.selectedViews,
        store.editor.propertyControlsInfo,
        store.editor.projectContents,
      ),
    'StyleSection inspectorPreferences',
  )

  const shouldShowContainerSection = inspectorPreferences.includes('layout')
  const shouldTextContainerSection = inspectorPreferences.includes('typography')
  const shouldShowVisualSections = inspectorPreferences.includes('visual')

  return (
    <React.Fragment>
      {when(shouldShowContainerSection, <ContainerSubsection />)}
      {when(shouldTextContainerSection, <TextSubsection />)}
      {when(
        shouldShowVisualSections,
        <>
          <TransformSubsection />
          <BackgroundSubsection />
          <BorderSubsection />
          <ShadowSubsection />
          <TextShadowSubsection />
        </>,
      )}
    </React.Fragment>
  )
})
StyleSection.displayName = 'StyleSection'
