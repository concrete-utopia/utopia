import * as React from 'react'
import { InspectorSectionHeader } from 'uuiui'
import { InspectorPartProps } from '../../inspector-core'
import { PropertyRow } from '../../widgets/property-row'
import { BackgroundSubsection } from './background-subsection/background-subsection'
import { ShadowSubsection } from './shadow-subsection/shadow-subsection'
import { BorderSubsection } from './border-subsection/border-subsection'
import { AutosizingTextSubsection } from './text-subsection/text-subsection'
import { TextShadowSubsection } from './text-subsection/text-shadow-subsection'
import { TransformSubsection } from './transform-subsection/transform-subsection'
import { betterReactMemo } from 'uuiui-deps'
import { ContainerSubsection } from './containter-subsection/container-subsection'

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

export const StyleSection = betterReactMemo('StyleSection', () => {
  return (
    <>
      <ContainerSubsection />
      <AutosizingTextSubsection />
      <TransformSubsection />
      <BackgroundSubsection />
      <BorderSubsection />
      <ShadowSubsection />
      <TextShadowSubsection />
    </>
  )
})
StyleSection.displayName = 'StyleSection'
