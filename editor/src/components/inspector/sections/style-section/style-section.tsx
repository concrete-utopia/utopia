import * as React from 'react'
import { betterReactMemo } from '../../../../uuiui-deps'
import { InspectorPartProps } from '../../inspector'
import { BackgroundSubsection } from './background-subsection/background-subsection'
import { BorderSubsection } from './border-subsection/border-subsection'
import { ContainerSubsection } from './containter-subsection/container-subsection'
import { ShadowSubsection } from './shadow-subsection/shadow-subsection'
import { TextShadowSubsection } from './text-subsection/text-shadow-subsection'
import { AutosizingTextSubsection } from './text-subsection/text-subsection'
import { TransformSubsection } from './transform-subsection/transform-subsection'

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
    <React.Fragment>
      <ContainerSubsection />
      <AutosizingTextSubsection />
      <TransformSubsection />
      <BackgroundSubsection />
      <BorderSubsection />
      <ShadowSubsection />
      <TextShadowSubsection />
    </React.Fragment>
  )
})
StyleSection.displayName = 'StyleSection'
