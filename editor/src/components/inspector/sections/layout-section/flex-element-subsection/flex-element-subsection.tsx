import * as React from 'react'

import { UIGridRow } from '../../../widgets/ui-grid-row'
import { PositionControl, MarginControl, AlignSelfControl } from './flex-element-controls'
import { PropertyLabel } from '../../../widgets/property-label'
import { createLayoutPropertyPath } from '../../../../../core/layout/layout-helpers-new'
import { betterReactMemo } from '../../../../../uuiui-deps'
import { FunctionIcons, InspectorSubsectionHeader, SquareButton } from '../../../../../uuiui'
import { ExpandableIndicator } from '../../../../navigator/navigator-item/expandable-indicator'
import {
  FlexBasisShorthandCSSNumberControl,
  FlexShorthandNumberControl,
  FlexStyleNumberControl,
} from '../self-layout-subsection/gigantic-size-pins-subsection'
import { InlineLink } from '../../../../../uuiui/inline-button'

const marginProps = [
  createLayoutPropertyPath('marginLeft'),
  createLayoutPropertyPath('marginTop'),
  createLayoutPropertyPath('marginRight'),
  createLayoutPropertyPath('marginBottom'),
]

const alignSelfProp = [createLayoutPropertyPath('alignSelf')]

export const FlexElementSubsection = betterReactMemo('FlexElementSubsection', () => {
  return (
    <>
      <UIGridRow tall padded={true} variant='<---1fr--->|------172px-------|'>
        <PropertyLabel
          target={marginProps}
          style={{ paddingBottom: 12 }}
          propNamesToUnset={['all margins']}
        >
          Margin
        </PropertyLabel>
        <MarginControl />
      </UIGridRow>
      <UIGridRow padded={true} variant='<---1fr--->|------172px-------|'>
        <PropertyLabel target={alignSelfProp}>Align Self</PropertyLabel>
        <AlignSelfControl />
      </UIGridRow>
    </>
  )
})

interface FlexElementSubsectionProps {
  parentFlexDirection: string | null
}

export const FlexElementSubsectionExperiment = betterReactMemo(
  'FlexElementSubsectionExperiment',
  (props: FlexElementSubsectionProps) => {
    return (
      <>
        <MainAxisControls {...props} />
        <CrossAxisControls {...props} />
      </>
    )
  },
)

const MainAxisControls = betterReactMemo(
  'MainAxisControls',
  (props: FlexElementSubsectionProps) => {
    const widthOrHeightControls =
      props.parentFlexDirection === 'row' || props.parentFlexDirection === 'row-reverse' ? (
        <FlexWidthControls />
      ) : (
        <FlexHeightControls />
      )
    return (
      <>
        <InspectorSubsectionHeader style={{ display: 'flex', justifyContent: 'space-between' }}>
          <span>Main Axis</span>{' '}
          <SquareButton highlight>
            <ExpandableIndicator
              testId='flex-element-subsection'
              visible
              collapsed={false}
              selected={false}
            />
          </SquareButton>
        </InspectorSubsectionHeader>
        <UIGridRow padded={true} variant='<-------------1fr------------->'>
          <FlexBasisShorthandCSSNumberControl label='B' />
        </UIGridRow>
        <FlexGrowShrinkRow />
        <InspectorSubsectionHeader>
          <InlineLink>Fixed</InlineLink>
          <SquareButton highlight>
            <FunctionIcons.Delete />
          </SquareButton>
        </InspectorSubsectionHeader>
        {widthOrHeightControls}
        <InspectorSubsectionHeader>
          <InlineLink>Advanced</InlineLink>
          <SquareButton highlight>
            <FunctionIcons.Delete />
          </SquareButton>
        </InspectorSubsectionHeader>
        <UIGridRow padded={true} variant='<--1fr--><--1fr-->'>
          <PropertyLabel target={alignSelfProp}>Align Self</PropertyLabel>
          <AlignSelfControl />
        </UIGridRow>
      </>
    )
  },
)

const CrossAxisControls = betterReactMemo(
  'CrossAxisControls',
  (props: FlexElementSubsectionProps) => {
    const widthOrHeightControls =
      props.parentFlexDirection === 'column' || props.parentFlexDirection === 'column-reverse' ? (
        <FlexHeightControls />
      ) : (
        <FlexWidthControls />
      )
    return (
      <>
        <InspectorSubsectionHeader style={{ display: 'flex', justifyContent: 'space-between' }}>
          <div style={{ justifySelf: 'flex-start' }}>Cross Axis</div>
          <div style={{ display: 'flex', alignItems: 'center' }}>
            <InlineLink>+</InlineLink>
            <SquareButton highlight>
              <ExpandableIndicator
                testId='flex-element-subsection'
                visible
                collapsed={false}
                selected={false}
              />
            </SquareButton>
          </div>
        </InspectorSubsectionHeader>
        {widthOrHeightControls}
      </>
    )
  },
)

const FlexWidthControls = betterReactMemo('FlexWidthControls', () => {
  return (
    <>
      <UIGridRow padded={true} variant='<--1fr--><--1fr-->'>
        <FlexBasisShorthandCSSNumberControl label='W' />
      </UIGridRow>
      <UIGridRow padded={true} variant='<--1fr--><--1fr-->'>
        <FlexStyleNumberControl label='min' styleProp='minWidth' />
        <FlexStyleNumberControl label='max' styleProp='maxWidth' />
      </UIGridRow>
    </>
  )
})
const FlexHeightControls = betterReactMemo('FlexWidthControls', () => {
  return (
    <>
      <UIGridRow padded={true} variant='<--1fr--><--1fr-->'>
        <FlexBasisShorthandCSSNumberControl label='H' />
      </UIGridRow>
      <UIGridRow padded={true} variant='<--1fr--><--1fr-->'>
        <FlexStyleNumberControl label='min' styleProp='minHeight' />
        <FlexStyleNumberControl label='max' styleProp='maxHeight' />
      </UIGridRow>
    </>
  )
})
const FlexGrowShrinkRow = betterReactMemo('FlexGrowShrinkRow', () => {
  return (
    <UIGridRow padded={true} variant='<--1fr--><--1fr-->'>
      <FlexShorthandNumberControl label='G' styleProp='flexGrow' />
      <FlexShorthandNumberControl label='S' styleProp='flexShrink' />
    </UIGridRow>
  )
})
