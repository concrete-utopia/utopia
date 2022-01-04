import React from 'react'

import { UIGridRow } from '../../../widgets/ui-grid-row'
import { PositionControl, MarginControl, AlignSelfControl } from './flex-element-controls'
import { PropertyLabel } from '../../../widgets/property-label'
import { createLayoutPropertyPath } from '../../../../../core/layout/layout-helpers-new'
import {
  FunctionIcons,
  Icons,
  InspectorSubsectionHeader,
  SquareButton,
  useColorTheme,
} from '../../../../../uuiui'
import { ExpandableIndicator } from '../../../../navigator/navigator-item/expandable-indicator'
import {
  FlexBasisShorthandCSSNumberControl,
  FlexShorthandNumberControl,
  FlexStyleNumberControl,
  PinsLayoutNumberControl,
} from '../self-layout-subsection/gigantic-size-pins-subsection'
import { InlineLink, InlineToggleButton } from '../../../../../uuiui/inline-button'
import { when } from '../../../../../utils/react-conditionals'
import {
  InspectorCallbackContext,
  useInspectorLayoutInfo,
} from '../../../common/property-path-hooks'
import { isNotUnsetDefaultOrDetected } from '../../../common/control-status'
import { useEditorState } from '../../../../editor/store/store-hook'
import { PropertyPath } from '../../../../../core/shared/project-file-types'
import { usePropControlledStateV2 } from '../../../common/inspector-utils'

const marginProps = [
  createLayoutPropertyPath('marginLeft'),
  createLayoutPropertyPath('marginTop'),
  createLayoutPropertyPath('marginRight'),
  createLayoutPropertyPath('marginBottom'),
]

export const FlexElementSubsection = React.memo(() => {
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
      <AlignSelfControl variant='<---1fr--->|------172px-------|' />
    </>
  )
})

interface FlexElementSubsectionProps {
  parentFlexDirection: string | null
}

export const FlexElementSubsectionExperiment = React.memo((props: FlexElementSubsectionProps) => {
  return (
    <>
      <MainAxisControls {...props} />
      <CrossAxisControls {...props} />
    </>
  )
})

export function useInitialFixedSectionState(parentFlexDirection: string | null): boolean {
  const isRowLayouted = parentFlexDirection === 'row' || parentFlexDirection === 'row-reverse'

  const width = useInspectorLayoutInfo('Width')
  const minWidth = useInspectorLayoutInfo('minWidth')
  const maxWidth = useInspectorLayoutInfo('maxWidth')
  const height = useInspectorLayoutInfo('Height')
  const minHeight = useInspectorLayoutInfo('minHeight')
  const maxHeight = useInspectorLayoutInfo('maxHeight')

  return isRowLayouted
    ? isNotUnsetDefaultOrDetected(width.controlStatus) ||
        isNotUnsetDefaultOrDetected(minWidth.controlStatus) ||
        isNotUnsetDefaultOrDetected(maxWidth.controlStatus)
    : isNotUnsetDefaultOrDetected(height.controlStatus) ||
        isNotUnsetDefaultOrDetected(minHeight.controlStatus) ||
        isNotUnsetDefaultOrDetected(maxHeight.controlStatus)
}

export function useInitialAdvancedSectionState(): boolean {
  const alignSelf = useInspectorLayoutInfo('alignSelf')
  return isNotUnsetDefaultOrDetected(alignSelf.controlStatus)
}

export function useInitialSizeSectionState(): boolean {
  const flexBasis = useInspectorLayoutInfo('flexBasis')
  const flexGrow = useInspectorLayoutInfo('flexGrow')
  const flexShrink = useInspectorLayoutInfo('flexShrink')
  return (
    isNotUnsetDefaultOrDetected(flexBasis.controlStatus) ||
    isNotUnsetDefaultOrDetected(flexGrow.controlStatus) ||
    isNotUnsetDefaultOrDetected(flexShrink.controlStatus)
  )
}

const MainAxisControls = React.memo((props: FlexElementSubsectionProps) => {
  const initialIsFixedSectionVisible = useInitialFixedSectionState(props.parentFlexDirection)
  const initialIsAdvancedSectionVisible = useInitialAdvancedSectionState()

  const [fixedControlsOpen, setFixedControlsOpen] = usePropControlledStateV2(
    initialIsFixedSectionVisible,
  )
  const toggleFixedSection = React.useCallback(() => setFixedControlsOpen(!fixedControlsOpen), [
    fixedControlsOpen,
    setFixedControlsOpen,
  ])
  const [advancedControlsOpen, setAdvancedControlsOpen] = usePropControlledStateV2(
    initialIsAdvancedSectionVisible,
  )
  const toggleAdvancedSection = React.useCallback(
    () => setAdvancedControlsOpen(!advancedControlsOpen),
    [advancedControlsOpen, setAdvancedControlsOpen],
  )
  return (
    <>
      <InspectorSubsectionHeader style={{ display: 'flex', justifyContent: 'space-between' }}>
        <span>Main Axis</span>
        <div style={{ display: 'flex', justifyContent: 'flex-end', alignItems: 'center', gap: 4 }}>
          <InlineToggleButton
            toggleValue={fixedControlsOpen}
            onClick={toggleFixedSection}
            style={{
              fontSize: 10,
            }}
          >
            Fixed
          </InlineToggleButton>
          <InlineToggleButton
            toggleValue={advancedControlsOpen}
            onClick={toggleAdvancedSection}
            style={{
              fontSize: 10,
            }}
          >
            Advanced
          </InlineToggleButton>
          <SquareButton highlight>
            <Icons.Cross />
          </SquareButton>
        </div>
      </InspectorSubsectionHeader>
      <FlexGrowShrinkRow />
      <UIGridRow padded={true} variant='<-------------1fr------------->'>
        <FlexBasisShorthandCSSNumberControl label='B' />
      </UIGridRow>
      {when(fixedControlsOpen, <FixedSubsectionControls {...props} />)}
      {when(advancedControlsOpen, <AdvancedSubsectionControls {...props} />)}
    </>
  )
})

const mainAxisFixedProps = (parentFlexDirection: string | null): PropertyPath[] => {
  if (parentFlexDirection === 'row' || parentFlexDirection === 'row-reverse') {
    return [
      createLayoutPropertyPath('Width'),
      createLayoutPropertyPath('minWidth'),
      createLayoutPropertyPath('maxWidth'),
    ]
  } else {
    return [
      createLayoutPropertyPath('Height'),
      createLayoutPropertyPath('minHeight'),
      createLayoutPropertyPath('maxHeight'),
    ]
  }
}
const mainAxisAdvancedProps: PropertyPath[] = [createLayoutPropertyPath('alignSelf')]

const FixedSubsectionControls = React.memo((props: FlexElementSubsectionProps) => {
  const widthOrHeightControls =
    props.parentFlexDirection === 'row' || props.parentFlexDirection === 'row-reverse' ? (
      <FlexWidthControls />
    ) : (
      <FlexHeightControls />
    )

  const colorTheme = useColorTheme()
  const { onUnsetValue } = React.useContext(InspectorCallbackContext)
  const deleteFixedProps = React.useCallback(() => {
    onUnsetValue(mainAxisFixedProps(props.parentFlexDirection), false)
  }, [props.parentFlexDirection, onUnsetValue])

  return (
    <>
      <InspectorSubsectionHeader>
        <span style={{ color: colorTheme.primary.value, fontSize: 10, flexGrow: 1 }}>Fixed</span>
        <SquareButton highlight onClick={deleteFixedProps}>
          <FunctionIcons.Delete />
        </SquareButton>
      </InspectorSubsectionHeader>
      {widthOrHeightControls}
    </>
  )
})

const AdvancedSubsectionControls = React.memo((props: FlexElementSubsectionProps) => {
  const { onUnsetValue } = React.useContext(InspectorCallbackContext)
  const deleteAdvancedProps = React.useCallback(() => {
    onUnsetValue(mainAxisAdvancedProps, false)
  }, [onUnsetValue])
  const colorTheme = useColorTheme()
  return (
    <>
      <InspectorSubsectionHeader>
        <span style={{ color: colorTheme.primary.value, fontSize: 10, flexGrow: 1 }}>Advanced</span>
        <SquareButton highlight onClick={deleteAdvancedProps}>
          <FunctionIcons.Delete />
        </SquareButton>
      </InspectorSubsectionHeader>
      <AlignSelfControl variant='<--1fr--><--1fr-->' />
    </>
  )
})

export function useInitialCrossSectionState(parentFlexDirection: string | null): boolean {
  const isColumnLayouted =
    parentFlexDirection === 'column' || parentFlexDirection === 'column-reverse'

  const width = useInspectorLayoutInfo('Width')
  const minWidth = useInspectorLayoutInfo('minWidth')
  const maxWidth = useInspectorLayoutInfo('maxWidth')
  const height = useInspectorLayoutInfo('Height')
  const minHeight = useInspectorLayoutInfo('minHeight')
  const maxHeight = useInspectorLayoutInfo('maxHeight')

  return isColumnLayouted
    ? isNotUnsetDefaultOrDetected(width.controlStatus) ||
        isNotUnsetDefaultOrDetected(minWidth.controlStatus) ||
        isNotUnsetDefaultOrDetected(maxWidth.controlStatus)
    : isNotUnsetDefaultOrDetected(height.controlStatus) ||
        isNotUnsetDefaultOrDetected(minHeight.controlStatus) ||
        isNotUnsetDefaultOrDetected(maxHeight.controlStatus)
}

const CrossAxisControls = React.memo((props: FlexElementSubsectionProps) => {
  const isCrossAxisVisible = useInitialCrossSectionState(props.parentFlexDirection)

  const [crossAxisControlsOpen, setCrossAxisControlsOpen] = usePropControlledStateV2(
    isCrossAxisVisible,
  )
  const toggleSection = React.useCallback(() => setCrossAxisControlsOpen(!crossAxisControlsOpen), [
    crossAxisControlsOpen,
    setCrossAxisControlsOpen,
  ])
  const isColumnLayouted =
    props.parentFlexDirection === 'column' || props.parentFlexDirection === 'column-reverse'
  const widthOrHeightControls = isColumnLayouted ? <FlexWidthControls /> : <FlexHeightControls />
  return (
    <>
      <InspectorSubsectionHeader style={{ display: 'flex', justifyContent: 'space-between' }}>
        <div style={{ justifySelf: 'flex-start' }}>Cross Axis</div>
        <div style={{ display: 'flex', alignItems: 'center' }}>
          <InlineLink>+</InlineLink>
          <SquareButton highlight onClick={toggleSection}>
            <ExpandableIndicator
              testId='flex-element-subsection'
              visible
              collapsed={!crossAxisControlsOpen}
              selected={false}
            />
          </SquareButton>
        </div>
      </InspectorSubsectionHeader>
      {when(crossAxisControlsOpen, widthOrHeightControls)}
    </>
  )
})

const FlexWidthControls = React.memo(() => {
  return (
    <>
      <UIGridRow padded={true} variant='<--1fr--><--1fr-->'>
        <PinsLayoutNumberControl label='W' prop='Width' />
      </UIGridRow>
      <UIGridRow padded={true} variant='<--1fr--><--1fr-->'>
        <FlexStyleNumberControl label='min' styleProp='minWidth' />
        <FlexStyleNumberControl label='max' styleProp='maxWidth' />
      </UIGridRow>
    </>
  )
})
const FlexHeightControls = React.memo(() => {
  return (
    <>
      <UIGridRow padded={true} variant='<--1fr--><--1fr-->'>
        <PinsLayoutNumberControl label='H' prop='Height' />
      </UIGridRow>
      <UIGridRow padded={true} variant='<--1fr--><--1fr-->'>
        <FlexStyleNumberControl label='min' styleProp='minHeight' />
        <FlexStyleNumberControl label='max' styleProp='maxHeight' />
      </UIGridRow>
    </>
  )
})
const FlexGrowShrinkRow = React.memo(() => {
  return (
    <UIGridRow padded={true} variant='<--1fr--><--1fr-->'>
      <FlexShorthandNumberControl label='G' styleProp='flexGrow' />
      <FlexShorthandNumberControl label='S' styleProp='flexShrink' />
    </UIGridRow>
  )
})
