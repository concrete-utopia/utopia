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
  PinsLayoutNumberControl,
} from '../self-layout-subsection/gigantic-size-pins-subsection'
import { InlineLink } from '../../../../../uuiui/inline-button'
import { when } from '../../../../../utils/react-conditionals'
import {
  InspectorCallbackContext,
  useInspectorLayoutInfo,
} from '../../../common/property-path-hooks'
import { isNotUnsetDefaultOrDetected } from '../../../common/control-status'
import { usePropControlledStateV2 } from '../../../common/inspector-utils'
import { useEditorState } from '../../../../editor/store/store-hook'
import { PropertyPath } from '../../../../../core/shared/project-file-types'

const marginProps = [
  createLayoutPropertyPath('marginLeft'),
  createLayoutPropertyPath('marginTop'),
  createLayoutPropertyPath('marginRight'),
  createLayoutPropertyPath('marginBottom'),
]

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
      <AlignSelfControl variant='<---1fr--->|------172px-------|' />
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
    const isRowLayouted =
      props.parentFlexDirection === 'row' || props.parentFlexDirection === 'row-reverse'

    const width = useInspectorLayoutInfo('Width')
    const minWidth = useInspectorLayoutInfo('minWidth')
    const maxWidth = useInspectorLayoutInfo('maxWidth')
    const height = useInspectorLayoutInfo('Height')
    const minHeight = useInspectorLayoutInfo('minHeight')
    const maxHeight = useInspectorLayoutInfo('maxHeight')
    const alignSelf = useInspectorLayoutInfo('alignSelf')
    const flexBasis = useInspectorLayoutInfo('flexBasis')
    const flexGrow = useInspectorLayoutInfo('flexGrow')
    const flexShrink = useInspectorLayoutInfo('flexShrink')

    const isMainAxisVisibleForFlexDirection = isRowLayouted
      ? isNotUnsetDefaultOrDetected(width.controlStatus) ||
        isNotUnsetDefaultOrDetected(minWidth.controlStatus) ||
        isNotUnsetDefaultOrDetected(maxWidth.controlStatus)
      : isNotUnsetDefaultOrDetected(height.controlStatus) ||
        isNotUnsetDefaultOrDetected(minHeight.controlStatus) ||
        isNotUnsetDefaultOrDetected(maxHeight.controlStatus)

    const isMainAxisVisible =
      isMainAxisVisibleForFlexDirection ||
      isNotUnsetDefaultOrDetected(alignSelf.controlStatus) ||
      isNotUnsetDefaultOrDetected(flexBasis.controlStatus) ||
      isNotUnsetDefaultOrDetected(flexGrow.controlStatus) ||
      isNotUnsetDefaultOrDetected(flexShrink.controlStatus)

    const [mainAxisControlsOpen, setMainAxisControlsOpen] = usePropControlledStateV2(
      isMainAxisVisible,
    )
    const toggleSection = React.useCallback(() => setMainAxisControlsOpen(!mainAxisControlsOpen), [
      mainAxisControlsOpen,
      setMainAxisControlsOpen,
    ])
    return (
      <>
        <InspectorSubsectionHeader style={{ display: 'flex', justifyContent: 'space-between' }}>
          <span>Main Axis</span>
          <SquareButton highlight onClick={toggleSection}>
            <ExpandableIndicator
              testId='flex-element-subsection'
              visible
              collapsed={!mainAxisControlsOpen}
              selected={false}
            />
          </SquareButton>
        </InspectorSubsectionHeader>
        {when(mainAxisControlsOpen, <MainAxisControlsContent {...props} />)}
      </>
    )
  },
)

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

const MainAxisControlsContent = betterReactMemo(
  'MainAxisControlsContent',
  (props: FlexElementSubsectionProps) => {
    const widthOrHeightControls =
      props.parentFlexDirection === 'row' || props.parentFlexDirection === 'row-reverse' ? (
        <FlexWidthControls />
      ) : (
        <FlexHeightControls />
      )

    const { onUnsetValue } = React.useContext(InspectorCallbackContext)
    const deleteFixedProps = React.useCallback(() => {
      onUnsetValue(mainAxisFixedProps(props.parentFlexDirection), false)
    }, [props.parentFlexDirection, onUnsetValue])
    const deleteAdvancedProps = React.useCallback(() => {
      onUnsetValue(mainAxisAdvancedProps, false)
    }, [onUnsetValue])
    return (
      <>
        <UIGridRow padded={true} variant='<-------------1fr------------->'>
          <FlexBasisShorthandCSSNumberControl label='B' />
        </UIGridRow>
        <FlexGrowShrinkRow />
        <InspectorSubsectionHeader>
          <InlineLink>Fixed</InlineLink>
          <SquareButton highlight onClick={deleteFixedProps}>
            <FunctionIcons.Delete />
          </SquareButton>
        </InspectorSubsectionHeader>
        {widthOrHeightControls}
        <InspectorSubsectionHeader>
          <InlineLink>Advanced</InlineLink>
          <SquareButton highlight onClick={deleteAdvancedProps}>
            <FunctionIcons.Delete />
          </SquareButton>
        </InspectorSubsectionHeader>
        <AlignSelfControl variant='<--1fr--><--1fr-->' />
      </>
    )
  },
)

const CrossAxisControls = betterReactMemo(
  'CrossAxisControls',
  (props: FlexElementSubsectionProps) => {
    const isColumnLayouted =
      props.parentFlexDirection === 'column' || props.parentFlexDirection === 'column-reverse'

    const width = useInspectorLayoutInfo('Width')
    const minWidth = useInspectorLayoutInfo('minWidth')
    const maxWidth = useInspectorLayoutInfo('maxWidth')
    const height = useInspectorLayoutInfo('Height')
    const minHeight = useInspectorLayoutInfo('minHeight')
    const maxHeight = useInspectorLayoutInfo('maxHeight')

    const isCrossAxisVisible = isColumnLayouted
      ? isNotUnsetDefaultOrDetected(width.controlStatus) ||
        isNotUnsetDefaultOrDetected(minWidth.controlStatus) ||
        isNotUnsetDefaultOrDetected(maxWidth.controlStatus)
      : isNotUnsetDefaultOrDetected(height.controlStatus) ||
        isNotUnsetDefaultOrDetected(minHeight.controlStatus) ||
        isNotUnsetDefaultOrDetected(maxHeight.controlStatus)

    const [crossAxisControlsOpen, setCrossAxisControlsOpen] = usePropControlledStateV2(
      isCrossAxisVisible,
    )
    const toggleSection = React.useCallback(
      () => setCrossAxisControlsOpen(!crossAxisControlsOpen),
      [crossAxisControlsOpen, setCrossAxisControlsOpen],
    )
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
  },
)

const FlexWidthControls = betterReactMemo('FlexWidthControls', () => {
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
const FlexHeightControls = betterReactMemo('FlexWidthControls', () => {
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
const FlexGrowShrinkRow = betterReactMemo('FlexGrowShrinkRow', () => {
  return (
    <UIGridRow padded={true} variant='<--1fr--><--1fr-->'>
      <FlexShorthandNumberControl label='G' styleProp='flexGrow' />
      <FlexShorthandNumberControl label='S' styleProp='flexShrink' />
    </UIGridRow>
  )
})
