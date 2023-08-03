import React from 'react'
import type {
  LayoutFlexElementNumericProp,
  LayoutPinnedProp,
} from '../../../../../core/layout/layout-helpers-new'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { unsetPropertyMenuItem } from '../../../common/context-menu-items'
import type { CSSNumber, UnknownOrEmptyInput } from '../../../common/css-utils'
import { cssNumber, isCSSNumber, isFixedSize } from '../../../common/css-utils'
import type { FramePinsInfo } from '../../../common/layout-property-path-hooks'
import { usePinToggling } from '../../../common/layout-property-path-hooks'
import {
  InspectorPropsContext,
  stylePropPathMappingFn,
  useGetLayoutControlStatus,
  useInspectorLayoutInfo,
} from '../../../common/property-path-hooks'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import { PinControl, PinHeightControl, PinWidthControl } from '../../../controls/pin-control'
import { PropertyLabel } from '../../../widgets/property-label'
import type { SelfLayoutTab } from './self-layout-subsection'
import {
  useWrappedEmptyOrUnknownOnSubmitValue,
  NumberInput,
  SquareButton,
  Icons,
  FlexColumn,
  SimpleNumberInput,
  useColorTheme,
} from '../../../../../uuiui'
import { useInspectorInfoLonghandShorthand } from '../../../common/longhand-shorthand-hooks'
import { isNotUnsetOrDefault } from '../../../common/control-status'
import { usePropControlledStateV2 } from '../../../common/inspector-utils'
import { useContextSelector } from 'use-context-selector'
import { isFeatureEnabled } from '../../../../../utils/feature-switches'
import { unless } from '../../../../../utils/react-conditionals'
import type { OnSubmitValueOrUnknownOrEmpty } from '../../../controls/control'

interface PinsLayoutNumberControlProps {
  label: string
  prop: LayoutPinnedProp
  fixedSizeHandler: ((value: CSSNumber, transient: boolean) => void) | null
}

export const pinLabels: { [key in LayoutPinnedProp]: string } = {
  left: 'L',
  top: 'T',
  right: 'R',
  bottom: 'B',
  width: 'W',
  height: 'H',
}

export const PinsLayoutNumberControl = React.memo((props: PinsLayoutNumberControlProps) => {
  const pointInfo = useInspectorLayoutInfo(props.prop)

  const defaultOnSubmit = useWrappedEmptyOrUnknownOnSubmitValue(
    pointInfo.onSubmitValue,
    pointInfo.onUnsetValues,
  )
  const defaultOnTransientSubmit = useWrappedEmptyOrUnknownOnSubmitValue(
    pointInfo.onTransientSubmitValue,
    pointInfo.onUnsetValues,
  )

  const onSubmit: OnSubmitValueOrUnknownOrEmpty<CSSNumber | undefined> = React.useCallback(
    (value: UnknownOrEmptyInput<CSSNumber | undefined>) => {
      if (props.fixedSizeHandler != null && isCSSNumber(value) && isFixedSize(value)) {
        // If a fixed size handler is present, use that instead of the regular callback.
        props.fixedSizeHandler(value, false)
      } else {
        // Otherwise default to this.
        defaultOnSubmit(value, false)
      }
    },
    [defaultOnSubmit, props],
  )
  const onTransientSubmit: OnSubmitValueOrUnknownOrEmpty<CSSNumber | undefined> = React.useCallback(
    (value: UnknownOrEmptyInput<CSSNumber | undefined>) => {
      if (props.fixedSizeHandler != null && isCSSNumber(value) && isFixedSize(value)) {
        // If a fixed size handler is present, use that instead of the regular callback.
        props.fixedSizeHandler(value, false)
      } else {
        // Otherwise default to this.
        defaultOnTransientSubmit(value, false)
      }
    },
    [defaultOnTransientSubmit, props],
  )

  return (
    <InspectorContextMenuWrapper
      id={`position-${props.prop}-context-menu`}
      items={[unsetPropertyMenuItem(props.prop, pointInfo.onUnsetValues)]}
      data={{}}
    >
      <NumberInput
        value={pointInfo.value}
        id={`position-${props.prop}-number-input`}
        testId={`position-${props.prop}-number-input`}
        labelInner={props.label}
        onSubmitValue={onSubmit}
        onTransientSubmitValue={onTransientSubmit}
        controlStatus={pointInfo.controlStatus}
        numberType={'LengthPercent'}
        defaultUnitToHide={'px'}
      />
    </InspectorContextMenuWrapper>
  )
})

export type StyleLayoutNumberProp =
  | 'paddingTop'
  | 'paddingRight'
  | 'paddingBottom'
  | 'paddingLeft'
  | 'marginTop'
  | 'marginRight'
  | 'marginBottom'
  | 'marginLeft'
  | 'left'
  | 'top'
  | 'right'
  | 'bottom'
  | 'minWidth'
  | 'maxWidth'
  | 'minHeight'
  | 'maxHeight'
  | 'flexGrow'
  | 'flexShrink'

interface FlexStyleNumberControlProps {
  label: string
  styleProp: StyleLayoutNumberProp
}

export const FlexStyleNumberControl = React.memo((props: FlexStyleNumberControlProps) => {
  const layoutPropInfo = useInspectorLayoutInfo(props.styleProp)
  const value =
    isCSSNumber(layoutPropInfo.value) || layoutPropInfo.value == null
      ? layoutPropInfo.value
      : undefined

  const wrappedOnSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    layoutPropInfo.onSubmitValue,
    layoutPropInfo.onUnsetValues,
  )
  const wrappedOnTransientSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    layoutPropInfo.onTransientSubmitValue,
    layoutPropInfo.onUnsetValues,
  )
  return (
    <InspectorContextMenuWrapper
      id={`position-${props.styleProp}-context-menu`}
      items={[unsetPropertyMenuItem(props.styleProp, layoutPropInfo.onUnsetValues)]}
      data={{}}
    >
      <NumberInput
        id={`position-${props.styleProp}-number-input`}
        testId={`position-${props.styleProp}-number-input`}
        value={value}
        onSubmitValue={wrappedOnSubmitValue}
        onTransientSubmitValue={wrappedOnTransientSubmitValue}
        controlStatus={layoutPropInfo.controlStatus}
        numberType={'UnitlessPercent'}
        labelInner={props.label}
        defaultUnitToHide={'px'}
      />
    </InspectorContextMenuWrapper>
  )
})

interface FlexShorthandNumberControlProps {
  label: string
  styleProp: 'flexGrow' | 'flexShrink'
}

export const FlexShorthandNumberControl = React.memo((props: FlexShorthandNumberControlProps) => {
  const propertyTarget = useContextSelector(InspectorPropsContext, (contextData) => {
    return contextData.targetPath
  })
  const layoutPropInfo = useInspectorInfoLonghandShorthand(
    ['flexGrow', 'flexShrink', 'flexBasis'],
    'flex',
    stylePropPathMappingFn,
  )[props.styleProp]
  const value = typeof layoutPropInfo.value === 'number' ? layoutPropInfo.value : undefined

  const wrappedOnSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    layoutPropInfo.onSubmitValue,
    layoutPropInfo.onUnsetValues,
  )
  const wrappedOnTransientSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    layoutPropInfo.onTransientSubmitValue,
    layoutPropInfo.onUnsetValues,
  )
  return (
    <InspectorContextMenuWrapper
      id={`position-${props.styleProp}-context-menu`}
      items={[unsetPropertyMenuItem(props.styleProp, layoutPropInfo.onUnsetValues)]}
      data={{}}
    >
      <SimpleNumberInput
        id={`position-${props.styleProp}-number-input`}
        testId={`position-${props.styleProp}-number-input`}
        value={value}
        onForcedSubmitValue={wrappedOnSubmitValue}
        onSubmitValue={wrappedOnSubmitValue}
        onTransientSubmitValue={wrappedOnTransientSubmitValue}
        controlStatus={layoutPropInfo.controlStatus}
        labelInner={props.label}
        defaultUnitToHide={'px'}
      />
    </InspectorContextMenuWrapper>
  )
})

interface FlexShorthandCSSNumberControlProps {
  label: string
}
export const FlexBasisShorthandCSSNumberControl = React.memo(
  (props: FlexShorthandCSSNumberControlProps) => {
    const propertyTarget = useContextSelector(InspectorPropsContext, (contextData) => {
      return contextData.targetPath
    })
    const layoutPropInfo = useInspectorInfoLonghandShorthand(
      ['flexGrow', 'flexShrink', 'flexBasis'],
      'flex',
      stylePropPathMappingFn,
    ).flexBasis
    const value = React.useMemo(() => {
      return typeof layoutPropInfo.value === 'number'
        ? cssNumber(layoutPropInfo.value)
        : layoutPropInfo.value
    }, [layoutPropInfo.value])
    const wrappedOnSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
      layoutPropInfo.onSubmitValue,
      layoutPropInfo.onUnsetValues,
    )
    const wrappedOnTransientSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
      layoutPropInfo.onTransientSubmitValue,
      layoutPropInfo.onUnsetValues,
    )
    return (
      <InspectorContextMenuWrapper
        id={`position-flexBasis-context-menu`}
        items={[unsetPropertyMenuItem('flexBasis', layoutPropInfo.onUnsetValues)]}
        data={{}}
      >
        <NumberInput
          id={`position-flexBasis-number-input`}
          testId={`position-flexBasis-number-input`}
          value={value}
          onSubmitValue={wrappedOnSubmitValue}
          onTransientSubmitValue={wrappedOnTransientSubmitValue}
          controlStatus={layoutPropInfo.controlStatus}
          numberType={'UnitlessPercent'}
          labelInner={props.label}
          defaultUnitToHide={'px'}
        />
      </InspectorContextMenuWrapper>
    )
  },
)

interface FlexLayoutNumberControlProps {
  label: string
  layoutProp: LayoutFlexElementNumericProp
}

export const FlexLayoutNumberControl = React.memo((props: FlexLayoutNumberControlProps) => {
  const layoutPropInfo = useInspectorLayoutInfo(props.layoutProp)

  const wrappedOnSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    layoutPropInfo.onSubmitValue,
    layoutPropInfo.onUnsetValues,
  )
  const wrappedOnTransientSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    layoutPropInfo.onTransientSubmitValue,
    layoutPropInfo.onUnsetValues,
  )

  return (
    <InspectorContextMenuWrapper
      id={`position-${props.layoutProp}-context-menu`}
      items={[unsetPropertyMenuItem(props.layoutProp, layoutPropInfo.onUnsetValues)]}
      data={{}}
    >
      <NumberInput
        id={`position-${props.layoutProp}-number-input`}
        testId={`position-${props.layoutProp}-number-input`}
        value={layoutPropInfo.value}
        onSubmitValue={wrappedOnSubmitValue}
        onTransientSubmitValue={wrappedOnTransientSubmitValue}
        controlStatus={layoutPropInfo.controlStatus}
        numberType={'LengthPercent'}
        labelInner={props.label}
        defaultUnitToHide={'px'}
      />
    </InspectorContextMenuWrapper>
  )
})

interface PinControlsProps {
  resetPins: () => void
  framePins: FramePinsInfo
  togglePin: (prop: LayoutPinnedProp) => void
}

const PinControls = React.memo((props: PinControlsProps) => {
  const resetPinsItem = {
    name: 'Reset positioning',
    enabled: true,
    action: props.resetPins,
  }

  return (
    <InspectorContextMenuWrapper id='pins-context-menu' items={[resetPinsItem]} data={null}>
      <PinControl
        framePoints={props.framePins}
        controlStatus='simple'
        handlePinMouseDown={props.togglePin}
        style={{ paddingTop: 7 }}
      />
    </InspectorContextMenuWrapper>
  )
})

function pinsLayoutNumberControl(prop: LayoutPinnedProp) {
  return <PinsLayoutNumberControl label={pinLabels[prop]} prop={prop} fixedSizeHandler={null} />
}

function flexLayoutNumberControl(label: string, layoutProp: LayoutFlexElementNumericProp) {
  return <FlexLayoutNumberControl label={label} layoutProp={layoutProp} />
}

function flexStyleNumberControl(label: string, styleProp: StyleLayoutNumberProp) {
  return <FlexStyleNumberControl label={label} styleProp={styleProp} />
}

const spacingButton = <SquareButton />

export const AspectRatioLockButtonTestId = 'AspectRatioLockButton'

interface WidthHeightRowProps {
  togglePin: (prop: LayoutPinnedProp) => void
  framePins: FramePinsInfo
}

const WidthHeightRow = React.memo((props: WidthHeightRowProps) => {
  const { togglePin, framePins } = props

  const toggleWidth = React.useCallback(() => {
    togglePin('width')
  }, [togglePin])

  const toggleHeight = React.useCallback(() => {
    togglePin('height')
  }, [togglePin])

  return (
    <UIGridRow padded={true} variant='<---1fr--->|------172px-------|'>
      <div
        id='width-height-row-toggles'
        style={{
          display: 'flex',
          flexDirection: 'row',
          justifyContent: 'space-between',
        }}
      >
        <PinWidthControl framePins={framePins} toggleWidth={toggleWidth} controlStatus='simple' />
        <PinHeightControl
          framePins={framePins}
          toggleHeight={toggleHeight}
          controlStatus='simple'
        />
      </div>
    </UIGridRow>
  )
})

const MinimumsRow = React.memo(() => {
  const minimumsProps = useContextSelector(InspectorPropsContext, (contextData) => {
    return [
      stylePropPathMappingFn('minWidth', contextData.targetPath),
      stylePropPathMappingFn('minHeight', contextData.targetPath),
    ]
  })

  return (
    <UIGridRow padded={true} variant='<---1fr--->|------172px-------|'>
      <PropertyLabel target={minimumsProps}>Minimum</PropertyLabel>
      <UIGridRow padded={false} variant='|--67px--||16px||--67px--||16px|'>
        {flexStyleNumberControl('W', 'minWidth')}
        {spacingButton}
        {flexStyleNumberControl('H', 'minHeight')}
        {spacingButton}
      </UIGridRow>
    </UIGridRow>
  )
})

const MaximumsRow = React.memo(() => {
  const maximumsProps = useContextSelector(InspectorPropsContext, (contextData) => {
    return [
      stylePropPathMappingFn('maxWidth', contextData.targetPath),
      stylePropPathMappingFn('maxHeight', contextData.targetPath),
    ]
  })

  return (
    <UIGridRow padded={true} variant='<---1fr--->|------172px-------|'>
      <PropertyLabel target={maximumsProps}>Maximum</PropertyLabel>
      <UIGridRow padded={false} variant='|--67px--||16px||--67px--||16px|'>
        {flexStyleNumberControl('W', 'maxWidth')}
        {spacingButton}
        {flexStyleNumberControl('H', 'maxHeight')}
        {spacingButton}
      </UIGridRow>
    </UIGridRow>
  )
})

const FlexWidthHeightRow = React.memo(() => {
  const flexWidthHeightProps = useContextSelector(InspectorPropsContext, (contextData) => {
    return [
      stylePropPathMappingFn('maxWidth', contextData.targetPath),
      stylePropPathMappingFn('maxHeight', contextData.targetPath),
    ]
  })

  return (
    <UIGridRow padded={true} variant='<---1fr--->|------172px-------|'>
      <PropertyLabel target={flexWidthHeightProps}>Size</PropertyLabel>
      <UIGridRow padded={false} variant='|--67px--||16px||--67px--||16px|'>
        {flexLayoutNumberControl('W', 'width')}
        {spacingButton}
        {flexLayoutNumberControl('H', 'height')}
        {spacingButton}
      </UIGridRow>
    </UIGridRow>
  )
})

const FlexGrowShrinkRow = React.memo(() => {
  const flexGrowShrinkProps = useContextSelector(InspectorPropsContext, (contextData) => {
    return [
      stylePropPathMappingFn('flexGrow', contextData.targetPath),
      stylePropPathMappingFn('flexShrink', contextData.targetPath),
    ]
  })

  return (
    <UIGridRow padded={true} variant='<---1fr--->|------172px-------|'>
      <PropertyLabel target={flexGrowShrinkProps}>Flex</PropertyLabel>
      <UIGridRow padded={false} variant='|--67px--||16px||--67px--||16px|'>
        <FlexShorthandNumberControl label='G' styleProp='flexGrow' />
        {spacingButton}
        <FlexShorthandNumberControl label='S' styleProp='flexShrink' />
        {spacingButton}
      </UIGridRow>
    </UIGridRow>
  )
})

const OtherPinsRow = React.memo((props: PinControlsProps) => {
  const { resetPins: resetPinsFn, framePins, togglePin } = props
  const firstXAxisControl: React.ReactElement = pinsLayoutNumberControl('top')
  const secondXAxisControl: React.ReactElement = pinsLayoutNumberControl('bottom')
  const firstYAxisControl: React.ReactElement = pinsLayoutNumberControl('left')
  const secondYAxisControl: React.ReactElement = pinsLayoutNumberControl('right')

  return (
    <UIGridRow
      alignItems='start'
      padded={true}
      variant='<---1fr--->|------172px-------|'
      style={{ height: undefined }}
    >
      <PinControls resetPins={resetPinsFn} framePins={framePins} togglePin={togglePin} />
      <FlexColumn>
        <UIGridRow padded={false} variant='|--67px--||16px||--67px--||16px|'>
          {firstXAxisControl}
          {spacingButton}
          {firstYAxisControl}
          {spacingButton}
        </UIGridRow>
        <UIGridRow padded={false} variant='|--67px--||16px||--67px--||16px|'>
          {secondXAxisControl}
          {spacingButton}
          {secondYAxisControl}
          {spacingButton}
        </UIGridRow>
      </FlexColumn>
    </UIGridRow>
  )
})

interface GiganticSizePinsSubsectionProps {
  layoutType: SelfLayoutTab
}

export const GiganticSizePinsSubsection = React.memo((props: GiganticSizePinsSubsectionProps) => {
  const { layoutType } = props

  const minWidth = useGetLayoutControlStatus('minWidth')
  const maxWidth = useGetLayoutControlStatus('maxWidth')
  const minHeight = useGetLayoutControlStatus('minHeight')
  const maxHeight = useGetLayoutControlStatus('maxHeight')

  const hasMinMaxValues =
    isNotUnsetOrDefault(minWidth) ||
    isNotUnsetOrDefault(maxWidth) ||
    isNotUnsetOrDefault(minHeight) ||
    isNotUnsetOrDefault(maxHeight)
  const [minMaxToggled, setMinMaxToggled] = usePropControlledStateV2(hasMinMaxValues)

  const { resetAllPins, framePins, togglePin } = usePinToggling()

  return (
    <div>
      <WidthHeightRow togglePin={togglePin} framePins={framePins} />
      {minMaxToggled ? (
        <>
          <MinimumsRow />
          <MaximumsRow />
        </>
      ) : null}
      {layoutType === 'flex' ? (
        <>
          <FlexGrowShrinkRow />
          <FlexWidthHeightRow />
        </>
      ) : null}
      {layoutType === 'absolute' ? (
        <OtherPinsRow resetPins={resetAllPins} framePins={framePins} togglePin={togglePin} />
      ) : null}
    </div>
  )
})
