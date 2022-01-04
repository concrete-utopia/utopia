import React from 'react'
import {
  createLayoutPropertyPath,
  framePointForPinnedProp,
  LayoutFlexElementNumericProp,
  LayoutPinnedProp,
  LayoutProp,
  StyleLayoutProp,
} from '../../../../../core/layout/layout-helpers-new'
import { LocalRectangle } from '../../../../../core/shared/math-utils'
import Utils from '../../../../../utils/utils'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { FullFrame, getFullFrame } from '../../../../frame'
import { unsetPropertyMenuItem } from '../../../common/context-menu-items'
import {
  CSSNumber,
  cssNumberToFramePin,
  framePinToCSSNumber,
  isCSSNumber,
  ParsedCSSPropertiesKeys,
} from '../../../common/css-utils'
import { FramePinsInfo, usePinToggling } from '../../../common/layout-property-path-hooks'
import {
  InspectorInfo,
  stylePropPathMappingFn,
  useInspectorLayoutInfo,
} from '../../../common/property-path-hooks'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import { PinControl, PinHeightControl, PinWidthControl } from '../../../controls/pin-control'
import { PropertyLabel } from '../../../widgets/property-label'
import { SelfLayoutTab } from './self-layout-subsection'
import {
  useWrappedEmptyOrUnknownOnSubmitValue,
  NumberInput,
  SquareButton,
  Icons,
  FlexColumn,
  SimpleNumberInput,
  useColorTheme,
} from '../../../../../uuiui'
import {
  InspectorInfoWithPropKeys,
  useInspectorInfoLonghandShorthand,
} from '../../../common/longhand-shorthand-hooks'
import { isNotUnsetOrDefault } from '../../../common/control-status'
import { usePropControlledStateV2 } from '../../../common/inspector-utils'

interface PinsLayoutNumberControlProps {
  label: string
  prop: LayoutPinnedProp
}

export const pinLabels: { [key in LayoutPinnedProp]: string } = {
  PinnedLeft: 'L',
  PinnedTop: 'T',
  PinnedRight: 'R',
  PinnedBottom: 'B',
  PinnedCenterX: 'cX',
  PinnedCenterY: 'cY',
  Width: 'W',
  Height: 'H',
}

export const PinsLayoutNumberControl = React.memo((props: PinsLayoutNumberControlProps) => {
  const framePoint = framePointForPinnedProp(props.prop)
  const pointInfo = useInspectorLayoutInfo(props.prop)

  const wrappedOnSubmit = useWrappedEmptyOrUnknownOnSubmitValue(
    pointInfo.onSubmitValue,
    pointInfo.onUnsetValues,
  )
  const wrappedOnTransientSubmit = useWrappedEmptyOrUnknownOnSubmitValue(
    pointInfo.onTransientSubmitValue,
    pointInfo.onUnsetValues,
  )

  return (
    <InspectorContextMenuWrapper
      id={`position-${props.prop}-context-menu`}
      items={[unsetPropertyMenuItem(framePoint, pointInfo.onUnsetValues)]}
      data={{}}
    >
      <NumberInput
        value={pointInfo.value}
        id={`position-${props.prop}-number-input`}
        testId={`position-${props.prop}-number-input`}
        labelInner={props.label}
        onSubmitValue={wrappedOnSubmit}
        onTransientSubmitValue={wrappedOnTransientSubmit}
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
  const layoutPropInfo = useInspectorInfoLonghandShorthand(
    ['flexGrow', 'flexShrink', 'flexBasis'],
    'flex',
    createLayoutPropertyPath,
  )[props.styleProp] as InspectorInfoWithPropKeys<'flexGrow' | 'flexShrink', 'flex'>
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
    const layoutPropInfo = useInspectorInfoLonghandShorthand(
      ['flexBasis', 'flexGrow', 'flexShrink'],
      'flex',
      createLayoutPropertyPath,
    ).flexBasis as InspectorInfoWithPropKeys<'flexBasis', 'flex'>
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
          value={layoutPropInfo.value}
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
  return <PinsLayoutNumberControl label={pinLabels[prop]} prop={prop} />
}

function flexLayoutNumberControl(label: string, layoutProp: LayoutFlexElementNumericProp) {
  return <FlexLayoutNumberControl label={label} layoutProp={layoutProp} />
}

function flexStyleNumberControl(label: string, styleProp: StyleLayoutNumberProp) {
  return <FlexStyleNumberControl label={label} styleProp={styleProp} />
}

const spacingButton = <SquareButton />

interface WidthHeightRowProps {
  layoutType: SelfLayoutTab
  toggleMinMax: () => void
  togglePin: (prop: LayoutPinnedProp) => void
  framePins: FramePinsInfo
  parentFlexDirection: string | null
  aspectRatioLocked: boolean
  toggleAspectRatioLock: () => void
}

const WidthHeightRow = React.memo((props: WidthHeightRowProps) => {
  const colorTheme = useColorTheme()
  const {
    layoutType,
    toggleMinMax,
    togglePin,
    framePins,
    parentFlexDirection,
    aspectRatioLocked,
    toggleAspectRatioLock,
  } = props

  let widthControl: React.ReactElement | null = null
  let heightControl: React.ReactElement | null = null
  if (layoutType === 'flex') {
    switch (parentFlexDirection) {
      case 'row':
      case 'row-reverse':
      case null:
        widthControl = <FlexBasisShorthandCSSNumberControl label='W' />
        break
      case 'column':
      case 'column-reverse':
        heightControl = <FlexBasisShorthandCSSNumberControl label='H' />
        break
      default:
        break
    }
  } else {
    widthControl = pinsLayoutNumberControl('Width')
    heightControl = pinsLayoutNumberControl('Height')
  }

  const toggleWidth = React.useCallback(() => {
    togglePin('Width')
  }, [togglePin])

  const toggleHeight = React.useCallback(() => {
    togglePin('Height')
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
      <UIGridRow padded={false} variant='|--67px--||16px||--67px--||16px|'>
        {widthControl}
        <SquareButton onClick={toggleAspectRatioLock} style={{ width: 16, height: 16 }}>
          {aspectRatioLocked ? <Icons.LockClosed /> : <Icons.LockOpen />}
        </SquareButton>
        {heightControl}
        <SquareButton
          data-testid='toggle-min-max-button'
          onClick={toggleMinMax}
          style={{
            width: 16,
            height: 16,
            fontSize: 8,
            color: colorTheme.subduedForeground.value,
          }}
        >
          <Icons.FourDots />
        </SquareButton>
      </UIGridRow>
    </UIGridRow>
  )
})

const minimumsProps = [createLayoutPropertyPath('minWidth'), createLayoutPropertyPath('minHeight')]

const MinimumsRow = React.memo(() => {
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

const maximumsProps = [createLayoutPropertyPath('maxWidth'), createLayoutPropertyPath('maxHeight')]

const MaximumsRow = React.memo(() => {
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

const flexWidthHeightProps = [createLayoutPropertyPath('Width'), createLayoutPropertyPath('Height')]

const FlexWidthHeightRow = React.memo(() => {
  return (
    <UIGridRow padded={true} variant='<---1fr--->|------172px-------|'>
      <PropertyLabel target={flexWidthHeightProps}>Size</PropertyLabel>
      <UIGridRow padded={false} variant='|--67px--||16px||--67px--||16px|'>
        {flexLayoutNumberControl('W', 'Width')}
        {spacingButton}
        {flexLayoutNumberControl('H', 'Height')}
        {spacingButton}
      </UIGridRow>
    </UIGridRow>
  )
})

const flexGrowShrinkProps = [
  createLayoutPropertyPath('flexGrow'),
  createLayoutPropertyPath('flexShrink'),
]

const FlexGrowShrinkRow = React.memo(() => {
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
  let firstXAxisControl: React.ReactElement = <div />
  let secondXAxisControl: React.ReactElement = <div />
  let firstYAxisControl: React.ReactElement = <div />
  let secondYAxisControl: React.ReactElement = <div />
  // TODO LAYOUT update these when there are new ways to set centerX/centerY
  // const centerXInfo = useInspectorLayoutInfo('PinnedCenterX')
  // const topInfo = useInspectorLayoutInfo('PinnedTop')
  // if (centerXInfo.value == null) {
  // No CenterX value, just show top and bottom.
  firstXAxisControl = pinsLayoutNumberControl('PinnedTop')
  secondXAxisControl = pinsLayoutNumberControl('PinnedBottom')
  // } else {
  //   // We have a CenterX value, so put that first and then top or bottom after it.
  //   firstXAxisControl = pinsLayoutNumberControl(frame, 'PinnedCenterX')
  //   if (topInfo.value == null) {
  //     secondXAxisControl = pinsLayoutNumberControl(frame, 'PinnedBottom')
  //   } else {
  //     secondXAxisControl = pinsLayoutNumberControl(frame, 'PinnedTop')
  //   }
  // }

  // TODO LAYOUT update these when there are new ways to set centerX/centerY
  // const centerYInfo = useInspectorLayoutInfo('PinnedCenterY')
  // const leftInfo = useInspectorLayoutInfo('PinnedLeft')
  // if (centerYInfo.value == null) {
  // No CenterY value, just show left and right.
  firstYAxisControl = pinsLayoutNumberControl('PinnedLeft')
  secondYAxisControl = pinsLayoutNumberControl('PinnedRight')
  // } else {
  //   // We have a CenterY value, so put that first and then left or right after it.
  //   firstYAxisControl = pinsLayoutNumberControl(frame, 'PinnedCenterY')
  //   if (leftInfo.value == null) {
  //     secondYAxisControl = pinsLayoutNumberControl(frame, 'PinnedRight')
  //   } else {
  //     secondYAxisControl = pinsLayoutNumberControl(frame, 'PinnedLeft')
  //   }
  // }

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
  parentFlexDirection: string | null
  aspectRatioLocked: boolean
  toggleAspectRatioLock: () => void
}

export const GiganticSizePinsSubsection = React.memo((props: GiganticSizePinsSubsectionProps) => {
  const { layoutType, parentFlexDirection, aspectRatioLocked, toggleAspectRatioLock } = props

  const minWidth = useInspectorLayoutInfo('minWidth')
  const maxWidth = useInspectorLayoutInfo('maxWidth')
  const minHeight = useInspectorLayoutInfo('minHeight')
  const maxHeight = useInspectorLayoutInfo('maxHeight')

  const hasMinMaxValues =
    isNotUnsetOrDefault(minWidth.controlStatus) ||
    isNotUnsetOrDefault(maxWidth.controlStatus) ||
    isNotUnsetOrDefault(minHeight.controlStatus) ||
    isNotUnsetOrDefault(maxHeight.controlStatus)
  const [minMaxToggled, setMinMaxToggled] = usePropControlledStateV2(hasMinMaxValues)
  const toggleMinMax = React.useCallback(() => {
    setMinMaxToggled(!minMaxToggled)
  }, [minMaxToggled, setMinMaxToggled])

  const { resetAllPins, framePins, togglePin } = usePinToggling()

  return (
    <>
      <WidthHeightRow
        layoutType={layoutType}
        togglePin={togglePin}
        framePins={framePins}
        toggleMinMax={toggleMinMax}
        parentFlexDirection={parentFlexDirection}
        aspectRatioLocked={aspectRatioLocked}
        toggleAspectRatioLock={toggleAspectRatioLock}
      />
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
    </>
  )
})
