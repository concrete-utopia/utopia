import * as React from 'react'
import {
  createLayoutPropertyPath,
  framePointForPinnedProp,
  LayoutFlexElementNumericProp,
  LayoutPinnedProp,
} from '../../../../../core/layout/layout-helpers-new'
import { LocalRectangle } from '../../../../../core/shared/math-utils'
import Utils from '../../../../../utils/utils'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { FullFrame, getFullFrame } from '../../../../frame'
import { unsetPropertyMenuItem } from '../../../common/context-menu-items'
import { CSSNumber, cssNumberToFramePin, framePinToCSSNumber } from '../../../common/css-utils'
import { FramePinsInfo, usePinToggling } from '../../../common/layout-property-path-hooks'
import { useInspectorLayoutInfo } from '../../../common/property-path-hooks'
import { GridRow } from '../../../widgets/grid-row'
import { PinControl, PinHeightControl, PinWidthControl } from '../../../controls/pin-control'
import { PropertyLabel } from '../../../widgets/property-label'
import { ResolvedLayoutProps } from '../layout-section'
import { SelfLayoutTab } from './self-layout-subsection'
import {
  useWrappedEmptyOrUnknownOnSubmitValue,
  NumberInput,
  SquareButton,
  Icons,
  FlexColumn,
} from '../../../../../uuiui'
import { betterReactMemo } from '../../../../../uuiui-deps'

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

export const PinsLayoutNumberControl = betterReactMemo(
  'PinsLayoutNumberControl',
  (props: PinsLayoutNumberControlProps) => {
    const framePoint = framePointForPinnedProp(props.prop)
    const pointInfo = useInspectorLayoutInfo(props.prop)
    const framePinToUse = pointInfo.value
    const asCSSNumber = framePinToCSSNumber(framePinToUse)
    const [onSubmitValue, onTransientSubmitValue] = pointInfo.useSubmitValueFactory(
      (newValue: CSSNumber) => {
        return cssNumberToFramePin(newValue)
      },
    )

    const wrappedOnSubmit = useWrappedEmptyOrUnknownOnSubmitValue(
      onSubmitValue,
      pointInfo.onUnsetValues,
    )
    const wrappedOnTransientSubmit = useWrappedEmptyOrUnknownOnSubmitValue(
      onTransientSubmitValue,
      pointInfo.onUnsetValues,
    )

    return (
      <InspectorContextMenuWrapper
        id={`position-${props.prop}-context-menu`}
        items={[unsetPropertyMenuItem(framePoint, pointInfo.onUnsetValues)]}
        data={{}}
      >
        <NumberInput
          value={asCSSNumber}
          id={`position-${props.prop}-number-input`}
          testId={`position-${props.prop}-number-input`}
          labelInner={props.label}
          onSubmitValue={wrappedOnSubmit}
          onTransientSubmitValue={wrappedOnTransientSubmit}
          controlStatus={pointInfo.controlStatus}
          numberType={'UnitlessPercent'}
          defaultUnitToHide={'px'}
        />
      </InspectorContextMenuWrapper>
    )
  },
)

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

export const FlexStyleNumberControl = betterReactMemo(
  'FlexStyleNumberControl',
  (props: FlexStyleNumberControlProps) => {
    const layoutPropInfo = useInspectorLayoutInfo(props.styleProp)

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

export const FlexLayoutNumberControl = betterReactMemo(
  'FlexLayoutNumberControl',
  (props: FlexLayoutNumberControlProps) => {
    const layoutPropInfo = useInspectorLayoutInfo(props.layoutProp)
    const asCSSNumber = framePinToCSSNumber(layoutPropInfo.value)
    const [onSubmitValue, onTransientSubmitValue] = layoutPropInfo.useSubmitValueFactory(
      (newValue: CSSNumber, oldValue) => {
        return cssNumberToFramePin(newValue)
      },
    )

    const wrappedOnSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
      onSubmitValue,
      layoutPropInfo.onUnsetValues,
    )
    const wrappedOnTransientSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
      onTransientSubmitValue,
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
          value={asCSSNumber}
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

interface PinControlsProps {
  frame: LocalRectangle | null
  resetPins: () => void
  framePins: FramePinsInfo
  togglePin: (prop: LayoutPinnedProp) => void
}

const PinControls = betterReactMemo('PinControls', (props: PinControlsProps) => {
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
  frame: LocalRectangle | null
  layoutType: SelfLayoutTab
  toggleMinMax: () => void
  togglePin: (prop: LayoutPinnedProp) => void
  framePins: FramePinsInfo
  parentFlexAxis: 'horizontal' | 'vertical' | null
  aspectRatioLocked: boolean
  toggleAspectRatioLock: () => void
}

const WidthHeightRow = betterReactMemo('WidthHeightRow', (props: WidthHeightRowProps) => {
  const {
    layoutType,
    toggleMinMax,
    togglePin,
    framePins,
    frame,
    parentFlexAxis,
    aspectRatioLocked,
    toggleAspectRatioLock,
  } = props

  let widthControl: React.ReactElement | null = null
  let heightControl: React.ReactElement | null = null
  if (layoutType === 'flex') {
    switch (parentFlexAxis) {
      case 'horizontal':
      case null:
        widthControl = flexLayoutNumberControl('W', 'FlexFlexBasis')
        break
      case 'vertical':
        heightControl = flexLayoutNumberControl('H', 'FlexFlexBasis')
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
    <GridRow padded={true} type='<---1fr--->|------172px-------|'>
      <div
        id='width-height-row-toggles'
        style={{
          display: 'flex',
          flexDirection: 'row',
          justifyContent: 'space-between',
        }}
      >
        {layoutType === 'absolute' ? (
          <>
            <PinWidthControl
              framePins={framePins}
              toggleWidth={toggleWidth}
              controlStatus='simple'
            />
            <PinHeightControl
              framePins={framePins}
              toggleHeight={toggleHeight}
              controlStatus='simple'
            />
          </>
        ) : null}
      </div>
      <GridRow padded={false} type='|--67px--||16px||--67px--||16px|'>
        {widthControl}
        <SquareButton onClick={toggleAspectRatioLock} style={{ width: 16, height: 16 }}>
          {aspectRatioLocked ? <Icons.LockClosed /> : <Icons.LockOpen />}
        </SquareButton>
        {heightControl}
        <SquareButton onClick={toggleMinMax} style={{ width: 16, height: 16, fontSize: 8 }}>
          min
          <br />
          max
        </SquareButton>
      </GridRow>
    </GridRow>
  )
})

const minimumsProps = [createLayoutPropertyPath('minWidth'), createLayoutPropertyPath('minHeight')]

const MinimumsRow = betterReactMemo('MinimumsRow', () => {
  return (
    <GridRow padded={true} type='<---1fr--->|------172px-------|'>
      <PropertyLabel target={minimumsProps}>Minimum</PropertyLabel>
      <GridRow padded={false} type='|--67px--||16px||--67px--||16px|'>
        {flexStyleNumberControl('W', 'minWidth')}
        {spacingButton}
        {flexStyleNumberControl('H', 'minHeight')}
        {spacingButton}
      </GridRow>
    </GridRow>
  )
})

const maximumsProps = [createLayoutPropertyPath('maxWidth'), createLayoutPropertyPath('maxHeight')]

const MaximumsRow = betterReactMemo('MaximumsRow', () => {
  return (
    <GridRow padded={true} type='<---1fr--->|------172px-------|'>
      <PropertyLabel target={maximumsProps}>Maximum</PropertyLabel>
      <GridRow padded={false} type='|--67px--||16px||--67px--||16px|'>
        {flexStyleNumberControl('W', 'maxWidth')}
        {spacingButton}
        {flexStyleNumberControl('H', 'maxHeight')}
        {spacingButton}
      </GridRow>
    </GridRow>
  )
})

const flexWidthHeightProps = [createLayoutPropertyPath('Width'), createLayoutPropertyPath('Height')]

const FlexWidthHeightRow = betterReactMemo('FixedWidthHeightRow', () => {
  return (
    <GridRow padded={true} type='<---1fr--->|------172px-------|'>
      <PropertyLabel target={flexWidthHeightProps}>Size</PropertyLabel>
      <GridRow padded={false} type='|--67px--||16px||--67px--||16px|'>
        {flexLayoutNumberControl('W', 'Width')}
        {spacingButton}
        {flexLayoutNumberControl('H', 'Height')}
        {spacingButton}
      </GridRow>
    </GridRow>
  )
})

const flexGrowShrinkProps = [
  createLayoutPropertyPath('flexGrow'),
  createLayoutPropertyPath('flexShrink'),
]

const FlexGrowShrinkRow = betterReactMemo('FlexGrowShrinkRow', () => {
  return (
    <GridRow padded={true} type='<---1fr--->|------172px-------|'>
      <PropertyLabel target={flexGrowShrinkProps}>Flex</PropertyLabel>
      <GridRow padded={false} type='|--67px--||16px||--67px--||16px|'>
        {flexStyleNumberControl('G', 'flexGrow')}
        {spacingButton}
        {flexStyleNumberControl('S', 'flexShrink')}
        {spacingButton}
      </GridRow>
    </GridRow>
  )
})

const OtherPinsRow = betterReactMemo('OtherPinsRow', (props: PinControlsProps) => {
  const { frame, resetPins: resetPinsFn, framePins, togglePin } = props
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
    <GridRow
      alignItems='start'
      padded={true}
      type='<---1fr--->|------172px-------|'
      style={{ height: undefined }}
    >
      <PinControls
        frame={frame}
        resetPins={resetPinsFn}
        framePins={framePins}
        togglePin={togglePin}
      />
      <FlexColumn>
        <GridRow padded={false} type='|--67px--||16px||--67px--||16px|'>
          {firstXAxisControl}
          {spacingButton}
          {firstYAxisControl}
          {spacingButton}
        </GridRow>
        <GridRow padded={false} type='|--67px--||16px||--67px--||16px|'>
          {secondXAxisControl}
          {spacingButton}
          {secondYAxisControl}
          {spacingButton}
        </GridRow>
      </FlexColumn>
    </GridRow>
  )
})

interface GiganticSizePinsSubsectionProps {
  input: ResolvedLayoutProps
  layoutType: SelfLayoutTab
  parentFlexAxis: 'horizontal' | 'vertical' | null
  aspectRatioLocked: boolean
  toggleAspectRatioLock: () => void
}

export const GiganticSizePinsSubsection = betterReactMemo(
  'GiganticSizePinsSubsection',
  (props: GiganticSizePinsSubsectionProps) => {
    const { input, layoutType, parentFlexAxis, aspectRatioLocked, toggleAspectRatioLock } = props
    const { frame } = input

    const minWidth = useInspectorLayoutInfo('minWidth')
    const maxWidth = useInspectorLayoutInfo('maxWidth')
    const minHeight = useInspectorLayoutInfo('minHeight')
    const maxHeight = useInspectorLayoutInfo('maxHeight')

    const hasMinMaxValues =
      minWidth.controlStatus !== 'unset' ||
      maxWidth.controlStatus !== 'unset' ||
      minHeight.controlStatus !== 'unset' ||
      maxHeight.controlStatus !== 'unset'

    const [minMaxToggled, setMinMaxToggled] = React.useState<boolean>(hasMinMaxValues)
    const toggleMinMax = React.useCallback(() => {
      setMinMaxToggled(!minMaxToggled)
    }, [minMaxToggled, setMinMaxToggled])

    const { resetAllPins, framePins, togglePin } = usePinToggling()

    return (
      <>
        <WidthHeightRow
          frame={frame}
          layoutType={layoutType}
          togglePin={togglePin}
          framePins={framePins}
          toggleMinMax={toggleMinMax}
          parentFlexAxis={parentFlexAxis}
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
          <OtherPinsRow
            frame={frame}
            resetPins={resetAllPins}
            framePins={framePins}
            togglePin={togglePin}
          />
        ) : null}
      </>
    )
  },
)
