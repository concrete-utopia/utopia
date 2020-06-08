import * as React from 'react'
import { LayoutSystem } from 'utopia-api'
import { betterReactMemo } from 'uuiui-deps'
import Utils from '../../../../utils/utils'
import { Icn, useWrappedEmptyOnSubmitValue } from '../../../../uuiui'
import { ControlStatus, ControlStyleDefaults, getControlStyles } from '../../common/control-status'
import { cssEmptyValues, layoutEmptyValues } from '../../common/css-utils'
import { useInspectorLayoutInfo, useInspectorStyleInfo } from '../../common/property-path-hooks'
import { SegmentControl, SegmentOption } from '../../controls/segment-control'
import { PropertyRow } from '../../widgets/property-row'
import {
  FlexAlignContentControl,
  FlexAlignItemsControl,
  FlexDirectionControl,
  FlexGapControl,
  FlexJustifyContentControl,
  FlexWrapControl,
  getDirectionAwareLabels,
} from '../layout-section/flex-container-subsection/flex-container-controls'

const simpleControlStatus: ControlStatus = 'simple'
const simpleControlStyles = getControlStyles(simpleControlStatus)

const scenePropertyRowStyle = {
  gridColumnGap: 16,
  marginTop: 8,
}

export const SceneFlexContainerSection = betterReactMemo('SceneFlexContainerSection', () => {
  const styleDisplayMetadata = useInspectorStyleInfo('display')
  const flexWrap = useInspectorLayoutInfo('flexWrap')
  const flexDirection = useInspectorLayoutInfo('flexDirection')
  const alignItems = useInspectorLayoutInfo('alignItems')
  const alignContent = useInspectorLayoutInfo('alignContent')
  const justifyContent = useInspectorLayoutInfo('justifyContent')
  const flexGapMain = useInspectorLayoutInfo('FlexGapMain')

  const wrappedFlexGapOnSubmitValue = useWrappedEmptyOnSubmitValue(
    flexGapMain.onSubmitValue,
    flexGapMain.onUnsetValues,
  )
  const wrappedFlexGapOnTransientSubmitValue = useWrappedEmptyOnSubmitValue(
    flexGapMain.onSubmitValue,
    flexGapMain.onUnsetValues,
  )

  if (styleDisplayMetadata.value === 'flex') {
    const flexDirectionValue = Utils.defaultIfNull(
      cssEmptyValues.flexDirection,
      flexDirection.value,
    )
    const alignItemsValue = Utils.defaultIfNull(cssEmptyValues.alignItems, alignItems.value)
    const flexWrapValue = Utils.defaultIfNull(cssEmptyValues.flexWrap, flexWrap.value)
    const justifyContentValue = Utils.defaultIfNull(
      cssEmptyValues.justifyContent,
      justifyContent.value,
    )
    const flexGapMainValue = Utils.defaultIfNull(layoutEmptyValues.gapMain, flexGapMain.value)
    const alignContentValue = Utils.defaultIfNull(cssEmptyValues.alignContent, alignContent.value)

    const {
      justifyFlexStart,
      justifyFlexEnd,
      alignDirection,
      alignItemsFlexStart,
      alignItemsFlexEnd,
      alignContentFlexStart,
      alignContentFlexEnd,
    } = getDirectionAwareLabels(flexWrapValue, flexDirectionValue)

    return (
      <>
        <PropertyRow>
          <FlexDirectionControl
            value={flexDirectionValue}
            controlStatus={simpleControlStatus}
            controlStyles={simpleControlStyles}
            onSubmitValue={flexDirection.onSubmitValue}
            onUnset={flexDirection.onUnsetValues}
            flexWrap={flexWrapValue}
          />
          <FlexAlignItemsControl
            value={alignItemsValue}
            controlStatus={simpleControlStatus}
            controlStyles={simpleControlStyles}
            onSubmitValue={alignItems.onSubmitValue}
            onUnset={alignItems.onUnsetValues}
            alignDirection={alignDirection}
            alignItemsFlexStart={alignItemsFlexStart}
            alignItemsFlexEnd={alignItemsFlexEnd}
          />
          <FlexWrapControl
            value={flexWrapValue}
            onSubmitValue={flexWrap.onSubmitValue}
            onUnset={flexWrap.onUnsetValues}
            controlStatus={simpleControlStatus}
            controlStyles={simpleControlStyles}
          />
          <FlexJustifyContentControl
            value={justifyContentValue}
            onSubmitValue={justifyContent.onSubmitValue}
            onUnset={justifyContent.onUnsetValues}
            controlStatus={simpleControlStatus}
            controlStyles={simpleControlStyles}
            flexDirection={flexDirectionValue}
            justifyFlexStart={justifyFlexStart}
            justifyFlexEnd={justifyFlexEnd}
          />
        </PropertyRow>
        <PropertyRow>
          <span>Gap</span>
          <FlexGapControl
            value={flexGapMainValue}
            onSubmitValue={wrappedFlexGapOnSubmitValue}
            onTransientSubmitValue={wrappedFlexGapOnTransientSubmitValue}
            onUnset={flexGapMain.onUnsetValues}
            controlStatus={simpleControlStatus}
            controlStyles={simpleControlStyles}
          />
        </PropertyRow>
        <div
          className='inspector-divider'
          style={{
            width: 'calc(100% - 32px)',
            height: 1,
            backgroundColor: ControlStyleDefaults.SetBorderColor,
            margin: '8px 16px 16px',
          }}
        />
        <PropertyRow style={{ marginTop: 16 }}>
          <FlexAlignContentControl
            value={alignContentValue}
            onSubmitValue={alignContent.onSubmitValue}
            onUnset={alignContent.onUnsetValues}
            controlStatus={simpleControlStatus}
            controlStyles={simpleControlStyles}
            alignDirection={alignDirection}
            alignContentFlexStart={alignContentFlexStart}
            alignContentFlexEnd={alignContentFlexEnd}
          />
        </PropertyRow>
      </>
    )
  } else {
    return null
  }
})

export const SceneContainerSections = betterReactMemo('SceneContainerSections', () => {
  // FIXME We need a hook for checking the actual layout system since it's now spread across 2 possible props
  const layoutSystemMetadata = useInspectorLayoutInfo('LayoutSystem')
  const [
    onSubmitValue,
  ] = layoutSystemMetadata.useSubmitValueFactory((newValue: LayoutSystem | 'flex'):
    | LayoutSystem
    | undefined => (newValue === 'flex' ? undefined : newValue))
  return (
    <>
      <PropertyRow style={scenePropertyRowStyle}>
        <div
          style={{
            gridColumn: '1 / span 6',
          }}
        >
          <SegmentControl<LayoutSystem | 'flex'>
            id={'layoutSystem'}
            onSubmitValue={onSubmitValue}
            value={layoutSystemMetadata.value ?? LayoutSystem.PinSystem}
            options={layoutSystemOptions}
            controlStatus={simpleControlStatus}
            controlStyles={simpleControlStyles}
          />
        </div>
      </PropertyRow>
      <SceneFlexContainerSection />
    </>
  )
})

SceneContainerSections.displayName = 'SceneContainerSections'

const layoutSystemOptions: Array<SegmentOption<LayoutSystem | 'flex'>> = [
  {
    value: LayoutSystem.PinSystem,
    tooltip: 'Layout children with pins',
    label: (
      <>
        View <Icn category='layout/systems' type='pins' color='darkgray' width={16} height={16} />
      </>
    ),
  },
  {
    value: 'flex',
    tooltip: 'Layout children with flexbox',
    label: (
      <>
        View{' '}
        <Icn category='layout/systems' type='flexbox' color='darkgray' width={16} height={16} />
      </>
    ),
  },
]
