import React from 'react'

import { useContextSelector } from 'use-context-selector'
import { LayoutSystem } from 'utopia-api/core'
import {
  DetectedLayoutSystem,
  SettableLayoutSystem,
} from '../../../../../core/shared/element-template'
import { PropertyPath } from '../../../../../core/shared/project-file-types'
import { when } from '../../../../../utils/react-conditionals'
import {
  ChainedNumberInput,
  FunctionIcons,
  Icons,
  SquareButton,
  useWrappedEmptyOrUnknownOnSubmitValue,
} from '../../../../../uuiui'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { switchLayoutSystem } from '../../../../editor/actions/action-creators'
import { useDispatch } from '../../../../editor/store/dispatch-context'
import { optionalAddOnUnsetValues } from '../../../common/context-menu-items'
import {
  ControlStatus,
  ControlStyles,
  getControlStatusFromPropertyStatus,
  getControlStyles,
} from '../../../common/control-status'
import { CSSNumber, isCSSNumber, UnknownOrEmptyInput } from '../../../common/css-utils'
import { useInspectorInfoLonghandShorthand } from '../../../common/longhand-shorthand-hooks'
import {
  InspectorCallbackContext,
  InspectorInfo,
  InspectorPropsContext,
  stylePropPathMappingFn,
  useInspectorInfoSimpleUntyped,
  useInspectorStyleInfo,
} from '../../../common/property-path-hooks'
import { OptionChainControl } from '../../../controls/option-chain-control'
import { PropertyLabel } from '../../../widgets/property-label'
import { UIGridRow } from '../../../widgets/ui-grid-row'

function useDefaultedLayoutSystemInfo(): {
  value: LayoutSystem | 'flow'
  controlStatus: ControlStatus
  controlStyles: ControlStyles
} {
  const styleDisplayMetadata = useInspectorStyleInfo('display')

  let metadataToUse: InspectorInfo<any> = styleDisplayMetadata
  if (styleDisplayMetadata.value === 'flex') {
    metadataToUse = styleDisplayMetadata
  }

  if (metadataToUse.value == null) {
    const updatedPropertyStatus = {
      ...metadataToUse.propertyStatus,
      set: true,
    }
    const controlStatus = getControlStatusFromPropertyStatus(updatedPropertyStatus)
    const controlStyles = getControlStyles(controlStatus)
    return {
      value: 'flow',
      controlStatus,
      controlStyles,
    }
  } else {
    return {
      value: metadataToUse.value,
      controlStatus: metadataToUse.controlStatus,
      controlStyles: metadataToUse.controlStyles,
    }
  }
}

export function useLayoutSystemData() {
  const dispatch = useDispatch()
  const targetPath = useContextSelector(InspectorPropsContext, (contextData) => {
    return contextData.targetPath
  })
  const onLayoutSystemChange = React.useCallback(
    (layoutSystem: SettableLayoutSystem) => {
      switch (layoutSystem) {
        case LayoutSystem.PinSystem:
        case 'flow':
        case 'flex':
          dispatch([switchLayoutSystem(layoutSystem, targetPath)], 'everyone')
          break
        case LayoutSystem.Group:
        case 'grid':
          // 'grid' and 'group' are not clickable buttons, they only have an indicative role
          break
        default:
          const _exhaustiveCheck: never = layoutSystem
          throw new Error(`Unknown layout system ${JSON.stringify(layoutSystem)}`)
      }
    },
    [dispatch, targetPath],
  )

  const { value, controlStatus, controlStyles } = useDefaultedLayoutSystemInfo()

  return {
    onSubmitValue: onLayoutSystemChange,
    value,
    controlStatus,
    controlStyles,
  }
}

interface LayoutSystemControlProps {
  layoutSystem: DetectedLayoutSystem | null
  providesCoordinateSystemForChildren: boolean
}

export const LayoutSystemControl = React.memo((props: LayoutSystemControlProps) => {
  const layoutSystemData = useLayoutSystemData()
  const detectedLayoutSystem = props.layoutSystem ?? layoutSystemData.value
  return (
    <OptionChainControl
      id={'layoutSystem'}
      key={'layoutSystem'}
      testId={'layoutSystem'}
      onSubmitValue={layoutSystemData.onSubmitValue}
      value={detectedLayoutSystem}
      options={layoutSystemOptions}
      controlStatus={layoutSystemData.controlStatus}
      controlStyles={layoutSystemData.controlStyles}
    />
  )
})

const layoutSystemOptions = [
  {
    value: 'flex',
    tooltip: 'Layout children with flexbox',
    label: 'Flex',
  },

  {
    value: 'grid',
    tooltip: 'Layout children with grid',
    label: 'Grid',
  },
]

export function buildPaddingPropsToUnset(
  propertyTarget: ReadonlyArray<string>,
): Array<PropertyPath> {
  return [
    stylePropPathMappingFn('padding', propertyTarget),
    stylePropPathMappingFn('paddingLeft', propertyTarget),
    stylePropPathMappingFn('paddingTop', propertyTarget),
    stylePropPathMappingFn('paddingRight', propertyTarget),
    stylePropPathMappingFn('paddingBottom', propertyTarget),
  ]
}

export const PaddingRow = React.memo(() => {
  const targetPath = useContextSelector(InspectorPropsContext, (contextData) => {
    return contextData.targetPath
  })
  const paddingPropsToUnset = React.useMemo(() => {
    return buildPaddingPropsToUnset(targetPath)
  }, [targetPath])
  const metadata = useInspectorInfoSimpleUntyped(
    paddingPropsToUnset,
    (v) => v,
    (v) => v,
  )

  const contextMenuLabel = React.useMemo(() => ['all paddings'], [])
  const contextMenuItems = React.useMemo(
    () =>
      optionalAddOnUnsetValues(
        metadata.propertyStatus.set,
        contextMenuLabel,
        metadata.onUnsetValues,
      ),
    [contextMenuLabel, metadata.propertyStatus.set, metadata.onUnsetValues],
  )

  return (
    <InspectorContextMenuWrapper
      id='padding-subsection-context-menu'
      items={contextMenuItems}
      data={null}
    >
      <UIGridRow tall padded={true} variant='<---1fr--->|------172px-------|'>
        <PropertyLabel
          target={paddingPropsToUnset}
          propNamesToUnset={contextMenuLabel}
          style={{ paddingBottom: 12 }}
        >
          Padding
        </PropertyLabel>
        <PaddingControl />
      </UIGridRow>
    </InspectorContextMenuWrapper>
  )
})

type ControlMode = 'single' | 'axis' | 'global'

interface PaddingValue {
  controlStatus: ControlStatus
  value: CSSNumber
}

function allDirectionsSetWithEqualValues(values: PaddingValue[]) {
  return {
    equal: values.every(
      (v) =>
        v.controlStatus === 'simple' &&
        v.value.value === values[0].value.value &&
        v.value.unit === values[0].value.unit,
    ),
    value: values[0].value,
  }
}

export const PaddingControl = React.memo(() => {
  const { paddingTop, paddingRight, paddingBottom, paddingLeft } =
    useInspectorInfoLonghandShorthand(
      ['paddingTop', 'paddingRight', 'paddingBottom', 'paddingLeft'],
      'padding',
      stylePropPathMappingFn,
    )

  const initial = {
    global: allDirectionsSetWithEqualValues([paddingTop, paddingBottom, paddingLeft, paddingRight]),
    horizontal: allDirectionsSetWithEqualValues([paddingLeft, paddingRight]),
    vertical: allDirectionsSetWithEqualValues([paddingTop, paddingBottom]),
  }

  const [mode, setMode] = React.useState<ControlMode>(
    initial.global.equal
      ? 'global'
      : initial.horizontal.equal && initial.vertical.equal
      ? 'axis'
      : 'single',
  )

  const [globalValue, setGlobalValue] = React.useState<number | null>(
    initial.global.equal ? initial.global.value.value : null,
  )
  const [horizontalValue, setHorizontalValue] = React.useState<number | null>(
    initial.horizontal.equal ? initial.horizontal.value.value : null,
  )
  const [verticalValue, setVerticalValue] = React.useState<number | null>(
    initial.vertical.equal ? initial.vertical.value.value : null,
  )

  React.useEffect(() => {
    const global = allDirectionsSetWithEqualValues([
      paddingTop,
      paddingBottom,
      paddingLeft,
      paddingRight,
    ])
    setGlobalValue(global.equal ? global.value.value : null)

    const horizontal = allDirectionsSetWithEqualValues([paddingLeft, paddingRight])
    setHorizontalValue(horizontal.equal ? horizontal.value.value : null)

    const vertical = allDirectionsSetWithEqualValues([paddingTop, paddingBottom])
    setVerticalValue(vertical.equal ? vertical.value.value : null)
  }, [paddingBottom, paddingTop, paddingLeft, paddingRight])

  const paddingTopOnSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    paddingTop.onSubmitValue,
    paddingTop.onUnsetValues,
  )
  const paddingTopOnTransientSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    paddingTop.onTransientSubmitValue,
    paddingTop.onUnsetValues,
  )
  const paddingRightOnSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    paddingRight.onSubmitValue,
    paddingRight.onUnsetValues,
  )
  const paddingRightOnTransientSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    paddingRight.onTransientSubmitValue,
    paddingRight.onUnsetValues,
  )
  const paddingBottomOnSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    paddingBottom.onSubmitValue,
    paddingBottom.onUnsetValues,
  )
  const paddingBottomOnTransientSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    paddingBottom.onTransientSubmitValue,
    paddingBottom.onUnsetValues,
  )
  const paddingLeftOnSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    paddingLeft.onSubmitValue,
    paddingLeft.onUnsetValues,
  )
  const paddingLeftOnTransientSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    paddingLeft.onTransientSubmitValue,
    paddingLeft.onUnsetValues,
  )

  const cycleMode = React.useCallback(() => {
    switch (mode) {
      case 'single':
        setMode('global')
        return
      case 'global':
        setMode('axis')
        return
      case 'axis':
        setMode('single')
        return
    }
  }, [mode])

  const singleValueOnSubmitValue = React.useCallback(
    (v: UnknownOrEmptyInput<CSSNumber>) => {
      if (isCSSNumber(v)) {
        setGlobalValue(v.value)
      }
      paddingTopOnSubmitValue(v)
      paddingRightOnSubmitValue(v)
      paddingBottomOnSubmitValue(v)
      paddingLeftOnSubmitValue(v)
    },
    [
      paddingTopOnSubmitValue,
      paddingRightOnSubmitValue,
      paddingBottomOnSubmitValue,
      paddingLeftOnSubmitValue,
    ],
  )
  const singleValueOnTransientSubmitValue = React.useCallback(
    (v: UnknownOrEmptyInput<CSSNumber>) => {
      if (isCSSNumber(v)) {
        setGlobalValue(v.value)
      }
      paddingTopOnTransientSubmitValue(v)
      paddingRightOnTransientSubmitValue(v)
      paddingBottomOnTransientSubmitValue(v)
      paddingLeftOnTransientSubmitValue(v)
    },
    [
      paddingTopOnTransientSubmitValue,
      paddingRightOnTransientSubmitValue,
      paddingBottomOnTransientSubmitValue,
      paddingLeftOnTransientSubmitValue,
    ],
  )

  const horizontalValueOnSubmitValue = React.useCallback(
    (v: UnknownOrEmptyInput<CSSNumber>) => {
      if (isCSSNumber(v)) {
        setHorizontalValue(v.value)
      }
      paddingRightOnSubmitValue(v)
      paddingLeftOnSubmitValue(v)
    },
    [paddingRightOnSubmitValue, paddingLeftOnSubmitValue],
  )
  const horizontalValueOnTransientSubmitValue = React.useCallback(
    (v: UnknownOrEmptyInput<CSSNumber>) => {
      if (isCSSNumber(v)) {
        setHorizontalValue(v.value)
      }
      paddingRightOnTransientSubmitValue(v)
      paddingLeftOnTransientSubmitValue(v)
    },
    [paddingRightOnTransientSubmitValue, paddingLeftOnTransientSubmitValue],
  )

  const verticalValueOnSubmitValue = React.useCallback(
    (v: UnknownOrEmptyInput<CSSNumber>) => {
      if (isCSSNumber(v)) {
        setVerticalValue(v.value)
      }
      paddingTopOnSubmitValue(v)
      paddingBottomOnSubmitValue(v)
    },
    [paddingTopOnSubmitValue, paddingBottomOnSubmitValue],
  )
  const verticalValueOnTransientSubmitValue = React.useCallback(
    (v: UnknownOrEmptyInput<CSSNumber>) => {
      if (isCSSNumber(v)) {
        setVerticalValue(v.value)
      }
      paddingTopOnTransientSubmitValue(v)
      paddingBottomOnTransientSubmitValue(v)
    },
    [paddingTopOnTransientSubmitValue, paddingBottomOnTransientSubmitValue],
  )

  return (
    <div style={{ display: 'flex', flexDirection: 'row', gap: 4, padding: 0, margin: 0 }}>
      {when(
        mode === 'global',
        <ChainedNumberInput
          idPrefix='padding'
          style={{ flex: 1 }}
          propsArray={[
            {
              DEPRECATED_labelBelow: '↔',
              value: globalValue != null ? { value: globalValue, unit: 'px' } : null,
              numberType: 'Px',
              defaultUnitToHide: 'px',
              testId: 'padding-single',
              onSubmitValue: singleValueOnSubmitValue,
              onTransientSubmitValue: singleValueOnTransientSubmitValue,
              style: { width: '100%' },
            },
          ]}
        />,
      )}
      {when(
        mode === 'axis',
        <ChainedNumberInput
          idPrefix='padding'
          style={{ flex: 1, gap: 4 }}
          propsArray={[
            {
              DEPRECATED_labelBelow: 'H',
              value: horizontalValue != null ? { value: horizontalValue, unit: 'px' } : null,
              numberType: 'Px',
              defaultUnitToHide: 'px',
              testId: 'padding-single',
              onSubmitValue: horizontalValueOnSubmitValue,
              onTransientSubmitValue: horizontalValueOnTransientSubmitValue,
              style: { width: '100%' },
            },
            {
              DEPRECATED_labelBelow: 'V',
              value: verticalValue != null ? { value: verticalValue, unit: 'px' } : null,
              numberType: 'Px',
              defaultUnitToHide: 'px',
              testId: 'padding-single',
              onSubmitValue: verticalValueOnSubmitValue,
              onTransientSubmitValue: verticalValueOnTransientSubmitValue,
            },
          ]}
        />,
      )}
      {when(
        mode === 'single',
        <ChainedNumberInput
          idPrefix='padding'
          style={{ flex: 1, gap: 4 }}
          propsArray={[
            {
              value: paddingTop.value,
              DEPRECATED_labelBelow: 'T',
              minimum: 0,
              onSubmitValue: paddingTopOnSubmitValue,
              onTransientSubmitValue: paddingTopOnTransientSubmitValue,
              controlStatus: paddingTop.controlStatus,
              numberType: 'LengthPercent',
              defaultUnitToHide: 'px',
              testId: 'padding-T',
            },
            {
              value: paddingRight.value,
              DEPRECATED_labelBelow: 'R',
              minimum: 0,
              onSubmitValue: paddingRightOnSubmitValue,
              onTransientSubmitValue: paddingRightOnTransientSubmitValue,
              controlStatus: paddingRight.controlStatus,
              numberType: 'LengthPercent',
              defaultUnitToHide: 'px',
              testId: 'padding-R',
            },
            {
              value: paddingBottom.value,
              DEPRECATED_labelBelow: 'B',
              minimum: 0,
              onSubmitValue: paddingBottomOnSubmitValue,
              onTransientSubmitValue: paddingBottomOnTransientSubmitValue,
              controlStatus: paddingBottom.controlStatus,
              numberType: 'LengthPercent',
              defaultUnitToHide: 'px',
              testId: 'padding-B',
            },
            {
              value: paddingLeft.value,
              DEPRECATED_labelBelow: 'L',
              minimum: 0,
              onSubmitValue: paddingLeftOnSubmitValue,
              onTransientSubmitValue: paddingLeftOnTransientSubmitValue,
              controlStatus: paddingLeft.controlStatus,
              numberType: 'LengthPercent',
              defaultUnitToHide: 'px',
              testId: 'padding-L',
            },
          ]}
        />,
      )}
      <SquareButton highlight onClick={cycleMode}>
        {when(mode === 'global', <Icons.Dot />)}
        {when(mode === 'single', <Icons.Dot />)}
        {when(mode === 'axis', <Icons.FourDots />)}
      </SquareButton>
    </div>
  )
})

function layoutSystemConfigPropertyPaths(
  propertyTarget: ReadonlyArray<string>,
): Array<PropertyPath> {
  return [
    stylePropPathMappingFn('alignContent', propertyTarget),
    stylePropPathMappingFn('alignItems', propertyTarget),
    stylePropPathMappingFn('display', propertyTarget),
    stylePropPathMappingFn('flexDirection', propertyTarget),
    stylePropPathMappingFn('flexWrap', propertyTarget),
    stylePropPathMappingFn('justifyContent', propertyTarget),
    stylePropPathMappingFn('marginBottom', propertyTarget),
    stylePropPathMappingFn('marginLeft', propertyTarget),
    stylePropPathMappingFn('marginRight', propertyTarget),
    stylePropPathMappingFn('marginTop', propertyTarget),
    stylePropPathMappingFn('margin', propertyTarget),
  ]
}

function useDeleteAllLayoutConfig() {
  const { onUnsetValue } = React.useContext(InspectorCallbackContext)
  const targetPath = useContextSelector(InspectorPropsContext, (contextData) => {
    return contextData.targetPath
  })
  return React.useCallback(() => {
    onUnsetValue(layoutSystemConfigPropertyPaths(targetPath), false)
  }, [onUnsetValue, targetPath])
}

export const DeleteAllLayoutSystemConfigButton = React.memo(() => {
  const onDeleteAllConfig = useDeleteAllLayoutConfig()

  return (
    <SquareButton highlight onClick={onDeleteAllConfig}>
      <FunctionIcons.Remove />
    </SquareButton>
  )
})
