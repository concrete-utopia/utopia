import React from 'react'

import * as PP from '../../../../../core/shared/property-path'
import { OptionChainControl } from '../../../controls/option-chain-control'
import {
  useInspectorLayoutInfo,
  InspectorCallbackContext,
  useInspectorInfo,
  useInspectorStyleInfo,
  InspectorInfo,
  stylePropPathMappingFn,
  InspectorPropsContext,
} from '../../../common/property-path-hooks'
import { useEditorState } from '../../../../editor/store/store-hook'
import { switchLayoutSystem } from '../../../../editor/actions/action-creators'
import {
  getControlStatusFromPropertyStatus,
  getControlStyles,
  ControlStatus,
  ControlStyles,
} from '../../../common/control-status'
import { LayoutSystem } from 'utopia-api'
import { StyleLayoutProp } from '../../../../../core/layout/layout-helpers-new'
import {
  DetectedLayoutSystem,
  SettableLayoutSystem,
  SpecialSizeMeasurements,
} from '../../../../../core/shared/element-template'
import {
  useWrappedEmptyOrUnknownOnSubmitValue,
  ChainedNumberInput,
  SquareButton,
  FunctionIcons,
} from '../../../../../uuiui'
import { useInspectorInfoLonghandShorthand } from '../../../common/longhand-shorthand-hooks'
import { PropertyPath } from '../../../../../core/shared/project-file-types'
import { useContextSelector } from 'use-context-selector'

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
  const dispatch = useEditorState((store) => store.dispatch, 'useLayoutSystemData dispatch')
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

export const PaddingControl = React.memo(() => {
  const {
    paddingTop,
    paddingRight,
    paddingBottom,
    paddingLeft,
  } = useInspectorInfoLonghandShorthand(
    ['paddingTop', 'paddingRight', 'paddingBottom', 'paddingLeft'],
    'padding',
    stylePropPathMappingFn,
  )

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

  return (
    <ChainedNumberInput
      idPrefix='padding'
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
    />
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
