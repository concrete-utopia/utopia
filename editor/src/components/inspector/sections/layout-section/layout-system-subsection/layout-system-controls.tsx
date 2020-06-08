import * as React from 'react'

import * as PP from '../../../../../core/shared/property-path'
import { betterReactMemo, OnSubmitValue } from 'uuiui-deps'
import { SegmentControl, SegmentOption } from '../../../controls/segment-control'
import {
  useInspectorLayoutInfo,
  InspectorCallbackContext,
  useInspectorInfo,
  useInspectorStyleInfo,
  InspectorInfo,
} from '../../../common/property-path-hooks'
import { useEditorState } from '../../../../editor/store/store-hook'
import { switchLayoutSystem } from '../../../../editor/actions/actions'
import {
  getControlStatusFromPropertyStatus,
  getControlStyles,
  ControlStatus,
  ControlStyles,
} from '../../../common/control-status'
import { LayoutSystem } from 'utopia-api'
import {
  ChainedNumberInput,
  useWrappedEmptyOnSubmitValue,
  SquareButton,
  FunctionIcons,
} from 'uuiui'
import { createLayoutPropertyPath } from '../../../../../core/layout/layout-helpers-new'
import {
  DetectedLayoutSystem,
  SettableLayoutSystem,
} from '../../../../../core/shared/element-template'

function useDefaultedLayoutSystemInfo(): {
  value: LayoutSystem | 'flow'
  controlStatus: ControlStatus
  controlStyles: ControlStyles
} {
  const layoutSystemMetadata = useInspectorLayoutInfo('LayoutSystem')
  const styleDisplayMetadata = useInspectorStyleInfo('display')

  let metadataToUse: InspectorInfo<any> = layoutSystemMetadata
  if (styleDisplayMetadata.value === 'flex') {
    metadataToUse = styleDisplayMetadata
  }

  if (metadataToUse.value == null) {
    const updatedPropertyStatus = {
      ...layoutSystemMetadata.propertyStatus,
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

function useLayoutSystemData() {
  const dispatch = useEditorState((store) => store.dispatch)

  const onLayoutSystemChange = React.useCallback(
    (layoutSystem: SettableLayoutSystem) => {
      switch (layoutSystem) {
        case LayoutSystem.PinSystem:
        case 'flow':
        case 'flex':
          dispatch([switchLayoutSystem(layoutSystem)], 'everyone')
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
    [dispatch],
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
}

// TODO this is not how this control should be used:
export const LayoutSystemControl = betterReactMemo(
  'LayoutSystemControl',
  (props: LayoutSystemControlProps) => {
    const layoutSystemData = useLayoutSystemData()
    return (
      <SegmentControl<LayoutSystem | DetectedLayoutSystem>
        id={'layoutSystem'}
        key={'layoutSystem'}
        onSubmitValue={
          layoutSystemData.onSubmitValue as OnSubmitValue<LayoutSystem | DetectedLayoutSystem>
        }
        value={props.layoutSystem ?? layoutSystemData.value}
        options={layoutSystemOptions}
        controlStatus={layoutSystemData.controlStatus}
        controlStyles={layoutSystemData.controlStyles}
      />
    )
  },
)

// for now, 'flow', 'grid' and 'group' are not clickable buttons, they only have an indicative role
const layoutSystemOptions: Array<SegmentOption<LayoutSystem | DetectedLayoutSystem>> = [
  {
    value: LayoutSystem.PinSystem,
    tooltip: 'Layout children with Pins',
    label: 'Pins',
  },
  {
    value: 'flow',
    tooltip: 'Default CSS Normal Flow Layout',
    label: 'Flow',
  },
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
  {
    value: 'nonfixed',
    tooltip: 'Nonfixed',
    label: 'Non-fixed',
  },
  {
    value: 'none',
    tooltip: 'None',
    label: 'None',
  },
]

export const paddingPropsToUnset = [
  createLayoutPropertyPath('paddingLeft'),
  createLayoutPropertyPath('paddingTop'),
  createLayoutPropertyPath('paddingRight'),
  createLayoutPropertyPath('paddingBottom'),
]

export const FlexPaddingControl = betterReactMemo('FlexPaddingControl', () => {
  const flexPaddingTop = useInspectorLayoutInfo('paddingTop')
  const flexPaddingRight = useInspectorLayoutInfo('paddingRight')
  const flexPaddingBottom = useInspectorLayoutInfo('paddingBottom')
  const flexPaddingLeft = useInspectorLayoutInfo('paddingLeft')

  const flexPaddingTopOnSubmitValue = useWrappedEmptyOnSubmitValue(
    flexPaddingTop.onSubmitValue,
    flexPaddingTop.onUnsetValues,
  )
  const flexPaddingTopOnTransientSubmitValue = useWrappedEmptyOnSubmitValue(
    flexPaddingTop.onTransientSubmitValue,
    flexPaddingTop.onUnsetValues,
  )
  const flexPaddingRightOnSubmitValue = useWrappedEmptyOnSubmitValue(
    flexPaddingRight.onSubmitValue,
    flexPaddingRight.onUnsetValues,
  )
  const flexPaddingRightOnTransientSubmitValue = useWrappedEmptyOnSubmitValue(
    flexPaddingRight.onTransientSubmitValue,
    flexPaddingRight.onUnsetValues,
  )
  const flexPaddingBottomOnSubmitValue = useWrappedEmptyOnSubmitValue(
    flexPaddingBottom.onSubmitValue,
    flexPaddingBottom.onUnsetValues,
  )
  const flexPaddingBottomOnTransientSubmitValue = useWrappedEmptyOnSubmitValue(
    flexPaddingBottom.onTransientSubmitValue,
    flexPaddingBottom.onUnsetValues,
  )
  const flexPaddingLeftOnSubmitValue = useWrappedEmptyOnSubmitValue(
    flexPaddingLeft.onSubmitValue,
    flexPaddingLeft.onUnsetValues,
  )
  const flexPaddingLeftOnTransientSubmitValue = useWrappedEmptyOnSubmitValue(
    flexPaddingLeft.onTransientSubmitValue,
    flexPaddingLeft.onUnsetValues,
  )

  return (
    <ChainedNumberInput
      idPrefix='flexPadding'
      propsArray={[
        {
          value: flexPaddingTop.value,
          labelBelow: 'T',
          minimum: 0,
          onSubmitValue: flexPaddingTopOnSubmitValue,
          onTransientSubmitValue: flexPaddingTopOnTransientSubmitValue,
          controlStatus: flexPaddingTop.controlStatus,
          disabled: !flexPaddingTop.controlStyles.interactive,
          numberType: 'UnitlessPercent',
        },
        {
          value: flexPaddingRight.value,
          labelBelow: 'R',
          minimum: 0,
          onSubmitValue: flexPaddingRightOnSubmitValue,
          onTransientSubmitValue: flexPaddingRightOnTransientSubmitValue,
          controlStatus: flexPaddingRight.controlStatus,
          disabled: !flexPaddingRight.controlStyles.interactive,
          numberType: 'UnitlessPercent',
        },
        {
          value: flexPaddingBottom.value,
          labelBelow: 'B',
          minimum: 0,
          onSubmitValue: flexPaddingBottomOnSubmitValue,
          onTransientSubmitValue: flexPaddingBottomOnTransientSubmitValue,
          controlStatus: flexPaddingBottom.controlStatus,
          disabled: !flexPaddingBottom.controlStyles.interactive,
          numberType: 'UnitlessPercent',
        },
        {
          value: flexPaddingLeft.value,
          labelBelow: 'L',
          minimum: 0,
          onSubmitValue: flexPaddingLeftOnSubmitValue,
          onTransientSubmitValue: flexPaddingLeftOnTransientSubmitValue,
          controlStatus: flexPaddingLeft.controlStatus,
          disabled: !flexPaddingLeft.controlStyles.interactive,
          numberType: 'UnitlessPercent',
        },
      ]}
    />
  )
})

const layoutSystemConfigPropertyPaths = [
  createLayoutPropertyPath('LayoutSystem'),
  PP.create(['style', 'display']),
  createLayoutPropertyPath('flexDirection'),
  createLayoutPropertyPath('FlexGapMain'),
  createLayoutPropertyPath('flexWrap'),
  createLayoutPropertyPath('justifyContent'),
  createLayoutPropertyPath('alignItems'),
  createLayoutPropertyPath('alignContent'),
  createLayoutPropertyPath('paddingLeft'),
  createLayoutPropertyPath('paddingTop'),
  createLayoutPropertyPath('paddingRight'),
  createLayoutPropertyPath('paddingBottom'),
]

function useDeleteAllLayoutConfig() {
  const { onUnsetValue } = React.useContext(InspectorCallbackContext)
  return React.useCallback(() => {
    onUnsetValue(layoutSystemConfigPropertyPaths)
  }, [onUnsetValue])
}

export const DeleteAllLayoutSystemConfigButton = betterReactMemo(
  'DeleteAllLayoutSystemConfigButton',
  () => {
    const onDeleteAllConfig = useDeleteAllLayoutConfig()

    return (
      <SquareButton highlight onClick={onDeleteAllConfig}>
        <FunctionIcons.Remove />
      </SquareButton>
    )
  },
)
