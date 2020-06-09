import * as React from 'react'
import {
  ChainedNumberInput,
  CheckboxInput,
  FlexRow,
  Icn,
  InspectorSubsectionHeader,
  NumberInput,
  SquareButton,
} from 'uuiui'
import { betterReactMemo } from 'uuiui-deps'
import { isRight } from '../../../../../core/shared/either'
import utils from '../../../../../utils/utils'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { ColorControl, StringColorControl } from '../../../controls/color-control'
import { OptionControl } from '../../../controls/option-control'
import { FakeUnknownArrayItem } from '../../../controls/unknown-array-item'
import { addOnUnsetValues } from '../../../common/context-menu-items'
import {
  CSSBorderWidthSplit,
  CSSBoxShadowAndBorderOrUnknown,
  CSSColor,
  CSSNumber,
  cssNumber,
  defaultBorder,
  defaultBorderWidth,
  defaultBorderWidthSplit,
  EmptyInputValue,
  fallbackOnEmptyInputValueToCSSEmptyValue,
  isCSSUnknownFunctionParameters,
  isEmptyInputValue,
  parseColor,
} from '../../../common/css-utils'
import { useGetSubsectionHeaderStyle } from '../../../common/inspector-utils'
import { useInspectorStyleInfo, useIsSubSectionVisible } from '../../../common/property-path-hooks'
import { getControlStyles } from '../../../common/control-status'
import { GridRow } from '../../../widgets/grid-row'

export function updateBorderEnabled(
  enabled: boolean,
  oldValue: CSSBoxShadowAndBorderOrUnknown,
): CSSBoxShadowAndBorderOrUnknown {
  if (isCSSUnknownFunctionParameters(oldValue)) {
    return oldValue
  } else {
    let newBoxShadowAndBorder = { ...oldValue }
    if (newBoxShadowAndBorder.border != null) {
      newBoxShadowAndBorder.border.enabled = enabled
    } else {
      newBoxShadowAndBorder = {
        ...oldValue,
        border: defaultBorder,
      }
    }
    return newBoxShadowAndBorder
  }
}

function updateBorderColor(
  newColor: CSSColor,
  oldValue: CSSBoxShadowAndBorderOrUnknown,
): CSSBoxShadowAndBorderOrUnknown {
  if (isCSSUnknownFunctionParameters(oldValue)) {
    return oldValue
  } else {
    const enabled = oldValue.border != null ? oldValue.border.enabled : true
    return {
      ...oldValue,
      border: {
        ...oldValue.border,
        enabled,
        borderColor: newColor,
      },
    }
  }
}

function updateBorderWidth(
  newWidth: CSSNumber | EmptyInputValue,
  oldValue: CSSBoxShadowAndBorderOrUnknown,
): CSSBoxShadowAndBorderOrUnknown {
  if (isCSSUnknownFunctionParameters(oldValue)) {
    return oldValue
  }
  if (isEmptyInputValue(newWidth)) {
    let newValue = {
      ...oldValue,
    }
    if (newValue.border != null) {
      delete newValue.border.borderWidth
    }
    return newValue
  } else {
    const enabled = oldValue.border != null ? oldValue.border.enabled : true
    return {
      ...oldValue,
      border: {
        ...oldValue.border,
        enabled,
        borderWidth: newWidth,
      },
    }
  }
}

function toggleBorderSplit(
  _: any,
  oldValue: CSSBoxShadowAndBorderOrUnknown,
): CSSBoxShadowAndBorderOrUnknown {
  if (isCSSUnknownFunctionParameters(oldValue)) {
    return oldValue
  } else {
    if (oldValue.border != null) {
      if (Array.isArray(oldValue.border.borderWidth)) {
        const enabled = oldValue.border.enabled
        return {
          ...oldValue,
          border: {
            enabled,
            borderColor: oldValue.border.borderColor,
            borderWidth: oldValue.border.borderWidth[0],
          },
        }
      } else if (oldValue.border.borderWidth != null) {
        const borderWidth = new Array<CSSNumber>(4).fill(
          oldValue.border.borderWidth,
        ) as CSSBorderWidthSplit
        const enabled = oldValue.border != null ? oldValue.border.enabled : true
        return {
          ...oldValue,
          border: {
            enabled,
            borderColor: oldValue.border.borderColor,
            borderWidth,
          },
        }
      }
    }
  }
  return oldValue
}

export function updateBorderColorString(
  newValue: string,
  oldValue: CSSBoxShadowAndBorderOrUnknown,
): CSSBoxShadowAndBorderOrUnknown {
  if (isCSSUnknownFunctionParameters(oldValue)) {
    return oldValue
  } else {
    const parsed = parseColor(newValue)
    if (isRight(parsed)) {
      const enabled = oldValue.border != null ? oldValue.border.enabled : true
      return {
        ...oldValue,
        border: {
          ...oldValue.border,
          enabled,
          borderColor: parsed.value,
        },
      }
    } else {
      return oldValue
    }
  }
}

function insertBorder(
  oldValue: CSSBoxShadowAndBorderOrUnknown,
  onBoxShadowAndBorderSubmitValue: (newValue: CSSBoxShadowAndBorderOrUnknown) => void,
): void {
  if (!isCSSUnknownFunctionParameters(oldValue)) {
    onBoxShadowAndBorderSubmitValue({
      ...oldValue,
      border: { ...defaultBorder },
    })
  }
}

export function unsetOnlyBorder(
  _: any,
  oldValue: CSSBoxShadowAndBorderOrUnknown,
): CSSBoxShadowAndBorderOrUnknown {
  if (isCSSUnknownFunctionParameters(oldValue)) {
    return oldValue
  } else {
    let newValue = { ...oldValue }
    delete newValue['border']
    return newValue
  }
}

function generateUpdateBorderEdgeWidth(
  edge: 0 | 1 | 2 | 3,
): (
  newWidth: CSSNumber | EmptyInputValue,
  oldValue: CSSBoxShadowAndBorderOrUnknown,
) => CSSBoxShadowAndBorderOrUnknown {
  return function updateBorderEdgeWidth(
    newWidth: CSSNumber | EmptyInputValue,
    oldValue: CSSBoxShadowAndBorderOrUnknown,
  ): CSSBoxShadowAndBorderOrUnknown {
    if (isCSSUnknownFunctionParameters(oldValue)) {
      return oldValue
    } else {
      if (oldValue.border != null) {
        if (Array.isArray(oldValue.border.borderWidth)) {
          let newWidths = [...oldValue.border.borderWidth] as CSSBorderWidthSplit
          newWidths[edge] = fallbackOnEmptyInputValueToCSSEmptyValue(cssNumber(0), newWidth)
          return {
            ...oldValue,
            border: {
              ...oldValue.border,
              borderWidth: newWidths,
            },
          }
        } else {
          let newWidths = [...defaultBorderWidthSplit] as CSSBorderWidthSplit
          newWidths[edge] = fallbackOnEmptyInputValueToCSSEmptyValue(cssNumber(0), newWidth)
          return {
            ...oldValue,
            border: {
              ...oldValue.border,
              borderWidth: newWidths,
            },
          }
        }
      }
      return oldValue
    }
  }
}

export const BorderSubsection: React.FunctionComponent = betterReactMemo('BorderSubsection', () => {
  const isVisible = useIsSubSectionVisible('border')

  const {
    value,
    controlStatus,
    controlStyles,
    propertyStatus,
    onSubmitValue,
    onUnsetValues,
    useSubmitValueFactory,
  } = useInspectorStyleInfo('boxShadow')
  const borderWidth =
    !isCSSUnknownFunctionParameters(value) &&
    value.border != null &&
    value.border.borderWidth != null
      ? value.border.borderWidth
      : defaultBorderWidth

  const headerStyle = useGetSubsectionHeaderStyle(controlStatus)

  const borderColor: CSSColor =
    !isCSSUnknownFunctionParameters(value) &&
    value.border != null &&
    value.border.borderColor != null
      ? value.border.borderColor
      : defaultBorder.borderColor

  const [borderEnabledSubmitValue] = useSubmitValueFactory(updateBorderEnabled)
  const [borderColorSubmitValue, borderColorTransientSubmitValue] = useSubmitValueFactory(
    updateBorderColor,
  )
  const [borderColorStringSubmitValue] = useSubmitValueFactory(updateBorderColorString)
  const [borderWidthSubmitValue, borderWidthTransientSubmitValue] = useSubmitValueFactory(
    updateBorderWidth,
  )
  const [borderTopWidthSubmitValue, borderTopWidthSubmitTransientValue] = useSubmitValueFactory(
    generateUpdateBorderEdgeWidth(0),
  )
  const [borderRightWidthSubmitValue, borderRightWidthSubmitTransientValue] = useSubmitValueFactory(
    generateUpdateBorderEdgeWidth(1),
  )
  const [
    borderBottomWidthSubmitValue,
    borderBottomWidthSubmitTransientValue,
  ] = useSubmitValueFactory(generateUpdateBorderEdgeWidth(2))
  const [borderLeftWidthSubmitValue, borderLeftWidthSubmitTransientValue] = useSubmitValueFactory(
    generateUpdateBorderEdgeWidth(3),
  )
  const [borderToggleSplitSubmitValue] = useSubmitValueFactory(toggleBorderSplit)
  const [onUnsetOnlyBorderSubmitValue] = useSubmitValueFactory(unsetOnlyBorder)

  const unsetBorder = React.useCallback(() => {
    if (isCSSUnknownFunctionParameters(value) || value.boxShadows == null) {
      onUnsetValues()
    } else {
      onUnsetOnlyBorderSubmitValue(null)
    }
  }, [value, onUnsetOnlyBorderSubmitValue, onUnsetValues])

  const borderEnabled =
    !isCSSUnknownFunctionParameters(value) && value.border != null && value.border.enabled

  const onCheckBoxChange = React.useCallback(() => {
    borderEnabledSubmitValue(!borderEnabled)
  }, [borderEnabledSubmitValue, borderEnabled])

  const allOrSplitControls = Array.isArray(borderWidth) ? (
    <GridRow tall alignItems='start' padded={false} type='<--------auto-------->||22px|'>
      <ChainedNumberInput
        idPrefix='borderWidth'
        propsArray={[
          {
            value: borderWidth[0],
            labelInner: 'T',
            minimum: 0,
            onSubmitValue: borderTopWidthSubmitValue,
            onTransientSubmitValue: borderTopWidthSubmitTransientValue,
            controlStatus: controlStatus,
            numberType: 'Length',
            defaultUnitToHide: 'px',
          },
          {
            value: borderWidth[1],
            labelInner: 'R',
            minimum: 0,
            onSubmitValue: borderRightWidthSubmitValue,
            onTransientSubmitValue: borderRightWidthSubmitTransientValue,
            controlStatus: controlStatus,
            numberType: 'Length',
            defaultUnitToHide: 'px',
          },
          {
            value: borderWidth[2],
            labelInner: 'B',
            minimum: 0,
            onSubmitValue: borderBottomWidthSubmitValue,
            onTransientSubmitValue: borderBottomWidthSubmitTransientValue,
            controlStatus: controlStatus,
            numberType: 'Length',
            defaultUnitToHide: 'px',
          },
          {
            value: borderWidth[3],
            labelInner: 'L',
            minimum: 0,
            onSubmitValue: borderLeftWidthSubmitValue,
            onTransientSubmitValue: borderLeftWidthSubmitTransientValue,
            controlStatus: controlStatus,
            numberType: 'Length',
            defaultUnitToHide: 'px',
          },
        ]}
      />
      <OptionControl
        id='independent-border'
        key='independent-border'
        value={true}
        onSubmitValue={borderToggleSplitSubmitValue}
        controlStatus='simple'
        controlStyles={getControlStyles('simple')}
        style={{
          width: 22,
        }}
        controlOptions={{
          tooltip: 'Independent Border',
          icon: {
            category: 'borders',
            type: 'notTheSame',
            color: 'darkgray',
            width: 16,
            height: 16,
          },
        }}
      />
    </GridRow>
  ) : (
    <GridRow tall alignItems='start' padded={false} type='<--------auto-------->|54px||22px|'>
      <StringColorControl
        id='border-color'
        key='border-color'
        value={borderColor}
        onSubmitValue={borderColorSubmitValue}
        onTransientSubmitValue={borderColorTransientSubmitValue}
        onSubmitSolidStringValue={borderColorStringSubmitValue}
        pickerOffset={{ x: -45, y: 0 }}
        controlStatus={controlStatus}
        controlStyles={controlStyles}
      />
      <NumberInput
        id='border-width'
        value={borderWidth}
        labelBelow='width'
        minimum={0}
        onSubmitValue={borderWidthSubmitValue}
        onTransientSubmitValue={borderWidthTransientSubmitValue}
        controlStatus={controlStatus}
        numberType='Length'
      />
      <OptionControl
        id='independent-border'
        key='independent-border'
        value={true}
        onSubmitValue={borderToggleSplitSubmitValue}
        controlStatus='simple'
        controlStyles={getControlStyles('simple')}
        style={{
          width: 22,
        }}
        controlOptions={{
          tooltip: 'Independent Border',
          icon: {
            category: 'borders',
            type: 'same',
            color: 'darkgray',
            width: 16,
            height: 16,
          },
        }}
      />
    </GridRow>
  )

  const showBorder: boolean =
    isCSSUnknownFunctionParameters(value) ||
    (value.border != null && (value.border.borderWidth != null || value.border.borderColor != null))

  const onUnsetBoxShadowParameters = () => {
    let newValue = { ...value }
    if ('border' in newValue && 'boxShadows' in newValue) {
      delete newValue['border']
      onSubmitValue(newValue)
    } else {
      onUnsetValues()
    }
  }

  const contextMenuItems = utils.stripNulls([
    'border' in value ? addOnUnsetValues(['border parameters'], onUnsetBoxShadowParameters) : null,
  ])

  const addBorder = React.useCallback(() => insertBorder(value, onSubmitValue), [
    value,
    onSubmitValue,
  ])

  if (!isVisible) {
    return null
  }
  return (
    <InspectorContextMenuWrapper
      id='border-subsection-context-menu'
      items={contextMenuItems}
      data={null}
    >
      <InspectorSubsectionHeader style={headerStyle}>
        <FlexRow
          style={{
            flexGrow: 1,
          }}
        >
          Border
        </FlexRow>
        {propertyStatus.overwritable ? (
          <SquareButton
            highlight
            onMouseDown={addBorder}
            disabled={
              isCSSUnknownFunctionParameters(value) ||
              value.border?.borderColor != null ||
              value.border?.borderWidth != null
            }
          >
            <Icn
              style={{ paddingTop: 1 }}
              category='semantic'
              type='plus'
              color={propertyStatus.controlled ? 'blue' : 'darkgray'}
              width={16}
              height={16}
            />
          </SquareButton>
        ) : null}
      </InspectorSubsectionHeader>

      {showBorder ? (
        isCSSUnknownFunctionParameters(value) ? (
          <FakeUnknownArrayItem controlStatus={controlStatus} />
        ) : (
          <GridRow tall alignItems='start' padded={true} type='<---1fr--->|------172px-------|'>
            <GridRow tall alignItems='start' padded={false} type='<-auto-><----------1fr--------->'>
              <CheckboxInput
                id={`shadow-enable-disable`}
                key={`shadow-enable-disable`}
                checked={borderEnabled}
                onChange={onCheckBoxChange}
                controlStatus={controlStatus}
              />
              <ColorControl
                id='border-color'
                key='border-color'
                value={borderColor}
                onSubmitValue={borderColorSubmitValue}
                onTransientSubmitValue={borderColorTransientSubmitValue}
                onSubmitSolidStringValue={borderColorStringSubmitValue}
                pickerOffset={{ x: -45, y: 0 }}
                controlStatus={controlStatus}
                controlStyles={controlStyles}
              />
            </GridRow>
            {allOrSplitControls}
          </GridRow>
        )
      ) : null}
    </InspectorContextMenuWrapper>
  )
})
BorderSubsection.displayName = 'BorderSubsection'
