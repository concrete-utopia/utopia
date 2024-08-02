import React from 'react'
import { isRight } from '../../../../../core/shared/either'
import {
  useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue,
  NumberInput,
  InspectorSubsectionHeader,
  FlexRow,
  SquareButton,
  Icn,
  CheckboxInput,
  Icons,
} from '../../../../../uuiui'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { addOnUnsetValues } from '../../../common/context-menu-items'
import type { CSSBorder, CSSColor, CSSNumber, EmptyInputValue } from '../../../common/css-utils'
import {
  cssKeyword,
  cssLineStyle,
  cssLineWidth,
  defaultBorderWidth,
  defaultCSSBorder,
  isCSSNumber,
  isCSSUnknownFunctionParameters,
  isEmptyInputValue,
  parseColor,
  toggleBorderEnabled,
} from '../../../common/css-utils'
import { RemovePropertyButton, useGetSubsectionHeaderStyle } from '../../../common/inspector-utils'
import { useInspectorStyleInfo, useIsSubSectionVisible } from '../../../common/property-path-hooks'
import { ColorControl, StringColorControl } from '../../../controls/color-control'
import { FakeUnknownArrayItem } from '../../../controls/unknown-array-item'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import { LabelBelowNumberTextStyles } from '../../../common/control-styles'

export function updateBorderWidth(
  newWidth: CSSNumber | EmptyInputValue,
  oldValue: CSSBorder,
): CSSBorder {
  if (isEmptyInputValue(newWidth)) {
    let newValue = {
      ...oldValue,
    }
    delete newValue.width
    return newValue
  } else {
    return {
      ...oldValue,
      style:
        (oldValue.style?.value.value ?? 'none') === 'none'
          ? cssLineStyle(cssKeyword('solid'))
          : oldValue.style,
      width: cssLineWidth(newWidth),
    }
  }
}

export function updateBorderColor(newColor: CSSColor, oldValue: CSSBorder): CSSBorder {
  return {
    ...oldValue,
    style:
      (oldValue.style?.value.value ?? 'none') === 'none'
        ? cssLineStyle(cssKeyword('solid'))
        : oldValue.style,
    color: newColor,
  }
}

export function updateBorderColorString(newValue: string, oldValue: CSSBorder): CSSBorder {
  const parsed = parseColor(newValue, 'hex-hash-optional')
  if (isRight(parsed)) {
    return updateBorderColor(parsed.value, oldValue)
  } else {
    return oldValue
  }
}

function insertBorder(_: null, oldValue: CSSBorder): CSSBorder {
  return { ...defaultCSSBorder }
}

export const BorderSubsection: React.FunctionComponent<React.PropsWithChildren<unknown>> =
  React.memo(() => {
    const isVisible = useIsSubSectionVisible('border')

    const {
      value,
      controlStatus,
      controlStyles,
      propertyStatus,
      onUnsetValues,
      useSubmitValueFactory,
    } = useInspectorStyleInfo('border')

    const headerStyle = useGetSubsectionHeaderStyle(controlStatus)

    const borderEnabled = (value.style?.value.value ?? 'none') !== 'none'
    const borderColor: CSSColor = value.color ?? defaultCSSBorder.color
    const borderWidth: CSSNumber = (() => {
      if (value.width == null) {
        return { ...defaultBorderWidth }
      } else if (isCSSNumber(value.width.value)) {
        return value.width.value
      } else {
        // TODO: CSSKeyword support in number controls
        return { ...defaultBorderWidth }
      }
    })()

    const [borderEnabledSubmitValue] = useSubmitValueFactory(toggleBorderEnabled)
    const onCheckboxChange = React.useCallback(() => {
      borderEnabledSubmitValue(null)
    }, [borderEnabledSubmitValue])

    const [onInsertBorderSubmitValue] = useSubmitValueFactory(insertBorder)
    const onInsertMouseDown = React.useCallback(() => {
      onInsertBorderSubmitValue(null)
    }, [onInsertBorderSubmitValue])

    const [borderColorSubmitValue, borderColorTransientSubmitValue] =
      useSubmitValueFactory(updateBorderColor)
    const [borderColorStringSubmitValue] = useSubmitValueFactory(updateBorderColorString)
    const [borderWidthSubmitValue, borderWidthTransientSubmitValue] =
      useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue(useSubmitValueFactory(updateBorderWidth))

    const allOrSplitControls = (
      <UIGridRow tall alignItems='start' padded={false} variant='<-------1fr------>|----80px----|'>
        <StringColorControl
          id='border-color'
          key='border-color'
          testId='border-color'
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
          testId='border-width'
          value={borderWidth}
          DEPRECATED_labelBelow='W'
          labelBelowStyle={LabelBelowNumberTextStyles}
          minimum={0}
          onSubmitValue={borderWidthSubmitValue}
          onTransientSubmitValue={borderWidthTransientSubmitValue}
          controlStatus={controlStatus}
          numberType='Length'
          defaultUnitToHide={'px'}
          incrementControls={false}
        />
      </UIGridRow>
    )

    const borderSet: boolean = controlStatus !== 'unset' && controlStatus !== 'trivial-default'

    const contextMenuItems = [addOnUnsetValues(['border parameters'], onUnsetValues)]

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
              gap: 8,
            }}
          >
            <span>Border</span>
          </FlexRow>
          {propertyStatus.overwritable ? (
            <FlexRow style={{ gap: 4 }}>
              <RemovePropertyButton
                testId='inspector-border-remove-all'
                onUnsetValues={onUnsetValues}
                propertySet={propertyStatus.set}
              />
              <SquareButton
                highlightOnHover
                onMouseDown={onInsertMouseDown}
                disabled={borderSet}
                style={{ width: 12 }}
              >
                <Icn category='semantic' type='plus' width={12} height={12} />
              </SquareButton>
            </FlexRow>
          ) : null}
        </InspectorSubsectionHeader>

        {borderSet ? (
          isCSSUnknownFunctionParameters(value) ? (
            <FakeUnknownArrayItem controlStatus={controlStatus} />
          ) : (
            <UIGridRow
              tall
              alignItems='start'
              padded={true}
              variant='<---1fr--->|------172px-------|'
            >
              <UIGridRow
                tall
                alignItems='start'
                padded={false}
                variant='<-auto-><----------1fr--------->'
              >
                <CheckboxInput
                  id={`shadow-enable-disable`}
                  key={`shadow-enable-disable`}
                  checked={borderEnabled}
                  onChange={onCheckboxChange}
                  controlStatus={controlStatus}
                />
                <ColorControl
                  id='border-color'
                  key='border-color'
                  testId='border-color'
                  value={borderColor}
                  onSubmitValue={borderColorSubmitValue}
                  onTransientSubmitValue={borderColorTransientSubmitValue}
                  onSubmitSolidStringValue={borderColorStringSubmitValue}
                  pickerOffset={{ x: -45, y: 0 }}
                  controlStatus={controlStatus}
                  controlStyles={controlStyles}
                />
              </UIGridRow>
              {allOrSplitControls}
            </UIGridRow>
          )
        ) : null}
      </InspectorContextMenuWrapper>
    )
  })
BorderSubsection.displayName = 'BorderSubsection'
