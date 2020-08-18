import * as React from 'react'
import { OptionsType } from 'react-select'
import { betterReactMemo } from 'uuiui-deps'
import { googleFontsList } from '../../../../../../assets/google-fonts-list'
import { identity } from '../../../../../core/shared/utils'
import utils from '../../../../../utils/utils'
import { PopupList, Tooltip } from '../../../../../uuiui'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import {
  defaultWebFontWeightsAndStyles,
  prettyNameForFontVariant,
  WebFontVariant,
} from '../../../../navigator/external-resources/google-fonts-utils'
import { addOnUnsetValues } from '../../../common/context-menu-items'
import {
  ParsedValues,
  stylePropPathMappingFn,
  useInspectorInfo,
  useInspectorStyleInfo,
} from '../../../common/property-path-hooks'

const weightAndStylePaths: Array<'fontWeight' | 'fontStyle'> = ['fontWeight', 'fontStyle']

function updateFontWeightAndStyle(
  newValue: FontWeightAndStyleSelectOption,
): ParsedValues<'fontWeight' | 'fontStyle'> {
  return {
    fontWeight: newValue.value.webFontWeight,
    fontStyle: newValue.value.webFontStyle,
  }
}

interface FontWeightAndStyleSelectOption {
  value: WebFontVariant
}

export const FontVariantSelect = betterReactMemo('FontVariantSelect', () => {
  const { value, controlStyles, onUnsetValues, useSubmitValueFactory } = useInspectorInfo(
    weightAndStylePaths,
    identity,
    identity,
    stylePropPathMappingFn,
  )

  const { value: fontFamilyValue } = useInspectorStyleInfo('fontFamily')
  const primaryFont = fontFamilyValue[0]

  const fontWeightAndStyleContextMenuItems = utils.stripNulls([
    controlStyles.unsettable ? addOnUnsetValues(['fontWeight', 'fontStyle'], onUnsetValues) : null,
  ])
  const fontWeightAndStyleOptions: OptionsType<FontWeightAndStyleSelectOption> = React.useMemo(() => {
    const variantsToMap =
      googleFontsList.find((font) => font.name === primaryFont)?.variants ??
      defaultWebFontWeightsAndStyles
    return variantsToMap.map((variant) => {
      return {
        value: variant,
        label: prettyNameForFontVariant(variant),
        style: { fontWeight: variant.webFontWeight, fontStyle: variant.webFontStyle },
      }
    })
  }, [primaryFont])

  const [onSubmitValue] = useSubmitValueFactory(updateFontWeightAndStyle)

  const selectValue = fontWeightAndStyleOptions.find(
    (v) => v.value.webFontStyle === value.fontStyle && v.value.webFontWeight === value.fontWeight,
  )

  return (
    <InspectorContextMenuWrapper
      id='fontWeightAndStyle-context-menu'
      items={fontWeightAndStyleContextMenuItems}
      data={null}
      style={{ gridColumn: '1' }}
    >
      <Tooltip title='Font Weight and Style' placement='top'>
        <PopupList
          id={'fontWeightAndStyle'}
          onSubmitValue={onSubmitValue}
          value={selectValue}
          options={fontWeightAndStyleOptions}
          controlStyles={controlStyles}
        />
      </Tooltip>
    </InspectorContextMenuWrapper>
  )
})
