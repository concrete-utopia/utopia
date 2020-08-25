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
  WebFontFamilyVariant,
  webFontFamilyVariant,
  webFontVariant,
} from '../../../../navigator/external-resources/google-fonts-utils'
import { addOnUnsetValues } from '../../../common/context-menu-items'
import {
  ParsedValues,
  stylePropPathMappingFn,
  useInspectorInfo,
  useInspectorStyleInfo,
} from '../../../common/property-path-hooks'
import {
  useExternalResources,
  ExternalResources,
} from '../../../../../printer-parsers/html/external-resources-parser'

const weightAndStylePaths: Array<'fontWeight' | 'fontStyle'> = ['fontWeight', 'fontStyle']

function updateFontWeightAndStyle(
  newValue: FontWeightAndStyleSelectOption,
): ParsedValues<'fontWeight' | 'fontStyle'> {
  return {
    fontWeight: newValue.value.fontVariant.webFontWeight,
    fontStyle: newValue.value.fontVariant.webFontStyle,
  }
}

function updateAddNewFontVariant(
  newValue: FontWeightAndStyleSelectOption,
  oldValue: ExternalResources,
): ExternalResources {
  const newVariant = newValue.value.fontVariant
  let workingGoogleFontsResources = [...oldValue.googleFontsResources]
  const workingFontFamilyIndex = workingGoogleFontsResources.findIndex(
    (v) => v.fontFamily === newValue.value.familyName,
  )

  const googleFontsListItemIndex = googleFontsList.findIndex(
    (v) => v.name === newValue.value.familyName,
  )
  if (googleFontsListItemIndex > -1) {
    const googleFontsListItem = googleFontsList[googleFontsListItemIndex]
    if (workingFontFamilyIndex > -1) {
      const workingFontFamily = workingGoogleFontsResources[workingFontFamilyIndex]
      const googleFontVariantExists =
        googleFontsListItem.variants.some(
          (v) =>
            v.webFontStyle === newVariant.webFontStyle &&
            v.webFontWeight === newVariant.webFontWeight,
        ) != null
      if (googleFontVariantExists) {
        const variantIsAlreadyAdded = workingFontFamily.variants.findIndex(
          (v) =>
            v.webFontStyle === newVariant.webFontStyle &&
            v.webFontWeight === newVariant.webFontWeight,
        )
        if (!variantIsAlreadyAdded) {
          const workingVariants = [...workingFontFamily.variants]
          workingVariants.push(newVariant)
          workingFontFamily.variants = workingVariants
          workingGoogleFontsResources[workingFontFamilyIndex] = workingFontFamily
        }
      }
    }
  }
  return {
    ...oldValue,
    googleFontsResources: workingGoogleFontsResources,
  }
}

interface FontWeightAndStyleSelectOption {
  value: WebFontFamilyVariant
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
        value: webFontFamilyVariant(primaryFont, variant),
        label: prettyNameForFontVariant(variant),
        style: { fontWeight: variant.webFontWeight, fontStyle: variant.webFontStyle },
      }
    })
  }, [primaryFont])

  const { useSubmitValueFactory: useResourcesSubmitValueFactory } = useExternalResources()
  const [onSubmitNewFontVariantToResources] = useResourcesSubmitValueFactory(
    updateAddNewFontVariant,
  )
  const [onSubmitFontWeightAndStyle] = useSubmitValueFactory(updateFontWeightAndStyle)
  const onSubmitValue = React.useCallback(
    (newValue: FontWeightAndStyleSelectOption) => {
      onSubmitNewFontVariantToResources(newValue)
      onSubmitFontWeightAndStyle(newValue)
    },
    [onSubmitNewFontVariantToResources, onSubmitFontWeightAndStyle],
  )

  const selectValue = fontWeightAndStyleOptions.find(
    (v) =>
      v.value.fontVariant.webFontStyle === value.fontStyle &&
      v.value.fontVariant.webFontWeight === value.fontWeight,
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
