import React from 'react'
import type { OptionsType } from 'react-select'
import { googleFontsList } from '../../../../../../assets/google-fonts-list'
import { identity } from '../../../../../core/shared/utils'
import type { ExternalResources } from '../../../../../printer-parsers/html/external-resources-parser'
import {
  googleFontsResource,
  useExternalResources,
} from '../../../../../printer-parsers/html/external-resources-parser'
import utils from '../../../../../utils/utils'
import { Tooltip, PopupList, UtopiaTheme } from '../../../../../uuiui'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import type { WebFontFamilyVariant } from '../../../../navigator/external-resources/google-fonts-utils'
import {
  defaultWebFontWeightsAndStyles,
  prettyNameForFontVariant,
  webFontFamilyVariant,
} from '../../../../navigator/external-resources/google-fonts-utils'
import { addOnUnsetValues } from '../../../common/context-menu-items'
import type { ParsedValues } from '../../../common/property-path-hooks'
import {
  stylePropPathMappingFn,
  useInspectorInfo,
  useInspectorStyleInfo,
} from '../../../common/property-path-hooks'

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
  const workingResourceIndex = workingGoogleFontsResources.findIndex(
    (v) => v.fontFamily === newValue.value.familyName,
  )

  const googleFontsListItemIndex = googleFontsList.findIndex(
    (v) => v.name === newValue.value.familyName,
  )
  if (googleFontsListItemIndex > -1) {
    const googleFontsListItem = googleFontsList[googleFontsListItemIndex]
    const googleFontVariantExists =
      googleFontsListItem.variants.some(
        (v) =>
          v.webFontStyle === newVariant.webFontStyle &&
          v.webFontWeight === newVariant.webFontWeight,
      ) != null
    if (workingResourceIndex > -1) {
      const workingResource = workingGoogleFontsResources[workingResourceIndex]
      if (googleFontVariantExists) {
        const variantIsAlreadyAdded = workingResource.variants.some(
          (v) =>
            v.webFontStyle === newVariant.webFontStyle &&
            v.webFontWeight === newVariant.webFontWeight,
        )
        if (!variantIsAlreadyAdded) {
          const workingVariants = [...workingResource.variants]
          workingVariants.push(newVariant)
          workingResource.variants = workingVariants
          workingGoogleFontsResources[workingResourceIndex] = workingResource
        }
      }
    } else {
      if (googleFontVariantExists) {
        workingGoogleFontsResources.push(
          googleFontsResource(newValue.value.familyName, [newVariant]),
        )
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

export const FontVariantSelect = React.memo(() => {
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
  const fontWeightAndStyleOptions: OptionsType<FontWeightAndStyleSelectOption> =
    React.useMemo(() => {
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
  const [onSubmitNewFontVariantToResources] =
    useResourcesSubmitValueFactory(updateAddNewFontVariant)
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
      style={{
        minHeight: UtopiaTheme.layout.rowHeight.normal,
        alignItems: 'stretch',
      }}
    >
      <Tooltip title='Font Weight and Style' placement='top'>
        <PopupList
          id={'fontWeightAndStyle'}
          onSubmitValue={onSubmitValue}
          value={selectValue}
          options={fontWeightAndStyleOptions}
          controlStyles={controlStyles}
          style={{ background: 'transparent' }}
        />
      </Tooltip>
    </InspectorContextMenuWrapper>
  )
})
