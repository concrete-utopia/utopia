import * as React from 'react'
import { betterReactMemo } from 'uuiui-deps'
import utils from '../../../../../utils/utils'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { addOnUnsetValues } from '../../../common/context-menu-items'
import { CSSFontFamily } from '../../../common/css-utils'
import { stylePropPathMappingFn, useInspectorInfo } from '../../../common/property-path-hooks'
import { SelectControl } from '../../../controls/select-control'
import { PropertyRow } from '../../../widgets/property-row'
import { fontFamilyArrayToCSSFontFamilyString, TypefaceInfo } from './fonts-list'
import { googleFontsList } from '../../../../../../assets/google-fonts-list'
import {
  googleVariantStringsIntoWebFontVariants,
  webFontVariant,
  WebFontFamily,
  webFontFamily,
  defaultWebFontWeightsAndStyles,
  WebFontFamilyVariant,
} from '../../../../navigator/external-resources/google-fonts-utils'
import { isRight } from '../../../../../core/shared/either'
import { FlexRow } from '../../../../../uuiui'

function getInfoForTypeface(cssFontFamily: CSSFontFamily): WebFontFamily {
  const value = googleFontsList.find((typeface) => typeface.family === cssFontFamily[0])
  if (value != null) {
    const parsedVariants = googleVariantStringsIntoWebFontVariants(value.variants)
    if (isRight(parsedVariants)) {
      return webFontFamily(value.family, parsedVariants.value)
    } else {
      return webFontFamily(value.family, [webFontVariant(400, 'normal')])
    }
  }
  return webFontFamily(cssFontFamily[0], { ...defaultWebFontWeightsAndStyles })
}

export const FontFamilySelect = betterReactMemo('TextSubsection', () => {
  const { value, useSubmitValueFactory, onUnsetValues, controlStyles } = useInspectorInfo(
    ['fontFamily', 'fontStyle', 'fontWeight'],
    (newValue) => newValue,
    (newValue) => newValue,
    stylePropPathMappingFn,
  )

  const fontFamilyContextMenuItems = utils.stripNulls([
    controlStyles.unsettable ? addOnUnsetValues(['fontFamily'], onUnsetValues) : null,
  ])

  return (
    <PropertyRow style={{ gridColumnGap: 8, marginBottom: 8 }}>
      <InspectorContextMenuWrapper
        id='fontFamily-context-menu'
        items={fontFamilyContextMenuItems}
        data={null}
        style={{ gridColumn: '1 / span 6' }}
      >
        <FlexRow>{value}</FlexRow>
      </InspectorContextMenuWrapper>
    </PropertyRow>
  )
})
FontFamilySelect.displayName = 'FontFamilySelect'
