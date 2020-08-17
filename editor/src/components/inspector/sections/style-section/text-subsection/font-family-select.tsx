import * as React from 'react'
import { betterReactMemo } from 'uuiui-deps'
import { googleFontsList } from '../../../../../../assets/google-fonts-list'
import { isRight } from '../../../../../core/shared/either'
import { identity } from '../../../../../core/shared/utils'
import utils from '../../../../../utils/utils'
import { FlexRow, Icons, UtopiaTheme } from '../../../../../uuiui'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import {
  defaultWebFontWeightsAndStyles,
  googleVariantStringsIntoWebFontVariants,
  WebFontFamily,
  webFontFamily,
  webFontVariant,
} from '../../../../navigator/external-resources/google-fonts-utils'
import { addOnUnsetValues } from '../../../common/context-menu-items'
import { CSSFontFamily } from '../../../common/css-utils'
import { stylePropPathMappingFn, useInspectorInfo } from '../../../common/property-path-hooks'
import { PropertyRow } from '../../../widgets/property-row'
import { FontFamilySelectPopup } from './font-family-select-popup'

function getInfoForTypeface(cssFontFamily: CSSFontFamily): WebFontFamily {
  const value = googleFontsList.find((typeface) => typeface.name === cssFontFamily[0])
  if (value != null) {
    const parsedVariants = googleVariantStringsIntoWebFontVariants(value.variants)
    if (isRight(parsedVariants)) {
      return webFontFamily(value.name, parsedVariants.value)
    } else {
      return webFontFamily(value.name, [webFontVariant(400, 'normal')])
    }
  }
  return webFontFamily(cssFontFamily[0], { ...defaultWebFontWeightsAndStyles })
}

export const FontFamilySelect = betterReactMemo('FontFamilySelect', () => {
  const [popupIsOpen, setPopupIsOpen] = React.useState(false)
  const togglePopup = React.useCallback(() => setPopupIsOpen((v) => !v), [])
  const closePopup = React.useCallback(() => setPopupIsOpen(false), [])

  const { value, useSubmitValueFactory, onUnsetValues, controlStyles } = useInspectorInfo(
    ['fontFamily', 'fontStyle', 'fontWeight'],
    identity,
    identity,
    stylePropPathMappingFn,
  )

  const fontFamilyContextMenuItems = utils.stripNulls([
    controlStyles.unsettable ? addOnUnsetValues(['fontFamily'], onUnsetValues) : null,
  ])

  return (
    <PropertyRow>
      <InspectorContextMenuWrapper
        id='fontFamily-context-menu'
        items={fontFamilyContextMenuItems}
        data={null}
        style={{ marginBottom: 8, gridColumn: '1 / span 6' }}
      >
        {popupIsOpen ? (
          <FontFamilySelectPopup
            value={value}
            onUnsetValues={onUnsetValues}
            controlStyles={controlStyles}
            closePopup={closePopup}
            useSubmitValueFactory={useSubmitValueFactory}
          />
        ) : null}
        <FlexRow
          onMouseDown={togglePopup}
          style={{
            boxShadow: `0 0 0 1px ${controlStyles.borderColor} inset`,
            padding: 4,
            fontSize: 14,
            height: 30,
            borderRadius: UtopiaTheme.inputBorderRadius,
          }}
        >
          <div style={{ flexGrow: 1 }}>{value.fontFamily[0]}</div>
          <Icons.ExpansionArrowDown />
        </FlexRow>
      </InspectorContextMenuWrapper>
    </PropertyRow>
  )
})
