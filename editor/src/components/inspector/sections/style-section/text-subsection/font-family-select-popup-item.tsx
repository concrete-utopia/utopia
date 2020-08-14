import * as React from 'react'
import { ListChildComponentProps } from 'react-window'
import { isRight } from '../../../../../core/shared/either'
import { useExternalResources } from '../../../../../printer-parsers/html/external-resources-parser'
import { FlexColumn } from '../../../../../uuiui'
import { updatePushNewFontFamilyVariant } from '../../../../navigator/external-resources/google-fonts-resources-list-search'
import {
  cssFontStyleToWebFontStyle,
  cssFontWeightToWebFontWeight,
  GoogleFontsTypefaceMetadata,
  SystemDefaultTypefaceMetadata,
  webFontFamilyVariant,
  webFontVariant,
} from '../../../../navigator/external-resources/google-fonts-utils'
import { CSSFontStyle, CSSFontWeight } from '../../../common/css-utils'
import { OnSubmitValue } from '../../../controls/control'
import { ItemData } from './font-family-select-popup'

interface FontsListChildComponentProps extends ListChildComponentProps {
  data: {
    fontWeight: CSSFontWeight
    fontStyle: CSSFontStyle
    onSubmitFontFamily: OnSubmitValue<SystemDefaultTypefaceMetadata | GoogleFontsTypefaceMetadata>
    itemsArray: Array<ItemData>
  }
}

export const FontFamilySelectPopupItem: React.FunctionComponent<FontsListChildComponentProps> = ({
  data: { itemsArray, onSubmitFontFamily, fontWeight, fontStyle },
  style,
  index,
}) => {
  const metadata = itemsArray[index].metadata
  const { useSubmitValueFactory } = useExternalResources()
  const [pushNewFontFamilyVariant] = useSubmitValueFactory(updatePushNewFontFamilyVariant)

  const onClick = React.useCallback(() => {
    const webFontWeight = cssFontWeightToWebFontWeight(fontWeight)
    const webFontStyle = cssFontStyleToWebFontStyle(fontStyle)
    if (isRight(webFontWeight) && isRight(webFontStyle)) {
      pushNewFontFamilyVariant(
        webFontFamilyVariant(
          metadata.name,
          webFontVariant(webFontWeight.value, webFontStyle.value),
        ),
      )
      // TOOD add toast explaining a font has been added
    } else {
      // TODO add toast explaining why a font cannot be added
    }
    onSubmitFontFamily(metadata)
  }, [onSubmitFontFamily, fontStyle, fontWeight, metadata, pushNewFontFamilyVariant])
  return (
    <FlexColumn style={style} onClick={onClick}>
      {metadata.type === 'system-default-typeface' ? (
        <>
          <div style={{ fontSize: 12 }}>System Default</div>
          <div style={{ fontSize: 11, color: '#888', whiteSpace: 'normal' }}>
            Use the default typeface of the operating system: SF Pro on macOS and iOS, Roboto on
            Android and Segoe UI on Windows
          </div>
        </>
      ) : (
        <div style={{ fontSize: 12 }}>{metadata.name}</div>
      )}
    </FlexColumn>
  )
}
