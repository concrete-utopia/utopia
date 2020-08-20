import * as React from 'react'
import { ListChildComponentProps } from 'react-window'
import { FlexColumn, UtopiaTheme } from '../../../../../uuiui'
import {
  GoogleFontsTypeface,
  SystemDefaultTypeface,
  WebFontFamilyVariant,
} from '../../../../navigator/external-resources/google-fonts-utils'
import { CSSFontStyle, CSSFontWeight } from '../../../common/css-utils'
import { OnSubmitValue } from '../../../controls/control'
import { ItemData, submitAndClosePopup } from './font-family-select-popup'

interface FontsListChildComponentProps extends ListChildComponentProps {
  data: {
    fontWeight: CSSFontWeight
    fontStyle: CSSFontStyle
    onSubmitFontFamily: OnSubmitValue<{
      fontFamily: string
      fontWeight: CSSFontWeight
      fontStyle: CSSFontStyle
    }>
    itemsArray: Array<ItemData>
    closePopup: () => void
    selectedIndex: number | null
    setSelectedOption: React.Dispatch<React.SetStateAction<ItemData>>
    pushNewFontFamilyVariant: (newValue: WebFontFamilyVariant) => void
  }
}

export const FontFamilySelectPopupItem: React.FunctionComponent<FontsListChildComponentProps> = ({
  data: {
    itemsArray,
    onSubmitFontFamily,
    fontWeight,
    fontStyle,
    closePopup,
    selectedIndex,
    setSelectedOption,
    pushNewFontFamilyVariant,
  },
  style,
  index,
}) => {
  const metadata = itemsArray[index].metadata

  const selected = selectedIndex === index

  const onMouseEnter = React.useCallback(() => {
    setSelectedOption(itemsArray[index])
  }, [setSelectedOption, itemsArray, index])

  const onClick = React.useCallback(() => {
    submitAndClosePopup(
      closePopup,
      onSubmitFontFamily,
      metadata,
      fontWeight,
      fontStyle,
      pushNewFontFamilyVariant,
    )
  }, [onSubmitFontFamily, fontStyle, fontWeight, metadata, pushNewFontFamilyVariant, closePopup])
  return (
    <FlexColumn
      style={{
        ...style,
        paddingLeft: 12,
        paddingRight: 12,
        paddingTop: 6,
        paddingBottom: 6,
        backgroundColor: selected ? UtopiaTheme.color.inspectorFocusedColor.value : undefined,
        color: selected ? 'white' : undefined,
      }}
      className='font-family-popup-item'
      onClick={onClick}
      onMouseEnter={onMouseEnter}
    >
      {metadata.type === 'system-default-typeface' ? (
        <React.Fragment>
          <div style={{ fontSize: 12 }}>System Default</div>
          <div
            style={{
              fontSize: 11,
              whiteSpace: 'normal',
              backgroundColor: selected ? UtopiaTheme.color.inspectorFocusedColor.value : undefined,
              color: selected ? 'white' : '#888',
            }}
          >
            Use the default typeface of the operating system: SF Pro on macOS and iOS, Roboto on
            Android and Segoe UI on Windows
          </div>
        </React.Fragment>
      ) : (
        <div style={{ fontSize: 12 }}>{metadata.name}</div>
      )}
    </FlexColumn>
  )
}
