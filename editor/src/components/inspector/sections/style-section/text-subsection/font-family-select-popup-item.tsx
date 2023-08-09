import React from 'react'
import type { ListChildComponentProps } from 'react-window'
import { FlexColumn, useColorTheme } from '../../../../../uuiui'
import type { WebFontFamilyVariant } from '../../../../navigator/external-resources/google-fonts-utils'
import type { CSSFontStyle, CSSFontWeight } from '../../../common/css-utils'
import type { OnSubmitValue } from '../../../controls/control'
import type { ItemData } from './font-family-select-popup'
import { isSelectableItemData, submitNewValue } from './font-family-select-popup'
import { ProjectTypefaceItem } from './project-typeface-item'
import { SystedDefaultFontItem } from './system-default-font-item'

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
    updateSizes: () => void
  }
}

export const FontFamilySelectPopupItem: React.FunctionComponent<
  React.PropsWithChildren<FontsListChildComponentProps>
> = ({
  data: {
    itemsArray,
    onSubmitFontFamily,
    fontWeight,
    fontStyle,
    closePopup,
    selectedIndex,
    setSelectedOption,
    pushNewFontFamilyVariant,
    updateSizes,
  },
  style,
  index,
}) => {
  const colorTheme = useColorTheme()
  const option = itemsArray[index]
  const metadata = option.metadata

  const selected = selectedIndex === index

  const onMouseOver = React.useCallback(() => {
    if (isSelectableItemData(option)) {
      setSelectedOption(option)
    }
  }, [setSelectedOption, option])

  const onClick = React.useCallback(() => {
    if (isSelectableItemData(option)) {
      closePopup()
      submitNewValue(onSubmitFontFamily, option, fontWeight, fontStyle, pushNewFontFamilyVariant)
    }
  }, [onSubmitFontFamily, fontStyle, fontWeight, option, pushNewFontFamilyVariant, closePopup])

  const getPopupItem = () => {
    switch (metadata.type) {
      case 'ui-item': {
        return <metadata.component />
      }
      case 'system-default-typeface': {
        return <SystedDefaultFontItem selected={selected} />
      }
      case 'google-fonts-typeface': {
        return <div>{metadata.name}</div>
      }
      case 'project-typeface': {
        return (
          <ProjectTypefaceItem
            typeface={metadata.typeface}
            selected={selected}
            updateSizes={updateSizes}
          />
        )
      }
      default: {
        const _exhaustiveCheck: never = metadata
        throw Error('Popup item type not found')
      }
    }
  }

  return (
    <FlexColumn
      style={{
        ...style,
        paddingTop: 5,
        paddingBottom: 4,
        paddingLeft: 12,
        paddingRight: 12,
        backgroundColor: selected ? colorTheme.inspectorFocusedColor.value : undefined,
        fontSize: 12,
        color: selected ? colorTheme.bg1.value : undefined,
      }}
      onClick={onClick}
      onMouseOver={onMouseOver}
    >
      {getPopupItem()}
    </FlexColumn>
  )
}
