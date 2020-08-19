import * as React from 'react'
import { VariableSizeList } from 'react-window'
import { betterReactMemo, ControlStyles, OnSubmitValue } from 'uuiui-deps'
import { googleFontsList } from '../../../../../../assets/google-fonts-list'
import { isRight } from '../../../../../core/shared/either'
import { useExternalResources } from '../../../../../printer-parsers/html/external-resources-parser'
import { FlexColumn, FlexRow, StringInput, UtopiaTheme } from '../../../../../uuiui'
import { updatePushNewFontFamilyVariant } from '../../../../navigator/external-resources/google-fonts-resources-list-search'
import {
  cssFontStyleToWebFontStyle,
  cssFontWeightToWebFontWeight,
  GoogleFontsTypeface,
  systemDefaultTypeface,
  SystemDefaultTypeface,
  WebFontFamilyVariant,
  webFontFamilyVariant,
  webFontVariant,
} from '../../../../navigator/external-resources/google-fonts-utils'
import { CSSFontStyle, CSSFontWeight } from '../../../common/css-utils'
import {
  OnUnsetValues,
  ParsedValues,
  UseSubmitValueFactory,
} from '../../../common/property-path-hooks'
import { InspectorModal } from '../../../widgets/inspector-modal'
import { FontFamilySelectPopupItem } from './font-family-select-popup-item'

function getOptionIndex(filteredData: Array<ItemData>, selectedItem: ItemData | null | undefined) {
  const foundIndex = filteredData.findIndex((v) => {
    if (selectedItem != null) {
      return (
        selectedItem.metadata.type === v.metadata.type &&
        selectedItem.metadata.name === v.metadata.name
      )
    } else {
      return false
    }
  })
  return foundIndex > -1 ? foundIndex : 0
}

interface FontFamilySelectPopupProps {
  value: ParsedValues<'fontFamily' | 'fontStyle' | 'fontWeight'>
  onUnsetValues: OnUnsetValues
  controlStyles: ControlStyles
  closePopup: () => void
  useSubmitValueFactory: UseSubmitValueFactory<
    ParsedValues<'fontFamily' | 'fontStyle' | 'fontWeight'>
  >
}

const ModalWidth = 220

const NormalItemSize = 26
const DefaultSystemFontSize = 84

export interface ItemData {
  metadata: SystemDefaultTypeface | GoogleFontsTypeface
  height: number
}
const itemData: Array<ItemData> = [systemDefaultTypeface, ...googleFontsList].map((item, i) => ({
  metadata: item,
  height: i === 0 ? DefaultSystemFontSize : NormalItemSize,
}))

function updateNewFontFamily(
  newValue: SystemDefaultTypeface | GoogleFontsTypeface,
  oldValue: ParsedValues<'fontFamily' | 'fontStyle' | 'fontWeight'>,
): ParsedValues<'fontFamily' | 'fontStyle' | 'fontWeight'> {
  switch (newValue.type) {
    case 'system-default-typeface': {
      return {
        ...oldValue,
        fontFamily: [
          '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol"',
        ],
      }
    }
    case 'google-fonts-typeface': {
      return {
        ...oldValue,
        fontFamily: [newValue.name],
      }
    }
  }
}

export function submitAndClosePopup(
  closePopup: () => void,
  onSubmitFontFamily: OnSubmitValue<SystemDefaultTypeface | GoogleFontsTypeface>,
  metadata: SystemDefaultTypeface | GoogleFontsTypeface,
  fontWeight: CSSFontWeight,
  fontStyle: CSSFontStyle,
  pushNewFontFamilyVariant: (newValue: WebFontFamilyVariant) => void,
) {
  closePopup()
  onSubmitFontFamily(metadata)
  if (metadata.type === 'google-fonts-typeface') {
    const webFontWeight = cssFontWeightToWebFontWeight(fontWeight)
    const webFontStyle = cssFontStyleToWebFontStyle(fontStyle)
    if (isRight(webFontWeight) && isRight(webFontStyle)) {
      const newWebFontFamilyVariant = webFontFamilyVariant(
        metadata.name,
        webFontVariant(webFontWeight.value, webFontStyle.value),
      )
      pushNewFontFamilyVariant(newWebFontFamilyVariant)
    }
  }
}

export const FontFamilySelectPopup = betterReactMemo<FontFamilySelectPopupProps>(
  'FontFamilySelectPopup',
  ({ value: { fontFamily, fontWeight, fontStyle }, useSubmitValueFactory, closePopup }) => {
    const wrapperRef = React.useRef<HTMLDivElement>(null)
    const stringInputRef = React.useRef<HTMLInputElement>(null)
    const variableSizeListRef = React.useRef<VariableSizeList>(null)

    const [searchTerm, setSearchTerm] = React.useState('')
    const lowerCaseSearchTerm = searchTerm.toLowerCase()

    const filteredData = React.useMemo(
      () =>
        itemData.filter((datum) => datum.metadata.name.toLowerCase().includes(lowerCaseSearchTerm)),
      [lowerCaseSearchTerm],
    )

    const valueOption =
      itemData.find((v) => {
        if (v.metadata.type === 'google-fonts-typeface') {
          return v.metadata.name === fontFamily[0]
        } else {
          return fontFamily.join(', ') === systemDefaultTypeface.name
        }
      }) ?? null

    const [onSubmitFontFamily] = useSubmitValueFactory(updateNewFontFamily)

    const { useSubmitValueFactory: useResourcesSubmitValueFactory } = useExternalResources()
    const [pushNewFontFamilyVariant] = useResourcesSubmitValueFactory(
      updatePushNewFontFamilyVariant,
    )

    const [selectedOption, setSelectedOption] = React.useState<ItemData>(
      valueOption ?? filteredData[0],
    )
    const selectedIndex = React.useMemo<number>(
      () => getOptionIndex(filteredData, selectedOption),
      [filteredData, selectedOption],
    )

    const onWrapperKeyDown = React.useCallback(
      (e: React.KeyboardEvent) => {
        e.stopPropagation()
        switch (e.key) {
          case 'ArrowUp': {
            setSelectedOption((v) => filteredData[Math.max(0, getOptionIndex(filteredData, v) - 1)])
            break
          }
          case 'ArrowDown': {
            setSelectedOption(
              (v) =>
                filteredData[
                  Math.min(getOptionIndex(filteredData, v) + 1, filteredData.length - 1)
                ],
            )
            break
          }
          case 'Enter': {
            submitAndClosePopup(
              closePopup,
              onSubmitFontFamily,
              selectedOption.metadata,
              fontWeight,
              fontStyle,
              pushNewFontFamilyVariant,
            )
            break
          }
          case 'Escape': {
            closePopup()
            break
          }
          default: {
            if (stringInputRef.current != null) {
              stringInputRef.current.focus()
              stringInputRef.current.dispatchEvent(e.nativeEvent)
            }
          }
        }
      },
      [
        closePopup,
        filteredData,
        fontStyle,
        fontWeight,
        onSubmitFontFamily,
        pushNewFontFamilyVariant,
        selectedOption,
      ],
    )

    const onStringInputKeyDown = React.useCallback((e: React.KeyboardEvent) => {
      if (
        (e.key === 'ArrowDown' || e.key === 'ArrowUp' || e.key === 'Enter' || e.key === 'Escape') &&
        wrapperRef.current != null
      ) {
        wrapperRef.current.dispatchEvent(e.nativeEvent)
      }
      e.stopPropagation()
    }, [])

    const getItemSize = React.useCallback(
      (index: number) => filteredData[index]?.height ?? NormalItemSize,
      [filteredData],
    )

    const onChange = React.useCallback((e: React.ChangeEvent<HTMLInputElement>) => {
      setSearchTerm(e.target.value)
      if (variableSizeListRef.current != null) {
        variableSizeListRef.current.resetAfterIndex(0)
      }
    }, [])

    React.useEffect(() => {
      if (variableSizeListRef.current != null && selectedIndex != null) {
        variableSizeListRef.current.scrollToItem(selectedIndex, 'auto')
      }
    }, [selectedIndex])

    React.useEffect(() => {
      if (stringInputRef.current != null) {
        stringInputRef.current.focus()
      }
    })

    return (
      <InspectorModal
        offsetX={-(ModalWidth + UtopiaTheme.layout.inspectorModalBaseOffset + 16)}
        offsetY={0}
        closePopup={closePopup}
      >
        <FlexColumn
          ref={wrapperRef}
          tabIndex={0}
          style={{
            backgroundColor: 'white',
            width: ModalWidth,
            boxShadow: `0 3px 6px #0002`,
          }}
          onKeyDown={onWrapperKeyDown}
        >
          <FlexRow style={{ padding: 12 }}>
            <StringInput
              ref={stringInputRef}
              placeholder='Search for fonts…'
              value={searchTerm}
              onKeyDown={onStringInputKeyDown}
              onChange={onChange}
              style={{ flexGrow: 1 }}
            />
          </FlexRow>
          <VariableSizeList
            ref={variableSizeListRef}
            itemSize={getItemSize}
            itemData={{
              onSubmitFontFamily,
              itemsArray: filteredData,
              fontWeight,
              fontStyle,
              closePopup,
              selectedIndex,
              setSelectedOption,
              pushNewFontFamilyVariant,
            }}
            width={'100%'}
            height={215}
            itemCount={filteredData.length}
          >
            {FontFamilySelectPopupItem}
          </VariableSizeList>
        </FlexColumn>
      </InspectorModal>
    )
  },
)
