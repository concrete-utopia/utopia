import * as React from 'react'
import { VariableSizeList } from 'react-window'
import { betterReactMemo, ControlStyles } from 'uuiui-deps'
import { googleFontsList } from '../../../../../../assets/google-fonts-list'
import { FlexColumn, FlexRow, StringInput, UtopiaTheme } from '../../../../../uuiui'
import {
  GoogleFontsTypeface,
  systemDefaultTypeface,
  SystemDefaultTypeface,
} from '../../../../navigator/external-resources/google-fonts-utils'
import {
  OnUnsetValues,
  ParsedValues,
  UseSubmitValueFactory,
} from '../../../common/property-path-hooks'
import { InspectorModal } from '../../../widgets/inspector-modal'
import { FontFamilySelectPopupItem } from './font-family-select-popup-item'

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

export const FontFamilySelectPopup = betterReactMemo<FontFamilySelectPopupProps>(
  'FontFamilySelectPopup',
  ({ value: { fontWeight, fontStyle }, useSubmitValueFactory, closePopup }) => {
    const ref = React.useRef<VariableSizeList>(null)
    const [searchTerm, setSearchTerm] = React.useState('')
    const lowerCaseSearchTerm = searchTerm.toLowerCase()
    const filteredData = React.useMemo(
      () =>
        itemData.filter((datum) => datum.metadata.name.toLowerCase().includes(lowerCaseSearchTerm)),
      [lowerCaseSearchTerm],
    )
    const getItemSize = React.useCallback((index: number) => filteredData[index].height, [
      filteredData,
    ])

    const onChange = React.useCallback((e: React.ChangeEvent<HTMLInputElement>) => {
      setSearchTerm(e.target.value)
      if (ref.current != null) {
        ref.current.resetAfterIndex(0)
      }
    }, [])

    const [onSubmitFontFamily] = useSubmitValueFactory(updateNewFontFamily)

    return (
      <InspectorModal
        offsetX={-(ModalWidth + UtopiaTheme.layout.inspectorModalBaseOffset + 16)}
        offsetY={0}
        closePopup={closePopup}
      >
        <FlexColumn
          style={{
            backgroundColor: 'white',
            width: ModalWidth,
            boxShadow: `0 3px 6px #0002`,
          }}
        >
          <FlexRow style={{ padding: 12 }}>
            <StringInput
              focusOnMount
              placeholder='Search for fonts…'
              value={searchTerm}
              onChange={onChange}
              style={{ flexGrow: 1 }}
            />
          </FlexRow>
          <VariableSizeList
            ref={ref}
            itemSize={getItemSize}
            itemData={{
              onSubmitFontFamily,
              itemsArray: filteredData,
              fontWeight,
              fontStyle,
              closePopup,
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
