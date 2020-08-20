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
  WebFontVariant,
} from '../../../../navigator/external-resources/google-fonts-utils'
import { CSSFontStyle, CSSFontWeight } from '../../../common/css-utils'
import {
  OnUnsetValues,
  ParsedValues,
  UseSubmitValueFactory,
} from '../../../common/property-path-hooks'
import { InspectorModal } from '../../../widgets/inspector-modal'
import { FontFamilySelectPopupItem } from './font-family-select-popup-item'

function getOptionIndex(
  filteredData: Array<ItemData>,
  selectedOption: ItemData | null | undefined,
): number {
  if (selectedOption != null) {
    const foundIndex = filteredData.findIndex((v) => {
      return (
        selectedOption.metadata.type === v.metadata.type &&
        selectedOption.metadata.name === v.metadata.name
      )
    })
    return foundIndex > -1 ? foundIndex : 0
  } else {
    return 0
  }
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

function updateNewFontVariant(
  newValue: { fontFamily: string; fontWeight: CSSFontWeight; fontStyle: CSSFontStyle },
  oldValue: ParsedValues<'fontFamily' | 'fontStyle' | 'fontWeight'>,
): ParsedValues<'fontFamily' | 'fontStyle' | 'fontWeight'> {
  if (
    newValue.fontFamily ===
    '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol"'
  ) {
    return {
      fontFamily: [
        '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol"',
      ],
      fontWeight: newValue.fontWeight,
      fontStyle: newValue.fontStyle,
    }
  } else {
    return {
      fontFamily: [newValue.fontFamily],
      fontWeight: newValue.fontWeight,
      fontStyle: newValue.fontStyle,
    }
  }
}

export function submitAndClosePopup(
  closePopup: () => void,
  onSubmitFontFamilyVariant: OnSubmitValue<{
    fontFamily: string
    fontWeight: CSSFontWeight
    fontStyle: CSSFontStyle
  }>,
  metadata: SystemDefaultTypeface | GoogleFontsTypeface,
  fontWeight: CSSFontWeight,
  fontStyle: CSSFontStyle,
  pushNewFontFamilyVariant: (newValue: WebFontFamilyVariant) => void,
) {
  closePopup()
  let targetFontWeight: CSSFontWeight = fontWeight
  let targetFontStyle: CSSFontStyle = fontStyle
  if (metadata.type === 'google-fonts-typeface') {
    const parsedNewWebFontWeight = cssFontWeightToWebFontWeight(fontWeight)
    const parsedNewWebFontStyle = cssFontStyleToWebFontStyle(fontStyle)
    if (isRight(parsedNewWebFontWeight) && isRight(parsedNewWebFontStyle)) {
      const newWebFontWeight = parsedNewWebFontWeight.value
      const newWebFontStyle = parsedNewWebFontStyle.value
      const googleFontsTypeface = googleFontsList.find((family) => family.name === metadata.name)
      if (googleFontsTypeface != null) {
        const variantExistsOnTypeface =
          googleFontsTypeface.variants.find(
            (variant) =>
              variant.webFontStyle === newWebFontStyle &&
              variant.webFontWeight === newWebFontWeight,
          ) != null
        const closestMatchingVariant: WebFontVariant = (() => {
          if (variantExistsOnTypeface) {
            return webFontVariant(newWebFontWeight, newWebFontStyle)
          } else {
            const variantsWithSameFontStyle = googleFontsTypeface?.variants.filter(
              (v) => v.webFontStyle === newWebFontStyle,
            )
            if (variantsWithSameFontStyle.length === 1) {
              return variantsWithSameFontStyle[0]
            } else if (variantsWithSameFontStyle.length > 1) {
              const closestVariant = variantsWithSameFontStyle.reduce(
                (workingClosest, challenger) => {
                  const workingClosestDistance = newWebFontWeight - workingClosest.webFontWeight
                  const challengerDistance = newWebFontWeight - challenger.webFontWeight
                  if (
                    (workingClosest.webFontStyle === newWebFontStyle) ===
                    (challenger.webFontStyle === newWebFontStyle)
                  ) {
                    return challengerDistance < workingClosestDistance ? challenger : workingClosest
                  } else {
                    return workingClosest
                  }
                },
              )
              return closestVariant
            } else {
              throw Error('No matching variant found')
            }
          }
        })()
        targetFontWeight = closestMatchingVariant.webFontWeight
        targetFontStyle = closestMatchingVariant.webFontStyle
        const newWebFontFamilyVariant = webFontFamilyVariant(metadata.name, closestMatchingVariant)
        pushNewFontFamilyVariant(newWebFontFamilyVariant)
      }
    }
  }
  onSubmitFontFamilyVariant({
    fontFamily: metadata.name,
    fontWeight: targetFontWeight,
    fontStyle: targetFontStyle,
  })
}

function filterData(data: Array<ItemData>, lowerCaseSearchTerm: string): Array<ItemData> {
  return data.filter((datum) => datum.metadata.name.toLowerCase().includes(lowerCaseSearchTerm))
}

export const FontFamilySelectPopup = betterReactMemo<FontFamilySelectPopupProps>(
  'FontFamilySelectPopup',
  ({ value: { fontFamily, fontWeight, fontStyle }, useSubmitValueFactory, closePopup }) => {
    const stringInputRef = React.useRef<HTMLInputElement>(null)
    const variableSizeListRef = React.useRef<VariableSizeList>(null)

    const [searchTerm, setSearchTerm] = React.useState('')
    const lowerCaseSearchTerm = searchTerm.toLowerCase()

    const filteredData = React.useRef(filterData(itemData, lowerCaseSearchTerm))

    const valueOption =
      itemData.find((v) => {
        if (v.metadata.type === 'google-fonts-typeface') {
          return v.metadata.name === fontFamily[0]
        } else {
          return fontFamily.join(', ') === systemDefaultTypeface.name
        }
      }) ?? null

    const [onSubmitFontVariant] = useSubmitValueFactory(updateNewFontVariant)

    const { useSubmitValueFactory: useResourcesSubmitValueFactory } = useExternalResources()
    const [pushNewFontFamilyVariant] = useResourcesSubmitValueFactory(
      updatePushNewFontFamilyVariant,
    )

    const [selectedOption, setSelectedOption] = React.useState<ItemData>(
      valueOption ?? filteredData.current[0],
    )
    const filteredDataCurrent = filteredData.current
    const selectedIndex = React.useMemo<number>(
      () => getOptionIndex(filteredDataCurrent, selectedOption),
      [filteredDataCurrent, selectedOption],
    )

    const onStringInputKeyDown = React.useCallback((e: React.KeyboardEvent) => {
      if (
        !(e.key === 'ArrowDown' || e.key === 'ArrowUp' || e.key === 'Enter' || e.key === 'Escape')
      ) {
        e.stopPropagation()
      }
    }, [])

    const onWrapperKeyDown = React.useCallback(
      (e: React.KeyboardEvent) => {
        e.stopPropagation()
        switch (e.key) {
          case 'ArrowUp': {
            setSelectedOption(
              (v) => filteredData.current[Math.max(0, getOptionIndex(filteredData.current, v) - 1)],
            )
            break
          }
          case 'ArrowDown': {
            setSelectedOption(
              (v) =>
                filteredData.current[
                  Math.min(
                    getOptionIndex(filteredData.current, v) + 1,
                    filteredData.current.length - 1,
                  )
                ],
            )
            break
          }
          case 'Enter': {
            submitAndClosePopup(
              closePopup,
              onSubmitFontVariant,
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
              onStringInputKeyDown(e)
            }
          }
        }
      },
      [
        closePopup,
        filteredData,
        fontStyle,
        fontWeight,
        onSubmitFontVariant,
        pushNewFontFamilyVariant,
        selectedOption,
        onStringInputKeyDown,
      ],
    )

    const getItemSize = React.useCallback(
      (index: number) => filteredData.current[index]?.height ?? NormalItemSize,
      [filteredData],
    )

    const onChange = React.useCallback(
      (e: React.ChangeEvent<HTMLInputElement>) => {
        if (variableSizeListRef.current != null) {
          variableSizeListRef.current.resetAfterIndex(0)
        }

        const newSearchTerm = e.target.value
        setSearchTerm(newSearchTerm)

        const newFilteredData = filterData(itemData, newSearchTerm)
        filteredData.current = newFilteredData
        const newFilteredDataSelectedIndex = getOptionIndex(newFilteredData, selectedOption)
        setSelectedOption(newFilteredData[newFilteredDataSelectedIndex])
      },
      [selectedOption],
    )

    React.useEffect(() => {
      if (variableSizeListRef.current != null && selectedIndex != null) {
        variableSizeListRef.current.scrollToItem(selectedIndex, 'auto')
      }
    }, [selectedIndex])

    React.useEffect(() => {
      if (variableSizeListRef.current != null && selectedIndex != null) {
        variableSizeListRef.current.scrollToItem(selectedIndex, 'start')
      }
      // we only want to do this on mount
      // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [])

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
              onSubmitFontFamily: onSubmitFontVariant,
              itemsArray: filteredData.current,
              fontWeight,
              fontStyle,
              closePopup,
              selectedIndex,
              setSelectedOption,
              pushNewFontFamilyVariant,
            }}
            width={'100%'}
            height={215}
            itemCount={filteredData.current.length}
          >
            {FontFamilySelectPopupItem}
          </VariableSizeList>
        </FlexColumn>
      </InspectorModal>
    )
  },
)
