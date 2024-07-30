import React from 'react'
import { VariableSizeList } from 'react-window'
import { googleFontsList } from '../../../../../../assets/google-fonts-list'
import { isRight } from '../../../../../core/shared/either'
import { useExternalResources } from '../../../../../printer-parsers/html/external-resources-parser'
import {
  FlexColumn,
  UtopiaTheme,
  FlexRow,
  StringInput,
  useColorTheme,
  UtopiaStyles,
} from '../../../../../uuiui'
import type { ControlStyles } from '../../../../../uuiui-deps'
import { Utils } from '../../../../../uuiui-deps'
import { updatePushNewFontFamilyVariant } from '../../../../navigator/external-resources/google-fonts-resources-list-search'
import type {
  GoogleFontsTypeface,
  SystemDefaultTypeface,
  WebFontFamilyVariant,
  WebFontVariant,
} from '../../../../navigator/external-resources/google-fonts-utils'
import {
  cssFontStyleToWebFontStyle,
  cssFontWeightToWebFontWeight,
  systemDefaultTypeface,
  webFontFamilyVariant,
  webFontVariant,
} from '../../../../navigator/external-resources/google-fonts-utils'
import type { CSSFontStyle, CSSFontWeight } from '../../../common/css-utils'
import type {
  OnUnsetValues,
  ParsedValues,
  UseSubmitValueFactory,
} from '../../../common/property-path-hooks'
import type { OnSubmitValue } from '../../../controls/control'
import { FontFamilySelectPopupItem } from './font-family-select-popup-item'
import { ProjectFontDividerItem } from './project-font-divider-item'
import { ProjectFontHeaderItem } from './project-font-header-item'

function getOptionIndex(
  filteredData: Array<ItemData>,
  selectedOption: ItemData | null | undefined,
): number {
  if (selectedOption != null && isSelectableItemData(selectedOption)) {
    const selectedTypeface = getTypefaceFromItemData(selectedOption)
    const foundIndex = filteredData.findIndex((v) => {
      if (isSelectableItemData(v)) {
        const typefaceToMatchAgainst = getTypefaceFromItemData(v)
        return (
          selectedOption.metadata.type === v.metadata.type &&
          selectedTypeface.type === typefaceToMatchAgainst.type &&
          selectedTypeface.name === typefaceToMatchAgainst.name
        )
      } else {
        return false
      }
    })
    return foundIndex > -1
      ? foundIndex
      : Math.max(
          filteredData.findIndex((v) => isSelectableItemData(v)),
          0,
        )
  } else {
    return Math.max(
      filteredData.findIndex((v) => isSelectableItemData(v)),
      0,
    )
  }
}

interface FontFamilySelectPopupProps {
  style: React.CSSProperties
  value: ParsedValues<'fontFamily' | 'fontStyle' | 'fontWeight'>
  onUnsetValues: OnUnsetValues
  controlStyles: ControlStyles
  closePopup: () => void
  useSubmitFontVariantFactory: UseSubmitValueFactory<
    ParsedValues<'fontFamily' | 'fontStyle' | 'fontWeight'>
  >
}

const NormalItemSize = 26
const DefaultSystemFontSize = 70

export interface ProjectTypeface {
  type: 'project-typeface'
  typeface: SystemDefaultTypeface | GoogleFontsTypeface
}
function projectTypeface(typeface: SystemDefaultTypeface | GoogleFontsTypeface): ProjectTypeface {
  return {
    type: 'project-typeface',
    typeface,
  }
}
function isProjectTypeface(
  value: ProjectTypeface | SystemDefaultTypeface | GoogleFontsTypeface,
): value is ProjectTypeface {
  return value.type === 'project-typeface'
}

interface UiItem {
  type: 'ui-item'
  componentId: string
  component: React.ElementType
}
function uiItem(component: React.ElementType, componentId: string): UiItem {
  return {
    type: 'ui-item',
    componentId,
    component,
  }
}

export function getTypefaceFromItemData(
  value: SelectableItemData,
): SystemDefaultTypeface | GoogleFontsTypeface {
  return isProjectTypeface(value.metadata) ? value.metadata.typeface : value.metadata
}

export interface ItemData {
  metadata: ProjectTypeface | SystemDefaultTypeface | GoogleFontsTypeface | UiItem
  height: number
}
function itemData(
  metadata: ProjectTypeface | SystemDefaultTypeface | GoogleFontsTypeface | UiItem,
  height: number,
): ItemData {
  return {
    metadata,
    height,
  }
}

export interface SelectableItemData extends ItemData {
  metadata: ProjectTypeface | SystemDefaultTypeface | GoogleFontsTypeface
}

export function isSelectableItemData(value: ItemData): value is SelectableItemData {
  return value.metadata.type !== 'ui-item'
}

interface ProjectTypefaceItemData extends ItemData {
  metadata: ProjectTypeface
}

function projectTypefaceItemData(
  metadata: ProjectTypeface,
  height: number,
): ProjectTypefaceItemData {
  return { metadata, height }
}

interface StartingItemData extends ItemData {
  metadata: SystemDefaultTypeface | GoogleFontsTypeface
}
const startingItemData: Array<StartingItemData> = [systemDefaultTypeface, ...googleFontsList].map(
  (item, i) => ({
    metadata: item,
    height: i === 0 ? DefaultSystemFontSize : NormalItemSize,
  }),
)

function updateNewFontVariant(newValue: {
  fontFamily: string
  fontWeight: CSSFontWeight
  fontStyle: CSSFontStyle
}): ParsedValues<'fontFamily' | 'fontStyle' | 'fontWeight'> {
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

export function submitNewValue(
  onSubmitFontFamilyVariant: OnSubmitValue<{
    fontFamily: string
    fontWeight: CSSFontWeight
    fontStyle: CSSFontStyle
  }>,
  selectedOption: SelectableItemData,
  fontWeight: CSSFontWeight,
  fontStyle: CSSFontStyle,
  pushNewFontFamilyVariant: (newValue: WebFontFamilyVariant) => void,
) {
  let targetFontWeight: CSSFontWeight = fontWeight
  let targetFontStyle: CSSFontStyle = fontStyle
  const selectedTypeface = getTypefaceFromItemData(selectedOption)
  if (selectedTypeface.type === 'google-fonts-typeface') {
    const parsedNewWebFontWeight = cssFontWeightToWebFontWeight(fontWeight)
    const parsedNewWebFontStyle = cssFontStyleToWebFontStyle(fontStyle)
    if (isRight(parsedNewWebFontWeight) && isRight(parsedNewWebFontStyle)) {
      const newWebFontWeight = parsedNewWebFontWeight.value
      const newWebFontStyle = parsedNewWebFontStyle.value
      const googleFontsTypeface = googleFontsList.find(
        (family) => family.name === selectedTypeface.name,
      )
      if (googleFontsTypeface != null) {
        const variantExistsOnTypeface = googleFontsTypeface.variants.some(
          (variant) =>
            variant.webFontStyle === newWebFontStyle && variant.webFontWeight === newWebFontWeight,
        )
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
        const newWebFontFamilyVariant = webFontFamilyVariant(
          selectedTypeface.name,
          closestMatchingVariant,
        )
        pushNewFontFamilyVariant(newWebFontFamilyVariant)
      }
    }
  }
  onSubmitFontFamilyVariant({
    fontFamily: selectedTypeface.name,
    fontWeight: targetFontWeight,
    fontStyle: targetFontStyle,
  })
}

const projectFontHeaderItem = itemData(
  uiItem(ProjectFontHeaderItem, 'project-font-header'),
  NormalItemSize,
)
const projectFontDividerItem = itemData(uiItem(ProjectFontDividerItem, 'project-font-divider'), 10)

function filterData(
  data: Array<StartingItemData>,
  lowerCaseSearchTerm: string,
  projectTypefaces: Array<ProjectTypefaceItemData>,
): Array<ItemData> {
  if (lowerCaseSearchTerm.length === 0) {
    if (projectTypefaces.length > 0) {
      if (data.length > 0) {
        return [projectFontHeaderItem, ...projectTypefaces, projectFontDividerItem, ...data]
      } else {
        return [projectFontHeaderItem, ...projectTypefaces]
      }
    } else {
      return [...data]
    }
  } else {
    const filteredProjectTypefaces: Array<SelectableItemData> = projectTypefaces.filter((datum) => {
      return getTypefaceFromItemData(datum).name.toLowerCase().includes(lowerCaseSearchTerm)
    })
    const filteredData: Array<ItemData> = data.filter((datum) => {
      const typeface = getTypefaceFromItemData(datum)
      if (filteredProjectTypefaces.some((v) => getTypefaceFromItemData(v).name === typeface.name)) {
        return false
      } else {
        return typeface.name.toLowerCase().includes(lowerCaseSearchTerm)
      }
    })

    if (filteredProjectTypefaces.length > 0) {
      if (filteredData.length > 0) {
        return [
          projectFontHeaderItem,
          ...filteredProjectTypefaces,
          projectFontDividerItem,
          ...filteredData,
        ]
      } else {
        return [projectFontHeaderItem, ...filteredProjectTypefaces]
      }
    } else {
      return filteredData
    }
  }
}

export const FontFamilySelectPopup = React.memo(
  React.forwardRef<HTMLDivElement, FontFamilySelectPopupProps>(
    (
      {
        style,
        value: { fontFamily, fontWeight, fontStyle },
        useSubmitFontVariantFactory,
        closePopup,
      },
      wrapperRef,
    ) => {
      const stringInputRef = React.useRef<HTMLInputElement>(null)
      const variableSizeListRef = React.useRef<VariableSizeList>(null)

      const [searchTerm, setSearchTerm] = React.useState('')
      const lowerCaseSearchTerm = searchTerm.toLowerCase()

      const colorTheme = useColorTheme()

      const { values, useSubmitValueFactory: useResourcesSubmitValueFactory } =
        useExternalResources()
      const projectTypefaces: Array<ProjectTypefaceItemData> = React.useMemo(() => {
        return isRight(values)
          ? Utils.stripNulls(
              values.value.googleFontsResources.map((googleFontResource) => {
                const familyName = googleFontResource.fontFamily
                const matchedTypeface = startingItemData.find(
                  (datum) => getTypefaceFromItemData(datum).name === familyName,
                )
                return matchedTypeface != null
                  ? projectTypefaceItemData(
                      projectTypeface(matchedTypeface.metadata),
                      NormalItemSize,
                    )
                  : null
              }),
            )
          : []
      }, [values])

      const [pushNewFontFamilyVariant] = useResourcesSubmitValueFactory(
        updatePushNewFontFamilyVariant,
      )

      const filteredData = React.useMemo(
        () => filterData(startingItemData, lowerCaseSearchTerm, projectTypefaces),
        [lowerCaseSearchTerm, projectTypefaces],
      )

      const valueOption: SelectableItemData | null =
        filteredData.find((v): v is SelectableItemData => {
          switch (v.metadata.type) {
            case 'project-typeface':
            case 'google-fonts-typeface': {
              return getTypefaceFromItemData(v as SelectableItemData).name === fontFamily[0]
            }
            case 'system-default-typeface': {
              return fontFamily.join(', ') === systemDefaultTypeface.name
            }
            case 'ui-item': {
              return false
            }
            default: {
              const _exhaustiveCheck: never = v.metadata
              throw Error(
                `Fallthrough case finding a value option with ${JSON.stringify(v.metadata)}`,
              )
            }
          }
        }) ?? null

      const [onSubmitFontVariant] = useSubmitFontVariantFactory(updateNewFontVariant)

      const [selectedOption, setSelectedOption] = React.useState<SelectableItemData | null>(
        valueOption,
      )
      const selectedIndex = React.useMemo<number>(
        () => getOptionIndex(filteredData, selectedOption),
        [filteredData, selectedOption],
      )

      const onStringInputKeyDown = React.useCallback(
        (e: React.KeyboardEvent) => {
          e.stopPropagation()
          switch (e.key) {
            case 'ArrowUp': {
              setSelectedOption(incrementSelectedOption(filteredData, -1))
              break
            }
            case 'ArrowDown': {
              setSelectedOption(incrementSelectedOption(filteredData, 1))
              break
            }
            case 'Enter': {
              if (selectedOption != null) {
                submitNewValue(
                  onSubmitFontVariant,
                  selectedOption,
                  fontWeight,
                  fontStyle,
                  pushNewFontFamilyVariant,
                )
              }
              closePopup()
              break
            }
            case 'Escape': {
              closePopup()
              break
            }
            default:
              if (filteredData.length > 0 && isSelectableItemData(filteredData[0])) {
                setSelectedOption(filteredData[0])
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
        ],
      )

      const getItemSize = React.useCallback(
        (index: number) => filteredData.at(index)?.height ?? 0,
        [filteredData],
      )

      const updateSizes = React.useCallback(() => {
        if (variableSizeListRef.current != null) {
          variableSizeListRef.current.resetAfterIndex(0)
        }
      }, [])

      const onChange = React.useCallback(
        (e: React.ChangeEvent<HTMLInputElement>) => {
          updateSizes()
          const newSearchTerm = e.target.value
          setSearchTerm(newSearchTerm)
        },
        [updateSizes],
      )

      React.useEffect(() => {
        if (variableSizeListRef.current != null && selectedIndex != null) {
          variableSizeListRef.current.scrollToItem(selectedIndex, 'auto')
        }
      }, [selectedIndex])

      React.useEffect(() => {
        if (
          variableSizeListRef.current != null &&
          selectedIndex != null &&
          selectedOption != null
        ) {
          if (selectedOption.metadata.type === 'project-typeface') {
            // try and get the "Project Fonts" header in
            variableSizeListRef.current.scrollToItem(selectedIndex, 'end')
          } else {
            variableSizeListRef.current.scrollToItem(selectedIndex, 'start')
          }
        }
        // we only want to do this on mount
        // eslint-disable-next-line react-hooks/exhaustive-deps
      }, [])

      React.useEffect(() => {
        setTimeout(() => {
          if (stringInputRef.current != null) {
            stringInputRef.current.focus()
          }
        }, 0)
      }, [stringInputRef])

      return (
        <div
          style={{
            background: 'transparent',
            position: 'fixed',
            top: 0,
            left: 0,
            right: 0,
            bottom: 0,
            zIndex: 1,
          }}
          onClick={closePopup}
        >
          <FlexColumn
            ref={wrapperRef}
            tabIndex={0}
            style={{
              ...style,
              backgroundColor: colorTheme.bg2.value,
              width: UtopiaTheme.layout.inspectorSmallPaddedWidth, // TODO should this be resize-aware
              boxShadow: UtopiaStyles.shadowStyles.mid.boxShadow,
              zIndex: 1,
              borderRadius: UtopiaTheme.panelStyles.panelBorderRadius,
              overflow: 'hidden',
            }}
          >
            <FlexRow style={{ padding: 12 }}>
              <StringInput
                testId='font-family-search'
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
                itemsArray: filteredData,
                fontWeight,
                fontStyle,
                closePopup,
                selectedIndex,
                setSelectedOption,
                pushNewFontFamilyVariant,
                updateSizes,
              }}
              width={'100%'}
              height={270}
              itemCount={filteredData.length}
            >
              {FontFamilySelectPopupItem}
            </VariableSizeList>
          </FlexColumn>
        </div>
      )
    },
  ),
)

function incrementSelectedOption(
  filteredData: ItemData[],
  delta: 1 | -1,
): React.SetStateAction<SelectableItemData | null> {
  return (v) => {
    const currentIndex = getOptionIndex(filteredData, v)
    let workingNextSelectedItem: SelectableItemData = filteredData[
      currentIndex
    ] as SelectableItemData
    let possibleNextIndex = currentIndex + delta
    let keepSearching = true
    while (keepSearching) {
      const possibleNextSelectedItem: ItemData = filteredData[possibleNextIndex]
      if (possibleNextSelectedItem != null) {
        if (isSelectableItemData(possibleNextSelectedItem)) {
          workingNextSelectedItem = possibleNextSelectedItem
          keepSearching = false
        }
      } else {
        keepSearching = false
      }
      possibleNextIndex = possibleNextIndex + delta
    }
    return workingNextSelectedItem
  }
}
