import throttle from 'lodash.throttle'
import React from 'react'
import { memoize } from '../../../../core/shared/memoize'
import { assertNever } from '../../../../core/shared/utils'
import { FlexColumn, FlexRow, Icons, Tooltip, UtopiaStyles, useColorTheme } from '../../../../uuiui'
import type { CartoucheSource, CartoucheUIProps } from './cartouche-ui'
import { CartoucheUI } from './cartouche-ui'
import {
  cartoucheFolderOrInfo,
  childTypeToCartoucheDataType,
  type DataPickerOption,
  type ObjectPath,
} from './data-picker-utils'

export interface DataSelectorSearchProps {
  setSearchTerm: (_: string | null) => void
  applyVariable: (_: ObjectPath) => void
  setNavigatedToPath: (_: ObjectPath) => void
  variableSources: { [valuePathString: string]: CartoucheSource }
  allVariablesInScope: DataPickerOption[]
  searchTerm: string
}

export const DataSelectorSearch = React.memo(
  ({
    setSearchTerm,
    applyVariable,
    setNavigatedToPath,
    searchTerm,
    variableSources,
    allVariablesInScope,
  }: DataSelectorSearchProps) => {
    const colorTheme = useColorTheme()

    const [hoveredSearchRow, setHoveredSearchRow] = React.useState<number>(-1)
    const setHoveredSearchRowCurried = React.useCallback(
      (i: number) => () => setHoveredSearchRow(i),
      [],
    )

    const applySearchResult = React.useCallback(
      (path: ObjectPath) => () => {
        setHoveredSearchRow(-1)
        setSearchTerm(null)
        applyVariable(path)
      },
      [applyVariable, setSearchTerm],
    )

    const onNavigateArrowClick = React.useCallback(
      (path: ObjectPath) => () => {
        setHoveredSearchRow(-1)
        setSearchTerm(null)
        setNavigatedToPath(path)
      },
      [setNavigatedToPath, setSearchTerm],
    )

    return (
      <FlexColumn
        style={{
          overflowX: 'hidden',
          overflowY: 'scroll',
          scrollbarWidth: 'auto',
          scrollbarColor: 'gray transparent',
          flexGrow: 1,
          gap: 8,
          paddingTop: 8,
        }}
      >
        {throttledSearch(allVariablesInScope, searchTerm.toLowerCase())?.map(
          (searchResult, idx) => (
            <FlexRow
              style={{ gap: 8, alignItems: 'center' }}
              key={[...searchResult.valuePath, idx].toString()}
              onMouseEnter={setHoveredSearchRowCurried(idx)}
              onMouseLeave={setHoveredSearchRowCurried(-1)}
            >
              <FlexRow style={{ gap: 2 }}>
                {searchResult.valuePath.map((v, i) => (
                  <CartoucheUI
                    key={`${v.value}-${i}`}
                    datatype={searchResult.cartoucheProps.datatype}
                    selected={false}
                    role={searchResult.cartoucheProps.role}
                    source={
                      variableSources[searchResult.originalValuePath.toString()] ?? 'internal'
                    }
                    testId={`data-selector-primitive-values-${v.value}-${i}`}
                  >
                    <SearchResultString
                      isMatch={v.matched}
                      label={v.value}
                      searchTerm={searchTerm}
                      fontWeightForMatch={900}
                    />
                  </CartoucheUI>
                ))}
              </FlexRow>
              <FlexRow
                style={{
                  opacity: 0.5,
                  ...UtopiaStyles.fontStyles.monospaced,
                }}
              >
                <SearchResultString
                  isMatch={searchResult.value.matched}
                  label={searchResult.value.value}
                  searchTerm={searchTerm}
                  fontWeightForMatch={900}
                />
              </FlexRow>
            </FlexRow>
          ),
        )}
      </FlexColumn>
    )
  },
)

interface SearchResult {
  originalValuePath: ObjectPath
  valuePath: Array<{ value: string; matched: boolean }>
  value: { value: string; matched: boolean }
  cartoucheProps: Pick<CartoucheUIProps, 'role' | 'datatype'>
}

function searchInValuePath(
  valuePath: ObjectPath,
  context: SearchContext,
): { valuePath: SearchResult['valuePath']; matched: boolean } {
  const segments: SearchResult['valuePath'] = []

  let foundMatch = false
  for (const segment of valuePath) {
    const segmentAsString = segment.toString()
    const containsMatch = context.matchesSearchQuery(segmentAsString)
    segments.push({ value: segmentAsString, matched: containsMatch })
    foundMatch ||= containsMatch
  }

  return { valuePath: segments, matched: foundMatch }
}

function searchInValue(value: unknown, context: SearchContext): SearchResult['value'] {
  if (typeof value === 'object' || Array.isArray(value)) {
    return { value: '', matched: false }
  }
  const valueAsString = `${value}`
  return {
    value: valueAsString,
    matched: context.matchesSearchQuery(valueAsString),
  }
}

interface SearchContext {
  matchesSearchQuery: (_: string) => boolean
}

function matches(option: DataPickerOption, context: SearchContext): SearchResult | null {
  const maybeValuePath = searchInValuePath(option.valuePath, context)
  const maybeValue = searchInValue(option.variableInfo.value, context)

  if (maybeValuePath.matched || maybeValue.matched) {
    return {
      originalValuePath: option.valuePath,
      value: maybeValue,
      valuePath: maybeValuePath.valuePath,
      cartoucheProps: {
        role: cartoucheFolderOrInfo(option, 'can-be-folder'),
        datatype: childTypeToCartoucheDataType(option.type),
      },
    }
  }

  return null
}

function search(options: DataPickerOption[], term: string): SearchResult[] {
  if (term.length === 0) {
    return []
  }

  const context: SearchContext = {
    matchesSearchQuery: memoize((text) => text.toLowerCase().includes(term), { maxSize: 25 }),
  }

  let results: SearchResult[] = []

  function walk(option: DataPickerOption) {
    const searchResult = matches(option, context)
    if (searchResult != null) {
      results.push(searchResult)
    }

    switch (option.type) {
      case 'array':
      case 'object':
        option.children.forEach((o) => walk(o))
        break
      case 'jsx':
      case 'primitive':
        break
      default:
        assertNever(option)
    }
  }

  options.forEach((o) => walk(o))

  return results
}

const throttledSearch = throttle(search, 50, {})

function matchedSegments({
  text,
  regexp,
}: {
  text: string
  regexp: RegExp
}): Array<{ text: string; matched: boolean }> {
  const result: Array<{ text: string; matched: boolean }> = []
  let current = 0

  let match
  // https://stackoverflow.com/a/2295681
  while ((match = regexp.exec(text)) != null) {
    result.push({ text: text.slice(current, match.index), matched: false })
    result.push({ text: text.slice(match.index, match.index + match[0].length), matched: true })
    current = match.index + match[0].length
  }
  result.push({ text: text.slice(current), matched: false })

  return result
}

function SearchResultString({
  label,
  isMatch,
  searchTerm,
  fontWeightForMatch,
}: {
  label: string
  isMatch: boolean
  searchTerm: string
  fontWeightForMatch: number
}) {
  const style: React.CSSProperties = {
    ...UtopiaStyles.fontStyles.monospaced,
    fontSize: 10,
  }

  const regexp = React.useMemo(() => new RegExp(searchTerm, 'gi'), [searchTerm])

  if (!isMatch) {
    return <span style={style}>{label}</span>
  }

  const segments = matchedSegments({ text: label, regexp: regexp })
  return (
    <>
      {segments.map(({ text, matched }, idx) => {
        if (text.length === 0) {
          return null
        }
        return (
          <span
            key={`${text}-${idx}`}
            style={{
              ...style,
              fontWeight: matched ? fontWeightForMatch : 200,
              whiteSpace: 'pre',
            }}
          >
            {text}
          </span>
        )
      })}
    </>
  )
}
