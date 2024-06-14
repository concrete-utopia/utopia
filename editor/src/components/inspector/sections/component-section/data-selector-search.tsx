import throttle from 'lodash.throttle'
import React from 'react'
import { memoize } from '../../../../core/shared/memoize'
import { assertNever } from '../../../../core/shared/utils'
import { FlexRow, Icons, SmallerIcons, UtopiaStyles } from '../../../../uuiui'
import { type DataPickerOption, type ObjectPath } from './data-picker-utils'
import { DataPickerCartouche, useVariableDataSource } from './data-selector-cartouche'
import { when } from '../../../../utils/react-conditionals'

export interface DataSelectorSearchProps {
  setNavigatedToPath: (_: DataPickerOption) => void
  allVariablesInScope: DataPickerOption[]
  searchTerm: string
}

export const DataSelectorSearch = React.memo(
  ({ setNavigatedToPath, searchTerm, allVariablesInScope }: DataSelectorSearchProps) => {
    const setNavigatedToPathCurried = React.useCallback(
      (data: DataPickerOption) => (e: React.MouseEvent) => {
        e.stopPropagation()
        e.preventDefault()
        setNavigatedToPath(data)
      },
      [setNavigatedToPath],
    )
    return (
      <div
        style={{
          overflowX: 'hidden',
          overflowY: 'scroll',
          scrollbarWidth: 'auto',
          scrollbarColor: 'gray transparent',
          flexGrow: 1,
          gap: 8,
          paddingTop: 8,
          paddingLeft: 8,
          display: 'grid',
          gridTemplateColumns: 'auto 1fr',
          gridAutoRows: 'min-content',
        }}
      >
        {throttledSearch(allVariablesInScope, searchTerm.toLowerCase())?.map(
          (searchResult, idx) => (
            <React.Fragment key={[...searchResult.valuePath, idx].toString()}>
              <FlexRow style={{ gap: 2 }}>
                <SearchResultCartouche
                  key={`${searchResult.option.variableInfo.expression}-${idx}`}
                  searchResult={searchResult}
                  setNavigatedToPath={setNavigatedToPathCurried(searchResult.option)}
                  searchTerm={searchTerm}
                />
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
            </React.Fragment>
          ),
        )}
      </div>
    )
  },
)

interface SearchResult {
  option: DataPickerOption
  valuePath: Array<{ value: string; matched: boolean }>
  value: { value: string; matched: boolean }
}

function searchInValuePath(
  valuePath: ObjectPath,
  context: SearchContext,
): { valuePath: SearchResult['valuePath']; matched: boolean } {
  const segments: SearchResult['valuePath'] = []

  let foundMatch = false
  for (const segment of valuePath) {
    const segmentAsString = segment.toString()
    if (typeof segment === 'number') {
      segments.push({ value: segmentAsString, matched: false })
    } else {
      const containsMatch = context.matchesSearchQuery(segmentAsString)
      segments.push({ value: segmentAsString, matched: containsMatch })
      foundMatch ||= containsMatch
    }
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
      option: option,
      value: maybeValue,
      valuePath: maybeValuePath.valuePath,
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

function SearchResultCartouche({
  searchResult,
  searchTerm,
  setNavigatedToPath: setNavigatedToPathCurried,
}: {
  searchResult: SearchResult
  searchTerm: string
  setNavigatedToPath: (e: React.MouseEvent) => void
}) {
  const dataSource = useVariableDataSource(searchResult.option)
  const iconColor = dataSource === 'external' ? 'green' : 'primary'

  return (
    <DataPickerCartouche
      data={searchResult.option}
      selected={false}
      forcedSource={dataSource ?? undefined}
      onClick={setNavigatedToPathCurried}
    >
      {searchResult.valuePath.map((v, i) => (
        <React.Fragment key={`${v.value}-${i}`}>
          <SearchResultString
            isMatch={v.matched}
            label={v.value}
            searchTerm={searchTerm}
            fontWeightForMatch={900}
          />
          {when(
            i < searchResult.valuePath.length - 1,
            <SmallerIcons.ExpansionArrowRight color={iconColor} />,
          )}
        </React.Fragment>
      ))}
    </DataPickerCartouche>
  )
}
