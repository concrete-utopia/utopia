import React from 'react'
import { betterReactMemo } from '../../uuiui-deps'
import { stripNulls } from '../shared/array-utils'
import { mapToArray, mapValues } from '../shared/object-utils'
import { NO_OP } from '../shared/utils'
import {
  AllAttributes,
  AttributeToClassNames,
  ClassNameToAttributes,
} from '../third-party/tailwind-defaults'
import Highlighter from 'react-highlight-words'

export interface TailWindOption {
  label: string
  value: string
  attributes?: string[]
  categories?: string[]
}

export let TailWindOptions: Array<TailWindOption> = []
export let AttributeOptionLookup: { [attribute: string]: Array<TailWindOption> }

async function loadTailwindOptions() {
  return new Promise<void>((resolve) => {
    TailWindOptions = mapToArray(
      (attributes, className) => ({
        label: className,
        value: className,
        attributes: attributes,
      }),
      ClassNameToAttributes,
    )

    AttributeOptionLookup = mapValues((classNames: Array<string>) => {
      const matchingOptions = classNames.map((className) =>
        TailWindOptions.find((option) => option.value === className),
      )
      return stripNulls(matchingOptions)
    }, AttributeToClassNames)

    resolve()
  })
}

loadTailwindOptions()

export function searchStringToIndividualTerms(searchString: string): Array<string> {
  return searchString.trim().toLowerCase().split(' ')
}

function findMatchingOptions<T>(
  searchTerms: Array<string>,
  options: Array<T>,
  toString: (t: T) => string,
  maxPerfectMatches: number,
): Array<Array<T>> {
  let orderedMatchedResults: Array<Array<T>> = []
  let perfectMatchCount = 0
  for (var i = 0; i < options.length && perfectMatchCount < maxPerfectMatches; i++) {
    const nextOption = options[i]
    const asString = toString(nextOption)
    const splitInputIndexResult = searchTerms.map((s) => asString.indexOf(s))
    const minimumIndexOf = Math.min(...splitInputIndexResult)
    if (minimumIndexOf > -1) {
      let existingMatched = orderedMatchedResults[minimumIndexOf] ?? []
      existingMatched.push(nextOption)
      orderedMatchedResults[minimumIndexOf] = existingMatched
      if (minimumIndexOf === 0) {
        perfectMatchCount++
      }
    }
  }

  return orderedMatchedResults
}

function takeBestOptions<T>(orderedSparseArray: Array<Array<T>>, maxMatches: number): Set<T> {
  let matchedResults: Set<T> = new Set()
  let matchCount = 0
  for (var i = 0; i < orderedSparseArray.length && matchCount < maxMatches; i++) {
    const nextMatches = orderedSparseArray[i]
    if (nextMatches != null) {
      const maxNextMatches = nextMatches.slice(0, maxMatches - matchCount)
      maxNextMatches.forEach((m) => matchedResults.add(m))
      matchCount = matchedResults.size
    }
  }

  return matchedResults
}

export function useFilteredOptions(
  filter: string,
  maxResults: number,
  onEmptyResults: () => void = NO_OP,
): Array<TailWindOption> {
  return React.useMemo(() => {
    const searchTerms = searchStringToIndividualTerms(filter)
    let results: Array<TailWindOption>

    if (searchTerms.length === 0) {
      results = TailWindOptions.slice(0, maxResults)
    } else {
      // First find all matches, and use a sparse array to keep the best matches at the front
      const orderedMatchedResults = findMatchingOptions(
        searchTerms,
        TailWindOptions,
        (option) => option.label,
        maxResults,
      )

      // Now go through and take the first n best matches
      let matchedResults = takeBestOptions(orderedMatchedResults, maxResults)

      // Next if we haven't hit our max result count, we find matches based on attributes
      const remainingAllowedMatches = maxResults - matchedResults.size
      if (remainingAllowedMatches > 0) {
        const orderedAttributeMatchedResults = findMatchingOptions(
          searchTerms,
          AllAttributes,
          (a) => a,
          remainingAllowedMatches,
        )
        const bestMatchedAttributes = takeBestOptions(
          orderedAttributeMatchedResults,
          remainingAllowedMatches,
        )

        bestMatchedAttributes.forEach((attribute) => {
          const matchingOptions = AttributeOptionLookup[attribute] ?? []
          matchingOptions.forEach((option) => matchedResults.add(option))
        })
      }

      results = Array.from(matchedResults)
    }

    if (results.length === 0) {
      onEmptyResults()
    }

    return results
  }, [filter, maxResults, onEmptyResults])
}

const Bold = betterReactMemo('Bold', ({ children }: { children: React.ReactNode }) => {
  return <strong>{children}</strong>
})

export const MatchHighlighter = betterReactMemo(
  'MatchHighlighter',
  ({ text, searchString }: { text: string; searchString: string | null | undefined }) => {
    const searchTerms = searchStringToIndividualTerms(searchString ?? '')
    return (
      <Highlighter
        highlightTag={Bold}
        searchWords={searchTerms}
        autoEscape={true}
        textToHighlight={text}
      />
    )
  },
)
