import React from 'react'
import { betterReactMemo } from '../../uuiui-deps'
import { flatMapArray, last, stripNulls } from '../shared/array-utils'
import { mapToArray, mapValues } from '../shared/object-utils'
import { NO_OP } from '../shared/utils'
import {
  AllAttributes,
  AttributeToClassNames,
  ClassNameToAttributes,
} from '../third-party/tailwind-defaults'
import Highlighter from 'react-highlight-words'
import { ElementPath, isParseSuccess, isTextFile } from '../shared/project-file-types'
import { useEditorState, useRefEditorState } from '../../components/editor/store/store-hook'
import { getOpenUIJSFileKey } from '../../components/editor/store/editor-state'
import { normalisePathToUnderlyingTarget } from '../../components/custom-code/code-file'
import { getContentsTreeFileFromString } from '../../components/assets'
import {
  isJSXAttributeNotFound,
  isJSXAttributeValue,
  isJSXElement,
  JSXElementChild,
} from '../shared/element-template'
import { findElementAtPath, MetadataUtils } from '../model/element-metadata-utils'
import { getUtopiaJSXComponentsFromSuccess } from '../model/project-file-utils'
import { eitherToMaybe } from '../shared/either'
import { getModifiableJSXAttributeAtPath } from '../shared/jsx-attributes'
import * as PP from '../shared/property-path'

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
  const splitOnSpaces = searchString.trim().toLowerCase().split(' ')
  return flatMapArray((s) => s.split(':'), splitOnSpaces)
}

function findMatchingOptions<T>(
  searchTerms: Array<string>,
  options: Array<T>,
  toStringParts: (t: T) => Array<string>,
  maxPerfectMatches: number,
): Array<Array<T>> {
  let orderedMatchedResults: Array<Array<T>> = []
  let perfectMatchCount = 0
  for (var i = 0; i < options.length && perfectMatchCount < maxPerfectMatches; i++) {
    const nextOption = options[i]
    const stringParts = toStringParts(nextOption)
    const combinatorialQualifiers = stringParts.slice(0, -1) // e.g. sm: hover: etc.
    const combinatorialPartsMatch = combinatorialQualifiers.every((combinatorialQualifier) =>
      searchTerms.some(
        // All combinatorial parts must be at least 50% matched by a search term
        // This feels better than a perfect match
        (searchTerm) =>
          combinatorialQualifier.indexOf(searchTerm) >= 0 &&
          combinatorialQualifier.length <= 2 * searchTerm.length,
      ),
    )
    if (combinatorialPartsMatch) {
      // Only proceed with the match if all combinatorial parts were matched
      const combinedParts = stringParts.join('')
      const splitInputIndexResult = searchTerms.map((s) => combinedParts.indexOf(s))
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
        (option) => option.label.split(':'),
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
          (a) => [a],
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

export function useGetSelectedTailwindOptions(): {
  selectedOptions: Array<TailWindOption> | null
  elementPath: ElementPath | null
  isMenuEnabled: boolean
} {
  const { classNameFromAttributes, elementPath, isMenuEnabled } = useEditorState((store) => {
    const openUIJSFileKey = getOpenUIJSFileKey(store.editor)
    if (openUIJSFileKey == null || store.editor.selectedViews.length !== 1) {
      return {
        elementPath: null,
        classNameFromAttributes: null,
        isMenuEnabled: false,
      }
    }
    const underlyingTarget = normalisePathToUnderlyingTarget(
      store.editor.projectContents,
      store.editor.nodeModules.files,
      openUIJSFileKey,
      store.editor.selectedViews[0],
    )
    const underlyingPath =
      underlyingTarget.type === 'NORMALISE_PATH_SUCCESS'
        ? underlyingTarget.filePath
        : openUIJSFileKey
    const projectFile = getContentsTreeFileFromString(store.editor.projectContents, underlyingPath)
    let element: JSXElementChild | null = null
    if (isTextFile(projectFile) && isParseSuccess(projectFile.fileContents.parsed)) {
      element = findElementAtPath(
        store.editor.selectedViews[0],
        getUtopiaJSXComponentsFromSuccess(projectFile.fileContents.parsed),
      )
    }

    let foundAttributeAsString: string | null = null
    let menuEnabled = false
    if (element != null && isJSXElement(element)) {
      const jsxAttributes = element.props
      let foundAttribute = eitherToMaybe(
        getModifiableJSXAttributeAtPath(jsxAttributes, PP.create(['className'])),
      )
      if (foundAttribute != null && isJSXAttributeValue(foundAttribute)) {
        foundAttributeAsString = foundAttribute.value
      }
      if (
        foundAttribute == null ||
        isJSXAttributeNotFound(foundAttribute) ||
        isJSXAttributeValue(foundAttribute)
      ) {
        menuEnabled = true
      }
    }

    return {
      elementPath:
        MetadataUtils.findElementByElementPath(
          store.editor.jsxMetadata,
          store.editor.selectedViews[0],
        )?.elementPath ?? null,
      classNameFromAttributes: foundAttributeAsString,
      isMenuEnabled: menuEnabled,
    }
  }, 'ClassNameSelect elementPath classNameFromAttributes isMenuEnabled')

  const metadataRef = useRefEditorState((store) => store.editor.jsxMetadata)

  const selectedOptions = React.useMemo((): TailWindOption[] | null => {
    let classNameValue: string | null = null
    if (classNameFromAttributes != null) {
      classNameValue = classNameFromAttributes
    } else {
      if (elementPath != null) {
        const element = MetadataUtils.findElementByElementPath(metadataRef.current, elementPath)
        classNameValue = element?.props['className']
      }
    }

    const splitClassNames =
      typeof classNameValue === 'string'
        ? classNameValue
            .split(' ')
            .map((s) => s.trim())
            .filter((s) => s !== '')
        : []
    return splitClassNames.length === 0
      ? null
      : splitClassNames.map((name: string) => ({
          label: name,
          value: name,
        }))
  }, [classNameFromAttributes, elementPath, metadataRef])

  return {
    selectedOptions: selectedOptions,
    elementPath: elementPath,
    isMenuEnabled: isMenuEnabled,
  }
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
