import React from 'react'
import { filterDuplicates, flatMapArray, last, stripNulls } from '../shared/array-utils'
import { mapToArray, mapValues } from '../shared/object-utils'
import { NO_OP } from '../shared/utils'
import {
  AllAttributes,
  AttributeToClassNames,
  ClassNameToAttributes,
} from '../third-party/tailwind-defaults'
import Highlighter from 'react-highlight-words'
import { ElementPath, isParseSuccess, isTextFile, NodeModules } from '../shared/project-file-types'
import { useEditorState, useRefEditorState } from '../../components/editor/store/store-hook'
import { getOpenUIJSFileKey } from '../../components/editor/store/editor-state'
import { normalisePathToUnderlyingTarget } from '../../components/custom-code/code-file'
import { getContentsTreeFileFromString, ProjectContentTreeRoot } from '../../components/assets'
import {
  isJSXAttributeNotFound,
  isJSXAttributeValue,
  isJSXElement,
  JSXElementChild,
} from '../shared/element-template'
import { findElementAtPath, MetadataUtils } from '../model/element-metadata-utils'
import { getUtopiaJSXComponentsFromSuccess } from '../model/project-file-utils'
import { eitherToMaybe, flatMapEither, foldEither, mapEither } from '../shared/either'
import {
  getModifiableJSXAttributeAtPath,
  jsxSimpleAttributeToValue,
} from '../shared/jsx-attributes'
import * as PP from '../shared/property-path'
import * as EP from '../shared/element-path'
import { isTwindEnabled } from './tailwind'
import { AttributeCategories, AttributeCategory } from './attribute-categories'

export interface TailWindOption {
  label: string
  value: string
  attributes?: Array<string>
  categories?: Array<AttributeCategory>
}

export let TailWindOptions: Array<TailWindOption> = []
let TailWindOptionLookup: { [className: string]: TailWindOption } = {}
export let AttributeOptionLookup: { [attribute: string]: Array<TailWindOption> } = {}

async function loadTailwindOptions() {
  return new Promise<void>((resolve) => {
    TailWindOptions = mapToArray((attributes, className) => {
      const categories = filterDuplicates(
        flatMapArray((attribute) => AttributeCategories[attribute] ?? [], attributes),
      )

      const option = {
        label: className,
        value: className,
        attributes: attributes,
        categories: categories,
      }

      TailWindOptionLookup[className] = option

      return option
    }, ClassNameToAttributes)

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

export function getTailwindOptionForClassName(className: string): TailWindOption {
  const foundOption = TailWindOptionLookup[className]
  return (
    foundOption ?? {
      label: className,
      value: className,
    }
  )
}

function searchStringToIndividualTerms(sanitisedFilter: string): Array<string> {
  return sanitisedFilter.split(/\W/).filter((s) => s != '')
}

function findMatchingOptions<T>(
  filter: string,
  searchTerms: Array<string>,
  options: Array<T>,
  toFullString: (t: T) => string,
  toStringParts: (t: T) => Array<string>,
  maxPerfectMatches: number,
): Array<Array<T>> {
  let orderedFullTermMatches: Array<Array<T>> = []
  let orderedIndividualTermMatches: Array<Array<T>> = []
  let perfectMatchCount = 0
  for (var i = 0; i < options.length && perfectMatchCount < maxPerfectMatches; i++) {
    const nextOption = options[i]
    const fullString = toFullString(nextOption)
    const stringParts = toStringParts(nextOption)
    const combinatorialQualifiers = stringParts.slice(0, -1) // e.g. sm: hover: etc.
    const combinatorialPartsMatch = combinatorialQualifiers.every((combinatorialQualifier) =>
      searchTerms.some(
        // All combinatorial parts must be at least 50% matched by a search term
        // This feels better than a perfect match
        (searchTerm) =>
          combinatorialQualifier.startsWith(searchTerm) &&
          combinatorialQualifier.length <= 2 * searchTerm.length,
      ),
    )
    if (combinatorialPartsMatch) {
      // Only proceed with the match if all combinatorial parts were matched
      const fullFilterIndexResult = fullString.indexOf(filter)
      if (fullFilterIndexResult > -1) {
        // Attempt a match against the full string first
        let existingMatched = orderedFullTermMatches[fullFilterIndexResult] ?? []
        existingMatched.push(nextOption)
        orderedFullTermMatches[fullFilterIndexResult] = existingMatched // Prioritise full string matches
        if (fullFilterIndexResult === 0) {
          perfectMatchCount++
        }
      } else {
        const splitInputIndexResult = searchTerms.map((s) => fullString.indexOf(s))
        const minimumIndexOf = Math.min(...splitInputIndexResult)
        if (minimumIndexOf > -1) {
          let existingMatched = orderedIndividualTermMatches[minimumIndexOf] ?? []
          existingMatched.push(nextOption)
          orderedIndividualTermMatches[minimumIndexOf] = existingMatched
          if (minimumIndexOf === 0) {
            perfectMatchCount++
          }
        }
      }
    }
  }

  // Combine the matches, giving priority to the full term matches
  return [...orderedFullTermMatches, ...orderedIndividualTermMatches]
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
    if (isTwindEnabled()) {
      const sanitisedFilter = filter.trim().toLowerCase()
      const searchTerms = searchStringToIndividualTerms(sanitisedFilter)
      let results: Array<TailWindOption>

      if (searchTerms.length === 0) {
        results = TailWindOptions.slice(0, maxResults)
      } else {
        // First find all matches, and use a sparse array to keep the best matches at the front
        const orderedMatchedResults = findMatchingOptions(
          sanitisedFilter,
          searchTerms,
          TailWindOptions,
          (option) => option.label,
          (option) => option.label.split(':'),
          maxResults,
        )

        // Now go through and take the first n best matches
        let matchedResults = takeBestOptions(orderedMatchedResults, maxResults)

        // Next if we haven't hit our max result count, we find matches based on attributes
        const remainingAllowedMatches = maxResults - matchedResults.size
        if (remainingAllowedMatches > 0) {
          const orderedAttributeMatchedResults = findMatchingOptions(
            sanitisedFilter,
            searchTerms,
            AllAttributes,
            (a) => a,
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
    } else {
      return []
    }
  }, [filter, maxResults, onEmptyResults])
}

function getJSXElementForTarget(
  target: ElementPath,
  openUIJSFileKey: string,
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
): JSXElementChild | null {
  const underlyingTarget = normalisePathToUnderlyingTarget(
    projectContents,
    nodeModules,
    openUIJSFileKey,
    target,
  )
  const underlyingPath =
    underlyingTarget.type === 'NORMALISE_PATH_SUCCESS' ? underlyingTarget.filePath : openUIJSFileKey
  const projectFile = getContentsTreeFileFromString(projectContents, underlyingPath)
  if (isTextFile(projectFile) && isParseSuccess(projectFile.fileContents.parsed)) {
    return findElementAtPath(
      target,
      getUtopiaJSXComponentsFromSuccess(projectFile.fileContents.parsed),
    )
  } else {
    return null
  }
}

function getClassNameAttribute(element: JSXElementChild | null): {
  value: string | null
  isSettable: boolean
} {
  if (element != null && isJSXElement(element)) {
    const jsxAttributes = element.props
    const foundAttribute = getModifiableJSXAttributeAtPath(jsxAttributes, PP.create(['className']))
    const foundAttributeValue = flatMapEither(jsxSimpleAttributeToValue, foundAttribute)
    const isSettable = foldEither(
      () => false,
      (r) => isJSXAttributeValue(r) || isJSXAttributeNotFound(r),
      foundAttribute,
    )

    return {
      value: eitherToMaybe(foundAttributeValue),
      isSettable: isSettable,
    }
  } else {
    return {
      value: null,
      isSettable: true,
    }
  }
}

export function useGetSelectedClasses(): {
  selectedClasses: Array<string>
  elementPaths: Array<ElementPath>
  isSettable: boolean
} {
  const metadataRef = useRefEditorState((store) => store.editor.jsxMetadata)
  const elements = useEditorState((store) => {
    const openUIJSFileKey = getOpenUIJSFileKey(store.editor)
    if (openUIJSFileKey == null) {
      return []
    } else {
      return store.editor.selectedViews.map((elementPath) =>
        getJSXElementForTarget(
          elementPath,
          openUIJSFileKey,
          store.editor.projectContents,
          store.editor.nodeModules.files,
        ),
      )
    }
  }, 'ClassNameSelect elements')

  const elementPaths = useEditorState(
    (store) => store.editor.selectedViews,
    'ClassNameSelect elementPaths',
  )

  const allElementProps = useEditorState(
    (store) => store.editor.allElementProps,
    'ClassNameSelect allElementProps',
    (oldProps, newProps) => oldProps === newProps,
  )

  const classNamesFromAttributesOrProps = React.useMemo(
    () =>
      elements.map((element, index) => {
        const fromAttributes = getClassNameAttribute(element)
        if (fromAttributes.value != null) {
          return fromAttributes
        } else {
          const elementPath = elementPaths[index]
          const elementMetadata = MetadataUtils.findElementByElementPath(
            metadataRef.current,
            elementPath,
          )
          return {
            value: allElementProps[EP.toString(elementPath)]?.className,
            isSettable: fromAttributes.isSettable,
          }
        }
      }),
    [elements, elementPaths, metadataRef, allElementProps],
  )

  const isSettable =
    elements.length > 0 && classNamesFromAttributesOrProps.every((v) => v.isSettable)

  const allClassNames = classNamesFromAttributesOrProps.reduce((working, next) => {
    const splitClassNames =
      typeof next.value === 'string'
        ? next.value
            .split(' ')
            .map((s) => s.trim())
            .filter((s) => s !== '')
        : []
    splitClassNames.forEach((className) => working.add(className))
    return working
  }, new Set<string>())

  const selectedClasses = Array.from(allClassNames)

  return {
    elementPaths: elementPaths,
    selectedClasses: selectedClasses,
    isSettable: isSettable,
  }
}

const Bold = React.memo(({ children }: { children: React.ReactNode }) => {
  return <strong>{children}</strong>
})

export const MatchHighlighter = React.memo(
  ({ text, searchString }: { text: string; searchString: string | null | undefined }) => {
    const sanitisedFilter = searchString?.trim().toLowerCase() ?? ''
    const individualTerms = searchStringToIndividualTerms(sanitisedFilter)
    const searchTerms = [sanitisedFilter, ...individualTerms]
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

const getColorForCategory = (category: AttributeCategory): string => {
  switch (category) {
    case 'animation':
      return '#B620E0'
    case 'aural':
      return '#007AFF'
    case 'background':
      return '#574BE2'
    case 'border':
      return '#FD003B'
    case 'container':
      return '#FA5E00'
    case 'esoteric':
      return 'hsl(120, 100%, 37%)'
    case 'layout-self':
      return '#5FACFF'
    case 'layout-system':
      return '#FCFF42'
    case 'meta':
      return 'hsl(0, 0%, 50%)'
    case 'transform':
      return '#D05300'
    case 'typographic':
      return '#F7B500'
    case 'shadow':
      return '#FF00FF'

    default:
      const _exhaustive: never = category
      throw new Error(`Unknown category ${category}`)
  }
}

const AngledStripe = React.memo((props: { category: AttributeCategory }) => {
  return (
    <div
      style={{
        width: 5,
        height: 22,
        borderRadius: 0,
        transform: 'translateY(-2px) skewX(-11deg)',
        backgroundColor: getColorForCategory(props.category),
      }}
    />
  )
})

export const LabelWithStripes = React.memo(
  (props: { label: string; categories: Array<AttributeCategory> }) => {
    const { label, categories } = props

    const stripes: Array<React.ReactNode> = React.useMemo(() => {
      if (categories.length > 0) {
        return categories.map((category, index) => (
          <AngledStripe key={label ?? index} category={category} />
        ))
      } else {
        return []
      }
    }, [label, categories])

    return (
      <div
        style={{
          display: 'flex',
          alignItems: 'center',
        }}
      >
        {label}
        <div
          style={{
            display: 'flex',
            height: 16,
            paddingRight: 4,
            paddingLeft: 4,
          }}
        >
          {stripes}
        </div>
      </div>
    )
  },
)
