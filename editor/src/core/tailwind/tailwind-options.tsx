import React from 'react'
import { filterDuplicates, flatMapArray, stripNulls } from '../shared/array-utils'
import { mapToArray, mapValues } from '../shared/object-utils'
import { NO_OP } from '../shared/utils'
import {
  AllAttributes,
  AttributeToClassNames,
  ClassNameToAttributes,
} from '../third-party/tailwind-defaults'
import Highlighter from 'react-highlight-words'
import type { ElementPath } from '../shared/project-file-types'
import {
  Substores,
  useEditorState,
  useRefEditorState,
} from '../../components/editor/store/store-hook'
import { getElementFromProjectContents } from '../../components/editor/store/editor-state'
import type { JSXElementChild } from '../shared/element-template'
import {
  modifiableAttributeIsAttributeNotFound,
  isJSXElement,
  modifiableAttributeIsAttributeValue,
} from '../shared/element-template'
import { eitherToMaybe, flatMapEither, foldEither } from '../shared/either'
import {
  getModifiableJSXAttributeAtPath,
  jsxSimpleAttributeToValue,
} from '../shared/jsx-attribute-utils'
import * as PP from '../shared/property-path'
import * as EP from '../shared/element-path'
import type { AttributeCategory } from './attribute-categories'
import { AttributeCategories } from './attribute-categories'
import { parse } from '@xengine/tailwindcss-class-parser'
import type { Config } from 'tailwindcss'
import { TAILWIND_CONFIG_SPIKE_STATE } from '../../components/navigator/dependency-list'

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

void loadTailwindOptions()

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

export function useFilteredOptions(onEmptyResults: () => void = NO_OP): Array<TailWindOption> {
  return React.useMemo(() => {
    return []
  }, [])
}

export function getClassNameAttribute(element: JSXElementChild | null): {
  value: string | null
  isSettable: boolean
} {
  if (element != null && isJSXElement(element)) {
    const jsxAttributes = element.props
    const foundAttribute = getModifiableJSXAttributeAtPath(jsxAttributes, PP.create('className'))
    const foundAttributeValue = flatMapEither(jsxSimpleAttributeToValue, foundAttribute)
    const isSettable = foldEither(
      () => false,
      (r) => modifiableAttributeIsAttributeValue(r) || modifiableAttributeIsAttributeNotFound(r),
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

function parseSafe(
  classString: string,
  config: Config | null,
): ReturnType<typeof parse> | { kind: 'error'; message: string } {
  try {
    return parse(classString, config ?? undefined)
  } catch (e) {
    return { kind: 'error', message: 'Failed to parse class string' }
  }
}

export function getClassNameMapping(classString: string): { [key: string]: string } {
  const mapping: { [key: string]: string } = {}
  const classParts = classString.split(' ')
  classParts.forEach((part) => {
    const parsed = parseSafe(part, TAILWIND_CONFIG_SPIKE_STATE.current)
    if (parsed.kind === 'error') {
      return
    }
    mapping[parsed.root] = parsed.value
  })
  return mapping
}

export function getStyleMapping(classString: string): { [key: string]: string } {
  const mapping: { [key: string]: string } = {}
  const classParts = classString.split(' ')
  classParts.forEach((part) => {
    const parsed = parseSafe(part, TAILWIND_CONFIG_SPIKE_STATE.current)
    if (parsed.kind === 'error') {
      return
    }
    mapping[parsed.property] = parsed.value
  })
  return mapping
}

export function useGetSelectedClasses(): {
  selectedClasses: Array<string>
  elementPaths: Array<ElementPath>
  isSettable: boolean
} {
  const elements = useEditorState(
    Substores.fullStore,
    (store) =>
      store.editor.selectedViews.map((elementPath) =>
        getElementFromProjectContents(elementPath, store.editor.projectContents),
      ),
    'ClassNameSelect elements',
  )

  const elementPaths = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews,
    'ClassNameSelect elementPaths',
  )

  const allElementPropsRef = useRefEditorState((store) => store.editor.allElementProps)

  const classNamesFromAttributesOrProps = React.useMemo(
    () =>
      elements.map((element, index) => {
        const fromAttributes = getClassNameAttribute(element)
        if (fromAttributes.value != null) {
          return fromAttributes
        } else {
          const elementPath = elementPaths[index]
          return {
            value: allElementPropsRef.current[EP.toString(elementPath)]?.className,
            isSettable: fromAttributes.isSettable,
          }
        }
      }),
    [elements, elementPaths, allElementPropsRef],
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
