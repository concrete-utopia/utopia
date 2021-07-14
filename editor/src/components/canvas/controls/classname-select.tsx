/** @jsx jsx */

import React from 'react'
import { jsx } from '@emotion/react'
import styled from '@emotion/styled'

import {
  AllAttributes,
  AttributeToClassNames,
  ClassNameToAttributes,
} from '../../../core/third-party/tailwind-defaults'
import WindowedSelect, {
  components,
  FormatOptionLabelMeta,
  IndicatorProps,
  MenuProps,
  MultiValueProps,
  ValueContainerProps,
} from 'react-windowed-select'
import type { StylesConfig } from 'react-select'
import chroma from 'chroma-js'
import * as EditorActions from '../../editor/actions/action-creators'
import { betterReactMemo } from '../../../uuiui-deps'
import { FlexColumn, FlexRow, useColorTheme } from '../../../uuiui'
import { useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { findElementAtPath, MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as PP from '../../../core/shared/property-path'
import {
  ElementInstanceMetadata,
  isJSXAttributeNotFound,
  isJSXAttributeValue,
  isJSXElement,
  jsxAttributeValue,
  JSXElementChild,
} from '../../../core/shared/element-template'
import { emptyComments } from '../../../core/workers/parser-printer/parser-printer-comments'
import { eitherToMaybe, isRight } from '../../../core/shared/either'
import {
  getModifiableJSXAttributeAtPath,
  ModifiableAttribute,
} from '../../../core/shared/jsx-attributes'
import {
  atomWithPubSub,
  usePubSubAtomReadOnly,
  usePubSubAtomWriteOnly,
} from '../../../core/shared/atom-with-pub-sub'
import { stripNulls } from '../../../core/shared/array-utils'
import { mapToArray, mapValues } from '../../../core/shared/object-utils'
import { getOpenUIJSFileKey } from '../../editor/store/editor-state'
import { normalisePathToUnderlyingTarget } from '../../custom-code/code-file'
import { getContentsTreeFileFromString } from '../../assets'
import { isParseSuccess, isTextFile } from '../../../core/shared/project-file-types'
import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
import Highlighter from 'react-highlight-words'

interface TailWindOption {
  label: string
  value: string
  attributes?: string[]
  categories?: string[]
}

let TailWindOptions: Array<TailWindOption> = []
let AttributeOptionLookup: { [attribute: string]: Array<TailWindOption> }

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

const DropdownIndicator = betterReactMemo(
  'DropdownIndicator',
  (props: IndicatorProps<TailWindOption, true>) => (
    <components.DropdownIndicator {...props}>
      <span style={{ lineHeight: '20px', opacity: props.isDisabled ? 0 : 1 }}> ↓ </span>
    </components.DropdownIndicator>
  ),
)

const ClearIndicator = () => null
const IndicatorSeparator = () => null

const NoOptionsMessage = betterReactMemo('NoOptionsMessage', (props: any) => (
  <span {...props}>No results found</span>
))

const AngledStripe = styled.div({
  width: 5,
  height: 22,
  borderRadius: 0,
  transform: 'skewX(-11deg)',
})

const getColorForCategory = (category: string) => {
  if (category === 'layout') {
    return '#5FACFF'
  } else if (category === 'typography') {
    return '#F7B500'
  } else if (category === 'interaction') {
    return '#B620E0'
  } else return 'pink'
}

interface OptionAndSelectedColor {
  primary: {
    optionColor: chroma.Color
    selectedColor: string
  }
  regular: {
    optionColor: chroma.Color
    selectedColor: string
  }
}

const getOptionColors = (
  optionAndSelectedColor: OptionAndSelectedColor,
  isFocused: boolean,
  isSelected: boolean,
  isDisabled: boolean,
  data: any,
) => {
  const categories = data?.categories ?? []
  let optionColor = optionAndSelectedColor.regular.optionColor
  let selectedColor = optionAndSelectedColor.regular.selectedColor
  if (categories.length === 1) {
    optionColor = optionAndSelectedColor.primary.optionColor
    selectedColor = optionAndSelectedColor.primary.selectedColor
  }

  let color: string = optionColor.css()
  let backgroundColor: string | undefined = undefined
  let activeBackgroundColor: string | undefined = optionColor.alpha(0.3).css()
  if (isFocused) {
    backgroundColor = optionColor.alpha(0.1).css()
  } else if (isSelected) {
    backgroundColor = optionColor.css()
    color = selectedColor
    activeBackgroundColor = data.color
  } else if (isDisabled) {
    backgroundColor = undefined
    activeBackgroundColor = undefined
    color = '#ccc'
  }

  return {
    color: color,
    backgroundColor: backgroundColor,
    activeBackgroundColor: activeBackgroundColor,
  }
}

const focusedOptionAtom = atomWithPubSub<TailWindOption | null>({
  key: 'classNameSelectFocusedOption',
  defaultValue: null,
})

const Bold = betterReactMemo('Bold', ({ children }: { children: React.ReactNode }) => {
  return <strong>{children}</strong>
})

const MatchHighlighter = betterReactMemo(
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

function formatOptionLabel(
  { label }: TailWindOption,
  { context, inputValue }: FormatOptionLabelMeta<TailWindOption, true>,
) {
  return context === 'menu' ? <MatchHighlighter text={label} searchString={inputValue} /> : label
}

const Menu = betterReactMemo('Menu', (props: MenuProps<TailWindOption, true>) => {
  const theme = useColorTheme()
  const focusedOption = usePubSubAtomReadOnly(focusedOptionAtom)
  const showFooter = props.options.length > 0
  const joinedAttributes = focusedOption?.attributes?.join(', ')
  const attributesText =
    joinedAttributes == null || joinedAttributes === '' ? '\u00a0' : `Sets: ${joinedAttributes}`

  return (
    <components.Menu {...props}>
      <React.Fragment>
        {props.children}
        {showFooter ? (
          <div
            css={{
              label: 'focusedElementMetadata',
              overflow: 'hidden',
              boxShadow: 'inset 0px 1px 0px 0px rgba(0,0,0,.1)',
              padding: '8px 8px',
              fontSize: '10px',
              pointerEvents: 'none',
              color: theme.textColor.value,
            }}
          >
            <FlexColumn>
              <FlexRow>
                <span>
                  <MatchHighlighter
                    text={attributesText}
                    searchString={props.selectProps.inputValue}
                  />
                </span>
              </FlexRow>
            </FlexColumn>
          </div>
        ) : null}
      </React.Fragment>
    </components.Menu>
  )
})

const MultiValueContainer = betterReactMemo(
  'MultiValueContainer',
  (props: MultiValueProps<TailWindOption>) => {
    const theme = useColorTheme()
    const { data } = props
    const stripes: jsx.JSX.Element[] = React.useMemo(() => {
      const categories = data.categories ?? []
      if (categories.length > 0) {
        return categories.map((category, index) => (
          <AngledStripe
            key={data.label ?? index}
            style={{ backgroundColor: getColorForCategory(category) }}
          />
        ))
      } else {
        return []
      }
    }, [data.label, data.categories])

    return (
      <div
        style={{
          display: 'flex',
          alignItems: 'center',
          backgroundColor: theme.inverted.bg1.value,
        }}
      >
        <components.MultiValueContainer {...props} />
        <div
          style={{
            display: 'flex',
            height: 16,
            paddingRight: 4,
            paddingLeft: 2,
          }}
        >
          {stripes}
        </div>
      </div>
    )
  },
)

const ValueContainer = betterReactMemo(
  'ValueContainer',
  (props: ValueContainerProps<TailWindOption, true>) => {
    return (
      <div style={{ overflowX: 'scroll', flex: 1 }}>
        <components.ValueContainer {...props} />
      </div>
    )
  },
)

const filterOption = () => true
const MaxResults = 500

function searchStringToIndividualTerms(searchString: string): Array<string> {
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

function takeBestOptions<T>(orderedSparseArray: Array<Array<T>>, maxMatches: number): Array<T> {
  let matchedResults: Array<T> = []
  let matchCount = 0
  for (var i = 0; i < orderedSparseArray.length && matchCount < maxMatches; i++) {
    const nextMatches = orderedSparseArray[i]
    if (nextMatches != null) {
      matchedResults.push(...nextMatches.slice(0, maxMatches - matchCount))
      matchCount += nextMatches.length
    }
  }

  return matchedResults
}

export const ClassNameSelect: React.FunctionComponent = betterReactMemo('ClassNameSelect', () => {
  const theme = useColorTheme()
  const targets = useEditorState((store) => store.editor.selectedViews, 'ClassNameSelect targets')
  const dispatch = useEditorState((store) => store.dispatch, 'ClassNameSelect dispatch')
  const [input, setInput] = React.useState('')
  const updateFocusedOption = usePubSubAtomWriteOnly(focusedOptionAtom)
  const clearFocusedOption = React.useCallback(() => {
    updateFocusedOption(null)
    dispatch([EditorActions.clearTransientProps()], 'canvas')
  }, [updateFocusedOption, dispatch])

  const filteredOptions = React.useMemo(() => {
    const searchTerms = searchStringToIndividualTerms(input)
    let results: Array<TailWindOption>

    if (searchTerms.length === 0) {
      results = TailWindOptions.slice(0, MaxResults)
    } else {
      // First find all matches, and use a sparse array to keep the best matches at the front
      const orderedMatchedResults = findMatchingOptions(
        searchTerms,
        TailWindOptions,
        (option) => option.label,
        MaxResults,
      )

      // Now go through and take the first n best matches
      let matchedResults = takeBestOptions(orderedMatchedResults, MaxResults)

      // Next if we haven't hit our max result count, we find matches based on attributes
      const remainingAllowedMatches = MaxResults - matchedResults.length
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
        const optionsForBestMatchedAttributes = bestMatchedAttributes.flatMap(
          (attribute) => AttributeOptionLookup[attribute] ?? [],
        )
        matchedResults.push(...optionsForBestMatchedAttributes)
      }

      results = matchedResults
    }

    if (results.length === 0) {
      clearFocusedOption()
    }

    return results
  }, [input, clearFocusedOption])

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
      elementPath: MetadataUtils.findElementByElementPath(
        store.editor.jsxMetadata,
        store.editor.selectedViews[0],
      )?.elementPath,
      classNameFromAttributes: foundAttributeAsString,
      isMenuEnabled: menuEnabled,
    }
  }, 'ClassNameSelect elementPath classNameFromAttributes isMenuEnabled')

  const selectedValues = React.useMemo((): TailWindOption[] | null => {
    let classNameValue: string | null = null
    if (classNameFromAttributes != null) {
      classNameValue = classNameFromAttributes
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
  }, [classNameFromAttributes])

  const ariaOnFocus = React.useCallback(
    ({ focused }: { focused: TailWindOption }) => {
      if (targets.length === 1) {
        const newClassNameString =
          selectedValues?.map((v) => v.label).join(' ') + ' ' + focused.label
        dispatch(
          [
            EditorActions.setPropTransient(
              targets[0],
              PP.create(['className']),
              jsxAttributeValue(newClassNameString, emptyComments),
            ),
          ],
          'canvas',
        )
      }
      updateFocusedOption(focused)
    },
    [updateFocusedOption, dispatch, targets, selectedValues],
  )
  const ariaLiveMessages = React.useMemo(() => ({ onFocus: ariaOnFocus }), [ariaOnFocus])

  const onChange = React.useCallback(
    (newValue: Array<{ label: string; value: string }>) => {
      if (elementPath != null) {
        dispatch(
          [
            EditorActions.setProp_UNSAFE(
              elementPath,
              PP.create(['className']),
              jsxAttributeValue(newValue.map((value) => value.value).join(' '), emptyComments),
            ),
          ],
          'everyone',
        )
      }
    },
    [dispatch, elementPath],
  )

  const optionAndSelectedColor: OptionAndSelectedColor = React.useMemo(() => {
    const themePrimary = chroma(theme.primary.value)
    return {
      primary: {
        optionColor: themePrimary,
        selectedColor: chroma.contrast(themePrimary, 'white') > 2 ? 'white' : 'black',
      },
      regular: {
        optionColor: chroma('black'),
        selectedColor: 'white',
      },
    }
  }, [theme.primary.value])
  const colourStyles: StylesConfig = React.useMemo(
    () => ({
      container: (styles: React.CSSProperties) => ({
        // the outermost element. It contains the popup menu, so don't set a height on it!
        // shouldn't contain any sizing
        ...styles,
        width: '100%',
      }),
      control: (styles) => ({
        // need to remove styles here, since that implicitly sets a height of 38
        // ...styles,
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'space-between',
        background: 'transparent',
        outline: 'none',
        ':focus-within': {
          outline: 'none',
          border: 'none',
        },
      }),
      valueContainer: () => ({
        // the container for added options (tags) and input
        // sibling to indicatorsContainer
        // default styles mess with layout, so ignore them
        display: 'flex',
        alignItems: 'center',
        gap: 4,
        maxWidth: 0,
      }),
      multiValue: () => {
        return {
          cursor: 'pointer',
          display: 'flex',
          alignItems: 'center',
          height: 18,
          backgroundColor: theme.inverted.bg1.value,
        }
      },
      multiValueLabel: () => ({
        fontSize: 10,
        padding: '2px 4px',
        color: theme.inverted.textColor.value,
      }),
      multiValueRemove: (styles: React.CSSProperties, { data }) => ({
        width: 11,
        display: 'flex',
        paddingTop: 2,
        opacity: 0.4,
        color: data.color,
        ':hover': {
          opacity: 1,
          backgroundColor: data.color,
          color: theme.inverted.textColor.value,
        },
        '& > svg': {
          overflow: 'hidden',
        },
      }),
      input: () => {
        return {
          fontSize: 11,
          color: theme.inverted.textColor.value,
          letterSpacing: 0.3,
          background: 'transparent',
          display: 'flex',
          alignItems: 'center',
        }
      },
      indicatorsContainer: (styles) => ({
        ...styles,
        height: 20,
      }),
      option: (styles: React.CSSProperties, { data, isDisabled, isFocused, isSelected }) => {
        // a single entry in the options list
        const optionColors = getOptionColors(
          optionAndSelectedColor,
          isFocused,
          isSelected,
          isDisabled,
          data,
        )
        return {
          minHeight: 27,
          display: 'flex',
          alignItems: 'center',
          paddingLeft: 8,
          paddingRight: 8,
          backgroundColor: optionColors.backgroundColor,
          color: optionColors.color,
          cursor: isDisabled ? 'not-allowed' : 'default',

          ':active': {
            ...(styles as any)[':active'],
            backgroundColor: optionColors.activeBackgroundColor,
          },
        }
      },
    }),
    [theme, optionAndSelectedColor],
  )

  return (
    <div
      css={{
        height: 22,
        borderRadius: 3,
        position: 'relative',
        padding: 4,
        flexGrow: 1,
        display: 'flex',
        alignItems: 'center',
        '&:focus-within': { boxShadow: `0px 0px 0px 1px ${theme.primary.value}` },
      }}
    >
      <WindowedSelect
        ariaLiveMessages={ariaLiveMessages}
        autoFocus={true}
        filterOption={filterOption}
        formatOptionLabel={formatOptionLabel}
        options={filteredOptions}
        onChange={onChange}
        onInputChange={setInput}
        onMenuClose={clearFocusedOption}
        value={selectedValues}
        isMulti={true}
        isDisabled={!isMenuEnabled}
        openMenuOnFocus={true}
        closeMenuOnSelect={false}
        styles={colourStyles}
        components={{
          DropdownIndicator,
          ClearIndicator,
          IndicatorSeparator,
          NoOptionsMessage,
          Menu,
          MultiValueContainer,
          ValueContainer,
        }}
      />
    </div>
  )
})
