/** @jsx jsx */

import React from 'react'
import { jsx } from '@emotion/react'
import styled from '@emotion/styled'

import {
  AllTailwindClasses,
  AllAttributes,
  AttributeToClassNames,
} from '../../../core/third-party/tailwind-defaults'
import WindowedSelect, {
  components,
  IndicatorProps,
  MultiValueProps,
  ValueContainerProps,
} from 'react-windowed-select'
import type { StylesConfig } from 'react-select'
import chroma from 'chroma-js'

import * as EditorActions from '../../editor/actions/action-creators'
import { betterReactMemo } from '../../../uuiui-deps'
import { useColorTheme } from '../../../uuiui'
import { useEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as PP from '../../../core/shared/property-path'
import {
  ElementInstanceMetadata,
  isJSXAttributeNotFound,
  isJSXAttributeValue,
  isJSXElement,
  jsxAttributeValue,
} from '../../../core/shared/element-template'
import { emptyComments } from '../../../core/workers/parser-printer/parser-printer-comments'
import { eitherToMaybe, isRight } from '../../../core/shared/either'
import {
  getModifiableJSXAttributeAtPath,
  ModifiableAttribute,
} from '../../../core/shared/jsx-attributes'
import { stripNulls } from '../../../core/shared/array-utils'

interface TailWindOption {
  label: string
  value: string
  categories?: string[]
}

const TailWindOptions: Array<TailWindOption> = AllTailwindClasses.map((className, index) => ({
  label: className,
  value: className,
}))

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

function findMatchingOptions<T>(
  input: string,
  options: Array<T>,
  toString: (t: T) => string,
  maxPerfectMatches: number,
): Array<Array<T>> {
  let orderedMatchedResults: Array<Array<T>> = []
  let perfectMatchCount = 0
  let splitInput = input.split(' ')
  for (var i = 0; i < options.length && perfectMatchCount < maxPerfectMatches; i++) {
    const nextOption = options[i]
    const asString = toString(nextOption)
    const splitInputIndexResult = splitInput.map((s) => asString.indexOf(s))
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

function mapToClassNames(attributes: Array<string>, maxMatches: number): Array<string> {
  let results: Set<string> = new Set()
  for (var i = 0; i < attributes.length && results.size < maxMatches; i++) {
    const nextAttribute = attributes[i]
    const classNames = AttributeToClassNames[nextAttribute]
    if (classNames != null) {
      classNames.forEach((className) => results.add(className))
    }
  }

  return Array.from(results)
}

export const ClassNameSelect: React.FunctionComponent = betterReactMemo('ClassNameSelect', () => {
  const theme = useColorTheme()
  const dispatch = useEditorState((store) => store.dispatch, 'ClassNameSelect dispatch')
  const [input, setInput] = React.useState('')
  const filteredOptions = React.useMemo(() => {
    const trimmedLowerCaseInput = input.trim().toLowerCase()
    if (trimmedLowerCaseInput === '') {
      return TailWindOptions.slice(0, MaxResults)
    } else {
      // First find all matches, and use a sparse array to keep the best matches at the front
      const orderedMatchedResults = findMatchingOptions(
        trimmedLowerCaseInput,
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
          trimmedLowerCaseInput,
          AllAttributes,
          (a) => a,
          remainingAllowedMatches,
        )
        const bestMatchedAttributes = takeBestOptions(
          orderedAttributeMatchedResults,
          remainingAllowedMatches,
        )
        const classNamesFromBestMatchedAttributes = mapToClassNames(
          bestMatchedAttributes,
          remainingAllowedMatches,
        )
        const optionsForClassNames = classNamesFromBestMatchedAttributes.map((className) =>
          TailWindOptions.find((option) => option.label === className),
        )
        matchedResults.push(...stripNulls(optionsForClassNames))
      }

      return matchedResults
    }
  }, [input])

  const { classNameAttribute, classNameFromProps, elementPath } = useEditorState((store) => {
    let element: ElementInstanceMetadata | null = null
    if (store.editor.selectedViews.length === 1) {
      element = MetadataUtils.findElementByElementPath(
        store.editor.jsxMetadata,
        store.editor.selectedViews[0],
      )
    }

    let foundAttribute: ModifiableAttribute | null = null
    if (element != null && isRight(element.element) && isJSXElement(element.element.value)) {
      const jsxAttributes = element.element.value.props
      foundAttribute = eitherToMaybe(
        getModifiableJSXAttributeAtPath(jsxAttributes, PP.create(['className'])),
      )
    }

    return {
      elementPath: element?.elementPath,
      classNameAttribute: foundAttribute,
      classNameFromProps: element?.props['className'],
    }
  }, 'ClassNameSelect selectedElement')

  const selectedValues = React.useMemo((): TailWindOption[] | null => {
    let classNameValue: string | null = null
    if (classNameAttribute != null && isJSXAttributeValue(classNameAttribute)) {
      classNameValue = classNameAttribute.value
    } else {
      classNameValue = classNameFromProps
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
  }, [classNameAttribute, classNameFromProps])

  const isMenuEnabled = React.useMemo(
    () =>
      classNameAttribute == null ||
      isJSXAttributeValue(classNameAttribute) ||
      isJSXAttributeNotFound(classNameAttribute),
    [classNameAttribute],
  )
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
        filterOption={filterOption}
        options={filteredOptions}
        onChange={onChange}
        onInputChange={setInput}
        value={selectedValues}
        isMulti={true}
        isDisabled={!isMenuEnabled}
        closeMenuOnSelect={false}
        styles={colourStyles}
        components={{
          DropdownIndicator,
          ClearIndicator,
          IndicatorSeparator,
          NoOptionsMessage,
          MultiValueContainer,
          ValueContainer,
        }}
      />
    </div>
  )
})
