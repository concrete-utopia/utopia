/** @jsx jsx */

import React from 'react'
import { jsx } from '@emotion/react'
import styled from '@emotion/styled'

import TailWindList from '../../../core/third-party/tailwind-all-classnames.json'
import WindowedSelect, {
  components,
  createFilter,
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
import * as EP from '../../../core/shared/element-path'
import * as PP from '../../../core/shared/property-path'
import { jsxAttributeValue } from '../../../core/shared/element-template'
import { emptyComments } from '../../../core/workers/parser-printer/parser-printer-comments'

interface TailWindOption {
  label: string
  value: string
  categories?: string[]
}

const TailWindOptions = TailWindList.classNames.map((className, index) => ({
  label: className,
  value: className,
}))

const DropdownIndicator = (props: IndicatorProps<TailWindOption, true>) => (
  <components.DropdownIndicator {...props}>
    <span> ↓ </span>
  </components.DropdownIndicator>
)

const ClearIndicator = () => null
const IndicatorSeparator = () => null

const NoOptionsMessage = (props: any) => <span {...props}>No other classes available</span>

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

const MultiValueContainer = (props: MultiValueProps<TailWindOption>) => {
  const getStriped = (data: TailWindOption) => {
    const categories = (data?.categories as Array<string>) ?? []
    if (categories.length > 0) {
      return categories.map((category, index) => (
        <AngledStripe
          key={data?.label ?? index}
          style={{ backgroundColor: getColorForCategory(category) }}
        />
      ))
    } else {
      return null
    }
  }

  return (
    <div
      style={{
        display: 'flex',
        alignItems: 'center',
        backgroundColor: 'black',
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
        {getStriped(props.data)}
      </div>
    </div>
  )
}

const ValueContainer = (props: ValueContainerProps<TailWindOption, true>) => {
  return (
    <div style={{ overflowX: 'scroll', flex: 1 }}>
      <components.ValueContainer {...props} />
    </div>
  )
}

export const ClassNameSelect: React.FunctionComponent = betterReactMemo('ClassNameSelect', () => {
  const theme = useColorTheme()
  const dispatch = useEditorState((store) => store.dispatch, 'ClassNameSelect dispatch')

  const selectedElement = useEditorState((store) => {
    const metadata = store.editor.jsxMetadata
    if (store.editor.selectedViews.length === 1) {
      return MetadataUtils.findElementByElementPath(metadata, store.editor.selectedViews[0])
    } else {
      return null
    }
  }, 'ClassNameSelect selectedElement')

  const onChange = React.useCallback(
    (newValue: Array<{ label: string; value: string }>) => {
      if (selectedElement != null) {
        dispatch(
          [
            EditorActions.setProp_UNSAFE(
              selectedElement.elementPath,
              PP.create(['className']),
              jsxAttributeValue(newValue.map((value) => value.value).join(' '), emptyComments),
            ),
          ],
          'everyone',
        )
      }
    },
    [dispatch, selectedElement],
  )

  const classNames = selectedElement?.props?.className
  const splitClassNames =
    typeof classNames === 'string'
      ? classNames
          .split(' ')
          .map((s) => s.trim())
          .filter((s) => s !== '')
      : []
  const selectedValues =
    splitClassNames.length === 0
      ? null
      : splitClassNames.map((name: string) => ({
          label: name,
          value: name,
        }))

  const colourStyles: StylesConfig = {
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
      paddingLeft: 4,
      paddingRight: 4,
      paddingTop: 0,
      paddingBottom: 0,
      maxWidth: 0,
    }),

    multiValue: () => {
      return {
        cursor: 'pointer',
        display: 'flex',
        alignItems: 'center',
        height: 18,
        backgroundColor: '#191818',
      }
    },
    multiValueLabel: () => ({
      fontSize: 10,
      padding: '2px 4px',
      color: 'white',
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
        color: 'white',
      },
    }),
    input: () => {
      return {
        fontSize: 11,
        color: theme.fg1.value,
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
      const categories = data?.categories ?? []

      let optionColor = chroma('black')
      if (categories.length === 1) {
        optionColor = chroma(theme.primary.value)
      }

      let backgroundColor: string | undefined = undefined
      if (isFocused) {
        backgroundColor = optionColor.alpha(0.1).css()
      } else if (isSelected) {
        backgroundColor = optionColor.css()
      } else if (isDisabled) {
        backgroundColor = undefined
      }

      let activeBackgroundColor =
        !isDisabled && (isSelected ? data.color : optionColor.alpha(0.3).css())

      let color: string = optionColor.css()
      if (isSelected) {
        if (chroma.contrast(optionColor, 'white') > 2) {
          color = 'white'
        } else {
          color = 'black'
        }
      } else if (isDisabled) {
        color = '#ccc'
      }

      return {
        minHeight: 27,
        display: 'flex',
        alignItems: 'center',
        paddingLeft: 8,
        paddingRight: 8,
        backgroundColor: backgroundColor,
        color: color,
        cursor: isDisabled ? 'not-allowed' : 'default',

        ':active': {
          ...(styles as any)[':active'],
          backgroundColor: activeBackgroundColor,
        },
      }
    },
  }

  return (
    <div
      css={{
        backgroundColor: theme.bg1.value,
        height: 22,
        borderRadius: 3,
        position: 'relative',
        flexGrow: 1,
        display: 'flex',
        alignItems: 'center',
        '&:focus-within': { boxShadow: `0px 0px 0px 1px ${theme.primary.value}` },
      }}
    >
      <WindowedSelect
        filterOption={createFilter({ ignoreAccents: false })}
        options={TailWindOptions}
        onChange={onChange}
        value={selectedValues}
        isMulti={true}
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
