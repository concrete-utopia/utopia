/** @jsx jsx */

import React from 'react'
import { jsx } from '@emotion/react'

import TailWindList from '../../../core/third-party/tailwind-all-classnames.json'
import WindowedSelect, { components, createFilter } from 'react-windowed-select'
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

const TailWindOptions = TailWindList.classNames.map((className, index) => ({
  label: className,
  value: className,
}))

const DropdownIndicator = (props: any) => (
  <components.DropdownIndicator {...props}>
    <span> ↓ </span>
  </components.DropdownIndicator>
)

const ClearIndicator = () => null
const IndicatorSeparator = () => null

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
  const selectedValues =
    classNames == null || classNames.length === 0
      ? null
      : classNames.split(' ').map((name: string) => ({
          label: name,
          value: name,
        }))

  const colourStyles: StylesConfig = {
    container: (styles: React.CSSProperties) => ({
      // the outermost element. It contains the popup menu,  so don't set a height on it!
      // shouldn't contain any sizing
      // backgroundColor: "yellow",
      ...styles,
      width: '100%',
    }),
    control: (styles) => ({
      // need to remove styles here, since that implicitly sets a height of 38
      // ...styles,
      display: 'flex',
      // background: "rgb(0,255,0,.1)",
      alignItems: 'center',
      justifyContent: 'space-between',
      background: 'transparent',
      outline: 'none',
      ':focus-within': {
        // backgroundColor: "rgb(0,255,0,.2)",
        outline: 'none',
        border: 'none',
      },
    }),
    valueContainer: () => ({
      // the container for added options (tags) and input
      // sibling to indicatorsContainer
      // default styles mess with layout, so ignore them
      // border: '1px solid green',
      overflowX: 'auto',
      display: 'flex',
      alignItems: 'center',
      gap: 4,
      // height: 22,
      paddingLeft: 4,
      paddingRight: 4,
      paddingTop: 0,
      paddingBottom: 0,
    }),

    multiValue: () => {
      return {
        // ...styles,
        cursor: 'pointer',
        display: 'flex',
        alignItems: 'center',
        // backgroundColor: color.css()
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
        color: 'white',
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

      let color = chroma('black')
      if (categories.length === 1) {
        color = chroma('#007aff')
      }

      // const color = chroma(data.color);
      return {
        minHeight: 27,
        display: 'flex',
        alignItems: 'center',
        paddingLeft: 8,
        paddingRight: 8,
        backgroundColor: isDisabled
          ? undefined
          : isSelected
          ? color.css()
          : isFocused
          ? color.alpha(0.1).css()
          : undefined,
        color: isDisabled
          ? '#ccc'
          : isSelected
          ? chroma.contrast(color, 'white') > 2
            ? 'white'
            : 'black'
          : color.css(),
        cursor: isDisabled ? 'not-allowed' : 'default',

        ':active': {
          ...(styles as any)[':active'],
          backgroundColor: !isDisabled && (isSelected ? data.color : color.alpha(0.3).css()),
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
        '&:focus-within': { boxShadow: '0px 0px 0px 1px #007aff' },
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
        }}
      />
    </div>
  )
})
