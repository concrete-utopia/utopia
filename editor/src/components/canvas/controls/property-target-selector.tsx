import React from 'react'
import { useContextSelector } from 'use-context-selector'
import { LayoutTargetableProp, StyleLayoutProp } from '../../../core/layout/layout-helpers-new'
import { getSimpleAttributeAtPath } from '../../../core/model/element-metadata-utils'
import { eitherToMaybe, left } from '../../../core/shared/either'
import { ElementInstanceMetadata } from '../../../core/shared/element-template'
import { LayoutTargetablePropArrayKeepDeepEquality } from '../../../utils/deep-equality-instances'
import { useColorTheme } from '../../../uuiui'
import {
  decrementResizeOptionsSelectedIndex,
  incrementResizeOptionsSelectedIndex,
  setResizeOptionsTargetOptions,
} from '../../editor/actions/action-creators'
import { usePrevious } from '../../editor/hook-utils'
import { useEditorState } from '../../editor/store/store-hook'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'

interface PropertyTargetSelectorProps {
  targetComponentMetadata: ElementInstanceMetadata | null
  top: number
  left: number
  options: Array<LayoutTargetableProp>
}

export const PropertyTargetSelector = React.memo(
  (props: PropertyTargetSelectorProps): JSX.Element => {
    const colorTheme = useColorTheme()
    const { resizeOptions, dispatch } = useEditorState(
      React.useCallback((editorState) => {
        return {
          resizeOptions: editorState.editor.canvas.resizeOptions,
          dispatch: editorState.dispatch,
        }
      }, []),
      'PropertyTargetSelector resizeOptions',
    )

    const onKeyDown = React.useCallback(
      (event: KeyboardEvent) => {
        if (event.key === 'Tab') {
          event.preventDefault()
          event.stopPropagation()
          const action = event.shiftKey
            ? decrementResizeOptionsSelectedIndex()
            : incrementResizeOptionsSelectedIndex()
          dispatch([action], 'canvas')
        }
      },
      [dispatch],
    )

    React.useEffect(() => {
      window.addEventListener('keydown', onKeyDown, true)
      return function cleanup() {
        window.removeEventListener('keydown', onKeyDown, true)
      }
    }, [dispatch, onKeyDown])

    const valuesForProp = React.useMemo(() => {
      return resizeOptions.propertyTargetOptions.map((option) =>
        eitherToMaybe(
          getSimpleAttributeAtPath(
            left(props.targetComponentMetadata?.props ?? {}),
            stylePropPathMappingFn(option, ['style']),
          ),
        ),
      )
    }, [resizeOptions.propertyTargetOptions, props.targetComponentMetadata?.props])

    const defaultSelectedOptionIndex = React.useMemo(() => {
      const indexOfFirstWithValue = valuesForProp.findIndex((value) => value != null)
      return indexOfFirstWithValue > -1 ? indexOfFirstWithValue : null
    }, [valuesForProp])

    // Update the current options to be the ones listed against this control
    React.useEffect(() => {
      dispatch([setResizeOptionsTargetOptions(props.options, defaultSelectedOptionIndex)], 'canvas')
      // important! the array is empty because it should only run once
      // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [])

    return (
      <div
        style={{
          position: 'absolute',
          backgroundColor: colorTheme.primary.shade(10).value,
          border: `1px solid ${colorTheme.primary.value}`,
          borderRadius: 5,
          top: props.top,
          left: props.left,
        }}
      >
        {resizeOptions.propertyTargetOptions.map((option, index) => {
          const valueForProp = valuesForProp[index] ?? '—'

          return (
            <div
              key={option}
              style={{
                padding: '0 3px',
                color:
                  resizeOptions.propertyTargetSelectedIndex === index
                    ? 'white'
                    : colorTheme.primary.value,
                backgroundColor:
                  resizeOptions.propertyTargetSelectedIndex === index
                    ? colorTheme.primary.value
                    : 'inherit',
                borderRadius: 5,
              }}
            >
              {option}: <span style={{ float: 'right' }}>{valueForProp}</span>
            </div>
          )
        })}
      </div>
    )
  },
)
