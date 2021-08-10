import * as React from 'react'
import {
  createLayoutPropertyPath,
  LayoutTargetableProp,
  StyleLayoutProp,
} from '../../../core/layout/layout-helpers-new'
import { getSimpleAttributeAtPath } from '../../../core/model/element-metadata-utils'
import { eitherToMaybe, left } from '../../../core/shared/either'
import { ElementInstanceMetadata } from '../../../core/shared/element-template'
import { LayoutTargetablePropArrayKeepDeepEquality } from '../../../utils/deep-equality-instances'
import { betterReactMemo } from '../../../utils/react-performance'
import { useColorTheme } from '../../../uuiui'
import {
  decrementResizeOptionsSelectedIndex,
  incrementResizeOptionsSelectedIndex,
  setResizeOptionsTargetOptions,
} from '../../editor/actions/action-creators'
import { usePrevious } from '../../editor/hook-utils'
import { useEditorState } from '../../editor/store/store-hook'

interface PropertyTargetSelectorProps {
  targetComponentMetadata: ElementInstanceMetadata | null
  top: number
  left: number
  options: Array<LayoutTargetableProp>
}

function labelForOption(option: LayoutTargetableProp): string {
  switch (option) {
    case 'Width':
      return 'width'
    case 'Height':
      return 'height'
    case 'PinnedLeft':
      return 'left'
    case 'PinnedTop':
      return 'top'
    case 'PinnedRight':
      return 'right'
    case 'PinnedBottom':
      return 'bottom'
    default:
      return option
  }
}

export const PropertyTargetSelector = betterReactMemo(
  'PropertyTargetSelector',
  (props: PropertyTargetSelectorProps): JSX.Element => {
    const colorTheme = useColorTheme()
    const { resizeOptions, dispatch } = useEditorState((editorState) => {
      return {
        resizeOptions: editorState.editor.canvas.resizeOptions,
        dispatch: editorState.dispatch,
      }
    }, 'PropertyTargetSelector resizeOptions')

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

    // Update the current options to be the ones listed against this control,
    // but only if they're different to the current options.
    React.useEffect(() => {
      if (
        !LayoutTargetablePropArrayKeepDeepEquality(
          resizeOptions.propertyTargetOptions,
          props.options,
        ).areEqual
      ) {
        dispatch([setResizeOptionsTargetOptions(props.options)], 'canvas')
      }
    }, [dispatch, props.options, resizeOptions])

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
          const valueForProp =
            eitherToMaybe(
              getSimpleAttributeAtPath(
                left(props.targetComponentMetadata?.props ?? {}),
                createLayoutPropertyPath(option),
              ),
            ) ?? '—'

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
              {labelForOption(option)}: <span style={{ float: 'right' }}>{valueForProp}</span>
            </div>
          )
        })}
      </div>
    )
  },
)
