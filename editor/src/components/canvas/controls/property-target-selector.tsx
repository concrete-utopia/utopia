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

export const PropertyTargetSelector = betterReactMemo(
  'PropertyTargetSelector',
  (props: PropertyTargetSelectorProps): JSX.Element => {
    const colorTheme = useColorTheme()
    const { resizeOptions, dispatch, shiftPressed } = useEditorState((editorState) => {
      return {
        resizeOptions: editorState.editor.canvas.resizeOptions,
        dispatch: editorState.dispatch,
        shiftPressed: editorState.editor.keysPressed.shift,
      }
    }, 'PropertyTargetSelector resizeOptions')

    const previousShiftPressed = usePrevious(shiftPressed)

    // Increment the position of the selector if shift has been depressed.
    React.useEffect(() => {
      if (shiftPressed && !previousShiftPressed) {
        dispatch([incrementResizeOptionsSelectedIndex()], 'canvas')
      }
    }, [dispatch, previousShiftPressed, shiftPressed])

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
              {option}: <span style={{ float: 'right' }}>{valueForProp}</span>
            </div>
          )
        })}
      </div>
    )
  },
)
