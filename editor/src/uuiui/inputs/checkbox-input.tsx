/** @jsx jsx */
import * as React from 'react'
import { jsx } from '@emotion/core'
import {
  ControlStatus,
  getControlStyles,
  ControlStyles,
} from '../../components/inspector/widgets/control-status'
import { UtopiaTheme } from '../styles/theme'
import { betterReactMemo } from 'uuiui-deps'

export interface CheckboxInputProps extends React.InputHTMLAttributes<HTMLInputElement> {
  controlStatus?: ControlStatus
  focusOnMount?: boolean
}

// TODO FIX MY REFs
export const CheckboxInput = betterReactMemo(
  'CheckboxInput',
  React.forwardRef<HTMLInputElement, CheckboxInputProps>(
    ({ controlStatus = 'simple', focusOnMount = false, ...props }, initialRef: any) => {
      const ref = initialRef != null ? initialRef : React.createRef()

      const controlStyles: ControlStyles = getControlStyles(controlStatus)

      React.useEffect(() => {
        if (ref.current != null) {
          ref.current.indeterminate = controlStyles.mixed
        }
      })

      const checked =
        props.checked != null && (controlStyles.interactive || controlStyles.showContent)
          ? props.checked
          : false

      return (
        <div style={props.style}>
          <input
            {...props}
            type='checkbox'
            disabled={!controlStyles.interactive}
            css={{
              WebkitAppearance: 'none',
              outline: 'none',
              margin: '5px 2px',
              boxShadow: `0 0 0 1px ${controlStyles.borderColor}`,
              backgroundColor: controlStyles.backgroundColor,
              borderRadius: UtopiaTheme.inputBorderRadius,
              width: 12,
              height: 12,
              backgroundRepeat: 'no-repeat',
              backgroundPosition: '55% 55%',
              backgroundSize: '12px 12px',
              cursor: controlStyles.interactive ? 'pointer' : 'default',
              '&:checked': {
                backgroundImage:
                  'url("/editor/icons/light/controls/checkbox/checked-dark-12x12@2x.png")',
              },
              '&:focus': {
                boxShadow: `0 0 0 1px ${UtopiaTheme.color.inspectorFocusedColor.value}`,
              },
              '&.widget-status-controlled-nodegraph': {
                backgroundImage:
                  'url("/editor/icons/light/controls/checkbox/checked-nodegraph-12x12@2x.png")',
              },
              '&.widget-status-controlled-component': {
                backgroundImage:
                  'url("/editor/icons/light/controls/checkbox/checked-component-12x12@2x.png")',
              },
              '&:not(:checked)': {
                backgroundImage: 'none',
              },
              '&:indeterminate': {
                backgroundImage:
                  'url("/editor/icons/light/controls/checkbox/mixed-dark-12x12@2x.png")',
              },
              '&.widget-status-off, &.widget-status-disabled': {
                cursor: 'inherit',
              },
              ...props.style,
            }}
            checked={checked}
            ref={ref}
          />
        </div>
      )
    },
  ),
)
