/** @jsx jsx */
import composeRefs from '@seznam/compose-react-refs'
import * as React from 'react'
import { jsx } from '@emotion/core'
import {
  ControlStatus,
  getControlStyles,
  ControlStyles,
} from '../../components/inspector/common/control-status'
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
    ({ controlStatus = 'simple', focusOnMount = false, style, ...props }, propsRef) => {
      const ref = React.useRef<HTMLInputElement>(null)

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
        <input
          {...props}
          type='checkbox'
          disabled={!controlStyles.interactive}
          style={style}
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
            '&.widget-status-controlled': {
              backgroundImage:
                'url("/editor/icons/light/controls/checkbox/checked-nodegraph-12x12@2x.png")',
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
          }}
          checked={checked}
          ref={composeRefs(ref, propsRef)}
        />
      )
    },
  ),
)
