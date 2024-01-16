/** @jsxRuntime classic */
/** @jsx jsx */
import composeRefs from '@seznam/compose-react-refs'
import React from 'react'
import { jsx } from '@emotion/react'
import type { ControlStatus } from '../../components/inspector/common/control-status'
import type { ControlStyles } from '../../components/inspector/common/control-styles'
import { getControlStyles } from '../../components/inspector/common/control-styles'
import { useColorTheme, UtopiaTheme } from '../styles/theme'
import { useControlsDisabledInSubtree } from '../utilities/disable-subtree'

export interface CheckboxInputProps extends React.InputHTMLAttributes<HTMLInputElement> {
  controlStatus?: ControlStatus
  focusOnMount?: boolean
}

// TODO FIX MY REFs
export const CheckboxInput = React.memo(
  React.forwardRef<HTMLInputElement, CheckboxInputProps>(
    ({ controlStatus = 'simple', focusOnMount = false, style, ...props }, propsRef) => {
      const ref = React.useRef<HTMLInputElement>(null)

      const controlStyles: ControlStyles = getControlStyles(controlStatus)

      const controlsDisabled = useControlsDisabledInSubtree()
      const disabled = !controlStyles.interactive || controlsDisabled

      React.useEffect(() => {
        if (ref.current != null) {
          ref.current.indeterminate = controlStyles.mixed
        }
      })

      React.useEffect(() => {
        if (focusOnMount && ref.current != null) {
          ref.current.focus()
        }
      }, [focusOnMount, ref])

      const colorTheme = useColorTheme()
      const checked =
        props.checked != null && (controlStyles.interactive || controlStyles.showContent)
          ? props.checked
          : false

      return (
        <input
          {...props}
          type='checkbox'
          disabled={disabled}
          style={style}
          css={{
            WebkitAppearance: 'none',
            outline: 'none',
            margin: '5px 2px',
            boxShadow: `0 0 0 1px ${controlStyles.borderColor}`,
            backgroundColor: colorTheme.bg5.value,
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
              boxShadow: `0 0 0 1px ${colorTheme.inspectorFocusedColor.value}`,
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
