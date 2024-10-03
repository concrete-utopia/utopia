/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { jsx } from '@emotion/react'
import type { DEPRECATEDControlProps, DEPRECATEDGenericControlOptions } from './control'
import { colorTheme } from '../../../uuiui'
import type { IcnProps } from '../../../uuiui'
import { UtopiaTheme, Tooltip, Icn } from '../../../uuiui'
import { useControlsDisabledInSubtree } from '../../../uuiui/utilities/disable-subtree'

export interface DEPRECATEDOptionControlOptions extends DEPRECATEDGenericControlOptions {
  icon?: IcnProps
  iconComponent?: React.ReactNode
  labelInner?: string
  tooltip?: string
  width?: number
  height?: number
  disabled?: boolean
}

export const OptionControl: React.FunctionComponent<
  React.PropsWithChildren<
    DEPRECATEDControlProps<boolean> & {
      className?: string
    }
  >
> = (props) => {
  const isChecked = props.value
  const propsOnSubmitValue = props.onSubmitValue
  const onSubmitValue = React.useCallback(() => {
    propsOnSubmitValue(!isChecked)
  }, [propsOnSubmitValue, isChecked])

  const controlOptions: DEPRECATEDOptionControlOptions = {
    width: UtopiaTheme.layout.inputHeight.default,
    height: UtopiaTheme.layout.inputHeight.default,
    ...(props.DEPRECATED_controlOptions as DEPRECATEDOptionControlOptions),
  }

  let controlOpacity: number
  if (props.controlStyles.set) {
    if (props.value) {
      controlOpacity = UtopiaTheme.styles.inspectorSetSelectedOpacity
    } else {
      controlOpacity = UtopiaTheme.styles.inspectorUnsetSelectedOpacity
    }
  } else {
    if (props.value) {
      controlOpacity = UtopiaTheme.styles.inspectorSetUnselectedOpacity
    } else {
      controlOpacity = UtopiaTheme.styles.inspectorUnsetUnselectedOpacity
    }
  }

  const controlsDisabled = useControlsDisabledInSubtree()
  const disabled = !props.controlStyles.interactive || controlsDisabled

  const background = (() => {
    if (props.controlStatus === 'overridden' && props.value) {
      return colorTheme.brandNeonPink10.value
    } else if (props.value) {
      return colorTheme.bg4.value
    } else {
      return 'transparent'
    }
  })()

  return (
    <div
      className={`${
        props.className != null ? props.className : ''
      } option-control-container segment`}
      style={{
        flex: '1',
        ...props.style,
      }}
    >
      <Tooltip
        disabled={controlOptions.tooltip == null}
        title={controlOptions.tooltip != null ? controlOptions.tooltip : ''}
        placement={'top'}
      >
        <label
          data-testid={props.testId}
          data-ischecked={isChecked}
          data-controlstatus={props.controlStatus}
          className={`option-control ${
            props.controlClassName != null ? props.controlClassName : ''
          }`}
          onContextMenu={props.onContextMenu}
          css={{
            display: 'flex',
            justifyContent: 'center',
            alignItems: 'center',
            flex: 1,
            padding: '0 4px',
            textAlign: 'center',
            minWidth: controlOptions.width,
            height: controlOptions.height,
            backgroundColor: background,
            color:
              isChecked && props.controlStatus === 'overridden'
                ? colorTheme.brandNeonPink.value
                : colorTheme.fg1.value,
            filter: props.controlStatus === 'disabled' ? 'grayscale(0)' : undefined,
            opacity: props.controlStatus === 'disabled' ? undefined : isChecked ? 1 : 0.7,
            // If part of a option chain control:
            '.option-chain-control-container &': {
              boxShadow: 'none !important',
              borderRadius: 2,
              opacity: controlOptions.disabled ? 0.2 : isChecked ? 1 : 0.7,
              '&:hover': {
                opacity: controlOptions.disabled ? undefined : 1,
                color:
                  isChecked && props.controlStatus === 'overridden'
                    ? colorTheme.brandNeonPink.value
                    : colorTheme.fg1.value,
              },
            },
            '&:hover': {
              opacity: props.controlStatus === 'disabled' ? undefined : 1,
            },
            '.control-option-icon-component': {
              opacity: 0.7,
            },
            '&:hover .control-option-icon-component': {
              opacity: 1,
            },
          }}
        >
          <input
            style={{
              visibility: 'hidden',
              display: 'none',
              height: 0,
              width: 0,
              padding: 0,
              margin: 0,
            }}
            type='checkbox'
            checked={isChecked}
            disabled={disabled}
            onChange={onSubmitValue}
          />
          <div
            style={{
              width: '100%',
              height: '100%',
              display: 'flex',
              justifyContent: 'center',
              alignItems: 'center',
            }}
          >
            {controlOptions.icon != null ? (
              <Icn
                style={{ marginRight: controlOptions.labelInner == null ? 0 : 4 }}
                {...controlOptions.icon}
              />
            ) : (
              <div className='control-option-icon-component'>
                {controlOptions.iconComponent ?? null}
              </div>
            )}
            {controlOptions.labelInner != null ? controlOptions.labelInner : null}
          </div>
        </label>
      </Tooltip>
    </div>
  )
}
