/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { jsx } from '@emotion/react'
import type { DEPRECATEDControlProps, DEPRECATEDGenericControlOptions } from './control'
import { colorTheme } from '../../../uuiui'
import type { IcnProps } from '../../../uuiui'
import { UtopiaTheme, Tooltip, Icn } from '../../../uuiui'
import { useIsMyProject } from '../../editor/store/collaborative-editing'
import { useControlsDisabledInSubtree } from '../../../uuiui/utilities/disable-subtree'

export interface DEPRECATEDOptionControlOptions extends DEPRECATEDGenericControlOptions {
  icon?: IcnProps
  labelInner?: string
  tooltip?: string
  roundCorners?:
    | 'right'
    | 'left'
    | 'top'
    | 'bottom'
    | 'topLeft'
    | 'topRight'
    | 'bottomRight'
    | 'bottomLeft'
    | 'none'
    | 'all'
  width?: number
  height?: number
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
      return colorTheme.bg2.value
    } else {
      return 'transparent'
    }
  })()

  const rc = controlOptions.roundCorners

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
            padding: '0 2px',
            textAlign: 'center',
            minWidth: controlOptions.width,
            height: controlOptions.height,
            backgroundColor: background,
            color:
              isChecked && props.controlStatus === 'overridden'
                ? colorTheme.brandNeonPink.value
                : colorTheme.fg1.value,
            // If just an option control:
            borderRadius: rc != null ? 0 : UtopiaTheme.inputBorderRadius,
            // If part of a option chain control:
            '.option-chain-control-container &': {
              borderRadius: 0,
              boxShadow: 'none !important',
            },
            '.option-chain-control-container .segment:first-of-type  &': {
              borderTopLeftRadius: UtopiaTheme.inputBorderRadius,
              borderBottomLeftRadius: UtopiaTheme.inputBorderRadius,
            },
            '.option-chain-control-container .segment:last-child &': {
              borderTopRightRadius: UtopiaTheme.inputBorderRadius,
              borderBottomRightRadius: UtopiaTheme.inputBorderRadius,
            },
            borderTopRightRadius:
              rc === 'all' || rc === 'right' || rc === 'topRight' || rc === 'top'
                ? UtopiaTheme.inputBorderRadius
                : undefined,
            borderBottomRightRadius:
              rc === 'all' || rc === 'right' || rc === 'bottomRight' || rc === 'bottom'
                ? UtopiaTheme.inputBorderRadius
                : undefined,
            borderTopLeftRadius:
              rc === 'all' || rc === 'left' || rc === 'topLeft' || rc === 'top'
                ? UtopiaTheme.inputBorderRadius
                : undefined,
            borderBottomLeftRadius:
              rc === 'all' || rc === 'left' || rc === 'bottomLeft' || rc === 'bottom'
                ? UtopiaTheme.inputBorderRadius
                : undefined,
            '&:hover': {
              opacity: props.controlStatus == 'disabled' ? undefined : controlOpacity + 0.2,
            },
            '&:active': {
              opacity: props.controlStatus == 'disabled' ? undefined : 1,
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
            ) : null}
            {controlOptions.labelInner != null ? controlOptions.labelInner : null}
          </div>
        </label>
      </Tooltip>
    </div>
  )
}
