/** @jsxRuntime classic */
/** @jsx jsx */
import type { Interpolation } from '@emotion/react'
import { jsx } from '@emotion/react'
import React from 'react'
import { UtopiaTheme, colorTheme } from '../../../uuiui'
import type { IcnProps } from '../../../uuiui'
import type { DEPRECATEDControlProps, DEPRECATEDGenericControlOptions } from './control'
import { OptionControl } from './option-control'
import Utils from '../../../utils/utils'

export interface OptionChainOption<T> {
  value: T
  icon?: IcnProps
  iconComponent?: React.ReactNode
  label?: string
  tooltip?: string
  forceCallOnSubmitValue?: boolean // Call the onSubmitValue again even when the control is already on that value
  disabled?: boolean
}

export function getOptionControlTestId(testIdPrefix: string, postfix: string): string {
  return `${testIdPrefix}-${postfix}`
}

// TODO come up with a typed OptionChainControl types!
export const OptionChainControl: React.FunctionComponent<
  React.PropsWithChildren<DEPRECATEDControlProps<any>>
> = React.memo(({ style, ...props }) => {
  const options = props.options as Array<OptionChainOption<string | number>>
  const labelBelow = (props.DEPRECATED_controlOptions as DEPRECATEDGenericControlOptions)
    ?.labelBelow
  if (!Array.isArray(props.options)) {
    throw new Error('OptionControl needs an array of `options`')
  }

  const optionCSS: Interpolation<any> = React.useMemo(() => {
    return {
      position: 'relative',
      // This is the divider in between controls
      '&:not(:first-of-type)::after': {
        content: '""',
        height: 10,
        backgroundColor: props.controlStyles.borderColor,
        position: 'absolute',
        left: 0,
        top: 6,
      },
    }
  }, [props.controlStyles.borderColor])

  const containerCSS: Interpolation<any> = React.useMemo(() => {
    return {
      display: 'flex',
      flexDirection: 'column',
      marginBottom: 0,
      width: '100%',
      ...(style as any), // TODO Emotion and React 18 types don't like each other
    }
  }, [style])

  return (
    <div id={props.id} key={props.key} css={containerCSS}>
      <div
        style={{
          display: 'flex',
          flexDirection: 'row',
          border: `1px solid ${colorTheme.bg4.value}`,
          borderRadius: UtopiaTheme.inputBorderRadius,
          padding: '1px',
        }}
        className={`option-chain-control-container ${Utils.pathOr(
          '',
          ['controlClassName'],
          props,
        )}`}
        onContextMenu={props.onContextMenu}
      >
        {options.map((option: OptionChainOption<number | string>, index) => (
          <OptionControl
            {...props}
            css={optionCSS}
            key={'option-' + index}
            testId={getOptionControlTestId(
              props.testId,
              option.label?.toLowerCase() ?? index.toString(),
            )}
            DEPRECATED_controlOptions={{
              tooltip: option.tooltip,
              icon: option.icon,
              iconComponent: option.iconComponent,
              labelInner: option.label,
              disabled: option.disabled,
            }}
            value={props.value === option.value}
            // eslint-disable-next-line react/jsx-no-bind
            onSubmitValue={(isChecked: boolean) => {
              if (option.disabled === true) {
                return
              }
              if (isChecked || option.forceCallOnSubmitValue) {
                props.onSubmitValue(option.value)
              } else {
                props.onUnsetValues?.()
              }
            }}
          />
        ))}
      </div>
      {labelBelow == null ? null : (
        <label
          htmlFor={props.id}
          onContextMenu={props.onContextMenu}
          style={{ fontSize: 10, color: props.controlStyles.mainColor, paddingTop: 2 }}
        >
          <span className='label-container'>{labelBelow}</span>
        </label>
      )}
    </div>
  )
})
