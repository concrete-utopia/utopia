/** @jsx jsx */
import { Interpolation, jsx } from '@emotion/react'
import * as React from 'react'
import { IcnProps, UtopiaTheme } from '../../../uuiui'
import { DEPRECATEDControlProps, DEPRECATEDGenericControlOptions } from './control'
import { OptionControl } from './option-control'
import Utils from '../../../utils/utils'

export interface OptionChainOption<T> {
  value: T
  icon?: IcnProps
  label?: string
  tooltip?: string
}

// TODO come up with a typed OptionChainControl types!
export const OptionChainControl: React.StatelessComponent<DEPRECATEDControlProps<any>> = ({
  style,
  ...props
}) => {
  const options = props.options as Array<OptionChainOption<string | number>>
  const labelBelow = (props.DEPRECATED_controlOptions as DEPRECATEDGenericControlOptions)
    ?.labelBelow
  if (!Array.isArray(props.options)) {
    throw new Error('OptionControl needs an array of `options`')
  }

  const optionCSS: Interpolation<any> = {
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

  return (
    <div
      id={props.id}
      key={props.key}
      css={{
        display: 'flex',
        flexDirection: 'column',
        marginBottom: 0,
        ...style,
      }}
    >
      <div
        style={{
          display: 'flex',
          flexDirection: 'row',
          height: UtopiaTheme.layout.inputHeight.default,
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
            DEPRECATED_controlOptions={{
              tooltip: option.tooltip,
              icon: option.icon,
              labelInner: option.label,
            }}
            value={props.value === option.value}
            // eslint-disable-next-line react/jsx-no-bind
            onSubmitValue={(value: boolean) => {
              if (value) {
                props.onSubmitValue(option.value)
              }
            }}
          />
        ))}
      </div>
      {!labelBelow ? null : (
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
}
