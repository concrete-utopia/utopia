/** @jsx jsx */
import { jsx } from '@emotion/core'
import * as React from 'react'
import { animated, useSpring } from 'react-spring'
import { UtopiaTheme } from 'uuiui'
import { ControlStatus, ControlStyles, OnSubmitValue } from '../../../uuiui-deps'
import { SegmentOptionTextControl } from './segment-option-text-control'

export interface SegmentOption<T extends string | number = string | number> {
  value: T
  label?: React.ReactNode
  tooltip?: string
}

interface SegmentControlProps<T extends string | number> {
  value: T | null
  onSubmitValue: OnSubmitValue<T>
  controlStatus: ControlStatus
  controlStyles: ControlStyles
  options: ReadonlyArray<SegmentOption<T>>
  labelBelow?: React.ReactNode
  style?: React.CSSProperties
  id?: string
}

export const SegmentControl = <T extends string | number>(props: SegmentControlProps<T>) => {
  const options = props.options
  const selectedIndex = options.findIndex((option) => option.value === props.value)

  const selectorStyle = useSpring({
    width: `${100 / options.length}%`,
    height: '100%',
    position: 'absolute',
    left: `${selectedIndex * (100 / options.length)}%`,
    backgroundColor: props.controlStyles.segmentSelectorColor,
    borderRadius: 2,
  } as React.CSSProperties)

  return (
    <div
      id={props.id}
      style={{
        width: '100%',
        display: 'flex',
        flexDirection: 'column',
        marginBottom: 0,
        ...props.style,
      }}
    >
      <div
        className='segment-control-track'
        css={{
          width: '100%',
          height: UtopiaTheme.layout.inputHeight.tall,
          boxShadow: props.controlStyles.isSet
            ? `0 0 0 1px ${props.controlStyles.borderColor} inset`
            : undefined,
          ':hover': {
            boxShadow: `0 0 0 1px ${props.controlStyles.borderColor} inset`,
          },
          alignItems: 'center',
          borderRadius: UtopiaTheme.inputBorderRadius,
        }}
      >
        <div
          style={{
            position: 'relative',
            margin: 3,
            width: 'calc(100% - 6px)',
            height: 'calc(100% - 6px)',
          }}
        >
          <animated.div style={selectorStyle} />
          <div
            style={{
              display: 'grid',
              gridTemplateColumns: 'repeat(auto-fit, minmax(0, 1fr))',
              position: 'relative',
              width: '100%',
              height: '100%',
            }}
          >
            {options.map((option, index) => (
              <SegmentOptionTextControl
                key={option.value}
                option={option}
                selected={selectedIndex === index}
                onSubmitValue={props.onSubmitValue}
                controlStyles={props.controlStyles}
              />
            ))}
          </div>
        </div>
      </div>
      {props.labelBelow != null ? (
        <label
          style={{
            display: 'block',
            textAlign: 'center',
            fontSize: 9,
            color: props.controlStyles.mainColor,
            paddingTop: 2,
          }}
          color={props.controlStyles.mainColor}
        >
          <span className='label-container'>{props.labelBelow}</span>
        </label>
      ) : null}
    </div>
  )
}
