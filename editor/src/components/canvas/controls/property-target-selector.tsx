import * as React from 'react'
import {
  createLayoutPropertyPath,
  LayoutTargetableProp,
} from '../../../core/layout/layout-helpers-new'
import { getSimpleAttributeAtPath } from '../../../core/model/element-metadata-utils'
import { bimapEither, eitherToMaybe, left, right } from '../../../core/shared/either'
import { ElementInstanceMetadata } from '../../../core/shared/element-template'
import { KeysPressed } from '../../../utils/keyboard'
import { colorTheme } from '../../../uuiui'
import { useTargetSelector } from './new-canvas-controls'

interface PropertyTargetSelector {
  targetComponentMetadata: ElementInstanceMetadata | null
  top: number
  left: number
  options: readonly LayoutTargetableProp[]
  targetIndex: number
  keysPressed: KeysPressed
}

export const PropertyTargetSelector = (props: PropertyTargetSelector): JSX.Element => {
  return (
    <div
      style={{
        position: 'absolute',
        backgroundColor: '#d4f3ff',
        border: `1px solid ${colorTheme.controlledBlue.value}`,
        borderRadius: 5,
        top: props.top,
        left: props.left,
      }}
    >
      {props.options.map((option, index) => {
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
              color: props.targetIndex === index ? 'white' : colorTheme.controlledBlue.value,
              backgroundColor:
                props.targetIndex === index ? colorTheme.controlledBlue.value : 'inherit',
              borderRadius: 5,
            }}
          >
            {option}: <span style={{ float: 'right' }}>{valueForProp}</span>
          </div>
        )
      })}
    </div>
  )
}
