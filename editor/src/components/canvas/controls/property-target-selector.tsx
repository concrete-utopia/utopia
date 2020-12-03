import * as React from 'react'
import {
  createLayoutPropertyPath,
  LayoutTargetableProp,
} from '../../../core/layout/layout-helpers-new'
import { getSimpleAttributeAtPath } from '../../../core/model/element-metadata-utils'
import { bimapEither, eitherToMaybe, left, right } from '../../../core/shared/either'
import { ElementInstanceMetadata } from '../../../core/shared/element-template'
import { isFeatureEnabled } from '../../../utils/feature-switches'
import { colorTheme } from '../../../uuiui'

interface PropertyTargetSelector {
  targetComponentMetadata: ElementInstanceMetadata | null
  top: number
  left: number
  options: LayoutTargetableProp[]
  selected: number
  setOptionsCallback: (options: Array<LayoutTargetableProp>) => void
}

export const PropertyTargetSelector = (props: PropertyTargetSelector): JSX.Element | null => {
  props.setOptionsCallback(props.options)

  if (!isFeatureEnabled('Element Resize Menu')) {
    return null
  }
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
              color: props.selected === index ? 'white' : colorTheme.controlledBlue.value,
              backgroundColor:
                props.selected === index ? colorTheme.controlledBlue.value : 'inherit',
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
