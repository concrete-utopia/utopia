import * as React from 'react'
import {
  createLayoutPropertyPath,
  LayoutTargetableProp,
} from '../../../core/layout/layout-helpers-new'
import { getSimpleAttributeAtPath } from '../../../core/model/element-metadata-utils'
import { bimapEither, eitherToMaybe, left, right } from '../../../core/shared/either'
import { ElementInstanceMetadata } from '../../../core/shared/element-template'
import { useColorTheme } from '../../../uuiui'

interface PropertyTargetSelectorProps {
  targetComponentMetadata: ElementInstanceMetadata | null
  top: number
  left: number
  options: LayoutTargetableProp[]
  selected: number
  setOptionsCallback: (options: Array<LayoutTargetableProp>) => void
}

export const PropertyTargetSelector = (props: PropertyTargetSelectorProps): JSX.Element => {
  const colorTheme = useColorTheme()
  props.setOptionsCallback(props.options)

  return (
    <div
      style={{
        position: 'absolute',
        backgroundColor: colorTheme.primary.shade(10).value,
        border: `1px solid ${colorTheme.primary.value}`,
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
              color: props.selected === index ? 'white' : colorTheme.primary.value,
              backgroundColor: props.selected === index ? colorTheme.primary.value : 'inherit',
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
