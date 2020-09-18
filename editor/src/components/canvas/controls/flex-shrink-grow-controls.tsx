import * as React from 'react'
import styled from '@emotion/styled'
import { eitherToMaybe, left } from '../../../core/shared/either'
import { getSimpleAttributeAtPath } from '../../../core/model/element-metadata-utils'
import { createLayoutPropertyPath } from '../../../core/layout/layout-helpers-new'
import { ElementInstanceMetadata } from '../../../core/shared/element-template'

const arrowHeight = 5

const Arrowhead = styled.div((props: { color: 'green' | 'red' }) => ({
  display: 'inline-block',
  width: 0,
  height: 0,
  borderTop: `${arrowHeight}px solid transparent`,
  borderBottom: `${arrowHeight}px solid transparent`,
  borderRight: `${arrowHeight}px solid transparent`,
  borderLeft: `${arrowHeight}px solid ${props.color}`,
}))

const Arrowbase = styled.div((props: { length: number; color: 'green' | 'red' }) => ({
  display: 'inline-block',
  position: 'relative',
  width: props.length,
  height: arrowHeight,
  top: -arrowHeight / 2,
  backgroundColor: props.color,
}))

const Arrow: React.FunctionComponent<{
  direction: 'left' | 'up' | 'right' | 'down'
  color: 'green' | 'red'
  arrowSize: number
}> = (props) => {
  const rotation = (() => {
    switch (props.direction) {
      case 'right':
        return 0
      case 'down':
        return 90
      case 'left':
        return 180
      case 'up':
        return 270
    }
  })()

  return (
    <div
      style={{
        transform: `rotate(${rotation}deg)`,
        transformOrigin: '0 50% 0',
      }}
    >
      <Arrowbase length={props.arrowSize} color={props.color} />
      <Arrowhead color={props.color} />
    </div>
  )
}

function useShouldShowArrow(
  direction: 'row' | 'column',
  targetComponentMetadata: ElementInstanceMetadata | null,
) {
  return targetComponentMetadata?.specialSizeMeasurements.parentFlexDirection === direction
}

function useGetArrowSize(
  flexProp: 'flexShrink' | 'flexGrow',
  targetComponentMetadata: ElementInstanceMetadata | null,
) {
  const valueForProp =
    eitherToMaybe(
      getSimpleAttributeAtPath(
        left(targetComponentMetadata?.props ?? {}),
        createLayoutPropertyPath(flexProp),
      ),
    ) ?? 0

  return 10 + valueForProp * 10
}

export const FlexGrowControl: React.FunctionComponent<{
  direction: 'row' | 'column'
  top: number
  left: number
  targetComponentMetadata: ElementInstanceMetadata | null
}> = (props) => {
  const arrowSize = useGetArrowSize('flexGrow', props.targetComponentMetadata)

  const shouldShow = useShouldShowArrow(props.direction, props.targetComponentMetadata)
  if (!shouldShow) {
    return null
  }

  return (
    <div
      style={{
        position: 'absolute',
        top: props.top,
        left: props.left,
      }}
    >
      <Arrow
        direction={props.direction === 'row' ? 'right' : 'down'}
        color={'green'}
        arrowSize={arrowSize}
      />
    </div>
  )
}

export const FlexShrinkControl: React.FunctionComponent<{
  direction: 'row' | 'column'
  top: number
  left: number
  targetComponentMetadata: ElementInstanceMetadata | null
}> = (props) => {
  const arrowSize = useGetArrowSize('flexShrink', props.targetComponentMetadata)

  const shouldShow = useShouldShowArrow(props.direction, props.targetComponentMetadata)
  if (!shouldShow) {
    return null
  }

  return (
    <div
      style={{
        position: 'absolute',
        top: props.top,
        left: props.left,
      }}
    >
      <Arrow
        direction={props.direction === 'row' ? 'left' : 'up'}
        color={'red'}
        arrowSize={arrowSize}
      />
    </div>
  )
}
