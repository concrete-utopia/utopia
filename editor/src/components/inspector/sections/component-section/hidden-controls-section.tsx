/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { css, jsx } from '@emotion/react'
import { Subdued, useColorTheme, UtopiaTheme } from '../../../../uuiui'
import type { ControlStatus } from '../../common/control-status'

interface HiddenControlsProps {
  hiddenPropNames: string[]
  showHiddenControl: (propName: string) => void
  indentationLevel: number
}

export const useHiddenElements = (): [string[], (path: string) => void] => {
  const [visibleEmptyControls, setVisibleEmptyControls] = React.useState<Array<string>>([])
  const showHiddenControl = React.useCallback(
    (propName: string) => setVisibleEmptyControls([...visibleEmptyControls, propName]),
    [setVisibleEmptyControls, visibleEmptyControls],
  )

  return [visibleEmptyControls, showHiddenControl]
}

function isControlUnset(controlStatus: ControlStatus): boolean {
  return controlStatus === 'unset' || controlStatus === 'multiselect-identical-unset'
}

function filterVisibleEmptyControls(pathNames: string[], visibleEmptyControls: string[]): string[] {
  const result = pathNames.filter((name) => !visibleEmptyControls.includes(name))
  return result
}

export function filterUnsetControls(
  pathNames: string[],
  propertyControlsStatus: { [path: string]: ControlStatus },
): string[] {
  return pathNames.filter((name) => isControlUnset(propertyControlsStatus[name]))
}

export const HiddenControls = React.memo((props: HiddenControlsProps): JSX.Element | null => {
  const indentation = props.indentationLevel * 8
  if (props.hiddenPropNames.length > 0) {
    return (
      <div
        style={{
          padding: '0px 8px',
          paddingLeft: indentation,
          minHeight: UtopiaTheme.layout.rowHeight.normal,
          display: 'flex',
          flexDirection: 'row',
          flexWrap: 'wrap',
          gap: '0px 2px',
          alignItems: 'center',
        }}
      >
        <Subdued>Available: </Subdued>
        {props.hiddenPropNames.map((name, index) => {
          return (
            <HiddenControlLabel
              key={name}
              propName={name}
              showHiddenControl={props.showHiddenControl}
              isLast={props.hiddenPropNames.length - 1 === index}
            />
          )
        })}
      </div>
    )
  } else {
    return null
  }
})

interface HiddenControlLabelProps {
  propName: string
  showHiddenControl: (propName: string) => void
  isLast: boolean
}

const HiddenControlLabel = React.memo((props: HiddenControlLabelProps): JSX.Element | null => {
  const colorTheme = useColorTheme()
  const { propName, showHiddenControl } = props
  const labelOnClick = React.useCallback(
    () => showHiddenControl(propName),
    [propName, showHiddenControl],
  )
  return (
    <Subdued
      css={{
        ':hover': {
          color: colorTheme.primary.value,
        },
      }}
      style={{ cursor: 'pointer' }}
      onClick={labelOnClick}
    >
      {propName}
      {props.isLast ? '' : ', '}
    </Subdued>
  )
})
