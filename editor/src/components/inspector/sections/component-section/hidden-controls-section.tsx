import React from 'react'
import { ParsedPropertyControls } from '../../../../core/property-controls/property-controls-parser'
import { foldEither, isRight } from '../../../../core/shared/either'
import { PropertyPath } from '../../../../core/shared/project-file-types'
import { ParseResult } from '../../../../utils/value-parser-utils'
import { betterReactMemo } from '../../../../uuiui-deps'
import { SectionRow } from './component-section'
import * as PP from '../../../../core/shared/property-path'
import { ControlStatus } from '../../common/control-status'
import { CSSCursor } from '../../../../uuiui-deps'

interface HiddenControlsProps {
  hiddenPropNames: string[]
  showHiddenControl: (propName: string) => void
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

export function filterNonUnsetAndEmptyControls(
  pathNames: string[],
  propertyControlsStatus: { [path: string]: ControlStatus },
  visibleEmptyControls: string[],
): string[] {
  return filterVisibleEmptyControls(
    pathNames.filter((name) => !isControlUnset(propertyControlsStatus[name])),
    visibleEmptyControls,
  )
}

export const HiddenControls = betterReactMemo(
  'HiddenControls',
  (props: HiddenControlsProps): JSX.Element | null => {
    if (props.hiddenPropNames.length > 0) {
      return (
        <>
          <div style={{ padding: '4px 8px', fontWeight: 600 }}>Additional Optional Properties</div>
          <div
            style={{
              padding: '0px 8px',
              display: 'flex',
              flexDirection: 'row',
              flexWrap: 'wrap',
              gap: '0px 2px',
            }}
          >
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
        </>
      )
    } else {
      return null
    }
  },
)

interface HiddenControlLabelProps {
  propName: string
  showHiddenControl: (propName: string) => void
  isLast: boolean
}

const HiddenControlLabel = betterReactMemo(
  'HiddenControlLabel',
  (props: HiddenControlLabelProps): JSX.Element | null => {
    const { propName, showHiddenControl } = props
    const labelOnClick = React.useCallback(() => showHiddenControl(propName), [
      propName,
      showHiddenControl,
    ])
    return (
      <span style={{ cursor: 'pointer' }} onClick={labelOnClick}>
        {propName}
        {props.isLast ? '' : ', '}
      </span>
    )
  },
)
