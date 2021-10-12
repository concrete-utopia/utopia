import React from 'react'
import { ParsedPropertyControls } from '../../../../core/property-controls/property-controls-parser'
import { isRight } from '../../../../core/shared/either'
import { PropertyPath } from '../../../../core/shared/project-file-types'
import { ParseResult } from '../../../../utils/value-parser-utils'
import { betterReactMemo } from '../../../../uuiui-deps'
import { SectionRow } from './component-section'
import * as PP from '../../../../core/shared/property-path'
import { ControlStatus } from '../../common/control-status'
import { CSSCursor } from '../../../../uuiui-deps'

interface HiddenControlsProps {
  propertyControls: ParseResult<ParsedPropertyControls>
  propertyControlsStatus: { [path: string]: ControlStatus }
  visibleEmptyControls: PropertyPath[]
  showHiddenControl: (path: PropertyPath) => void
  setGlobalCursor: (cursor: CSSCursor | null) => void
}

export const useHiddenElements = (): [PropertyPath[], (path: PropertyPath) => void] => {
  const [visibleEmptyControls, setVisibleEmptyControls] = React.useState<Array<PropertyPath>>([])
  const showHiddenControl = React.useCallback(
    (path: PropertyPath) => setVisibleEmptyControls([...visibleEmptyControls, path]),
    [setVisibleEmptyControls, visibleEmptyControls],
  )

  return [visibleEmptyControls, showHiddenControl]
}

function isControlUnset(controlStatus: ControlStatus): boolean {
  return controlStatus === 'unset' || controlStatus === 'multiselect-identical-unset'
}

function filterVisibleEmptyControls(
  pathNames: string[],
  visibleEmptyControls: PropertyPath[],
): string[] {
  return pathNames.filter(
    (name) => !visibleEmptyControls.find((visible) => PP.toString(visible) === name),
  )
}

export function filterNonUnsetAndEmptyControls(
  pathNames: string[],
  propertyControlsStatus: { [path: string]: ControlStatus },
  visibleEmptyControls: PropertyPath[],
): string[] {
  return filterVisibleEmptyControls(
    pathNames.filter((name) => !isControlUnset(propertyControlsStatus[name])),
    visibleEmptyControls,
  )
}

function filterUnsetAndEmptyControls(
  pathNames: string[],
  propertyControlsStatus: { [path: string]: ControlStatus },
  visibleEmptyControls: PropertyPath[],
): string[] {
  return filterVisibleEmptyControls(
    pathNames.filter((name) => isControlUnset(propertyControlsStatus[name])),
    visibleEmptyControls,
  )
}

export const HiddenControls = betterReactMemo(
  'HiddenControls',
  (props: HiddenControlsProps): JSX.Element | null => {
    const visibleEmptyControls = React.useMemo(
      () =>
        props.visibleEmptyControls.map((path) => {
          if (isRight(props.propertyControls)) {
            const propertyControl = props.propertyControls.value[PP.toString(path)]
            if (propertyControl != null && isRight(propertyControl)) {
              return (
                <SectionRow
                  key={PP.toString(path)}
                  propPath={path}
                  controlDescription={propertyControl.value}
                  isScene={false}
                  setGlobalCursor={props.setGlobalCursor}
                />
              )
            } else {
              return null
            }
          } else {
            return null
          }
        }),
      [props.visibleEmptyControls, props.propertyControls, props.setGlobalCursor],
    )

    const propNameList = React.useMemo(() => {
      const hiddenPropNames = filterUnsetAndEmptyControls(
        Object.keys(props.propertyControlsStatus),
        props.propertyControlsStatus,
        props.visibleEmptyControls,
      )
      if (hiddenPropNames.length > 0) {
        return (
          <>
            <div style={{ padding: '4px 8px', fontWeight: 600 }}>
              Additional Optional Properties
            </div>
            <div
              style={{
                padding: '0px 8px',
                display: 'flex',
                flexDirection: 'row',
                flexWrap: 'wrap',
                gap: '0px 2px',
              }}
            >
              {hiddenPropNames.map((name, index) => {
                return (
                  <HiddenControlLabel
                    key={name}
                    propPath={PP.create([name])}
                    showHiddenControl={props.showHiddenControl}
                    isLast={hiddenPropNames.length - 1 === index}
                  />
                )
              })}
            </div>
          </>
        )
      } else {
        return null
      }
    }, [props.propertyControlsStatus, props.showHiddenControl, props.visibleEmptyControls])

    return (
      <>
        {visibleEmptyControls}
        {propNameList}
      </>
    )
  },
)

interface HiddenControlLabelProps {
  propPath: PropertyPath
  showHiddenControl: (path: PropertyPath) => void
  isLast: boolean
}

const HiddenControlLabel = betterReactMemo(
  'HiddenControlLabel',
  (props: HiddenControlLabelProps): JSX.Element | null => {
    const { propPath, showHiddenControl } = props
    const labelOnClick = React.useCallback(() => showHiddenControl(propPath), [
      propPath,
      showHiddenControl,
    ])
    return (
      <span style={{ cursor: 'pointer' }} onClick={labelOnClick}>
        {PP.toString(propPath)}
        {props.isLast ? '' : ', '}
      </span>
    )
  },
)
