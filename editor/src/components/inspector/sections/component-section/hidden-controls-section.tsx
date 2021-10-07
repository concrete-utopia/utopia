import React from 'react'
import { ParsedPropertyControls } from '../../../../core/property-controls/property-controls-parser'
import { filterSpecialProps } from '../../../../core/property-controls/property-controls-utils'
import { isRight } from '../../../../core/shared/either'
import { PropertyPath } from '../../../../core/shared/project-file-types'
import { ParseResult } from '../../../../utils/value-parser-utils'
import { betterReactMemo } from '../../../../uuiui-deps'
import { SectionRow } from './component-section'
import * as PP from '../../../../core/shared/property-path'
import * as EP from '../../../../core/shared/element-path'
import { ControlDescription } from 'utopia-api'
import { InspectorInfo } from '../../common/property-path-hooks'
import { useInspectorInfoForPropertyControl } from '../../common/property-controls-hooks'
import { ControlStatus } from '../../common/control-status'

interface HiddenControlsProps {
  propertyControls: ParseResult<ParsedPropertyControls>
}

export const HiddenControls = betterReactMemo(
  'HiddenControls',
  (props: HiddenControlsProps): JSX.Element | null => {
    const [visibleEmptyElements, setVisibleEmptyElements] = React.useState<Array<PropertyPath>>([])
    const showHiddenControl = React.useCallback(
      (path: PropertyPath) => setVisibleEmptyElements([...visibleEmptyElements, path]),
      [setVisibleEmptyElements, visibleEmptyElements],
    )

    const visibleEmptyControls = React.useMemo(
      () =>
        visibleEmptyElements.map((path) => {
          if (isRight(props.propertyControls)) {
            const propertyControl = props.propertyControls.value[PP.toString(path)]
            if (propertyControl != null && isRight(propertyControl)) {
              return (
                <SectionRow
                  key={PP.toString(path)}
                  propPath={path}
                  controlDescription={propertyControl.value}
                  isScene={false}
                  hideUnset={false}
                />
              )
            } else {
              return null
            }
          } else {
            return null
          }
        }),
      [visibleEmptyElements, props.propertyControls],
    )

    const propNameList = React.useMemo(() => {
      if (isRight(props.propertyControls)) {
        const success = props.propertyControls.value
        const propNames = filterSpecialProps(Object.keys(success)).filter(
          (name) => !visibleEmptyElements.find((visible) => PP.toString(visible) === name),
        )
        if (propNames.length > 0) {
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
                {propNames.map((name, index) => {
                  const propertyControl = success[name]
                  if (propertyControl != null && isRight(propertyControl)) {
                    return (
                      <>
                        <HiddenControlLabel
                          key={name}
                          propPath={PP.create([name])}
                          controlDescription={propertyControl.value}
                          showHiddenControl={showHiddenControl}
                          isLast={propNames.length - 1 === index}
                        />
                      </>
                    )
                  } else {
                    return null
                  }
                })}
              </div>
            </>
          )
        } else {
          return null
        }
      } else {
        return null
      }
    }, [props.propertyControls, showHiddenControl, visibleEmptyElements])

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
  controlDescription: ControlDescription
  showHiddenControl: (path: PropertyPath) => void
  isLast: boolean
}

const HiddenControlLabel = betterReactMemo(
  'HiddenControlLabel',
  (props: HiddenControlLabelProps): JSX.Element | null => {
    const { propPath, showHiddenControl } = props
    const propMetadata = useInspectorInfoForPropertyControl(propPath, props.controlDescription)
    const labelOnClick = React.useCallback(() => showHiddenControl(propPath), [
      propPath,
      showHiddenControl,
    ])
    if (isControlHidden(propMetadata.controlStatus)) {
      return (
        <span style={{ cursor: 'pointer' }} onClick={labelOnClick}>
          {PP.toString(propPath)}
          {props.isLast ? '.' : ', '}
        </span>
      )
    } else {
      return null
    }
  },
)

export function isControlHidden(controlStatus: ControlStatus): boolean {
  return controlStatus === 'unset'
}
