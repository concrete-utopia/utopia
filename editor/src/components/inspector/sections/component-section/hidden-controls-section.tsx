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
  const result = pathNames.filter(
    (name) => !visibleEmptyControls.some((visible) => PP.toString(visible) === name),
  )
  return result
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
    const visibleEmptyControls = React.useMemo(() => {
      const visibleEmptyControlsSet = new Set(props.visibleEmptyControls.map(PP.toString))
      return foldEither(
        () => {
          return null
        },
        (propertyControlsSuccess) => {
          return (
            <>
              {Object.keys(propertyControlsSuccess).map((propName) => {
                const propertyControl = propertyControlsSuccess[propName]
                return foldEither(
                  (propertyError) => {
                    return null
                  },
                  (propertySuccess) => {
                    return (
                      <SectionRow
                        key={propName}
                        propPath={PP.create([propName])}
                        isScene={false}
                        setGlobalCursor={props.setGlobalCursor}
                        controlDescription={propertySuccess}
                        propNamesToDisplay={visibleEmptyControlsSet}
                        indentationLevel={1}
                      />
                    )
                  },
                  propertyControl,
                )
              })}
            </>
          )
        },
        props.propertyControls,
      )
    }, [props.visibleEmptyControls, props.propertyControls, props.setGlobalCursor])

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
