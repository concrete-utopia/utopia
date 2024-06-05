/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { jsx } from '@emotion/react'
import { unless, when } from '../../../../utils/react-conditionals'
import type { CSSCursor } from '../../../canvas/canvas-types'
import type {
  ControlDescription,
  PropertyControls,
} from '../../../custom-code/internal-property-controls'
import { inferControlTypeBasedOnValue } from './component-section-utils'
import { HiddenControls } from './hidden-controls-section'
import * as PP from '../../../../core/shared/property-path'
import { useColorTheme } from '../../../../uuiui'
import { RowOrFolderWrapper } from './row-or-folder-wrapper'
import { RowForControl } from './component-section'
import {
  isAdvancedFolderLabel,
  specialPropertiesToIgnore,
} from '../../../../core/property-controls/property-controls-utils'

interface FolderSectionProps {
  isRoot: boolean
  propertyControls: PropertyControls
  indentationLevel: number
  visibleEmptyControls: string[]
  unsetPropNames: string[]
  detectedPropsAndValuesWithoutControls: Record<string, unknown>
  setGlobalCursor: (cursor: CSSCursor | null) => void
  showHiddenControl: (path: string) => void
  title?: string
  propsToIgnore: string[]
}

export const FolderSection = React.memo((props: FolderSectionProps) => {
  const [open, setOpen] = React.useState(!isAdvancedFolderLabel(props.title))
  const colorTheme = useColorTheme()
  const hiddenPropsList = React.useMemo(
    () =>
      Object.keys(props.propertyControls).filter((prop) => {
        const control = props.propertyControls[prop]
        const isVisibleByDefault = control.visibleByDefault ?? true
        return (
          !isVisibleByDefault &&
          props.unsetPropNames.includes(prop) &&
          !props.visibleEmptyControls.includes(prop)
        )
      }),
    [props.unsetPropNames, props.visibleEmptyControls, props.propertyControls],
  )

  const controlsWithValue = React.useMemo(
    () =>
      Object.keys(props.propertyControls).filter((prop) => {
        return !hiddenPropsList.includes(prop) && !props.visibleEmptyControls.includes(prop)
      }),
    [props.propertyControls, props.visibleEmptyControls, hiddenPropsList],
  )
  const emptyControls = React.useMemo(
    () =>
      props.visibleEmptyControls.filter((prop) => {
        return Object.keys(props.propertyControls).includes(prop)
      }),
    [props.propertyControls, props.visibleEmptyControls],
  )

  const toggleOpen = React.useCallback(() => {
    setOpen(!open)
  }, [open, setOpen])

  const cssHoverEffect = React.useMemo(
    () =>
      props.isRoot
        ? {}
        : {
            '&:hover': {
              boxShadow: `inset 1px 0px 0px 0px ${colorTheme.fg7.value}`,
              background: 'hsl(0,0%,0%,1%)',
            },
            '&:focus-within': {
              boxShadow: `inset 1px 0px 0px 0px ${colorTheme.fg7.value}`,
              background: 'hsl(0,0%,0%,1%)',
            },
          },
    [props.isRoot, colorTheme],
  )

  const createRowForControl = (propName: string, focusOnMount: boolean) => {
    const controlDescription = props.propertyControls[propName]
    return (
      <RowOrFolderWrapper
        key={`section-row-${propName}`}
        propPath={PP.create(propName)}
        controlDescription={controlDescription}
        isScene={false}
        setGlobalCursor={props.setGlobalCursor}
        indentationLevel={props.indentationLevel + 1}
        visibleEmptyControls={props.visibleEmptyControls}
        unsetPropNames={props.unsetPropNames}
        showHiddenControl={props.showHiddenControl}
        focusOnMount={focusOnMount}
      />
    )
  }

  return (
    <div css={cssHoverEffect}>
      {unless(
        props.isRoot,
        <FolderLabel
          indentationLevel={1}
          showIndentation={true}
          open={open}
          toggleOpen={toggleOpen}
          title={props.title ?? ''}
        />,
      )}
      {when(
        open,
        controlsWithValue.map((control) => createRowForControl(control, false)),
      )}
      {when(
        props.isRoot,
        Object.keys(props.detectedPropsAndValuesWithoutControls).map((propName) => {
          if (props.propsToIgnore.includes(propName)) {
            return null
          } else {
            const propValue = props.detectedPropsAndValuesWithoutControls[propName]
            const controlDescription: ControlDescription = inferControlTypeBasedOnValue(
              propValue,
              propName,
            )
            return (
              <RowForControl
                key={propName}
                propPath={PP.create(propName)}
                controlDescription={controlDescription}
                isScene={false}
                setGlobalCursor={props.setGlobalCursor}
                indentationLevel={props.indentationLevel + 1}
                showHiddenControl={props.showHiddenControl}
                focusOnMount={false}
              />
            )
          }
        }),
      )}
      {when(
        open,
        emptyControls.map((control) => createRowForControl(control, true)),
      )}
      {when(
        open,
        <HiddenControls
          hiddenPropNames={hiddenPropsList}
          showHiddenControl={props.showHiddenControl}
          indentationLevel={props.indentationLevel + 1}
        />,
      )}
    </div>
  )
})

interface FolderLabelProps {
  indentationLevel: number
  open: boolean
  toggleOpen: () => void
  title: string
  showIndentation: boolean
}

const FolderLabel = React.memo((props: FolderLabelProps) => {
  const { toggleOpen } = props
  const indentation = props.showIndentation ? props.indentationLevel * 8 : 0
  const handleOnClick = React.useCallback(() => toggleOpen(), [toggleOpen])
  return (
    <div
      style={{
        paddingLeft: indentation,
        display: 'flex',
        alignItems: 'center',
        height: 34,
        fontWeight: 500,
        gap: 4,
        cursor: 'pointer',
        transition: 'padding-left 100ms ease-in-out',
      }}
      onClick={handleOnClick}
    >
      <ExpansionArrowSVG
        style={{
          transform: props.open ? 'none' : 'rotate(-90deg)',
          transition: 'all linear .1s',
        }}
      />
      <span>{props.title}</span>
    </div>
  )
})

interface ExpansionArrowSVGProps {
  style: React.CSSProperties
}

const ExpansionArrowSVG = React.memo((props: ExpansionArrowSVGProps) => {
  const colorTheme = useColorTheme()
  return (
    <svg width='7px' height='5px' viewBox='0 0 7 5' version='1.1' style={props.style}>
      <g
        strokeWidth='1'
        fillRule='evenodd'
        strokeLinecap='round'
        strokeLinejoin='round'
        id='expansion-triangle-open'
        transform='translate(-1.000000, 0.000000)'
        fill={colorTheme.textColor.value}
        stroke={colorTheme.textColor.value}
      >
        <polygon
          transform='translate(3.828427, 0.828427) rotate(-45.000000) translate(-3.828427, -0.828427) '
          points='1.82842712 -1.17157288 1.82842712 2.82842712 5.82842712 2.82842712'
        />
      </g>
    </svg>
  )
})
