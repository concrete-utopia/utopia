/**@jsx jsx */
import React from 'react'
import { css, jsx } from '@emotion/react'
import { ParsedPropertyControls } from '../../../../core/property-controls/property-controls-parser'
import { eitherToMaybe, foldEither } from '../../../../core/shared/either'
import { unless, when } from '../../../../utils/react-conditionals'
import { betterReactMemo } from '../../../../uuiui-deps'
import { CSSCursor } from '../../../canvas/canvas-types'
import { ControlDescription } from 'utopia-api'
import { inferControlTypeBasedOnValue } from './component-section-utils'
import { HiddenControls } from './hidden-controls-section'
import * as PP from '../../../../core/shared/property-path'
import * as EP from '../../../../core/shared/element-path'
import { useColorTheme } from '../../../../uuiui'
import { RowOrFolderWrapper } from './row-or-folder-wrapper'
import { RowForControl, RowForInvalidControl } from './component-section'

interface FolderSectionProps {
  isRoot: boolean
  parsedPropertyControls: ParsedPropertyControls
  indentationLevel: number
  visibleEmptyControls: string[]
  unsetPropNames: string[]
  detectedPropsAndValuesWithoutControls: Record<string, unknown>
  setGlobalCursor: (cursor: CSSCursor | null) => void
  showHiddenControl: (path: string) => void
  title?: string
}

export const FolderSection = betterReactMemo('FolderSection', (props: FolderSectionProps) => {
  const [open, setOpen] = React.useState(true)
  const colorTheme = useColorTheme()
  const hiddenPropsList = React.useMemo(
    () =>
      Object.keys(props.parsedPropertyControls).filter((prop) => {
        const isNotFolder = eitherToMaybe(props.parsedPropertyControls[prop])?.type !== 'folder'
        return (
          isNotFolder &&
          props.unsetPropNames.includes(prop) &&
          !props.visibleEmptyControls.includes(prop)
        )
      }),
    [props.unsetPropNames, props.visibleEmptyControls, props.parsedPropertyControls],
  )

  const controlsWithValue = React.useMemo(
    () =>
      Object.keys(props.parsedPropertyControls).filter((prop) => {
        return !props.unsetPropNames.includes(prop) && !props.visibleEmptyControls.includes(prop)
      }),
    [props.parsedPropertyControls, props.unsetPropNames, props.visibleEmptyControls],
  )
  const emptyControls = React.useMemo(
    () =>
      props.visibleEmptyControls.filter((prop) => {
        return Object.keys(props.parsedPropertyControls).includes(prop)
      }),
    [props.parsedPropertyControls, props.visibleEmptyControls],
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

  const createRowForControl = (propName: string) => {
    const controlDescription = props.parsedPropertyControls[propName]
    return foldEither(
      (propertyError) => {
        return (
          <RowForInvalidControl
            key={propName}
            title={propName}
            propName={propName}
            propertyError={propertyError}
          />
        )
      },
      (propertySuccess) => {
        return (
          <RowOrFolderWrapper
            key={`section-row-${propName}`}
            propPath={PP.create([propName])}
            controlDescription={propertySuccess}
            isScene={false}
            setGlobalCursor={props.setGlobalCursor}
            indentationLevel={props.indentationLevel + 1}
            visibleEmptyControls={props.visibleEmptyControls}
            unsetPropNames={props.unsetPropNames}
            showHiddenControl={props.showHiddenControl}
          />
        )
      },
      controlDescription,
    )
  }

  return (
    <div css={cssHoverEffect}>
      {unless(
        props.isRoot,
        <FolderLabel
          indentationLevel={props.indentationLevel}
          open={open}
          toggleOpen={toggleOpen}
          title={props.title ?? ''}
        />,
      )}
      {when(open, controlsWithValue.map(createRowForControl))}
      {when(
        props.isRoot,
        Object.keys(props.detectedPropsAndValuesWithoutControls).map((propName) => {
          const propValue = props.detectedPropsAndValuesWithoutControls[propName]
          const controlDescription: ControlDescription = inferControlTypeBasedOnValue(
            propValue,
            propName,
          )
          return (
            <RowForControl
              key={propName}
              propPath={PP.create([propName])}
              controlDescription={controlDescription}
              isScene={false}
              setGlobalCursor={props.setGlobalCursor}
              indentationLevel={props.indentationLevel + 1}
            />
          )
        }),
      )}
      {when(open, emptyControls.map(createRowForControl))}
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
}

const FolderLabel = betterReactMemo('FolderLabel', (props: FolderLabelProps) => {
  const { toggleOpen } = props
  const indentation = props.indentationLevel * 8
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

const ExpansionArrowSVG = betterReactMemo('ExpansionArrowSVG', (props: ExpansionArrowSVGProps) => {
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
