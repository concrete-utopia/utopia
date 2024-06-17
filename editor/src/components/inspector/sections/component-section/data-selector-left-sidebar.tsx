import React from 'react'
import type { ElementPath } from 'utopia-shared/src/types'
import { CheckboxInput, FlexColumn, FlexRow, LargerIcons, colorTheme } from '../../../../uuiui'
import {
  insertionCeilingToString,
  insertionCeilingsEqual,
  type FileRootPath,
} from '../../../canvas/ui-jsx-canvas'
import { stopPropagation } from '../../common/inspector-utils'
import { NO_OP } from '../../../../core/shared/utils'

interface SelectableScope {
  label: string
  scope: ElementPath | FileRootPath
  hasContent: boolean
}

interface DataSelectorLeftSidebarProps {
  scopes: Array<SelectableScope>
  activeScope: ElementPath | FileRootPath
  setSelectedScope: (scope: ElementPath | FileRootPath) => void
}

export const DataSelectorLeftSidebar = React.memo((props: DataSelectorLeftSidebarProps) => {
  const { scopes, setSelectedScope, activeScope } = props
  return (
    <FlexColumn
      onClick={stopPropagation}
      style={{
        alignSelf: 'stretch',
        minWidth: 120,
        padding: 16,
        borderRight: `1px solid ${colorTheme.subduedBorder.cssValue}`,
        contain: 'layout',
        gap: 8,
      }}
    >
      <span style={{ fontSize: 10, fontWeight: 500, color: colorTheme.fg5.cssValue }}>Sources</span>
      <FlexColumn>
        {scopes.map((scope) => {
          return (
            <ScopeRow
              key={insertionCeilingToString(scope.scope)}
              scope={scope}
              selected={insertionCeilingsEqual(scope.scope, activeScope)}
              disabled={!scope.hasContent}
              setSelectedScope={setSelectedScope}
            />
          )
        })}
      </FlexColumn>
      <span
        style={{
          marginTop: 16,
          fontSize: 10,
          fontWeight: 500,
          color: colorTheme.fg5.cssValue,
        }}
      >
        Origin
      </span>
      <FlexColumn>
        <RowWithCheckbox
          label={'Local'}
          color={colorTheme.selectionBlue.value}
          selected={true}
          disabled={false}
          onClick={NO_OP}
        />
        <RowWithCheckbox
          label={'Loader'}
          color={colorTheme.green.value}
          selected={true}
          disabled={false}
          onClick={NO_OP}
        />
      </FlexColumn>
    </FlexColumn>
  )
})

const ScopeRow = React.memo(
  (props: {
    scope: SelectableScope
    selected: boolean
    disabled: boolean
    setSelectedScope: (scope: ElementPath | FileRootPath) => void
  }) => {
    const { scope, selected, disabled, setSelectedScope } = props

    const onClick = React.useCallback(
      (e: React.MouseEvent<HTMLDivElement>) => {
        e.stopPropagation()
        if (!disabled) {
          setSelectedScope(scope.scope)
        }
      },
      [setSelectedScope, disabled, scope.scope],
    )

    return (
      <RowWithCheckbox
        label={scope.label}
        selected={selected}
        disabled={disabled}
        onClick={onClick}
      />
    )
  },
)

interface RowWithCheckboxProps {
  label: string
  color?: string
  selected: boolean
  disabled: boolean
  onClick: (e: React.MouseEvent<HTMLDivElement>) => void
}

const RowWithCheckbox = React.memo((props: RowWithCheckboxProps) => {
  const { label, color, selected, disabled, onClick } = props
  return (
    <FlexRow
      style={{
        justifyContent: 'flex-start',
        gap: 8,
        fontSize: 11,
        fontWeight: 400,
        color: color ?? (disabled ? colorTheme.fg6.value : colorTheme.fg2.value),
      }}
      onClick={onClick}
    >
      <CheckboxInput
        style={{
          backgroundColor: colorTheme.bg0.value,
          border: `1px solid ${colorTheme.fg4.cssValue}`,
        }}
        checked={selected}
        onChange={stopPropagation}
      />
      {label}
    </FlexRow>
  )
})
