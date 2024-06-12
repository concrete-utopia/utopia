import React from 'react'
import type { ElementPath } from 'utopia-shared/src/types'
import { FlexColumn, FlexRow, LargerIcons, colorTheme } from '../../../../uuiui'
import {
  insertionCeilingToString,
  insertionCeilingsEqual,
  type FileRootPath,
} from '../../../canvas/ui-jsx-canvas'
import { stopPropagation } from '../../common/inspector-utils'
import { DataPickerRow } from './data-selector-columns'

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
    <>
      <span style={{ fontSize: 10, fontWeight: 'bold', color: colorTheme.fg4.cssValue }}>
        Sources
      </span>
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
    </>
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
      <DataPickerRow
        style={{
          backgroundColor: selected ? colorTheme.bg4.value : undefined,
          color: disabled ? colorTheme.fg6.value : colorTheme.neutralForeground.value,
        }}
        onClick={onClick}
        disabled={disabled}
      >
        {scope.label}
      </DataPickerRow>
    )
  },
)
