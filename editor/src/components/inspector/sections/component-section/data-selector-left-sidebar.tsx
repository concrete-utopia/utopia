import React from 'react'
import type { ElementPath } from 'utopia-shared/src/types'
import * as EP from '../../../../core/shared/element-path'
import { FlexColumn, colorTheme } from '../../../../uuiui'
import {
  insertionCeilingToString,
  insertionCeilingsEqual,
  type FileRootPath,
} from '../../../canvas/ui-jsx-canvas'
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
    <FlexColumn
      style={{
        alignSelf: 'stretch',
        minWidth: 150,
        paddingRight: 8,
        borderRight: `1px solid ${colorTheme.subduedBorder.cssValue}`,
      }}
    >
      {scopes.map((scope) => {
        return (
          <ScopeRow
            key={insertionCeilingToString(scope.scope)}
            scope={scope}
            selected={insertionCeilingsEqual(scope.scope, activeScope)}
            setSelectedScope={setSelectedScope}
          />
        )
      })}
    </FlexColumn>
  )
})

const ScopeRow = React.memo(
  (props: {
    scope: SelectableScope
    selected: boolean
    setSelectedScope: (scope: ElementPath | FileRootPath) => void
  }) => {
    const { scope, selected, setSelectedScope } = props

    const onClick = React.useCallback(
      (e: React.MouseEvent<HTMLDivElement>) => {
        e.stopPropagation()
        setSelectedScope(scope.scope)
      },
      [setSelectedScope, scope.scope],
    )

    return (
      <DataPickerRow
        style={{
          backgroundColor: selected ? colorTheme.bg4.value : undefined,
        }}
        onClick={onClick}
      >
        {scope.label}
      </DataPickerRow>
    )
  },
)
