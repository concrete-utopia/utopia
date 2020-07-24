import * as React from 'react'
import { GenericExternalResource } from '../../../printer-parsers/html/external-resources-parser'
import { betterReactMemo } from '../../../uuiui-deps'
import { GridRow } from '../../inspector/widgets/grid-row'
import { ResourcesListGridRowConfig } from './generic-external-resources-list'

interface GenericExternalResourcesListItemProps {
  value: GenericExternalResource
  index: number
  setEditingIndex: React.Dispatch<number | null>
}

export const GenericExternalResourcesListItem = betterReactMemo<
  GenericExternalResourcesListItemProps
>('GenericExternalResourcesListItem', ({ value, index, setEditingIndex }) => {
  const onDoubleClick = React.useCallback(() => {
    setEditingIndex(index)
  }, [index, setEditingIndex])
  return (
    <GridRow {...ResourcesListGridRowConfig} onDoubleClick={onDoubleClick}>
      <div
        style={{
          textOverflow: 'ellipsis',
          overflow: 'hidden',
          whiteSpace: 'nowrap',
        }}
      >
        {value.href}
      </div>
      <div
        style={{
          textOverflow: 'ellipsis',
          overflow: 'hidden',
          whiteSpace: 'nowrap',
          textAlign: 'right',
          fontStyle: 'italic',
          paddingRight: 1,
        }}
      >
        {value.rel}
      </div>
    </GridRow>
  )
})
