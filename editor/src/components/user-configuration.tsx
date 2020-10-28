/** @jsx jsx */
import * as React from 'react'
import { jsx } from '@emotion/core'
import { Table, Checkbox } from 'antd'
import Column from 'antd/lib/table/Column'
import 'antd/dist/antd.css'
import { RenderedCell, GetComponentProps } from 'rc-table/lib/interface'
import { useEditorState } from './editor/store/store-hook'
import { getShortcutDetails, Shortcut } from './editor/shortcut-definitions'
import { comparePrimitive } from '../utils/compare'
import { mapToArray } from '../core/shared/object-utils'
import { Key } from '../utils/keyboard'
import { capitalize } from '../core/shared/string-utils'
import Keyboard from '../utils/keyboard'
import { setShortcut } from './editor/actions/actions'

interface ShortcutWithName extends Shortcut {
  name: string
}

interface DataSourceEntry {
  description: string
  shortcut: Array<Key>
  keydown: boolean
  name: string
}

export function UserConfiguration() {
  const { dispatch, shortcutConfig } = useEditorState((store) => {
    return {
      dispatch: store.dispatch,
      shortcutConfig: store.userState.shortcutConfig,
    }
  }, 'UserConfiguration')

  const [editingIndex, setEditingIndex] = React.useState<number | null>(null)

  const dataSource: Array<DataSourceEntry> = React.useMemo(() => {
    const detailsMap = getShortcutDetails(shortcutConfig)
    let detailsArray: Array<ShortcutWithName> = mapToArray((shortcut, name) => {
      return {
        ...shortcut,
        name: name,
      }
    }, detailsMap)
    detailsArray.sort((first, second) => comparePrimitive(first.description, second.description))
    return detailsArray.map((shortcut) => {
      return {
        description: shortcut.description,
        shortcut: shortcut.shortcutKeys,
        // TODO: Possibly this needs including the shortcut column somehow.
        keydown: shortcut.shortcutKeys.every((key) => key.keyDownOrUp === 'keydown'),
        name: shortcut.name,
      }
    })
  }, [shortcutConfig])

  const renderKeyDownField = React.useCallback(
    (value: boolean, record: DataSourceEntry, index: number) => {
      function onChange() {
        const newKeys: Array<Key> = record.shortcut.map((key) => {
          return {
            ...key,
            keyDownOrUp: key.keyDownOrUp === 'keydown' ? 'keyup' : 'keydown',
          }
        })
        const actions = newKeys.map((key) => setShortcut(record.name, key))
        dispatch(actions, 'everyone')
      }
      return <Checkbox checked={value} onChange={onChange} />
    },
    [dispatch],
  )

  const setEditingShortcut = React.useCallback(
    (event: React.KeyboardEvent) => {
      event.stopPropagation()
      event.preventDefault()
      if (editingIndex != null) {
        const nativeEvent = event.nativeEvent
        const newShortcut = Keyboard.keyFromEvent(nativeEvent)
        switch (newShortcut.character) {
          case 'shift':
          case 'ctrl':
          case 'alt':
          case 'cmd':
            // Need to ignore modifiers.
            break
          default:
            if (editingIndex in dataSource) {
              const dataEntry = dataSource[editingIndex]
              dispatch([setShortcut(dataEntry.name, newShortcut)], 'everyone')
              setEditingIndex(null)
            }
        }
      }
    },
    [editingIndex, setEditingIndex, dispatch, dataSource],
  )

  const renderKeyAndModifiers = React.useCallback(
    (
      value: [Key],
      record: DataSourceEntry,
      index: number,
    ): React.ReactNode | RenderedCell<DataSourceEntry> => {
      if (index === editingIndex) {
        return (
          <input
            type='text'
            autoFocus={true}
            style={{
              padding: '0px',
              border: '0px',
              caretColor: 'transparent',
            }}
            onKeyDown={setEditingShortcut}
            value='<Press New Shortcut>'
          />
        )
      } else {
        const renderedKeys = value.map((key) => {
          if (key.modifiers.length === 0) {
            return capitalize(key.character)
          } else {
            return `${key.modifiers.map(capitalize).join('+')}+${capitalize(key.character)}`
          }
        })
        return renderedKeys.join(', ')
      }
    },
    [setEditingShortcut, editingIndex],
  )

  const onRow: GetComponentProps<DataSourceEntry> = React.useCallback(
    (data: DataSourceEntry, index?: number) => {
      return {
        onDoubleClick: (event: React.MouseEvent) => {
          setEditingIndex(index ?? null)
        },
      }
    },
    [setEditingIndex],
  )

  return (
    <Table dataSource={dataSource} pagination={false} onRow={onRow}>
      <Column title='Description' key='description' dataIndex='description' />
      <Column title='Shortcut' key='shortcut' dataIndex='shortcut' render={renderKeyAndModifiers} />
      <Column title='Key Down' key='keydown' dataIndex='keydown' render={renderKeyDownField} />
    </Table>
  )
}
