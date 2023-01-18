import * as React from 'react'
import { Button } from '../../../uuiui'
import { v4 as UUID } from 'uuid'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { useDispatch } from '../../editor/store/dispatch-context'
import { updateColorPresets } from '../../editor/actions/action-creators'
import { TrashIcon } from './color-picker-presets-icons'

export interface ColorPickerPresetsProps {
  onSelectColor: (hex: string) => void
  currentColor: string
}

export type ColorPreset = {
  id: string
  hex: string
}

export function newColorPreset(id: string, hex: string): ColorPreset {
  return {
    id: id,
    hex: hex,
  }
}

export const ColorPickerPresets = (props: ColorPickerPresetsProps) => {
  const { currentColor, onSelectColor } = props

  const storeColorPresets = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.colorPresets,
    'ColorPickerPresets color presets',
  )

  const [editing, setEditing] = React.useState(false)
  const [colorPresets, setColorPresets] = React.useState<Array<ColorPreset>>(storeColorPresets)

  const dispatch = useDispatch()

  React.useEffect(() => {
    dispatch([updateColorPresets(colorPresets)], 'everyone')
  }, [colorPresets, dispatch])

  React.useEffect(() => {
    if (colorPresets.length <= 0) {
      setEditing(false)
    }
  }, [colorPresets])

  const toggleEditing = React.useCallback(() => {
    setEditing(!editing)
  }, [editing])

  const onAddPreset = React.useCallback(() => {
    const preset: ColorPreset = {
      id: UUID(),
      hex: currentColor,
    }
    setColorPresets(colorPresets.concat(preset))
  }, [currentColor, colorPresets])

  const onClickPreset = React.useCallback(
    (c: ColorPreset) => {
      if (editing) {
        setColorPresets(colorPresets.filter((p) => p.id != c.id))
      } else {
        onSelectColor(c.hex)
      }
    },
    [editing, onSelectColor, colorPresets],
  )

  return (
    <div
      style={{
        padding: '0px 8px 4px 8px',
        marginTop: 4,
        display: 'flex',
        flexDirection: 'column',
        gap: 4,
      }}
    >
      <div
        style={{
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'space-between',
        }}
      >
        <div
          style={{
            fontWeight: 600,
          }}
        >
          Presets
        </div>
        {colorPresets.length > 0 && (
          <Button spotlight highlight style={{ padding: '0 6px' }} onMouseDown={toggleEditing}>
            {editing ? 'Done' : 'Edit'}
          </Button>
        )}
      </div>
      <div style={{ display: 'flex', gap: 4, alignItems: 'center', flexWrap: 'wrap' }}>
        {colorPresets.map((c, index) => {
          return (
            <div
              onClick={() => onClickPreset(c)}
              key={index}
              style={{
                background: c.hex,
                border: '1px solid #000',
                width: 26,
                height: 26,
                borderRadius: 2,
                cursor: 'pointer',
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center',
              }}
            >
              {editing && <TrashIcon />}
            </div>
          )
        })}
        {!editing && (
          <Button spotlight highlight style={{ padding: '0 6px' }} onMouseDown={onAddPreset}>
            Add
          </Button>
        )}
      </div>
    </div>
  )
}
