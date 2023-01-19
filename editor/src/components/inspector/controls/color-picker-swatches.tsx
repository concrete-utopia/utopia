import * as React from 'react'
import { Button, colorTheme, FlexColumn, FlexRow } from '../../../uuiui'
import { v4 as UUID } from 'uuid'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { useDispatch } from '../../editor/store/dispatch-context'
import { updateColorSwatches } from '../../editor/actions/action-creators'

export interface ColorPickerSwatchesProps {
  onSelectColor: (hex: string) => void
  currentColor: string
}

export type ColorSwatch = {
  id: string
  hex: string
}

export function newColorSwatch(id: string, hex: string): ColorSwatch {
  return {
    id: id,
    hex: hex,
  }
}

const SWATCH_SIZE = 24

export const ColorPickerSwatches = React.memo((props: ColorPickerSwatchesProps) => {
  const { currentColor, onSelectColor } = props

  const storeColorSwatches = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.colorSwatches,
    'ColorPickerPresets color presets',
  )

  const [editing, setEditing] = React.useState(false)
  const [colorSwatches, setColorSwatches] = React.useState<Array<ColorSwatch>>(storeColorSwatches)

  const dispatch = useDispatch()

  React.useEffect(() => {
    dispatch([updateColorSwatches(colorSwatches)], 'everyone')
  }, [colorSwatches, dispatch])

  React.useEffect(() => {
    if (colorSwatches.length <= 0) {
      setEditing(false)
    }
  }, [colorSwatches])

  const toggleEditing = React.useCallback(() => {
    setEditing(!editing)
  }, [editing])

  const onAddPreset = React.useCallback(() => {
    const preset: ColorSwatch = {
      id: UUID(),
      hex: currentColor,
    }
    setColorSwatches(colorSwatches.concat(preset))
  }, [currentColor, colorSwatches])

  const onClickPreset = React.useCallback(
    (c: ColorSwatch) => () => {
      if (editing) {
        setColorSwatches(colorSwatches.filter((p) => p.id != c.id))
      } else {
        onSelectColor(c.hex)
      }
    },
    [editing, onSelectColor, colorSwatches],
  )

  return (
    <FlexColumn
      style={{
        padding: '0px 8px 4px 8px',
        marginTop: 4,
        gap: 4,
      }}
    >
      <FlexRow style={{ justifyContent: 'space-between' }}>
        <div style={{ fontWeight: 600 }}>Swatches</div>
        {colorSwatches.length > 0 && (
          <Button spotlight highlight style={{ padding: '0 6px' }} onMouseDown={toggleEditing}>
            {editing ? 'Done' : 'Edit'}
          </Button>
        )}
      </FlexRow>

      <FlexRow style={{ flexWrap: 'wrap', gap: 2 }}>
        {colorSwatches.map((c, index) => {
          return (
            <Button
              onClick={onClickPreset(c)}
              key={index}
              title={editing ? 'Remove color' : ''}
              style={{
                position: 'relative',
                width: SWATCH_SIZE,
                height: SWATCH_SIZE,
                background: c.hex,
                border: `1px solid ${
                  currentColor === c.hex ? colorTheme.fg0.value : colorTheme.border0.value
                }`,
                borderRadius: 2,
                zIndex: 0,
              }}
            >
              {editing && <TrashIcon />}
              {!editing && currentColor === c.hex && <CurrentColorBadge />}
            </Button>
          )
        })}

        {!editing && (
          <Button
            spotlight
            highlight
            style={{
              padding: '0 6px',
              width: SWATCH_SIZE,
              height: SWATCH_SIZE,
            }}
            onMouseDown={onAddPreset}
          >
            <PlusIcon />
          </Button>
        )}
      </FlexRow>
    </FlexColumn>
  )
})

const CurrentColorBadge = React.memo(() => {
  return (
    <div
      style={{
        position: 'absolute',
        top: -2,
        right: -2,
        width: 7,
        height: 7,
        background: colorTheme.primary.value,
        border: `1px solid ${colorTheme.border0.value}`,
        borderRadius: '100%',
        zIndex: 1,
        boxShadow: `${colorTheme.canvasControlsSizeBoxShadowColor20.value} 0px 0px 1px, ${colorTheme.canvasControlsSizeBoxShadowColor21.value} 0px 1px 2px 1px`,
      }}
    />
  )
})

const TrashIcon: React.FC = () => {
  return (
    <FlexColumn>
      <svg
        xmlns='http://www.w3.org/2000/svg'
        width='12'
        height='12'
        viewBox='0 0 24 24'
        fill={colorTheme.bg0.value}
        stroke={colorTheme.fg0.value}
        strokeWidth='2'
        strokeLinecap='round'
        strokeLinejoin='round'
      >
        <polyline points='3 6 5 6 21 6'></polyline>
        <path d='M19 6v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6m3 0V4a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v2'></path>
        <line x1='10' y1='11' x2='10' y2='17'></line>
        <line x1='14' y1='11' x2='14' y2='17'></line>
      </svg>
    </FlexColumn>
  )
}

const PlusIcon: React.FC = () => {
  return (
    <FlexColumn>
      <svg
        xmlns='http://www.w3.org/2000/svg'
        width='14'
        height='14'
        viewBox='0 0 24 24'
        fill={colorTheme.bg0.value}
        stroke={colorTheme.fg0.value}
        strokeWidth='2'
        strokeLinecap='round'
        strokeLinejoin='round'
      >
        <line x1='12' y1='5' x2='12' y2='19'></line>
        <line x1='5' y1='12' x2='19' y2='12'></line>
      </svg>
    </FlexColumn>
  )
}
