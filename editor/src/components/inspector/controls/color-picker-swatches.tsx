import * as React from 'react'
import { v4 as UUID } from 'uuid'
import { unless, when } from '../../../utils/react-conditionals'
import { Button, colorTheme, FlexColumn, FlexRow, Icons } from '../../../uuiui'
import { updateColorSwatches } from '../../editor/actions/action-creators'
import { useDispatch } from '../../editor/store/dispatch-context'
import { ColorSwatch } from '../../editor/store/editor-state'
import { Substores, useEditorState } from '../../editor/store/store-hook'

export interface ColorPickerSwatchesProps {
  onSelectColor: (hex: string) => void
  currentColor: string
}

const SWATCH_SIZE = 24

export const ColorPickerSwatches = React.memo((props: ColorPickerSwatchesProps) => {
  const { currentColor, onSelectColor } = props

  const storeColorSwatches = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.colorSwatches,
    'ColorPickerSwatches color swatches',
  )

  const [editing, setEditing] = React.useState(false)
  const [colorSwatches, setColorSwatches] = React.useState<Array<ColorSwatch>>(storeColorSwatches)

  const dispatch = useDispatch()

  const isCurrentColorSaved = React.useMemo(() => {
    return colorSwatches.some((c) => c.hex === currentColor)
  }, [currentColor, colorSwatches])

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

  const onAddColor = React.useCallback(() => {
    if (currentColor == null) {
      return
    }
    if (isCurrentColorSaved) {
      return
    }
    const swatch: ColorSwatch = {
      id: UUID(),
      hex: currentColor,
    }
    setColorSwatches(colorSwatches.concat(swatch))
  }, [currentColor, colorSwatches, isCurrentColorSaved])

  const onClickSwatch = React.useCallback(
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
        <div
          style={{
            fontWeight: 600,
            color: colorTheme.fg0.value,
          }}
        >
          Project colors
        </div>
        {colorSwatches.length > 0 && (
          <Button
            spotlight
            highlight
            style={{
              padding: '0 6px',
            }}
            onMouseDown={toggleEditing}
          >
            {when(editing, <Icons.Checkmark />)}
            {unless(editing, <Icons.EditPencil />)}
          </Button>
        )}
      </FlexRow>

      <FlexRow style={{ flexWrap: 'wrap', gap: 2 }}>
        {colorSwatches.map((c) => {
          return (
            <Button
              onClick={onClickSwatch(c)}
              key={c.id}
              title={editing ? 'Remove color' : ''}
              style={{
                position: 'relative',
                width: SWATCH_SIZE,
                height: SWATCH_SIZE,
                background: c.hex,
                border: `1px solid ${
                  currentColor === c.hex ? colorTheme.primary.value : colorTheme.border0.value
                }`,
                borderRadius: 2,
                zIndex: 0,
              }}
            >
              {when(editing, <Icons.Bin />)}
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
            onMouseDown={onAddColor}
            disabled={isCurrentColorSaved}
          >
            <Icons.Plus />
          </Button>
        )}
      </FlexRow>
    </FlexColumn>
  )
})
