import * as React from 'react'
import { v4 as UUID } from 'uuid'
import { unless, when } from '../../../utils/react-conditionals'
import {
  Button,
  colorTheme,
  FlexColumn,
  FlexRow,
  Icons,
  Tooltip,
  UtopiaStyles,
} from '../../../uuiui'
import { updateColorSwatches } from '../../editor/actions/action-creators'
import { useDispatch } from '../../editor/store/dispatch-context'
import type { ColorSwatch } from '../../editor/store/editor-state'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { cssColorToChromaColorOrDefault } from '../common/css-utils'
import { useIsMyProject } from '../../editor/store/collaborative-editing'
import { useControlsDisabledInSubtree } from '../../../uuiui/utilities/disable-subtree'

const { checkerboardBackground } = UtopiaStyles.backgrounds

export interface ColorPickerSwatchesProps {
  onSelectColor: (hex: string) => void
  currentColor: string
}

const SWATCH_SIZE = 24

export const ColorPickerSwatches = React.memo((props: ColorPickerSwatchesProps) => {
  const { currentColor, onSelectColor } = props

  const storeColorSwatches = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.colorSwatches ?? [],
    'ColorPickerSwatches color swatches',
  )

  const [editing, setEditing] = React.useState(false)
  const [colorSwatches, setColorSwatches] = React.useState<Array<ColorSwatch>>(storeColorSwatches)

  const dispatch = useDispatch()

  const isCurrentColorSaved = React.useMemo(() => {
    return colorSwatches.some((c) => c.hex === currentColor)
  }, [currentColor, colorSwatches])

  React.useEffect(() => {
    setTimeout(() => dispatch([updateColorSwatches(colorSwatches)], 'everyone'), 0)
  }, [colorSwatches, dispatch])

  React.useEffect(() => {
    if (colorSwatches.length <= 0) {
      setEditing(false)
    }
  }, [colorSwatches])

  const controlsDisabled = useControlsDisabledInSubtree()
  const disabled = controlsDisabled

  const toggleEditing = React.useCallback(() => {
    if (disabled) {
      return
    }
    setEditing(!editing)
  }, [editing, disabled])

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
            highlightAlways
            highlightOnHover
            style={{
              padding: '0 6px',
            }}
            disabled={disabled}
            onMouseDown={toggleEditing}
          >
            {when(editing, <Icons.Checkmark />)}
            {unless(editing, <Icons.EditPencil />)}
          </Button>
        )}
      </FlexRow>

      <FlexRow style={{ flexWrap: 'wrap', gap: 2 }}>
        {colorSwatches.map((c) => {
          const [r, g, b, a] = cssColorToChromaColorOrDefault({ type: 'Hex', hex: c.hex }).rgba()
          const rgbString = `rgba(${r}, ${g}, ${b})`
          const rgbaString = `rgba(${r}, ${g}, ${b}, ${a})`
          return (
            <Tooltip placement='top' title={c.hex} key={c.id}>
              <Button
                onClick={onClickSwatch(c)}
                title={editing ? 'Remove color' : ''}
                style={{
                  width: SWATCH_SIZE,
                  height: SWATCH_SIZE,
                  backgroundImage: `linear-gradient(to bottom right, transparent 65%, ${rgbString} 65%), linear-gradient(${rgbaString}, ${rgbaString}), ${checkerboardBackground.backgroundImage}`,
                  backgroundSize: `100% 100%, ${checkerboardBackground.backgroundSize}`,
                  backgroundPosition: `0 0, ${checkerboardBackground.backgroundPosition}`,
                  border: `1px solid ${colorTheme.bg3.value}`,
                  boxShadow: currentColor === c.hex ? `${c.hex} 0px 0px 3px` : 'none',
                  borderRadius: 2,
                }}
              >
                {when(editing, <Icons.Bin />)}
              </Button>
            </Tooltip>
          )
        })}

        {!editing && (
          <Button
            highlightAlways
            highlightOnHover
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
