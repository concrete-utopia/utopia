import { atom, useAtom } from 'jotai'
import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import type { CanvasRectangle } from '../../../../core/shared/math-utils'
import { canvasRectangle, nullIfInfinity } from '../../../../core/shared/math-utils'
import { FlexRow, useColorTheme, UtopiaStyles, UtopiaTheme } from '../../../../uuiui'
import { useEditorState, Substores } from '../../../editor/store/store-hook'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'

type UpdateInPlaceSetting = 'update-in-place' | 'update-in-cms' | 'not-applicable'

export const ShouldUpdateInPlaceAtom = atom<UpdateInPlaceSetting>('not-applicable')

// eslint-disable-next-line no-shadow-restricted-names
const Infinity = 100000 // the built-in Infinity is not a valid CSS positioning value

export const FloatingTextEditControls = React.memo(() => {
  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'PostActionMenu scale',
  )

  const editorMode = useEditorState(
    Substores.fullStore,
    (store) => store.editor.mode.type,
    'FloatingTextEditControls editorMode',
  )

  const [shouldUpdateInPlace, setShouldUpdateInPlace] = useAtom(ShouldUpdateInPlaceAtom)

  const colorTheme = useColorTheme()

  const setUpdateInPlace = React.useCallback(
    (e: React.MouseEvent) => {
      e.stopPropagation()
      e.preventDefault()
      setShouldUpdateInPlace('update-in-place')
    },
    [setShouldUpdateInPlace],
  )
  const setUpdateInCMS = React.useCallback(
    (e: React.MouseEvent) => {
      e.stopPropagation()
      e.preventDefault()
      setShouldUpdateInPlace('update-in-cms')
    },
    [setShouldUpdateInPlace],
  )

  const positioningProps: CanvasRectangle = useEditorState(
    Substores.fullStore,
    (store) => {
      if (store.editor.mode.type !== 'textEdit') {
        return canvasRectangle({ x: -Infinity, y: -Infinity, width: 0, height: 0 })
      }

      const selectedElementBounds = nullIfInfinity(
        MetadataUtils.getFrameInCanvasCoords(
          store.editor.mode.editedText,
          store.editor.jsxMetadata,
        ),
      )
      if (selectedElementBounds == null) {
        return canvasRectangle({ x: -Infinity, y: -Infinity, width: 0, height: 0 })
      }

      return canvasRectangle({
        x: selectedElementBounds.x,
        y: selectedElementBounds.y + selectedElementBounds.height + 12 / scale,
        width: selectedElementBounds.width,
        height: selectedElementBounds.height,
      })
    },
    'PostActionMenu positioningProps',
  )

  if (editorMode !== 'textEdit' || shouldUpdateInPlace === 'not-applicable') {
    return null
  }

  return (
    <CanvasOffsetWrapper>
      <div
        style={{
          justifyContent: 'center',
          pointerEvents: 'initial',
          position: 'absolute',
          top: positioningProps.y,
          left: positioningProps.x,
          width: positioningProps.width,
          fontSize: 12 / scale,
          fontWeight: 400,
        }}
        onMouseDown={(e) => {
          e.preventDefault()
          e.stopPropagation()
        }}
      >
        <FlexRow
          style={{
            backgroundColor: colorTheme.bg0.value,
            gap: 4,
          }}
        >
          <div
            onClick={setUpdateInPlace}
            style={{
              cursor: 'pointer',
              padding: 6 / scale,
              borderRadius: UtopiaTheme.panelStyles.panelBorderRadius,
              boxShadow: UtopiaStyles.shadowStyles.mid.boxShadow,
              backgroundColor:
                shouldUpdateInPlace === 'update-in-place'
                  ? colorTheme.denimBlue.value
                  : colorTheme.bg0.value,
              color: colorTheme.fg0.value,
            }}
          >
            Overwrite
          </div>
          <div
            onClick={setUpdateInCMS}
            style={{
              cursor: 'pointer',
              padding: 6 / scale,
              borderRadius: UtopiaTheme.panelStyles.panelBorderRadius,
              boxShadow: UtopiaStyles.shadowStyles.mid.boxShadow,
              backgroundColor:
                shouldUpdateInPlace === 'update-in-cms'
                  ? colorTheme.denimBlue.value
                  : colorTheme.bg0.value,
              color: colorTheme.fg0.value,
            }}
          >
            Update
          </div>
        </FlexRow>
      </div>
    </CanvasOffsetWrapper>
  )
})
FloatingTextEditControls.displayName = 'FloatingTextEditControls'
