import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import { isFiniteRectangle, nullIfInfinity } from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { FlexRow, useColorTheme } from '../../../../uuiui'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import { CMSUpdateStateAtom, type CMSUpdateStatus } from '../../../editor/jurassic-cms'
import { useAtom } from 'jotai'

export const CMSUpdateLabelControl = React.memo(() => {
  const [cmsUpdateState] = useAtom(CMSUpdateStateAtom)

  return (
    <>
      {Object.entries(cmsUpdateState).map(([pathString, status]) => (
        <CMSUpdateLabel
          key={`${pathString}-${status.type}`}
          elementPath={EP.fromString(pathString)}
          status={status}
        />
      ))}
    </>
  )
})

interface CMSUpdateLabelProps {
  elementPath: ElementPath
  status: CMSUpdateStatus
}

const CMSUpdateLabel = React.memo<CMSUpdateLabelProps>((props) => {
  const colorTheme = useColorTheme()

  const frame = useEditorState(
    Substores.metadata,
    (store) =>
      nullIfInfinity(
        MetadataUtils.getFrameInCanvasCoords(props.elementPath, store.editor.jsxMetadata),
      ),
    'CMSUpdateLabel frame',
  )

  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'CMSUpdateLabel scale',
  )

  const baseFontSize = 10
  const scaledFontSize = baseFontSize / scale
  const scaledLineHeight = 17 / scale
  const paddingY = scaledFontSize / 4
  const paddingX = paddingY * 2
  const offsetY = scaledFontSize / 1.5
  const borderRadius = 3 / scale

  const backgroundColor = colorTheme.componentPurple05solid.value

  if (frame != null && isFiniteRectangle(frame)) {
    return (
      <CanvasOffsetWrapper>
        <FlexRow
          style={{
            pointerEvents: 'none',
            color: colorTheme.fg2.value,
            position: 'absolute',
            fontWeight: 600,
            left: frame.x,
            bottom: -frame.y + offsetY,
            width: frame.width,
            padding: `${paddingY}px ${paddingX}px`,
            fontFamily: 'Utopian-Inter',
            fontSize: scaledFontSize,
            lineHeight: `${scaledLineHeight}px`,
            whiteSpace: 'nowrap',
            overflow: 'hidden',
            textOverflow: 'ellipsis',
            borderRadius: borderRadius,
            backgroundColor: backgroundColor,
          }}
        >
          <div
            style={{
              fontWeight: 600,
            }}
          >
            {props.status.type}
          </div>
        </FlexRow>
      </CanvasOffsetWrapper>
    )
  } else {
    return null
  }
})
