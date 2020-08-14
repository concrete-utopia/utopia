import * as React from 'react'
import { ControlFontSize } from '../canvas-controls-frame'
import { colorTheme } from 'uuiui'
import { Size } from '../../../core/shared/math-utils'
import { isFeatureEnabled } from '../../../utils/feature-switches'
import { ResizeDragState } from '../canvas-types'
import * as TP from '../../../core/shared/template-path'
import * as PP from '../../../core/shared/property-path'
import { useEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { getJSXElementNameAsString, isJSXElement } from '../../../core/shared/element-template'
import Utils from '../../../utils/utils'
import { eitherToMaybe } from '../../../core/shared/either'
import { determineElementsToOperateOnForDragging } from './select-mode/move-utils'
import { LayoutHelpers } from '../../../core/layout/layout-helpers'
import { PropertyPath } from '../../../core/shared/project-file-types'
import { createLayoutPropertyPath } from '../../../core/layout/layout-helpers-new'

interface SizeBoxLabelProps {
  visible: boolean
  left: number
  top: number
  scale: number
  size: Size
  imageMultiplier: number | null
  dragState: ResizeDragState | null
}

export const SizeBoxLabel = React.memo((props: SizeBoxLabelProps) => {
  const showAdvancedResizeLabel = isFeatureEnabled('Advanced Resize Box')
  const fontSize = ControlFontSize / props.scale
  const borderRadius = 2 / props.scale
  const padding = 2 / props.scale

  return (
    <div
      style={{
        position: 'fixed',
        left: props.left,
        top: props.top,
        display: props.visible ? 'block' : 'none',
        fontSize: fontSize,
        borderRadius: borderRadius,
        whiteSpace: 'nowrap',
        overflow: 'visible',
        padding: padding,
        backgroundColor: colorTheme.resizingDisplayBackground.value,
        color: colorTheme.resizingDisplayForeground.value,
      }}
    >
      {showAdvancedResizeLabel ? <ResizeLabel {...props} /> : <SimpleResizeLabel {...props} />}
    </div>
  )
})

const ResizeLabel = (props: SizeBoxLabelProps) => {
  const metadata = useEditorState((state) => state.editor.jsxMetadataKILLME)
  const targets = determineElementsToOperateOnForDragging(
    props.dragState?.draggedElements ?? [],
    metadata,
    false,
    true,
  )
  let elementNames: string[] = []
  let targetProperties: {
    horizontal: PropertyPath
    vertical: PropertyPath
  } = {
    horizontal: createLayoutPropertyPath('Width'),
    vertical: createLayoutPropertyPath('Height'),
  }
  Utils.fastForEach(targets, (target) => {
    if (TP.isScenePath(target)) {
      const element = MetadataUtils.findSceneByTemplatePath(metadata, target)
      if (element != null) {
        elementNames.push(element.label ?? 'Scene')
      }
    } else {
      const element = MetadataUtils.getElementByInstancePathMaybe(metadata, target)
      if (element != null) {
        const jsxElement = eitherToMaybe(element.element)
        if (jsxElement != null && isJSXElement(jsxElement)) {
          elementNames.push(getJSXElementNameAsString(jsxElement.name))
          targetProperties = LayoutHelpers.getElementSizePropertyPaths(element)
        }
      }
    }
  })
  const isWidthResize = props.dragState?.edgePosition.x !== 0.5
  const isHeightResize = props.dragState?.edgePosition.y !== 0.5
  const padding = 2 / props.scale
  return (
    <div style={{ display: 'flex', flexDirection: 'column' }}>
      <div style={{ padding: padding }}>
        <div>{elementNames.join(', ')}</div>
      </div>
      {isWidthResize && (
        <div style={{ display: 'flex' }}>
          <div style={{ padding: padding }}>{PP.toString(targetProperties?.horizontal)}</div>
          <div style={{ padding: padding }}>{props.size.width}</div>
        </div>
      )}
      {isHeightResize && (
        <div style={{ display: 'flex' }}>
          <div style={{ padding: padding }}>{PP.toString(targetProperties?.vertical)}</div>
          <div style={{ padding: padding }}>{props.size.height}</div>
        </div>
      )}
    </div>
  )
}

const SimpleResizeLabel = (props: SizeBoxLabelProps) => {
  const padding = 2 / props.scale
  return (
    <table
      style={{
        padding: padding,
        borderCollapse: 'collapse',
      }}
    >
      <tbody>
        <tr>
          <td style={{ padding: padding }}>W:</td>
          <td style={{ padding: padding }}>{props.size.width}</td>
        </tr>
        <tr>
          <td style={{ padding: padding }}>H:</td>
          <td style={{ padding: padding }}>{props.size.height}</td>
        </tr>
        {props.imageMultiplier == null ? null : (
          <tr>
            <td style={{ padding: padding }}>@{props.imageMultiplier}x</td>
          </tr>
        )}
      </tbody>
    </table>
  )
}
