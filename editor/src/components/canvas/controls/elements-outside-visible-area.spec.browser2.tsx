import { act, screen } from '@testing-library/react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { getRectCenter, isFiniteRectangle, windowRectangle } from '../../../core/shared/math-utils'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { selectComponents } from '../../editor/actions/meta-actions'
import { ToolbarIndicatorElementsOutsideVisibleAreaId } from '../../editor/elements-outside-visible-area-indicator'
import { DefaultNavigatorWidth } from '../../editor/store/editor-state'
import { canvasPointToWindowPoint } from '../dom-lookup'
import { keyDown, mouseClickAtPoint, mouseDragFromPointToPoint } from '../event-helpers.test-utils'
import type { EditorRenderResult } from '../ui-jsx.test-utils'
import { makeTestProjectCodeWithSnippet, renderTestEditorWithCode } from '../ui-jsx.test-utils'
import { CanvasControlsContainerID } from './new-canvas-controls'

async function panCanvas(
  x: number,
  y: number,
  canvasControlsLayer: HTMLElement,
  controlsBounds: DOMRect,
) {
  return act(async () => {
    keyDown('Space')
    await mouseDragFromPointToPoint(
      canvasControlsLayer,
      {
        x: controlsBounds.x + controlsBounds.width / 2,
        y: controlsBounds.y + controlsBounds.height / 2,
      },
      {
        x: controlsBounds.x + controlsBounds.width / 2 + x,
        y: controlsBounds.y + controlsBounds.height / 2 + y,
      },
    )
  })
}

async function selectAndPan(
  renderResult: EditorRenderResult,
  targetPaths: ElementPath[],
  panX: number,
  panY: number,
) {
  await renderResult.dispatch(selectComponents(targetPaths, true), true)

  const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
  const controlsBounds = canvasControlsLayer.getBoundingClientRect()

  await panCanvas(panX, panY, canvasControlsLayer, controlsBounds)
}

const farAway = 99999999 // px

describe('elements outside visible area', () => {
  describe('single element', () => {
    it('shows the indicator on the toolbar', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='foo' style={{
          width: 50,
          height: 50,
          position: "absolute",
          left: 100,
          top: 100,
          background: "#f09"
        }} />
      `),
        'await-first-dom-report',
      )

      const targetPath = EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:foo')

      expect(screen.queryByTestId(ToolbarIndicatorElementsOutsideVisibleAreaId)).toBeNull()

      await selectAndPan(renderResult, [targetPath], -farAway, 0)

      expect(screen.queryByTestId(ToolbarIndicatorElementsOutsideVisibleAreaId)).not.toBe(null)
    })
  })
  describe('scroll', () => {
    it('brings the element to the center of the canvas when clicking its indicator', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='foo' style={{
          width: 50,
          height: 50,
          position: "absolute",
          left: 100,
          top: 100,
          background: "#f09"
        }} />
      `),
        'await-first-dom-report',
      )

      const targetPath = EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:foo')

      await selectAndPan(renderResult, [targetPath], -farAway, 0)

      const indicator = await screen.findByTestId(ToolbarIndicatorElementsOutsideVisibleAreaId)
      const indicatorRect = indicator.getBoundingClientRect()

      await mouseClickAtPoint(indicator, { x: indicatorRect.x + 2, y: indicatorRect.y + 2 })

      const canvasRect = renderResult.renderedDOM.getByTestId('canvas-root').getBoundingClientRect()
      const elementFrame = MetadataUtils.getFrameInCanvasCoords(
        targetPath,
        renderResult.getEditorState().editor.jsxMetadata,
      )
      if (elementFrame == null || !isFiniteRectangle(elementFrame)) {
        throw new Error('element frame not found')
      }

      const pointOnWindow = canvasPointToWindowPoint(
        elementFrame,
        renderResult.getEditorState().editor.canvas.scale,
        renderResult.getEditorState().editor.canvas.roundedCanvasOffset,
      )

      const canvasCenter = getRectCenter(
        windowRectangle({
          x: canvasRect.x + DefaultNavigatorWidth,
          y: canvasRect.y,
          width: canvasRect.width - DefaultNavigatorWidth - 255, // 255 being the default inspector width (inspectorSmallWidth)
          height: canvasRect.height,
        }),
      )
      expect(pointOnWindow.x).toBeLessThan(canvasCenter.x)
      expect(pointOnWindow.x + elementFrame.width).toBeGreaterThan(canvasCenter.x)
      expect(pointOnWindow.y).toBeLessThan(canvasCenter.y)
      expect(pointOnWindow.y + elementFrame.height).toBeGreaterThan(canvasCenter.y)
    })
  })
})
