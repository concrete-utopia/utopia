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
})
