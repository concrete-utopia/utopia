import { act, screen } from '@testing-library/react'
import * as EP from '../../../core/shared/element-path'
import { selectComponents } from '../../editor/actions/meta-actions'
import { keyDown, mouseClickAtPoint, mouseDragFromPointToPoint } from '../event-helpers.test-utils'
import {
  EditorRenderResult,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../ui-jsx.test-utils'
import { CanvasControlsContainerID } from './new-canvas-controls'
import { getIndicatorId } from './elements-outside-visible-area-hooks'
import { ElementPath } from '../../../core/shared/project-file-types'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import {
  getRectCenter,
  isFiniteRectangle,
  offsetPoint,
  windowPoint,
  windowRectangle,
} from '../../../core/shared/math-utils'
import { canvasPointToWindowPoint } from '../dom-lookup'
import { DefaultNavigatorWidth } from '../../editor/store/editor-state'

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
    it('shows the indicator on the left side', async () => {
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
      expect(screen.queryByTestId(getIndicatorId(targetPath, ['left']))).toBeNull()

      await selectAndPan(renderResult, [targetPath], -farAway, 0)

      const indicator = screen.queryByTestId(getIndicatorId(targetPath, ['left']))
      expect(indicator).not.toBeNull()
      expect(indicator?.style.top).toBe('182px')
      expect(indicator?.style.left).toBe('363px')
    })
    it('shows the indicator on the right side', async () => {
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
      expect(screen.queryByTestId(getIndicatorId(targetPath, ['right']))).toBeNull()

      await selectAndPan(renderResult, [targetPath], farAway, 0)

      const indicator = screen.queryByTestId(getIndicatorId(targetPath, ['right']))
      expect(indicator).not.toBeNull()
      expect(indicator?.style.top).toBe('182px')
      expect(indicator?.style.left).toBe('1920px')
    })
    it('shows the indicator on the top side', async () => {
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
      expect(screen.queryByTestId(getIndicatorId(targetPath, ['top']))).toBeNull()

      await selectAndPan(renderResult, [targetPath], 0, -farAway)

      const indicator = screen.queryByTestId(getIndicatorId(targetPath, ['top']))
      expect(indicator).not.toBeNull()
      expect(indicator?.style.top).toBe('0px')
      expect(indicator?.style.left).toMatch('493px')
    })
    it('shows the indicator on the bottom side', async () => {
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
      expect(screen.queryByTestId(getIndicatorId(targetPath, ['bottom']))).toBeNull()

      await selectAndPan(renderResult, [targetPath], 0, farAway)

      const indicator = screen.queryByTestId(getIndicatorId(targetPath, ['bottom']))
      expect(indicator).not.toBeNull()
      expect(indicator?.style.top).toBe('936px')
      expect(indicator?.style.left).toMatch('493px')
    })
    it('shows the indicator on the top-left side', async () => {
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
      expect(screen.queryByTestId(getIndicatorId(targetPath, ['top', 'left']))).toBeNull()

      await selectAndPan(renderResult, [targetPath], -farAway, -farAway)

      const indicator = screen.queryByTestId(getIndicatorId(targetPath, ['top', 'left']))
      expect(indicator).not.toBeNull()
      expect(indicator?.style.top).toBe('0px')
      expect(indicator?.style.left).toMatch('363px')
    })
    it('shows the indicator on the top-right side', async () => {
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
      expect(screen.queryByTestId(getIndicatorId(targetPath, ['top', 'right']))).toBeNull()

      await selectAndPan(renderResult, [targetPath], farAway, -farAway)

      const indicator = screen.queryByTestId(getIndicatorId(targetPath, ['top', 'right']))
      expect(indicator).not.toBeNull()
      expect(indicator?.style.top).toBe('0px')
      expect(indicator?.style.left).toMatch('1920px')
    })
    it('shows the indicator on the bottom-left side', async () => {
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
      expect(screen.queryByTestId(getIndicatorId(targetPath, ['bottom', 'left']))).toBeNull()

      await selectAndPan(renderResult, [targetPath], -farAway, farAway)

      const indicator = screen.queryByTestId(getIndicatorId(targetPath, ['bottom', 'left']))
      expect(indicator).not.toBeNull()
      expect(indicator?.style.top).toBe('936px')
      expect(indicator?.style.left).toMatch('282px')
    })
    it('shows the indicator on the bottom-right side', async () => {
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
      expect(screen.queryByTestId(getIndicatorId(targetPath, ['bottom', 'right']))).toBeNull()

      await selectAndPan(renderResult, [targetPath], farAway, farAway)

      const indicator = screen.queryByTestId(getIndicatorId(targetPath, ['bottom', 'right']))
      expect(indicator).not.toBeNull()
      expect(indicator?.style.top).toBe('936px')
      expect(indicator?.style.left).toMatch('1920px')
    })
  })
  describe('multiple elements', () => {
    it('shows indicators for two elements outside of the visible area', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='container'>
          <div data-uid='foo' style={{
            width: 50,
            height: 50,
            position: "absolute",
            left: 300,
            top: 100,
            background: "#f09"
          }} />
          <div data-uid='bar' style={{
            width: 50,
            height: 50,
            position: "absolute",
            left: 0,
            top: 300,
            background: "#09f"
          }} />
        </div>
      `),
        'await-first-dom-report',
      )

      const foo = EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:container/foo')
      const bar = EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:container/bar')
      expect(screen.queryByTestId(getIndicatorId(foo, ['top']))).toBeNull()
      expect(screen.queryByTestId(getIndicatorId(bar, ['left']))).toBeNull()

      await selectAndPan(renderResult, [foo, bar], -250, -300)

      const indicatorFoo = screen.queryByTestId(getIndicatorId(foo, ['top']))
      expect(indicatorFoo).not.toBeNull()
      expect(indicatorFoo?.style.top).toBe('0px')
      expect(indicatorFoo?.style.left).toMatch('443px')

      const indicatorBar = screen.queryByTestId(getIndicatorId(bar, ['left']))
      expect(indicatorBar).not.toBeNull()
      expect(indicatorBar?.style.top).toBe('82px')
      expect(indicatorBar?.style.left).toMatch('363px')
    })
    it('shows a single indicator for clustered elements', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='container'>
          <div data-uid='foo' style={{
            width: 50,
            height: 50,
            position: "absolute",
            left: 300,
            top: 100,
            background: "#f09"
          }} />
          <div data-uid='bar' style={{
            width: 50,
            height: 50,
            position: "absolute",
            left: 0,
            top: 300,
            background: "#09f"
          }} />
        </div>
      `),
        'await-first-dom-report',
      )

      const foo = EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:container/foo')
      const bar = EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:container/bar')
      expect(screen.queryByTestId(getIndicatorId(foo, ['top']))).toBeNull()
      expect(screen.queryByTestId(getIndicatorId(bar, ['left']))).toBeNull()
      expect(screen.queryByTestId(getIndicatorId(bar, ['top', 'left']) + '-cluster-2')).toBeNull()

      await selectAndPan(renderResult, [foo, bar], -farAway, -farAway)

      expect(screen.queryByTestId(getIndicatorId(foo, ['top']))).toBeNull()
      expect(screen.queryByTestId(getIndicatorId(bar, ['left']))).toBeNull()

      const indicator = screen.queryByTestId(getIndicatorId(foo, ['top', 'left']) + '-cluster-2')
      expect(indicator).not.toBeNull()
      expect(indicator?.style.top).toBe('0px')
      expect(indicator?.style.left).toMatch('363px')
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
      expect(screen.queryByTestId(getIndicatorId(targetPath, ['left']))).toBeNull()

      await selectAndPan(renderResult, [targetPath], -farAway, 0)

      const indicator = screen.queryByTestId(getIndicatorId(targetPath, ['left']))
      if (indicator == null) {
        throw new Error('indicator not found')
      }
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

      const bodyRect = document.body.getBoundingClientRect()
      const pointOnWindow = offsetPoint(
        canvasPointToWindowPoint(
          elementFrame,
          renderResult.getEditorState().editor.canvas.scale,
          renderResult.getEditorState().editor.canvas.roundedCanvasOffset,
        ),
        windowPoint({ x: 0, y: -(bodyRect.y + canvasRect.y + 20) }), // accommodate for Karma runner vertical skew
      )

      const canvasCenter = getRectCenter(
        windowRectangle({
          x: canvasRect.x + DefaultNavigatorWidth,
          y: canvasRect.y,
          width: canvasRect.width - DefaultNavigatorWidth,
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
