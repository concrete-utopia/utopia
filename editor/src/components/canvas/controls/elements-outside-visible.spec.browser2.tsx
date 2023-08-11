import { act, screen } from '@testing-library/react'
import * as EP from '../../../core/shared/element-path'
import { selectComponents } from '../../editor/actions/meta-actions'
import { keyDown, mouseClickAtPoint, mouseDragFromPointToPoint } from '../event-helpers.test-utils'
import type { EditorRenderResult } from '../ui-jsx.test-utils'
import { makeTestProjectCodeWithSnippet, renderTestEditorWithCode } from '../ui-jsx.test-utils'
import { CanvasControlsContainerID } from './new-canvas-controls'
import type { ElementOutsideVisibleAreaDirection } from './elements-outside-visible-area-hooks'
import { getIndicatorId } from './elements-outside-visible-area-hooks'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { getRectCenter, isFiniteRectangle, windowRectangle } from '../../../core/shared/math-utils'
import { canvasPointToWindowPoint } from '../dom-lookup'
import { DefaultNavigatorWidth } from '../../editor/store/editor-state'
import { parseCSSNumber } from '../../inspector/common/css-utils'
import { isRight } from '../../../core/shared/either'

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
  function indicatorPoints(
    targetPath: ElementPath,
    sides: ElementOutsideVisibleAreaDirection[],
    suffix: string = '',
  ) {
    const indicator = screen.queryByTestId(getIndicatorId(targetPath, sides) + suffix)
    if (indicator == null) {
      return null
    }
    function cssValueOrNull(s: string) {
      const got = parseCSSNumber(s, 'Px')
      return isRight(got) ? got.value.value : null
    }
    return {
      top: cssValueOrNull(indicator.style.top),
      left: cssValueOrNull(indicator.style.left),
    }
  }
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

      const points = indicatorPoints(targetPath, ['left'])
      expect(points).not.toBeNull()
      expect(points?.top).toBeGreaterThan(180)
      expect(points?.left).toBeGreaterThan(300)
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

      const points = indicatorPoints(targetPath, ['right'])
      expect(points).not.toBeNull()
      expect(points?.top).toBeGreaterThan(180)
      expect(points?.left).toBeGreaterThan(1500)
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

      const points = indicatorPoints(targetPath, ['top'])
      expect(points).not.toBeNull()
      expect(points?.top).toEqual(0)
      expect(points?.left).toBeGreaterThan(400)
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

      const points = indicatorPoints(targetPath, ['bottom'])
      expect(points).not.toBeNull()
      expect(points?.top).toBeGreaterThan(900)
      expect(points?.left).toBeGreaterThan(450)
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

      const points = indicatorPoints(targetPath, ['top', 'left'])
      expect(points).not.toBeNull()
      expect(points?.top).toEqual(0)
      expect(points?.left).toBeGreaterThan(300)
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
      const points = indicatorPoints(targetPath, ['top', 'right'])
      expect(points).not.toBeNull()
      expect(points?.top).toEqual(0)
      expect(points?.left).toBeGreaterThan(1500)
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

      const points = indicatorPoints(targetPath, ['bottom', 'left'])
      expect(points).not.toBeNull()
      expect(points?.top).toBeGreaterThan(900)
      expect(points?.left).toBeGreaterThan(250)
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

      const points = indicatorPoints(targetPath, ['bottom', 'right'])
      expect(points).not.toBeNull()
      expect(points?.top).toBeGreaterThan(900)
      expect(points?.left).toBeGreaterThan(1500)
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

      expect(indicatorPoints(foo, ['top'])).toBeNull()
      expect(indicatorPoints(bar, ['left'])).toBeNull()

      await selectAndPan(renderResult, [foo, bar], -250, -300)

      const pointsFoo = indicatorPoints(foo, ['top'])
      expect(pointsFoo).not.toBeNull()
      expect(pointsFoo?.top).toEqual(0)
      expect(pointsFoo?.left).toBeGreaterThan(400)

      const pointsBar = indicatorPoints(bar, ['left'])
      expect(pointsBar).not.toBeNull()
      expect(pointsBar?.top).toBeGreaterThan(50)
      expect(pointsBar?.left).toBeGreaterThan(300)
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

      expect(indicatorPoints(foo, ['top'])).toBeNull()
      expect(indicatorPoints(bar, ['left'])).toBeNull()
      expect(screen.queryByTestId(getIndicatorId(bar, ['top', 'left']) + '-cluster-2')).toBeNull()

      await selectAndPan(renderResult, [foo, bar], -farAway, -farAway)

      expect(indicatorPoints(foo, ['top'])).toBeNull()
      expect(indicatorPoints(bar, ['left'])).toBeNull()

      const points = indicatorPoints(foo, ['top', 'left'], '-cluster-2')
      expect(points).not.toBeNull()
      expect(points?.top).toEqual(0)
      expect(points?.left).toBeGreaterThan(300)
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
      expect(indicatorPoints(targetPath, ['top'])).toBeNull()

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
