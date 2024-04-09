import { wait } from '../utils'
import type { BoundingBox, Page, Point } from 'puppeteer'
import {
  enterCommentMode,
  getElementWithSelector,
  initBrowserTest,
  initSignedInBrowserTest,
  placeCommentOnCanvas,
} from './comment-utils'
import { createUtopiaPuppeteerBrowser } from './test-utils'

const CommentIndicatorSelector = 'div[data-testid="comment-indicator-div"]'
const PlaygroundSceneSelector = '#playground-scene'
const SceneToolbarSelector = 'div[data-testid="scene-label"]'

async function getBoundingBox(page: Page, selector: string): Promise<BoundingBox> {
  const element = await getElementWithSelector(page, selector)
  const boundingBox = await element!.boundingBox()
  return boundingBox!
}

function roundTo(number: number, digits: number = 0): number {
  const multiplicator = Math.pow(10, digits)
  const n = parseFloat((number * multiplicator).toFixed(11))
  return Math.round(n) / multiplicator
}
const roundToNearestWhole = (x: number) => roundTo(x, 0)

const roundBoundingBox = (bb: BoundingBox): BoundingBox => ({
  x: roundToNearestWhole(bb.x),
  y: roundToNearestWhole(bb.y),
  width: roundToNearestWhole(bb.width),
  height: roundToNearestWhole(bb.height),
})

const roundPoint = (bb: Point): Point => ({
  x: roundToNearestWhole(bb.x),
  y: roundToNearestWhole(bb.y),
})

const center = (bb: BoundingBox): Point => ({ x: bb.x + bb.width / 2, y: bb.y + bb.height / 2 })

const offsetPoint = (point: Point, by: { offsetX: number; offsetY: number }) => ({
  x: point.x + by.offsetX,
  y: point.y + by.offsetY,
})

async function drag(page: Page, from: { x: number; y: number }, to: { x: number; y: number }) {
  await page.mouse.move(from.x, from.y)
  await page.mouse.down()

  await page.mouse.move(to.x, to.y, { steps: 10 })
  await page.mouse.up()
}

async function dismissHoverPreview(page: Page) {
  await page.mouse.move(500, 500)

  // TODO instead of this horrible timer, actually away the animation finishing on screen, probably using selectors!
  await wait(1000) // wait for the animation duration (100ms) + flaky test buffer (900ms)
}

describe('Comments test', () => {
  let utopiaBrowser = createUtopiaPuppeteerBrowser()

  xit('Basic comment workflow (place, reply)', async () => {
    const page = await initSignedInBrowserTest(utopiaBrowser)

    // Enter comment mode using the toolbar
    const commentModeButton = await getElementWithSelector(
      page,
      'div[data-testid="canvas-toolbar-comment-mode-connected"]',
    )
    await commentModeButton!.click()

    // Click on the canvas to open the comment popup
    const canvasControlsContainer = await getElementWithSelector(
      page,
      '#new-canvas-controls-container',
    )
    await canvasControlsContainer!.click({ offset: { x: 500, y: 500 } })

    // Write to comment text and submit it
    const commentBox = await getElementWithSelector(page, '[contenteditable="true"]')
    await commentBox!.focus()
    await commentBox!.type('hello comments')
    await page.keyboard.press('Enter')

    // Check if the comment text is on the screen
    console.info(
      'waiting for element with function',
      'document.querySelector("body").innerText.includes("hello comments")',
    )
    const thread = await page.waitForFunction(
      'document.querySelector("body").innerText.includes("hello comments")',
    )
    expect(thread).not.toBeNull()

    // Click away to close the comment popup
    await canvasControlsContainer!.click({ offset: { x: 700, y: 700 } })

    // Check if the comment indicator is still visible
    const commentIndicator = await getElementWithSelector(page, CommentIndicatorSelector)

    expect(commentIndicator).not.toBeNull()

    // Check that the comment popup is closed
    const popupAfterClose = await page.$$('div[data-testid="comment-popup"]')
    expect(popupAfterClose).toHaveLength(0)

    // Open the comment popup again
    const comment = await page.waitForSelector('div[data-testid="comment-indicator-wrapper"]')
    await comment!.click({ offset: { x: 10, y: 10 } })

    // Check that the comment popup is open
    const popupAfterReopen = await page.$$('div[data-testid="comment-popup"]')
    expect(popupAfterReopen).toHaveLength(1)

    // Submit a reply
    const commentBox2 = await getElementWithSelector(page, '[contenteditable="true"]')
    await commentBox2!.focus()
    await commentBox2!.type('this is a reply')
    await page.keyboard.press('Enter')

    // Check if the original comment and the reply comment are both on the screen
    console.info(
      'waiting for element with function',
      'document.querySelector("body").innerText.includes("hello comments")',
    )
    const originalComment = await page.waitForFunction(
      'document.querySelector("body").innerText.includes("hello comments")',
    )
    expect(originalComment).not.toBeNull()

    console.info(
      'waiting for element with function',
      'document.querySelector("body").innerText.includes("this is a reply")',
    )
    const replyComment = await page.waitForFunction(
      'document.querySelector("body").innerText.includes("this is a reply")',
    )
    expect(replyComment).not.toBeNull()

    // Leave comment mode by pressing ESC
    await page.keyboard.press('Escape')

    // Check that the comment popup is closed
    const popupAfterEsc = await page.$$('div[data-testid="comment-popup"]')
    expect(popupAfterEsc).toHaveLength(0)
  })

  it('Placing a comment without submitting does not create a comment', async () => {
    const page = await initSignedInBrowserTest(utopiaBrowser)

    await enterCommentMode(page)

    // Clicking to add a comment
    const canvasControlsContainer = await getElementWithSelector(
      page,
      '#new-canvas-controls-container',
    )
    await canvasControlsContainer!.click({ offset: { x: 500, y: 500 } })

    // Pressing escape without submitting
    await page.keyboard.press('Escape')

    // There are no comment indicators on the canvas
    const commentIndicators = await page.$$(CommentIndicatorSelector)
    expect(commentIndicators).toHaveLength(0)
  })
  it('Resolve comment', async () => {
    const page = await initSignedInBrowserTest(utopiaBrowser)

    await enterCommentMode(page)
    await placeCommentOnCanvas(page, 'hello comments', 500, 500)

    // Resolve the comment

    const resolveButton = await getElementWithSelector(
      page,
      'div[data-testid="resolve-thread-button"]',
    )
    await resolveButton!.click()

    // Check that the comment indicator is gone
    const commentIndicators = await page.$$(CommentIndicatorSelector)
    expect(commentIndicators).toHaveLength(0)
  })

  xit('Close comment popup with the mouse', async () => {
    const page = await initSignedInBrowserTest(utopiaBrowser)

    await enterCommentMode(page)
    await placeCommentOnCanvas(page, 'hello comments', 500, 500)

    const closeCommentButton = await getElementWithSelector(
      page,
      'div[data-testid="close-comment"]',
    )
    await closeCommentButton!.click()

    // Check that the comment popup is closed but the indicator is still there
    const commentPopups = await page.$$('div[data-testid="comment-popup"]')
    expect(commentPopups).toHaveLength(0)

    const commentIndicators = await page.$$(CommentIndicatorSelector)
    expect(commentIndicators).toHaveLength(1)
  })

  it('There is a comment tab when logged in', async () => {
    const page = await initSignedInBrowserTest(utopiaBrowser)

    const commentTabs = await getElementWithSelector(page, 'div[data-testid="comments-tab"]')
    expect(commentTabs).not.toBeNull()
  })

  xit('can reparent comment', async () => {
    const page = await initSignedInBrowserTest(utopiaBrowser)
    await enterCommentMode(page)

    const playgroundSceneBoundingBox = roundBoundingBox(
      await getBoundingBox(page, PlaygroundSceneSelector),
    )

    await placeCommentOnCanvas(
      page,
      'hello comments',
      playgroundSceneBoundingBox.x - 100,
      playgroundSceneBoundingBox.y - 100,
    )

    const originalCommentIndicatorBoundingBox = roundBoundingBox(
      await getBoundingBox(page, CommentIndicatorSelector),
    )

    expect(originalCommentIndicatorBoundingBox).toEqual({
      height: 26,
      width: 26,
      x: 486,
      y: 63,
    })

    // drag the comment on the scene
    await drag(
      page,
      center(originalCommentIndicatorBoundingBox),
      center(playgroundSceneBoundingBox),
    )

    await dismissHoverPreview(page)
    const commentBoundingBoxInScene = roundBoundingBox(
      await getBoundingBox(page, CommentIndicatorSelector),
    )
    expect(commentBoundingBoxInScene).toEqual({
      height: 26,
      width: 26,
      x: 918,
      y: 555,
    })
    expect(commentBoundingBoxInScene).not.toEqual(originalCommentIndicatorBoundingBox)

    const sceneToolBarBoundingBox = roundBoundingBox(
      await getBoundingBox(page, SceneToolbarSelector),
    )
    const sceneToolBarCenter = roundPoint(center(sceneToolBarBoundingBox))

    // move the scene, the comment indicator shoud move with the scene
    await drag(
      page,
      sceneToolBarCenter,
      offsetPoint(sceneToolBarCenter, { offsetX: 50, offsetY: 50 }),
    )

    const commentBoundingBoxInMovedScene = roundBoundingBox(
      await getBoundingBox(page, CommentIndicatorSelector),
    )
    expect(commentBoundingBoxInMovedScene).toEqual({
      height: 26,
      width: 26,
      x: 968,
      y: 605,
    })

    const movedSceneBoundingBox = roundBoundingBox(
      await getBoundingBox(page, PlaygroundSceneSelector),
    )
    const commentBoundingBoxInMovedSceneCenter = center(commentBoundingBoxInMovedScene)

    // drag the comment indicator out of the scene, back to the canvas
    await drag(page, commentBoundingBoxInMovedSceneCenter, {
      x: movedSceneBoundingBox.x - 50,
      y: movedSceneBoundingBox.y - 50,
    })

    await dismissHoverPreview(page)
    const commentBoundingBoxBackOnCanvasBeforeMove = roundBoundingBox(
      await getBoundingBox(page, CommentIndicatorSelector),
    )

    expect(commentBoundingBoxBackOnCanvasBeforeMove).toEqual({
      height: 26,
      width: 26,
      x: 568,
      y: 176,
    })

    const movedSceneLabelCenter = center(
      roundBoundingBox(await getBoundingBox(page, SceneToolbarSelector)),
    )

    // drag the scene, the comment indicator should not move with it
    await drag(
      page,
      movedSceneLabelCenter,
      offsetPoint(movedSceneLabelCenter, { offsetX: 150, offsetY: 150 }),
    )
    const commentBoundingBoxBackOnCanvasAfterMove = roundBoundingBox(
      await getBoundingBox(page, CommentIndicatorSelector),
    )

    expect(commentBoundingBoxBackOnCanvasAfterMove).toEqual({
      height: 26,
      width: 26,
      x: 568,
      y: 176,
    })
    expect(commentBoundingBoxBackOnCanvasBeforeMove).toEqual(
      commentBoundingBoxBackOnCanvasAfterMove,
    )
  })

  xit('scene comment canvas coordinates are maintained when scene is moved', async () => {
    const page = await initSignedInBrowserTest(utopiaBrowser)
    await enterCommentMode(page)

    const playgroundSceneBoundingBox = roundBoundingBox(
      await getBoundingBox(page, PlaygroundSceneSelector),
    )

    // Add a scene comment
    await placeCommentOnCanvas(
      page,
      'hello comments',
      playgroundSceneBoundingBox.x + 100,
      playgroundSceneBoundingBox.y + 100,
    )

    // Leave comment mode by pressing ESC twice
    await page.keyboard.press('Escape')
    await page.keyboard.press('Escape')

    const sceneToolBarBoundingBox = roundBoundingBox(
      await getBoundingBox(page, SceneToolbarSelector),
    )
    const sceneToolBarCenter = roundPoint(center(sceneToolBarBoundingBox))

    // move the scene, the comment indicator should move with the scene
    await drag(
      page,
      sceneToolBarCenter,
      offsetPoint(sceneToolBarCenter, { offsetX: 50, offsetY: 50 }),
    )

    const movedSceneBoundingBox = roundBoundingBox(await getBoundingBox(page, SceneToolbarSelector))
    const movedSceneBoundingBoxCenter = roundPoint(center(movedSceneBoundingBox))

    await page.mouse.click(movedSceneBoundingBoxCenter.x, movedSceneBoundingBoxCenter.y)

    const commentBoundingBoxBackOnCanvasBeforeDelete = roundBoundingBox(
      await getBoundingBox(page, CommentIndicatorSelector),
    )
    // delete the scene
    await page.keyboard.press('Backspace')

    const commentBoundingBoxBackOnCanvasAfterDelete = roundBoundingBox(
      await getBoundingBox(page, CommentIndicatorSelector),
    )
    // the comment indicator should not move on the canvas
    expect(commentBoundingBoxBackOnCanvasAfterDelete).toEqual(
      commentBoundingBoxBackOnCanvasBeforeDelete,
    )
  })
})
