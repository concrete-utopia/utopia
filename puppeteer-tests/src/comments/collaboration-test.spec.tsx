import type { Page } from 'puppeteer'
import { setupBrowser, wait } from '../utils'

const TIMEOUT = 120000

const BRANCH_NAME = process.env.BRANCH_NAME ? `&branch_name=${process.env.BRANCH_NAME}` : ''

async function signIn(page: Page) {
  const signInButton = await page.waitForSelector('div[data-testid="sign-in-button"]')
  await signInButton!.click()
  await page.waitForSelector('#playground-scene') // wait for the scene to render
}

async function clickCanvasContainer(page: Page, { x, y }: { x: number; y: number }) {
  const canvasControlsContainer = await page.waitForSelector('#new-canvas-controls-container')
  await canvasControlsContainer!.click({ offset: { x, y } })
}

async function expectNSelectors(page: Page, selector: string, n: number) {
  const elementsMatchingSelector = await page.$$(selector)
  expect(elementsMatchingSelector).toHaveLength(n)
}

async function waitForCoeditPropagation() {
  await wait(2000)
}

describe('Comments test', () => {
  it(
    'can place a comment',
    async () => {
      const [{ page: page1, browser: browser1 }, { page: page2, browser: browser2 }] =
        await Promise.all([
          setupBrowser(
            `http://localhost:8000/p/56a2ac40-caramel-yew?fakeUser=alice&Multiplayer=true${BRANCH_NAME}`,
            TIMEOUT,
          ),
          setupBrowser(
            `http://localhost:8000/p/56a2ac40-caramel-yew?fakeUser=bob&Multiplayer=true${BRANCH_NAME}`,
            TIMEOUT,
          ),
        ])

      await Promise.all([signIn(page1), signIn(page2)])

      await expectNSelectors(page2, 'div[data-testid="scene-label"]', 2)

      await clickCanvasContainer(page1, { x: 500, y: 500 })
      await clickCanvasContainer(page2, { x: 500, y: 500 })

      const sceneLabel = await page1.waitForSelector('div[data-testid="scene-label"]')
      sceneLabel!.click({ offset: { x: 5, y: 5 } })

      await wait(5000)

      await page1.keyboard.press('Backspace')
      await clickCanvasContainer(page2, { x: 500, y: 500 })

      await waitForCoeditPropagation()

      await expectNSelectors(page2, 'div[data-testid="scene-label"]', 1)

      await page1.keyboard.down('MetaLeft')
      await page1.keyboard.press('z', {})
      await page1.keyboard.up('MetaLeft')

      await page1.close()
      await browser1.close()
      await page2.close()
      await browser2.close()
    },
    TIMEOUT,
  )
})
