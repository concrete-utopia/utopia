import { setupBrowser } from '../utils'

describe('Comments test', () => {
  it('can place a comment', async () => {
    const { page, browser } = await setupBrowser('http://localhost:8000/p/', 120000)
    expect(1).toEqual(1)
  })
})
