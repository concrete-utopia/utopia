import { waitFor } from '@testing-library/react'
import { HydrogenTestProject } from '../../../test-cases/hydrogen-november.test-utils'
import { mouseClickAtElementCenter } from '../event-helpers.test-utils'
import {
  DefaultStartingFeatureSwitches,
  createBuiltinDependenciesWithTestWorkers,
  renderTestEditorWithModel,
} from '../ui-jsx.test-utils'

import * as RemixOxygen from '@shopify/remix-oxygen'
import * as PackageJSON from '../../../../package.json'

describe('Hydrogen Project Using Real Server Calls - disable me if there is no internet access', () => {
  it('can navigate in play mode to a detail page', async () => {
    console.log('logpoint 0')
    const renderResult = await renderTestEditorWithModel(
      HydrogenTestProject,
      'await-first-dom-report',
      DefaultStartingFeatureSwitches,
      createBuiltinDependenciesWithTestWorkers([
        {
          moduleName: '@shopify/remix-oxygen',
          nodeModule: RemixOxygen,
          version: PackageJSON.devDependencies['@shopify/remix-oxygen'],
        },
      ]),
    )

    console.log('logpoint 1')

    expect(await renderResult.renderedDOM.findByText('Mock.shop')).toBeDefined()

    console.log('logpoint 2')

    {
      /* Enter play mode, turning the play button white-on-blue */
      const playModeButton = await renderResult.renderedDOM.getByTestId('canvas-toolbar-play-mode')
      expect(playModeButton.innerHTML).not.toContain('white')
      await mouseClickAtElementCenter(playModeButton)
      expect(playModeButton.innerHTML).toContain('white')
    }
    console.log('logpoint 3')

    {
      /* Navigate to Hoodie's product detail page */
      await mouseClickAtElementCenter((await renderResult.renderedDOM.findAllByText('Unisex'))[1])
      console.log('logpoint 4')
      await mouseClickAtElementCenter(await renderResult.renderedDOM.findByAltText('Hoodie'))
    }

    console.log('logpoint 5')

    {
      // A size selector button with the text Ocean appears
      expect(await renderResult.renderedDOM.findByText('Ocean')).toBeDefined()
    }

    console.log('logpoint 6')
  }, 60000)
})
