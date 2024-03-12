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

    expect(await renderResult.renderedDOM.findByText('Mock.shop')).toBeDefined()

    {
      /* Enter play mode, turning the play button white-on-blue */
      const playModeButton = await renderResult.renderedDOM.getByTestId('canvas-toolbar-play-mode')
      expect(playModeButton.innerHTML).not.toContain('white')
      await mouseClickAtElementCenter(playModeButton)
      expect(playModeButton.innerHTML).toContain('white')
    }

    {
      /* Navigate to Hoodie's product detail page */
      await mouseClickAtElementCenter((await renderResult.renderedDOM.findAllByText('Unisex'))[1])
      await mouseClickAtElementCenter(await renderResult.renderedDOM.findByAltText('Hoodie'))
    }

    {
      // A size selector button with the text Ocean appears
      expect(await renderResult.renderedDOM.findByText('Ocean')).toBeDefined()
    }
  })
})
