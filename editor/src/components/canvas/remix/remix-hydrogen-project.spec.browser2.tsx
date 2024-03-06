import * as RemixOxygen from '@shopify/remix-oxygen'

import * as PackageJSON from '../../../../package.json'

import { HydrogenTestProject } from '../../../test-cases/hydrogen-november'
import {
  DefaultStartingFeatureSwitches,
  createBuiltinDependenciesWithTestWorkers,
  renderTestEditorWithModel,
} from '../ui-jsx.test-utils'
import { wait } from '../../../core/model/performance-scripts'
import { mouseClickAtPoint, mouseClickAtElementCenter } from '../event-helpers.test-utils'

describe('Hydrogen November Project', () => {
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

    await mouseClickAtElementCenter(
      await renderResult.renderedDOM.getByTestId('canvas-toolbar-play-mode'),
    )
  })
})
