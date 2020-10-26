import { PropertyControlsInfo } from '../../components/custom-code/code-file'
import { npmVersion } from '../../components/editor/npm-dependency/npm-dependency'
import {
  extractNodeModulesFromPackageResponse,
  resetDepPackagerCache,
} from '../es-modules/package-manager/fetch-packages'
import { mangleNodeModulePaths } from '../es-modules/package-manager/merge-modules'
import { getJsDelivrFileUrl } from '../es-modules/package-manager/packager-url'
import * as antdPackagerResponse from '../es-modules/test-cases/antd-packager-response.json'
import { PackagerServerResponse, requestedNpmDependency } from '../shared/npm-dependency-types'
import { initPropertyControlsProcessor } from './property-controls-processor'
import { fullNodeModulesUpdate, getThirdPartyPropertyControls } from './property-controls-utils'

require('jest-fetch-mock').enableMocks()

const simpleCssContent = '.utopiaClass { background-color: red; }'

beforeEach(() => {
  resetDepPackagerCache()
})

describe('Property Controls Processor', () => {
  it('Returns the expected controls for antd', async (done) => {
    ;(fetch as any).mockResponse(
      (request: Request): Promise<{ body?: string; status?: number }> => {
        switch (request.url) {
          case getJsDelivrFileUrl('antd@4.2.5', '/dist/antd.css'):
            return Promise.resolve({ body: simpleCssContent })
          default:
            throw new Error(`unexpected fetch called: ${request.url}`)
        }
      },
    )

    const packageName = 'antd'
    const packageVersion = '4.2.5'

    const fakedPackagerResponse = extractNodeModulesFromPackageResponse(
      packageName,
      npmVersion(packageVersion),
      antdPackagerResponse as PackagerServerResponse,
    )
    const nodeModules = mangleNodeModulePaths('antd', fakedPackagerResponse)

    const onControlsProcess = (propertyControlsInfo: PropertyControlsInfo) => {
      expect(propertyControlsInfo).toEqual(
        getThirdPartyPropertyControls(packageName, packageVersion),
      )
      done()
    }

    const processControls = initPropertyControlsProcessor(onControlsProcess)
    processControls(
      [requestedNpmDependency(packageName, packageVersion)],
      fullNodeModulesUpdate(nodeModules),
      {},
      {},
      [],
    )
  })

  it('Calls the callback twice if a placeholder dependency is imported', async (done) => {
    ;(fetch as any).mockResponse(
      (request: Request): Promise<{ body?: string; status?: number }> => {
        switch (request.url) {
          case getJsDelivrFileUrl('antd@4.2.5', '/dist/antd.css'):
            return Promise.resolve({ body: simpleCssContent })
          default:
            throw new Error(`unexpected fetch called: ${request.url}`)
        }
      },
    )

    const packageName = 'antd'
    const packageVersion = '4.2.5'

    const fakedPackagerResponse = extractNodeModulesFromPackageResponse(
      packageName,
      npmVersion(packageVersion),
      antdPackagerResponse as PackagerServerResponse,
    )
    const nodeModules = mangleNodeModulePaths('antd', fakedPackagerResponse)

    let callbackCount = 0
    const onControlsProcess = (propertyControlsInfo: PropertyControlsInfo) => {
      callbackCount++

      expect(propertyControlsInfo).toEqual(
        getThirdPartyPropertyControls(packageName, packageVersion),
      )

      if (callbackCount === 2) {
        done()
      }
    }

    const processControls = initPropertyControlsProcessor(onControlsProcess)
    processControls(
      [requestedNpmDependency(packageName, packageVersion)],
      fullNodeModulesUpdate(nodeModules),
      {},
      {
        '/src/index.js': {
          transpiledCode: `require("antd/dist/antd.css");`,
          sourceMap: null,
          errors: [],
        },
      },
      [],
    )
  })
})
