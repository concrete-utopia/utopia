import * as fileNoImports from '../test-cases/file-no-imports.json'
import * as fileWithImports from '../test-cases/file-with-imports.json'
import * as fileWithLocalImport from '../test-cases/file-with-local-import.json'
import * as reactSpringServerResponse from '../test-cases/react-spring-server-response.json'
import * as jsdelivrApiResponse from '../test-cases/jsdelivr-api-response-example.json'
import * as antdPackagerResponse from '../test-cases/antd-packager-response.json'
import { getRequireFn, getDependencyTypeDefinitions } from './package-manager'
import { createNodeModules } from './test-utils'
import { evaluator } from '../evaluator/evaluator'
import { fetchNodeModules, resetDepPackagerCache } from './fetch-packages'
import { ESCodeFile } from '../../shared/project-file-types'
import { NO_OP } from '../../shared/utils'
import { NodeModules } from '../../shared/project-file-types'
import { getPackagerUrl, getJsDelivrListUrl, getJsDelivrFileUrl } from './packager-url'
import { InjectedCSSFilePrefix } from '../../shared/css-style-loader'
import { VersionLookupResult } from '../../../components/editor/npm-dependency/npm-dependency'
import { requestedNpmDependency, resolvedNpmDependency } from '../../shared/npm-dependency-types'

require('jest-fetch-mock').enableMocks()

const simpleCssContent = '.utopiaClass { background-color: red; }'

beforeEach(() => {
  resetDepPackagerCache()
})

jest.mock('../../../components/editor/npm-dependency/npm-dependency', () => ({
  ...(jest.requireActual('../../../components/editor/npm-dependency/npm-dependency') as any),
  findMatchingVersion: async (
    packageName: string,
    versionRange: string,
  ): Promise<VersionLookupResult> => {
    return Promise.resolve({
      type: 'VERSION_LOOKUP_SUCCESS',
      version: versionRange,
    })
  },
  checkPackageVersionExists: async (
    packageName: string,
    version: string,
  ): Promise<VersionLookupResult> => {
    return Promise.resolve({
      type: 'VERSION_LOOKUP_SUCCESS',
      version: version,
    })
  },
}))

describe('ES Dependency Package Manager', () => {
  it('resolves a file with no imports', () => {
    const reqFn = getRequireFn(NO_OP, createNodeModules(fileNoImports.contents))
    const requireResult = reqFn('/src/index.js', 'mypackage')
    expect(requireResult).toHaveProperty('hello')
    expect((requireResult as any).hello).toEqual('hello!')
  })

  it('resolves a file with one simple import', () => {
    const reqFn = getRequireFn(NO_OP, createNodeModules(fileWithImports.contents))
    const requireResult = reqFn('/src/index.js', 'mypackage')
    expect(requireResult).toHaveProperty('hello')
    expect((requireResult as any).hello).toEqual('hello!')
  })

  it('resolves a file with one local import', () => {
    const reqFn = getRequireFn(NO_OP, createNodeModules(fileWithLocalImport.contents))
    const requireResult = reqFn('/src/index.js', 'mypackage')
    expect(requireResult).toHaveProperty('hello')
    expect((requireResult as any).hello).toEqual('hello!')
  })

  it('throws exception on not found dependency', () => {
    const reqFn = getRequireFn(NO_OP, createNodeModules(fileWithImports.contents))
    const test = () => reqFn('/src/index.js', 'mypackage2')
    expect(test).toThrowError(`Could not find dependency: 'mypackage2' relative to '/src/index.js`)
  })
})

describe('ES Dependency Manager — Cycles', () => {
  it('handles circular dependencies properly without running into an infinite loop', () => {
    const spyEvaluator = jest.fn(evaluator)
    const reqFn = getRequireFn(NO_OP, createNodeModules(fileWithImports.contents), spyEvaluator)
    const requireResult = reqFn('/src/index.js', 'mypackage/moduleA')
    expect(requireResult).toHaveProperty('hello')
    expect((requireResult as any).hello).toEqual('hello world!')
    expect(spyEvaluator).toHaveBeenCalledTimes(2)
  })
})

describe('ES Dependency Manager — Real-life packages', () => {
  it('react-spring@8.0.27', async () => {
    ;(fetch as any).mockResponse(
      (request: Request): Promise<{ body?: string; status?: number }> => {
        switch (request.url) {
          case getPackagerUrl(resolvedNpmDependency('react-spring', '8.0.27')):
            return Promise.resolve({ status: 200, body: JSON.stringify(reactSpringServerResponse) })
          case getJsDelivrListUrl(resolvedNpmDependency('react-spring', '8.0.27')):
            return Promise.resolve({ status: 404 }) // we don't care about the jsdelivr response now
          default:
            throw new Error(`unexpected fetch called: ${request.url}`)
        }
      },
    )
    const fetchNodeModulesResult = await fetchNodeModules([
      requestedNpmDependency('react-spring', '8.0.27'),
    ])
    if (fetchNodeModulesResult.dependenciesWithError.length > 0) {
      fail(`Expected successful nodeModules fetch`)
    }
    const nodeModules = fetchNodeModulesResult.nodeModules
    const req = getRequireFn(NO_OP, nodeModules)
    const reactSpring = req('/src/index.js', 'react-spring')
    expect(Object.keys(reactSpring)).not.toHaveLength(0)
  })

  it('antd@4.2.5', async (done) => {
    const spyEvaluator = jest.fn(evaluator)
    ;(fetch as any).mockResponse(
      (request: Request): Promise<{ body?: string; status?: number }> => {
        switch (request.url) {
          case getPackagerUrl(resolvedNpmDependency('antd', '4.2.5')):
            return Promise.resolve({ status: 200, body: JSON.stringify(antdPackagerResponse) })
          case getJsDelivrListUrl(resolvedNpmDependency('antd', '4.2.5')):
            return Promise.resolve({ status: 200, body: JSON.stringify(jsdelivrApiResponse) })
          case getJsDelivrFileUrl(resolvedNpmDependency('antd', '4.2.5'), '/dist/antd.css'):
            return Promise.resolve({ body: simpleCssContent })
          default:
            throw new Error(`unexpected fetch called: ${request.url}`)
        }
      },
    )
    const fetchNodeModulesResult = await fetchNodeModules([requestedNpmDependency('antd', '4.2.5')])
    if (fetchNodeModulesResult.dependenciesWithError.length > 0) {
      fail(`Expected successful nodeModules fetch`)
    }
    const nodeModules = fetchNodeModulesResult.nodeModules
    function addNewModules(newModules: NodeModules) {
      const updatedNodeModules = { ...nodeModules, ...newModules }
      const updatedReq = getRequireFn(NO_OP, updatedNodeModules, spyEvaluator)

      // this is like calling `import 'antd/dist/antd.css';`, we only care about the side effect
      updatedReq('/src/index.js', 'antd/dist/antd.css')

      // our CSS side effect code ran by now, so we should be able to find the relevant style tag on the JSDOM
      const styleTag = document.getElementById('/node_modules/antd/dist/antd.css')
      expect(styleTag).toBeDefined()
      expect(spyEvaluator).toHaveBeenCalledTimes(940)
      done()
    }
    const req = getRequireFn(addNewModules, nodeModules, spyEvaluator)
    const antd = req('/src/index.js', 'antd')
    expect(Object.keys(antd)).not.toHaveLength(0)
    expect(antd).toHaveProperty('Button')
    req('/src/index.js', 'antd/dist/antd.css')
  })
})

describe('ES Dependency Manager — d.ts', () => {
  it('get typings from node modules', async () => {
    ;(fetch as any).mockResponse(
      (request: Request): Promise<{ body?: string; status?: number }> => {
        switch (request.url) {
          case getPackagerUrl(resolvedNpmDependency('react-spring', '8.0.27')):
            return Promise.resolve({ status: 200, body: JSON.stringify(reactSpringServerResponse) })
          case getJsDelivrListUrl(resolvedNpmDependency('react-spring', '8.0.27')):
            return Promise.resolve({ status: 404 }) // we don't care about the jsdelivr response now
          default:
            throw new Error(`unexpected fetch called: ${request.url}`)
        }
      },
    )

    const fetchNodeModulesResult = await fetchNodeModules([
      requestedNpmDependency('react-spring', '8.0.27'),
    ])
    if (fetchNodeModulesResult.dependenciesWithError.length > 0) {
      fail(`Expected successful nodeModules fetch`)
    }
    const nodeModules = fetchNodeModulesResult.nodeModules
    const typings = getDependencyTypeDefinitions(nodeModules)
    expect(typings['/node_modules/react-spring/index.d.ts']).toBeDefined()
    expect(typings['/node_modules/react-spring/native.d.ts']).toBeDefined()
    expect(typings['/node_modules/react-spring/web.cjs.js']).toBeUndefined()
    expect(typings['/node_modules/react-spring/index.d.ts']).toEqual(
      (nodeModules['/node_modules/react-spring/index.d.ts'] as ESCodeFile).fileContents,
    )
    expect(typings['/node_modules/react-spring/native.d.ts']).toEqual(
      (nodeModules['/node_modules/react-spring/native.d.ts'] as ESCodeFile).fileContents,
    )
  })
})

describe('ES Dependency Manager — Downloads extra files as-needed', () => {
  it('downloads a css file from jsdelivr, if needed', async (done) => {
    ;(fetch as any).mockResponse(
      (request: Request): Promise<{ body?: string; status?: number }> => {
        switch (request.url) {
          case getPackagerUrl(resolvedNpmDependency('mypackage', '0.0.1')):
            return Promise.resolve({ status: 200, body: JSON.stringify(fileNoImports) })
          case getJsDelivrListUrl(resolvedNpmDependency('mypackage', '0.0.1')):
            return Promise.resolve({ status: 200, body: JSON.stringify(jsdelivrApiResponse) })
          case getJsDelivrFileUrl(resolvedNpmDependency('mypackage', '0.0.1'), '/dist/style.css'):
            return Promise.resolve({ body: simpleCssContent })
          default:
            throw new Error(`unexpected fetch called: ${request.url}`)
        }
      },
    )
    const fetchNodeModulesResult = await fetchNodeModules([
      requestedNpmDependency('mypackage', '0.0.1'),
    ])
    if (fetchNodeModulesResult.dependenciesWithError.length > 0) {
      fail(`Expected successful nodeModules fetch`)
    }
    const nodeModules = fetchNodeModulesResult.nodeModules
    function addNewModules(newModules: NodeModules) {
      const updatedNodeModules = { ...nodeModules, ...newModules }
      const updatedReq = getRequireFn(NO_OP, updatedNodeModules)

      // this is like calling `import 'mypackage/dist/style.css';`, we only care about the side effect
      updatedReq('/src/index.js', 'mypackage/dist/style.css')

      // our CSS side effect code ran by now, so we should be able to find the relevant style tag on the JSDOM
      const styleTag = document.getElementById(
        `${InjectedCSSFilePrefix}/node_modules/mypackage/dist/style.css`,
      )
      expect(styleTag?.innerHTML).toEqual(simpleCssContent)

      done()
    }
    const req = getRequireFn(addNewModules, nodeModules)
    const styleCss = req('/src/index.js', 'mypackage/dist/style.css')
    expect(Object.keys(styleCss)).toHaveLength(0)
  })
})

describe('ES Dependency manager - retry behavior', () => {
  it('retries a failed request', async (done) => {
    let requestCounter = 0
    ;(fetch as any).mockResponse(
      (request: Request): Promise<{ body?: string; status?: number }> => {
        switch (request.url) {
          case getPackagerUrl(resolvedNpmDependency('react-spring', '8.0.27')):
            if (requestCounter === 0) {
              requestCounter++
              throw new Error('First request fails')
            }
            return Promise.resolve({ status: 200, body: JSON.stringify(reactSpringServerResponse) })
          case getJsDelivrListUrl(resolvedNpmDependency('react-spring', '8.0.27')):
            return Promise.resolve({ status: 404 }) // we don't care about the jsdelivr response now
          default:
            throw new Error(`unexpected fetch called: ${request.url}`)
        }
      },
    )

    fetchNodeModules([requestedNpmDependency('react-spring', '8.0.27')]).then(
      (fetchNodeModulesResult) => {
        if (fetchNodeModulesResult.dependenciesWithError.length > 0) {
          fail(`Expected successful nodeModule fetch`)
        }
        expect(Object.keys(fetchNodeModulesResult.nodeModules)).toHaveLength(228)
        expect(
          fetchNodeModulesResult.nodeModules['/node_modules/react-spring/index.d.ts'],
        ).toBeDefined()
        done()
      },
    )
  })

  it('stops retrying after retry limit reached', async (done) => {
    ;(fetch as any).mockResponse(
      (request: Request): Promise<{ body?: string; status?: number }> => {
        throw new Error('All requests fail')
      },
    )

    fetchNodeModules([requestedNpmDependency('react-spring', '8.0.27')]).then(
      (fetchNodeModulesResult) => {
        expect(fetchNodeModulesResult.dependenciesWithError).toHaveLength(1)
        expect(fetchNodeModulesResult.dependenciesWithError[0].name).toBe('react-spring')
        expect(Object.keys(fetchNodeModulesResult.nodeModules)).toHaveLength(0)
        done()
      },
    )
  })

  it('does not retry if set to', async (done) => {
    let requestCounter = 0
    ;(fetch as any).mockResponse(
      (request: Request): Promise<{ body?: string; status?: number }> => {
        switch (request.url) {
          case getPackagerUrl(resolvedNpmDependency('react-spring', '8.0.27')):
            if (requestCounter === 0) {
              requestCounter++
              throw new Error('First request fails')
            }
            return Promise.resolve({ status: 200, body: JSON.stringify(reactSpringServerResponse) })
          case getJsDelivrListUrl(resolvedNpmDependency('react-spring', '8.0.27')):
            return Promise.resolve({ status: 404 }) // we don't care about the jsdelivr response now
          default:
            throw new Error(`unexpected fetch called: ${request.url}`)
        }
      },
    )

    fetchNodeModules([requestedNpmDependency('react-spring', '8.0.27')], false).then(
      (fetchNodeModulesResult) => {
        expect(fetchNodeModulesResult.dependenciesWithError).toHaveLength(1)
        expect(fetchNodeModulesResult.dependenciesWithError[0].name).toBe('react-spring')
        expect(Object.keys(fetchNodeModulesResult.nodeModules)).toHaveLength(0)
        done()
      },
    )
  })
})
