import * as fileNoImports from '../test-cases/file-no-imports.json'
import * as fileWithImports from '../test-cases/file-with-imports.json'
import * as fileWithLocalImport from '../test-cases/file-with-local-import.json'
import * as reactSpringServerResponse from '../test-cases/react-spring-server-response.json'
import * as antdPackagerResponse from '../test-cases/antd-packager-response.json'
import {
  getRequireFn,
  getDependencyTypeDefinitions,
  createResolvingRemoteDependencyError,
} from './package-manager'
import { evaluator } from '../evaluator/evaluator'
import {
  extractNodeModulesFromPackageResponse,
  fetchNodeModules,
  resetDepPackagerCache,
} from './fetch-packages'
import type { ESCodeFile } from '../../shared/project-file-types'
import { NO_OP } from '../../shared/utils'
import type { NodeModules } from '../../shared/project-file-types'
import { getPackagerUrl, getJsDelivrFileUrl } from './packager-url'
import type { VersionLookupResult } from '../../../components/editor/npm-dependency/npm-dependency'
import type { PackagerServerResponse } from '../../shared/npm-dependency-types'
import { requestedNpmDependency } from '../../shared/npm-dependency-types'
import {
  InjectedCSSFilePrefix,
  unimportAllButTheseCSSFiles,
} from '../../webpack-loaders/css-loader'
import { createBuiltInDependenciesList } from './built-in-dependencies-list'
import * as moduleResolutionExamples from '../test-cases/module-resolution-examples.json'
import { createNodeModules } from './test-utils'

require('jest-fetch-mock').enableMocks()

const simpleCssContent = '.utopiaClass{background-color:red}'

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
      version: {
        type: 'NPM_VERSION',
        version: versionRange,
      },
    })
  },
  checkPackageVersionExists: async (packageName: string, version: string): Promise<boolean> => {
    return Promise.resolve(true)
  },
}))

jest.mock('../../shared/css-utils', () => ({
  convertCssToUtopia: (input: string): string => input,
}))

describe('ES Dependency Package Manager', () => {
  it('resolves a file with no imports', () => {
    const reqFn = getRequireFn(
      NO_OP,
      {},
      extractNodeModulesFromPackageResponse(fileNoImports as PackagerServerResponse),
      {},
      createBuiltInDependenciesList(null),
    )
    const requireResult = reqFn('/src/index.js', 'mypackage')
    expect(requireResult).toHaveProperty('hello')
    expect((requireResult as any).hello).toEqual('hello!')
  })

  it('resolves a file with one simple import', () => {
    const reqFn = getRequireFn(
      NO_OP,
      {},
      extractNodeModulesFromPackageResponse(fileWithImports),
      {},
      createBuiltInDependenciesList(null),
    )
    const requireResult = reqFn('/src/index.js', 'mypackage')
    expect(requireResult).toHaveProperty('hello')
    expect((requireResult as any).hello).toEqual('hello!')
  })

  it('resolves a file with one local import', () => {
    const reqFn = getRequireFn(
      NO_OP,
      {},
      extractNodeModulesFromPackageResponse(fileWithLocalImport),
      {},
      createBuiltInDependenciesList(null),
    )
    const requireResult = reqFn('/src/index.js', 'mypackage')
    expect(requireResult).toHaveProperty('hello')
    expect((requireResult as any).hello).toEqual('hello!')
  })

  it('resolves a css import', () => {
    const reqFn = getRequireFn(
      NO_OP,
      {},
      extractNodeModulesFromPackageResponse(fileWithImports),
      {},
      createBuiltInDependenciesList(null),
    )
    reqFn('/src/index.js', 'mypackage/simple.css')

    const styleTag = document.getElementById('/node_modules/mypackage/simple.css')
    expect(styleTag).toBeDefined()
  })

  it('unloads a previously loaded css import', () => {
    const reqFn = getRequireFn(
      NO_OP,
      {},
      extractNodeModulesFromPackageResponse(fileWithImports),
      {},
      createBuiltInDependenciesList(null),
    )
    reqFn('/src/index.js', 'mypackage/simple.css')

    expect(document.getElementById('/node_modules/mypackage/simple.css')).toBeDefined()

    unimportAllButTheseCSSFiles([])
    expect(document.getElementById('/node_modules/mypackage/simple.css')).toBeNull()
  })

  it('throws exception on not found dependency', () => {
    const reqFn = getRequireFn(
      NO_OP,
      {},
      extractNodeModulesFromPackageResponse(fileWithImports),
      {},
      createBuiltInDependenciesList(null),
    )
    const test = () => reqFn('/src/index.js', 'mypackage2')
    expect(test).toThrowError(`Could not find dependency: 'mypackage2' relative to '/src/index.js`)
  })
})

describe('ES Dependency Manager — Cycles', () => {
  it('handles circular dependencies properly without running into an infinite loop', () => {
    const spyEvaluator = jest.fn(evaluator)
    const reqFn = getRequireFn(
      NO_OP,
      {},
      extractNodeModulesFromPackageResponse(fileWithImports),
      {},
      createBuiltInDependenciesList(null),
      spyEvaluator,
    )
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
          case getPackagerUrl('react-spring@8.0.27'):
            return Promise.resolve({ status: 200, body: JSON.stringify(reactSpringServerResponse) })
          default:
            throw new Error(`unexpected fetch called: ${request.url}`)
        }
      },
    )
    const fetchNodeModulesResult = await fetchNodeModules(
      NO_OP,
      [requestedNpmDependency('react-spring', '8.0.27')],
      createBuiltInDependenciesList(null),
    )
    if (fetchNodeModulesResult.dependenciesWithError.length > 0) {
      throw new Error(`Expected successful nodeModules fetch`)
    }
    const nodeModules = fetchNodeModulesResult.nodeModules
    const onRemoteModuleDownload = jest.fn()
    const req = getRequireFn(
      onRemoteModuleDownload,
      {},
      nodeModules,
      {},
      createBuiltInDependenciesList(null),
    )
    const reactSpring = req('/src/index.js', 'react-spring')
    expect(Object.keys(reactSpring)).not.toHaveLength(0)
    expect(onRemoteModuleDownload).toBeCalledTimes(0)
  })

  it('antd@4.2.5', (done) => {
    const spyEvaluator = jest.fn(evaluator)
    ;(fetch as any).mockResponse(
      (request: Request): Promise<{ body?: string; status?: number }> => {
        switch (request.url) {
          case getPackagerUrl('antd@4.2.5'):
            return Promise.resolve({ status: 200, body: JSON.stringify(antdPackagerResponse) })
          case getJsDelivrFileUrl('antd@4.2.5', '/dist/antd.css'):
            return Promise.resolve({ body: simpleCssContent })
          default:
            throw new Error(`unexpected fetch called: ${request.url}`)
        }
      },
    )
    void fetchNodeModules(
      NO_OP,
      [requestedNpmDependency('antd', '4.2.5')],
      createBuiltInDependenciesList(null),
    ).then((fetchNodeModulesResult) => {
      if (fetchNodeModulesResult.dependenciesWithError.length > 0) {
        throw new Error(`Expected successful nodeModules fetch`)
      }
      const nodeModules = fetchNodeModulesResult.nodeModules

      const onRemoteModuleDownload = async (moduleDownload: Promise<NodeModules>) => {
        const downloadedModules = await moduleDownload
        const updatedNodeModules = { ...nodeModules, ...downloadedModules }
        const innerOnRemoteModuleDownload = jest.fn()
        const updatedReq = getRequireFn(
          innerOnRemoteModuleDownload,
          {},
          updatedNodeModules,
          {},
          createBuiltInDependenciesList(null),
          spyEvaluator,
        )

        // this is like calling `import 'antd/dist/antd.css';`, we only care about the side effect
        updatedReq('/src/index.js', 'antd/dist/antd.css')

        // our CSS side effect code ran by now, so we should be able to find the relevant style tag on the JSDOM
        const styleTag = document.getElementById('/node_modules/antd/dist/antd.css')
        expect(styleTag).toBeDefined()
        expect(spyEvaluator).toHaveBeenCalledTimes(940)
        expect(innerOnRemoteModuleDownload).toBeCalledTimes(0)

        done()
      }

      const req = getRequireFn(
        onRemoteModuleDownload,
        {},
        nodeModules,
        {},
        createBuiltInDependenciesList(null),
        spyEvaluator,
      )
      const antd = req('/src/index.js', 'antd')
      expect(Object.keys(antd)).not.toHaveLength(0)
      expect(antd).toHaveProperty('Button')
      expect(() => req('/src/index.js', 'antd/dist/antd.css')).toThrow(
        createResolvingRemoteDependencyError('antd/dist/antd.css'),
      )
    })
  })
})

describe('ES Dependency Manager', () => {
  it('handles modules that throw an exception on import', async () => {
    const brokenModule = {
      contents: {
        '/node_modules/broken/index.js': {
          content: "throw new Error('Fail on import.')",
        },
      },
    }
    ;(fetch as any).mockResponse(
      (request: Request): Promise<{ body?: string; status?: number }> => {
        switch (request.url) {
          case getPackagerUrl('broken@1.0.0'):
            return Promise.resolve({ status: 200, body: JSON.stringify(brokenModule) })
          default:
            throw new Error(`unexpected fetch called: ${request.url}`)
        }
      },
    )
    const fetchNodeModulesResult = await fetchNodeModules(
      NO_OP,
      [requestedNpmDependency('broken', '1.0.0')],
      createBuiltInDependenciesList(null),
    )
    if (fetchNodeModulesResult.dependenciesWithError.length > 0) {
      throw new Error(`Expected successful nodeModules fetch`)
    }
    const nodeModules = fetchNodeModulesResult.nodeModules
    const onRemoteModuleDownload = jest.fn()
    const req = getRequireFn(
      onRemoteModuleDownload,
      {},
      nodeModules,
      {},
      createBuiltInDependenciesList(null),
    )
    expect(() => req('/src/index.js', 'broken')).toThrowErrorMatchingInlineSnapshot(
      `"Fail on import."`,
    )
  })
})

describe('ES Dependency Manager — d.ts', () => {
  it('get typings from node modules', async () => {
    ;(fetch as any).mockResponse(
      (request: Request): Promise<{ body?: string; status?: number }> => {
        switch (request.url) {
          case getPackagerUrl('react-spring@8.0.27'):
            return Promise.resolve({ status: 200, body: JSON.stringify(reactSpringServerResponse) })
          default:
            throw new Error(`unexpected fetch called: ${request.url}`)
        }
      },
    )

    const fetchNodeModulesResult = await fetchNodeModules(
      NO_OP,
      [requestedNpmDependency('react-spring', '8.0.27')],
      createBuiltInDependenciesList(null),
    )
    if (fetchNodeModulesResult.dependenciesWithError.length > 0) {
      throw new Error(`Expected successful nodeModules fetch`)
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
  it('downloads a css file from jsdelivr, if needed', (done) => {
    ;(fetch as any).mockResponse(
      (request: Request): Promise<{ body?: string; status?: number }> => {
        switch (request.url) {
          case getPackagerUrl('mypackage@0.0.1'):
            return Promise.resolve({ status: 200, body: JSON.stringify(fileNoImports) })
          case getJsDelivrFileUrl('mypackage@0.0.1', '/dist/style.css'):
            return Promise.resolve({ body: simpleCssContent })
          default:
            throw new Error(`unexpected fetch called: ${request.url}`)
        }
      },
    )
    void fetchNodeModules(
      NO_OP,
      [requestedNpmDependency('mypackage', '0.0.1')],
      createBuiltInDependenciesList(null),
    ).then((fetchNodeModulesResult) => {
      if (fetchNodeModulesResult.dependenciesWithError.length > 0) {
        throw new Error(`Expected successful nodeModules fetch`)
      }
      const nodeModules = fetchNodeModulesResult.nodeModules

      const onRemoteModuleDownload = async (moduleDownload: Promise<NodeModules>) => {
        const downloadedModules = await moduleDownload
        const updatedNodeModules = { ...nodeModules, ...downloadedModules }
        const innerOnRemoteModuleDownload = jest.fn()
        const updatedReq = getRequireFn(
          innerOnRemoteModuleDownload,
          {},
          updatedNodeModules,
          {},
          createBuiltInDependenciesList(null),
        )

        // this is like calling `import 'mypackage/dist/style.css';`, we only care about the side effect
        updatedReq('/src/index.js', 'mypackage/dist/style.css')

        // our CSS side effect code ran by now, so we should be able to find the relevant style tag on the JSDOM
        const styleTag = document.getElementById(
          `${InjectedCSSFilePrefix}/node_modules/mypackage/dist/style.css`,
        )
        expect(styleTag?.innerHTML).toEqual(simpleCssContent)
        expect(innerOnRemoteModuleDownload).toBeCalledTimes(0)

        done()
      }

      const req = getRequireFn(
        onRemoteModuleDownload,
        {},
        nodeModules,
        {},
        createBuiltInDependenciesList(null),
      )
      expect(() => req('/src/index.js', 'mypackage/dist/style.css')).toThrow(
        createResolvingRemoteDependencyError('mypackage/dist/style.css'),
      )
    })
  })
})

describe('ES Dependency manager - retry behavior', () => {
  it('retries a failed request', (done) => {
    let requestCounter = 0
    ;(fetch as any).mockResponse(
      (request: Request): Promise<{ body?: string; status?: number }> => {
        switch (request.url) {
          case getPackagerUrl('react-spring@8.0.27'):
            if (requestCounter === 0) {
              requestCounter++
              throw new Error('First request fails')
            }
            return Promise.resolve({ status: 200, body: JSON.stringify(reactSpringServerResponse) })
          default:
            throw new Error(`unexpected fetch called: ${request.url}`)
        }
      },
    )

    void fetchNodeModules(
      NO_OP,
      [requestedNpmDependency('react-spring', '8.0.27')],
      createBuiltInDependenciesList(null),
    ).then((fetchNodeModulesResult) => {
      if (fetchNodeModulesResult.dependenciesWithError.length > 0) {
        throw new Error(`Expected successful nodeModule fetch`)
      }
      expect(Object.keys(fetchNodeModulesResult.nodeModules)).toHaveLength(228)
      expect(
        fetchNodeModulesResult.nodeModules['/node_modules/react-spring/index.d.ts'],
      ).toBeDefined()
      done()
    })
  })

  it('stops retrying after retry limit reached', (done) => {
    ;(fetch as any).mockResponse(
      (request: Request): Promise<{ body?: string; status?: number }> => {
        throw new Error('All requests fail')
      },
    )

    void fetchNodeModules(
      NO_OP,
      [requestedNpmDependency('react-spring', '8.0.27')],
      createBuiltInDependenciesList(null),
    ).then((fetchNodeModulesResult) => {
      expect(fetchNodeModulesResult.dependenciesWithError).toHaveLength(1)
      expect(fetchNodeModulesResult.dependenciesWithError[0].name).toBe('react-spring')
      expect(Object.keys(fetchNodeModulesResult.nodeModules)).toHaveLength(0)
      done()
    })
  })

  it('does not retry if set to', (done) => {
    let requestCounter = 0
    ;(fetch as any).mockResponse(
      (request: Request): Promise<{ body?: string; status?: number }> => {
        switch (request.url) {
          case getPackagerUrl('react-spring@8.0.27'):
            if (requestCounter === 0) {
              requestCounter++
              throw new Error('First request fails')
            }
            return Promise.resolve({ status: 200, body: JSON.stringify(reactSpringServerResponse) })
          default:
            throw new Error(`unexpected fetch called: ${request.url}`)
        }
      },
    )

    void fetchNodeModules(
      NO_OP,
      [requestedNpmDependency('react-spring', '8.0.27')],
      createBuiltInDependenciesList(null),
      false,
    ).then((fetchNodeModulesResult) => {
      expect(fetchNodeModulesResult.dependenciesWithError).toHaveLength(1)
      expect(fetchNodeModulesResult.dependenciesWithError[0].name).toBe('react-spring')
      expect(Object.keys(fetchNodeModulesResult.nodeModules)).toHaveLength(0)
      done()
    })
  })
})

describe('ES Dependency manager - browser field substitutions', () => {
  const reqFn = getRequireFn(
    NO_OP,
    {},
    createNodeModules(moduleResolutionExamples.contents),
    {},
    createBuiltInDependenciesList(null),
  )

  it('returns the replaced module', () => {
    const requireResult = reqFn(
      '/node_modules/module-with-browser-replacements-chained/index.module.js',
      'some-module',
    )
    expect(requireResult).toEqual({
      value: 'local-shim for some-module chained through other-module',
    })
  })

  it('returns an empty object for an ignored dependency', () => {
    const requireResult = reqFn(
      '/node_modules/module-with-browser-replacements-ignore-module/index.module.js',
      'some-module',
    )
    expect(requireResult).toEqual({})
  })
})
