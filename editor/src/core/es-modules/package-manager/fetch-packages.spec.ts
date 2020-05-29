import { fetchMissingFileDependency, resetDepPackagerCache } from './fetch-packages'
import { esRemoteDependencyPlaceholder } from '../../shared/project-file-types'

require('jest-fetch-mock').enableMocks()

const simpleCssContent = '.utopiaClass { background-color: red; }'

describe('Fetch missing file dependency', () => {
  it('extract node modules from package response', async () => {
    ;(fetch as any).mockResponse(
      (request: Request): Promise<{ body?: string; status?: number }> => {
        switch (request.url) {
          case 'https://cdn.jsdelivr.net/npm/mypackage@0.0.1/dist/style.css':
            return Promise.resolve({ body: simpleCssContent })
          default:
            throw new Error(`unexpected fetch called: ${request.url}`)
        }
      },
    )
    const updateNodeModules = jest.fn()
    await fetchMissingFileDependency(
      updateNodeModules,
      esRemoteDependencyPlaceholder('mypackage', '0.0.1', false),
      'mypackage/dist/style.css',
    )
    expect(updateNodeModules).toBeCalledWith({
      ['mypackage/dist/style.css']: {
        evalResultCache: null,
        fileContents: simpleCssContent,
        type: 'ES_CODE_FILE',
      },
    })
  })
})
