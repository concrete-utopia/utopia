import { fetchMissingFileDependency } from './fetch-packages'
import { esRemoteDependencyPlaceholder } from '../../shared/project-file-types'
import { getJsDelivrFileUrl } from './packager-url'

require('jest-fetch-mock').enableMocks()

const simpleCssContent = '.utopiaClass { background-color: red; }'

describe('Fetch missing file dependency', () => {
  it('extract node modules from package response', async () => {
    const fetchUrl = getJsDelivrFileUrl('mypackage@0.0.1', '/dist/style.css')
    ;(fetch as any).mockResponse(
      (request: Request): Promise<{ body?: string; status?: number }> => {
        switch (request.url) {
          case fetchUrl:
            return Promise.resolve({ body: simpleCssContent })
          default:
            throw new Error(`unexpected fetch called: ${request.url}`)
        }
      },
    )
    const updateNodeModules = jest.fn()
    await fetchMissingFileDependency(
      esRemoteDependencyPlaceholder(fetchUrl, false),
      'mypackage/dist/style.css',
    ).then(updateNodeModules)
    expect(updateNodeModules).toBeCalledWith({
      ['mypackage/dist/style.css']: {
        fileContents: simpleCssContent,
        type: 'ES_CODE_FILE',
        fullPath: 'mypackage/dist/style.css',
        origin: 'NODE_MODULES',
      },
    })
  })
})
