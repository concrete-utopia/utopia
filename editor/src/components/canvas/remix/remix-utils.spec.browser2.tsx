import { createModifiedProject } from '../../../sample-projects/sample-project-utils.test-utils'
import { setFeatureForBrowserTestsUseInDescribeBlockOnly } from '../../../utils/utils.test-utils'
import { CreateRemixDerivedDataRefs, StoryboardFilePath } from '../../editor/store/editor-state'
import { renderTestEditorWithModel } from '../ui-jsx.test-utils'
import {
  DefaultFutureConfig,
  createRouteManifestFromProjectContents,
  getRoutesAndModulesFromManifest,
  getRoutesFromRouteManifest,
} from './remix-utils'

const storyboardFileContent = `
import * as React from 'react';
import Utopia, {
  Scene,
  Storyboard,
  RemixScene,
} from 'utopia-api';


export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <RemixScene
      data-uid='scene'
      style={{ position: 'absolute', left: 400, top: 0, width: 375, height: 812 }}
    />
  </Storyboard>
);
`

describe('Route manifest', () => {
  setFeatureForBrowserTestsUseInDescribeBlockOnly('Remix support', true)
  it('Parses the route manifest from a simple project', async () => {
    const project = createModifiedProject({
      [StoryboardFilePath]: storyboardFileContent,
      ['src/root.js']: '',
      ['src/routes/_index.js']: '',
      ['src/routes/posts.$postId.js']: '',
      ['src/routes/posts._index.js']: '',
    })
    const renderResult = await renderTestEditorWithModel(project, 'await-first-dom-report')

    const remixManifest = createRouteManifestFromProjectContents(
      renderResult.getEditorState().editor.projectContents,
    )

    expect(remixManifest).toEqual({
      'routes/posts.$postId': {
        file: 'routes/posts.$postId.js',
        id: 'routes/posts.$postId',
        path: 'posts/:postId',
        parentId: 'root',
        module: '/src/routes/posts.$postId.js',
        hasAction: false,
        hasLoader: false,
        hasCatchBoundary: false,
        hasErrorBoundary: false,
      },
      'routes/posts._index': {
        file: 'routes/posts._index.js',
        id: 'routes/posts._index',
        path: 'posts',
        index: true,
        parentId: 'root',
        module: '/src/routes/posts._index.js',
        hasAction: false,
        hasLoader: false,
        hasCatchBoundary: false,
        hasErrorBoundary: false,
      },
      root: {
        path: '',
        id: 'root',
        file: 'root.js',
        parentId: '',
        module: '/src/root.js',
        hasAction: false,
        hasLoader: false,
        hasCatchBoundary: false,
        hasErrorBoundary: false,
      },
      'routes/_index': {
        file: 'routes/_index.js',
        hasAction: false,
        hasCatchBoundary: false,
        hasErrorBoundary: false,
        hasLoader: false,
        id: 'routes/_index',
        index: true,
        module: '/src/routes/_index.js',
        parentId: 'root',
        path: undefined,
      },
    })
  })
  it('Returns null to a non-remix project', async () => {
    const project = createModifiedProject({
      [StoryboardFilePath]: storyboardFileContent,
    })
    const renderResult = await renderTestEditorWithModel(project, 'await-first-dom-report')

    const remixManifest = createRouteManifestFromProjectContents(
      renderResult.getEditorState().editor.projectContents,
    )

    expect(remixManifest).toBeNull()
  })
  it('Parses the route manifest from the Remix Blog Tutorial project files', async () => {
    const project = createModifiedProject({
      [StoryboardFilePath]: storyboardFileContent,
      ['src/root.js']: '',
      ['src/routes/_index.js']: '',
      ['src/routes/healthcheck.js']: '',
      ['src/routes/join.js']: '',
      ['src/routes/logout.js']: '',
      ['src/routes/notes._index.js']: '',
      ['src/routes/notes.$noteId.js']: '',
      ['src/routes/notes.new.js']: '',
      ['src/routes/notes.js']: '',
    })
    const renderResult = await renderTestEditorWithModel(project, 'await-first-dom-report')

    const remixManifest = createRouteManifestFromProjectContents(
      renderResult.getEditorState().editor.projectContents,
    )

    expect(remixManifest).toEqual({
      'routes/notes.$noteId': {
        file: 'routes/notes.$noteId.js',
        id: 'routes/notes.$noteId',
        path: ':noteId',
        parentId: 'routes/notes',
        module: '/src/routes/notes.$noteId.js',
        hasAction: false,
        hasLoader: false,
        hasCatchBoundary: false,
        hasErrorBoundary: false,
      },
      'routes/notes._index': {
        file: 'routes/notes._index.js',
        id: 'routes/notes._index',
        index: true,
        parentId: 'routes/notes',
        module: '/src/routes/notes._index.js',
        hasAction: false,
        hasLoader: false,
        hasCatchBoundary: false,
        hasErrorBoundary: false,
      },
      'routes/healthcheck': {
        file: 'routes/healthcheck.js',
        id: 'routes/healthcheck',
        path: 'healthcheck',
        parentId: 'root',
        module: '/src/routes/healthcheck.js',
        hasAction: false,
        hasLoader: false,
        hasCatchBoundary: false,
        hasErrorBoundary: false,
      },
      'routes/notes.new': {
        file: 'routes/notes.new.js',
        id: 'routes/notes.new',
        path: 'new',
        parentId: 'routes/notes',
        module: '/src/routes/notes.new.js',
        hasAction: false,
        hasLoader: false,
        hasCatchBoundary: false,
        hasErrorBoundary: false,
      },
      'routes/_index': {
        file: 'routes/_index.js',
        id: 'routes/_index',
        index: true,
        parentId: 'root',
        module: '/src/routes/_index.js',
        hasAction: false,
        hasLoader: false,
        hasCatchBoundary: false,
        hasErrorBoundary: false,
      },
      'routes/logout': {
        file: 'routes/logout.js',
        id: 'routes/logout',
        path: 'logout',
        parentId: 'root',
        module: '/src/routes/logout.js',
        hasAction: false,
        hasLoader: false,
        hasCatchBoundary: false,
        hasErrorBoundary: false,
      },
      'routes/notes': {
        file: 'routes/notes.js',
        id: 'routes/notes',
        path: 'notes',
        parentId: 'root',
        module: '/src/routes/notes.js',
        hasAction: false,
        hasLoader: false,
        hasCatchBoundary: false,
        hasErrorBoundary: false,
      },
      'routes/join': {
        file: 'routes/join.js',
        id: 'routes/join',
        path: 'join',
        parentId: 'root',
        module: '/src/routes/join.js',
        hasAction: false,
        hasLoader: false,
        hasCatchBoundary: false,
        hasErrorBoundary: false,
      },
      root: {
        path: '',
        id: 'root',
        file: 'root.js',
        parentId: '',
        module: '/src/root.js',
        hasAction: false,
        hasLoader: false,
        hasCatchBoundary: false,
        hasErrorBoundary: false,
      },
    })
  })
})

describe('Routes', () => {
  setFeatureForBrowserTestsUseInDescribeBlockOnly('Remix support', true)
  it('Parses the routes from a simple project', async () => {
    const project = createModifiedProject({
      [StoryboardFilePath]: storyboardFileContent,
      ['src/root.js']: '',
      ['src/routes/_index.js']: '',
      ['src/routes/posts.$postId.js']: '',
      ['src/routes/posts._index.js']: '',
    })
    const renderResult = await renderTestEditorWithModel(project, 'await-first-dom-report')

    const remixManifest = createRouteManifestFromProjectContents(
      renderResult.getEditorState().editor.projectContents,
    )
    expect(remixManifest).not.toBeNull()
    const remixRoutes = getRoutesFromRouteManifest(remixManifest!, DefaultFutureConfig)

    expect(remixRoutes).toHaveLength(1)
    expect(remixRoutes[0]).toEqual(
      expect.objectContaining({ id: 'root', path: '', index: undefined }),
    )
    expect(remixRoutes[0].children).toHaveLength(3)
    expect(remixRoutes[0]!.children![0]).toEqual(
      expect.objectContaining({
        id: 'routes/posts.$postId',
        path: 'posts/:postId',
        index: undefined,
      }),
    )
    expect(remixRoutes[0]!.children![1]).toEqual(
      expect.objectContaining({ id: 'routes/posts._index', path: 'posts', index: true }),
    )
    expect(remixRoutes[0]!.children![2]).toEqual(
      expect.objectContaining({ id: 'routes/_index', path: undefined, index: true }),
    )
  })
  it('Parses the routes from the Remix Blog Tutorial project files', async () => {
    const project = createModifiedProject({
      [StoryboardFilePath]: storyboardFileContent,
      ['src/root.js']: '',
      ['src/routes/_index.js']: '',
      ['src/routes/healthcheck.js']: '',
      ['src/routes/join.js']: '',
      ['src/routes/logout.js']: '',
      ['src/routes/notes._index.js']: '',
      ['src/routes/notes.$noteId.js']: '',
      ['src/routes/notes.new.js']: '',
      ['src/routes/notes.js']: '',
    })
    const renderResult = await renderTestEditorWithModel(project, 'await-first-dom-report')

    const remixManifest = createRouteManifestFromProjectContents(
      renderResult.getEditorState().editor.projectContents,
    )

    expect(remixManifest).not.toBeNull()
    const remixRoutes = getRoutesFromRouteManifest(remixManifest!, DefaultFutureConfig)

    expect(remixRoutes).toHaveLength(1)
    expect(remixRoutes[0]).toEqual(
      expect.objectContaining({ id: 'root', path: '', index: undefined }),
    )

    expect(remixRoutes[0].children).toHaveLength(5)
    expect(remixRoutes[0]!.children![0]).toEqual(
      expect.objectContaining({
        id: 'routes/healthcheck',
        path: 'healthcheck',
        index: undefined,
      }),
    )
    expect(remixRoutes[0]!.children![1]).toEqual(
      expect.objectContaining({ id: 'routes/_index', path: undefined, index: true }),
    )
    expect(remixRoutes[0]!.children![2]).toEqual(
      expect.objectContaining({ id: 'routes/logout', path: 'logout', index: undefined }),
    )
    expect(remixRoutes[0]!.children![3]).toEqual(
      expect.objectContaining({ id: 'routes/notes', path: 'notes', index: undefined }),
    )
    expect(remixRoutes[0]!.children![4]).toEqual(
      expect.objectContaining({ id: 'routes/join', path: 'join', index: undefined }),
    )

    expect(remixRoutes[0]!.children![3].children).toHaveLength(3)
    expect(remixRoutes[0]!.children![3].children![0]).toEqual(
      expect.objectContaining({ id: 'routes/notes.$noteId', path: ':noteId', index: undefined }),
    )
    expect(remixRoutes[0]!.children![3].children![1]).toEqual(
      expect.objectContaining({ id: 'routes/notes._index', path: undefined, index: true }),
    )
    expect(remixRoutes[0]!.children![3].children![2]).toEqual(
      expect.objectContaining({ id: 'routes/notes.new', path: 'new', index: undefined }),
    )
  })
})

describe('Route modules', () => {
  setFeatureForBrowserTestsUseInDescribeBlockOnly('Remix support', true)
  it('Parses the route modules from a simple project', async () => {
    const project = createModifiedProject({
      [StoryboardFilePath]: storyboardFileContent,
      ['src/root.js']: `import * as React from 'react'
      import { Outlet } from '@remix-run/react'
      
      export default function App() {
        return (
          <div
            style={{
              backgroundColor: '#a5c0db',
              width: '100%',
              height: '100%',
              contain: 'layout',
            }}
            data-uid='bf5'
          >
            <div
              data-uid='d4d'
              style={{
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'flex-end',
                fontSize: '8px',
                fontWeight: 700,
                paddingTop: 0,
                paddingRight: 0,
                paddingBottom: 0,
                paddingLeft: 0,
              }}
            >
              Root.js
            </div>
            <Outlet data-uid='11c' />
          </div>
        )
      }      
`,
      ['src/routes/_index.js']: `import React from 'react'
      import { Link } from '@remix-run/react'
      
      export default function Index() {
        return (
          <div
            style={{
              display: 'flex',
              flexDirection: 'column',
              gap: 24,
              padding: '0px 8px',
            }}
          >
            <span
              style={{
                fontSize: '40px',
                fontWeight: 700,
                fontStyle: 'normal',
                color: 'rgb(0, 0, 0, 1)',
              }}
              data-uid='ttt'
            >
              Beaches
            </span>
            <div
              style={{
                backgroundColor: '#e6e6e6',
                width: '100%',
                height: 79,
                display: 'flex',
                flexDirection: 'row',
                alignItems: 'center',
                justifyContent: 'center',
                borderRadius: 26,
              }}
              data-uid='419'
            >
              <Link
                style={{
                  wordBreak: 'break-word',
                  color: 'rgb(255, 251, 251, 1)',
                  contain: 'layout',
                  fontSize: '20px',
                  width: 'max-content',
                  height: 'max-content',
                  fontWeight: 700,
                }}
                to='/posts'
              >
                Check avaliable beaches
              </Link>
            </div>
          </div>
        )
      }
      
`,
      ['src/routes/posts.$postId.js']: '',
      ['src/routes/posts._index.js']: `import React from 'react'
      import { Link } from '@remix-run/react'
      import { json, useLoaderData } from 'react-router'
      
      export function loader() {
        return json({
          beaches: [
            {
              id: 1,
              name: 'La Digue',
              src:
                'https://source.unsplash.com/jPmurJKSL_0/600x800',
            },
            {
              id: 2,
              name: 'McWay Falls',
              src:
                'https://source.unsplash.com/07mSKrzKiRw/600x800',
            },
          ],
        })
      }
      
      export default function Posts() {
        const { beaches } = useLoaderData()
        return (
          <div
            style={{
              padding: '10px',
              backgroundColor: 'white',
              height: '100%',
            }}
            data-uid='289'
          >
            <span
              style={{
                fontSize: '30px',
                fontWeight: 700,
                fontStyle: 'normal',
                marginBottom: 20,
                display: 'inline-block',
              }}
              data-uid='8b1'
            >
              Beaches near you
            </span>
            {beaches.map(({ id, name, src }) => (
              <div
                style={{
                  width: '100%',
                  height: 'max-content',
                  display: 'flex',
                  flexDirection: 'row',
                  gap: 26,
                  padding: '15px',
                  alignItems: 'center',
                  justifyContent: 'center',
                  borderRadius: 20,
                  border: '1px solid black',
                  marginBottom: '10px',
                }}
                data-uid='1b9'
              >
                <img
                  style={{
                    backgroundColor: '#aaaaaa33',
                    width: 77,
                    height: 75,
                    contain: 'layout',
                    borderRadius: 20,
                  }}
                  src={src}
                  data-uid='824'
                />
                <span
                  style={{
                    wordBreak: 'break-word',
                    width: 110,
                    height: 33,
                    contain: 'layout',
                  }}
                  data-uid='97f'
                >
                  <Link to={\`\${id}\`} data-uid='5bb'>
                    {name}
                  </Link>
                </span>
              </div>
            ))}
          </div>
        )
      }
      `,
    })

    const renderResult = await renderTestEditorWithModel(project, 'await-first-dom-report')

    const editor = renderResult.getEditorState().editor
    const remixManifest = createRouteManifestFromProjectContents(editor.projectContents)

    const spyContainer = { current: {} }
    const propsContainer = { current: {} }

    const metadataCtx = {
      current: {
        spyValues: { metadata: spyContainer.current, allElementProps: propsContainer.current },
      },
    }

    const routesAndRouteModules = getRoutesAndModulesFromManifest(
      remixManifest!,
      DefaultFutureConfig,
      editor.codeResultCache.curriedRequireFn,
      editor.codeResultCache.curriedResolveFn,
      metadataCtx,
      editor.projectContents,
      CreateRemixDerivedDataRefs.mutableContext,
      CreateRemixDerivedDataRefs.topLevelComponentRendererComponents,
    )

    expect(routesAndRouteModules).not.toBeNull()
  })
})
