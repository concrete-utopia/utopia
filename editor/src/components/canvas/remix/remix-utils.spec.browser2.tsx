import { createModifiedProject } from '../../../sample-projects/sample-project-utils.test-utils'
import { setFeatureForBrowserTestsUseInDescribeBlockOnly } from '../../../utils/utils.test-utils'
import { StoryboardFilePath } from '../../editor/store/editor-state'
import { renderTestEditorWithModel } from '../ui-jsx.test-utils'
import {
  DefaultFutureConfig,
  createRouteManifestFromProjectContents,
  getRoutesAndModulesFromManifest,
} from './remix-utils'

const storyboardFileContent = `
import * as React from 'react';
import Utopia, {
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

const rootFileContent = `import React from 'react'
import { Outlet } from '@remix-run/react'

export default function Root() {
  return (
    <div>
      This is root!
      <Outlet />
    </div>
  )
}`

const routeFileContent = (text: string) => `import React from 'react'

export default function Index() {
  return <h1>${text}</h1>
}
`

const routeFileContentWithOutlet = (text: string) => `import React from 'react'
import { Outlet } from '@remix-run/react'
export default function Index() {
  return <div>
    <h1>${text}</h1>
    <Outlet />
  </div>
}
`

describe('Route manifest', () => {
  setFeatureForBrowserTestsUseInDescribeBlockOnly('Remix support', true)
  it('Parses the route manifest from a simple project', async () => {
    const project = createModifiedProject({
      [StoryboardFilePath]: storyboardFileContent,
      ['/src/root.js']: rootFileContent,
      ['/src/routes/_index.js']: routeFileContent('Index route'),
      ['/src/routes/posts.$postId.js']: routeFileContent('A specific post'),
      ['/src/routes/posts._index.js']: routeFileContent('Posts'),
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
      ['/src/root.js']: rootFileContent,
      ['/src/routes/_index.js']: routeFileContent('Index route'),
      ['/src/routes/healthcheck.js']: routeFileContent("Stayin' alive"),
      ['/src/routes/join.js']: routeFileContent('Join me, and together we can rule the galaxy'),
      ['/src/routes/logout.js']: routeFileContent('Goodbye'),
      ['/src/routes/notes.js']: routeFileContentWithOutlet('Notes'),
      ['/src/routes/notes._index.js']: routeFileContent('Notes too'),
      ['/src/routes/notes.$noteId.js']: routeFileContent('A specific note'),
      ['/src/routes/notes.new.js']: routeFileContent('Dear diary'),
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
      ['/src/root.js']: rootFileContent,
      ['/src/routes/_index.js']: routeFileContent('Index route'),
      ['/src/routes/posts.$postId.js']: routeFileContent('A specific post'),
      ['/src/routes/posts._index.js']: routeFileContent('Posts'),
    })
    const renderResult = await renderTestEditorWithModel(project, 'await-first-dom-report')

    const remixManifest = createRouteManifestFromProjectContents(
      renderResult.getEditorState().editor.projectContents,
    )
    expect(remixManifest).not.toBeNull()

    let routeModuleCache = { current: {} }
    const remixRoutes = getRoutesAndModulesFromManifest(
      remixManifest!,
      DefaultFutureConfig,
      renderResult.getEditorState().editor.codeResultCache.curriedRequireFn,
      renderResult.getEditorState().editor.codeResultCache.curriedResolveFn,
      renderResult.getEditorState().editor.projectContents,
      routeModuleCache.current,
    )?.routes
    expect(remixRoutes).toBeDefined()

    expect(remixRoutes).toHaveLength(1)
    expect(remixRoutes![0]).toEqual(
      expect.objectContaining({ id: 'root', path: '', index: undefined }),
    )
    expect(remixRoutes![0].children).toHaveLength(3)
    expect(remixRoutes![0]!.children![0]).toEqual(
      expect.objectContaining({
        id: 'routes/posts.$postId',
        path: 'posts/:postId',
        index: undefined,
      }),
    )
    expect(remixRoutes![0]!.children![1]).toEqual(
      expect.objectContaining({ id: 'routes/posts._index', path: 'posts', index: true }),
    )
    expect(remixRoutes![0]!.children![2]).toEqual(
      expect.objectContaining({ id: 'routes/_index', path: undefined, index: true }),
    )
  })
  it('Parses the routes from the Remix Blog Tutorial project files', async () => {
    const project = createModifiedProject({
      [StoryboardFilePath]: storyboardFileContent,
      ['/src/root.js']: rootFileContent,
      ['/src/routes/_index.js']: routeFileContent('Index route'),
      ['/src/routes/healthcheck.js']: routeFileContent("Stayin' alive"),
      ['/src/routes/join.js']: routeFileContent('Join me, and together we can rule the galaxy'),
      ['/src/routes/logout.js']: routeFileContent('Goodbye'),
      ['/src/routes/notes._index.js']: routeFileContent('Notes too'),
      ['/src/routes/notes.$noteId.js']: routeFileContent('A specific note'),
      ['/src/routes/notes.new.js']: routeFileContent('Dear diary'),
      ['/src/routes/notes.js']: routeFileContentWithOutlet('Notes'),
    })
    const renderResult = await renderTestEditorWithModel(project, 'await-first-dom-report')

    const remixManifest = createRouteManifestFromProjectContents(
      renderResult.getEditorState().editor.projectContents,
    )

    let routeModuleCache = { current: {} }
    expect(remixManifest).not.toBeNull()
    const remixRoutes = getRoutesAndModulesFromManifest(
      remixManifest!,
      DefaultFutureConfig,
      renderResult.getEditorState().editor.codeResultCache.curriedRequireFn,
      renderResult.getEditorState().editor.codeResultCache.curriedResolveFn,
      renderResult.getEditorState().editor.projectContents,
      routeModuleCache.current,
    )?.routes
    expect(remixRoutes).toBeDefined()

    expect(remixRoutes).toHaveLength(1)
    expect(remixRoutes![0]).toEqual(
      expect.objectContaining({ id: 'root', path: '', index: undefined }),
    )

    expect(remixRoutes![0].children).toHaveLength(5)
    expect(remixRoutes![0]!.children![0]).toEqual(
      expect.objectContaining({
        id: 'routes/healthcheck',
        path: 'healthcheck',
        index: undefined,
      }),
    )
    expect(remixRoutes![0]!.children![1]).toEqual(
      expect.objectContaining({ id: 'routes/_index', path: undefined, index: true }),
    )
    expect(remixRoutes![0]!.children![2]).toEqual(
      expect.objectContaining({ id: 'routes/logout', path: 'logout', index: undefined }),
    )
    expect(remixRoutes![0]!.children![3]).toEqual(
      expect.objectContaining({ id: 'routes/notes', path: 'notes', index: undefined }),
    )
    expect(remixRoutes![0]!.children![4]).toEqual(
      expect.objectContaining({ id: 'routes/join', path: 'join', index: undefined }),
    )

    expect(remixRoutes![0]!.children![3].children).toHaveLength(3)
    expect(remixRoutes![0]!.children![3].children![0]).toEqual(
      expect.objectContaining({ id: 'routes/notes.$noteId', path: ':noteId', index: undefined }),
    )
    expect(remixRoutes![0]!.children![3].children![1]).toEqual(
      expect.objectContaining({ id: 'routes/notes._index', path: undefined, index: true }),
    )
    expect(remixRoutes![0]!.children![3].children![2]).toEqual(
      expect.objectContaining({ id: 'routes/notes.new', path: 'new', index: undefined }),
    )
  })
})
