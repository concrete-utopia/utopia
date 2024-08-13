import { createModifiedProject } from '../../../sample-projects/sample-project-utils.test-utils'
import { StoryboardFilePath } from '../../editor/store/editor-state'
import {
  CreateRemixDerivedDataRefsGLOBAL,
  REMIX_CONFIG_JS_PATH,
  getRemixRootDir,
} from '../../editor/store/remix-derived-data'
import { renderTestEditorWithModel } from '../ui-jsx.test-utils'
import {
  DefaultFutureConfig,
  createRouteManifestFromProjectContents,
  getRemixRootFile,
  getRoutesAndModulesFromManifest,
} from './remix-utils'
import { RouteExportsForRouteObject } from './utopia-remix-root-component'

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

const remixConfigJsFromRemixDocs = `
/** @type {import('@remix-run/dev').AppConfig} */
module.exports = {
  appDirectory: "src",
};
`

const rootFileContentWithExportedStuff = `import React from 'react'
import { Outlet } from '@remix-run/react'
import { json } from 'react-router'

export function loader() {
  return json({
    activities: [
      {
        id: 0,
        name: 'Do the thing',
      },
    ]
  })
}

export const handle = () => ({
  its: "all yours",
});

export const shouldRevalidate = () => true;

export const links = () => [{
  rel: "stylesheet",
  href: "https://example.com/some/styles.css",
}];

export const meta = () => [{ title: "Very cool app | Remix" }];

export function action() {
  return json({ message: "this is a dummy action function" })
}

export default function Root() {
  return (
    <div>
      This is root!
      <Outlet />
    </div>
  )
}`

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
  it('Parses the route manifest from a simple project', async () => {
    const project = createModifiedProject({
      [StoryboardFilePath]: storyboardFileContent,
      ['/app/root.js']: rootFileContent,
      ['/app/routes/_index.js']: routeFileContent('Index route'),
      ['/app/routes/posts.$postId.js']: routeFileContent('A specific post'),
      ['/app/routes/posts._index.js']: routeFileContent('Posts'),
    })
    const renderResult = await renderTestEditorWithModel(project, 'await-first-dom-report')

    const rootDir = getRemixRootDir(renderResult.getEditorState().editor.projectContents)
    const rootFilePath =
      getRemixRootFile(rootDir, renderResult.getEditorState().editor.projectContents)?.path ?? ''
    const remixManifest = createRouteManifestFromProjectContents(
      { rootDir, rootFilePath },
      renderResult.getEditorState().editor.projectContents,
    )

    expect(remixManifest).toEqual({
      'routes/posts.$postId': {
        file: 'routes/posts.$postId.js',
        id: 'routes/posts.$postId',
        path: 'posts/:postId',
        parentId: 'root',
        module: '/app/routes/posts.$postId.js',
        hasAction: false,
        hasLoader: false,
        hasErrorBoundary: false,
      },
      'routes/posts._index': {
        file: 'routes/posts._index.js',
        id: 'routes/posts._index',
        path: 'posts',
        index: true,
        parentId: 'root',
        module: '/app/routes/posts._index.js',
        hasAction: false,
        hasLoader: false,
        hasErrorBoundary: false,
      },
      root: {
        path: '',
        id: 'root',
        file: 'root.js',
        parentId: '',
        module: '/app/root.js',
        hasAction: false,
        hasLoader: false,
        hasErrorBoundary: false,
      },
      'routes/_index': {
        file: 'routes/_index.js',
        hasAction: false,
        hasErrorBoundary: false,
        hasLoader: false,
        id: 'routes/_index',
        index: true,
        module: '/app/routes/_index.js',
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

    const rootDir = getRemixRootDir(renderResult.getEditorState().editor.projectContents)
    const rootFilePath =
      getRemixRootFile(rootDir, renderResult.getEditorState().editor.projectContents)?.path ?? ''
    const remixManifest = createRouteManifestFromProjectContents(
      { rootDir, rootFilePath },
      renderResult.getEditorState().editor.projectContents,
    )

    expect(remixManifest).toBeNull()
  })
  it('Parses the route manifest from the Remix Blog Tutorial project files', async () => {
    const project = createModifiedProject({
      [StoryboardFilePath]: storyboardFileContent,
      ['/app/root.js']: rootFileContent,
      ['/app/routes/_index.js']: routeFileContent('Index route'),
      ['/app/routes/healthcheck.js']: routeFileContent("Stayin' alive"),
      ['/app/routes/join.js']: routeFileContent('Join me, and together we can rule the galaxy'),
      ['/app/routes/logout.js']: routeFileContent('Goodbye'),
      ['/app/routes/notes.js']: routeFileContentWithOutlet('Notes'),
      ['/app/routes/notes._index.js']: routeFileContent('Notes too'),
      ['/app/routes/notes.$noteId.js']: routeFileContent('A specific note'),
      ['/app/routes/notes.new.js']: routeFileContent('Dear diary'),
    })
    const renderResult = await renderTestEditorWithModel(project, 'await-first-dom-report')

    const rootDir = getRemixRootDir(renderResult.getEditorState().editor.projectContents)
    const rootFilePath =
      getRemixRootFile(rootDir, renderResult.getEditorState().editor.projectContents)?.path ?? ''
    const remixManifest = createRouteManifestFromProjectContents(
      { rootDir, rootFilePath },
      renderResult.getEditorState().editor.projectContents,
    )

    expect(remixManifest).toEqual({
      'routes/notes.$noteId': {
        file: 'routes/notes.$noteId.js',
        id: 'routes/notes.$noteId',
        path: ':noteId',
        parentId: 'routes/notes',
        module: '/app/routes/notes.$noteId.js',
        hasAction: false,
        hasLoader: false,
        hasErrorBoundary: false,
      },
      'routes/notes._index': {
        file: 'routes/notes._index.js',
        id: 'routes/notes._index',
        index: true,
        parentId: 'routes/notes',
        module: '/app/routes/notes._index.js',
        hasAction: false,
        hasLoader: false,
        hasErrorBoundary: false,
      },
      'routes/healthcheck': {
        file: 'routes/healthcheck.js',
        id: 'routes/healthcheck',
        path: 'healthcheck',
        parentId: 'root',
        module: '/app/routes/healthcheck.js',
        hasAction: false,
        hasLoader: false,
        hasErrorBoundary: false,
      },
      'routes/notes.new': {
        file: 'routes/notes.new.js',
        id: 'routes/notes.new',
        path: 'new',
        parentId: 'routes/notes',
        module: '/app/routes/notes.new.js',
        hasAction: false,
        hasLoader: false,
        hasErrorBoundary: false,
      },
      'routes/_index': {
        file: 'routes/_index.js',
        id: 'routes/_index',
        index: true,
        parentId: 'root',
        module: '/app/routes/_index.js',
        hasAction: false,
        hasLoader: false,
        hasErrorBoundary: false,
      },
      'routes/logout': {
        file: 'routes/logout.js',
        id: 'routes/logout',
        path: 'logout',
        parentId: 'root',
        module: '/app/routes/logout.js',
        hasAction: false,
        hasLoader: false,
        hasErrorBoundary: false,
      },
      'routes/notes': {
        file: 'routes/notes.js',
        id: 'routes/notes',
        path: 'notes',
        parentId: 'root',
        module: '/app/routes/notes.js',
        hasAction: false,
        hasLoader: false,
        hasErrorBoundary: false,
      },
      'routes/join': {
        file: 'routes/join.js',
        id: 'routes/join',
        path: 'join',
        parentId: 'root',
        module: '/app/routes/join.js',
        hasAction: false,
        hasLoader: false,
        hasErrorBoundary: false,
      },
      root: {
        path: '',
        id: 'root',
        file: 'root.js',
        parentId: '',
        module: '/app/root.js',
        hasAction: false,
        hasLoader: false,
        hasErrorBoundary: false,
      },
    })
  })
})

describe('Routes', () => {
  it('Parses the routes from a simple project', async () => {
    const project = createModifiedProject({
      [StoryboardFilePath]: storyboardFileContent,
      ['/app/root.js']: rootFileContent,
      ['/app/routes/_index.js']: routeFileContent('Index route'),
      ['/app/routes/posts.$postId.js']: routeFileContent('A specific post'),
      ['/app/routes/posts._index.js']: routeFileContent('Posts'),
    })
    const renderResult = await renderTestEditorWithModel(project, 'await-first-dom-report')

    const rootDir = getRemixRootDir(renderResult.getEditorState().editor.projectContents)
    const rootFilePath =
      getRemixRootFile(rootDir, renderResult.getEditorState().editor.projectContents)?.path ?? ''
    const remixManifest = createRouteManifestFromProjectContents(
      { rootDir, rootFilePath },
      renderResult.getEditorState().editor.projectContents,
    )
    expect(remixManifest).not.toBeNull()

    let routeModuleCache = { current: {} }
    const remixRoutes = getRoutesAndModulesFromManifest(
      getRemixRootFile(rootDir, renderResult.getEditorState().editor.projectContents)!.file,
      remixManifest!,
      DefaultFutureConfig,
      renderResult.getEditorState().editor.codeResultCache.curriedRequireFn,
      renderResult.getEditorState().editor.codeResultCache.curriedResolveFn,
      renderResult.getEditorState().editor.projectContents,
      routeModuleCache.current,
      renderResult.getEditorState().editor.editorRemixConfig,
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
  it('Parses exported functions', async () => {
    const project = createModifiedProject({
      [StoryboardFilePath]: storyboardFileContent,
      ['/app/root.js']: rootFileContentWithExportedStuff,
      ['/app/routes/_index.js']: routeFileContent('Index route'),
    })

    const renderResult = await renderTestEditorWithModel(project, 'await-first-dom-report')

    const remixRoutes = renderResult.getEditorState().derived.remixData?.routes
    expect(remixRoutes).toBeDefined()

    expect(remixRoutes).toHaveLength(1)
    for (const routeExport of RouteExportsForRouteObject) {
      expect(remixRoutes![0][routeExport]).toBeDefined()
    }

    expect(remixRoutes![0].handle?.()).toEqual({
      its: 'all yours',
    })

    expect(remixRoutes![0].shouldRevalidate?.({} as any)).toEqual(true)

    const { meta, links } = CreateRemixDerivedDataRefsGLOBAL.routeModulesCache.current['root']
    expect(meta).toBeDefined()
    expect(links).toBeDefined()

    expect(meta?.({} as any)).toEqual([{ title: 'Very cool app | Remix' }])
    expect(links?.()).toEqual([
      {
        rel: 'stylesheet',
        href: 'https://example.com/some/styles.css',
      },
    ])
  })
  it('Parses root.jsx', async () => {
    const project = createModifiedProject({
      [StoryboardFilePath]: storyboardFileContent,
      ['/app/root.jsx']: rootFileContentWithExportedStuff,
      ['/app/routes/_index.js']: routeFileContent('Index route'),
    })

    const renderResult = await renderTestEditorWithModel(project, 'await-first-dom-report')

    const remixRoutes = renderResult.getEditorState().derived.remixData?.routes
    expect(remixRoutes).toBeDefined()

    expect(remixRoutes).toHaveLength(1)
    expect(remixRoutes![0].id).toEqual('root')
    expect(remixRoutes![0].children).toHaveLength(1)
  })
  it('Parses different route dir', async () => {
    const project = createModifiedProject({
      [REMIX_CONFIG_JS_PATH]: remixConfigJsFromRemixDocs,
      [StoryboardFilePath]: storyboardFileContent,
      ['/src/root.js']: rootFileContentWithExportedStuff,
      ['/src/routes/_index.js']: routeFileContent('Index route'),
    })

    const renderResult = await renderTestEditorWithModel(project, 'await-first-dom-report')

    const remixRoutes = renderResult.getEditorState().derived.remixData?.routes
    expect(remixRoutes).toBeDefined()

    expect(remixRoutes).toHaveLength(1)
    expect(remixRoutes![0].children).toHaveLength(1)
  })
  it('Parses the routes from the Remix Blog Tutorial project files', async () => {
    const project = createModifiedProject({
      [StoryboardFilePath]: storyboardFileContent,
      ['/app/root.js']: rootFileContent,
      ['/app/routes/_index.js']: routeFileContent('Index route'),
      ['/app/routes/healthcheck.js']: routeFileContent("Stayin' alive"),
      ['/app/routes/join.js']: routeFileContent('Join me, and together we can rule the galaxy'),
      ['/app/routes/logout.js']: routeFileContent('Goodbye'),
      ['/app/routes/notes._index.js']: routeFileContent('Notes too'),
      ['/app/routes/notes.$noteId.js']: routeFileContent('A specific note'),
      ['/app/routes/notes.new.js']: routeFileContent('Dear diary'),
      ['/app/routes/notes.js']: routeFileContentWithOutlet('Notes'),
    })
    const renderResult = await renderTestEditorWithModel(project, 'await-first-dom-report')

    const rootDir = getRemixRootDir(renderResult.getEditorState().editor.projectContents)
    const rootFilePath =
      getRemixRootFile(rootDir, renderResult.getEditorState().editor.projectContents)?.path ?? ''
    const remixManifest = createRouteManifestFromProjectContents(
      { rootDir, rootFilePath },
      renderResult.getEditorState().editor.projectContents,
    )

    let routeModuleCache = { current: {} }
    expect(remixManifest).not.toBeNull()
    const remixRoutes = getRoutesAndModulesFromManifest(
      getRemixRootFile(rootDir, renderResult.getEditorState().editor.projectContents)!.file,
      remixManifest!,
      DefaultFutureConfig,
      renderResult.getEditorState().editor.codeResultCache.curriedRequireFn,
      renderResult.getEditorState().editor.codeResultCache.curriedResolveFn,
      renderResult.getEditorState().editor.projectContents,
      routeModuleCache.current,
      renderResult.getEditorState().editor.editorRemixConfig,
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
