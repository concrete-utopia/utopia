import { createModifiedProject } from '../../../sample-projects/sample-project-utils.test-utils'
import { setFeatureForBrowserTestsUseInDescribeBlockOnly } from '../../../utils/utils.test-utils'
import { StoryboardFilePath } from '../../editor/store/editor-state'
import { renderTestEditorWithModel } from '../ui-jsx.test-utils'
import { createRouteManifestFromProjectContents } from './remix-utils'

const storyboardFileContent = `
import * as React from 'react';
import Utopia, {
  Scene,
  Storyboard,
  RemixContainer,
} from 'utopia-api';


export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <Scene
      data-uid='scene'
      style={{ position: 'absolute', left: 400, top: 0, width: 375, height: 812 }}
    >
      <RemixContainer />
    </Scene>
  </Storyboard>
);
`

describe('Route manifest', () => {
  setFeatureForBrowserTestsUseInDescribeBlockOnly('Remix support', true)
  it('Parses the route manifest from a simple project', async () => {
    const project = createModifiedProject({
      [StoryboardFilePath]: storyboardFileContent,
      ['src/root.js']: '',
      ['src/_index.js']: '',
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
        module: '',
        hasAction: false,
        hasLoader: false,
        hasCatchBoundary: false,
        hasErrorBoundary: false,
        filePath: '/src/routes/posts.$postId.js',
      },
      'routes/posts._index': {
        file: 'routes/posts._index.js',
        id: 'routes/posts._index',
        path: 'posts',
        index: true,
        parentId: 'root',
        module: '',
        hasAction: false,
        hasLoader: false,
        hasCatchBoundary: false,
        hasErrorBoundary: false,
        filePath: '/src/routes/posts._index.js',
      },
      root: {
        path: '',
        id: 'root',
        file: 'root.js',
        parentId: '',
        module: '',
        hasAction: false,
        hasLoader: false,
        hasCatchBoundary: false,
        hasErrorBoundary: false,
        filePath: '/src/root.js',
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
        module: '',
        hasAction: false,
        hasLoader: false,
        hasCatchBoundary: false,
        hasErrorBoundary: false,
        filePath: '/src/routes/notes.$noteId.js',
      },
      'routes/notes._index': {
        file: 'routes/notes._index.js',
        id: 'routes/notes._index',
        index: true,
        parentId: 'routes/notes',
        module: '',
        hasAction: false,
        hasLoader: false,
        hasCatchBoundary: false,
        hasErrorBoundary: false,
        filePath: '/src/routes/notes._index.js',
      },
      'routes/healthcheck': {
        file: 'routes/healthcheck.js',
        id: 'routes/healthcheck',
        path: 'healthcheck',
        parentId: 'root',
        module: '',
        hasAction: false,
        hasLoader: false,
        hasCatchBoundary: false,
        hasErrorBoundary: false,
        filePath: '/src/routes/healthcheck.js',
      },
      'routes/notes.new': {
        file: 'routes/notes.new.js',
        id: 'routes/notes.new',
        path: 'new',
        parentId: 'routes/notes',
        module: '',
        hasAction: false,
        hasLoader: false,
        hasCatchBoundary: false,
        hasErrorBoundary: false,
        filePath: '/src/routes/notes.new.js',
      },
      'routes/_index': {
        file: 'routes/_index.js',
        id: 'routes/_index',
        index: true,
        parentId: 'root',
        module: '',
        hasAction: false,
        hasLoader: false,
        hasCatchBoundary: false,
        hasErrorBoundary: false,
        filePath: '/src/routes/_index.js',
      },
      'routes/logout': {
        file: 'routes/logout.js',
        id: 'routes/logout',
        path: 'logout',
        parentId: 'root',
        module: '',
        hasAction: false,
        hasLoader: false,
        hasCatchBoundary: false,
        hasErrorBoundary: false,
        filePath: '/src/routes/logout.js',
      },
      'routes/notes': {
        file: 'routes/notes.js',
        id: 'routes/notes',
        path: 'notes',
        parentId: 'root',
        module: '',
        hasAction: false,
        hasLoader: false,
        hasCatchBoundary: false,
        hasErrorBoundary: false,
        filePath: '/src/routes/notes.js',
      },
      'routes/join': {
        file: 'routes/join.js',
        id: 'routes/join',
        path: 'join',
        parentId: 'root',
        module: '',
        hasAction: false,
        hasLoader: false,
        hasCatchBoundary: false,
        hasErrorBoundary: false,
        filePath: '/src/routes/join.js',
      },
      root: {
        path: '',
        id: 'root',
        file: 'root.js',
        parentId: '',
        module: '',
        hasAction: false,
        hasLoader: false,
        hasCatchBoundary: false,
        hasErrorBoundary: false,
        filePath: '/src/root.js',
      },
    })
  })
})
