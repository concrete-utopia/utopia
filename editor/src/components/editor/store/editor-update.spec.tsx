import * as Chai from 'chai'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import {
  emptyComments,
  isJSXElement,
  jsxAttributesFromMap,
  jsExpressionValue,
  jsxElement,
  jsxElementName,
} from '../../../core/shared/element-template'
import { findJSXElementChildAtPath } from '../../../core/model/element-template-utils'
import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
import type {
  ElementPath,
  ProjectContents,
  StaticElementPath,
} from '../../../core/shared/project-file-types'
import { directory } from '../../../core/shared/project-file-types'
import {
  isTextFile,
  esCodeFile,
  importDetails,
  importAlias,
  isParseSuccess,
  RevisionsState,
  unparsed,
  textFile,
  textFileContents,
} from '../../../core/shared/project-file-types'
import { MockUtopiaTsWorkers } from '../../../core/workers/workers'
import { isRight, right } from '../../../core/shared/either'
import { createEditorStates, createFakeMetadataForEditor } from '../../../utils/utils.test-utils'
import Utils, { front } from '../../../utils/utils'
import { renameComponent } from '../../navigator/actions'
import * as EP from '../../../core/shared/element-path'
import * as fileWithImports from '../../../core/es-modules/test-cases/file-with-imports.json'
import * as fileNoImports from '../../../core/es-modules/test-cases/file-no-imports.json'
import { createNodeModules } from '../../../core/es-modules/package-manager/test-utils'
import {
  clearSelection,
  duplicateSelected,
  duplicateSpecificElements,
  insertJSXElement,
  moveSelectedBackward,
  openCodeEditorFile,
  selectComponents,
  toggleCollapse,
  togglePanel,
  updateFrameDimensions,
  setSafeMode,
  setSaveError,
  updateNodeModulesContents,
  updatePackageJson,
  addToast,
  removeToast,
  deleteSelected,
  insertAsChildTarget,
} from '../actions/action-creators'
import * as History from '../history'
import type { EditorState } from './editor-state'
import {
  defaultUserState,
  StoryboardFilePath,
  getJSXComponentsAndImportsForPathFromState,
  DefaultPackageJson,
  regularNavigatorEntry,
  emptyCollaborativeEditingSupport,
} from './editor-state'
import { runLocalEditorAction } from './editor-update'
import { getLayoutPropertyOr } from '../../../core/layout/getLayoutProperty'
import {
  ScenePathForTestUiJsFile,
  ScenePath1ForTestUiJsFile,
  Scene1UID,
} from '../../../core/model/test-ui-js-file.test-utils'
import { emptyUiJsxCanvasContextData } from '../../canvas/ui-jsx-canvas'
import { requestedNpmDependency } from '../../../core/shared/npm-dependency-types'
import { contentsToTree, getProjectFileByFilePath } from '../../assets'
import { forceParseSuccessFromFileOrFail } from '../../../core/workers/parser-printer/parser-printer.test-utils'
import { notice } from '../../common/notice'
import {
  getPrintedUiJsCode,
  renderTestEditorWithProjectContent,
  TestAppUID,
  TestScenePath,
  TestSceneUID,
} from '../../canvas/ui-jsx.test-utils'
import { PrettierConfig } from 'utopia-vscode-common'
import { BakedInStoryboardUID } from '../../../core/model/scene-utils'
import { createCodeFile } from '../../custom-code/code-file.test-utils'
import * as Prettier from 'prettier'
import type { BuiltInDependencies } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { createBuiltInDependenciesList } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { NO_OP } from '../../../core/shared/utils'
import { cssNumber } from '../../inspector/common/css-utils'
import { testStaticElementPath } from '../../../core/shared/element-path.test-utils'
import { styleStringInArray } from '../../../utils/common-constants'
import { getUtopiaID } from '../../../core/shared/uid-utils'
import { printCode, printCodeOptions } from '../../../core/workers/parser-printer/parser-printer'
import { emptyProjectServerState } from './project-server-state'

const chaiExpect = Chai.expect

/* eslint-disable @typescript-eslint/no-empty-function */

const workers = new MockUtopiaTsWorkers()

const testScenePath = ScenePath1ForTestUiJsFile
const testElementPath = EP.appendNewElementPath(ScenePath1ForTestUiJsFile, ['pancake'])
const builtInDependencies: BuiltInDependencies = createBuiltInDependenciesList(null)

describe('action SELECT_VIEWS', () => {
  it('updates selectedview in editor', () => {
    const { editor, derivedState, dispatch } = createEditorStates()
    const action = selectComponents([testElementPath], false)
    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      defaultUserState,
      workers,
      action,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
      builtInDependencies,
      emptyCollaborativeEditingSupport(),
      emptyProjectServerState(),
    )
    chaiExpect(updatedEditor.selectedViews).to.deep.equal([testElementPath])
  })
  it('doesnt update navigator collapse toggle when no child is selected', () => {
    const { editor, derivedState, dispatch } = createEditorStates()
    const action = toggleCollapse(testElementPath)
    const editorAfterToggle = runLocalEditorAction(
      editor,
      derivedState,
      defaultUserState,
      workers,
      action,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
      builtInDependencies,
      emptyCollaborativeEditingSupport(),
      emptyProjectServerState(),
    )
    const action2 = selectComponents([testElementPath], false)
    const updatedEditor = runLocalEditorAction(
      editorAfterToggle,
      derivedState,
      defaultUserState,
      workers,
      action2,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
      builtInDependencies,
      emptyCollaborativeEditingSupport(),
      emptyProjectServerState(),
    )
    chaiExpect(updatedEditor.selectedViews).to.deep.equal([testElementPath])
    chaiExpect(updatedEditor.navigator.collapsedViews).to.deep.equal([testElementPath])
  })
  it('allows single selection of a scene', () => {
    const { editor, derivedState, dispatch } = createEditorStates()
    const action = selectComponents([testScenePath], false)
    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      defaultUserState,
      workers,
      action,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
      builtInDependencies,
      emptyCollaborativeEditingSupport(),
      emptyProjectServerState(),
    )
    chaiExpect(updatedEditor.selectedViews).to.deep.equal([testScenePath])
  })
})

describe('action CLEAR_SELECTION', () => {
  it('clears selectedview in editor', () => {
    const { editor, derivedState, dispatch } = createEditorStates()
    const action = selectComponents([testElementPath], false)
    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      defaultUserState,
      workers,
      action,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
      builtInDependencies,
      emptyCollaborativeEditingSupport(),
      emptyProjectServerState(),
    )
    chaiExpect(updatedEditor.selectedViews).to.deep.equal([testElementPath])

    const clearAction = clearSelection()
    const updatedEditor2 = runLocalEditorAction(
      updatedEditor,
      derivedState,
      defaultUserState,
      workers,
      clearAction,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
      builtInDependencies,
      emptyCollaborativeEditingSupport(),
      emptyProjectServerState(),
    )
    chaiExpect(updatedEditor2.selectedViews).to.deep.equal([])
  })
})

describe('action RENAME_COMPONENT', () => {
  function checkRename(target: ElementPath, expectedDefaultName: string): void {
    const { editor, derivedState, dispatch } = createEditorStates()
    const newName = 'newName'
    const renameAction = renameComponent(target, newName)
    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      defaultUserState,
      workers,
      renameAction,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
      builtInDependencies,
      emptyCollaborativeEditingSupport(),
      emptyProjectServerState(),
    )
    const updatedMetadata = createFakeMetadataForEditor(updatedEditor)
    chaiExpect(
      MetadataUtils.getElementLabel(
        editor.allElementProps,
        target,
        editor.elementPathTree,
        updatedMetadata,
      ),
    ).to.deep.equal(newName)

    const clearNameAction = renameComponent(target, null)
    const clearedNameEditor = runLocalEditorAction(
      updatedEditor,
      derivedState,
      defaultUserState,
      workers,
      clearNameAction,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
      builtInDependencies,
      emptyCollaborativeEditingSupport(),
      emptyProjectServerState(),
    )
    const clearedNameMetadata = createFakeMetadataForEditor(clearedNameEditor)
    chaiExpect(
      MetadataUtils.getElementLabel(
        editor.allElementProps,
        target,
        editor.elementPathTree,
        clearedNameMetadata,
      ),
    ).to.deep.equal(expectedDefaultName)
  }

  it('renames an existing scene', () => checkRename(ScenePathForTestUiJsFile, 'Test'))
  it('renames an existing element', () =>
    checkRename(EP.appendNewElementPath(ScenePathForTestUiJsFile, ['aaa']), 'View'))
})

describe('action TOGGLE_PANE', () => {
  it('can toggle inspector visibility', () => {
    const { editor, derivedState, dispatch } = createEditorStates()
    const action = togglePanel('inspector')
    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      defaultUserState,
      workers,
      action,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
      builtInDependencies,
      emptyCollaborativeEditingSupport(),
      emptyProjectServerState(),
    )
    const updatedEditor2 = runLocalEditorAction(
      updatedEditor,
      derivedState,
      defaultUserState,
      workers,
      action,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
      builtInDependencies,
      emptyCollaborativeEditingSupport(),
      emptyProjectServerState(),
    )
    chaiExpect(updatedEditor2.inspector.visible).to.not.equal(updatedEditor.inspector.visible)
  })

  it('can toggle preview visibility', () => {
    const { editor, derivedState, dispatch } = createEditorStates()
    const action = togglePanel('preview')
    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      defaultUserState,
      workers,
      action,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
      builtInDependencies,
      emptyCollaborativeEditingSupport(),
      emptyProjectServerState(),
    )
    const updatedEditor2 = runLocalEditorAction(
      updatedEditor,
      derivedState,
      defaultUserState,
      workers,
      action,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
      builtInDependencies,
      emptyCollaborativeEditingSupport(),
      emptyProjectServerState(),
    )
    chaiExpect(updatedEditor2.preview.visible).to.not.equal(updatedEditor.preview.visible)
  })
})

describe('action DUPLICATE_SPECIFIC_ELEMENTS', () => {
  it('duplicates 1 element', () => {
    const { editor, derivedState, dispatch } = createEditorStates([
      EP.appendNewElementPath(ScenePathForTestUiJsFile, ['aaa', 'iii']),
    ])
    const duplicateAction = duplicateSpecificElements([
      EP.appendNewElementPath(ScenePathForTestUiJsFile, ['aaa', 'iii']),
    ])
    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      defaultUserState,
      workers,
      duplicateAction,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
      builtInDependencies,
      emptyCollaborativeEditingSupport(),
      emptyProjectServerState(),
    )
    const mainUIJSFile = getProjectFileByFilePath(updatedEditor.projectContents, StoryboardFilePath)
    const oldUIJSFile = getProjectFileByFilePath(editor.projectContents, StoryboardFilePath)
    if (
      oldUIJSFile != null &&
      isTextFile(oldUIJSFile) &&
      isParseSuccess(oldUIJSFile.fileContents.parsed) &&
      mainUIJSFile != null &&
      isTextFile(mainUIJSFile) &&
      isParseSuccess(mainUIJSFile.fileContents.parsed)
    ) {
      const updatedChildren = Utils.pathOr(
        [],
        [0, 'rootElement', 'children'],
        getUtopiaJSXComponentsFromSuccess(mainUIJSFile.fileContents.parsed),
      )
      const oldChildren = Utils.pathOr(
        [],
        [0, 'rootElement', 'children'],
        getUtopiaJSXComponentsFromSuccess(oldUIJSFile.fileContents.parsed),
      )
      expect(updatedChildren).toHaveLength(oldChildren.length + 1)
    } else {
      chaiExpect.fail('src/app.js file was the wrong type.')
    }
  })
  it('duplicates multiple elements', () => {
    const element1 = EP.appendNewElementPath(ScenePathForTestUiJsFile, ['aaa', 'iii'])
    const element2 = EP.appendNewElementPath(ScenePathForTestUiJsFile, ['aaa', 'ddd'])
    const { editor, derivedState, dispatch } = createEditorStates([element1, element2])
    const duplicateAction = duplicateSelected()
    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      defaultUserState,
      workers,
      duplicateAction,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
      builtInDependencies,
      emptyCollaborativeEditingSupport(),
      emptyProjectServerState(),
    )
    const mainUIJSFile = getProjectFileByFilePath(updatedEditor.projectContents, StoryboardFilePath)
    const oldUIJSFile = getProjectFileByFilePath(editor.projectContents, StoryboardFilePath)
    if (
      oldUIJSFile != null &&
      isTextFile(oldUIJSFile) &&
      isParseSuccess(oldUIJSFile.fileContents.parsed) &&
      mainUIJSFile != null &&
      isTextFile(mainUIJSFile) &&
      isParseSuccess(mainUIJSFile.fileContents.parsed)
    ) {
      const updatedComponents = getUtopiaJSXComponentsFromSuccess(mainUIJSFile.fileContents.parsed)
      const updatedChildren = Utils.pathOr([], [0, 'rootElement', 'children'], updatedComponents)
      const originalComponents = getUtopiaJSXComponentsFromSuccess(oldUIJSFile.fileContents.parsed)
      const originalChildren = Utils.pathOr([], [0, 'rootElement', 'children'], originalComponents)
      expect(updatedChildren).toHaveLength(originalChildren.length + 2)
      expect(updatedEditor.selectedViews).toHaveLength(2)
      expect(updatedEditor.selectedViews.find((view) => view === element1)).toBe(undefined)
      expect(updatedEditor.selectedViews.find((view) => view === element2)).toBe(undefined)
      const newElements = Utils.stripNulls(
        updatedEditor.selectedViews.map((view) => {
          return findJSXElementChildAtPath(updatedComponents, view as StaticElementPath)
        }),
      )
      const newElementsInOriginalModel = Utils.stripNulls(
        updatedEditor.selectedViews.map((view) => {
          return findJSXElementChildAtPath(originalComponents, view as StaticElementPath)
        }),
      )
      expect(newElements).toHaveLength(2)
      expect(newElementsInOriginalModel).toHaveLength(0)
    } else {
      chaiExpect.fail('src/app.js file was the wrong type.')
    }
  })
  it('does not duplicate a root element of an instance', () => {
    const element1 = EP.appendNewElementPath(ScenePathForTestUiJsFile, ['aaa'])
    const { editor, derivedState, dispatch } = createEditorStates([element1])
    const duplicateAction = duplicateSelected()
    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      defaultUserState,
      workers,
      duplicateAction,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
      builtInDependencies,
      emptyCollaborativeEditingSupport(),
      emptyProjectServerState(),
    )
    expect(updatedEditor.projectContents).toEqual(editor.projectContents)
  })
})

describe('action DELETE_SELECTED', () => {
  it('deletes all selected elements', () => {
    const firstTargetElementPath = EP.appendNewElementPath(
      ScenePathForTestUiJsFile,
      EP.staticElementPath(['aaa', 'mmm', 'bbb']),
    )
    const secondTargetElementPath = EP.appendNewElementPath(
      ScenePathForTestUiJsFile,
      EP.staticElementPath(['aaa', 'iii']),
    )
    const targetScenePath = ScenePath1ForTestUiJsFile

    const { editor, derivedState, dispatch } = createEditorStates([
      firstTargetElementPath,
      secondTargetElementPath,
      targetScenePath,
    ])

    const parseSuccess = forceParseSuccessFromFileOrFail(
      getProjectFileByFilePath(editor.projectContents, StoryboardFilePath),
    )
    const originalChildrenCount = Utils.pathOr(
      [],
      [0, 'rootElement', 'children'],
      parseSuccess.topLevelElements,
    ).length

    const deleteAction = deleteSelected()
    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      defaultUserState,
      workers,
      deleteAction,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
      builtInDependencies,
      emptyCollaborativeEditingSupport(),
      emptyProjectServerState(),
    )
    const mainUIJSFile = getProjectFileByFilePath(updatedEditor.projectContents, StoryboardFilePath)
    if (
      mainUIJSFile != null &&
      isTextFile(mainUIJSFile) &&
      isParseSuccess(mainUIJSFile.fileContents.parsed)
    ) {
      expect(
        Utils.pathOr(
          [],
          [0, 'rootElement', 'children'],
          getUtopiaJSXComponentsFromSuccess(mainUIJSFile.fileContents.parsed),
        ),
      ).toHaveLength(originalChildrenCount - 1)
      expect(
        Utils.pathOr(
          [],
          [0, 'rootElement', 'children', 0, 'children'],
          getUtopiaJSXComponentsFromSuccess(mainUIJSFile.fileContents.parsed),
        ),
      ).toHaveLength(1)
      expect(
        findJSXElementChildAtPath(
          getUtopiaJSXComponentsFromSuccess(mainUIJSFile.fileContents.parsed),
          firstTargetElementPath,
        ),
      ).toBeNull()
      expect(
        findJSXElementChildAtPath(
          getUtopiaJSXComponentsFromSuccess(mainUIJSFile.fileContents.parsed),
          secondTargetElementPath,
        ),
      ).toBeNull()
      expect(updatedEditor.selectedViews).toEqual([
        EP.appendNewElementPath(ScenePathForTestUiJsFile, EP.staticElementPath(['aaa', 'mmm'])),
        EP.appendNewElementPath(ScenePathForTestUiJsFile, EP.staticElementPath(['aaa'])),
        testStaticElementPath([[BakedInStoryboardUID, Scene1UID]]),
      ])
    } else {
      chaiExpect.fail('src/app.js file was the wrong type.')
    }
  })
  it('deletes selected element multifile', async () => {
    const appFilePath = '/src/app.js'
    let projectContents: ProjectContents = {
      '/package.json': textFile(
        textFileContents(
          JSON.stringify(DefaultPackageJson, null, 2),
          unparsed,
          RevisionsState.BothMatch,
        ),
        null,
        null,
        0,
      ),
      '/src': directory(),
      '/utopia': directory(),
      [StoryboardFilePath]: createCodeFile(
        StoryboardFilePath,
        `
  import * as React from 'react'
  import Utopia, {
    Scene,
    Storyboard,
  } from 'utopia-api'
  import { App } from '/src/app.js'

  export var storyboard = (
    <Storyboard data-uid='${BakedInStoryboardUID}'>
      <Scene
        data-uid='${TestSceneUID}'
        style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
      >
        <App data-uid='${TestAppUID}' />
      </Scene>
    </Storyboard>
  )`,
      ),
      [appFilePath]: createCodeFile(
        appFilePath,
        `
  import * as React from 'react'
  export var App = (props) => {
    return <div data-uid='app-outer-div' style={{position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF'}}>
      <div data-uid='app-inner-div' />
      <div data-uid='app-inner-div-to-delete' style={{width: 10}}><span>hello</span></div>
    </div>
  }`,
      ),
    }
    const renderResult = await renderTestEditorWithProjectContent(
      contentsToTree(projectContents),
      'dont-await-first-dom-report',
    )
    const targetPath = EP.appendNewElementPath(TestScenePath, [
      'app-outer-div',
      'app-inner-div-to-delete',
    ])

    await renderResult.dispatch([selectComponents([targetPath], false)], false)
    await renderResult.dispatch([deleteSelected()], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState(), appFilePath)).toEqual(
      Prettier.format(
        `import * as React from 'react'
      export var App = (props) => {
        return (
          <div
            data-uid='app-outer-div'
            style={{ position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}
          >
            <div data-uid='app-inner-div' />
          </div>
        )
      }`,
        PrettierConfig,
      ),
    )
  })
})

describe('INSERT_JSX_ELEMENT', () => {
  function testInsertionToParent(parentPath: StaticElementPath) {
    const { editor, derivedState, dispatch } = createEditorStates()

    const parentBeforeInsert = findJSXElementChildAtPath(
      getJSXComponentsAndImportsForPathFromState(parentPath, editor).components,
      parentPath,
    )

    const elementToInsert = jsxElement(
      jsxElementName('View', []),
      'TestView',
      jsxAttributesFromMap({ 'data-uid': jsExpressionValue('TestView', emptyComments) }),
      [],
    )
    const insertAction = insertJSXElement(elementToInsert, parentPath, {
      'utopia-api': importDetails(null, [importAlias('View')], null),
    })
    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      defaultUserState,
      workers,
      insertAction,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
      builtInDependencies,
      emptyCollaborativeEditingSupport(),
      emptyProjectServerState(),
    )
    const updatedComponents = getJSXComponentsAndImportsForPathFromState(
      parentPath,
      updatedEditor,
    ).components
    const parentAfterInsert = findJSXElementChildAtPath(updatedComponents, parentPath)
    const insertedElement = findJSXElementChildAtPath(
      updatedComponents,
      EP.appendToPath(parentPath, 'TestView'),
    )
    if (parentAfterInsert != null && parentBeforeInsert != null) {
      expect(Utils.pathOr(0, ['children', 'length'], parentAfterInsert)).toEqual(
        Utils.pathOr(0, ['children', 'length'], parentBeforeInsert) + 1,
      )
    }
    expect(insertedElement).toBeDefined()
  }

  it('inserts an element', () => {
    testInsertionToParent(
      EP.appendNewElementPath(ScenePathForTestUiJsFile, EP.staticElementPath(['aaa'])),
    )
    testInsertionToParent(
      EP.appendNewElementPath(
        ScenePathForTestUiJsFile,
        EP.staticElementPath(['aaa', 'mmm', 'bbb']),
      ),
    )
    testInsertionToParent(
      EP.appendNewElementPath(
        ScenePathForTestUiJsFile,
        EP.staticElementPath(['aaa', 'ddd', 'eee']),
      ),
    )
  })

  it('fails to insert to nonexistent parent', () => {
    expect(() => {
      testInsertionToParent(
        EP.appendNewElementPath(
          ScenePathForTestUiJsFile,
          EP.staticElementPath(['aaa', 'i-dont-exist']),
        ),
      )
    }).toThrow()
  })

  xit('inserts an element as a new root component', () => {
    // TODO Scene Implementation
    const { editor, derivedState, dispatch } = createEditorStates()
    const editorWithNoHighlighted: EditorState = {
      ...editor,
      highlightedViews: [],
    }

    const componentsBeforeInsert = getJSXComponentsAndImportsForPathFromState(
      ScenePathForTestUiJsFile,
      editor,
    ).components

    const elementToInsert = jsxElement(
      jsxElementName('View', []),
      'TestView',
      jsxAttributesFromMap({ 'data-uid': jsExpressionValue('TestView', emptyComments) }),
      [],
    )
    const insertAction = insertJSXElement(elementToInsert, null, {
      'utopia-api': importDetails(null, [importAlias('View')], null),
    })
    const updatedEditor = runLocalEditorAction(
      editorWithNoHighlighted,
      derivedState,
      defaultUserState,
      workers,
      insertAction,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
      builtInDependencies,
      emptyCollaborativeEditingSupport(),
      emptyProjectServerState(),
    )
    const updatedComponents = getJSXComponentsAndImportsForPathFromState(
      ScenePathForTestUiJsFile,
      updatedEditor,
    ).components

    const insertedElement = findJSXElementChildAtPath(
      updatedComponents,
      EP.appendNewElementPath(ScenePathForTestUiJsFile, EP.staticElementPath(['TestView'])),
    )
    expect(updatedComponents.length).toEqual(componentsBeforeInsert.length + 1)
    expect(insertedElement).toBeDefined()
  })
})

describe('action UPDATE_FRAME_DIMENSIONS', () => {
  it('updates text element frame dimension', () => {
    const { editor, derivedState, dispatch } = createEditorStates()
    const targetText = EP.appendNewElementPath(
      ScenePathForTestUiJsFile,
      EP.staticElementPath(['aaa', 'hhh']),
    )
    const newWidth = 300
    const newHeight = 400
    const updateFrameDimensionsAction = updateFrameDimensions(targetText, newWidth, newHeight)
    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      defaultUserState,
      workers,
      updateFrameDimensionsAction,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
      builtInDependencies,
      emptyCollaborativeEditingSupport(),
      emptyProjectServerState(),
    )
    const mainUIJSFile = getProjectFileByFilePath(updatedEditor.projectContents, StoryboardFilePath)
    if (
      mainUIJSFile != null &&
      isTextFile(mainUIJSFile) &&
      isParseSuccess(mainUIJSFile.fileContents.parsed)
    ) {
      const components = getUtopiaJSXComponentsFromSuccess(mainUIJSFile.fileContents.parsed)
      const textElement = Utils.forceNotNull(
        'Target text should exist',
        findJSXElementChildAtPath(components, targetText),
      )
      if (isJSXElement(textElement)) {
        expect(
          getLayoutPropertyOr(undefined, 'width', right(textElement.props), styleStringInArray),
        ).toEqual(cssNumber(newWidth))
        expect(
          getLayoutPropertyOr(undefined, 'height', right(textElement.props), styleStringInArray),
        ).toEqual(cssNumber(newHeight))
      } else {
        chaiExpect.fail('Not a JSX element.')
      }
    } else {
      chaiExpect.fail('src/app.js file was the wrong type.')
    }
  })
})

describe('action SET_SAFE_MODE', () => {
  it('Sets safe mode to true', () => {
    const { editor, derivedState, dispatch } = createEditorStates()
    expect(editor.safeMode).toBeFalsy()
    const action = setSafeMode(true)
    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      defaultUserState,
      workers,
      action,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
      builtInDependencies,
      emptyCollaborativeEditingSupport(),
      emptyProjectServerState(),
    )
    expect(updatedEditor.safeMode).toBeTruthy()
  })
})

describe('action SET_SAVE_ERROR', () => {
  it('Sets save error to true', () => {
    const { editor, derivedState, dispatch } = createEditorStates()
    expect(editor.saveError).toBeFalsy()
    const action = setSaveError(true)
    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      defaultUserState,
      workers,
      action,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
      builtInDependencies,
      emptyCollaborativeEditingSupport(),
      emptyProjectServerState(),
    )
    expect(updatedEditor.saveError).toBeTruthy()
  })
})

describe('action ADD_TOAST and REMOVE_TOAST', () => {
  it('ADD_TOAST pushes to existing toasts in state, REMOVE_TOAST removes the toast with the given id', () => {
    const { editor, derivedState, dispatch } = createEditorStates()

    const firstToast = notice('toast1')
    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      defaultUserState,
      workers,
      addToast(firstToast),
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
      builtInDependencies,
      emptyCollaborativeEditingSupport(),
      emptyProjectServerState(),
    )
    expect(updatedEditor.toasts).toHaveLength(1)
    expect(updatedEditor.toasts[0]).toEqual(firstToast)

    const secondToast = notice('toast2')
    const updatedEditor2 = runLocalEditorAction(
      updatedEditor,
      derivedState,
      defaultUserState,
      workers,
      addToast(secondToast),
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
      builtInDependencies,
      emptyCollaborativeEditingSupport(),
      emptyProjectServerState(),
    )
    expect(updatedEditor2.toasts).toHaveLength(2)
    expect(updatedEditor2.toasts[0]).toEqual(firstToast)
    expect(updatedEditor2.toasts[1]).toEqual(secondToast)

    const thirdToast = notice('toast3')
    const updatedEditor3 = runLocalEditorAction(
      updatedEditor2,
      derivedState,
      defaultUserState,
      workers,
      addToast(thirdToast),
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
      builtInDependencies,
      emptyCollaborativeEditingSupport(),
      emptyProjectServerState(),
    )
    expect(updatedEditor3.toasts).toHaveLength(3)
    expect(updatedEditor3.toasts[0]).toEqual(firstToast)
    expect(updatedEditor3.toasts[1]).toEqual(secondToast)
    expect(updatedEditor3.toasts[2]).toEqual(thirdToast)

    const updatedEditor4 = runLocalEditorAction(
      updatedEditor3,
      derivedState,
      defaultUserState,
      workers,
      removeToast(secondToast.id),
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
      builtInDependencies,
      emptyCollaborativeEditingSupport(),
      emptyProjectServerState(),
    )

    expect(updatedEditor4.toasts).toHaveLength(2)
    expect(updatedEditor4.toasts[0]).toEqual(firstToast)
    expect(updatedEditor4.toasts[1]).toEqual(thirdToast)
  })
})

describe('updating node_modules', () => {
  it('action UPDATE_NODE_MODULES incrementally', () => {
    const { editor, derivedState } = createEditorStates()
    const mockDispatch = jest.fn()
    editor.nodeModules = {
      skipDeepFreeze: true,
      files: {
        '/node_modules/example.js': esCodeFile(
          'nothing to see here',
          'NODE_MODULES',
          '/node_modules/example.js',
        ),
      },
      projectFilesBuildResults: {},
      packageStatus: {},
    }

    const nodeModules = createNodeModules(fileWithImports.contents)
    const action = updateNodeModulesContents(nodeModules)
    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      defaultUserState,
      workers,
      action,
      History.init(editor, derivedState),
      mockDispatch,
      emptyUiJsxCanvasContextData(),
      builtInDependencies,
      emptyCollaborativeEditingSupport(),
      emptyProjectServerState(),
    )

    expect(updatedEditor.nodeModules.files['/node_modules/example.js']).toBeDefined()
    expect(
      updatedEditor.nodeModules.files['/node_modules/mypackage/code-using-module-exports.js'],
    ).toEqual(nodeModules['/node_modules/mypackage/code-using-module-exports.js'])
  })

  it('action UPDATE_NODE_MODULES from scratch', () => {
    const { editor, derivedState } = createEditorStates()
    const mockDispatch = jest.fn()

    const nodeModules = createNodeModules(fileWithImports.contents)
    const action = updateNodeModulesContents(nodeModules)
    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      defaultUserState,
      workers,
      action,
      History.init(editor, derivedState),
      mockDispatch,
      emptyUiJsxCanvasContextData(),
      builtInDependencies,
      emptyCollaborativeEditingSupport(),
      emptyProjectServerState(),
    )

    expect(updatedEditor.nodeModules.files['/node_modules/example.js']).toBeUndefined()
    expect(updatedEditor.nodeModules.files).toEqual(nodeModules)
  })
})

describe('updating package.json', () => {
  it('action UPDATE_PACKAGE_JSON', () => {
    const { editor, derivedState } = createEditorStates()
    const mockDispatch = jest.fn()

    const deps = [
      requestedNpmDependency('mypackage', '1.0.0'),
      requestedNpmDependency('smart', '2.3.1'),
    ]
    const action = updatePackageJson(deps)
    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      defaultUserState,
      workers,
      action,
      History.init(editor, derivedState),
      mockDispatch,
      emptyUiJsxCanvasContextData(),
      builtInDependencies,
      emptyCollaborativeEditingSupport(),
      emptyProjectServerState(),
    )

    const packageJsonFile = getProjectFileByFilePath(updatedEditor.projectContents, '/package.json')
    if (packageJsonFile == null || packageJsonFile.type != 'TEXT_FILE') {
      throw new Error('Package.json file should exist and should be a TextFile')
    } else {
      expect(packageJsonFile.fileContents).toMatchInlineSnapshot(`
        Object {
          "code": "{
          \\"name\\": \\"utopia-project\\",
          \\"version\\": \\"0.1.0\\",
          \\"utopia\\": {
            \\"main-ui\\": \\"utopia/storyboard.js\\",
            \\"html\\": \\"public/index.html\\",
            \\"js\\": \\"src/index.js\\"
          },
          \\"dependencies\\": {
            \\"mypackage\\": \\"1.0.0\\",
            \\"smart\\": \\"2.3.1\\"
          }
        }",
          "parsed": Object {
            "type": "UNPARSED",
          },
          "revisionsState": "CODE_AHEAD_BUT_PLEASE_TELL_VSCODE_ABOUT_IT",
        }
      `)
    }
  })
})
