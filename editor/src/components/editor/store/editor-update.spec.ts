import * as Chai from 'chai'
import { NormalisedFrame } from 'utopia-api'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import {
  isJSXElement,
  jsxAttributesFromMap,
  jsxAttributeValue,
  jsxElement,
  jsxElementName,
} from '../../../core/shared/element-template'
import { findJSXElementChildAtPath, getUtopiaID } from '../../../core/model/element-template-utils'
import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
import {
  StaticInstancePath,
  TemplatePath,
  isTextFile,
  TextFile,
  esCodeFile,
  importDetails,
  importAlias,
  isParseSuccess,
} from '../../../core/shared/project-file-types'
import { MockUtopiaTsWorkers } from '../../../core/workers/workers'
import { isRight, right } from '../../../core/shared/either'
import { createEditorStates, createFakeMetadataForEditor } from '../../../utils/utils.test-utils'
import Utils from '../../../utils/utils'
import { renameComponent, reparentComponents } from '../../navigator/actions'
import * as TP from '../../../core/shared/template-path'
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
} from '../actions/action-creators'
import * as History from '../history'
import {
  EditorState,
  getOpenUtopiaJSXComponentsFromState,
  defaultUserState,
  StoryboardFilePath,
} from './editor-state'
import { runLocalEditorAction } from './editor-update'
import { getLayoutPropertyOr } from '../../../core/layout/getLayoutProperty'
import {
  ScenePathForTestUiJsFile,
  ScenePath1ForTestUiJsFile,
  InstancePath1ForTestUiJsFile,
} from '../../../core/model/test-ui-js-file.test-utils'
import { emptyUiJsxCanvasContextData } from '../../canvas/ui-jsx-canvas'
import { requestedNpmDependency } from '../../../core/shared/npm-dependency-types'
import { getContentsTreeFileFromString } from '../../assets'
import { forceParseSuccessFromFileOrFail } from '../../../core/workers/parser-printer/parser-printer.test-utils'
import { emptyComments } from '../../../core/workers/parser-printer/parser-printer-comments'
import { notice } from '../../common/notice'
import { testStaticInstancePath } from '../../../core/shared/template-path.test-utils'

const chaiExpect = Chai.expect

/* eslint-disable @typescript-eslint/no-empty-function */

const workers = new MockUtopiaTsWorkers()

const testScenePath = ScenePath1ForTestUiJsFile
const testTemplatePath = TP.instancePath(ScenePath1ForTestUiJsFile, ['pancake'])

jest.useFakeTimers()

describe('action SELECT_VIEWS', () => {
  it('updates selectedview in editor', () => {
    const { editor, derivedState, dispatch } = createEditorStates()
    const action = selectComponents([testTemplatePath], false)
    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      defaultUserState,
      workers,
      action,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
    )
    chaiExpect(updatedEditor.selectedViews).to.deep.equal([testTemplatePath])
  })
  it('doesnt update navigator collapse toggle when no child is selected', () => {
    const { editor, derivedState, dispatch } = createEditorStates()
    const action = toggleCollapse(testTemplatePath)
    const editorAfterToggle = runLocalEditorAction(
      editor,
      derivedState,
      defaultUserState,
      workers,
      action,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
    )
    const action2 = selectComponents([testTemplatePath], false)
    const updatedEditor = runLocalEditorAction(
      editorAfterToggle,
      derivedState,
      defaultUserState,
      workers,
      action2,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
    )
    chaiExpect(updatedEditor.selectedViews).to.deep.equal([testTemplatePath])
    chaiExpect(updatedEditor.navigator.collapsedViews).to.deep.equal([testTemplatePath])
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
    )
    chaiExpect(updatedEditor.selectedViews).to.deep.equal([testScenePath])
  })
  it('prevents multiselection if a scene is selected, taking the last scene path selected', () => {
    const { editor, derivedState, dispatch } = createEditorStates()

    const withOneScene = runLocalEditorAction(
      editor,
      derivedState,
      defaultUserState,
      workers,
      selectComponents([testScenePath, testTemplatePath], false),
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
    )
    chaiExpect(withOneScene.selectedViews).to.deep.equal([testScenePath])

    const withMultipleScenes = runLocalEditorAction(
      editor,
      derivedState,
      defaultUserState,
      workers,
      selectComponents([ScenePathForTestUiJsFile, testTemplatePath, testScenePath], false),
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
    )
    chaiExpect(withMultipleScenes.selectedViews).to.deep.equal([testScenePath])
  })
})

describe('action CLEAR_SELECTION', () => {
  it('clears selectedview in editor', () => {
    const { editor, derivedState, dispatch } = createEditorStates()
    const action = selectComponents([testTemplatePath], false)
    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      defaultUserState,
      workers,
      action,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
    )
    chaiExpect(updatedEditor.selectedViews).to.deep.equal([testTemplatePath])

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
    )
    chaiExpect(updatedEditor2.selectedViews).to.deep.equal([])
  })
})

describe('action RENAME_COMPONENT', () => {
  function checkRename(target: TemplatePath, expectedDefaultName: string): void {
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
    )
    const updatedMetadata = createFakeMetadataForEditor(updatedEditor)
    chaiExpect(MetadataUtils.getElementLabel(target, updatedMetadata)).to.deep.equal(newName)

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
    )
    const clearedNameMetadata = createFakeMetadataForEditor(clearedNameEditor)
    chaiExpect(MetadataUtils.getElementLabel(target, clearedNameMetadata)).to.deep.equal(
      expectedDefaultName,
    )
  }

  it('renames an existing scene', () => checkRename(ScenePathForTestUiJsFile, 'Test'))
  it('renames an existing element', () =>
    checkRename(TP.instancePath(ScenePathForTestUiJsFile, ['aaa']), 'View'))
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
    )
    chaiExpect(updatedEditor2.preview.visible).to.not.equal(updatedEditor.preview.visible)
  })
})

describe('action NAVIGATOR_REORDER', () => {
  xit('reparents one element, which was a scene before set it to child of target and removes from scene', () => {
    // TODO Scene Implementation
    const { editor, derivedState, dispatch } = createEditorStates()
    const reparentAction = reparentComponents(
      [TP.instancePath(ScenePath1ForTestUiJsFile, ['jjj'])],
      TP.instancePath(ScenePathForTestUiJsFile, ['aaa']),
    )
    const mainUIJSFile = getContentsTreeFileFromString(editor.projectContents, StoryboardFilePath)
    if (isTextFile(mainUIJSFile) && isParseSuccess(mainUIJSFile.fileContents.parsed)) {
      const topLevelElements = mainUIJSFile.fileContents.parsed.topLevelElements
      const utopiaJSXComponents = getUtopiaJSXComponentsFromSuccess(
        mainUIJSFile.fileContents.parsed,
      )
      const element = utopiaJSXComponents.find((comp) => getUtopiaID(comp.rootElement) === 'aaa')
      if (element != null) {
        const targetChildren = Utils.pathOr([], ['rootElement', 'children'], element)
        const updatedEditor = runLocalEditorAction(
          editor,
          derivedState,
          defaultUserState,
          workers,
          reparentAction,
          History.init(editor, derivedState),
          dispatch,
          emptyUiJsxCanvasContextData(),
        )

        const updatedMainUIJSFile = getContentsTreeFileFromString(
          updatedEditor.projectContents,
          StoryboardFilePath,
        )
        if (
          isTextFile(updatedMainUIJSFile) &&
          isParseSuccess(updatedMainUIJSFile.fileContents.parsed)
        ) {
          const updatedTopLevelElements = updatedMainUIJSFile.fileContents.parsed.topLevelElements
          const updatedUtopiaJSXComponents = getUtopiaJSXComponentsFromSuccess(
            updatedMainUIJSFile.fileContents.parsed,
          )
          const updatedElement = updatedUtopiaJSXComponents.find(
            (comp) => getUtopiaID(comp.rootElement) === 'aaa',
          )
          if (updatedElement != null) {
            const updatedTargetChildren = Utils.pathOr(
              [],
              ['rootElement', 'children'],
              updatedElement,
            )

            // check the number of children after reparenting
            expect(updatedTopLevelElements).toHaveLength(topLevelElements.length - 1)
            expect(updatedTargetChildren).toHaveLength(targetChildren.length + 1)

            // check if the reparented elements are really in their new parent
            expect(
              updatedTargetChildren.find((child) => getUtopiaID(child) === 'jjj'),
            ).toBeDefined()
          } else {
            chaiExpect.fail('couldn’t find element after updating.')
          }
        } else {
          chaiExpect.fail('updated src/app.js file was the wrong type.')
        }
      } else {
        chaiExpect.fail('couldn’t find element.')
      }
    } else {
      chaiExpect.fail('original src/app.js file was the wrong type.')
    }
  })
})

describe('action DUPLICATE_SPECIFIC_ELEMENTS', () => {
  it('duplicates 1 element', () => {
    const { editor, derivedState, dispatch } = createEditorStates([
      TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'iii']),
    ])
    const duplicateAction = duplicateSpecificElements([
      TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'iii']),
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
    )
    const mainUIJSFile = getContentsTreeFileFromString(
      updatedEditor.projectContents,
      StoryboardFilePath,
    )
    const oldUIJSFile = getContentsTreeFileFromString(editor.projectContents, StoryboardFilePath)
    if (
      isTextFile(oldUIJSFile) &&
      isParseSuccess(oldUIJSFile.fileContents.parsed) &&
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
    const element1 = TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'iii'])
    const element2 = TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'ddd'])
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
    )
    const mainUIJSFile = getContentsTreeFileFromString(
      updatedEditor.projectContents,
      StoryboardFilePath,
    )
    const oldUIJSFile = getContentsTreeFileFromString(editor.projectContents, StoryboardFilePath)
    if (
      isTextFile(oldUIJSFile) &&
      isParseSuccess(oldUIJSFile.fileContents.parsed) &&
      isTextFile(mainUIJSFile) &&
      isParseSuccess(mainUIJSFile.fileContents.parsed)
    ) {
      const updatedComponents = getOpenUtopiaJSXComponentsFromState(updatedEditor)
      const updatedChildren = Utils.pathOr([], [0, 'rootElement', 'children'], updatedComponents)
      const originalComponents = getOpenUtopiaJSXComponentsFromState(editor)
      const originalChildren = Utils.pathOr([], [0, 'rootElement', 'children'], originalComponents)
      expect(updatedChildren).toHaveLength(originalChildren.length + 2)
      expect(updatedEditor.selectedViews).toHaveLength(2)
      expect(updatedEditor.selectedViews.find((view) => view === element1)).toBe(undefined)
      expect(updatedEditor.selectedViews.find((view) => view === element2)).toBe(undefined)
      const newElements = Utils.stripNulls(
        updatedEditor.selectedViews.map((view) => {
          return findJSXElementChildAtPath(updatedComponents, view as StaticInstancePath)
        }),
      )
      const newElementsInOriginalModel = Utils.stripNulls(
        updatedEditor.selectedViews.map((view) => {
          return findJSXElementChildAtPath(originalComponents, view as StaticInstancePath)
        }),
      )
      expect(newElements).toHaveLength(2)
      expect(newElementsInOriginalModel).toHaveLength(0)
    } else {
      chaiExpect.fail('src/app.js file was the wrong type.')
    }
  })
})

describe('action DELETE_SELECTED', () => {
  it('deletes all selected elements', () => {
    const firstTargetElementPath = testStaticInstancePath(ScenePathForTestUiJsFile, ['aaa', 'bbb'])
    const secondTargetElementPath = testStaticInstancePath(ScenePathForTestUiJsFile, ['aaa', 'iii'])
    const targetScenePath = InstancePath1ForTestUiJsFile

    const { editor, derivedState, dispatch } = createEditorStates([
      firstTargetElementPath,
      secondTargetElementPath,
      targetScenePath,
    ])

    const parseSuccess = forceParseSuccessFromFileOrFail(
      getContentsTreeFileFromString(editor.projectContents, StoryboardFilePath),
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
    )
    const mainUIJSFile = getContentsTreeFileFromString(
      updatedEditor.projectContents,
      StoryboardFilePath,
    )
    if (isTextFile(mainUIJSFile) && isParseSuccess(mainUIJSFile.fileContents.parsed)) {
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
    } else {
      chaiExpect.fail('src/app.js file was the wrong type.')
    }
  })
})

describe('INSERT_JSX_ELEMENT', () => {
  function testInsertionToParent(parentPath: StaticInstancePath) {
    const { editor, derivedState, dispatch } = createEditorStates()

    const parentBeforeInsert = findJSXElementChildAtPath(
      getOpenUtopiaJSXComponentsFromState(editor),
      parentPath,
    )

    const elementToInsert = jsxElement(
      jsxElementName('View', []),
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('TestView', emptyComments) }),
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
    )
    const updatedComponents = getOpenUtopiaJSXComponentsFromState(updatedEditor)
    const parentAfterInsert = findJSXElementChildAtPath(updatedComponents, parentPath)
    const insertedElement = findJSXElementChildAtPath(
      updatedComponents,
      TP.appendToPath(parentPath, 'TestView'),
    )
    if (parentAfterInsert != null && parentBeforeInsert != null) {
      expect(Utils.pathOr(0, ['children', 'length'], parentAfterInsert)).toEqual(
        Utils.pathOr(0, ['children', 'length'], parentBeforeInsert) + 1,
      )
    }
    expect(insertedElement).toBeDefined()
  }

  it('inserts an element', () => {
    testInsertionToParent(testStaticInstancePath(ScenePathForTestUiJsFile, ['aaa']))
    testInsertionToParent(testStaticInstancePath(ScenePathForTestUiJsFile, ['aaa', 'bbb']))
    testInsertionToParent(testStaticInstancePath(ScenePathForTestUiJsFile, ['aaa', 'ddd', 'eee']))
  })

  it('fails to insert to nonexistent parent', () => {
    expect(() => {
      testInsertionToParent(
        testStaticInstancePath(ScenePathForTestUiJsFile, ['aaa', 'i-dont-exist']),
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

    const componentsBeforeInsert = getOpenUtopiaJSXComponentsFromState(editorWithNoHighlighted)

    const elementToInsert = jsxElement(
      jsxElementName('View', []),
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('TestView', emptyComments) }),
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
    )
    const updatedComponents = getOpenUtopiaJSXComponentsFromState(updatedEditor)
    const insertedElement = findJSXElementChildAtPath(
      updatedComponents,
      testStaticInstancePath(ScenePathForTestUiJsFile, ['TestView']),
    )
    expect(updatedComponents.length).toEqual(componentsBeforeInsert.length + 1)
    expect(insertedElement).toBeDefined()
  })
})

describe('action MOVE_SELECTED_BACKWARD', () => {
  it('moves the element backward', () => {
    const { editor, derivedState, dispatch } = createEditorStates()
    const editorWithSelectedView = {
      ...editor,
      selectedViews: [TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'ddd'])],
    }
    const reparentAction = moveSelectedBackward()
    const updatedEditor = runLocalEditorAction(
      editorWithSelectedView,
      derivedState,
      defaultUserState,
      workers,
      reparentAction,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
    )
    const updatedMetadata = createFakeMetadataForEditor(updatedEditor)

    const updatedZIndex = MetadataUtils.getViewZIndexFromMetadata(
      updatedMetadata,
      TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'ddd']),
    )
    const oldZIndex = MetadataUtils.getViewZIndexFromMetadata(
      editor.jsxMetadata,
      TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'ddd']),
    )
    expect(updatedZIndex).toBe(oldZIndex - 2)
  })
})

describe('action UPDATE_FRAME_DIMENSIONS', () => {
  it('updates text element frame dimension', () => {
    const { editor, derivedState, dispatch } = createEditorStates()
    const targetText = testStaticInstancePath(ScenePathForTestUiJsFile, ['aaa', 'hhh'])
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
    )
    const mainUIJSFile = getContentsTreeFileFromString(
      updatedEditor.projectContents,
      StoryboardFilePath,
    )
    if (isTextFile(mainUIJSFile) && isParseSuccess(mainUIJSFile.fileContents.parsed)) {
      const components = getUtopiaJSXComponentsFromSuccess(mainUIJSFile.fileContents.parsed)
      const textElement = Utils.forceNotNull(
        'Target text should exist',
        findJSXElementChildAtPath(components, targetText),
      )
      if (isJSXElement(textElement)) {
        expect(getLayoutPropertyOr(undefined, 'Width', right(textElement.props))).toEqual(newWidth)
        expect(getLayoutPropertyOr(undefined, 'Height', right(textElement.props))).toEqual(
          newHeight,
        )
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
    )

    expect(updatedEditor4.toasts).toHaveLength(2)
    expect(updatedEditor4.toasts[0]).toEqual(firstToast)
    expect(updatedEditor4.toasts[1]).toEqual(thirdToast)
  })

  it('ADD_TOAST schedules a REMOVE_TOAST', () => {
    const { editor, derivedState } = createEditorStates()
    const mockDispatch = jest.fn()

    const toast = notice('toast1')
    runLocalEditorAction(
      editor,
      derivedState,
      defaultUserState,
      workers,
      addToast(toast),
      History.init(editor, derivedState),
      mockDispatch,
      emptyUiJsxCanvasContextData(),
    )

    jest.runAllTimers()

    expect(mockDispatch).toBeCalledTimes(1)
    expect(mockDispatch).toBeCalledWith([removeToast(toast.id)], 'everyone')
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
    const action = updateNodeModulesContents(nodeModules, 'incremental')
    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      defaultUserState,
      workers,
      action,
      History.init(editor, derivedState),
      mockDispatch,
      emptyUiJsxCanvasContextData(),
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
    const action = updateNodeModulesContents(nodeModules, 'full-build')
    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      defaultUserState,
      workers,
      action,
      History.init(editor, derivedState),
      mockDispatch,
      emptyUiJsxCanvasContextData(),
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
    )

    const packageJsonFile = getContentsTreeFileFromString(
      updatedEditor.projectContents,
      '/package.json',
    )
    if (packageJsonFile == null || packageJsonFile.type != 'TEXT_FILE') {
      fail('Package.json file should exist and should be a TextFile')
    } else {
      expect(packageJsonFile.fileContents).toMatchInlineSnapshot(`
        Object {
          "code": "{
          \\"name\\": \\"Utopia Project\\",
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
          "revisionsState": "CODE_AHEAD",
        }
      `)
    }
  })
})
