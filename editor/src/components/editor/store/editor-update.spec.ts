import * as Chai from 'chai'
import { NormalisedFrame } from 'utopia-api'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import {
  isJSXElement,
  jsxAttributeValue,
  jsxElement,
  jsxElementName,
} from '../../../core/shared/element-template'
import { findJSXElementChildAtPath, getUtopiaID } from '../../../core/model/element-template-utils'
import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
import {
  StaticInstancePath,
  TemplatePath,
  isUIJSFile,
  CodeFile,
  esCodeFile,
  importDetails,
  importAlias,
} from '../../../core/shared/project-file-types'
import { MockUtopiaTsWorkers } from '../../../core/workers/workers'
import { isRight, right } from '../../../core/shared/either'
import { createEditorStates, createFakeMetadataForEditor } from '../../../utils/test-utils'
import Utils from '../../../utils/utils'
import { renameComponent, reparentComponents } from '../../navigator/actions'
import * as TP from '../../../core/shared/template-path'
import * as fileWithImports from '../../../core/es-modules/test-cases/file-with-imports.json'
import * as fileNoImports from '../../../core/es-modules/test-cases/file-no-imports.json'
import { createNodeModules } from '../../../core/es-modules/package-manager/test-utils'
import { notLoggedIn } from '../action-types'
import {
  clearSelection,
  deleteViews,
  duplicateSelected,
  duplicateSpecificElements,
  insertJSXElement,
  moveSelectedBackward,
  openEditorTab,
  selectComponents,
  toggleCollapse,
  togglePanel,
  updateFrameDimensions,
  setSafeMode,
  setSaveError,
  pushToast,
  popToast,
  updateNodeModulesContents,
  updatePackageJson,
} from '../actions/actions'
import * as History from '../history'
import { EditorState, getOpenUtopiaJSXComponentsFromState, openFileTab } from './editor-state'
import { runLocalEditorAction } from './editor-update'
import { getLayoutPropertyOr } from '../../../core/layout/getLayoutProperty'
import {
  ScenePathForTestUiJsFile,
  ScenePath1ForTestUiJsFile,
} from '../../../core/model/test-ui-js-file'
import { npmDependency } from '../../../core/shared/npm-dependency-types'
import {emptyUiJsxCanvasContextData} from '../../canvas/ui-jsx-canvas'
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
      notLoggedIn,
      workers,
      action,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData()
    )
    chaiExpect(updatedEditor.selectedViews).to.deep.equal([testTemplatePath])
  })
  it('doesnt update navigator collapse toggle when no child is selected', () => {
    const { editor, derivedState, dispatch } = createEditorStates()
    const action = toggleCollapse(testTemplatePath)
    const editorAfterToggle = runLocalEditorAction(
      editor,
      derivedState,
      notLoggedIn,
      workers,
      action,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData()
    )
    const action2 = selectComponents([testTemplatePath], false)
    const updatedEditor = runLocalEditorAction(
      editorAfterToggle,
      derivedState,
      notLoggedIn,
      workers,
      action2,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData()
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
      notLoggedIn,
      workers,
      action,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData()
    )
    chaiExpect(updatedEditor.selectedViews).to.deep.equal([testScenePath])
  })
  it('prevents multiselection if a scene is selected, taking the last scene path selected', () => {
    const { editor, derivedState, dispatch } = createEditorStates()

    const withOneScene = runLocalEditorAction(
      editor,
      derivedState,
      notLoggedIn,
      workers,
      selectComponents([testScenePath, testTemplatePath], false),
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData()
    )
    chaiExpect(withOneScene.selectedViews).to.deep.equal([testScenePath])

    const withMultipleScenes = runLocalEditorAction(
      editor,
      derivedState,
      notLoggedIn,
      workers,
      selectComponents([ScenePathForTestUiJsFile, testTemplatePath, testScenePath], false),
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData()
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
      notLoggedIn,
      workers,
      action,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData()
    )
    chaiExpect(updatedEditor.selectedViews).to.deep.equal([testTemplatePath])

    const clearAction = clearSelection()
    const updatedEditor2 = runLocalEditorAction(
      updatedEditor,
      derivedState,
      notLoggedIn,
      workers,
      clearAction,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData()
    )
    chaiExpect(updatedEditor2.selectedViews).to.deep.equal([])
  })
})

describe('action RENAME_COMPONENT', () => {
  function checkRename(target: TemplatePath, expectedDefaultName: string): void {
    const { editor, derivedState, dispatch } = createEditorStates('/src/app.js')
    const newName = 'newName'
    const renameAction = renameComponent(target, newName)
    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      notLoggedIn,
      workers,
      renameAction,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData()
    )
    const updatedMetadata = createFakeMetadataForEditor(updatedEditor)
    chaiExpect(MetadataUtils.getElementLabel(target, updatedMetadata)).to.deep.equal(newName)

    const clearNameAction = renameComponent(target, null)
    const clearedNameEditor = runLocalEditorAction(
      updatedEditor,
      derivedState,
      notLoggedIn,
      workers,
      clearNameAction,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData()
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
      notLoggedIn,
      workers,
      action,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData()
    )
    const updatedEditor2 = runLocalEditorAction(
      updatedEditor,
      derivedState,
      notLoggedIn,
      workers,
      action,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData()
    )
    chaiExpect(updatedEditor2.inspector.visible).to.not.equal(updatedEditor.inspector.visible)
  })

  it('can toggle preview visibility', () => {
    const { editor, derivedState, dispatch } = createEditorStates()
    const action = togglePanel('preview')
    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      notLoggedIn,
      workers,
      action,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData()
    )
    const updatedEditor2 = runLocalEditorAction(
      updatedEditor,
      derivedState,
      notLoggedIn,
      workers,
      action,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData()
    )
    chaiExpect(updatedEditor2.preview.visible).to.not.equal(updatedEditor.preview.visible)
  })
})

describe('action NAVIGATOR_REORDER', () => {
  xit('reparents one element, which was a scene before set it to child of target and removes from scene', () => {
    // TODO Scene Implementation
    const { editor, derivedState, dispatch } = createEditorStates('/src/app.js')
    const reparentAction = reparentComponents(
      [TP.instancePath(ScenePath1ForTestUiJsFile, ['jjj'])],
      TP.instancePath(ScenePathForTestUiJsFile, ['aaa']),
    )
    const mainUIJSFile = editor.projectContents['/src/app.js']
    if (isUIJSFile(mainUIJSFile) && isRight(mainUIJSFile.fileContents)) {
      const topLevelElements = mainUIJSFile.fileContents.value.topLevelElements
      const utopiaJSXComponents = getUtopiaJSXComponentsFromSuccess(mainUIJSFile.fileContents.value)
      const targetChildren = Utils.pathOr(
        [],
        ['rootElement', 'children'],
        utopiaJSXComponents.find((comp) => getUtopiaID(comp.rootElement) === 'aaa'),
      )

      const updatedEditor = runLocalEditorAction(
        editor,
        derivedState,
        notLoggedIn,
        workers,
        reparentAction,
        History.init(editor, derivedState),
        dispatch,
        emptyUiJsxCanvasContextData()
      )

      const updatedMainUIJSFile = updatedEditor.projectContents['/src/app.js']
      if (isUIJSFile(updatedMainUIJSFile) && isRight(updatedMainUIJSFile.fileContents)) {
        const updatedTopLevelElements = updatedMainUIJSFile.fileContents.value.topLevelElements
        const updatedUtopiaJSXComponents = getUtopiaJSXComponentsFromSuccess(
          updatedMainUIJSFile.fileContents.value,
        )
        const updatedTargetChildren = Utils.pathOr(
          [],
          ['rootElement', 'children'],
          updatedUtopiaJSXComponents.find((comp) => getUtopiaID(comp.rootElement) === 'aaa'),
        )

        // check the number of children after reparenting
        expect(updatedTopLevelElements).toHaveLength(topLevelElements.length - 1)
        expect(updatedTargetChildren).toHaveLength(targetChildren.length + 1)

        // check if the reparented elements are really in their new parent
        expect(updatedTargetChildren.find((child) => getUtopiaID(child) === 'jjj')).toBeDefined()
      } else {
        chaiExpect.fail('updated src/app.js file was the wrong type.')
      }
    } else {
      chaiExpect.fail('original src/app.js file was the wrong type.')
    }
  })
})

describe('action DUPLICATE_SPECIFIC_ELEMENTS', () => {
  it('duplicates 1 element', () => {
    const { editor, derivedState, dispatch } = createEditorStates('/src/app.js', [
      TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'iii']),
    ])
    const duplicateAction = duplicateSpecificElements([
      TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'iii']),
    ])
    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      notLoggedIn,
      workers,
      duplicateAction,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData()
    )
    const mainUIJSFile = updatedEditor.projectContents['/src/app.js']
    const oldUIJSFile = editor.projectContents['/src/app.js']
    if (
      isUIJSFile(oldUIJSFile) &&
      isRight(oldUIJSFile.fileContents) &&
      isUIJSFile(mainUIJSFile) &&
      isRight(mainUIJSFile.fileContents)
    ) {
      const updatedChildren = Utils.pathOr(
        [],
        [0, 'rootElement', 'children'],
        getUtopiaJSXComponentsFromSuccess(mainUIJSFile.fileContents.value),
      )
      const oldChildren = Utils.pathOr(
        [],
        [0, 'rootElement', 'children'],
        getUtopiaJSXComponentsFromSuccess(oldUIJSFile.fileContents.value),
      )
      expect(updatedChildren).toHaveLength(oldChildren.length + 1)
    } else {
      chaiExpect.fail('src/app.js file was the wrong type.')
    }
  })
  it('duplicates multiple elements', () => {
    const element1 = TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'iii'])
    const element2 = TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'ddd'])
    const { editor, derivedState, dispatch } = createEditorStates('/src/app.js', [
      element1,
      element2,
    ])
    const duplicateAction = duplicateSelected()
    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      notLoggedIn,
      workers,
      duplicateAction,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData()
    )
    const mainUIJSFile = updatedEditor.projectContents['/src/app.js']
    const oldUIJSFile = editor.projectContents['/src/app.js']
    if (
      isUIJSFile(oldUIJSFile) &&
      isRight(oldUIJSFile.fileContents) &&
      isUIJSFile(mainUIJSFile) &&
      isRight(mainUIJSFile.fileContents)
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

describe('action DELETE_VIEWS', () => {
  it('deletes all target elements', () => {
    const { editor, derivedState, dispatch } = createEditorStates('/src/app.js')

    const originalChildrenCount = Utils.pathOr(
      [],
      [0, 'rootElement', 'children'],
      getUtopiaJSXComponentsFromSuccess(
        (editor.projectContents['/src/app.js'] as any).fileContents.value,
      ),
    ).length

    const firstTargetElementPath = TP.staticInstancePath(ScenePathForTestUiJsFile, ['aaa', 'bbb'])
    const secondTargetElementPath = TP.staticInstancePath(ScenePathForTestUiJsFile, ['aaa', 'iii'])
    const targetScenePath = ScenePath1ForTestUiJsFile
    const deleteAction = deleteViews([
      firstTargetElementPath,
      secondTargetElementPath,
      targetScenePath,
    ])
    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      notLoggedIn,
      workers,
      deleteAction,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData()
    )
    const mainUIJSFile = updatedEditor.projectContents['/src/app.js']
    if (isUIJSFile(mainUIJSFile) && isRight(mainUIJSFile.fileContents)) {
      expect(
        Utils.pathOr(
          [],
          [0, 'rootElement', 'children'],
          getUtopiaJSXComponentsFromSuccess(mainUIJSFile.fileContents.value),
        ),
      ).toHaveLength(originalChildrenCount - 2)
      expect(
        findJSXElementChildAtPath(
          getUtopiaJSXComponentsFromSuccess(mainUIJSFile.fileContents.value),
          firstTargetElementPath,
        ),
      ).toBeNull()
      expect(
        findJSXElementChildAtPath(
          getUtopiaJSXComponentsFromSuccess(mainUIJSFile.fileContents.value),
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
    const { editor, derivedState, dispatch } = createEditorStates('/src/app.js')

    const parentBeforeInsert = findJSXElementChildAtPath(
      getOpenUtopiaJSXComponentsFromState(editor),
      parentPath,
    )

    const elementToInsert = jsxElement(
      jsxElementName('View', []),
      { 'data-uid': jsxAttributeValue('TestView') },
      [],
      null,
    )
    const insertAction = insertJSXElement(elementToInsert, parentPath, {
      'utopia-api': importDetails(null, [importAlias('View')], null),
    })
    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      notLoggedIn,
      workers,
      insertAction,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData()
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
    testInsertionToParent(TP.staticInstancePath(ScenePathForTestUiJsFile, ['aaa']))
    testInsertionToParent(TP.staticInstancePath(ScenePathForTestUiJsFile, ['aaa', 'bbb']))
    testInsertionToParent(TP.staticInstancePath(ScenePathForTestUiJsFile, ['aaa', 'ddd', 'eee']))
  })

  it('fails to insert to nonexistent parent', () => {
    expect(() => {
      testInsertionToParent(
        TP.staticInstancePath(ScenePathForTestUiJsFile, ['aaa', 'i-dont-exist']),
      )
    }).toThrow()
  })

  xit('inserts an element as a new root component', () => {
    // TODO Scene Implementation
    const { editor, derivedState, dispatch } = createEditorStates('/src/app.js')
    const editorWithNoHighlighted: EditorState = {
      ...editor,
      highlightedViews: [],
    }

    const componentsBeforeInsert = getOpenUtopiaJSXComponentsFromState(editorWithNoHighlighted)

    const elementToInsert = jsxElement(
      jsxElementName('View', []),
      { 'data-uid': jsxAttributeValue('TestView') },
      [],
      null,
    )
    const insertAction = insertJSXElement(elementToInsert, null, {
      'utopia-api': importDetails(null, [importAlias('View')], null),
    })
    const updatedEditor = runLocalEditorAction(
      editorWithNoHighlighted,
      derivedState,
      notLoggedIn,
      workers,
      insertAction,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData()
    )
    const updatedComponents = getOpenUtopiaJSXComponentsFromState(updatedEditor)
    const insertedElement = findJSXElementChildAtPath(
      updatedComponents,
      TP.staticInstancePath(ScenePathForTestUiJsFile, ['TestView']),
    )
    expect(updatedComponents.length).toEqual(componentsBeforeInsert.length + 1)
    expect(insertedElement).toBeDefined()
  })
})

describe('action MOVE_SELECTED_BACKWARD', () => {
  it('moves the element backward', () => {
    const { editor, derivedState, dispatch } = createEditorStates('/src/app.js')
    const editorWithSelectedView = {
      ...editor,
      selectedViews: [TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'ddd'])],
    }
    const reparentAction = moveSelectedBackward()
    const updatedEditor = runLocalEditorAction(
      editorWithSelectedView,
      derivedState,
      notLoggedIn,
      workers,
      reparentAction,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData()
    )
    const updatedMetadata = createFakeMetadataForEditor(updatedEditor)

    const updatedZIndex = MetadataUtils.getViewZIndexFromMetadata(
      updatedMetadata,
      TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'ddd']),
    )
    const oldZIndex = MetadataUtils.getViewZIndexFromMetadata(
      editor.jsxMetadataKILLME,
      TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'ddd']),
    )
    expect(updatedZIndex).toBe(oldZIndex - 1)
  })
})

describe('action UPDATE_FRAME_DIMENSIONS', () => {
  it('updates text element frame dimension', () => {
    const { editor, derivedState, dispatch } = createEditorStates('/src/app.js')
    const targetText = TP.staticInstancePath(ScenePathForTestUiJsFile, ['aaa', 'hhh'])
    const newWidth = 300
    const newHeight = 400
    const updateFrameDimensionsAction = updateFrameDimensions(targetText, newWidth, newHeight)
    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      notLoggedIn,
      workers,
      updateFrameDimensionsAction,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData()
    )
    const mainUIJSFile = updatedEditor.projectContents['/src/app.js']
    if (isUIJSFile(mainUIJSFile) && isRight(mainUIJSFile.fileContents)) {
      const components = getUtopiaJSXComponentsFromSuccess(mainUIJSFile.fileContents.value)
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

describe('action OPEN_FILE', () => {
  it('opens a new file with initial cursor position', () => {
    const { editor, derivedState, dispatch } = createEditorStates('/src/app.js')
    const cursorPosition = {
      line: 15,
      column: 11,
    }
    const action = openEditorTab(openFileTab('/src/components.js'), cursorPosition)
    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      notLoggedIn,
      workers,
      action,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData()
    )
    const selectedFile = updatedEditor.selectedFile
    if (selectedFile == null) {
      fail('Unable to find the scene that should have been updated')
    } else {
      expect(selectedFile.tab).toEqual(openFileTab('/src/components.js'))
      expect(selectedFile.initialCursorPosition).toEqual(cursorPosition)
    }
  })
})

describe('action SET_SAFE_MODE', () => {
  it('Sets safe mode to true', () => {
    const { editor, derivedState, dispatch } = createEditorStates('/src/app.js')
    expect(editor.safeMode).toBeFalsy()
    const action = setSafeMode(true)
    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      notLoggedIn,
      workers,
      action,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData()
    )
    expect(updatedEditor.safeMode).toBeTruthy()
  })
})

describe('action SET_SAVE_ERROR', () => {
  it('Sets save error to true', () => {
    const { editor, derivedState, dispatch } = createEditorStates('/src/app.js')
    expect(editor.saveError).toBeFalsy()
    const action = setSaveError(true)
    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      notLoggedIn,
      workers,
      action,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData()
    )
    expect(updatedEditor.saveError).toBeTruthy()
  })
})

describe('action PUSH_TOAST and POP_TOAST', () => {
  it('PUSH_TOAST pushes to existing toasts in state, POP_TOAST removes oldest one', () => {
    const { editor, derivedState, dispatch } = createEditorStates('/src/app.js')

    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      notLoggedIn,
      workers,
      pushToast({ message: 'toast1' }),
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData()
    )
    expect(updatedEditor.toasts).toHaveLength(1)
    expect(updatedEditor.toasts[0]).toEqual({ message: 'toast1' })

    const updatedEditor2 = runLocalEditorAction(
      updatedEditor,
      derivedState,
      notLoggedIn,
      workers,
      pushToast({ message: 'toast2' }),
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData()
    )
    expect(updatedEditor2.toasts).toHaveLength(2)
    expect(updatedEditor2.toasts[0]).toEqual({ message: 'toast1' })
    expect(updatedEditor2.toasts[1]).toEqual({ message: 'toast2' })

    const updatedEditor3 = runLocalEditorAction(
      updatedEditor2,
      derivedState,
      notLoggedIn,
      workers,
      popToast(),
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData()
    )

    expect(updatedEditor3.toasts).toHaveLength(1)
    expect(updatedEditor3.toasts[0]).toEqual({ message: 'toast2' })
  })

  it('PUSH_TOAST schedules a POP_TOAST', () => {
    const { editor, derivedState } = createEditorStates('/src/app.js')
    const mockDispatch = jest.fn()

    runLocalEditorAction(
      editor,
      derivedState,
      notLoggedIn,
      workers,
      pushToast({ message: 'toast1' }),
      History.init(editor, derivedState),
      mockDispatch,
      emptyUiJsxCanvasContextData()
    )

    jest.runAllTimers()

    expect(mockDispatch).toBeCalledTimes(1)
    expect(mockDispatch).toBeCalledWith([{ action: 'POP_TOAST' }], 'everyone')
  })

  it('action UPDATE_NODE_MODULES incrementally', () => {
    const { editor, derivedState } = createEditorStates('/src/app.ui.js')
    const mockDispatch = jest.fn()
    editor.nodeModules = {
      skipDeepFreeze: true,
      files: {
        '/node_modules/example.js': esCodeFile('nothing to see here', null),
      },
    }

    const nodeModules = createNodeModules(fileWithImports.contents)
    const action = updateNodeModulesContents(nodeModules, false)
    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      notLoggedIn,
      workers,
      action,
      History.init(editor, derivedState),
      mockDispatch,
      emptyUiJsxCanvasContextData()
    )

    expect(updatedEditor.nodeModules.files['/node_modules/example.js']).toBeDefined()
    expect(
      updatedEditor.nodeModules.files['/node_modules/mypackage/code-using-module-exports.js'],
    ).toEqual(nodeModules['/node_modules/mypackage/code-using-module-exports.js'])
  })

  it('action UPDATE_NODE_MODULES from scratch', () => {
    const { editor, derivedState } = createEditorStates('/src/app.ui.js')
    const mockDispatch = jest.fn()

    const nodeModules = createNodeModules(fileWithImports.contents)
    const action = updateNodeModulesContents(nodeModules, true)
    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      notLoggedIn,
      workers,
      action,
      History.init(editor, derivedState),
      mockDispatch,
      emptyUiJsxCanvasContextData()
    )

    expect(updatedEditor.nodeModules.files['/node_modules/example.js']).toBeUndefined()
    expect(updatedEditor.nodeModules.files).toEqual(nodeModules)
  })

  it('action UPDATE_PACKAGE_JSON', () => {
    const { editor, derivedState } = createEditorStates('/src/app.ui.js')
    const mockDispatch = jest.fn()

    const deps = [npmDependency('mypackage', '1.0.0'), npmDependency('smart', '2.3.1')]
    const action = updatePackageJson(deps)
    const updatedEditor = runLocalEditorAction(
      editor,
      derivedState,
      notLoggedIn,
      workers,
      action,
      History.init(editor, derivedState),
      mockDispatch,
      emptyUiJsxCanvasContextData()
    )

    const packageJsonFile = updatedEditor.projectContents['/package.json']
    if (packageJsonFile == null || packageJsonFile.type != 'CODE_FILE') {
      fail('Package.json file should exist and should be a CodeFile')
    } else {
      expect(packageJsonFile.fileContents).toMatchInlineSnapshot(`
        "{
          \\"name\\": \\"Utopia Project\\",
          \\"version\\": \\"0.1.0\\",
          \\"utopia\\": {
            \\"main-ui\\": \\"src/app.js\\",
            \\"html\\": \\"public/index.html\\",
            \\"js\\": \\"src/index.js\\"
          },
          \\"dependencies\\": {
            \\"mypackage\\": \\"1.0.0\\",
            \\"smart\\": \\"2.3.1\\"
          }
        }"
      `)
    }
  })
})
