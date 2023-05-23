import * as Chai from 'chai'
import { FramePin, LayoutSystem } from 'utopia-api/core'
import {
  isUtopiaJSXComponent,
  jsxAttributeNestedObjectSimple,
  JSXAttributes,
  jsExpressionValue,
  jsxElement,
  JSXElement,
  jsxElementName,
  TopLevelElement,
  utopiaJSXComponent,
  UtopiaJSXComponent,
  defaultPropsParam,
  emptySpecialSizeMeasurements,
  clearTopLevelElementUniqueIDs,
  emptyComputedStyle,
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  jsxAttributesFromMap,
  emptyAttributeMetadatada,
  jsExpressionOtherJavaScript,
  JSXElementChild,
  partOfJsxAttributeValue,
  jsxElementWithoutUID,
  jsxAttributesEntry,
  elementInstanceMetadata,
  emptyComments,
  SpecialSizeMeasurements,
  unparsedCode,
  clearExpressionUniqueIDs,
} from '../../../core/shared/element-template'
import {
  clearModifiableAttributeUniqueIDs,
  getModifiableJSXAttributeAtPath,
} from '../../../core/shared/jsx-attributes'
import {
  ParseSuccess,
  RevisionsState,
  TextFile,
  isParseSuccess,
  isTextFile,
  textFileContents,
  textFile,
  TextFileContents,
  unparsed,
  EmptyExportsDetail,
  importAlias,
  exportVariable,
  exportVariables,
  exportFunction,
  parseSuccess,
  isUnparsed,
  ParsedTextFile,
} from '../../../core/shared/project-file-types'
import { addImport, emptyImports } from '../../../core/workers/common/project-file-utils'
import { deepFreeze } from '../../../utils/deep-freeze'
import { right, forceRight, left, isRight, mapEither } from '../../../core/shared/either'
import {
  createFakeMetadataForComponents,
  createFakeMetadataForEditor,
} from '../../../utils/utils.test-utils'
import Utils from '../../../utils/utils'
import {
  canvasRectangle,
  CanvasRectangle,
  LocalRectangle,
  localRectangle,
  zeroRectangle,
} from '../../../core/shared/math-utils'
import { getFrameChange } from '../../canvas/canvas-utils'
import * as PP from '../../../core/shared/property-path'
import * as EP from '../../../core/shared/element-path'
import {
  createEditorState,
  deriveState,
  EditorState,
  reconstructJSXMetadata,
  getOpenUIJSFile,
  PersistentModel,
  StoryboardFilePath,
  defaultUserState,
  editorModelFromPersistentModel,
  withUnderlyingTargetFromEditorState,
  ElementProps,
} from '../store/editor-state'
import { editorMoveTemplate, UPDATE_FNS } from './actions'
import {
  insertInsertable,
  runEscapeHatch,
  setCanvasFrames,
  setFocusedElement,
  setProp_UNSAFE,
  switchLayoutSystem,
  updateFilePath,
  updateFromWorker,
  workerCodeAndParsedUpdate,
} from './action-creators'
import { getLayoutPropertyOr } from '../../../core/layout/getLayoutProperty'
import {
  ScenePathForTestUiJsFile,
  ScenePath1ForTestUiJsFile,
  sampleImportsForTests,
  TestScene0UID,
  TestMainComponentUID,
} from '../../../core/model/test-ui-js-file.test-utils'
import {
  BakedInStoryboardUID,
  BakedInStoryboardVariableName,
} from '../../../core/model/scene-utils'
import { sampleCode } from '../../../core/model/new-project-files'
import {
  getEditorState,
  makeTestProjectCodeWithSnippet,
  testPrintCodeFromEditorState,
  TestScenePath,
} from '../../canvas/ui-jsx.test-utils'
import { NO_OP } from '../../../core/shared/utils'
import { CURRENT_PROJECT_VERSION } from './migrations/migrations'
import { generateCodeResultCache } from '../../custom-code/code-file'
import {
  contentsToTree,
  getContentsTreeFileFromString,
  treeToContents,
  walkContentsTreeForParseSuccess,
} from '../../assets'
import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
import {
  getComponentGroups,
  insertableComponent,
  InsertableComponent,
} from '../../shared/project-components'
import { immediatelyResolvableDependenciesWithEditorRequirements } from '../npm-dependency/npm-dependency'
import { printCode, printCodeOptions } from '../../../core/workers/parser-printer/parser-printer'
import { resolvedNpmDependency } from '../../../core/shared/npm-dependency-types'
import { forceNotNull } from '../../../core/shared/optional-utils'
import { complexDefaultProjectPreParsed } from '../../../sample-projects/sample-project-utils.test-utils'
import { DefaultThirdPartyControlDefinitions } from '../../../core/third-party/third-party-controls'
import { cssNumber } from '../../inspector/common/css-utils'
import { createBuiltInDependenciesList } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { styleStringInArray } from '../../../utils/common-constants'
import { childInsertionPath } from '../store/insertion-path'
import {
  compose5Optics,
  compose2Optics,
  compose4Optics,
  Optic,
} from '../../../core/shared/optics/optics'
import { fromField, filtered, fromTypeGuard } from '../../../core/shared/optics/optic-creators'
import { contentsTreeOptic } from '../../../components/assets'
import { unsafeGet } from '../../../core/shared/optics/optic-utilities'

const chaiExpect = Chai.expect

function storyboardComponent(numberOfScenes: number): UtopiaJSXComponent {
  let scenes: Array<JSXElement> = []
  for (let sceneIndex = 0; sceneIndex < numberOfScenes; sceneIndex++) {
    scenes.push(
      jsxElement(
        'Scene',
        `scene-${sceneIndex}`,
        jsxAttributesFromMap({
          'data-uid': jsExpressionValue(`scene-${sceneIndex}`, emptyComments),
        }),
        [
          jsxElement(
            `MyView${sceneIndex + 1}`,
            `main-component-${sceneIndex}`,
            jsxAttributesFromMap({
              'data-uid': jsExpressionValue(`main-component-${sceneIndex}`, emptyComments),
              style: jsExpressionValue(
                {
                  position: 'absolute',
                  left: 0,
                  top: 0,
                  width: 375,
                  height: 812,
                },
                emptyComments,
              ),
            }),
            [],
          ),
        ],
      ),
    )
  }
  return utopiaJSXComponent(
    BakedInStoryboardVariableName,
    false,
    'var',
    'block',
    null,
    [],
    jsxElement(
      'Storyboard',
      BakedInStoryboardUID,
      jsxAttributesFromMap({
        'data-uid': jsExpressionValue(BakedInStoryboardUID, emptyComments),
      }),
      scenes,
    ),
    null,
    false,
    emptyComments,
  )
}

const originalModel = deepFreeze(
  parseSuccess(
    addImport(
      '/code.js',
      'utopia-api',
      null,
      [importAlias('View'), importAlias('Scene'), importAlias('Storyboard')],
      null,
      sampleImportsForTests,
    ),
    [
      utopiaJSXComponent(
        'MyView1',
        true,
        'var',
        'block',
        defaultPropsParam,
        [],
        jsxElement(
          jsxElementName('View', []),
          'aaa',
          jsxAttributesFromMap({
            'data-uid': jsExpressionValue('aaa', emptyComments),
          }),
          [
            jsxElement(
              jsxElementName('View', []),
              'bbb',
              jsxAttributesFromMap({
                test: jsxAttributeNestedObjectSimple(
                  jsxAttributesFromMap({ prop: jsExpressionValue(5, emptyComments) }),
                  emptyComments,
                ),
                'data-uid': jsExpressionValue('bbb', emptyComments),
              }),
              [],
            ),
          ],
        ),
        null,
        false,
        emptyComments,
      ),
      storyboardComponent(1),
    ],
    {},
    null,
    null,
    [exportFunction('whatever')],
    {},
  ),
)
const testEditor: EditorState = deepFreeze({
  ...createEditorState(NO_OP),
  projectContents: contentsToTree({
    [StoryboardFilePath]: textFile(
      textFileContents('', originalModel, RevisionsState.ParsedAhead),
      null,
      originalModel,
      0,
    ),
  }),
  jsxMetadata: createFakeMetadataForComponents(originalModel.topLevelElements),
})

describe('SET_PROP', () => {
  it('updates a simple value property', () => {
    const action = setProp_UNSAFE(
      EP.appendNewElementPath(ScenePathForTestUiJsFile, ['aaa', 'bbb']),
      PP.create('test', 'prop'),
      jsExpressionValue(100, emptyComments),
    )
    const newEditor = UPDATE_FNS.SET_PROP(action, testEditor)
    const newUiJsFile = getContentsTreeFileFromString(
      newEditor.projectContents,
      StoryboardFilePath,
    ) as TextFile
    expect(isTextFile(newUiJsFile)).toBeTruthy()
    expect(isParseSuccess(newUiJsFile.fileContents.parsed)).toBeTruthy()
    const newTopLevelElements: TopLevelElement[] = (newUiJsFile.fileContents.parsed as ParseSuccess)
      .topLevelElements
    const updatedRoot = newTopLevelElements[0] as UtopiaJSXComponent
    expect(isUtopiaJSXComponent(updatedRoot)).toBeTruthy()
    const updatedViewProps = Utils.pathOr<JSXAttributes>(
      [],
      ['rootElement', 'children', 0, 'props'],
      updatedRoot,
    )
    const updatedTestProp = getModifiableJSXAttributeAtPath(
      updatedViewProps,
      PP.create('test', 'prop'),
    )
    chaiExpect(mapEither(clearModifiableAttributeUniqueIDs, updatedTestProp)).to.deep.equal(
      right(clearExpressionUniqueIDs(jsExpressionValue(100, emptyComments))),
    )
  })
})

describe('SET_CANVAS_FRAMES', () => {
  const derivedState = deriveState(testEditor, null)
  it('Updates the frame of the child correctly', () => {
    const action = setCanvasFrames(
      [
        getFrameChange(
          EP.appendNewElementPath(ScenePathForTestUiJsFile, ['aaa', 'bbb']),
          canvasRectangle({ x: 20, y: 20, width: 50, height: 50 }),
          false,
        ),
      ],
      false,
    )
    const newEditor = UPDATE_FNS.SET_CANVAS_FRAMES(action, testEditor, derivedState)
    const newUiJsFile = getContentsTreeFileFromString(
      newEditor.projectContents,
      StoryboardFilePath,
    ) as TextFile
    expect(isTextFile(newUiJsFile)).toBeTruthy()
    expect(isParseSuccess(newUiJsFile.fileContents.parsed)).toBeTruthy()
    const newTopLevelElements: TopLevelElement[] = (newUiJsFile.fileContents.parsed as ParseSuccess)
      .topLevelElements
    const updatedRoot = newTopLevelElements[0] as UtopiaJSXComponent
    expect(isUtopiaJSXComponent(updatedRoot)).toBeTruthy()
    const updatedViewProps = Utils.pathOr<JSXAttributes>(
      [],
      ['rootElement', 'children', 0, 'props'],
      updatedRoot,
    )
    const leftProp = getLayoutPropertyOr(
      undefined,
      'left',
      right(updatedViewProps),
      styleStringInArray,
    )
    const top = getLayoutPropertyOr(undefined, 'top', right(updatedViewProps), styleStringInArray)
    const width = getLayoutPropertyOr(
      undefined,
      'width',
      right(updatedViewProps),
      styleStringInArray,
    )
    const height = getLayoutPropertyOr(
      undefined,
      'height',
      right(updatedViewProps),
      styleStringInArray,
    )
    chaiExpect(leftProp).to.deep.equal(cssNumber(20))
    chaiExpect(top).to.deep.equal(cssNumber(20))
    chaiExpect(width).to.deep.equal(cssNumber(50))
    chaiExpect(height).to.deep.equal(cssNumber(50))
  })
})

xdescribe('moveTemplate', () => {
  function fileModel(rootElements: Array<JSXElement>): Readonly<ParseSuccess> {
    return deepFreeze(
      parseSuccess(
        sampleImportsForTests,
        [
          storyboardComponent(rootElements.length),
          ...rootElements.map((element, index) => {
            const componentName = `MyView${index + 1}`
            return utopiaJSXComponent(
              componentName,
              true,
              'var',
              'block',
              defaultPropsParam,
              [],
              element,
              null,
              false,
              emptyComments,
            )
          }),
        ],
        {},
        null,
        null,
        [exportFunction('whatever')],
        {},
      ),
    )
  }

  function view(
    uid: string,
    children: Array<JSXElement> = [],
    x: FramePin = 0,
    y: FramePin = 0,
    width: FramePin = 0,
    height: FramePin = 0,
    name: string = 'View1',
  ): JSXElement {
    return jsxElement(
      jsxElementName(name, []),
      uid,
      jsxAttributesFromMap({
        style: jsxAttributeNestedObjectSimple(
          jsxAttributesFromMap({
            left: jsExpressionValue(x, emptyComments),
            top: jsExpressionValue(y, emptyComments),
            width: jsExpressionValue(width, emptyComments),
            height: jsExpressionValue(height, emptyComments),
          }),
          emptyComments,
        ),
        'data-uid': jsExpressionValue(uid, emptyComments),
      }),
      children,
    )
  }

  function testEditorFromParseSuccess(uiFile: Readonly<ParseSuccess>): EditorState {
    let editor: EditorState = {
      ...createEditorState(NO_OP),
      projectContents: contentsToTree({
        [StoryboardFilePath]: textFile(
          textFileContents('', uiFile, RevisionsState.ParsedAhead),
          null,
          uiFile,
          0,
        ),
      }),
    }
    editor.jsxMetadata = createFakeMetadataForComponents(uiFile.topLevelElements)

    return deepFreeze(editor)
  }

  it('reparents a simple child', () => {
    const view1 = view('bbb')
    const view2 = view('ccc')
    const root = view('aaa', [view1, view2])
    const editor = testEditorFromParseSuccess(fileModel([root]))

    const newEditor = editorMoveTemplate(
      EP.appendNewElementPath(ScenePathForTestUiJsFile, ['aaa', 'ccc']),
      EP.appendNewElementPath(ScenePathForTestUiJsFile, ['aaa', 'ccc']),
      'skipFrameChange',
      { type: 'front' },
      EP.appendNewElementPath(ScenePathForTestUiJsFile, ['aaa', 'bbb']),
      null,
      editor,
      null,
      null,
    ).editor

    const newUiJsFile = getContentsTreeFileFromString(
      newEditor.projectContents,
      StoryboardFilePath,
    ) as TextFile
    expect(isTextFile(newUiJsFile)).toBeTruthy()
    expect(isParseSuccess(newUiJsFile.fileContents.parsed)).toBeTruthy()
    const newComponents = getUtopiaJSXComponentsFromSuccess(
      newUiJsFile.fileContents.parsed as ParseSuccess,
    )
    const updatedRoot = newComponents[1] as UtopiaJSXComponent
    expect(isUtopiaJSXComponent(updatedRoot)).toBeTruthy()
    expect(Utils.pathOr([], ['rootElement', 'children'], updatedRoot)).toHaveLength(1)
    const expectedView2 = Utils.path(['rootElement', 'children', 0, 'children', 0], updatedRoot)
    chaiExpect(expectedView2).to.deep.equal(view2)
  })

  // TODO BALAZS FIX THIS BY MARCH 10 2020
  xit('does update the frame', () => {
    const view1 = view('bbb', [], 5, 5, 100, 100)
    const view2 = view('ccc', [], 15, 15, 100, 100)
    const root = view('aaa', [view1, view2], 10, 10, 100, 100)
    const editor = testEditorFromParseSuccess(fileModel([root]))

    const newEditor = editorMoveTemplate(
      EP.appendNewElementPath(ScenePathForTestUiJsFile, ['aaa', 'ccc']),
      EP.appendNewElementPath(ScenePathForTestUiJsFile, ['aaa', 'ccc']),
      {
        x: 25,
        y: 25,
        width: 100,
        height: 100,
      } as CanvasRectangle,
      { type: 'front' },
      EP.appendNewElementPath(ScenePathForTestUiJsFile, ['aaa', 'bbb']),
      {
        x: 15,
        y: 15,
        width: 100,
        height: 100,
      } as CanvasRectangle,
      editor,
      null,
      null,
    ).editor

    const newUiJsFile = getContentsTreeFileFromString(newEditor.projectContents, StoryboardFilePath)
    if (newUiJsFile != null && isTextFile(newUiJsFile)) {
      if (isParseSuccess(newUiJsFile.fileContents.parsed)) {
        const newTopLevelElements = newUiJsFile.fileContents.parsed.topLevelElements
        const updatedRoot = newTopLevelElements[0]
        if (isUtopiaJSXComponent(updatedRoot)) {
          expect(Utils.pathOr([], ['rootElement', 'children'], updatedRoot)).toHaveLength(1)
          const movedView = Utils.path(['rootElement', 'children', 0, 'children', 0], updatedRoot)
          expect(movedView).toEqual(view('ccc', [], 10, 10, 100, 100))
        } else {
          throw new Error('First top level element is not a component.')
        }
      } else {
        throw new Error('File does not contain parse success.')
      }
    } else {
      throw new Error('src/app.js is not a UI JS file.')
    }
  })

  // TODO BALAZS FIX THIS BY MARCH 10 2020
  xit('does update a relative frame too', () => {
    const view1 = view('bbb', [], '5%', '5%', 100, 100, 'BBB')
    const view2 = view('ccc', [], '15%', '15%', 100, 100, 'CCC')
    const root = view('aaa', [view1, view2], 10, 10, 100, 100, 'AAA')
    const editor = testEditorFromParseSuccess(fileModel([root]))

    const newEditor = editorMoveTemplate(
      EP.appendNewElementPath(ScenePathForTestUiJsFile, ['aaa', 'ccc']),
      EP.appendNewElementPath(ScenePathForTestUiJsFile, ['aaa', 'ccc']),
      {
        x: 25,
        y: 25,
        width: 100,
        height: 100,
      } as CanvasRectangle,
      { type: 'front' },
      EP.appendNewElementPath(ScenePathForTestUiJsFile, ['aaa', 'bbb']),
      {
        x: 15,
        y: 15,
        width: 100,
        height: 100,
      } as CanvasRectangle,
      editor,
      null,
      null,
    ).editor

    const newUiJsFile = getContentsTreeFileFromString(
      newEditor.projectContents,
      StoryboardFilePath,
    ) as TextFile
    expect(isTextFile(newUiJsFile)).toBeTruthy()
    expect(isParseSuccess(newUiJsFile.fileContents.parsed)).toBeTruthy()
    const newComponents = getUtopiaJSXComponentsFromSuccess(
      newUiJsFile.fileContents.parsed as ParseSuccess,
    )
    const updatedRoot = newComponents[1] as UtopiaJSXComponent
    expect(isUtopiaJSXComponent(updatedRoot)).toBeTruthy()
    expect(Utils.pathOr([], ['rootElement', 'children'], updatedRoot)).toHaveLength(1)
    const actual = Utils.path(['rootElement', 'children', 0, 'children', 0], updatedRoot)
    const expected = view('ccc', [], '10%', '10%', 100, 100, 'CCC')
    expect(actual).toEqual(expected)
  })

  it('reparents into a child arrays end', () => {
    const view2 = view('ccc')
    const view1 = view('bbb', [view2])
    const root = view('aaa', [view1])
    const editor = testEditorFromParseSuccess(fileModel([root]))

    const newEditor = editorMoveTemplate(
      EP.appendNewElementPath(ScenePathForTestUiJsFile, ['aaa', 'bbb', 'ccc']),
      EP.appendNewElementPath(ScenePathForTestUiJsFile, ['aaa', 'bbb', 'ccc']),
      'skipFrameChange',
      { type: 'front' },
      EP.appendNewElementPath(ScenePathForTestUiJsFile, ['aaa']),
      null,
      editor,
      null,
      null,
    ).editor

    const newUiJsFile = getContentsTreeFileFromString(
      newEditor.projectContents,
      StoryboardFilePath,
    ) as TextFile
    expect(isTextFile(newUiJsFile)).toBeTruthy()
    expect(isParseSuccess(newUiJsFile.fileContents.parsed)).toBeTruthy()
    const newComponents = getUtopiaJSXComponentsFromSuccess(
      newUiJsFile.fileContents.parsed as ParseSuccess,
    )
    const updatedRoot = newComponents[1] as UtopiaJSXComponent
    expect(isUtopiaJSXComponent(updatedRoot)).toBeTruthy()
    expect(Utils.pathOr([], ['rootElement', 'children'], updatedRoot)).toHaveLength(2)
    const actual = Utils.path(['rootElement', 'children', 1], updatedRoot)
    chaiExpect(actual).to.deep.equal(view2)
  })

  it('reparents into a child arrays beginning', () => {
    const view2 = view('ccc')
    const view1 = view('bbb', [view2])
    const root = view('aaa', [view1])
    const editor = testEditorFromParseSuccess(fileModel([root]))

    const newEditor = editorMoveTemplate(
      EP.appendNewElementPath(ScenePathForTestUiJsFile, ['aaa', 'bbb', 'ccc']),
      EP.appendNewElementPath(ScenePathForTestUiJsFile, ['aaa', 'bbb', 'ccc']),
      'skipFrameChange',
      { type: 'back' },
      EP.appendNewElementPath(ScenePathForTestUiJsFile, ['aaa']),
      null,
      editor,
      null,
      null,
    ).editor

    const newUiJsFile = getContentsTreeFileFromString(
      newEditor.projectContents,
      StoryboardFilePath,
    ) as TextFile
    expect(isTextFile(newUiJsFile)).toBeTruthy()
    expect(isParseSuccess(newUiJsFile.fileContents.parsed)).toBeTruthy()
    const newComponents = getUtopiaJSXComponentsFromSuccess(
      newUiJsFile.fileContents.parsed as ParseSuccess,
    )
    const updatedRoot = newComponents[1] as UtopiaJSXComponent
    expect(isUtopiaJSXComponent(updatedRoot)).toBeTruthy()
    expect(Utils.pathOr([], ['rootElement', 'children'], updatedRoot)).toHaveLength(2)
    const actual = Utils.path(['rootElement', 'children', 0], updatedRoot)
    chaiExpect(actual).to.deep.equal(view2)
  })

  it('reparents into a child arrays index', () => {
    const view2 = view('ccc')
    const view1 = view('bbb', [view2])
    const view3 = view('ddd')
    const root = view('aaa', [view1, view3])
    const editor = testEditorFromParseSuccess(fileModel([root]))

    const newEditor = editorMoveTemplate(
      EP.appendNewElementPath(ScenePathForTestUiJsFile, ['aaa', 'bbb', 'ccc']),
      EP.appendNewElementPath(ScenePathForTestUiJsFile, ['aaa', 'bbb', 'ccc']),
      'skipFrameChange',
      { type: 'absolute', index: 1 },
      EP.appendNewElementPath(ScenePathForTestUiJsFile, ['aaa']),
      null,
      editor,
      null,
      null,
    ).editor

    const newUiJsFile = getContentsTreeFileFromString(
      newEditor.projectContents,
      StoryboardFilePath,
    ) as TextFile
    expect(isTextFile(newUiJsFile)).toBeTruthy()
    expect(isParseSuccess(newUiJsFile.fileContents.parsed)).toBeTruthy()
    const newComponents = getUtopiaJSXComponentsFromSuccess(
      newUiJsFile.fileContents.parsed as ParseSuccess,
    )
    const updatedRoot = newComponents[1] as UtopiaJSXComponent
    expect(isUtopiaJSXComponent(updatedRoot)).toBeTruthy()
    expect(Utils.pathOr([], ['rootElement', 'children'], updatedRoot)).toHaveLength(3)
    const actual = Utils.path(['rootElement', 'children', 1], updatedRoot)
    chaiExpect(actual).to.deep.equal(view2)
  })

  it('reparents across components', () => {
    const view1 = view('bbb')
    const root1 = view('aaa', [view1])
    const root2 = view('ccc', [])
    const editor = testEditorFromParseSuccess(fileModel([root1, root2]))

    const newEditor = editorMoveTemplate(
      EP.appendNewElementPath(ScenePathForTestUiJsFile, ['aaa', 'bbb']),
      EP.appendNewElementPath(ScenePathForTestUiJsFile, ['aaa', 'bbb']),
      'skipFrameChange',
      { type: 'front' },
      EP.appendNewElementPath(ScenePath1ForTestUiJsFile, ['ccc']),
      null,
      editor,
      null,
      null,
    ).editor

    const newUiJsFile = getContentsTreeFileFromString(
      newEditor.projectContents,
      StoryboardFilePath,
    ) as TextFile
    expect(isTextFile(newUiJsFile)).toBeTruthy()
    expect(isParseSuccess(newUiJsFile.fileContents.parsed)).toBeTruthy()
    const newComponents = getUtopiaJSXComponentsFromSuccess(
      newUiJsFile.fileContents.parsed as ParseSuccess,
    )
    const updatedRoot1 = newComponents[1] as UtopiaJSXComponent
    expect(isUtopiaJSXComponent(updatedRoot1)).toBeTruthy()
    expect(Utils.pathOr([], ['rootElement', 'children'], updatedRoot1)).toHaveLength(0)
    const updatedRoot2 = newComponents[2] as UtopiaJSXComponent
    expect(isUtopiaJSXComponent(updatedRoot2)).toBeTruthy()
    expect(Utils.pathOr([], ['rootElement', 'children'], updatedRoot2)).toHaveLength(1)
    const actual = Utils.path(['rootElement', 'children', 0], updatedRoot2)
    chaiExpect(actual).to.deep.equal(view1)
  })

  it('reparents from pinned to group with frame props updated', () => {
    const view1 = jsxElement(
      jsxElementName('bbb', []),
      'bbb',
      jsxAttributesFromMap({
        style: jsxAttributeNestedObjectSimple(
          jsxAttributesFromMap({
            bottom: jsExpressionValue(50, emptyComments),
            right: jsExpressionValue(50, emptyComments),
            width: jsExpressionValue(100, emptyComments),
            height: jsExpressionValue(100, emptyComments),
          }),
          emptyComments,
        ),
        'data-uid': jsExpressionValue('bbb', emptyComments),
      }),
      [],
    )
    const root2 = view('ddd', [], -10, -10, 100, 100, 'Group')
    const root1 = view('aaa', [view1])
    const editor = testEditorFromParseSuccess(fileModel([root1, root2]))
    const groupFrame = canvasRectangle({ x: -10, y: -10, width: 100, height: 100 })

    const newEditor = editorMoveTemplate(
      EP.appendNewElementPath(ScenePathForTestUiJsFile, ['aaa', 'bbb']),
      EP.appendNewElementPath(ScenePathForTestUiJsFile, ['aaa', 'bbb']),
      canvasRectangle({ x: 10, y: 10, width: 100, height: 100 }),
      { type: 'front' },
      EP.appendNewElementPath(ScenePath1ForTestUiJsFile, ['ddd']),
      groupFrame,
      editor,
      LayoutSystem.Group,
      null,
    ).editor

    const newUiJsFile = getContentsTreeFileFromString(
      newEditor.projectContents,
      StoryboardFilePath,
    ) as TextFile
    expect(isTextFile(newUiJsFile)).toBeTruthy()
    expect(isParseSuccess(newUiJsFile.fileContents.parsed)).toBeTruthy()
    const newComponents = getUtopiaJSXComponentsFromSuccess(
      newUiJsFile.fileContents.parsed as ParseSuccess,
    )
    const updatedGroup = newComponents[2] as UtopiaJSXComponent
    expect(isUtopiaJSXComponent(updatedGroup)).toBeTruthy()
    expect(Utils.pathOr([], ['rootElement', 'children'], updatedGroup)).toHaveLength(1)
    const actual: any = Utils.path(['rootElement', 'children', 0], updatedGroup)
    expect(
      getLayoutPropertyOr(undefined, 'left', right(actual.props), styleStringInArray),
    ).toBeDefined()
    expect(
      getLayoutPropertyOr(undefined, 'top', right(actual.props), styleStringInArray),
    ).toBeDefined()
    expect(
      getLayoutPropertyOr(undefined, 'width', right(actual.props), styleStringInArray),
    ).toBeDefined()
    expect(
      getLayoutPropertyOr(undefined, 'height', right(actual.props), styleStringInArray),
    ).toBeDefined()
    expect(
      getLayoutPropertyOr(undefined, 'right', right(actual.props), styleStringInArray),
    ).not.toBeDefined()
    expect(
      getLayoutPropertyOr(undefined, 'bottom', right(actual.props), styleStringInArray),
    ).not.toBeDefined()
  })
})

function getOpenFileComponents(editor: EditorState): Array<UtopiaJSXComponent> {
  const openFile = getOpenUIJSFile(editor)
  if (openFile == null) {
    return []
  } else {
    if (isParseSuccess(openFile.fileContents.parsed)) {
      return getUtopiaJSXComponentsFromSuccess(openFile.fileContents.parsed)
    } else {
      return []
    }
  }
}

describe('SWITCH_LAYOUT_SYSTEM', () => {
  const childElement = jsxElement(
    'View',
    'bbb',
    jsxAttributesFromMap({
      'data-uid': jsExpressionValue('bbb', emptyComments),
      style: jsExpressionValue(
        {
          left: 5,
          top: 10,
          width: 200,
          height: 300,
        },
        emptyComments,
      ),
    }),
    [],
  )
  const rootElement = jsxElement(
    'View',
    'aaa',
    jsxAttributesFromMap({
      'data-uid': jsExpressionValue('aaa', emptyComments),
      style: jsExpressionValue({ backgroundColor: '#FFFFFF' }, emptyComments),
    }),
    [childElement],
  )
  const firstTopLevelElement = utopiaJSXComponent(
    'App',
    true,
    'var',
    'block',
    null,
    [],
    rootElement,
    null,
    false,
    emptyComments,
  )
  const storyboard = utopiaJSXComponent(
    BakedInStoryboardVariableName,
    false,
    'var',
    'block',
    null,
    [],
    jsxElement(
      'Storyboard',
      BakedInStoryboardUID,
      jsxAttributesFromMap({
        'data-uid': jsExpressionValue(BakedInStoryboardUID, emptyComments),
      }),
      [
        jsxElement(
          'Scene',
          'scene-0',
          jsxAttributesFromMap({
            component: jsExpressionOtherJavaScript('App', `return App`, ['App'], null, {}),
            'data-uid': jsExpressionValue('scene-0', emptyComments),
          }),
          [],
        ),
      ],
    ),
    null,
    false,
    emptyComments,
  )

  const parsedUIFile = parseSuccess(
    sampleImportsForTests,
    [firstTopLevelElement, storyboard],
    {},
    null,
    null,
    [exportFunction('whatever')],
    {},
  )

  const fileForUI = textFile(
    textFileContents('', parsedUIFile, RevisionsState.ParsedAhead),
    null,
    parsedUIFile,
    0,
  )
  const rootElementPath = EP.elementPath([[BakedInStoryboardUID, 'scene-0'], ['aaa']])
  const childElementPath = EP.elementPath([
    [BakedInStoryboardUID, 'scene-0'],
    ['aaa', 'bbb'],
  ])

  const rootElementProps: ElementProps = {
    'data-uid': 'aaa',
  }
  const rootElementMetadata: ElementInstanceMetadata = {
    elementPath: rootElementPath,
    element: right(firstTopLevelElement.rootElement),
    globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
    localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 100 }),
    componentInstance: false,
    isEmotionOrStyledComponent: false,
    specialSizeMeasurements: emptySpecialSizeMeasurements,
    computedStyle: emptyComputedStyle,
    attributeMetadatada: emptyAttributeMetadatada,
    label: null,
    importInfo: null,
    conditionValue: 'not-a-conditional',
    textContent: null,
  }

  const childElementProps: ElementProps = {
    'data-uid': 'bbb',
    style: {
      left: 5,
      top: 10,
      width: 200,
      height: 300,
    },
  }
  const childElementMetadata: ElementInstanceMetadata = {
    elementPath: childElementPath,
    element: right(childElement),
    globalFrame: canvasRectangle({ x: 0, y: 0, width: 200, height: 300 }),
    localFrame: localRectangle({ x: 0, y: 0, width: 200, height: 300 }),
    componentInstance: false,
    isEmotionOrStyledComponent: false,
    specialSizeMeasurements: emptySpecialSizeMeasurements,
    computedStyle: emptyComputedStyle,
    attributeMetadatada: emptyAttributeMetadatada,
    label: null,
    importInfo: null,
    conditionValue: 'not-a-conditional',
    textContent: null,
  }

  const elementMetadataMap: ElementInstanceMetadataMap = {
    [EP.toString(rootElementPath)]: rootElementMetadata,
    [EP.toString(childElementPath)]: childElementMetadata,
  }

  const testEditorWithPins: EditorState = deepFreeze({
    ...createEditorState(NO_OP),
    projectContents: contentsToTree({
      [StoryboardFilePath]: fileForUI,
    }),
    jsxMetadata: elementMetadataMap,
    selectedViews: [EP.elementPath([[BakedInStoryboardUID, 'scene-0'], ['aaa']])],
  })
  it('switches from pins to flex correctly', () => {
    const switchActionToFlex = switchLayoutSystem('flex', styleStringInArray)
    const result = UPDATE_FNS.SWITCH_LAYOUT_SYSTEM(switchActionToFlex, testEditorWithPins)
    expect(getOpenFileComponents(result).map(clearTopLevelElementUniqueIDs)).toMatchSnapshot()
  })
  it('switches from flex to pins correctly', () => {
    const switchActionToFlex = switchLayoutSystem('flex', styleStringInArray)
    let result = UPDATE_FNS.SWITCH_LAYOUT_SYSTEM(switchActionToFlex, testEditorWithPins)
    const switchActionToPins = switchLayoutSystem(LayoutSystem.PinSystem, styleStringInArray)
    result = UPDATE_FNS.SWITCH_LAYOUT_SYSTEM(switchActionToPins, result)
    expect(getOpenFileComponents(result).map(clearTopLevelElementUniqueIDs)).toMatchSnapshot()
  })
})

describe('LOAD', () => {
  it('Parses all UIJS files and bins any previously stored parsed model data', () => {
    const firstUIJSFile = StoryboardFilePath
    const secondUIJSFile = '/src/some/other/file.js'
    const initialFileContents: TextFileContents = textFileContents(
      sampleCode,
      unparsed,
      RevisionsState.CodeAhead,
    )
    const loadedModel: PersistentModel = {
      appID: null,
      forkedFromProjectId: null,
      projectVersion: CURRENT_PROJECT_VERSION,
      projectDescription: '',
      projectContents: contentsToTree({
        [firstUIJSFile]: textFile(initialFileContents, null, null, 0),
        [secondUIJSFile]: textFile(initialFileContents, null, null, 0),
      }),
      exportsInfo: [],
      codeEditorErrors: {
        buildErrors: {},
        lintErrors: {},
      },
      lastUsedFont: null,
      hiddenInstances: [],
      fileBrowser: {
        minimised: false,
      },
      dependencyList: {
        minimised: false,
      },
      projectSettings: {
        minimised: false,
      },
      navigator: {
        minimised: false,
      },
      githubSettings: {
        targetRepository: null,
        originCommit: null,
        branchName: null,
        pendingCommit: null,
        branchLoaded: false,
      },
      githubChecksums: null,
      branchContents: null,
      assetChecksums: {},
      colorSwatches: [],
    }

    const action = {
      action: 'LOAD' as const,
      model: loadedModel,
      nodeModules: {},
      packageResult: {},
      codeResultCache: generateCodeResultCache({}, {}, [], {}, NO_OP, {}, []),
      title: '',
      projectId: '',
      storedState: null,
      safeMode: false,
    }

    const startingState = deepFreeze(createEditorState(NO_OP))
    const result = UPDATE_FNS.LOAD(action, startingState, NO_OP)
    const newFirstFileContents = (
      getContentsTreeFileFromString(result.projectContents, firstUIJSFile) as TextFile
    ).fileContents
    expect(isUnparsed(newFirstFileContents.parsed)).toBeTruthy()
    expect(newFirstFileContents.code).toEqual(initialFileContents.code)
    const newSecondFileContents = (
      getContentsTreeFileFromString(result.projectContents, secondUIJSFile) as TextFile
    ).fileContents
    expect(isUnparsed(newSecondFileContents.parsed)).toBeTruthy()
    expect(newSecondFileContents.code).toEqual(initialFileContents.code)
  })
})

describe('UPDATE_FILE_PATH', () => {
  it('updates the files in a directory and imports related to it', () => {
    const project = complexDefaultProjectPreParsed()
    const editorState = editorModelFromPersistentModel(project, NO_OP)
    const actualResult = UPDATE_FNS.UPDATE_FILE_PATH(
      updateFilePath('/src', '/src2'),
      editorState,
      defaultUserState,
    )
    let filesAndTheirImports: { [filename: string]: Array<string> } = {}
    walkContentsTreeForParseSuccess(actualResult.projectContents, (fullPath, success) => {
      filesAndTheirImports[fullPath] = Object.keys(success.imports).sort()
    })
    expect(filesAndTheirImports).toMatchInlineSnapshot(`
      Object {
        "/src2/app.js": Array [
          "/src2/card.js",
          "react",
        ],
        "/src2/card.js": Array [
          "non-existant-dummy-library",
          "react",
        ],
        "/src2/index.js": Array [
          "./app.js",
          "react",
          "react-dom",
        ],
        "/utopia/storyboard.js": Array [
          "/src2/app.js",
          "react",
          "utopia-api",
        ],
      }
    `)
  })
})

describe('INSERT_INSERTABLE', () => {
  it('inserts an element into the project with the given values', () => {
    const project = complexDefaultProjectPreParsed()
    const editorState = editorModelFromPersistentModel(project, NO_OP)

    const insertableGroups = getComponentGroups(
      { antd: { status: 'loaded' } },
      { antd: DefaultThirdPartyControlDefinitions.antd },
      editorState.projectContents,
      [resolvedNpmDependency('antd', '4.0.0')],
      StoryboardFilePath,
    )
    const antdGroup = forceNotNull(
      'Group should exist.',
      insertableGroups.find((group) => {
        return (
          group.source.type === 'PROJECT_DEPENDENCY_GROUP' && group.source.dependencyName === 'antd'
        )
      }),
    )
    const menuInsertable = forceNotNull(
      'Component should exist.',
      antdGroup.insertableComponents.find((insertable) => {
        return insertable.name === 'Menu'
      }),
    )

    const targetPath = EP.elementPath([
      ['storyboard-entity', 'scene-1-entity', 'app-entity'],
      ['app-outer-div', 'card-instance'],
      ['card-outer-div'],
    ])

    const action = insertInsertable(
      childInsertionPath(targetPath),
      menuInsertable,
      'do-not-add',
      null,
    )

    const actualResult = UPDATE_FNS.INSERT_INSERTABLE(action, editorState)
    const cardFile = getContentsTreeFileFromString(actualResult.projectContents, '/src/card.js')
    if (cardFile != null && isTextFile(cardFile)) {
      const parsed = cardFile.fileContents.parsed
      if (isParseSuccess(parsed)) {
        const printedCode = printCode(
          '/src/card.js',
          printCodeOptions(false, true, true, true),
          parsed.imports,
          parsed.topLevelElements,
          parsed.jsxFactoryFunction,
          parsed.exportsDetail,
        )
        expect(printedCode).toMatchInlineSnapshot(`
          "import * as React from 'react'
          import { Spring } from 'non-existant-dummy-library'
          import { Menu } from 'antd'
          import 'antd/dist/antd.css'
          export var Card = (props) => {
            return (
              <div style={{ ...props.style }}>
                <div
                  style={{
                    position: 'absolute',
                    left: 0,
                    top: 0,
                    width: 50,
                    height: 50,
                    backgroundColor: 'red',
                  }}
                />
                <Spring
                  data-testid='spring'
                  style={{
                    position: 'absolute',
                    left: 100,
                    top: 200,
                    width: 50,
                    height: 50,
                    backgroundColor: 'blue',
                  }}
                />
                <Menu
                  forceSubMenuRender={false}
                  inlineCollapsed={false}
                  inlineIndent={24}
                  mode='inline'
                  multiple={false}
                  selectable
                  subMenuCloseDelay={0.1}
                  subMenuOpenDelay={0}
                  theme='light'
                />
              </div>
            )
          }
          "
        `)
      } else {
        throw new Error('File does not contain parse success.')
      }
    } else {
      throw new Error('File is not a text file.')
    }
  })

  it('inserts an element into the project with the given values, also adding style props', () => {
    const project = complexDefaultProjectPreParsed()
    const editorState = editorModelFromPersistentModel(project, NO_OP)

    const insertableGroups = getComponentGroups(
      { antd: { status: 'loaded' } },
      { antd: DefaultThirdPartyControlDefinitions.antd },
      editorState.projectContents,
      [resolvedNpmDependency('antd', '4.0.0')],
      StoryboardFilePath,
    )
    const antdGroup = forceNotNull(
      'Group should exist.',
      insertableGroups.find((group) => {
        return (
          group.source.type === 'PROJECT_DEPENDENCY_GROUP' && group.source.dependencyName === 'antd'
        )
      }),
    )
    const menuInsertable = forceNotNull(
      'Component should exist.',
      antdGroup.insertableComponents.find((insertable) => {
        return insertable.name === 'Menu'
      }),
    )

    const targetPath = EP.elementPath([
      ['storyboard-entity', 'scene-1-entity', 'app-entity'],
      ['app-outer-div', 'card-instance'],
      ['card-outer-div'],
    ])

    const action = insertInsertable(
      childInsertionPath(targetPath),
      menuInsertable,
      'add-size',
      null,
    )

    const actualResult = UPDATE_FNS.INSERT_INSERTABLE(action, editorState)
    const cardFile = getContentsTreeFileFromString(actualResult.projectContents, '/src/card.js')
    if (cardFile != null && isTextFile(cardFile)) {
      const parsed = cardFile.fileContents.parsed
      if (isParseSuccess(parsed)) {
        const printedCode = printCode(
          '/src/card.js',
          printCodeOptions(false, true, true, true),
          parsed.imports,
          parsed.topLevelElements,
          parsed.jsxFactoryFunction,
          parsed.exportsDetail,
        )
        expect(printedCode).toMatchInlineSnapshot(`
          "import * as React from 'react'
          import { Spring } from 'non-existant-dummy-library'
          import { Menu } from 'antd'
          import 'antd/dist/antd.css'
          export var Card = (props) => {
            return (
              <div style={{ ...props.style }}>
                <div
                  style={{
                    position: 'absolute',
                    left: 0,
                    top: 0,
                    width: 50,
                    height: 50,
                    backgroundColor: 'red',
                  }}
                />
                <Spring
                  data-testid='spring'
                  style={{
                    position: 'absolute',
                    left: 100,
                    top: 200,
                    width: 50,
                    height: 50,
                    backgroundColor: 'blue',
                  }}
                />
                <Menu
                  forceSubMenuRender={false}
                  inlineCollapsed={false}
                  inlineIndent={24}
                  mode='inline'
                  multiple={false}
                  selectable
                  subMenuCloseDelay={0.1}
                  subMenuOpenDelay={0}
                  theme='light'
                  style={{ width: 100, height: 100 }}
                />
              </div>
            )
          }
          "
        `)
      } else {
        throw new Error('File does not contain parse success.')
      }
    } else {
      throw new Error('File is not a text file.')
    }
  })

  it('inserts an img element into the project, also adding style props', () => {
    const project = complexDefaultProjectPreParsed()
    const editorState = editorModelFromPersistentModel(project, NO_OP)

    const insertableGroups = getComponentGroups(
      {},
      {},
      editorState.projectContents,
      [],
      StoryboardFilePath,
    )
    const htmlGroup = forceNotNull(
      'Group should exist.',
      insertableGroups.find((group) => {
        return group.source.type === 'HTML_GROUP'
      }),
    )
    const imgInsertable = forceNotNull(
      'Component should exist.',
      htmlGroup.insertableComponents.find((insertable) => {
        return insertable.name === 'img'
      }),
    )

    const targetPath = EP.elementPath([
      ['storyboard-entity', 'scene-1-entity', 'app-entity'],
      ['app-outer-div', 'card-instance'],
      ['card-outer-div'],
    ])

    const action = insertInsertable(childInsertionPath(targetPath), imgInsertable, 'add-size', null)

    const actualResult = UPDATE_FNS.INSERT_INSERTABLE(action, editorState)
    const cardFile = getContentsTreeFileFromString(actualResult.projectContents, '/src/card.js')
    if (cardFile != null && isTextFile(cardFile)) {
      const parsed = cardFile.fileContents.parsed
      if (isParseSuccess(parsed)) {
        const printedCode = printCode(
          '/src/card.js',
          printCodeOptions(false, true, true, true),
          parsed.imports,
          parsed.topLevelElements,
          parsed.jsxFactoryFunction,
          parsed.exportsDetail,
        )
        expect(printedCode).toMatchInlineSnapshot(`
          "import * as React from 'react'
          import { Spring } from 'non-existant-dummy-library'
          export var Card = (props) => {
            return (
              <div style={{ ...props.style }}>
                <div
                  style={{
                    position: 'absolute',
                    left: 0,
                    top: 0,
                    width: 50,
                    height: 50,
                    backgroundColor: 'red',
                  }}
                />
                <Spring
                  data-testid='spring'
                  style={{
                    position: 'absolute',
                    left: 100,
                    top: 200,
                    width: 50,
                    height: 50,
                    backgroundColor: 'blue',
                  }}
                />
                <img
                  style={{
                    width: 100,
                    height: 100,
                    position: 'absolute',
                  }}
                  src='/editor/icons/favicons/favicon-128.png?hash=nocommit'
                />
              </div>
            )
          }
          "
        `)
      } else {
        throw new Error('File does not contain parse success.')
      }
    } else {
      throw new Error('File is not a text file.')
    }
  })

  it('inserts an img element into the project, also adding style props, added at the back', () => {
    const project = complexDefaultProjectPreParsed()
    const editorState = editorModelFromPersistentModel(project, NO_OP)

    const insertableGroups = getComponentGroups(
      {},
      {},
      editorState.projectContents,
      [],
      StoryboardFilePath,
    )
    const htmlGroup = forceNotNull(
      'Group should exist.',
      insertableGroups.find((group) => {
        return group.source.type === 'HTML_GROUP'
      }),
    )
    const imgInsertable = forceNotNull(
      'Component should exist.',
      htmlGroup.insertableComponents.find((insertable) => {
        return insertable.name === 'img'
      }),
    )

    const targetPath = EP.elementPath([
      ['storyboard-entity', 'scene-1-entity', 'app-entity'],
      ['app-outer-div', 'card-instance'],
      ['card-outer-div'],
    ])

    const action = insertInsertable(childInsertionPath(targetPath), imgInsertable, 'add-size', {
      type: 'back',
    })

    const actualResult = UPDATE_FNS.INSERT_INSERTABLE(action, editorState)
    const cardFile = getContentsTreeFileFromString(actualResult.projectContents, '/src/card.js')
    if (cardFile != null && isTextFile(cardFile)) {
      const parsed = cardFile.fileContents.parsed
      if (isParseSuccess(parsed)) {
        const printedCode = printCode(
          '/src/card.js',
          printCodeOptions(false, true, true, true),
          parsed.imports,
          parsed.topLevelElements,
          parsed.jsxFactoryFunction,
          parsed.exportsDetail,
        )
        expect(printedCode).toMatchInlineSnapshot(`
          "import * as React from 'react'
          import { Spring } from 'non-existant-dummy-library'
          export var Card = (props) => {
            return (
              <div style={{ ...props.style }}>
                <img
                  style={{
                    width: 100,
                    height: 100,
                    position: 'absolute',
                  }}
                  src='/editor/icons/favicons/favicon-128.png?hash=nocommit'
                />
                <div
                  style={{
                    position: 'absolute',
                    left: 0,
                    top: 0,
                    width: 50,
                    height: 50,
                    backgroundColor: 'red',
                  }}
                />
                <Spring
                  data-testid='spring'
                  style={{
                    position: 'absolute',
                    left: 100,
                    top: 200,
                    width: 50,
                    height: 50,
                    backgroundColor: 'blue',
                  }}
                />
              </div>
            )
          }
          "
        `)
      } else {
        throw new Error('File does not contain parse success.')
      }
    } else {
      throw new Error('File is not a text file.')
    }
  })
})

describe('SET_FOCUSED_ELEMENT', () => {
  it('prevents focusing a non-focusable element', () => {
    const project = complexDefaultProjectPreParsed()
    let editorState = editorModelFromPersistentModel(project, NO_OP)
    const pathToFocus = EP.fromString('storyboard-entity/scene-1-entity/app-entity:app-outer-div')
    const underlyingElement = forceNotNull(
      'Should be able to find this.',
      withUnderlyingTargetFromEditorState(pathToFocus, editorState, null, (_, element) => element),
    )
    const divElementMetadata = elementInstanceMetadata(
      pathToFocus,
      right(underlyingElement),
      zeroRectangle as CanvasRectangle,
      zeroRectangle as LocalRectangle,
      false,
      false,
      emptySpecialSizeMeasurements,
      emptyComputedStyle,
      emptyAttributeMetadatada,
      null,
      null,
      'not-a-conditional',
      null,
    )
    const fakeMetadata: ElementInstanceMetadataMap = {
      [EP.toString(pathToFocus)]: divElementMetadata,
    }
    editorState = {
      ...editorState,
      jsxMetadata: fakeMetadata,
    }
    const action = setFocusedElement(pathToFocus)
    const updatedEditorState = UPDATE_FNS.SET_FOCUSED_ELEMENT(action, editorState)
    expect(updatedEditorState).toBe(editorState)
  })
  it('focuses a focusable element without a problem', () => {
    const project = complexDefaultProjectPreParsed()
    let editorState = editorModelFromPersistentModel(project, NO_OP)
    const pathToFocus = EP.fromString(
      'storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance',
    )
    const underlyingElement = forceNotNull(
      'Should be able to find this.',
      withUnderlyingTargetFromEditorState(pathToFocus, editorState, null, (_, element) => element),
    )
    const cardElementMetadata = elementInstanceMetadata(
      pathToFocus,
      right(underlyingElement),
      zeroRectangle as CanvasRectangle,
      zeroRectangle as LocalRectangle,
      false,
      false,
      emptySpecialSizeMeasurements,
      emptyComputedStyle,
      emptyAttributeMetadatada,
      null,
      null,
      'not-a-conditional',
      null,
    )
    const fakeMetadata: ElementInstanceMetadataMap = {
      [EP.toString(pathToFocus)]: cardElementMetadata,
    }
    editorState = {
      ...editorState,
      jsxMetadata: fakeMetadata,
    }
    const action = setFocusedElement(pathToFocus)
    const updatedEditorState = UPDATE_FNS.SET_FOCUSED_ELEMENT(action, editorState)
    expect(updatedEditorState.focusedElementPath).toEqual(pathToFocus)
  })
})

function textFileFromEditorStateOptic(filename: string): Optic<EditorState, TextFile> {
  return compose5Optics(
    fromField('projectContents'),
    contentsTreeOptic,
    filtered(({ fullPath }) => fullPath === filename),
    fromField('file'),
    fromTypeGuard(isTextFile),
  )
}

function lastRevisedTimeOptic(filename: string): Optic<EditorState, number> {
  return compose2Optics(textFileFromEditorStateOptic(filename), fromField('lastRevisedTime'))
}
function parsedTextFileOptic(filename: string): Optic<EditorState, ParseSuccess> {
  return compose4Optics(
    textFileFromEditorStateOptic(filename),
    fromField('fileContents'),
    fromField('parsed'),
    fromTypeGuard(isParseSuccess),
  )
}

describe('UPDATE_FROM_WORKER', () => {
  it('should prevent all updates from applying if any are stale', () => {
    // Setup and getting some starting values.
    const project = complexDefaultProjectPreParsed()
    const startingEditorState = editorModelFromPersistentModel(project, NO_OP)
    const storyboardFile = unsafeGet(parsedTextFileOptic(StoryboardFilePath), startingEditorState)
    const updatedStoryboardFile: ParseSuccess = {
      ...storyboardFile,
      topLevelElements: [...storyboardFile.topLevelElements, unparsedCode('// Nonsense')],
    }
    const appJSFile = unsafeGet(parsedTextFileOptic('/src/app.js'), startingEditorState)
    const updatedAppJSFile: ParseSuccess = {
      ...appJSFile,
      topLevelElements: [...appJSFile.topLevelElements, unparsedCode('// Other nonsense.')],
    }
    const lastRevisedTimeOfStoryboard = unsafeGet(
      lastRevisedTimeOptic(StoryboardFilePath),
      startingEditorState,
    )
    const lastRevisedTimeOfAppJS = unsafeGet(
      lastRevisedTimeOptic('/src/app.js'),
      startingEditorState,
    )

    // Create the action and fire it.
    const updateToCheck = updateFromWorker([
      workerCodeAndParsedUpdate(
        StoryboardFilePath,
        '// Not relevant.',
        updatedStoryboardFile,
        lastRevisedTimeOfStoryboard + 1,
      ),
      workerCodeAndParsedUpdate(
        '/src/app.js',
        '// Not relevant.',
        updatedAppJSFile,
        lastRevisedTimeOfAppJS - 1,
      ),
    ])
    const updatedEditorState = UPDATE_FNS.UPDATE_FROM_WORKER(updateToCheck, startingEditorState)

    // Check that the model hasn't changed, because of the stale revised time.
    expect(updatedEditorState).toBe(startingEditorState)
  })
  it('should apply all if none are stale', () => {
    // Setup and getting some starting values.
    const project = complexDefaultProjectPreParsed()
    const startingEditorState = editorModelFromPersistentModel(project, NO_OP)
    const storyboardFile = unsafeGet(parsedTextFileOptic(StoryboardFilePath), startingEditorState)
    const updatedStoryboardFile: ParseSuccess = {
      ...storyboardFile,
      topLevelElements: [...storyboardFile.topLevelElements, unparsedCode('// Nonsense')],
    }
    const appJSFile = unsafeGet(parsedTextFileOptic('/src/app.js'), startingEditorState)
    const updatedAppJSFile: ParseSuccess = {
      ...appJSFile,
      topLevelElements: [...appJSFile.topLevelElements, unparsedCode('// Other nonsense.')],
    }
    const lastRevisedTimeOfStoryboard = unsafeGet(
      lastRevisedTimeOptic(StoryboardFilePath),
      startingEditorState,
    )
    const lastRevisedTimeOfAppJS = unsafeGet(
      lastRevisedTimeOptic('/src/app.js'),
      startingEditorState,
    )

    // Create the action and fire it.
    const updateToCheck = updateFromWorker([
      workerCodeAndParsedUpdate(
        StoryboardFilePath,
        '// Not relevant.',
        updatedStoryboardFile,
        lastRevisedTimeOfStoryboard + 1,
      ),
      workerCodeAndParsedUpdate(
        '/src/app.js',
        '// Not relevant.',
        updatedAppJSFile,
        lastRevisedTimeOfAppJS + 1,
      ),
    ])
    const updatedEditorState = UPDATE_FNS.UPDATE_FROM_WORKER(updateToCheck, startingEditorState)

    // Get the same values that we started with but from the updated editor state.
    const updatedStoryboardLastRevisedTimeFromState = unsafeGet(
      lastRevisedTimeOptic(StoryboardFilePath),
      updatedEditorState,
    )
    const updatedAppJSLastRevisedTimeFromState = unsafeGet(
      lastRevisedTimeOptic('/src/app.js'),
      updatedEditorState,
    )
    const updatedStoryboardFileFromState = unsafeGet(
      parsedTextFileOptic(StoryboardFilePath),
      updatedEditorState,
    )
    const updatedAppJSFileFromState = unsafeGet(
      parsedTextFileOptic('/src/app.js'),
      updatedEditorState,
    )

    // Check that the changes were applied into the model.
    expect(updatedStoryboardLastRevisedTimeFromState).toBeGreaterThanOrEqual(
      lastRevisedTimeOfStoryboard,
    )
    expect(updatedAppJSLastRevisedTimeFromState).toBeGreaterThanOrEqual(lastRevisedTimeOfAppJS)
    expect(updatedStoryboardFileFromState).toStrictEqual(updatedStoryboardFile)
    expect(updatedAppJSFileFromState).toStrictEqual(updatedAppJSFile)
  })
})
