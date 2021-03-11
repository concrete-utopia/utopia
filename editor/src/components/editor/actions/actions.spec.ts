import * as Chai from 'chai'
import { FramePin, LayoutSystem } from 'utopia-api'
import {
  isUtopiaJSXComponent,
  jsxAttributeNestedObjectSimple,
  JSXAttributes,
  jsxAttributeValue,
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
  jsxMetadata,
  ComponentMetadata,
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  jsxAttributesFromMap,
  emptyAttributeMetadatada,
} from '../../../core/shared/element-template'
import { getModifiableJSXAttributeAtPath } from '../../../core/shared/jsx-attributes'
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
  addModifierExportToDetail,
  EmptyExportsDetail,
} from '../../../core/shared/project-file-types'
import { emptyImports, parseSuccess } from '../../../core/workers/common/project-file-utils'
import { deepFreeze } from '../../../utils/deep-freeze'
import { right, forceRight, left, isRight } from '../../../core/shared/either'
import { createFakeMetadataForComponents } from '../../../utils/test-utils'
import Utils from '../../../utils/utils'
import {
  canvasRectangle,
  CanvasRectangle,
  LocalRectangle,
  localRectangle,
} from '../../../core/shared/math-utils'
import { createTestProjectWithCode, getFrameChange } from '../../canvas/canvas-utils'
import * as PP from '../../../core/shared/property-path'
import * as TP from '../../../core/shared/template-path'
import {
  createEditorState,
  deriveState,
  EditorState,
  reconstructJSXMetadata,
  getOpenUIJSFile,
  getOpenUtopiaJSXComponentsFromState,
  PersistentModel,
} from '../store/editor-state'
import { editorMoveTemplate, UPDATE_FNS } from './actions'
import { setCanvasFrames, setProp_UNSAFE, switchLayoutSystem } from './action-creators'
import { getLayoutPropertyOr } from '../../../core/layout/getLayoutProperty'
import {
  ScenePathForTestUiJsFile,
  ScenePath1ForTestUiJsFile,
  sampleDefaultImports,
} from '../../../core/model/test-ui-js-file'
import { BakedInStoryboardUID } from '../../../core/model/scene-utils'
import { getDefaultUIJsFile, sampleCode } from '../../../core/model/new-project-files'
import { TestScenePath } from '../../canvas/ui-jsx.test-utils'
import { NO_OP } from '../../../core/shared/utils'
import { CURRENT_PROJECT_VERSION } from './migrations/migrations'
import { generateCodeResultCache } from '../../custom-code/code-file'
import { contentsToTree, getContentsTreeFileFromString } from '../../assets'
import { openFileTab } from '../store/editor-tabs'
import { emptyComments } from '../../../core/workers/parser-printer/parser-printer-comments'
const chaiExpect = Chai.expect

describe('SET_PROP', () => {
  const originalModel = deepFreeze(
    parseSuccess(
      emptyImports(),
      [
        utopiaJSXComponent(
          'MyView',
          true,
          'var',
          'block',
          defaultPropsParam,
          [],
          jsxElement(
            jsxElementName('View', []),
            jsxAttributesFromMap({
              'data-uid': jsxAttributeValue('aaa', emptyComments),
            }),
            [
              jsxElement(
                jsxElementName('View', []),
                jsxAttributesFromMap({
                  test: jsxAttributeNestedObjectSimple(
                    jsxAttributesFromMap({ prop: jsxAttributeValue(5, emptyComments) }),
                    emptyComments,
                  ),
                  'data-uid': jsxAttributeValue('bbb', emptyComments),
                }),
                [],
              ),
            ],
          ),
          null,
          false,
          emptyComments,
        ),
      ],
      {},
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    ),
  )
  const testEditor: EditorState = deepFreeze({
    ...createEditorState(NO_OP),
    projectContents: contentsToTree({
      '/src/app.js': textFile(
        textFileContents('', originalModel, RevisionsState.ParsedAhead),
        null,
        0,
      ),
    }),
    selectedFile: {
      tab: openFileTab('/src/app.js'),
      initialCursorPosition: null,
    },
    jsxMetadataKILLME: createFakeMetadataForComponents(originalModel.topLevelElements),
  })
  it('updates a simple value property', () => {
    const action = setProp_UNSAFE(
      TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'bbb']),
      PP.create(['test', 'prop']),
      jsxAttributeValue(100, emptyComments),
    )
    const newEditor = UPDATE_FNS.SET_PROP(action, testEditor)
    const newUiJsFile = getContentsTreeFileFromString(
      newEditor.projectContents,
      '/src/app.js',
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
      PP.create(['test', 'prop']),
    )
    chaiExpect(updatedTestProp).to.deep.equal(right(jsxAttributeValue(100, emptyComments)))
  })
})

describe('SET_CANVAS_FRAMES', () => {
  // Annoyingly, to test this properly we'd need the spy functions to run, so right now
  // we're making do with having everything zero offset
  const components = [
    utopiaJSXComponent(
      'MyView',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      jsxElement(
        jsxElementName('View', []),
        jsxAttributesFromMap({
          layout: jsxAttributeNestedObjectSimple(
            jsxAttributesFromMap({
              top: jsxAttributeValue(0, emptyComments),
              left: jsxAttributeValue(0, emptyComments),
              width: jsxAttributeValue(100, emptyComments),
              height: jsxAttributeValue(100, emptyComments),
            }),
            emptyComments,
          ),
          'data-uid': jsxAttributeValue('aaa', emptyComments),
        }),
        [
          jsxElement(
            jsxElementName('View', []),
            jsxAttributesFromMap({
              layout: jsxAttributeNestedObjectSimple(
                jsxAttributesFromMap({
                  top: jsxAttributeValue(0, emptyComments),
                  left: jsxAttributeValue(0, emptyComments),
                  width: jsxAttributeValue(10, emptyComments),
                  height: jsxAttributeValue(10, emptyComments),
                }),
                emptyComments,
              ),
              'data-uid': jsxAttributeValue('bbb', emptyComments),
            }),
            [],
          ),
        ],
      ),
      null,
      false,
      emptyComments,
    ),
  ]

  const originalModel = deepFreeze(
    parseSuccess(
      emptyImports(),
      components,
      {},
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    ),
  )
  const testEditor: EditorState = deepFreeze({
    ...createEditorState(NO_OP),
    projectContents: contentsToTree({
      '/src/app.js': textFile(
        textFileContents('', originalModel, RevisionsState.ParsedAhead),
        null,
        0,
      ),
    }),
    selectedFile: {
      tab: openFileTab('/src/app.js'),
      initialCursorPosition: null,
    },
    jsxMetadataKILLME: createFakeMetadataForComponents(originalModel.topLevelElements),
  })
  const derivedState = deriveState(testEditor, null)
  it('Updates the frame of the child correctly', () => {
    const action = setCanvasFrames(
      [
        getFrameChange(
          TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'bbb']),
          canvasRectangle({ x: 20, y: 20, width: 50, height: 50 }),
          false,
        ),
      ],
      false,
    )
    const newEditor = UPDATE_FNS.SET_CANVAS_FRAMES(action, testEditor, derivedState)
    const newUiJsFile = getContentsTreeFileFromString(
      newEditor.projectContents,
      '/src/app.js',
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
    const leftProp = getLayoutPropertyOr(undefined, 'PinnedLeft', right(updatedViewProps))
    const top = getLayoutPropertyOr(undefined, 'PinnedTop', right(updatedViewProps))
    const width = getLayoutPropertyOr(undefined, 'Width', right(updatedViewProps))
    const height = getLayoutPropertyOr(undefined, 'Height', right(updatedViewProps))
    chaiExpect(leftProp).to.equal(20)
    chaiExpect(top).to.equal(20)
    chaiExpect(width).to.equal(50)
    chaiExpect(height).to.equal(50)
  })
})

describe('moveTemplate', () => {
  function fileModel(rootElements: Array<JSXElement>): Readonly<ParseSuccess> {
    return deepFreeze(
      parseSuccess(
        emptyImports(),
        rootElements.map((element, index) =>
          utopiaJSXComponent(
            `MyView${index}`,
            true,
            'var',
            'block',
            defaultPropsParam,
            [],
            element,
            null,
            false,
            emptyComments,
          ),
        ),
        {},
        null,
        null,
        addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
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
      jsxAttributesFromMap({
        layout: jsxAttributeNestedObjectSimple(
          jsxAttributesFromMap({
            left: jsxAttributeValue(x, emptyComments),
            top: jsxAttributeValue(y, emptyComments),
            width: jsxAttributeValue(width, emptyComments),
            height: jsxAttributeValue(height, emptyComments),
          }),
          emptyComments,
        ),
        'data-uid': jsxAttributeValue(uid, emptyComments),
      }),
      children,
    )
  }

  function group(
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
      jsxAttributesFromMap({
        layout: jsxAttributeNestedObjectSimple(
          jsxAttributesFromMap({
            left: jsxAttributeValue(x, emptyComments),
            top: jsxAttributeValue(y, emptyComments),
            width: jsxAttributeValue(width, emptyComments),
            height: jsxAttributeValue(height, emptyComments),
            layoutSystem: jsxAttributeValue(LayoutSystem.Group, emptyComments),
          }),
          emptyComments,
        ),
        'data-uid': jsxAttributeValue(uid, emptyComments),
      }),
      children,
    )
  }

  function testEditor(uiFile: Readonly<ParseSuccess>): EditorState {
    let editor: EditorState = {
      ...createEditorState(NO_OP),
      projectContents: contentsToTree({
        '/src/app.js': textFile(textFileContents('', uiFile, RevisionsState.ParsedAhead), null, 0),
      }),
      selectedFile: {
        tab: openFileTab('/src/app.js'),
        initialCursorPosition: null,
      },
    }
    editor.jsxMetadataKILLME = createFakeMetadataForComponents(uiFile.topLevelElements)

    return deepFreeze(editor)
  }

  it('reparents a simple child', () => {
    const view1 = view('bbb')
    const view2 = view('ccc')
    const root = view('aaa', [view1, view2])
    const editor = testEditor(fileModel([root]))

    const newEditor = editorMoveTemplate(
      TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'ccc']),
      TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'ccc']),
      'skipFrameChange',
      { type: 'front' },
      TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'bbb']),
      null,
      editor,
      null,
    ).editor

    const newUiJsFile = getContentsTreeFileFromString(
      newEditor.projectContents,
      '/src/app.js',
    ) as TextFile
    expect(isTextFile(newUiJsFile)).toBeTruthy()
    expect(isParseSuccess(newUiJsFile.fileContents.parsed)).toBeTruthy()
    const newTopLevelElements: TopLevelElement[] = (newUiJsFile.fileContents.parsed as ParseSuccess)
      .topLevelElements
    const updatedRoot = newTopLevelElements[0] as UtopiaJSXComponent
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
    const editor = testEditor(fileModel([root]))

    const newEditor = editorMoveTemplate(
      TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'ccc']),
      TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'ccc']),
      {
        x: 25,
        y: 25,
        width: 100,
        height: 100,
      } as CanvasRectangle,
      { type: 'front' },
      TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'bbb']),
      {
        x: 15,
        y: 15,
        width: 100,
        height: 100,
      } as CanvasRectangle,
      editor,
      null,
    ).editor

    const newUiJsFile = getContentsTreeFileFromString(newEditor.projectContents, '/src/app.js')
    if (newUiJsFile != null && isTextFile(newUiJsFile)) {
      if (isParseSuccess(newUiJsFile.fileContents.parsed)) {
        const newTopLevelElements = newUiJsFile.fileContents.parsed.topLevelElements
        const updatedRoot = newTopLevelElements[0]
        if (isUtopiaJSXComponent(updatedRoot)) {
          expect(Utils.pathOr([], ['rootElement', 'children'], updatedRoot)).toHaveLength(1)
          const movedView = Utils.path(['rootElement', 'children', 0, 'children', 0], updatedRoot)
          expect(movedView).toEqual(view('ccc', [], 10, 10, 100, 100))
        } else {
          fail('First top level element is not a component.')
        }
      } else {
        fail('File does not contain parse success.')
      }
    } else {
      fail('src/app.js is not a UI JS file.')
    }
  })

  // TODO BALAZS FIX THIS BY MARCH 10 2020
  xit('does update a relative frame too', () => {
    const view1 = view('bbb', [], '5%', '5%', 100, 100, 'BBB')
    const view2 = view('ccc', [], '15%', '15%', 100, 100, 'CCC')
    const root = view('aaa', [view1, view2], 10, 10, 100, 100, 'AAA')
    const editor = testEditor(fileModel([root]))

    const newEditor = editorMoveTemplate(
      TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'ccc']),
      TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'ccc']),
      {
        x: 25,
        y: 25,
        width: 100,
        height: 100,
      } as CanvasRectangle,
      { type: 'front' },
      TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'bbb']),
      {
        x: 15,
        y: 15,
        width: 100,
        height: 100,
      } as CanvasRectangle,
      editor,
      null,
    ).editor

    const newUiJsFile = getContentsTreeFileFromString(
      newEditor.projectContents,
      '/src/app.js',
    ) as TextFile
    expect(isTextFile(newUiJsFile)).toBeTruthy()
    expect(isParseSuccess(newUiJsFile.fileContents.parsed)).toBeTruthy()
    const newTopLevelElements: TopLevelElement[] = (newUiJsFile.fileContents.parsed as ParseSuccess)
      .topLevelElements
    const updatedRoot = newTopLevelElements[0] as UtopiaJSXComponent
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
    const editor = testEditor(fileModel([root]))

    const newEditor = editorMoveTemplate(
      TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'bbb', 'ccc']),
      TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'bbb', 'ccc']),
      'skipFrameChange',
      { type: 'front' },
      TP.instancePath(ScenePathForTestUiJsFile, ['aaa']),
      null,
      editor,
      null,
    ).editor

    const newUiJsFile = getContentsTreeFileFromString(
      newEditor.projectContents,
      '/src/app.js',
    ) as TextFile
    expect(isTextFile(newUiJsFile)).toBeTruthy()
    expect(isParseSuccess(newUiJsFile.fileContents.parsed)).toBeTruthy()
    const newTopLevelElements: TopLevelElement[] = (newUiJsFile.fileContents.parsed as ParseSuccess)
      .topLevelElements
    const updatedRoot = newTopLevelElements[0] as UtopiaJSXComponent
    expect(isUtopiaJSXComponent(updatedRoot)).toBeTruthy()
    expect(Utils.pathOr([], ['rootElement', 'children'], updatedRoot)).toHaveLength(2)
    const actual = Utils.path(['rootElement', 'children', 1], updatedRoot)
    chaiExpect(actual).to.deep.equal(view2)
  })

  it('reparents into a child arrays beginning', () => {
    const view2 = view('ccc')
    const view1 = view('bbb', [view2])
    const root = view('aaa', [view1])
    const editor = testEditor(fileModel([root]))

    const newEditor = editorMoveTemplate(
      TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'bbb', 'ccc']),
      TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'bbb', 'ccc']),
      'skipFrameChange',
      { type: 'back' },
      TP.instancePath(ScenePathForTestUiJsFile, ['aaa']),
      null,
      editor,
      null,
    ).editor

    const newUiJsFile = getContentsTreeFileFromString(
      newEditor.projectContents,
      '/src/app.js',
    ) as TextFile
    expect(isTextFile(newUiJsFile)).toBeTruthy()
    expect(isParseSuccess(newUiJsFile.fileContents.parsed)).toBeTruthy()
    const newTopLevelElements: TopLevelElement[] = (newUiJsFile.fileContents.parsed as ParseSuccess)
      .topLevelElements
    const updatedRoot = newTopLevelElements[0] as UtopiaJSXComponent
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
    const editor = testEditor(fileModel([root]))

    const newEditor = editorMoveTemplate(
      TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'bbb', 'ccc']),
      TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'bbb', 'ccc']),
      'skipFrameChange',
      { type: 'absolute', index: 1 },
      TP.instancePath(ScenePathForTestUiJsFile, ['aaa']),
      null,
      editor,
      null,
    ).editor

    const newUiJsFile = getContentsTreeFileFromString(
      newEditor.projectContents,
      '/src/app.js',
    ) as TextFile
    expect(isTextFile(newUiJsFile)).toBeTruthy()
    expect(isParseSuccess(newUiJsFile.fileContents.parsed)).toBeTruthy()
    const newTopLevelElements: TopLevelElement[] = (newUiJsFile.fileContents.parsed as ParseSuccess)
      .topLevelElements
    const updatedRoot = newTopLevelElements[0] as UtopiaJSXComponent
    expect(isUtopiaJSXComponent(updatedRoot)).toBeTruthy()
    expect(Utils.pathOr([], ['rootElement', 'children'], updatedRoot)).toHaveLength(3)
    const actual = Utils.path(['rootElement', 'children', 1], updatedRoot)
    chaiExpect(actual).to.deep.equal(view2)
  })

  it('reparents across components', () => {
    const view1 = view('bbb')
    const root1 = view('aaa', [view1])
    const root2 = view('ccc', [])
    const editor = testEditor(fileModel([root1, root2]))

    const newEditor = editorMoveTemplate(
      TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'bbb']),
      TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'bbb']),
      'skipFrameChange',
      { type: 'front' },
      TP.instancePath(ScenePath1ForTestUiJsFile, ['ccc']),
      null,
      editor,
      null,
    ).editor

    const newUiJsFile = getContentsTreeFileFromString(
      newEditor.projectContents,
      '/src/app.js',
    ) as TextFile
    expect(isTextFile(newUiJsFile)).toBeTruthy()
    expect(isParseSuccess(newUiJsFile.fileContents.parsed)).toBeTruthy()
    const newTopLevelElements: TopLevelElement[] = (newUiJsFile.fileContents.parsed as ParseSuccess)
      .topLevelElements
    const updatedRoot1 = newTopLevelElements[0] as UtopiaJSXComponent
    expect(isUtopiaJSXComponent(updatedRoot1)).toBeTruthy()
    expect(Utils.pathOr([], ['rootElement', 'children'], updatedRoot1)).toHaveLength(0)
    const updatedRoot2 = newTopLevelElements[1] as UtopiaJSXComponent
    expect(isUtopiaJSXComponent(updatedRoot2)).toBeTruthy()
    expect(Utils.pathOr([], ['rootElement', 'children'], updatedRoot2)).toHaveLength(1)
    const actual = Utils.path(['rootElement', 'children', 0], updatedRoot2)
    chaiExpect(actual).to.deep.equal(view1)
  })
  it('reparents from pinned to group with frame props updated', () => {
    const view1 = jsxElement(
      jsxElementName('bbb', []),
      jsxAttributesFromMap({
        layout: jsxAttributeNestedObjectSimple(
          jsxAttributesFromMap({
            bottom: jsxAttributeValue(50, emptyComments),
            right: jsxAttributeValue(50, emptyComments),
            width: jsxAttributeValue(100, emptyComments),
            height: jsxAttributeValue(100, emptyComments),
          }),
          emptyComments,
        ),
        'data-uid': jsxAttributeValue('bbb', emptyComments),
      }),
      [],
    )
    const group1 = group('ddd', [], -10, -10, 100, 100, 'Group')
    const root1 = view('aaa', [view1])
    const editor = testEditor(fileModel([root1, group1]))
    const groupFrame = canvasRectangle({ x: -10, y: -10, width: 100, height: 100 })

    const newEditor = editorMoveTemplate(
      TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'bbb']),
      TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'bbb']),
      canvasRectangle({ x: 10, y: 10, width: 100, height: 100 }),
      { type: 'front' },
      TP.instancePath(ScenePath1ForTestUiJsFile, ['ddd']),
      groupFrame,
      editor,
      LayoutSystem.Group,
    ).editor

    const newUiJsFile = getContentsTreeFileFromString(
      newEditor.projectContents,
      '/src/app.js',
    ) as TextFile
    expect(isTextFile(newUiJsFile)).toBeTruthy()
    expect(isParseSuccess(newUiJsFile.fileContents.parsed)).toBeTruthy()
    const newTopLevelElements: TopLevelElement[] = (newUiJsFile.fileContents.parsed as ParseSuccess)
      .topLevelElements
    const updatedGroup = newTopLevelElements[1] as UtopiaJSXComponent
    expect(isUtopiaJSXComponent(updatedGroup)).toBeTruthy()
    expect(Utils.pathOr([], ['rootElement', 'children'], updatedGroup)).toHaveLength(1)
    const actual: any = Utils.path(['rootElement', 'children', 0], updatedGroup)
    expect(getLayoutPropertyOr(undefined, 'PinnedLeft', right(actual.props))).toBeDefined()
    expect(getLayoutPropertyOr(undefined, 'PinnedTop', right(actual.props))).toBeDefined()
    expect(getLayoutPropertyOr(undefined, 'Width', right(actual.props))).toBeDefined()
    expect(getLayoutPropertyOr(undefined, 'Height', right(actual.props))).toBeDefined()
    expect(getLayoutPropertyOr(undefined, 'PinnedRight', right(actual.props))).not.toBeDefined()
    expect(getLayoutPropertyOr(undefined, 'PinnedBottom', right(actual.props))).not.toBeDefined()
  })
  xit('reparents from group to flex with frame props updated', () => {
    // FIXME This requires setting the special size measurements during tests
    const view1 = view('bbb', [], 50, 50, 100, 100)
    const group1 = group('ddd', [view1], 50, 50, 100, 100, 'Group')
    const flexView = jsxElement(
      jsxElementName('aaa', []),
      jsxAttributesFromMap({
        layout: jsxAttributeNestedObjectSimple(
          jsxAttributesFromMap({
            left: jsxAttributeValue(50, emptyComments),
            top: jsxAttributeValue(50, emptyComments),
            width: jsxAttributeValue(200, emptyComments),
            height: jsxAttributeValue(200, emptyComments),
          }),
          emptyComments,
        ),
        style: jsxAttributeNestedObjectSimple(
          jsxAttributesFromMap({
            display: jsxAttributeValue('flex', emptyComments),
          }),
          emptyComments,
        ),
        'data-uid': jsxAttributeValue('aaa', emptyComments),
      }),
      [],
    )
    const editor = testEditor(fileModel([flexView, group1]))

    const newEditor = editorMoveTemplate(
      TP.instancePath(ScenePath1ForTestUiJsFile, ['ddd', 'bbb']),
      TP.instancePath(ScenePath1ForTestUiJsFile, ['ddd', 'bbb']),
      canvasRectangle({ x: 50, y: 50, width: 100, height: 100 }),
      { type: 'front' },
      TP.instancePath(ScenePathForTestUiJsFile, ['aaa']),
      null,
      editor,
      null,
    ).editor

    const newUiJsFile = getContentsTreeFileFromString(
      newEditor.projectContents,
      '/src/app.js',
    ) as TextFile
    expect(isTextFile(newUiJsFile)).toBeTruthy()
    expect(isParseSuccess(newUiJsFile.fileContents.parsed)).toBeTruthy()
    const newTopLevelElements: TopLevelElement[] = (newUiJsFile.fileContents.parsed as ParseSuccess)
      .topLevelElements
    const updatedRoot1 = newTopLevelElements[0] as UtopiaJSXComponent
    expect(isUtopiaJSXComponent(updatedRoot1)).toBeTruthy()
    expect(Utils.pathOr([], ['rootElement', 'children'], updatedRoot1)).toHaveLength(1)
    const actual: any = Utils.path(['rootElement', 'children', 0], updatedRoot1)
    expect(getLayoutPropertyOr(undefined, 'flexBasis', right(actual.props))).toBeDefined()
    expect(getLayoutPropertyOr(undefined, 'FlexCrossBasis', right(actual.props))).toBeDefined()
    expect(getLayoutPropertyOr(undefined, 'PinnedLeft', right(actual.props))).not.toBeDefined()
    expect(getLayoutPropertyOr(undefined, 'PinnedTop', right(actual.props))).not.toBeDefined()
  })

  // TODO BALAZS FIX THIS BY MARCH 10 2020
  xit('reparents from group to pinned with frame props unchanged', () => {
    const view1 = jsxElement(
      jsxElementName('bbb', []),
      jsxAttributesFromMap({
        layout: jsxAttributeNestedObjectSimple(
          jsxAttributesFromMap({
            top: jsxAttributeValue(50, emptyComments),
            left: jsxAttributeValue(50, emptyComments),
            width: jsxAttributeValue(100, emptyComments),
            height: jsxAttributeValue(100, emptyComments),
          }),
          emptyComments,
        ),
        'data-uid': jsxAttributeValue('bbb', emptyComments),
      }),
      [],
    )
    const group1 = group('ddd', [view1], 50, 50, 100, 100, 'Group')
    const root1 = view('aaa', [], 0, 0, 200, 200)
    const editor = testEditor(fileModel([root1, group1]))

    const newEditor = editorMoveTemplate(
      TP.instancePath(ScenePathForTestUiJsFile, ['ddd', 'bbb']),
      TP.instancePath(ScenePath1ForTestUiJsFile, ['ddd', 'bbb']),
      canvasRectangle({ x: 50, y: 50, width: 100, height: 100 }),
      { type: 'front' },
      TP.instancePath(ScenePathForTestUiJsFile, ['aaa']),
      null,
      editor,
      null,
    ).editor

    const newUiJsFile = getContentsTreeFileFromString(
      newEditor.projectContents,
      '/src/app.js',
    ) as TextFile
    expect(isTextFile(newUiJsFile)).toBeTruthy()
    expect(isParseSuccess(newUiJsFile.fileContents.parsed)).toBeTruthy()
    const newTopLevelElements: TopLevelElement[] = (newUiJsFile.fileContents.parsed as ParseSuccess)
      .topLevelElements
    const updatedRoot1 = newTopLevelElements[0] as UtopiaJSXComponent
    expect(isUtopiaJSXComponent(updatedRoot1)).toBeTruthy()
    expect(Utils.pathOr([], ['rootElement', 'children'], updatedRoot1)).toHaveLength(1)
    const actual = Utils.path(['rootElement', 'children', 0], updatedRoot1) as JSXElement
    chaiExpect(actual).to.deep.equal(view1)
  })
})

describe('SWITCH_LAYOUT_SYSTEM', () => {
  const childElement = jsxElement(
    'View',
    jsxAttributesFromMap({
      'data-uid': jsxAttributeValue('bbb', emptyComments),
      style: jsxAttributeValue(
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
    jsxAttributesFromMap({
      'data-uid': jsxAttributeValue('aaa', emptyComments),
      style: jsxAttributeValue({ backgroundColor: '#FFFFFF' }, emptyComments),
      layout: jsxAttributeValue({ layoutSystem: 'pinSystem' }, emptyComments),
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
  const fileForUI = textFile(
    textFileContents(
      '',
      parseSuccess(
        sampleDefaultImports,
        [firstTopLevelElement],
        {},
        null,
        null,
        addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
      ),
      RevisionsState.BothMatch,
    ),
    null,
    0,
  )
  const scenePath = TP.scenePath([BakedInStoryboardUID, `scene-0`])
  const sceneTemplatePath = TP.instancePath([], [BakedInStoryboardUID, `scene-0`])
  const rootElementPath = TP.instancePath(TP.scenePath([BakedInStoryboardUID, 'scene-0']), ['aaa'])
  const childElementPath = TP.instancePath(TP.scenePath([BakedInStoryboardUID, 'scene-0']), [
    'aaa',
    'bbb',
  ])

  const componentMetadata: ComponentMetadata = {
    scenePath: scenePath,
    templatePath: sceneTemplatePath,
    component: 'App',
    globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
    sceneResizesContent: false,
    style: { width: 100, height: 100 },
    rootElements: [rootElementPath],
  }

  const rootElementMetadata: ElementInstanceMetadata = {
    templatePath: rootElementPath,
    element: right(firstTopLevelElement.rootElement),
    props: {
      'data-uid': 'aaa',
    },
    globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 100 }),
    localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 100 }),
    children: [childElementPath],
    componentInstance: false,
    isEmotionComponent: false,
    specialSizeMeasurements: emptySpecialSizeMeasurements,
    computedStyle: emptyComputedStyle,
    attributeMetadatada: emptyAttributeMetadatada,
  }

  const childElementMetadata: ElementInstanceMetadata = {
    templatePath: childElementPath,
    element: right(childElement),
    props: {
      'data-uid': 'bbb',
      style: {
        left: 5,
        top: 10,
        width: 200,
        height: 300,
      },
    },
    globalFrame: canvasRectangle({ x: 0, y: 0, width: 200, height: 300 }),
    localFrame: localRectangle({ x: 0, y: 0, width: 200, height: 300 }),
    children: [],
    componentInstance: false,
    isEmotionComponent: false,
    specialSizeMeasurements: emptySpecialSizeMeasurements,
    computedStyle: emptyComputedStyle,
    attributeMetadatada: emptyAttributeMetadatada,
  }

  const elementMetadataMap: ElementInstanceMetadataMap = {
    [TP.toString(rootElementPath)]: rootElementMetadata,
    [TP.toString(childElementPath)]: childElementMetadata,
  }

  const testEditorWithPins: EditorState = deepFreeze({
    ...createEditorState(NO_OP),
    projectContents: contentsToTree({
      '/src/app.js': fileForUI,
    }),
    selectedFile: {
      tab: openFileTab('/src/app.js'),
      initialCursorPosition: null,
    },
    jsxMetadataKILLME: jsxMetadata([componentMetadata], elementMetadataMap),
    selectedViews: [TP.instancePath(TP.scenePath([BakedInStoryboardUID, 'scene-0']), ['aaa'])],
  })
  it('switches from pins to flex correctly', () => {
    const switchActionToFlex = switchLayoutSystem('flex')
    const result = UPDATE_FNS.SWITCH_LAYOUT_SYSTEM(switchActionToFlex, testEditorWithPins)
    expect(
      getOpenUtopiaJSXComponentsFromState(result).map(clearTopLevelElementUniqueIDs),
    ).toMatchSnapshot()
  })
  it('switches from flex to pins correctly', () => {
    const switchActionToFlex = switchLayoutSystem('flex')
    let result = UPDATE_FNS.SWITCH_LAYOUT_SYSTEM(switchActionToFlex, testEditorWithPins)
    const switchActionToPins = switchLayoutSystem(LayoutSystem.PinSystem)
    result = UPDATE_FNS.SWITCH_LAYOUT_SYSTEM(switchActionToPins, result)
    expect(
      getOpenUtopiaJSXComponentsFromState(result).map(clearTopLevelElementUniqueIDs),
    ).toMatchSnapshot()
  })
})

describe('LOAD', () => {
  it('Parses all UIJS files and bins any previously stored parsed model data', () => {
    const firstUIJSFile = '/src/app.js'
    const secondUIJSFile = '/src/some/other/file.js'
    const initialFileContents: TextFileContents = textFileContents(
      sampleCode,
      unparsed,
      RevisionsState.BothMatch,
    )
    const loadedModel: PersistentModel = {
      appID: null,
      projectVersion: CURRENT_PROJECT_VERSION,
      projectContents: contentsToTree({
        [firstUIJSFile]: textFile(initialFileContents, null, 0),
        [secondUIJSFile]: textFile(initialFileContents, null, 0),
      }),
      exportsInfo: [],
      openFiles: [],
      selectedFile: null,
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
    }

    const action = {
      action: 'LOAD' as const,
      model: loadedModel,
      nodeModules: {},
      packageResult: {},
      codeResultCache: generateCodeResultCache(
        {},
        {},
        {},
        [],
        {},
        NO_OP,
        'full-build',
        null,
        false,
      ),
      title: '',
      projectId: '',
      storedState: null,
      safeMode: false,
    }

    const startingState = deepFreeze(createEditorState(NO_OP))
    const result = UPDATE_FNS.LOAD(action, startingState, NO_OP)
    const newFirstFileContents = (getContentsTreeFileFromString(
      result.projectContents,
      firstUIJSFile,
    ) as TextFile).fileContents
    expect(isParseSuccess(newFirstFileContents.parsed)).toBeTruthy()
    expect(newFirstFileContents.code).toEqual(initialFileContents.code)
    const newSecondFileContents = (getContentsTreeFileFromString(
      result.projectContents,
      secondUIJSFile,
    ) as TextFile).fileContents
    expect(isParseSuccess(newSecondFileContents.parsed)).toBeTruthy()
    expect(newSecondFileContents.code).toEqual(initialFileContents.code)
  })
})
