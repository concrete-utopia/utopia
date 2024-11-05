import * as Chai from 'chai'
import { contentsTreeOptic, walkContentsTree } from '../../../components/assets'
import { getLayoutPropertyOr } from '../../../core/layout/getLayoutProperty'
import {
  emptyTextFile,
  getFileWithCssImport,
  sampleCode,
} from '../../../core/model/new-project-files'
import {
  BakedInStoryboardUID,
  BakedInStoryboardVariableName,
} from '../../../core/model/scene-utils'
import {
  ScenePathForTestUiJsFile,
  sampleImportsForTests,
} from '../../../core/model/test-ui-js-file.test-utils'
import { mapEither, right } from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import type {
  ElementInstanceMetadataMap,
  JSXAttributes,
  JSXElement,
  TopLevelElement,
  UtopiaJSXComponent,
} from '../../../core/shared/element-template'
import {
  clearExpressionUniqueIDs,
  defaultPropsParam,
  elementInstanceMetadata,
  emptyAttributeMetadata,
  emptyComments,
  emptyComputedStyle,
  emptySpecialSizeMeasurements,
  isUtopiaJSXComponent,
  jsExpressionValue,
  jsxAttributeNestedObjectSimple,
  jsxAttributesFromMap,
  jsxElement,
  jsxElementName,
  unparsedCode,
  utopiaJSXComponent,
} from '../../../core/shared/element-template'
import { clearModifiableAttributeUniqueIDs } from '../../../core/shared/jsx-attributes'
import { getModifiableJSXAttributeAtPath } from '../../../core/shared/jsx-attribute-utils'
import type { CanvasRectangle, LocalRectangle } from '../../../core/shared/math-utils'
import { canvasRectangle, zeroRectangle } from '../../../core/shared/math-utils'
import { resolvedNpmDependency } from '../../../core/shared/npm-dependency-types'
import { filtered, fromField, fromTypeGuard } from '../../../core/shared/optics/optic-creators'
import { unsafeGet } from '../../../core/shared/optics/optic-utilities'
import type { Optic } from '../../../core/shared/optics/optics'
import { forceNotNull } from '../../../core/shared/optional-utils'
import type {
  ParseSuccess,
  RevisionsStateType,
  TextFile,
  TextFileContents,
} from '../../../core/shared/project-file-types'
import {
  RevisionsState,
  exportFunction,
  importAlias,
  importDetails,
  isParseSuccess,
  isTextFile,
  isUnparsed,
  parseSuccess,
  textFile,
  textFileContents,
  unparsed,
} from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import { NO_OP } from '../../../core/shared/utils'
import { DefaultThirdPartyControlDefinitions } from '../../../core/third-party/third-party-controls'
import { addImport } from '../../../core/workers/common/project-file-utils'
import { printCode, printCodeOptions } from '../../../core/workers/parser-printer/parser-printer'
import {
  complexDefaultProjectPreParsed,
  createModifiedProject,
  parseProjectContents,
} from '../../../sample-projects/sample-project-utils.test-utils'
import { styleStringInArray } from '../../../utils/common-constants'
import { deepFreeze } from '../../../utils/deep-freeze'
import Utils from '../../../utils/utils'
import {
  createFakeMetadataForComponents,
  setFeatureForUnitTestsUseInDescribeBlockOnly,
} from '../../../utils/utils.test-utils'
import {
  contentsToTree,
  getProjectFileByFilePath,
  walkContentsTreeForParseSuccess,
} from '../../assets'
import { getFrameChange } from '../../canvas/canvas-utils'
import { generateCodeResultCache } from '../../custom-code/code-file'
import { cssNumber } from '../../inspector/common/css-utils'
import { getComponentGroups, insertableComponent } from '../../shared/project-components'
import type { EditorState, PersistentModel } from '../store/editor-state'
import {
  StoryboardFilePath,
  createEditorState,
  defaultUserState,
  deriveState,
  editorModelFromPersistentModel,
  emptyCollaborativeEditingSupport,
  withUnderlyingTargetFromEditorState,
} from '../store/editor-state'
import { childInsertionPath } from '../store/insertion-path'
import { unpatchedCreateRemixDerivedDataMemo } from '../store/remix-derived-data'
import {
  insertInsertable,
  resetCanvas,
  setCanvasFrames,
  setFocusedElement,
  setProp_UNSAFE,
  updateFilePath,
  updateFromWorker,
  updateTopLevelElementsFromCollaborationUpdate,
  workerCodeAndParsedUpdate,
} from './action-creators'
import { UPDATE_FNS, replaceFilePath } from './actions'
import { CURRENT_PROJECT_VERSION } from './migrations/migrations'
import { getUidMappings } from '../../../core/model/get-uid-mappings'
import { simpleDefaultProject } from '../../../sample-projects/sample-project-utils'
import { InjectedCSSFilePrefix } from '../../../core/webpack-loaders/css-loader'
import { renderTestEditorWithModel } from '../../../components/canvas/ui-jsx.test-utils'
import { TailwindConfigPath } from '../../../core/tailwind/tailwind-config'

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
    [],
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
      [],
      'utopia-api',
      null,
      [importAlias('View'), importAlias('Scene'), importAlias('Storyboard')],
      null,
      sampleImportsForTests,
    ).imports,
    [
      utopiaJSXComponent(
        'MyView1',
        true,
        'var',
        'block',
        [],
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
    const newUiJsFile = getProjectFileByFilePath(
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

  const TailwindProject = createModifiedProject({
    [StoryboardFilePath]: `
  import React from 'react'
  import { Scene, Storyboard } from 'utopia-api'
  export var storyboard = (
    <Storyboard data-uid='sb'>
      <Scene
        id='scene'
        commentId='scene'
        data-uid='scene'
        className='w-[700px] h-[750px] absolute left-[200px] top-[128px]'
      >
        <div
          data-uid='mydiv'
          data-testid='mydiv'
          className='top-10 left-10 absolute flex flex-row'
        >
          <div className='bg-red-500 w-10 h-10' data-uid='child-1' />
          <div className='bg-red-500 w-10 h-10' data-uid='child-2' />
        </div>  
      </Scene>
    </Storyboard>
  )
  
  `,
    [TailwindConfigPath]: `
      const TailwindConfig = { }
      export default TailwindConfig
  `,
    'app.css': `
      @tailwind base;
      @tailwind components;
      @tailwind utilities;`,
  })

  describe('Tailwind', () => {
    setFeatureForUnitTestsUseInDescribeBlockOnly('Tailwind', true)

    it('updates `top`', async () => {
      const editor = await renderTestEditorWithModel(TailwindProject, 'await-first-dom-report')
      await editor.dispatch(
        [
          setProp_UNSAFE(
            EP.fromString('sb/scene/mydiv'),
            PP.create('style', 'top'),
            jsExpressionValue(42, emptyComments),
          ),
        ],
        true,
      )

      await editor.getDispatchFollowUpActionsFinished()

      const { className } = editor.renderedDOM.getByTestId('mydiv')
      expect(className).toEqual('top-[42px] left-10 absolute flex flex-row')
    })
  })
})

describe('RESET_CANVAS', () => {
  it('keeps css imports when resetting the canvas', async () => {
    const action = resetCanvas()
    const project = simpleDefaultProject({
      storyboardFile: getFileWithCssImport(),
      additionalFiles: { '/src/app.css': emptyTextFile() },
    })
    const updatedProjectContents = parseProjectContents(project.projectContents)

    const projectPreParsed = {
      ...project,
      projectContents: updatedProjectContents,
    }

    const editor = await renderTestEditorWithModel(projectPreParsed, 'await-first-dom-report')
    expect(document.getElementById(`${InjectedCSSFilePrefix}/src/app.css`)).not.toBeNull()
    const newEditor = UPDATE_FNS.RESET_CANVAS(action, editor.getEditorState().editor)
    expect(document.getElementById(`${InjectedCSSFilePrefix}/src/app.css`)).not.toBeNull()
  })
})

describe('SET_CANVAS_FRAMES', () => {
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
    const newEditor = UPDATE_FNS.SET_CANVAS_FRAMES(action, testEditor)
    const newUiJsFile = getProjectFileByFilePath(
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
        componentDescriptorErrors: {},
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
    const result = UPDATE_FNS.LOAD(action, startingState, NO_OP, emptyCollaborativeEditingSupport())
    const newFirstFileContents = (
      getProjectFileByFilePath(result.projectContents, firstUIJSFile) as TextFile
    ).fileContents
    expect(isUnparsed(newFirstFileContents.parsed)).toBeTruthy()
    expect(newFirstFileContents.code).toEqual(initialFileContents.code)
    const newSecondFileContents = (
      getProjectFileByFilePath(result.projectContents, secondUIJSFile) as TextFile
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
    const derivedState = deriveState(
      editorState,
      null,
      'unpatched',
      unpatchedCreateRemixDerivedDataMemo,
    )

    const insertableGroups = getComponentGroups(
      'insert',
      { ['utopia-api']: { status: 'loaded' } },
      { ['utopia-api']: DefaultThirdPartyControlDefinitions['utopia-api'] },
      editorState.projectContents,
      [resolvedNpmDependency('utopia-api', '0.5.1')],
      StoryboardFilePath,
    )
    const utopiaApiGroup = forceNotNull(
      'Group should exist.',
      insertableGroups.find((group) => {
        return (
          group.source.type === 'PROJECT_DEPENDENCY_GROUP' &&
          group.source.dependencyName === 'utopia-api'
        )
      }),
    )
    const menuInsertable = forceNotNull(
      'Component should exist.',
      utopiaApiGroup.insertableComponents.find((insertable) => {
        return insertable.name === 'View'
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
    const cardFile = getProjectFileByFilePath(actualResult.projectContents, '/src/card.js')
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
          import { View } from 'utopia-api'
          export var Card = (props) => {
            return (
              <div style={{ ...props.style }}>
                <div
                  data-testid='card-inner-div'
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
                <View
                  style={{
                    backgroundColor: '#aaaaaa33',
                    position: 'absolute',
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

  it('inserts an element into the project with the given values, also adding style props', () => {
    const project = complexDefaultProjectPreParsed()
    const editorState = editorModelFromPersistentModel(project, NO_OP)
    const derivedState = deriveState(
      editorState,
      null,
      'unpatched',
      unpatchedCreateRemixDerivedDataMemo,
    )

    const insertableGroups = getComponentGroups(
      'insert',
      { ['utopia-api']: { status: 'loaded' } },
      { ['utopia-api']: DefaultThirdPartyControlDefinitions['utopia-api'] },
      editorState.projectContents,
      [resolvedNpmDependency('utopia-api', '0.5.1')],
      StoryboardFilePath,
    )
    const utopiaApiGroup = forceNotNull(
      'Group should exist.',
      insertableGroups.find((group) => {
        return (
          group.source.type === 'PROJECT_DEPENDENCY_GROUP' &&
          group.source.dependencyName === 'utopia-api'
        )
      }),
    )
    const menuInsertable = forceNotNull(
      'Component should exist.',
      utopiaApiGroup.insertableComponents.find((insertable) => {
        return insertable.name === 'View'
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
    const cardFile = getProjectFileByFilePath(actualResult.projectContents, '/src/card.js')
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
          import { View } from 'utopia-api'
          export var Card = (props) => {
            return (
              <div style={{ ...props.style }}>
                <div
                  data-testid='card-inner-div'
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
                <View
                  style={{
                    backgroundColor: '#aaaaaa33',
                    position: 'absolute',
                    width: 100,
                    height: 100,
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

  it('inserts an element into the project with the given values, and duplicate name, also adding style props', () => {
    const project = complexDefaultProjectPreParsed('View')
    const editorState = editorModelFromPersistentModel(project, NO_OP)

    const insertableGroups = getComponentGroups(
      'insert',
      { ['utopia-api']: { status: 'loaded' } },
      { ['utopia-api']: DefaultThirdPartyControlDefinitions['utopia-api'] },
      editorState.projectContents,
      [resolvedNpmDependency('utopia-api', '0.5.1')],
      StoryboardFilePath,
    )
    const utopiaApiGroup = forceNotNull(
      'Group should exist.',
      insertableGroups.find((group) => {
        return (
          group.source.type === 'PROJECT_DEPENDENCY_GROUP' &&
          group.source.dependencyName === 'utopia-api'
        )
      }),
    )
    const viewInsertable = insertableComponent(
      {
        './test.js': importDetails(null, [importAlias('View')], null),
      },
      () => jsxElement('View', 'view', jsxAttributesFromMap({}), []),
      'View',
      [],
      null,
      { type: 'file-root' },
      null,
    )

    const targetPath = EP.elementPath([
      ['storyboard-entity', 'scene-1-entity', 'app-entity'],
      ['app-outer-div', 'card-instance'],
      ['card-outer-div'],
    ])

    const action = insertInsertable(
      childInsertionPath(targetPath),
      viewInsertable,
      'add-size',
      null,
    )

    const actualResult = UPDATE_FNS.INSERT_INSERTABLE(action, editorState)
    const cardFile = getProjectFileByFilePath(actualResult.projectContents, '/src/card.js')
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
          import { View } from 'non-existant-dummy-library'
          import { View as View_2 } from './test.js'
          export var Card = (props) => {
            return (
              <div style={{ ...props.style }}>
                <div
                  data-testid='card-inner-div'
                  style={{
                    position: 'absolute',
                    left: 0,
                    top: 0,
                    width: 50,
                    height: 50,
                    backgroundColor: 'red',
                  }}
                />
                <View
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
                <View_2 style={{ width: 100, height: 100 }} />
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
    const derivedState = deriveState(
      editorState,
      null,
      'unpatched',
      unpatchedCreateRemixDerivedDataMemo,
    )

    const insertableGroups = getComponentGroups(
      'insert',
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
    const cardFile = getProjectFileByFilePath(actualResult.projectContents, '/src/card.js')
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
                  data-testid='card-inner-div'
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
                  src='/editor/utopia-logo-white-fill.png?hash=nocommit'
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
    const derivedState = deriveState(
      editorState,
      null,
      'unpatched',
      unpatchedCreateRemixDerivedDataMemo,
    )

    const insertableGroups = getComponentGroups(
      'insert',
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
    const cardFile = getProjectFileByFilePath(actualResult.projectContents, '/src/card.js')
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
                  src='/editor/utopia-logo-white-fill.png?hash=nocommit'
                />
                <div
                  data-testid='card-inner-div'
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
    const derivedState = deriveState(editorState, null, 'unpatched', () => right(null))
    const pathToFocus = EP.fromString('storyboard-entity/scene-1-entity/app-entity:app-outer-div')
    const underlyingElement = forceNotNull(
      'Should be able to find this.',
      withUnderlyingTargetFromEditorState(pathToFocus, editorState, null, (_, element) => element),
    )
    const divElementMetadata = elementInstanceMetadata(
      pathToFocus,
      right(underlyingElement),
      zeroRectangle as CanvasRectangle,
      zeroRectangle as CanvasRectangle,
      false,
      false,
      emptySpecialSizeMeasurements,
      emptyComputedStyle,
      emptyAttributeMetadata,
      null,
      null,
      'not-a-conditional',
      null,
      null,
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
    const updatedEditorState = UPDATE_FNS.SET_FOCUSED_ELEMENT(action, editorState, derivedState)
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
      zeroRectangle as CanvasRectangle,
      false,
      false,
      emptySpecialSizeMeasurements,
      emptyComputedStyle,
      emptyAttributeMetadata,
      null,
      null,
      'not-a-conditional',
      null,
      null,
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
    const derivedState = deriveState(
      editorState,
      null,
      'unpatched',
      unpatchedCreateRemixDerivedDataMemo,
    )
    const updatedEditorState = UPDATE_FNS.SET_FOCUSED_ELEMENT(action, editorState, derivedState)
    expect(updatedEditorState.focusedElementPath).toEqual(pathToFocus)
  })
})

function textFileFromEditorStateOptic(filename: string): Optic<EditorState, TextFile> {
  return fromField<EditorState, 'projectContents'>('projectContents')
    .compose(contentsTreeOptic)
    .compose(filtered(({ fullPath }) => fullPath === filename))
    .compose(fromField('file'))
    .compose(fromTypeGuard(isTextFile))
}

function versionNumberOptic(filename: string): Optic<EditorState, number> {
  return textFileFromEditorStateOptic(filename).compose(fromField('versionNumber'))
}
function parsedTextFileOptic(filename: string): Optic<EditorState, ParseSuccess> {
  return textFileFromEditorStateOptic(filename)
    .compose(fromField('fileContents'))
    .compose(fromField('parsed'))
    .compose(fromTypeGuard(isParseSuccess))
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
    const versionNumberOfStoryboard = unsafeGet(
      versionNumberOptic(StoryboardFilePath),
      startingEditorState,
    )
    const versionNumberOfAppJS = unsafeGet(versionNumberOptic('/src/app.js'), startingEditorState)

    // Create the action and fire it.
    const updateToCheck = updateFromWorker([
      workerCodeAndParsedUpdate(
        StoryboardFilePath,
        '// Not relevant.',
        updatedStoryboardFile,
        versionNumberOfStoryboard + 1,
      ),
      workerCodeAndParsedUpdate(
        '/src/app.js',
        '// Not relevant.',
        updatedAppJSFile,
        versionNumberOfAppJS - 1,
      ),
    ])
    const updatedEditorState = UPDATE_FNS.UPDATE_FROM_WORKER(
      updateToCheck,
      startingEditorState,
      defaultUserState,
      NO_OP,
    )

    // Check that the model hasn't changed, because of the stale revised time.
    expect(updatedEditorState).toEqual({
      ...startingEditorState,
      previousParseOrPrintSkipped: true,
    })
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
    const versionNumberOfStoryboard = unsafeGet(
      versionNumberOptic(StoryboardFilePath),
      startingEditorState,
    )
    const versionNumberOfAppJS = unsafeGet(versionNumberOptic('/src/app.js'), startingEditorState)

    // Create the action and fire it.
    const updateToCheck = updateFromWorker([
      workerCodeAndParsedUpdate(
        StoryboardFilePath,
        '// Not relevant.',
        updatedStoryboardFile,
        versionNumberOfStoryboard + 1,
      ),
      workerCodeAndParsedUpdate(
        '/src/app.js',
        '// Not relevant.',
        updatedAppJSFile,
        versionNumberOfAppJS + 1,
      ),
    ])
    const updatedEditorState = UPDATE_FNS.UPDATE_FROM_WORKER(
      updateToCheck,
      startingEditorState,
      defaultUserState,
      NO_OP,
    )

    // Get the same values that we started with but from the updated editor state.
    const updatedStoryboardVersionNumberFromState = unsafeGet(
      versionNumberOptic(StoryboardFilePath),
      updatedEditorState,
    )
    const updatedAppJSVersionNumberFromState = unsafeGet(
      versionNumberOptic('/src/app.js'),
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
    expect(updatedStoryboardVersionNumberFromState).toBeGreaterThanOrEqual(
      versionNumberOfStoryboard,
    )
    expect(updatedAppJSVersionNumberFromState).toBeGreaterThanOrEqual(versionNumberOfAppJS)
    expect(updatedStoryboardFileFromState).toStrictEqual(updatedStoryboardFile)
    expect(updatedAppJSFileFromState).toStrictEqual(updatedAppJSFile)
  })
})

describe('UPDATE_TOP_LEVEL_ELEMENTS_FROM_COLLABORATION', () => {
  it('fixes up the uids if there are duplicates', () => {
    // Setup and getting some starting values.
    const project = complexDefaultProjectPreParsed()
    const startingEditorState = editorModelFromPersistentModel(project, NO_OP)
    const appJSFile = unsafeGet(parsedTextFileOptic('/src/app.js'), startingEditorState)
    // Doubling up the top level elements should definitely create some duplicates.
    const newTopLevelElements = [...appJSFile.topLevelElements, ...appJSFile.topLevelElements]
    const action = updateTopLevelElementsFromCollaborationUpdate('/src/app.js', newTopLevelElements)
    const updatedEditorState = UPDATE_FNS.UPDATE_TOP_LEVEL_ELEMENTS_FROM_COLLABORATION_UPDATE(
      action,
      startingEditorState,
    )
    const uniqueUIDsResult = getUidMappings(updatedEditorState.projectContents)
    expect(uniqueUIDsResult.duplicateIDs.size).toEqual(0)
  })
})

describe('replaceFilePath', () => {
  it('only marks the relevant files as parsed ahead', () => {
    const project = complexDefaultProjectPreParsed()
    const editorState = editorModelFromPersistentModel(project, NO_OP)
    const replaceResults = replaceFilePath(
      '/src/app.js',
      '/src/app2.js',
      editorState.projectContents,
      editorState.codeResultCache.curriedRequireFn,
    )
    if (replaceResults.type === 'SUCCESS') {
      expect(replaceResults.updatedFiles).toMatchInlineSnapshot(`
        Array [
          Object {
            "newPath": "/src/app2.js",
            "oldPath": "/src/app.js",
          },
        ]
      `)
      let textFilesAndRevisionStates: { [filename: string]: RevisionsStateType } = {}
      walkContentsTree(replaceResults.projectContents, (fullPath, file) => {
        if (isTextFile(file)) {
          textFilesAndRevisionStates[fullPath] = file.fileContents.revisionsState
        }
      })
      expect(textFilesAndRevisionStates).toMatchInlineSnapshot(`
        Object {
          "/package.json": "BOTH_MATCH",
          "/public/index.html": "BOTH_MATCH",
          "/src/app2.js": "PARSED_AHEAD",
          "/src/card.js": "BOTH_MATCH",
          "/src/index.js": "PARSED_AHEAD",
          "/utopia/storyboard.js": "PARSED_AHEAD",
        }
      `)
    } else {
      throw new Error('Should have returned a success.')
    }
  })
})
