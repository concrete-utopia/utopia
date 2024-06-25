import { KrazyGeorgeTestUrl } from 'utopia-api/core'
import type { Imports } from '../shared/project-file-types'
import { importAlias } from '../shared/project-file-types'
import {
  jsxAttributeNestedObjectSimple,
  jsExpressionOtherJavaScript,
  jsExpressionValue,
  jsxElement,
  utopiaJSXComponent,
  defaultPropsParam,
  jsxFragment,
  jsxAttributesFromMap,
  emptyComments,
  jsOpaqueArbitraryStatement,
} from '../shared/element-template'
import { addImport, emptyImports, mergeImports } from '../workers/common/project-file-utils'
import { BakedInStoryboardUID, BakedInStoryboardVariableName } from './scene-utils'
import { defaultSceneElement } from '../../components/editor/defaults'
import { testStaticElementPath } from '../shared/element-path.test-utils'

const sampleIncludedElementTypes: Array<string> = [
  'Ellipse',
  'Image',
  'Rectangle',
  'Storyboard',
  'Text',
  'View',
  'Scene',
]

export const onlyImportReact: Imports = {
  react: {
    importedWithName: null,
    importedFromWithin: [],
    importedAs: 'React',
  },
}

export const sampleDefaultImports: Imports = mergeImports('/code.js', [], onlyImportReact, {
  'utopia-api': {
    importedWithName: null,
    importedFromWithin: [importAlias('UtopiaUtils')],
    importedAs: null,
  },
}).imports

export const sampleImportsForTests: Imports = mergeImports(
  '/code.js',
  [],
  sampleDefaultImports,
  sampleIncludedElementTypes.reduce<Imports>(
    (working, elementType) =>
      addImport('/code.js', [], 'utopia-api', null, [importAlias(elementType)], null, working)
        .imports,
    emptyImports(),
  ),
).imports

const MainComponentForTestsName = 'Test'

const mainComponentForTests = utopiaJSXComponent(
  MainComponentForTestsName,
  true,
  'var',
  'block',
  [],
  defaultPropsParam,
  [],
  jsxElement(
    'View',
    'aaa',
    jsxAttributesFromMap({
      layout: jsxAttributeNestedObjectSimple(
        jsxAttributesFromMap({
          left: jsExpressionOtherJavaScript(
            [],
            `props.style.left`,
            `props.style.left`,
            `return props.style.left`,
            ['props'],
            null,
            {},
            emptyComments,
          ),
          top: jsExpressionOtherJavaScript(
            [],
            `props.style.top`,
            `props.style.top`,
            `return props.style.top`,
            ['props'],
            null,
            {},
            emptyComments,
          ),
          width: jsExpressionOtherJavaScript(
            [],
            `props.style.width`,
            `props.style.width`,
            `return props.style.width`,
            ['props'],
            null,
            {},
            emptyComments,
          ),
          height: jsExpressionOtherJavaScript(
            [],
            `props.style.height`,
            `props.style.height`,
            `return props.style.height`,
            ['props'],
            null,
            {},
            emptyComments,
          ),
        }),
        emptyComments,
      ),
      style: jsExpressionValue(
        {
          position: 'absolute',
          backgroundColor: 'lightgrey',
        },
        emptyComments,
      ),
      'data-uid': jsExpressionValue('aaa', emptyComments),
    }),
    [
      jsxFragment(
        'mmm',
        [
          jsxElement(
            'Ellipse',
            'bbb',
            jsxAttributesFromMap({
              layout: jsExpressionValue(
                {
                  left: 150,
                  top: 25,
                  width: 100,
                  height: 100,
                },
                emptyComments,
              ),
              style: jsxAttributeNestedObjectSimple(
                jsxAttributesFromMap({
                  backgroundColor: jsExpressionValue('lightgreen', emptyComments),
                }),
                emptyComments,
              ),
              'data-uid': jsExpressionValue('bbb', emptyComments),
            }),
            [],
          ),
          jsxElement(
            'Rectangle',
            'ccc',
            jsxAttributesFromMap({
              layout: jsExpressionValue(
                {
                  left: 25,
                  top: 25,
                  width: 100,
                  height: 100,
                },
                emptyComments,
              ),
              style: jsxAttributeNestedObjectSimple(
                jsxAttributesFromMap({
                  backgroundColor: jsExpressionValue('orange', emptyComments),
                }),
                emptyComments,
              ),
              'data-uid': jsExpressionValue('ccc', emptyComments),
            }),
            [],
          ),
        ],
        true,
      ),
      jsxElement(
        'View',
        'ddd',
        jsxAttributesFromMap({
          layout: jsExpressionValue(
            {
              left: 150,
              top: 150,
              width: 100,
              height: 100,
              layoutSystem: 'group',
            },
            emptyComments,
          ),
          style: jsxAttributeNestedObjectSimple(
            jsxAttributesFromMap({
              position: jsExpressionValue('absolute', emptyComments),
              backgroundColor: jsExpressionValue('red', emptyComments),
              boxShadow: jsExpressionValue('10px 10px 8px #888888', emptyComments),
            }),
            emptyComments,
          ),
          'data-uid': jsExpressionValue('ddd', emptyComments),
        }),
        [
          jsxElement(
            'Rectangle',
            'eee',
            jsxAttributesFromMap({
              layout: jsExpressionValue(
                {
                  left: 220,
                  top: 220,
                  width: 20,
                  height: 20,
                },
                emptyComments,
              ),
              style: jsxAttributeNestedObjectSimple(
                jsxAttributesFromMap({
                  backgroundColor: jsExpressionValue('orange', emptyComments),
                }),
                emptyComments,
              ),
              'data-uid': jsExpressionValue('eee', emptyComments),
            }),
            [],
          ),
          jsxElement(
            'Rectangle',
            'fff',
            jsxAttributesFromMap({
              layout: jsExpressionValue(
                {
                  left: 90,
                  top: 90,
                  width: 100,
                  height: 100,
                },
                emptyComments,
              ),
              style: jsxAttributeNestedObjectSimple(
                jsxAttributesFromMap({
                  backgroundColor: jsExpressionValue('orange', emptyComments),
                }),
                emptyComments,
              ),
              'data-uid': jsExpressionValue('fff', emptyComments),
            }),
            [],
          ),
        ],
      ),
      jsxElement(
        'View',
        'ggg',
        jsxAttributesFromMap({
          layout: jsxAttributeNestedObjectSimple(
            jsxAttributesFromMap({
              left: jsExpressionValue(50, emptyComments),
              top: jsExpressionValue(250, emptyComments),
              width: jsExpressionValue(100, emptyComments),
              height: jsExpressionValue(200, emptyComments),
            }),
            emptyComments,
          ),
          style: jsExpressionValue(
            {
              position: 'absolute',
              backgroundColor: 'blue',
            },
            emptyComments,
          ),
          'data-uid': jsExpressionValue('ggg', emptyComments),
        }),
        [],
      ),
      jsxElement(
        'Text',
        'hhh',
        jsxAttributesFromMap({
          layout: jsExpressionValue(
            {
              left: 200,
              top: 200,
              width: 200,
              height: 100,
            },
            emptyComments,
          ),
          text: jsExpressionValue('TEST', emptyComments),
          style: jsExpressionValue(
            {
              fontSize: 16,
            },
            emptyComments,
          ),
          'data-uid': jsExpressionValue('hhh', emptyComments),
        }),
        [],
      ),
      jsxElement(
        'Image',
        'iii',
        jsxAttributesFromMap({
          layout: jsExpressionValue(
            {
              left: 200,
              top: 300,
              width: 484 / 2,
              height: 426 / 2,
            },
            emptyComments,
          ),
          src: jsExpressionValue(KrazyGeorgeTestUrl, emptyComments),
          fillType: jsExpressionValue('fill', emptyComments),
          'data-uid': jsExpressionValue('iii', emptyComments),
        }),
        [],
      ),
      jsxElement(
        'MyComponent',
        'mycomponent',
        jsxAttributesFromMap({ 'data-uid': jsExpressionValue('mycomponent', emptyComments) }),
        [],
      ),
    ],
  ),
  null,
  false,
  emptyComments,
)

const scene = utopiaJSXComponent(
  'MyComponent',
  true,
  'var',
  'block',
  [],
  defaultPropsParam,
  [],
  jsxElement(
    'View',
    'jjj',
    jsxAttributesFromMap({
      style: jsExpressionValue({ backgroundColor: 'green' }, emptyComments),
      'data-uid': jsExpressionValue('jjj', emptyComments),
    }),
    [],
  ),
  null,
  false,
  emptyComments,
)

export const TestScene0UID = 'scene-0'
export const TestMainComponentUID = 'main-component-0'
export const ElementPathForTestUiJsFile = [
  BakedInStoryboardUID,
  TestScene0UID,
  TestMainComponentUID,
]
export const ScenePathForTestUiJsFile = testStaticElementPath([ElementPathForTestUiJsFile])
export const Scene1UID = 'scene-1'
export const TestMainComponent1UID = 'main-component-1'
export const ElementPath1ForTestUiJsFile = [BakedInStoryboardUID, Scene1UID, TestMainComponent1UID]
export const ScenePath1ForTestUiJsFile = testStaticElementPath([ElementPath1ForTestUiJsFile])

const Scene1 = defaultSceneElement(
  TestScene0UID,
  { left: 0, top: 0, width: 375, height: 812 },
  'Test Scene',
  [
    jsxElement(
      MainComponentForTestsName,
      TestMainComponentUID,
      jsxAttributesFromMap({
        'data-uid': jsExpressionValue(TestMainComponentUID, emptyComments),
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
)
const Scene2 = defaultSceneElement(
  Scene1UID,
  { left: 500, top: 0, width: 375, height: 812 },
  'Test Scene 2',
  [],
)

const TestStoryboard = utopiaJSXComponent(
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
    jsxAttributesFromMap({ 'data-uid': jsExpressionValue(BakedInStoryboardUID, emptyComments) }),
    [Scene1, Scene2],
  ),
  null,
  false,
  emptyComments,
)

export const sampleJsxComponentWithScene = [mainComponentForTests, scene, TestStoryboard]

export const sampleJsxComponents = [mainComponentForTests, TestStoryboard]
