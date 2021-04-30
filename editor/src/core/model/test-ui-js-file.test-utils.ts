import { KrazyGeorgeTestUrl } from 'utopia-api'
import { createLayoutPropertyPathString } from '../layout/layout-helpers-new'
import { Imports, importAlias } from '../shared/project-file-types'
import {
  jsxAttributeNestedObjectSimple,
  jsxAttributeOtherJavaScript,
  jsxAttributeValue,
  jsxElement,
  utopiaJSXComponent,
  defaultPropsParam,
  jsxFragment,
  jsxAttributesFromMap,
} from '../shared/element-template'
import { addImport, emptyImports, mergeImports } from '../workers/common/project-file-utils'
import { BakedInStoryboardUID, BakedInStoryboardVariableName } from './scene-utils'
import { defaultSceneElement } from '../../components/editor/defaults'
import { emptyComments } from '../workers/parser-printer/parser-printer-comments'
import { testStaticTemplatePath } from '../shared/template-path.test-utils'

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

export const sampleDefaultImports: Imports = mergeImports(onlyImportReact, {
  'utopia-api': {
    importedWithName: null,
    importedFromWithin: [importAlias('UtopiaUtils')],
    importedAs: null,
  },
})

export const sampleImportsForTests: Imports = mergeImports(
  sampleDefaultImports,
  sampleIncludedElementTypes.reduce<Imports>(
    (working, elementType) =>
      addImport('utopia-api', null, [importAlias(elementType)], null, working),
    emptyImports(),
  ),
)

const MainComponentForTestsName = 'Test'

const mainComponentForTests = utopiaJSXComponent(
  MainComponentForTestsName,
  true,
  'var',
  'block',
  defaultPropsParam,
  [],
  jsxElement(
    'View',
    jsxAttributesFromMap({
      layout: jsxAttributeNestedObjectSimple(
        jsxAttributesFromMap({
          left: jsxAttributeOtherJavaScript(
            `props.${createLayoutPropertyPathString('PinnedLeft')}`,
            `return props.${createLayoutPropertyPathString('PinnedLeft')}`,
            ['props'],
            null,
          ),
          top: jsxAttributeOtherJavaScript(
            `props.${createLayoutPropertyPathString('PinnedTop')}`,
            `return props.${createLayoutPropertyPathString('PinnedTop')}`,
            ['props'],
            null,
          ),
          width: jsxAttributeOtherJavaScript(
            `props.${createLayoutPropertyPathString('Width')}`,
            `return props.${createLayoutPropertyPathString('Width')}`,
            ['props'],
            null,
          ),
          height: jsxAttributeOtherJavaScript(
            `props.${createLayoutPropertyPathString('Height')}`,
            `return props.${createLayoutPropertyPathString('Height')}`,
            ['props'],
            null,
          ),
        }),
        emptyComments,
      ),
      style: jsxAttributeValue(
        {
          position: 'absolute',
          backgroundColor: 'lightgrey',
        },
        emptyComments,
      ),
      'data-uid': jsxAttributeValue('aaa', emptyComments),
    }),
    [
      jsxFragment(
        [
          jsxElement(
            'Ellipse',
            jsxAttributesFromMap({
              layout: jsxAttributeValue(
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
                  backgroundColor: jsxAttributeValue('lightgreen', emptyComments),
                }),
                emptyComments,
              ),
              'data-uid': jsxAttributeValue('bbb', emptyComments),
            }),
            [],
          ),
          jsxElement(
            'Rectangle',
            jsxAttributesFromMap({
              layout: jsxAttributeValue(
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
                  backgroundColor: jsxAttributeValue('orange', emptyComments),
                }),
                emptyComments,
              ),
              'data-uid': jsxAttributeValue('ccc', emptyComments),
            }),
            [],
          ),
        ],
        false,
      ),
      jsxElement(
        'View',
        jsxAttributesFromMap({
          layout: jsxAttributeValue(
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
              position: jsxAttributeValue('absolute', emptyComments),
              backgroundColor: jsxAttributeValue('red', emptyComments),
              boxShadow: jsxAttributeValue('10px 10px 8px #888888', emptyComments),
            }),
            emptyComments,
          ),
          'data-uid': jsxAttributeValue('ddd', emptyComments),
        }),
        [
          jsxElement(
            'Rectangle',
            jsxAttributesFromMap({
              layout: jsxAttributeValue(
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
                  backgroundColor: jsxAttributeValue('orange', emptyComments),
                }),
                emptyComments,
              ),
              'data-uid': jsxAttributeValue('eee', emptyComments),
            }),
            [],
          ),
          jsxElement(
            'Rectangle',
            jsxAttributesFromMap({
              layout: jsxAttributeValue(
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
                  backgroundColor: jsxAttributeValue('orange', emptyComments),
                }),
                emptyComments,
              ),
              'data-uid': jsxAttributeValue('fff', emptyComments),
            }),
            [],
          ),
        ],
      ),
      jsxElement(
        'View',
        jsxAttributesFromMap({
          layout: jsxAttributeNestedObjectSimple(
            jsxAttributesFromMap({
              left: jsxAttributeValue(50, emptyComments),
              top: jsxAttributeValue(250, emptyComments),
              width: jsxAttributeValue(100, emptyComments),
              height: jsxAttributeValue(200, emptyComments),
            }),
            emptyComments,
          ),
          style: jsxAttributeValue(
            {
              position: 'absolute',
              backgroundColor: 'blue',
            },
            emptyComments,
          ),
          'data-uid': jsxAttributeValue('ggg', emptyComments),
        }),
        [],
      ),
      jsxElement(
        'Text',
        jsxAttributesFromMap({
          layout: jsxAttributeValue(
            {
              left: 200,
              top: 200,
              width: 200,
              height: 100,
            },
            emptyComments,
          ),
          text: jsxAttributeValue('TEST', emptyComments),
          style: jsxAttributeValue(
            {
              fontSize: 16,
            },
            emptyComments,
          ),
          textSizing: jsxAttributeValue('auto', emptyComments),
          'data-uid': jsxAttributeValue('hhh', emptyComments),
        }),
        [],
      ),
      jsxElement(
        'Image',
        jsxAttributesFromMap({
          layout: jsxAttributeValue(
            {
              left: 200,
              top: 300,
              width: 484 / 2,
              height: 426 / 2,
            },
            emptyComments,
          ),
          src: jsxAttributeValue(KrazyGeorgeTestUrl, emptyComments),
          fillType: jsxAttributeValue('fill', emptyComments),
          'data-uid': jsxAttributeValue('iii', emptyComments),
        }),
        [],
      ),
      jsxElement(
        'MyComponent',
        jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('mycomponent', emptyComments) }),
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
  defaultPropsParam,
  [],
  jsxElement(
    'View',
    jsxAttributesFromMap({
      style: jsxAttributeValue({ backgroundColor: 'green' }, emptyComments),
      'data-uid': jsxAttributeValue('jjj', emptyComments),
    }),
    [],
  ),
  null,
  false,
  emptyComments,
)

export const TestScene0UID = 'scene-0'
export const TestMainComponentUID = 'main-component-0'
const ElementPathForTestUiJsFile = [BakedInStoryboardUID, TestScene0UID, TestMainComponentUID]
export const ScenePathForTestUiJsFile = testStaticTemplatePath([ElementPathForTestUiJsFile])
const Scene1UID = 'scene-1'
const TestMainComponent1UID = 'main-component-1'
const ElementPath1ForTestUiJsFile = [BakedInStoryboardUID, Scene1UID, TestMainComponent1UID]
export const ScenePath1ForTestUiJsFile = testStaticTemplatePath([ElementPath1ForTestUiJsFile])

const Scene1 = defaultSceneElement(
  TestScene0UID,
  { left: 0, top: 0, width: 375, height: 812 },
  'Test Scene',
  [
    jsxElement(
      MainComponentForTestsName,
      jsxAttributesFromMap({
        'data-uid': jsxAttributeValue(TestMainComponentUID, emptyComments),
        style: jsxAttributeValue(
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
  null,
  [],
  jsxElement(
    'Storyboard',
    jsxAttributesFromMap({ 'data-uid': jsxAttributeValue(BakedInStoryboardUID, emptyComments) }),
    [Scene1, Scene2],
  ),
  null,
  false,
  emptyComments,
)

export const sampleJsxComponentWithScene = [mainComponentForTests, scene, TestStoryboard]

export const sampleJsxComponents = [mainComponentForTests, TestStoryboard]
