import { KrazyGeorgeTestUrl } from 'utopia-api'
import { createLayoutPropertyPathString } from '../layout/layout-helpers-new'
import { Imports, importAlias } from '../shared/project-file-types'
import {
  jsxAttributeFunctionCall,
  jsxAttributeNestedObjectSimple,
  jsxAttributeOtherJavaScript,
  jsxAttributeValue,
  jsxElement,
  utopiaJSXComponent,
  defaultPropsParam,
  jsxFragment,
} from '../shared/element-template'
import { addImport, emptyImports, mergeImports } from '../workers/common/project-file-utils'
import {
  convertScenesToUtopiaCanvasComponent,
  BakedInStoryboardUID,
  BakedInStoryboardVariableName,
} from './scene-utils'
import { scenePath } from '../shared/template-path'
import { defaultSceneElement } from '../../components/editor/defaults'
import { emptyComments } from '../workers/parser-printer/parser-printer-comments'

const sampleIncludedElementTypes: Array<string> = [
  'Ellipse',
  'Text',
  'Rectangle',
  'Storyboard',
  'View',
  'Image',
]

export const sampleDefaultImports = {
  react: {
    importedWithName: null,
    importedFromWithin: [],
    importedAs: 'React',
    comments: emptyComments,
  },
  'utopia-api': {
    importedWithName: null,
    importedFromWithin: [importAlias('UtopiaUtils')],
    importedAs: null,
    comments: emptyComments,
  },
}

export const sampleImportsForTests: Imports = mergeImports(
  sampleDefaultImports,
  sampleIncludedElementTypes.reduce<Imports>(
    (working, elementType) =>
      addImport('utopia-api', null, [importAlias(elementType)], null, emptyComments, working),
    emptyImports(),
  ),
)

const MainComponentForTestsName = 'Test'

const mainComponentForTests = utopiaJSXComponent(
  MainComponentForTestsName,
  true,
  defaultPropsParam,
  [],
  jsxElement(
    'View',
    {
      layout: jsxAttributeNestedObjectSimple({
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
      style: jsxAttributeValue({
        position: 'absolute',
        backgroundColor: 'lightgrey',
      }),
      'data-uid': jsxAttributeValue('aaa'),
    },
    [
      jsxFragment(
        [
          jsxElement(
            'Ellipse',
            {
              layout: jsxAttributeValue({
                left: 150,
                top: 25,
                width: 100,
                height: 100,
              }),
              style: jsxAttributeNestedObjectSimple({
                backgroundColor: jsxAttributeValue('lightgreen'),
              }),
              'data-uid': jsxAttributeValue('bbb'),
            },
            [],
          ),
          jsxElement(
            'Rectangle',
            {
              layout: jsxAttributeValue({
                left: 25,
                top: 25,
                width: 100,
                height: 100,
              }),
              style: jsxAttributeNestedObjectSimple({
                backgroundColor: jsxAttributeValue('orange'),
              }),
              'data-uid': jsxAttributeValue('ccc'),
            },
            [],
          ),
        ],
        false,
      ),
      jsxElement(
        'View',
        {
          layout: jsxAttributeValue({
            left: 150,
            top: 150,
            width: 100,
            height: 100,
            layoutSystem: 'group',
          }),
          style: jsxAttributeNestedObjectSimple({
            position: jsxAttributeValue('absolute'),
            backgroundColor: jsxAttributeValue('red'),
            boxShadow: jsxAttributeValue('10px 10px 8px #888888'),
          }),
          'data-uid': jsxAttributeValue('ddd'),
        },
        [
          jsxElement(
            'Rectangle',
            {
              layout: jsxAttributeValue({
                left: 220,
                top: 220,
                width: 20,
                height: 20,
              }),
              style: jsxAttributeNestedObjectSimple({
                backgroundColor: jsxAttributeValue('orange'),
              }),
              'data-uid': jsxAttributeValue('eee'),
            },
            [],
          ),
          jsxElement(
            'Rectangle',
            {
              layout: jsxAttributeValue({
                left: 90,
                top: 90,
                width: 100,
                height: 100,
              }),
              style: jsxAttributeNestedObjectSimple({
                backgroundColor: jsxAttributeValue('orange'),
              }),
              'data-uid': jsxAttributeValue('fff'),
            },
            [],
          ),
        ],
      ),
      jsxElement(
        'View',
        {
          layout: jsxAttributeNestedObjectSimple({
            left: jsxAttributeValue(50),
            top: jsxAttributeValue(250),
            width: jsxAttributeValue(100),
            height: jsxAttributeValue(200),
          }),
          style: jsxAttributeValue({
            position: 'absolute',
            backgroundColor: 'blue',
          }),
          'data-uid': jsxAttributeValue('ggg'),
        },
        [],
      ),
      jsxElement(
        'Text',
        {
          layout: jsxAttributeValue({
            left: 200,
            top: 200,
            width: 200,
            height: 100,
          }),
          text: jsxAttributeValue('TEST'),
          style: jsxAttributeValue({
            fontSize: 16,
          }),
          textSizing: jsxAttributeValue('auto'),
          'data-uid': jsxAttributeValue('hhh'),
        },
        [],
      ),
      jsxElement(
        'Image',
        {
          layout: jsxAttributeValue({
            left: 200,
            top: 300,
            width: 484 / 2,
            height: 426 / 2,
          }),
          src: jsxAttributeValue(KrazyGeorgeTestUrl),
          fillType: jsxAttributeValue('fill'),
          'data-uid': jsxAttributeValue('iii'),
        },
        [],
      ),
      jsxElement('MyComponent', { 'data-uid': jsxAttributeValue('mycomponent') }, []),
    ],
  ),
  null,
  false,
  emptyComments,
  emptyComments,
)

const scene = utopiaJSXComponent(
  'MyComponent',
  true,
  defaultPropsParam,
  [],
  jsxElement(
    'View',
    {
      style: jsxAttributeValue({ backgroundColor: 'green' }),
      'data-uid': jsxAttributeValue('jjj'),
    },
    [],
  ),
  null,
  false,
  emptyComments,
  emptyComments,
)

const Scene0UID = 'scene-0'
export const ScenePathForTestUiJsFile = scenePath([BakedInStoryboardUID, Scene0UID])
const Scene1UID = 'scene-1'
export const ScenePath1ForTestUiJsFile = scenePath([BakedInStoryboardUID, Scene1UID])

const Scene1 = defaultSceneElement(
  Scene0UID,
  MainComponentForTestsName,
  { left: 0, top: 0, width: 375, height: 812 },
  'Test Scene',
)
const Scene2 = defaultSceneElement(
  Scene1UID,
  null,
  { left: 500, top: 0, width: 375, height: 812 },
  'Test Scene 2',
)

const TestStoryboard = utopiaJSXComponent(
  BakedInStoryboardVariableName,
  false,
  null,
  [],
  jsxElement('Storyboard', { 'data-uid': jsxAttributeValue(BakedInStoryboardUID) }, [
    Scene1,
    Scene2,
  ]),
  null,
  false,
  emptyComments,
  emptyComments,
)

export const sampleJsxComponentWithScene = [mainComponentForTests, scene, TestStoryboard]

export const sampleJsxComponents = [mainComponentForTests, TestStoryboard]
