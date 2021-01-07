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

export const onlyImportReact: Imports = {
  react: {
    importedWithName: null,
    importedFromWithin: [],
    importedAs: 'React',
    comments: emptyComments,
  },
}

export const sampleDefaultImports: Imports = mergeImports(onlyImportReact, {
  'utopia-api': {
    importedWithName: null,
    importedFromWithin: [importAlias('UtopiaUtils')],
    importedAs: null,
    comments: emptyComments,
  },
})

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
  'var',
  'block',
  defaultPropsParam,
  [],
  jsxElement(
    'View',
    {
      layout: jsxAttributeNestedObjectSimple(
        {
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
        },
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
    },
    [
      jsxFragment(
        [
          jsxElement(
            'Ellipse',
            {
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
                {
                  backgroundColor: jsxAttributeValue('lightgreen', emptyComments),
                },
                emptyComments,
              ),
              'data-uid': jsxAttributeValue('bbb', emptyComments),
            },
            [],
          ),
          jsxElement(
            'Rectangle',
            {
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
                {
                  backgroundColor: jsxAttributeValue('orange', emptyComments),
                },
                emptyComments,
              ),
              'data-uid': jsxAttributeValue('ccc', emptyComments),
            },
            [],
          ),
        ],
        false,
      ),
      jsxElement(
        'View',
        {
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
            {
              position: jsxAttributeValue('absolute', emptyComments),
              backgroundColor: jsxAttributeValue('red', emptyComments),
              boxShadow: jsxAttributeValue('10px 10px 8px #888888', emptyComments),
            },
            emptyComments,
          ),
          'data-uid': jsxAttributeValue('ddd', emptyComments),
        },
        [
          jsxElement(
            'Rectangle',
            {
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
                {
                  backgroundColor: jsxAttributeValue('orange', emptyComments),
                },
                emptyComments,
              ),
              'data-uid': jsxAttributeValue('eee', emptyComments),
            },
            [],
          ),
          jsxElement(
            'Rectangle',
            {
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
                {
                  backgroundColor: jsxAttributeValue('orange', emptyComments),
                },
                emptyComments,
              ),
              'data-uid': jsxAttributeValue('fff', emptyComments),
            },
            [],
          ),
        ],
      ),
      jsxElement(
        'View',
        {
          layout: jsxAttributeNestedObjectSimple(
            {
              left: jsxAttributeValue(50, emptyComments),
              top: jsxAttributeValue(250, emptyComments),
              width: jsxAttributeValue(100, emptyComments),
              height: jsxAttributeValue(200, emptyComments),
            },
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
        },
        [],
      ),
      jsxElement(
        'Text',
        {
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
        },
        [],
      ),
      jsxElement(
        'Image',
        {
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
        },
        [],
      ),
      jsxElement(
        'MyComponent',
        { 'data-uid': jsxAttributeValue('mycomponent', emptyComments) },
        [],
      ),
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
  'var',
  'block',
  defaultPropsParam,
  [],
  jsxElement(
    'View',
    {
      style: jsxAttributeValue({ backgroundColor: 'green' }, emptyComments),
      'data-uid': jsxAttributeValue('jjj', emptyComments),
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
  'var',
  'block',
  null,
  [],
  jsxElement('Storyboard', { 'data-uid': jsxAttributeValue(BakedInStoryboardUID, emptyComments) }, [
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
