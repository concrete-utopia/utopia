import { handleMessage, IncomingWorkerMessage } from './ts-worker'
import { LayoutSystem } from 'utopia-api'
import { RevisionsState } from '../../shared/project-file-types'
import { convertScenesToUtopiaCanvasComponent } from '../../model/scene-utils'

import * as SampleTypeDefinitions from './sample-type-definitions.json'
import { contentsToTree } from '../../../components/assets'

describe('Typescript worker builds the project', () => {
  it('initializing a new project', (done) => {
    handleMessage(SampleInitTSWorkerMessage, (msg) => {
      expect(msg).toMatchSnapshot()
      done()
    })
  }),
    it('updating a single file', (done) => {
      handleMessage(SampleUpdateFileMessage, (msg) => {
        expect(msg).toMatchSnapshot()
        done()
      })
    }),
    it('updating a single file with error', (done) => {
      handleMessage(SampleUpdateFileMessageWithError, (msg) => {
        expect(msg).toMatchSnapshot()
        done()
      })
    })
})

describe('Typescript worker applies the loaders', () => {
  it('applies the file loader for matching image files on init', (done) => {
    handleMessage(InitWorkerMessageNeedingLoaders, (msg) => {
      if (msg.type === 'build') {
        // Ensure no errors
        for (const builtFile in msg.buildResult) {
          if (msg.buildResult[builtFile].errors.length > 0) {
            fail(`Build errors found in built file ${builtFile}`)
          }
        }

        done()
      }
    })
  })

  it('applies the file loader for matching image files on single file update', (done) => {
    handleMessage(UpdateFileMessageNeedingLoaders, (msg) => {
      if (msg.type === 'updateprocessed') {
        done()
      }
    })
  })
})

const SampleInitTSWorkerMessage: IncomingWorkerMessage = {
  type: 'inittsworker',
  typeDefinitions: SampleTypeDefinitions,
  projectContents: contentsToTree({
    '/package.json': {
      type: 'CODE_FILE',
      fileContents:
        '{\n  "name": "Utopia Project",\n  "version": "0.1.0",\n  "utopia": {\n    "main-ui": "app.js",\n    "html": "index.html",\n    "js": "index.js"\n  },\n  "dependencies": {\n    "react": "16.8.6",\n    "@types/react": "16.8.17",\n    "csstype": "2.6.7",\n    "react-dom": "16.8.6",\n    "@types/react-dom": "16.8.4",\n    "utopia-api": "0.4.1",\n    "react-spring": "8.0.27"\n  }\n}',
      lastSavedContents: null,
    },
    '/app.js': {
      type: 'UI_JS_FILE',
      fileContents: {
        type: 'RIGHT',
        value: {
          imports: {
            react: {
              importedWithName: null,
              importedFromWithin: [],
              importedAs: 'React',
            },
            'utopia-api': {
              importedWithName: null,
              importedFromWithin: [
                { name: 'Ellipse', alias: 'Ellipse' },
                { name: 'UtopiaUtils', alias: 'UtopiaUtils' },
                { name: 'Image', alias: 'Image' },
                { name: 'NodeImplementations', alias: 'NodeImplementations' },
                { name: 'Rectangle', alias: 'Rectangle' },
                { name: 'Text', alias: 'Text' },
                { name: 'View', alias: 'View' },
                { name: 'jsx', alias: 'jsx' },
              ],
              importedAs: null,
            },
            uuiui: {
              importedWithName: null,
              importedFromWithin: [
                { name: 'ActionSheet', alias: 'ActionSheet' },
                { name: 'Avatar', alias: 'Avatar' },
                { name: 'Button', alias: 'Button' },
                { name: 'CheckboxInput', alias: 'CheckboxInput' },
                { name: 'ControlledTextArea', alias: 'ControlledTextArea' },
                { name: 'Dialog', alias: 'Dialog' },
                { name: 'FlexColumn', alias: 'FlexColumn' },
                { name: 'FlexRow', alias: 'FlexRow' },
                { name: 'FunctionIcons', alias: 'FunctionIcons' },
                { name: 'H1', alias: 'H1' },
                { name: 'H2', alias: 'H2' },
                { name: 'H3', alias: 'H3' },
                { name: 'Icn', alias: 'Icn' },
                { name: 'Icons', alias: 'Icons' },
                { name: 'InspectorSectionHeader', alias: 'InspectorSectionHeader' },
                { name: 'InspectorSubsectionHeader', alias: 'InspectorSubsectionHeader' },
                { name: 'Isolator', alias: 'Isolator' },
                { name: 'LargerIcons', alias: 'LargerIcons' },
                { name: 'MenuIcons', alias: 'MenuIcons' },
                { name: 'NumberInput', alias: 'NumberInput' },
                { name: 'OnClickOutsideHOC', alias: 'OnClickOutsideHOC' },
                { name: 'PopupList', alias: 'PopupList' },
                { name: 'ResizableFlexColumn', alias: 'ResizableFlexColumn' },
                { name: 'Section', alias: 'Section' },
                { name: 'SectionBodyArea', alias: 'SectionBodyArea' },
                { name: 'SectionTitleRow', alias: 'SectionTitleRow' },
                { name: 'StringInput', alias: 'StringInput' },
                { name: 'Subdued', alias: 'Subdued' },
                { name: 'TabComponent', alias: 'TabComponent' },
                { name: 'Title', alias: 'Title' },
                { name: 'TitledSection', alias: 'TitledSection' },
                { name: 'Tooltip', alias: 'Tooltip' },
                { name: 'UtopiaListItem', alias: 'UtopiaListItem' },
                { name: 'UtopiaListSelect', alias: 'UtopiaListSelect' },
                { name: 'colorTheme', alias: 'colorTheme' },
              ],
              importedAs: null,
            },
          },
          topLevelElements: [
            {
              type: 'UTOPIA_JSX_COMPONENT',
              name: 'App',
              isFunction: true,
              param: {
                type: 'PARAM',
                dotDotDotToken: false,
                boundParam: {
                  type: 'REGULAR_PARAM',
                  paramName: 'props',
                  defaultExpression: null,
                },
              },
              propsUsed: ['style'],
              rootElement: {
                type: 'JSX_ELEMENT',
                name: {
                  baseVariable: 'View',
                  propertyPath: {
                    propertyElements: [],
                  },
                },
                props: {
                  style: {
                    type: 'ATTRIBUTE_NESTED_OBJECT',
                    content: [
                      {
                        type: 'SPREAD_ASSIGNMENT',
                        value: {
                          type: 'ATTRIBUTE_OTHER_JAVASCRIPT',
                          javascript: 'props.style',
                          transpiledJavascript: 'return props.style;',
                          definedElsewhere: ['props'],
                          sourceMap: {
                            version: 3,
                            sources: ['code.tsx'],
                            names: ['props', '.', 'style', '{', '('],
                            mappings: 'OAmEoBA,KAAKC,CAACC,KAANF,IAAeG,EAAhBC',
                            file: 'code.tsx',
                            sourcesContent: [
                              "/** @jsx jsx */\nimport * as React from 'react'\nimport {\n  Ellipse,\n  HelperFunctions,\n  Image,\n  NodeImplementations,\n  Rectangle,\n  Text,\n  View,\n  jsx,\n} from 'utopia-api'\nimport {\n  colorTheme,\n  Button,\n  Dialog,\n  Icn,\n  Icons,\n  LargerIcons,\n  FunctionIcons,\n  MenuIcons,\n  Isolator,\n  TabComponent,\n  Tooltip,\n  ActionSheet,\n  Avatar,\n  ControlledTextArea,\n  Title,\n  H1,\n  H2,\n  H3,\n  Subdued,\n  InspectorSectionHeader,\n  InspectorSubsectionHeader,\n  FlexColumn,\n  FlexRow,\n  ResizableFlexColumn,\n  PopupList,\n  Section,\n  TitledSection,\n  SectionTitleRow,\n  SectionBodyArea,\n  UtopiaListSelect,\n  UtopiaListItem,\n  CheckboxInput,\n  NumberInput,\n  StringInput,\n  OnClickOutsideHOC,\n} from 'uuiui'\n\nexport var canvasMetadata = {\n  scenes: [\n    {\n      component: 'App',\n      frame: { height: 812, left: 0, width: 375, top: 0 },\n      props: { layout: { top: 0, left: 0, bottom: 0, right: 0 } },\n      container: { layoutSystem: 'pinSystem' },\n    },\n  ],\n  elementMetadata: {},\n}\n\nexport var App = (props) => {\n  return (\n    <View\n      style={{ ...props.style, backgroundColor: colorTheme.white.value }}\n      layout={{ layoutSystem: 'pinSystem' }}\n      data-uid={'aaa'}\n    ></View>\n  )\n}\n\n",
                            ],
                          },
                          uniqueID: '00df44d9-e76c-47d8-b832-60ee0fb3a5bc',
                        },
                      },
                      {
                        type: 'PROPERTY_ASSIGNMENT',
                        key: 'backgroundColor',
                        value: {
                          type: 'ATTRIBUTE_OTHER_JAVASCRIPT',
                          javascript: 'colorTheme.white.value',
                          transpiledJavascript: 'return colorTheme.white.value;',
                          definedElsewhere: ['colorTheme'],
                          sourceMap: {
                            version: 3,
                            sources: ['code.tsx'],
                            names: ['colorTheme', '.', 'white', 'value', ' '],
                            mappings: 'OAmEyDA,UAAUC,CAACC,KAAXF,CAAiBG,KAAlBC',
                            file: 'code.tsx',
                            sourcesContent: [
                              "/** @jsx jsx */\nimport * as React from 'react'\nimport {\n  Ellipse,\n  HelperFunctions,\n  Image,\n  NodeImplementations,\n  Rectangle,\n  Text,\n  View,\n  jsx,\n} from 'utopia-api'\nimport {\n  colorTheme,\n  Button,\n  Dialog,\n  Icn,\n  Icons,\n  LargerIcons,\n  FunctionIcons,\n  MenuIcons,\n  Isolator,\n  TabComponent,\n  Tooltip,\n  ActionSheet,\n  Avatar,\n  ControlledTextArea,\n  Title,\n  H1,\n  H2,\n  H3,\n  Subdued,\n  InspectorSectionHeader,\n  InspectorSubsectionHeader,\n  FlexColumn,\n  FlexRow,\n  ResizableFlexColumn,\n  PopupList,\n  Section,\n  TitledSection,\n  SectionTitleRow,\n  SectionBodyArea,\n  UtopiaListSelect,\n  UtopiaListItem,\n  CheckboxInput,\n  NumberInput,\n  StringInput,\n  OnClickOutsideHOC,\n} from 'uuiui'\n\nexport var canvasMetadata = {\n  scenes: [\n    {\n      component: 'App',\n      frame: { height: 812, left: 0, width: 375, top: 0 },\n      props: { layout: { top: 0, left: 0, bottom: 0, right: 0 } },\n      container: { layoutSystem: 'pinSystem' },\n    },\n  ],\n  elementMetadata: {},\n}\n\nexport var App = (props) => {\n  return (\n    <View\n      style={{ ...props.style, backgroundColor: colorTheme.white.value }}\n      layout={{ layoutSystem: 'pinSystem' }}\n      data-uid={'aaa'}\n    ></View>\n  )\n}\n\n",
                            ],
                          },
                          uniqueID: '6173f632-5d44-42bc-86a2-dc857c77d767',
                        },
                      },
                    ],
                  },
                  layout: {
                    type: 'ATTRIBUTE_VALUE',
                    value: {
                      layoutSystem: LayoutSystem.PinSystem,
                    },
                  },
                  'data-uid': {
                    type: 'ATTRIBUTE_VALUE',
                    value: 'aaa',
                  },
                },
                children: [],
              },
              arbitraryJSBlock: null,
            },
            convertScenesToUtopiaCanvasComponent([
              {
                uid: 'scene-aaa',
                component: 'App',
                props: {
                  layout: {
                    top: 0,
                    left: 0,
                    bottom: 0,
                    right: 0,
                  },
                },
                frame: {
                  left: 0,
                  top: 0,
                  width: 375,
                  height: 812,
                },
                container: {
                  layoutSystem: LayoutSystem.PinSystem,
                },
              },
            ]),
          ],
          code:
            "/** @jsx jsx */\nimport * as React from 'react'\nimport {\n  Ellipse,\n  HelperFunctions,\n  Image,\n  NodeImplementations,\n  Rectangle,\n  Text,\n  View,\n  jsx,\n} from 'utopia-api'\nimport {\n  colorTheme,\n  Button,\n  Dialog,\n  Icn,\n  Icons,\n  LargerIcons,\n  FunctionIcons,\n  MenuIcons,\n  Isolator,\n  TabComponent,\n  Tooltip,\n  ActionSheet,\n  Avatar,\n  ControlledTextArea,\n  Title,\n  H1,\n  H2,\n  H3,\n  Subdued,\n  InspectorSectionHeader,\n  InspectorSubsectionHeader,\n  FlexColumn,\n  FlexRow,\n  ResizableFlexColumn,\n  PopupList,\n  Section,\n  TitledSection,\n  SectionTitleRow,\n  SectionBodyArea,\n  UtopiaListSelect,\n  UtopiaListItem,\n  CheckboxInput,\n  NumberInput,\n  StringInput,\n  OnClickOutsideHOC,\n} from 'uuiui'\n\nexport var canvasMetadata = {\n  scenes: [\n    {\n      component: 'App',\n      frame: { height: 812, left: 0, width: 375, top: 0 },\n      props: { layout: { top: 0, left: 0, bottom: 0, right: 0 } },\n      container: { layoutSystem: 'pinSystem' },\n    },\n  ],\n  elementMetadata: {},\n}\n\nexport var App = (props) => {\n  return (\n    <View\n      style={{ ...props.style, backgroundColor: colorTheme.white.value }}\n      layout={{ layoutSystem: 'pinSystem' }}\n      data-uid={'aaa'}\n    ></View>\n  )\n}\n\n",
          highlightBounds: {
            aaa: {
              startCol: 4,
              startLine: 66,
              endCol: 12,
              endLine: 70,
              uid: 'aaa',
            },
          },
          jsxFactoryFunction: 'jsx',
          combinedTopLevelArbitraryBlock: null,
        },
      },
      lastSavedContents: null,
      revisionsState: RevisionsState.BothMatch,
      lastRevisedTime: 1585568960067,
    },
    '/src': {
      type: 'DIRECTORY',
    },
    '/src/components.js': {
      type: 'CODE_FILE',
      fileContents:
        "// component library\nimport * as React from 'react'\nimport { Text, View } from 'utopia-api'\n\nexport default (props) => (\n  <View layout={props.layout} style={props.style} onMouseDown={props.onMouseDown}>\n    <Text\n      style={{ fontSize: 16, textAlign: 'center' }}\n      text={props.text}\n      layout={{\n        left: 0,\n        top: 10,\n        width: '100%',\n        height: '100%',\n      }}\n      textSizing={'fixed'}\n    />\n  </View>\n)\n\nexport const LABEL = 'press me! 😉'\n\nexport const ComponentWithProps = (props) => {\n  return (\n    <div\n      style={{\n        ...props.style,\n        backgroundColor: props.pink ? 'hotpink' : 'transparent',\n        whiteSpace: 'normal',\n      }}\n    >\n      {(props.text + ' ').repeat(props.num)}\n    </div>\n  )\n}\n\nComponentWithProps.propertyControls = {\n  text: {\n    type: 'string',\n    title: 'Title',\n    defaultValue: 'Change me',\n  },\n  num: {\n    type: 'number',\n    title: 'amount',\n    defaultValue: 2,\n  },\n  pink: {\n    type: 'boolean',\n    title: 'Enabled',\n    defaultValue: true,\n  },\n}\n\n",
      lastSavedContents: null,
    },
    '/assets': {
      type: 'DIRECTORY',
    },
    '/public': {
      type: 'DIRECTORY',
    },
    '/src/index.js': {
      type: 'CODE_FILE',
      fileContents:
        'import * as React from "react";\nimport * as ReactDOM from "react-dom";\nimport { App } from "../app";\n\nconst root = document.getElementById("root");\nif (root != null) {\nReactDOM.render(<App />, root);\n}',
      lastSavedContents: null,
    },
    '/public/index.html': {
      type: 'CODE_FILE',
      fileContents:
        '<!DOCTYPE html>\n<html lang="en">\n<head>\n<meta charset="utf-8">\n<title>Utopia React App</title>\n</head>\n<body>\n<div id="root"></div>\n</body>\n</html>',
      lastSavedContents: null,
    },
  }),
  buildOrParsePrint: 'build',
  jobID: '9897a53d_15b6_400d_be5f_ca5d66e47087',
}

const SampleUpdateFileMessage: IncomingWorkerMessage = {
  type: 'updatefile',
  filename: '/src/components.js',
  content: {
    type: 'CODE_FILE',
    fileContents:
      "// component library\nimport * as React from 'react'\nimport { Text, View } from 'utopia-api'\n\n\nexport default (props) => (\n  <View layout={props.layout} style={props.style} onMouseDown={props.onMouseDown}>\n    <Text\n      style={{ fontSize: 16, textAlign: 'center' }}\n      text={props.text}\n      layout={{\n        left: 0,\n        top: 10,\n        width: '100%',\n        height: '100%',\n      }}\n      textSizing={'fixed'}\n    />\n  </View>\n)\n\nexport const LABEL = 'press me! 😉'\n\nexport const ComponentWithProps = (props) => {\n  return (\n    <div\n      style={{\n        ...props.style,\n        backgroundColor: props.pink ? 'hotpink' : 'transparent',\n        whiteSpace: 'normal',\n      }}\n    >\n      {(props.text + ' ').repeat(props.num)}\n    </div>\n  )\n}\n\nComponentWithProps.propertyControls = {\n  text: {\n    type: 'string',\n    title: 'Title',\n    defaultValue: 'Change me',\n  },\n  num: {\n    type: 'number',\n    title: 'amount',\n    defaultValue: 2,\n  },\n  pink: {\n    type: 'boolean',\n    title: 'Enabled',\n    defaultValue: true,\n  },\n}\n",
    lastSavedContents:
      "// component library\nimport * as React from 'react'\nimport { Text, View } from 'utopia-api'\n\nexport default (props) => (\n  <View layout={props.layout} style={props.style} onMouseDown={props.onMouseDown}>\n    <Text\n      style={{ fontSize: 16, textAlign: 'center' }}\n      text={props.text}\n      layout={{\n        left: 0,\n        top: 10,\n        width: '100%',\n        height: '100%',\n      }}\n      textSizing={'fixed'}\n    />\n  </View>\n)\n\nexport const LABEL = 'press me! 😉'\n\nexport const ComponentWithProps = (props) => {\n  return (\n    <div\n      style={{\n        ...props.style,\n        backgroundColor: props.pink ? 'hotpink' : 'transparent',\n        whiteSpace: 'normal',\n      }}\n    >\n      {(props.text + ' ').repeat(props.num)}\n    </div>\n  )\n}\n\nComponentWithProps.propertyControls = {\n  text: {\n    type: 'string',\n    title: 'Title',\n    defaultValue: 'Change me',\n  },\n  num: {\n    type: 'number',\n    title: 'amount',\n    defaultValue: 2,\n  },\n  pink: {\n    type: 'boolean',\n    title: 'Enabled',\n    defaultValue: true,\n  },\n}\n",
  },
  jobID: '689b7b1b_d5ed_4876_ba62_bc66e2096bf6',
}

const SampleUpdateFileMessageWithError: IncomingWorkerMessage = {
  type: 'updatefile',
  filename: '/src/components.js',
  content: {
    type: 'CODE_FILE',
    fileContents:
      "// component library\nimport * as React from 'react'\nimport { Text, View } from 'utopia-api'\n\n+\nexport default (props) => (\n  <View layout={props.layout} style={props.style} onMouseDown={props.onMouseDown}>\n    <Text\n      style={{ fontSize: 16, textAlign: 'center' }}\n      text={props.text}\n      layout={{\n        left: 0,\n        top: 10,\n        width: '100%',\n        height: '100%',\n      }}\n      textSizing={'fixed'}\n    />\n  </View>\n)\n\nexport const LABEL = 'press me! 😉'\n\nexport const ComponentWithProps = (props) => {\n  return (\n    <div\n      style={{\n        ...props.style,\n        backgroundColor: props.pink ? 'hotpink' : 'transparent',\n        whiteSpace: 'normal',\n      }}\n    >\n      {(props.text + ' ').repeat(props.num)}\n    </div>\n  )\n}\n\nComponentWithProps.propertyControls = {\n  text: {\n    type: 'string',\n    title: 'Title',\n    defaultValue: 'Change me',\n  },\n  num: {\n    type: 'number',\n    title: 'amount',\n    defaultValue: 2,\n  },\n  pink: {\n    type: 'boolean',\n    title: 'Enabled',\n    defaultValue: true,\n  },\n}\n",
    lastSavedContents:
      "// component library\nimport * as React from 'react'\nimport { Text, View } from 'utopia-api'\n\n\nexport default (props) => (\n  <View layout={props.layout} style={props.style} onMouseDown={props.onMouseDown}>\n    <Text\n      style={{ fontSize: 16, textAlign: 'center' }}\n      text={props.text}\n      layout={{\n        left: 0,\n        top: 10,\n        width: '100%',\n        height: '100%',\n      }}\n      textSizing={'fixed'}\n    />\n  </View>\n)\n\nexport const LABEL = 'press me! 😉'\n\nexport const ComponentWithProps = (props) => {\n  return (\n    <div\n      style={{\n        ...props.style,\n        backgroundColor: props.pink ? 'hotpink' : 'transparent',\n        whiteSpace: 'normal',\n      }}\n    >\n      {(props.text + ' ').repeat(props.num)}\n    </div>\n  )\n}\n\nComponentWithProps.propertyControls = {\n  text: {\n    type: 'string',\n    title: 'Title',\n    defaultValue: 'Change me',\n  },\n  num: {\n    type: 'number',\n    title: 'amount',\n    defaultValue: 2,\n  },\n  pink: {\n    type: 'boolean',\n    title: 'Enabled',\n    defaultValue: true,\n  },\n}\n",
  },
  jobID: 'ffcc378d_6bc8_4635_9fd9_e54565241f27',
}

const InitWorkerMessageNeedingLoaders: IncomingWorkerMessage = {
  type: 'inittsworker',
  typeDefinitions: SampleTypeDefinitions,
  projectContents: contentsToTree({
    '/package.json': {
      type: 'CODE_FILE',
      fileContents:
        '{\n  "name": "Utopia Project",\n  "version": "0.1.0",\n  "utopia": {\n    "main-ui": "app.js",\n    "html": "index.html",\n    "js": "index.js"\n  },\n  "dependencies": {\n    "react": "16.8.6",\n    "@types/react": "16.8.17",\n    "csstype": "2.6.7",\n    "react-dom": "16.8.6",\n    "@types/react-dom": "16.8.4",\n    "utopia-api": "0.4.1",\n    "react-spring": "8.0.27"\n  }\n}',
      lastSavedContents: null,
    },
    '/app.js': {
      type: 'CODE_FILE',
      fileContents:
        "/** @jsx jsx */\nimport * as React from 'react'\nimport { Scene, Storyboard, jsx } from 'utopia-api'\nimport icon from './icon.png'\nexport var App = (props) => {\n  return (\n    <div\n      style={{ width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}\n      layout={{ layoutSystem: 'pinSystem' }}\n    >\n      <img src={icon} />\n    </div>\n  )\n}\nexport var storyboard = (\n  <Storyboard layout={{ layoutSystem: 'pinSystem' }}>\n    <Scene\n      component={App}\n      props={{}}\n      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}\n    />\n  </Storyboard>\n)\n",
      lastSavedContents: null,
    },
    '/src': {
      type: 'DIRECTORY',
    },
    '/assets': {
      type: 'DIRECTORY',
    },
    '/public': {
      type: 'DIRECTORY',
    },
    '/src/icon.png': {
      type: 'ASSET_FILE',
    },
    '/src/index.js': {
      type: 'CODE_FILE',
      fileContents:
        'import * as React from "react";\nimport * as ReactDOM from "react-dom";\nimport { App } from "../app";\n\nconst root = document.getElementById("root");\nif (root != null) {\nReactDOM.render(<App />, root);\n}',
      lastSavedContents: null,
    },
    '/public/index.html': {
      type: 'CODE_FILE',
      fileContents:
        '<!DOCTYPE html>\n<html lang="en">\n<head>\n<meta charset="utf-8">\n<title>Utopia React App</title>\n</head>\n<body>\n<div id="root"></div>\n</body>\n</html>',
      lastSavedContents: null,
    },
  }),
  buildOrParsePrint: 'build',
  jobID: 'LOADER_INIT_JOB_ID',
}

const UpdateFileMessageNeedingLoaders: IncomingWorkerMessage = {
  type: 'updatefile',
  filename: '/src/icon.png',
  content: {
    type: 'ASSET_FILE',
  },
  jobID: 'LOADER_UPDATE_JOB_ID',
}
