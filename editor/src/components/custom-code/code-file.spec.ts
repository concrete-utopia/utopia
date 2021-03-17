import {
  generateCodeResultCache,
  normalisePathEndsAtDependency,
  normalisePathError,
  normalisePathSuccess,
  normalisePathToUnderlyingTarget,
  normalisePathUnableToProceed,
} from './code-file'
import {
  ExportsInfo,
  MultiFileBuildResult,
  EmitFileResult,
  DefaultLanguageServiceCompilerOptions,
  configureLanguageService,
  emitFile,
  FileVersion,
  writeFileForTests,
  initBrowserFS,
} from '../../core/workers/ts/ts-worker'
import { NO_OP, fastForEach } from '../../core/shared/utils'
import {
  NodeModules,
  esCodeFile,
  ProjectContents,
  RevisionsState,
  textFile,
  textFileContents,
  unparsed,
  TextFile,
  isTextFile,
  codeFile,
  ProjectFile,
  StaticInstancePath,
} from '../../core/shared/project-file-types'
import { MapLike } from 'typescript'
import { objectMap } from '../../core/shared/object-utils'
import {
  getDefaultUIJsFile,
  getSamplePreviewFile,
  getSamplePreviewHTMLFile,
} from '../../core/model/new-project-files'
import { directory, updateFileContents } from '../../core/model/project-file-utils'
import { contentsToTree, getContentsTreeFileFromString, ProjectContentTreeRoot } from '../assets'
import {
  PersistentModel,
  DefaultPackageJson,
  StoryboardFilePath,
  persistentModelForProjectContents,
} from '../editor/store/editor-state'
import { lintAndParse, parseCode } from '../../core/workers/parser-printer/parser-printer'
import * as TP from '../../core/shared/template-path'

function transpileCode(
  rootFilenames: Array<string>,
  files: MapLike<string>,
): MapLike<EmitFileResult> {
  initBrowserFS({}, {})
  const fileVersions: MapLike<FileVersion> = objectMap((_) => {
    return { versionNr: 1, asStringCached: null }
  }, files)
  const services = configureLanguageService(
    rootFilenames,
    fileVersions,
    DefaultLanguageServiceCompilerOptions,
  )
  fastForEach(Object.keys(files), (filename) => {
    const fileContents = files[filename]
    writeFileForTests(filename, fileContents)
  })
  return objectMap((_, filename) => emitFile(services, `${filename}`), files)
}

const SampleSingleFileBuildResult = transpileCode(['/app.js'], {
  '/app.js':
    "/** @jsx jsx */\nimport * as React from 'react'\nimport {\n  Ellipse,\n  HelperFunctions,\n  Image,\n  NodeImplementations,\n  Rectangle,\n  Text,\n  View,\n  jsx,\n} from 'utopia-api'\nimport {\n  colorTheme,\n  Button,\n  Dialog,\n  Icn,\n  Icons,\n  LargerIcons,\n  FunctionIcons,\n  MenuIcons,\n  Isolator,\n  TabComponent,\n  Tooltip,\n  ActionSheet,\n  Avatar,\n  ControlledTextArea,\n  Title,\n  H1,\n  H2,\n  H3,\n  Subdued,\n  InspectorSectionHeader,\n  InspectorSubsectionHeader,\n  FlexColumn,\n  FlexRow,\n  ResizableFlexColumn,\n  PopupList,\n  Section,\n  SectionTitleRow,\n  SectionBodyArea,\n  UtopiaListSelect,\n  UtopiaListItem,\n  CheckboxInput,\n  NumberInput,\n  StringInput,\n  OnClickOutsideHOC,\n} from 'uuiui'\n\nexport var canvasMetadata = {\n  specialNodes: [],\n  nodeMetadata: {},\n  scenes: [\n    {\n      component: 'App',\n      frame: { height: 812, left: 0, width: 375, top: 0 },\n      props: { layout: { top: 0, left: 0, bottom: 0, right: 0 } },\n      container: { layoutSystem: 'pinSystem' },\n    },\n  ],\n  elementMetadata: {},\n}\n\nexport var App = (props) => {\n  return (\n    <View\n      style={{ ...props.style, backgroundColor: colorTheme.white.value }}\n      layout={{ layoutSystem: 'pinSystem' }}\n      data-uid={'aaa'}\n    ></View>\n  )\n}\n\n",
})

const SampleSingleFileExportsInfo = [
  {
    filename: '/app.js',
    code:
      "/** @jsx jsx */\nimport * as React from 'react'\nimport {\n  Ellipse,\n  HelperFunctions,\n  Image,\n  NodeImplementations,\n  Rectangle,\n  Text,\n  View,\n  jsx,\n} from 'utopia-api'\nimport {\n  colorTheme,\n  Button,\n  Dialog,\n  Icn,\n  Icons,\n  LargerIcons,\n  FunctionIcons,\n  MenuIcons,\n  Isolator,\n  TabComponent,\n  Tooltip,\n  ActionSheet,\n  Avatar,\n  ControlledTextArea,\n  Title,\n  H1,\n  H2,\n  H3,\n  Subdued,\n  InspectorSectionHeader,\n  InspectorSubsectionHeader,\n  FlexColumn,\n  FlexRow,\n  ResizableFlexColumn,\n  PopupList,\n  Section,\n  SectionTitleRow,\n  SectionBodyArea,\n  UtopiaListSelect,\n  UtopiaListItem,\n  CheckboxInput,\n  NumberInput,\n  StringInput,\n  OnClickOutsideHOC,\n} from 'uuiui'\n\nexport var canvasMetadata = {\n  specialNodes: [],\n  nodeMetadata: {},\n  scenes: [\n    {\n      component: 'App',\n      frame: { height: 812, left: 0, width: 375, top: 0 },\n      props: { layout: { top: 0, left: 0, bottom: 0, right: 0 } },\n      container: { layoutSystem: 'pinSystem' },\n    },\n  ],\n  elementMetadata: {},\n}\n\nexport var App = (props) => {\n  return (\n    <View\n      style={{ ...props.style, backgroundColor: colorTheme.white.value }}\n      layout={{ layoutSystem: 'pinSystem' }}\n      data-uid={'aaa'}\n    ></View>\n  )\n}\n\n",
    exportTypes: {
      canvasMetadata: {
        type:
          '{ specialNodes: any[]; nodeMetadata: {}; scenes: { component: string; frame: { height: number; left: number; width: number; top: number; }; props: { layout: { top: number; left: number; bottom: number; right: number; }; }; container: { ...; }; }[]; elementMetadata: {}; }',
        functionInfo: null,
        reactClassInfo: null,
      },
      App: {
        type: '(props: any) => Element',
        functionInfo: [
          {
            name: 'props',
            memberInfo: {
              type: 'any',
              members: {},
            },
          },
        ],
        reactClassInfo: null,
      },
    },
  },
]

const SampleMultiFileBuildResult = transpileCode(
  ['/app.js', '/src/components.js', '/public/preview.jsx'],
  {
    '/app.js':
      "/** @jsx jsx */\nimport * as React from 'react'\nimport {\n  Ellipse,\n  HelperFunctions,\n  Image,\n  NodeImplementations,\n  Rectangle,\n  Text,\n  View,\n  jsx,\n} from 'utopia-api'\nimport {\n  colorTheme,\n  Button,\n  Dialog,\n  Icn,\n  Icons,\n  LargerIcons,\n  FunctionIcons,\n  MenuIcons,\n  Isolator,\n  TabComponent,\n  Tooltip,\n  ActionSheet,\n  Avatar,\n  ControlledTextArea,\n  Title,\n  H1,\n  H2,\n  H3,\n  Subdued,\n  InspectorSectionHeader,\n  InspectorSubsectionHeader,\n  FlexColumn,\n  FlexRow,\n  ResizableFlexColumn,\n  PopupList,\n  Section,\n  SectionTitleRow,\n  SectionBodyArea,\n  UtopiaListSelect,\n  UtopiaListItem,\n  CheckboxInput,\n  NumberInput,\n  StringInput,\n  OnClickOutsideHOC,\n} from 'uuiui'\n\nexport var canvasMetadata = {\n  specialNodes: [],\n  nodeMetadata: {},\n  scenes: [\n    {\n      component: 'App',\n      frame: { height: 812, left: 0, width: 375, top: 0 },\n      props: { layout: { top: 0, left: 0, bottom: 0, right: 0 } },\n      container: { layoutSystem: 'pinSystem' },\n    },\n  ],\n  elementMetadata: {},\n}\n\n\nexport var App = (props) => {\n  return (\n    <View\n      style={{ ...props.style, backgroundColor: colorTheme.white.value }}\n      layout={{ layoutSystem: 'pinSystem' }}\n      data-uid={'aaa'}\n    ></View>\n  )\n}\n\n",
    '/src/components.js':
      "// component library\nimport * as React from 'react'\nimport { Text, View } from 'utopia-api'\n\nexport default (props) => (\n  <View layout={props.layout} style={props.style} onMouseDown={props.onMouseDown}>\n    <Text\n      style={{ fontSize: 16, textAlign: 'center' }}\n      text={props.text}\n      layout={{\n        left: 0,\n        top: 10,\n        width: '100%',\n        height: '100%',\n      }}\n      textSizing={'fixed'}\n    />\n  </View>\n)\n\nexport const LABEL = 'press me! 😉'\n\nexport const ComponentWithProps = (props) => {\n  return (\n    <div\n      style={{\n        ...props.style,\n        backgroundColor: props.pink ? 'hotpink' : 'transparent',\n        whiteSpace: 'normal',\n      }}\n    >\n      {(props.text + ' ').repeat(props.num)}\n    </div>\n  )\n}\n\nComponentWithProps.propertyControls = {\n  text: {\n    type: 'string',\n    title: 'Title',\n    defaultValue: 'Change me',\n  },\n  num: {\n    type: 'number',\n    title: 'amount',\n    defaultValue: 2,\n  },\n  pink: {\n    type: 'boolean',\n    title: 'Enabled',\n    defaultValue: true,\n  },\n}\n\n",
    '/public/preview.jsx':
      'import * as React from "react";\nimport * as ReactDOM from "react-dom";\nimport { App } from "../app";\n\nconst root = document.getElementById("root");\nif (root != null) {\nReactDOM.render(<App />, root);\n}',
  },
)

const SampleMultiFileExportsInfo: Array<ExportsInfo> = [
  {
    filename: '/app.js',
    code:
      "/** @jsx jsx */\nimport * as React from 'react'\nimport {\n  Ellipse,\n  HelperFunctions,\n  Image,\n  NodeImplementations,\n  Rectangle,\n  Text,\n  View,\n  jsx,\n} from 'utopia-api'\nimport {\n  colorTheme,\n  Button,\n  Dialog,\n  Icn,\n  Icons,\n  LargerIcons,\n  FunctionIcons,\n  MenuIcons,\n  Isolator,\n  TabComponent,\n  Tooltip,\n  ActionSheet,\n  Avatar,\n  ControlledTextArea,\n  Title,\n  H1,\n  H2,\n  H3,\n  Subdued,\n  InspectorSectionHeader,\n  InspectorSubsectionHeader,\n  FlexColumn,\n  FlexRow,\n  ResizableFlexColumn,\n  PopupList,\n  Section,\n  SectionTitleRow,\n  SectionBodyArea,\n  UtopiaListSelect,\n  UtopiaListItem,\n  CheckboxInput,\n  NumberInput,\n  StringInput,\n  OnClickOutsideHOC,\n} from 'uuiui'\n\nexport var canvasMetadata = {\n  specialNodes: [],\n  nodeMetadata: {},\n  scenes: [\n    {\n      component: 'App',\n      frame: { height: 812, left: 0, width: 375, top: 0 },\n      props: { layout: { top: 0, left: 0, bottom: 0, right: 0 } },\n      container: { layoutSystem: 'pinSystem' },\n    },\n  ],\n  elementMetadata: {},\n}\n\n\nexport var App = (props) => {\n  return (\n    <View\n      style={{ ...props.style, backgroundColor: colorTheme.white.value }}\n      layout={{ layoutSystem: 'pinSystem' }}\n      data-uid={'aaa'}\n    ></View>\n  )\n}\n\n",
    exportTypes: {
      canvasMetadata: {
        type:
          '{ specialNodes: any[]; nodeMetadata: {}; scenes: { component: string; frame: { height: number; left: number; width: number; top: number; }; props: { layout: { top: number; left: number; bottom: number; right: number; }; }; container: { ...; }; }[]; elementMetadata: {}; }',
        functionInfo: null,
        reactClassInfo: null,
      },
      App: {
        type: '(props: any) => Element',
        functionInfo: [
          {
            name: 'props',
            memberInfo: {
              type: 'any',
              members: {},
            },
          },
        ],
        reactClassInfo: null,
      },
    },
  },
  {
    filename: '/src/components.js',
    code:
      "// component library\nimport * as React from 'react'\nimport { Text, View } from 'utopia-api'\n\nexport default (props) => (\n  <View layout={props.layout} style={props.style} onMouseDown={props.onMouseDown}>\n    <Text\n      style={{ fontSize: 16, textAlign: 'center' }}\n      text={props.text}\n      layout={{\n        left: 0,\n        top: 10,\n        width: '100%',\n        height: '100%',\n      }}\n      textSizing={'fixed'}\n    />\n  </View>\n)\n\nexport const LABEL = 'press me! 😉'\n\nexport const ComponentWithProps = (props) => {\n  return (\n    <div\n      style={{\n        ...props.style,\n        backgroundColor: props.pink ? 'hotpink' : 'transparent',\n        whiteSpace: 'normal',\n      }}\n    >\n      {(props.text + ' ').repeat(props.num)}\n    </div>\n  )\n}\n\nComponentWithProps.propertyControls = {\n  text: {\n    type: 'string',\n    title: 'Title',\n    defaultValue: 'Change me',\n  },\n  num: {\n    type: 'number',\n    title: 'amount',\n    defaultValue: 2,\n  },\n  pink: {\n    type: 'boolean',\n    title: 'Enabled',\n    defaultValue: true,\n  },\n}\n\n",
    exportTypes: {
      LABEL: {
        type: '"press me! 😉"',
        functionInfo: null,
        reactClassInfo: null,
      },
      ComponentWithProps: {
        type:
          '{ (props: any): Element; propertyControls: { text: { type: string; title: string; defaultValue: string; }; num: { type: string; title: string; defaultValue: number; }; pink: { type: string; title: string; defaultValue: boolean; }; }; }',
        functionInfo: [
          {
            name: 'props',
            memberInfo: {
              type: 'any',
              members: {},
            },
          },
        ],
        reactClassInfo: null,
      },
    },
  },
  {
    filename: '/src/index.js',
    code:
      'import * as React from "react";\nimport * as ReactDOM from "react-dom";\nimport { App } from "../app";\n\nconst root = document.getElementById("root");\nif (root != null) {\nReactDOM.render(<App />, root);\n}',
    exportTypes: {},
  },
]

const SampleBuildResultWithError: MultiFileBuildResult = {
  '/src/components.js': {
    errors: [
      {
        fileName: '/src/components.js',
        startLine: 14,
        startColumn: 24,
        endLine: 14,
        endColumn: 26,
        codeSnippet: "  14 |         height: '100%',++\n       ~~~~~~~~~~~~~~~~~~~~~~~~~\n",
        severity: 'fatal',
        type: 'Error',
        message: 'Property assignment expected.',
        errorCode: 'TS1136',
        source: 'build',
        passTime: null,
      },
      {
        fileName: '/src/components.js',
        startLine: 15,
        startColumn: 8,
        endLine: 15,
        endColumn: 9,
        codeSnippet: '  15 |       }}\n       ~~~~~~~~\n',
        severity: 'fatal',
        type: 'Error',
        message: 'Identifier expected.',
        errorCode: 'TS1003',
        source: 'build',
        passTime: null,
      },
    ],
    transpiledCode: null,
    sourceMap: null,
  },
}

const SampleExportsInfoWithError = [
  {
    filename: '/src/components.js',
    code:
      "// component library\nimport * as React from 'react'\nimport { Text, View } from 'utopia-api'\n\nexport default (props) => (\n  <View layout={props.layout} style={props.style} onMouseDown={props.onMouseDown}>\n    <Text\n      style={{ fontSize: 16, textAlign: 'center' }}\n      text={props.text}\n      layout={{\n        left: 0,\n        top: 10,\n        width: '100%',\n        height: '100%',++\n      }}\n      textSizing={'fixed'}\n    />\n  </View>\n)\n\nexport const LABEL = 'press me! 😉'\n\nexport const ComponentWithProps = (props) => {\n  return (\n    <div\n      style={{\n        ...props.style,\n        backgroundColor: props.pink ? 'hotpink' : 'transparent',\n        whiteSpace: 'normal',\n      }}\n    >\n      {(props.text + ' ').repeat(props.num)}\n    </div>\n  )\n}\n\nComponentWithProps.propertyControls = {\n  text: {\n    type: 'string',\n    title: 'Title',\n    defaultValue: 'Change me',\n  },\n  num: {\n    type: 'number',\n    title: 'amount',\n    defaultValue: 2,\n  },\n  pink: {\n    type: 'boolean',\n    title: 'Enabled',\n    defaultValue: true,\n  },\n}\n\n",
    exportTypes: {
      LABEL: {
        type: '"press me! 😉"',
        functionInfo: null,
        reactClassInfo: null,
      },
      ComponentWithProps: {
        type:
          '{ (props: any): Element; propertyControls: { text: { type: string; title: string; defaultValue: string; }; num: { type: string; title: string; defaultValue: number; }; pink: { type: string; title: string; defaultValue: boolean; }; }; }',
        functionInfo: [
          {
            name: 'props',
            memberInfo: {
              type: 'any',
              members: {},
            },
          },
        ],
        reactClassInfo: null,
      },
    },
  },
]

const SampleBuildResultWithException = transpileCode(['/src/code.js'], {
  '/src/code.js': "export const boom = (() => {\n  throw Error('booom')\n})()\n",
})

const SampleExportsInfoWithException = [
  {
    filename: '/src/code.js',
    code: "export const boom = (() => {\n  throw Error('booom')\n})()\n",
    exportTypes: {
      boom: {
        type: 'never',
        functionInfo: null,
        reactClassInfo: null,
      },
    },
  },
]

const ImportCSSTestCode = `/** @jsx jsx */
  import * as React from 'react'
  import icon from './icon.css'
  export var App = (props) => <div data-uid={'aaa'}>{icon}</div>
  `

const SampleNodeModules: NodeModules = {
  '/node_modules/utopia-api/index.js': esCodeFile(`export {}`, null),
  '/node_modules/utopia-api/package.json': esCodeFile(JSON.stringify({ main: './index.js' }), null),
  '/node_modules/uuiui/index.js': esCodeFile(`export {}`, null),
  '/node_modules/uuiui/package.json': esCodeFile(JSON.stringify({ main: './index.js' }), null),
  '/node_modules/react/index.js': esCodeFile(`export {}`, null),
  '/node_modules/react/package.json': esCodeFile(JSON.stringify({ main: './index.js' }), null),
  '/node_modules/react-dom/index.js': esCodeFile(`export {}`, null),
  '/node_modules/react-dom/package.json': esCodeFile(JSON.stringify({ main: './index.js' }), null),
}

describe('transpileCode', () => {
  it('transpiles imports with the file loader when doing a standard import of CSS', () => {
    const importTestFileBuildResult = transpileCode(['/app.js'], {
      '/app.js': ImportCSSTestCode,
    })
    expect(importTestFileBuildResult).toMatchInlineSnapshot(`
      Object {
        "/app.js": Object {
          "errors": Array [],
          "sourceMap": Object {
            "file": "app.js",
            "mappings": ";;;;;;;;;;;;;AAEE,IAAA,UAAA,GAAA,eAAA,CAAA,OAAA,CAAA,YAAA,CAAA,CAAA;;AACW,OAAA,CAAA,GAAA,GAAM,UAAC,KAAD;AAAA,SAAW,GAAA,CAAA,KAAA,EAAA;AAAA,gBAAe;AAAf,GAAA,EAAuB,UAAA,CAAA,OAAvB,CAAX;AAAA,CAAN,C",
            "names": Array [],
            "sourceRoot": "",
            "sources": Array [
              "../app.js",
            ],
            "sourcesContent": Array [
              "/** @jsx jsx */
        import * as React from 'react'
        import icon from './icon.css'
        export var App = (props) => <div data-uid={'aaa'}>{icon}</div>
        ",
            ],
            "version": 3,
          },
          "transpiledCode": "\\"use strict\\";

      var __importDefault = this && this.__importDefault || function (mod) {
        return mod && mod.__esModule ? mod : {
          \\"default\\": mod
        };
      };

      Object.defineProperty(exports, \\"__esModule\\", {
        value: true
      });
      exports.App = void 0;

      var icon_css_1 = __importDefault(require(\\"./icon.css\\"));

      exports.App = function (props) {
        return jsx(\\"div\\", {
          \\"data-uid\\": 'aaa'
        }, icon_css_1.default);
      }; //# sourceMappingURL=app.js.map",
        },
      }
    `)
  })
})

describe('Generating codeResultCache', () => {
  it('Generates codeResultCache for single file build result', () => {
    const codeResultCache = generateCodeResultCache(
      {},
      {},
      SampleSingleFileBuildResult,
      SampleSingleFileExportsInfo,
      {},
      NO_OP,
      'incremental',
      null,
      false,
    )

    expect(codeResultCache).toMatchSnapshot()
  })

  it('Generates codeResultCache for multi file build result', () => {
    const codeResultCache = generateCodeResultCache(
      {},
      {},
      SampleMultiFileBuildResult,
      SampleMultiFileExportsInfo,
      SampleNodeModules,
      NO_OP,
      'incremental',
      null,
      false,
    )

    expect(codeResultCache).toMatchSnapshot()
  })
  it('Generates codeResultCache for build error', () => {
    const codeResultCache = generateCodeResultCache(
      {},
      {},
      SampleBuildResultWithError,
      SampleExportsInfoWithError,
      {},
      NO_OP,
      'incremental',
      null,
      false,
    )

    expect(codeResultCache).toMatchSnapshot()
  })
})

describe('Creating require function', () => {
  it('Creates require function for single file build result', () => {
    const codeResultCache = generateCodeResultCache(
      {},
      {},
      SampleSingleFileBuildResult,
      SampleSingleFileExportsInfo,
      {},
      NO_OP,
      'incremental',
      null,
      false,
    )

    expect(codeResultCache.requireFn('/', './app', false)).toMatchSnapshot()
  })
  it('Creates require function for multi file build result', () => {
    const codeResultCache = generateCodeResultCache(
      {},
      {},
      SampleMultiFileBuildResult,
      SampleMultiFileExportsInfo,
      {},
      NO_OP,
      'incremental',
      null,
      false,
    )

    expect(codeResultCache.requireFn('/', './app', false)).toMatchSnapshot()
    expect(codeResultCache.requireFn('/', './src/components', false)).toMatchSnapshot()
  })
  it('Require throws exception for module code', () => {
    const codeResultCache = generateCodeResultCache(
      {},
      {},
      SampleBuildResultWithException,
      SampleExportsInfoWithException,
      {},
      NO_OP,
      'incremental',
      null,
      false,
    )

    expect(() => codeResultCache.requireFn('/', './src/code', false)).toThrowErrorMatchingSnapshot()
  })
  it('Require throws exception for import from non-existing module', () => {
    const codeResultCache = generateCodeResultCache(
      {},
      {},
      SampleSingleFileBuildResult,
      SampleSingleFileExportsInfo,
      {},
      NO_OP,
      'incremental',
      null,
      false,
    )

    expect(() => codeResultCache.requireFn('/', 'foo', false)).toThrowErrorMatchingSnapshot()
  })
})

function createCodeFile(path: string, contents: string): TextFile {
  const result = lintAndParse(path, contents)
  return textFile(textFileContents(contents, result, RevisionsState.CodeAhead), null, Date.now())
}

function defaultProjectContentsForNormalising(): ProjectContentTreeRoot {
  let projectContents: ProjectContents = {
    '/package.json': textFile(
      textFileContents(
        JSON.stringify(DefaultPackageJson, null, 2),
        unparsed,
        RevisionsState.BothMatch,
      ),
      null,
      0,
    ),
    '/src': directory(),
    '/utopia': directory(),
    [StoryboardFilePath]: createCodeFile(
      StoryboardFilePath,
      `/** @jsx jsx */
import * as React from 'react'
import { Scene, Storyboard, jsx } from 'utopia-api'
import { App } from '/src/app.js'

export var SameFileApp = (props) => {
  return <div data-uid='same-file-app-div' />
}

export var storyboard = (
  <Storyboard data-uid='storyboard-entity'>
    <Scene
      data-uid='scene-1-entity'
      component={App}
      props={{}}
      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
    />
    <Scene
      data-uid='scene-2-entity'
      component={SameFileApp}
      props={{}}
      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
    />
  </Storyboard>
)
`,
    ),
    '/src/app.js': createCodeFile(
      '/src/app.js',
      `/** @jsx jsx */
import * as React from 'react'
import { jsx } from 'utopia-api'
import { Card } from '/src/card.js'
export var App = (props) => {
  return <div data-uid='app-outer-div'>
    <Card data-uid='card-instance' />
  </div>
}
`,
    ),
    '/src/card.js': createCodeFile(
      '/src/app.js',
      `/** @jsx jsx */
import * as React from 'react'
import { jsx, Rectangle } from 'utopia-api'
export var Card = (props) => {
  return <div data-uid='card-outer-div'>
    <div data-uid='card-inner-div' />
    <Rectangle data-uid='card-inner-rectangle' /> 
  </div>
}
`,
    ),
    '/utopia/unparsedstoryboard.js': createCodeFile(
      StoryboardFilePath,
      `/** @jsx jsx */
import * as React from 'react'
import { Scene, Storyboard, jsx } from 'utopia-api'
import { App } from '/src/app.js'
export var storyboard = (
  <Storyboard data-uid='storyboard-entity'>
    <Scene
      data-uid='scene-1-entity'
      component={App}
      props={{}}
      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
    />
  </Storyboard>
)
`,
    ),
  }

  projectContents = objectMap((projectFile: ProjectFile, fullPath: string) => {
    if (isTextFile(projectFile) && fullPath !== '/utopia/unparsedstoryboard.js') {
      const code = projectFile.fileContents.code
      const parsedFile = parseCode(fullPath, code)
      return textFile(textFileContents(code, parsedFile, RevisionsState.BothMatch), null, 1000)
    } else {
      return projectFile
    }
  }, projectContents)

  return contentsToTree(projectContents)
}

function getTextFileByPath(projectContents: ProjectContentTreeRoot, path: string): TextFile {
  const possibleResult = getContentsTreeFileFromString(projectContents, path)
  if (isTextFile(possibleResult)) {
    return possibleResult
  } else {
    throw new Error(`Unable to find a text file at path ${path}.`)
  }
}

function staticInstancePathFromString(path: string): StaticInstancePath {
  const fromStringResult = TP.fromString(path)
  if (TP.isScenePath(fromStringResult)) {
    throw new Error(`${path} represents a scene path.`)
  } else {
    return TP.dynamicPathToStaticPath(fromStringResult)
  }
}

describe('normalisePathToUnderlyingTarget', () => {
  const projectContents = defaultProjectContentsForNormalising()
  it('handles finding the target within the same file', () => {
    const actualResult = normalisePathToUnderlyingTarget(
      projectContents,
      StoryboardFilePath,
      staticInstancePathFromString('storyboard-entity/scene-2-entity:same-file-app-div'),
    )
    const expectedResult = normalisePathSuccess(
      staticInstancePathFromString(':same-file-app-div'),
      StoryboardFilePath,
      getTextFileByPath(projectContents, StoryboardFilePath),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('jumps across multiple files to reach the actual target', () => {
    const actualResult = normalisePathToUnderlyingTarget(
      projectContents,
      StoryboardFilePath,
      staticInstancePathFromString(
        'storyboard-entity/scene-1-entity:app-outer-div/card-instance:card-outer-div/card-inner-div',
      ),
    )
    const expectedResult = normalisePathSuccess(
      staticInstancePathFromString(':card-outer-div/card-inner-div'),
      '/src/card.js',
      getTextFileByPath(projectContents, '/src/card.js'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('returns the same path because there are no hops to take', () => {
    const actualResult = normalisePathToUnderlyingTarget(
      projectContents,
      '/src/card.js',
      staticInstancePathFromString(':card-outer-div/card-inner-div'),
    )
    const expectedResult = normalisePathSuccess(
      staticInstancePathFromString(':card-outer-div/card-inner-div'),
      '/src/card.js',
      getTextFileByPath(projectContents, '/src/card.js'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('gives an error when a file does not exist', () => {
    const actualResult = normalisePathToUnderlyingTarget(
      projectContents,
      '/src/nonexistant.js',
      staticInstancePathFromString(':card-outer-div/card-inner-div'),
    )
    const expectedResult = normalisePathUnableToProceed('/src/nonexistant.js')
    expect(actualResult).toEqual(expectedResult)
  })
  it('skips attempting to traverse when confronted with an unparsed code file', () => {
    const actualResult = normalisePathToUnderlyingTarget(
      projectContents,
      '/utopia/unparsedstoryboard.js',
      staticInstancePathFromString(
        'storyboard-entity/scene-1-entity:app-outer-div/card-instance:card-outer-div/card-inner-div',
      ),
    )
    const expectedResult = normalisePathUnableToProceed('/utopia/unparsedstoryboard.js')
    expect(actualResult).toEqual(expectedResult)
  })
  it('handles hitting an external dependency', () => {
    const actualResult = normalisePathToUnderlyingTarget(
      projectContents,
      StoryboardFilePath,
      staticInstancePathFromString(
        'storyboard-entity/scene-1-entity:app-outer-div/card-instance:card-outer-div/card-inner-rectangle:rectangle-inner-div',
      ),
    )
    const expectedResult = normalisePathEndsAtDependency('utopia-api')
    expect(actualResult).toEqual(expectedResult)
  })
})
