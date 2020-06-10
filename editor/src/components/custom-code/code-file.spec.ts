import { generateCodeResultCache } from './code-file'
import { ExportsInfo, MultiFileBuildResult } from '../../core/workers/ts/ts-worker'
import * as Es6MicroLoader from './es6-micro-loader'

const SampleSingleFileBuildResult = {
  '/app.ui.js': {
    errors: [],
    transpiledCode:
      'System.register(["utopia-api", "uuiui"], function (exports_1, context_1) {\r\n    "use strict";\r\n    var utopia_api_1, uuiui_1, canvasMetadata, App;\r\n    var __moduleName = context_1 && context_1.id;\r\n    return {\r\n        setters: [\r\n            function (utopia_api_1_1) {\r\n                utopia_api_1 = utopia_api_1_1;\r\n            },\r\n            function (uuiui_1_1) {\r\n                uuiui_1 = uuiui_1_1;\r\n            }\r\n        ],\r\n        execute: function () {\r\n            exports_1("canvasMetadata", canvasMetadata = {\r\n                specialNodes: [],\r\n                nodeMetadata: {},\r\n                scenes: [\r\n                    {\r\n                        component: \'App\',\r\n                        frame: { height: 812, left: 0, width: 375, top: 0 },\r\n                        props: { layout: { top: 0, left: 0, bottom: 0, right: 0 } },\r\n                        container: { layoutSystem: \'pinSystem\' },\r\n                    },\r\n                ],\r\n                elementMetadata: {},\r\n            });\r\n            exports_1("App", App = (props) => {\r\n                return (utopia_api_1.jsx(utopia_api_1.View, { style: Object.assign(Object.assign({}, (props.style || {})), { backgroundColor: uuiui_1.colorTheme.white.value }), layout: { layoutSystem: \'pinSystem\' }, "data-uid": \'aaa\' }));\r\n            });\r\n        }\r\n    };\r\n});\r\n//# sourceMappingURL=app.ui.js.map',
    sourceMap: {
      version: 3,
      file: 'app.ui.js',
      sourceRoot: '',
      sources: ['../app.ui.js'],
      names: [],
      mappings:
        ';;;;;;;;;;;;;;YAiDA,4BAAW,cAAc,GAAG;gBAC1B,YAAY,EAAE,EAAE;gBAChB,YAAY,EAAE,EAAE;gBAChB,MAAM,EAAE;oBACN;wBACE,SAAS,EAAE,KAAK;wBAChB,KAAK,EAAE,EAAE,MAAM,EAAE,GAAG,EAAE,IAAI,EAAE,CAAC,EAAE,KAAK,EAAE,GAAG,EAAE,GAAG,EAAE,CAAC,EAAE;wBACnD,KAAK,EAAE,EAAE,MAAM,EAAE,EAAE,GAAG,EAAE,CAAC,EAAE,IAAI,EAAE,CAAC,EAAE,MAAM,EAAE,CAAC,EAAE,KAAK,EAAE,CAAC,EAAE,EAAE;wBAC3D,SAAS,EAAE,EAAE,YAAY,EAAE,WAAW,EAAE;qBACzC;iBACF;gBACD,eAAe,EAAE,EAAE;aACpB,EAAA;YAED,iBAAW,GAAG,GAAG,CAAC,KAAK,EAAE,EAAE;gBACzB,OAAO,CACL,iBAAC,iBAAI,IACH,KAAK,kCAAO,CAAC,KAAK,CAAC,KAAK,IAAI,EAAE,CAAC,KAAE,eAAe,EAAE,kBAAU,CAAC,KAAK,CAAC,KAAK,KACxE,MAAM,EAAE,EAAE,YAAY,EAAE,WAAW,EAAE,cAC3B,KAAK,GACT,CACT,CAAA;YACH,CAAC,EAAA;QAED,CAAC',
      sourcesContent: [
        "/** @jsx jsx */\nimport * as React from 'react'\nimport {\n  Ellipse,\n  HelperFunctions,\n  Image,\n  NodeImplementations,\n  Rectangle,\n  Text,\n  View,\n  jsx,\n} from 'utopia-api'\nimport {\n  colorTheme,\n  Button,\n  Dialog,\n  Icn,\n  Icons,\n  LargerIcons,\n  FunctionIcons,\n  MenuIcons,\n  Isolator,\n  TabComponent,\n  Tooltip,\n  ActionSheet,\n  Avatar,\n  ControlledTextArea,\n  Title,\n  H1,\n  H2,\n  H3,\n  Subdued,\n  InspectorSectionHeader,\n  InspectorSubsectionHeader,\n  FlexColumn,\n  FlexRow,\n  ResizableFlexColumn,\n  PopupList,\n  Section,\n  SectionTitleRow,\n  SectionBodyArea,\n  UtopiaListSelect,\n  UtopiaListItem,\n  CheckboxInput,\n  NumberInput,\n  StringInput,\n  OnClickOutsideHOC,\n} from 'uuiui'\n\nexport var canvasMetadata = {\n  specialNodes: [],\n  nodeMetadata: {},\n  scenes: [\n    {\n      component: 'App',\n      frame: { height: 812, left: 0, width: 375, top: 0 },\n      props: { layout: { top: 0, left: 0, bottom: 0, right: 0 } },\n      container: { layoutSystem: 'pinSystem' },\n    },\n  ],\n  elementMetadata: {},\n}\n\nexport var App = (props) => {\n  return (\n    <View\n      style={{ ...(props.style || {}), backgroundColor: colorTheme.white.value }}\n      layout={{ layoutSystem: 'pinSystem' }}\n      data-uid={'aaa'}\n    ></View>\n  )\n}\n\n",
      ],
    },
  },
}

const SampleSingleFileExportsInfo = [
  {
    filename: '/app.ui.js',
    code:
      "/** @jsx jsx */\nimport * as React from 'react'\nimport {\n  Ellipse,\n  HelperFunctions,\n  Image,\n  NodeImplementations,\n  Rectangle,\n  Text,\n  View,\n  jsx,\n} from 'utopia-api'\nimport {\n  colorTheme,\n  Button,\n  Dialog,\n  Icn,\n  Icons,\n  LargerIcons,\n  FunctionIcons,\n  MenuIcons,\n  Isolator,\n  TabComponent,\n  Tooltip,\n  ActionSheet,\n  Avatar,\n  ControlledTextArea,\n  Title,\n  H1,\n  H2,\n  H3,\n  Subdued,\n  InspectorSectionHeader,\n  InspectorSubsectionHeader,\n  FlexColumn,\n  FlexRow,\n  ResizableFlexColumn,\n  PopupList,\n  Section,\n  SectionTitleRow,\n  SectionBodyArea,\n  UtopiaListSelect,\n  UtopiaListItem,\n  CheckboxInput,\n  NumberInput,\n  StringInput,\n  OnClickOutsideHOC,\n} from 'uuiui'\n\nexport var canvasMetadata = {\n  specialNodes: [],\n  nodeMetadata: {},\n  scenes: [\n    {\n      component: 'App',\n      frame: { height: 812, left: 0, width: 375, top: 0 },\n      props: { layout: { top: 0, left: 0, bottom: 0, right: 0 } },\n      container: { layoutSystem: 'pinSystem' },\n    },\n  ],\n  elementMetadata: {},\n}\n\nexport var App = (props) => {\n  return (\n    <View\n      style={{ ...(props.style || {}), backgroundColor: colorTheme.white.value }}\n      layout={{ layoutSystem: 'pinSystem' }}\n      data-uid={'aaa'}\n    ></View>\n  )\n}\n\n",
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

const SampleMultiFileBuildResult = {
  '/app.ui.js': {
    errors: [],
    transpiledCode:
      'System.register(["utopia-api", "uuiui"], function (exports_1, context_1) {\r\n    "use strict";\r\n    var utopia_api_1, uuiui_1, canvasMetadata, App;\r\n    var __moduleName = context_1 && context_1.id;\r\n    return {\r\n        setters: [\r\n            function (utopia_api_1_1) {\r\n                utopia_api_1 = utopia_api_1_1;\r\n            },\r\n            function (uuiui_1_1) {\r\n                uuiui_1 = uuiui_1_1;\r\n            }\r\n        ],\r\n        execute: function () {\r\n            exports_1("canvasMetadata", canvasMetadata = {\r\n                specialNodes: [],\r\n                nodeMetadata: {},\r\n                scenes: [\r\n                    {\r\n                        component: \'App\',\r\n                        frame: { height: 812, left: 0, width: 375, top: 0 },\r\n                        props: { layout: { top: 0, left: 0, bottom: 0, right: 0 } },\r\n                        container: { layoutSystem: \'pinSystem\' },\r\n                    },\r\n                ],\r\n                elementMetadata: {},\r\n            });\r\n            exports_1("App", App = (props) => {\r\n                return (utopia_api_1.jsx(utopia_api_1.View, { style: Object.assign(Object.assign({}, (props.style || {})), { backgroundColor: uuiui_1.colorTheme.white.value }), layout: { layoutSystem: \'pinSystem\' }, "data-uid": \'aaa\' }));\r\n            });\r\n        }\r\n    };\r\n});\r\n//# sourceMappingURL=app.ui.js.map',
    sourceMap: {
      version: 3,
      file: 'app.ui.js',
      sourceRoot: '',
      sources: ['../app.ui.js'],
      names: [],
      mappings:
        ';;;;;;;;;;;;;;YAkDA,4BAAW,cAAc,GAAG;gBAC1B,YAAY,EAAE,EAAE;gBAChB,YAAY,EAAE,EAAE;gBAChB,MAAM,EAAE;oBACN;wBACE,SAAS,EAAE,KAAK;wBAChB,KAAK,EAAE,EAAE,MAAM,EAAE,GAAG,EAAE,IAAI,EAAE,CAAC,EAAE,KAAK,EAAE,GAAG,EAAE,GAAG,EAAE,CAAC,EAAE;wBACnD,KAAK,EAAE,EAAE,MAAM,EAAE,EAAE,GAAG,EAAE,CAAC,EAAE,IAAI,EAAE,CAAC,EAAE,MAAM,EAAE,CAAC,EAAE,KAAK,EAAE,CAAC,EAAE,EAAE;wBAC3D,SAAS,EAAE,EAAE,YAAY,EAAE,WAAW,EAAE;qBACzC;iBACF;gBACD,eAAe,EAAE,EAAE;aACpB,EAAA;YAGD,iBAAW,GAAG,GAAG,CAAC,KAAK,EAAE,EAAE;gBACzB,OAAO,CACL,iBAAC,iBAAI,IACH,KAAK,kCAAO,CAAC,KAAK,CAAC,KAAK,IAAI,EAAE,CAAC,KAAE,eAAe,EAAE,kBAAU,CAAC,KAAK,CAAC,KAAK,KACxE,MAAM,EAAE,EAAE,YAAY,EAAE,WAAW,EAAE,cAC3B,KAAK,GACT,CACT,CAAA;YACH,CAAC,EAAA;QAED,CAAC',
      sourcesContent: [
        "/** @jsx jsx */\nimport * as React from 'react'\nimport {\n  Ellipse,\n  HelperFunctions,\n  Image,\n  NodeImplementations,\n  Rectangle,\n  Text,\n  View,\n  jsx,\n} from 'utopia-api'\nimport {\n  colorTheme,\n  Button,\n  Dialog,\n  Icn,\n  Icons,\n  LargerIcons,\n  FunctionIcons,\n  MenuIcons,\n  Isolator,\n  TabComponent,\n  Tooltip,\n  ActionSheet,\n  Avatar,\n  ControlledTextArea,\n  Title,\n  H1,\n  H2,\n  H3,\n  Subdued,\n  InspectorSectionHeader,\n  InspectorSubsectionHeader,\n  FlexColumn,\n  FlexRow,\n  ResizableFlexColumn,\n  PopupList,\n  Section,\n  SectionTitleRow,\n  SectionBodyArea,\n  UtopiaListSelect,\n  UtopiaListItem,\n  CheckboxInput,\n  NumberInput,\n  StringInput,\n  OnClickOutsideHOC,\n} from 'uuiui'\n\nexport var canvasMetadata = {\n  specialNodes: [],\n  nodeMetadata: {},\n  scenes: [\n    {\n      component: 'App',\n      frame: { height: 812, left: 0, width: 375, top: 0 },\n      props: { layout: { top: 0, left: 0, bottom: 0, right: 0 } },\n      container: { layoutSystem: 'pinSystem' },\n    },\n  ],\n  elementMetadata: {},\n}\n\n\nexport var App = (props) => {\n  return (\n    <View\n      style={{ ...(props.style || {}), backgroundColor: colorTheme.white.value }}\n      layout={{ layoutSystem: 'pinSystem' }}\n      data-uid={'aaa'}\n    ></View>\n  )\n}\n\n",
      ],
    },
  },
  '/src/components.js': {
    errors: [],
    transpiledCode:
      "System.register([\"react\", \"utopia-api\"], function (exports_1, context_1) {\r\n    \"use strict\";\r\n    var React, utopia_api_1, LABEL, ComponentWithProps;\r\n    var __moduleName = context_1 && context_1.id;\r\n    return {\r\n        setters: [\r\n            function (React_1) {\r\n                React = React_1;\r\n            },\r\n            function (utopia_api_1_1) {\r\n                utopia_api_1 = utopia_api_1_1;\r\n            }\r\n        ],\r\n        execute: function () {\r\n            exports_1(\"default\", (props) => (React.createElement(utopia_api_1.View, { layout: props.layout, style: props.style, onMouseDown: props.onMouseDown },\r\n                React.createElement(utopia_api_1.Text, { style: { fontSize: 16, textAlign: 'center' }, text: props.text, layout: {\r\n                        left: 0,\r\n                        top: 10,\r\n                        width: '100%',\r\n                        height: '100%',\r\n                    }, textSizing: 'fixed' }))));\r\n            exports_1(\"LABEL\", LABEL = 'press me! 😉');\r\n            exports_1(\"ComponentWithProps\", ComponentWithProps = (props) => {\r\n                return (React.createElement(\"div\", { style: Object.assign(Object.assign({}, props.style), { backgroundColor: props.pink ? 'hotpink' : 'transparent', whiteSpace: 'normal' }) }, (props.text + ' ').repeat(props.num)));\r\n            });\r\n            ComponentWithProps.propertyControls = {\r\n                text: {\r\n                    type: 'string',\r\n                    title: 'Title',\r\n                    defaultValue: 'Change me',\r\n                },\r\n                num: {\r\n                    type: 'number',\r\n                    title: 'amount',\r\n                    defaultValue: 2,\r\n                },\r\n                pink: {\r\n                    type: 'boolean',\r\n                    title: 'Enabled',\r\n                    defaultValue: true,\r\n                },\r\n            };\r\n        }\r\n    };\r\n});\r\n//# sourceMappingURL=components.js.map",
    sourceMap: {
      version: 3,
      file: 'components.js',
      sourceRoot: '',
      sources: ['../../src/components.js'],
      names: [],
      mappings:
        ';;;;;;;;;;;;;;iCAIe,CAAC,KAAK,EAAE,EAAE,CAAC,CACxB,oBAAC,iBAAI,IAAC,MAAM,EAAE,KAAK,CAAC,MAAM,EAAE,KAAK,EAAE,KAAK,CAAC,KAAK,EAAE,WAAW,EAAE,KAAK,CAAC,WAAW;gBAC5E,oBAAC,iBAAI,IACH,KAAK,EAAE,EAAE,QAAQ,EAAE,EAAE,EAAE,SAAS,EAAE,QAAQ,EAAE,EAC5C,IAAI,EAAE,KAAK,CAAC,IAAI,EAChB,MAAM,EAAE;wBACN,IAAI,EAAE,CAAC;wBACP,GAAG,EAAE,EAAE;wBACP,KAAK,EAAE,MAAM;wBACb,MAAM,EAAE,MAAM;qBACf,EACD,UAAU,EAAE,OAAO,GACnB,CACG,CACR;YAED,mBAAa,KAAK,GAAG,cAAc,EAAA;YAEnC,gCAAa,kBAAkB,GAAG,CAAC,KAAK,EAAE,EAAE;gBAC1C,OAAO,CACL,6BACE,KAAK,kCACA,KAAK,CAAC,KAAK,KACd,eAAe,EAAE,KAAK,CAAC,IAAI,CAAC,CAAC,CAAC,SAAS,CAAC,CAAC,CAAC,aAAa,EACvD,UAAU,EAAE,QAAQ,OAGrB,CAAC,KAAK,CAAC,IAAI,GAAG,GAAG,CAAC,CAAC,MAAM,CAAC,KAAK,CAAC,GAAG,CAAC,CACjC,CACP,CAAA;YACH,CAAC,EAAA;YAED,kBAAkB,CAAC,gBAAgB,GAAG;gBACpC,IAAI,EAAE;oBACJ,IAAI,EAAE,QAAQ;oBACd,KAAK,EAAE,OAAO;oBACd,YAAY,EAAE,WAAW;iBAC1B;gBACD,GAAG,EAAE;oBACH,IAAI,EAAE,QAAQ;oBACd,KAAK,EAAE,QAAQ;oBACf,YAAY,EAAE,CAAC;iBAChB;gBACD,IAAI,EAAE;oBACJ,IAAI,EAAE,SAAS;oBACf,KAAK,EAAE,SAAS;oBAChB,YAAY,EAAE,IAAI;iBACnB;aACF,CAAA;QAED,CAAC',
      sourcesContent: [
        "// component library\nimport * as React from 'react'\nimport { Text, View } from 'utopia-api'\n\nexport default (props) => (\n  <View layout={props.layout} style={props.style} onMouseDown={props.onMouseDown}>\n    <Text\n      style={{ fontSize: 16, textAlign: 'center' }}\n      text={props.text}\n      layout={{\n        left: 0,\n        top: 10,\n        width: '100%',\n        height: '100%',\n      }}\n      textSizing={'fixed'}\n    />\n  </View>\n)\n\nexport const LABEL = 'press me! 😉'\n\nexport const ComponentWithProps = (props) => {\n  return (\n    <div\n      style={{\n        ...props.style,\n        backgroundColor: props.pink ? 'hotpink' : 'transparent',\n        whiteSpace: 'normal',\n      }}\n    >\n      {(props.text + ' ').repeat(props.num)}\n    </div>\n  )\n}\n\nComponentWithProps.propertyControls = {\n  text: {\n    type: 'string',\n    title: 'Title',\n    defaultValue: 'Change me',\n  },\n  num: {\n    type: 'number',\n    title: 'amount',\n    defaultValue: 2,\n  },\n  pink: {\n    type: 'boolean',\n    title: 'Enabled',\n    defaultValue: true,\n  },\n}\n\n",
      ],
    },
  },
  '/public/preview.jsx': {
    errors: [],
    transpiledCode:
      'System.register(["react", "react-dom", "../app.ui"], function (exports_1, context_1) {\r\n    "use strict";\r\n    var React, ReactDOM, app_ui_1, root;\r\n    var __moduleName = context_1 && context_1.id;\r\n    return {\r\n        setters: [\r\n            function (React_1) {\r\n                React = React_1;\r\n            },\r\n            function (ReactDOM_1) {\r\n                ReactDOM = ReactDOM_1;\r\n            },\r\n            function (app_ui_1_1) {\r\n                app_ui_1 = app_ui_1_1;\r\n            }\r\n        ],\r\n        execute: function () {\r\n            root = document.getElementById("root");\r\n            if (root != null) {\r\n                ReactDOM.render(React.createElement(app_ui_1.App, { style: { left: 0, top: 0, width: 375, height: 812 } }), root);\r\n            }\r\n        }\r\n    };\r\n});\r\n//# sourceMappingURL=preview.js.map',
    sourceMap: {
      version: 3,
      file: 'preview.js',
      sourceRoot: '',
      sources: ['../../public/preview.jsx'],
      names: [],
      mappings:
        ';;;;;;;;;;;;;;;;;YAIM,IAAI,GAAG,QAAQ,CAAC,cAAc,CAAC,MAAM,CAAC,CAAC;YAC7C,IAAI,IAAI,IAAI,IAAI,EAAE;gBAClB,QAAQ,CAAC,MAAM,CAAC,oBAAC,YAAG,IAAC,KAAK,EAAE,EAAC,IAAI,EAAE,CAAC,EAAE,GAAG,EAAE,CAAC,EAAE,KAAK,EAAE,GAAG,EAAE,MAAM,EAAE,GAAG,EAAC,GAAI,EAAE,IAAI,CAAC,CAAC;aACjF;QAAA,CAAC',
      sourcesContent: [
        'import * as React from "react";\nimport * as ReactDOM from "react-dom";\nimport { App } from "../app.ui";\n\nconst root = document.getElementById("root");\nif (root != null) {\nReactDOM.render(<App />, root);\n}',
      ],
    },
  },
}

const SampleMultiFileExportsInfo: Array<ExportsInfo> = [
  {
    filename: '/app.ui.js',
    code:
      "/** @jsx jsx */\nimport * as React from 'react'\nimport {\n  Ellipse,\n  HelperFunctions,\n  Image,\n  NodeImplementations,\n  Rectangle,\n  Text,\n  View,\n  jsx,\n} from 'utopia-api'\nimport {\n  colorTheme,\n  Button,\n  Dialog,\n  Icn,\n  Icons,\n  LargerIcons,\n  FunctionIcons,\n  MenuIcons,\n  Isolator,\n  TabComponent,\n  Tooltip,\n  ActionSheet,\n  Avatar,\n  ControlledTextArea,\n  Title,\n  H1,\n  H2,\n  H3,\n  Subdued,\n  InspectorSectionHeader,\n  InspectorSubsectionHeader,\n  FlexColumn,\n  FlexRow,\n  ResizableFlexColumn,\n  PopupList,\n  Section,\n  SectionTitleRow,\n  SectionBodyArea,\n  UtopiaListSelect,\n  UtopiaListItem,\n  CheckboxInput,\n  NumberInput,\n  StringInput,\n  OnClickOutsideHOC,\n} from 'uuiui'\n\nexport var canvasMetadata = {\n  specialNodes: [],\n  nodeMetadata: {},\n  scenes: [\n    {\n      component: 'App',\n      frame: { height: 812, left: 0, width: 375, top: 0 },\n      props: { layout: { top: 0, left: 0, bottom: 0, right: 0 } },\n      container: { layoutSystem: 'pinSystem' },\n    },\n  ],\n  elementMetadata: {},\n}\n\n\nexport var App = (props) => {\n  return (\n    <View\n      style={{ ...(props.style || {}), backgroundColor: colorTheme.white.value }}\n      layout={{ layoutSystem: 'pinSystem' }}\n      data-uid={'aaa'}\n    ></View>\n  )\n}\n\n",
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
    filename: '/public/preview.jsx',
    code:
      'import * as React from "react";\nimport * as ReactDOM from "react-dom";\nimport { App } from "../app.ui";\n\nconst root = document.getElementById("root");\nif (root != null) {\nReactDOM.render(<App />, root);\n}',
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

const SampleBuildResultWithException = {
  '/src/code.js': {
    errors: [],
    transpiledCode:
      'System.register([], function (exports_1, context_1) {\r\n    "use strict";\r\n    var boom;\r\n    var __moduleName = context_1 && context_1.id;\r\n    return {\r\n        setters: [],\r\n        execute: function () {\r\n            exports_1("boom", boom = (() => {\r\n                throw Error(\'booom\');\r\n            })());\r\n        }\r\n    };\r\n});\r\n//# sourceMappingURL=code.js.map',
    sourceMap: {
      version: 3,
      file: 'code.js',
      sourceRoot: '',
      sources: ['../../src/code.js'],
      names: [],
      mappings:
        ';;;;;;;YAAA,kBAAa,IAAI,GAAG,CAAC,GAAG,EAAE;gBACxB,MAAM,KAAK,CAAC,OAAO,CAAC,CAAA;YACtB,CAAC,CAAC,EAAE,EAAA;QACJ,CAAC',
      sourcesContent: ["export const boom = (() => {\n  throw Error('booom')\n})()\n"],
    },
  },
}

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

const SampleBuildResultWithInvalidImport = {
  '/src/app.ui.js': {
    errors: [],
    transpiledCode:
      'System.register(["utopia-api"], function (exports_1, context_1) {\r\n    "use strict";\r\n    var utopia_api_1, canvasMetadata, App;\r\n    var __moduleName = context_1 && context_1.id;\r\n    return {\r\n        setters: [\r\n            function (utopia_api_1_1) {\r\n                utopia_api_1 = utopia_api_1_1;\r\n            }\r\n        ],\r\n        execute: function () {\r\n            exports_1("canvasMetadata", canvasMetadata = {\r\n                scenes: [\r\n                    {\r\n                        component: \'App\',\r\n                        frame: { height: 812, left: 0, width: 375, top: 0 },\r\n                        props: { layout: { top: 0, left: 0, bottom: 0, right: 0 } },\r\n                        container: { layoutSystem: \'pinSystem\' },\r\n                    },\r\n                ],\r\n                elementMetadata: {},\r\n            });\r\n            exports_1("App", App = (props) => {\r\n                return (utopia_api_1.jsx(utopia_api_1.View, { style: Object.assign(Object.assign({}, (props.style || {})), { backgroundColor: \'#FFFFFF\' }), layout: { layoutSystem: \'pinSystem\' }, "data-uid": \'aaa\' }));\r\n            });\r\n        }\r\n    };\r\n});\r\n//# sourceMappingURL=app.ui.js.map',
    sourceMap: {
      version: 3,
      file: 'app.ui.js',
      sourceRoot: '',
      sources: ['../../src/app.ui.js'],
      names: [],
      mappings:
        ';;;;;;;;;;;YAKA,4BAAW,cAAc,GAAG;gBAC1B,MAAM,EAAE;oBACN;wBACE,SAAS,EAAE,KAAK;wBAChB,KAAK,EAAE,EAAE,MAAM,EAAE,GAAG,EAAE,IAAI,EAAE,CAAC,EAAE,KAAK,EAAE,GAAG,EAAE,GAAG,EAAE,CAAC,EAAE;wBACnD,KAAK,EAAE,EAAE,MAAM,EAAE,EAAE,GAAG,EAAE,CAAC,EAAE,IAAI,EAAE,CAAC,EAAE,MAAM,EAAE,CAAC,EAAE,KAAK,EAAE,CAAC,EAAE,EAAE;wBAC3D,SAAS,EAAE,EAAE,YAAY,EAAE,WAAW,EAAE;qBACzC;iBACF;gBACD,eAAe,EAAE,EAAE;aACpB,EAAA;YAED,iBAAW,GAAG,GAAG,CAAC,KAAK,EAAE,EAAE;gBACzB,OAAO,CACL,iBAAC,iBAAI,IACH,KAAK,kCAAO,CAAC,KAAK,CAAC,KAAK,IAAI,EAAE,CAAC,KAAE,eAAe,EAAE,SAAS,KAC3D,MAAM,EAAE,EAAE,YAAY,EAAE,WAAW,EAAE,cAC3B,KAAK,GACT,CACT,CAAA;YACH,CAAC,EAAA;QACD,CAAC',
      sourcesContent: [
        "/** @jsx jsx */\nimport * as React from 'react'\nimport { View, jsx } from 'utopia-api'\nimport { Foo } from 'bar'\n\nexport var canvasMetadata = {\n  scenes: [\n    {\n      component: 'App',\n      frame: { height: 812, left: 0, width: 375, top: 0 },\n      props: { layout: { top: 0, left: 0, bottom: 0, right: 0 } },\n      container: { layoutSystem: 'pinSystem' },\n    },\n  ],\n  elementMetadata: {},\n}\n\nexport var App = (props) => {\n  return (\n    <View\n      style={{ ...(props.style || {}), backgroundColor: '#FFFFFF' }}\n      layout={{ layoutSystem: 'pinSystem' }}\n      data-uid={'aaa'}\n    ></View>\n  )\n}\n",
      ],
    },
  },
}

const SampleExportsInfoWithInvalidImport = [
  {
    filename: '/src/app.ui.js',
    code:
      "/** @jsx jsx */\nimport * as React from 'react'\nimport { View, jsx } from 'utopia-api'\nimport { Foo } from 'bar'\n\nexport var canvasMetadata = {\n  scenes: [\n    {\n      component: 'App',\n      frame: { height: 812, left: 0, width: 375, top: 0 },\n      props: { layout: { top: 0, left: 0, bottom: 0, right: 0 } },\n      container: { layoutSystem: 'pinSystem' },\n    },\n  ],\n  elementMetadata: {},\n}\n\nexport var App = (props) => {\n  return (\n    <View\n      style={{ ...(props.style || {}), backgroundColor: '#FFFFFF' }}\n      layout={{ layoutSystem: 'pinSystem' }}\n      data-uid={'aaa'}\n    ></View>\n  )\n}\n",
    exportTypes: {
      canvasMetadata: {
        type:
          '{ scenes: { component: string; frame: { height: number; left: number; width: number; top: number; }; props: { layout: { top: number; left: number; bottom: number; right: number; }; }; container: { layoutSystem: string; }; }[]; elementMetadata: {}; }',
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

const mockRequire = (importOrigin: string, toImport: string) => {
  if (
    toImport === 'utopia-api' ||
    toImport === 'uuiui' ||
    toImport === 'react' ||
    toImport === 'react-dom'
  ) {
    return {}
  }
  return undefined
}

describe('Generating codeResultCache', () => {
  it('Generates codeResultCache for single file build result', () => {
    const codeResultCache = generateCodeResultCache(
      SampleSingleFileBuildResult,
      SampleSingleFileExportsInfo,
      mockRequire,
      false,
    )

    expect(codeResultCache).toMatchSnapshot()
  })
  it('Generates codeResultCache for multi file build result', () => {
    const codeResultCache = generateCodeResultCache(
      SampleMultiFileBuildResult,
      SampleMultiFileExportsInfo,
      mockRequire,
      false,
    )

    expect(codeResultCache).toMatchSnapshot()
  })
  it('Generates codeResultCache for build error', () => {
    const codeResultCache = generateCodeResultCache(
      SampleBuildResultWithError,
      SampleExportsInfoWithError,
      mockRequire,
      false,
    )

    expect(codeResultCache).toMatchSnapshot()
  })
})
describe('Filling in SystemJS', () => {
  it('Fills System with data for single file build result', () => {
    generateCodeResultCache(
      SampleSingleFileBuildResult,
      SampleSingleFileExportsInfo,
      mockRequire,
      true,
    )

    const system = Es6MicroLoader.System
    expect(system.getModule('/', './app.ui.js', false)).toMatchSnapshot()
  })
  it('Fills System with data for multi file build result', () => {
    generateCodeResultCache(
      SampleMultiFileBuildResult,
      SampleMultiFileExportsInfo,
      mockRequire,
      true,
    )

    const system = Es6MicroLoader.System
    expect(system.getModule('/', './app.ui.js', false)).toMatchSnapshot()
    expect(system.getModule('/', './src/components.js', false)).toMatchSnapshot()
  })
})
