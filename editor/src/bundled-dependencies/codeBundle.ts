import { ExportsInfo } from '../core/workers/ts/ts-worker'

// If you wish to update this auto-generated file, go to codeBundle.spec.ts and find the test called
// 'Update Saved Bundle' and change the test function from xit(... to it(... to enable it

export const SampleFileBuildResult = JSON.parse(`{
  "/src/app.js": {
    "errors": [],
    "transpiledCode": "\\"use strict\\";\\n\\nObject.defineProperty(exports, \\"__esModule\\", {\\n  value: true\\n});\\nexports.storyboard = exports.App = void 0;\\n\\nvar utopia_api_1 = require(\\"utopia-api\\");\\n\\nexports.App = function (props) {\\n  return utopia_api_1.jsx(utopia_api_1.View, {\\n    style: {\\n      width: '100%',\\n      height: '100%',\\n      backgroundColor: '#FFFFFF'\\n    },\\n    layout: {\\n      layoutSystem: 'pinSystem'\\n    }\\n  });\\n};\\n\\nexports.storyboard = utopia_api_1.jsx(utopia_api_1.Storyboard, {\\n  layout: {\\n    layoutSystem: 'pinSystem'\\n  }\\n}, utopia_api_1.jsx(utopia_api_1.Scene, {\\n  component: exports.App,\\n  props: {},\\n  static: true,\\n  style: {\\n    position: 'absolute',\\n    left: 0,\\n    top: 0,\\n    width: 375,\\n    height: 812\\n  }\\n})); //# sourceMappingURL=app.js.map",
    "sourceMap": {
      "version": 3,
      "sources": [
        "../src/app.js"
      ],
      "names": [],
      "mappings": ";;;;;;;AAEA,IAAA,YAAA,GAAA,OAAA,CAAA,YAAA,CAAA;;AACW,OAAA,CAAA,GAAA,GAAM,UAAC,KAAD,EAAU;AACzB,SACE,YAAA,CAAA,GAAA,CAAC,YAAA,CAAA,IAAD,EAAK;AACH,IAAA,KAAK,EAAE;AAAE,MAAA,KAAK,EAAE,MAAT;AAAiB,MAAA,MAAM,EAAE,MAAzB;AAAiC,MAAA,eAAe,EAAE;AAAlD,KADJ;AAEH,IAAA,MAAM,EAAE;AAAE,MAAA,YAAY,EAAE;AAAhB;AAFL,GAAL,CADF;AAMD,CAPU;;AAQA,OAAA,CAAA,UAAA,GACT,YAAA,CAAA,GAAA,CAAC,YAAA,CAAA,UAAD,EAAW;AAAC,EAAA,MAAM,EAAE;AAAE,IAAA,YAAY,EAAE;AAAhB;AAAT,CAAX,EACE,YAAA,CAAA,GAAA,CAAC,YAAA,CAAA,KAAD,EAAM;AACJ,EAAA,SAAS,EAAE,OAAA,CAAA,GADP;AAEJ,EAAA,KAAK,EAAE,EAFH;AAGJ,EAAA,MAAM,EAAA,IAHF;AAIJ,EAAA,KAAK,EAAE;AAAE,IAAA,QAAQ,EAAE,UAAZ;AAAwB,IAAA,IAAI,EAAE,CAA9B;AAAiC,IAAA,GAAG,EAAE,CAAtC;AAAyC,IAAA,KAAK,EAAE,GAAhD;AAAqD,IAAA,MAAM,EAAE;AAA7D;AAJH,CAAN,CADF,CADS,C",
      "sourcesContent": [
        "/** @jsx jsx */\\nimport * as React from 'react'\\nimport { Scene, Storyboard, View, jsx } from 'utopia-api'\\nexport var App = (props) => {\\n  return (\\n    <View\\n      style={{ width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}\\n      layout={{ layoutSystem: 'pinSystem' }}\\n    />\\n  )\\n}\\nexport var storyboard = (\\n  <Storyboard layout={{ layoutSystem: 'pinSystem' }}>\\n    <Scene\\n      component={App}\\n      props={{}}\\n      static\\n      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}\\n    />\\n  </Storyboard>\\n)\\n\\n"
      ],
      "sourceRoot": "",
      "file": "app.js"
    }
  },
  "/src/index.js": {
    "errors": [],
    "transpiledCode": "\\"use strict\\";\\n\\nvar __createBinding = this && this.__createBinding || (Object.create ? function (o, m, k, k2) {\\n  if (k2 === undefined) k2 = k;\\n  Object.defineProperty(o, k2, {\\n    enumerable: true,\\n    get: function get() {\\n      return m[k];\\n    }\\n  });\\n} : function (o, m, k, k2) {\\n  if (k2 === undefined) k2 = k;\\n  o[k2] = m[k];\\n});\\n\\nvar __setModuleDefault = this && this.__setModuleDefault || (Object.create ? function (o, v) {\\n  Object.defineProperty(o, \\"default\\", {\\n    enumerable: true,\\n    value: v\\n  });\\n} : function (o, v) {\\n  o[\\"default\\"] = v;\\n});\\n\\nvar __importStar = this && this.__importStar || function (mod) {\\n  if (mod && mod.__esModule) return mod;\\n  var result = {};\\n  if (mod != null) for (var k in mod) {\\n    if (k !== \\"default\\" && Object.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);\\n  }\\n\\n  __setModuleDefault(result, mod);\\n\\n  return result;\\n};\\n\\nObject.defineProperty(exports, \\"__esModule\\", {\\n  value: true\\n});\\n\\nvar React = __importStar(require(\\"react\\"));\\n\\nvar ReactDOM = __importStar(require(\\"react-dom\\"));\\n\\nvar app_1 = require(\\"../src/app\\");\\n\\nvar root = document.getElementById(\\"root\\");\\n\\nif (root != null) {\\n  ReactDOM.render(React.createElement(app_1.App, null), root);\\n} //# sourceMappingURL=index.js.map",
    "sourceMap": {
      "version": 3,
      "sources": [
        "../src/index.js"
      ],
      "names": [],
      "mappings": ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;AAAA,IAAA,KAAA,GAAA,YAAA,CAAA,OAAA,CAAA,OAAA,CAAA,CAAA;;AACA,IAAA,QAAA,GAAA,YAAA,CAAA,OAAA,CAAA,WAAA,CAAA,CAAA;;AACA,IAAA,KAAA,GAAA,OAAA,CAAA,YAAA,CAAA;;AAEA,IAAM,IAAI,GAAG,QAAQ,CAAC,cAAT,CAAwB,MAAxB,CAAb;;AACA,IAAI,IAAI,IAAI,IAAZ,EAAkB;AAChB,EAAA,QAAQ,CAAC,MAAT,CAAgB,KAAA,CAAA,aAAA,CAAC,KAAA,CAAA,GAAD,EAAI,IAAJ,CAAhB,EAAyB,IAAzB;AACD,C",
      "sourcesContent": [
        "import * as React from \\"react\\";\\nimport * as ReactDOM from \\"react-dom\\";\\nimport { App } from \\"../src/app\\";\\n\\nconst root = document.getElementById(\\"root\\");\\nif (root != null) {\\n  ReactDOM.render(<App />, root);\\n}"
      ],
      "sourceRoot": "",
      "file": "index.js"
    }
  }
}`)

export const SampleFileBundledExportsInfo: Array<ExportsInfo> = JSON.parse(`[
  {
    "filename": "/src/app.js",
    "code": "/** @jsx jsx */\\nimport * as React from 'react'\\nimport { Scene, Storyboard, View, jsx } from 'utopia-api'\\nexport var App = (props) => {\\n  return (\\n    <View\\n      style={{ width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}\\n      layout={{ layoutSystem: 'pinSystem' }}\\n    />\\n  )\\n}\\nexport var storyboard = (\\n  <Storyboard layout={{ layoutSystem: 'pinSystem' }}>\\n    <Scene\\n      component={App}\\n      props={{}}\\n      static\\n      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}\\n    />\\n  </Storyboard>\\n)\\n\\n",
    "exportTypes": {
      "App": {
        "type": "(props: any) => Element",
        "functionInfo": [
          {
            "name": "props",
            "memberInfo": {
              "type": "any",
              "members": {}
            }
          }
        ],
        "reactClassInfo": null
      },
      "storyboard": {
        "type": "Element",
        "functionInfo": null,
        "reactClassInfo": null
      }
    }
  },
  {
    "filename": "/src/index.js",
    "code": "import * as React from \\"react\\";\\nimport * as ReactDOM from \\"react-dom\\";\\nimport { App } from \\"../src/app\\";\\n\\nconst root = document.getElementById(\\"root\\");\\nif (root != null) {\\n  ReactDOM.render(<App />, root);\\n}",
    "exportTypes": {}
  }
]`)
