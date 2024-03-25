import type { ExportsInfo } from '../core/workers/common/worker-types'

// If you wish to update this auto-generated file, go to codeBundle.spec.ts and find the test called
// 'Update Saved Bundle' and change the test function from xit(... to it(... to enable it

export const SampleFileBuildResult = JSON.parse(`{
  "/src/app.js": {
    "errors": [],
    "transpiledCode": "\\"use strict\\";\\n\\nvar __createBinding = this && this.__createBinding || (Object.create ? function (o, m, k, k2) {\\n  if (k2 === undefined) k2 = k;\\n  var desc = Object.getOwnPropertyDescriptor(m, k);\\n\\n  if (!desc || (\\"get\\" in desc ? !m.__esModule : desc.writable || desc.configurable)) {\\n    desc = {\\n      enumerable: true,\\n      get: function () {\\n        return m[k];\\n      }\\n    };\\n  }\\n\\n  Object.defineProperty(o, k2, desc);\\n} : function (o, m, k, k2) {\\n  if (k2 === undefined) k2 = k;\\n  o[k2] = m[k];\\n});\\n\\nvar __setModuleDefault = this && this.__setModuleDefault || (Object.create ? function (o, v) {\\n  Object.defineProperty(o, \\"default\\", {\\n    enumerable: true,\\n    value: v\\n  });\\n} : function (o, v) {\\n  o[\\"default\\"] = v;\\n});\\n\\nvar __importStar = this && this.__importStar || function (mod) {\\n  if (mod && mod.__esModule) return mod;\\n  var result = {};\\n  if (mod != null) for (var k in mod) if (k !== \\"default\\" && Object.prototype.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);\\n\\n  __setModuleDefault(result, mod);\\n\\n  return result;\\n};\\n\\nObject.defineProperty(exports, \\"__esModule\\", {\\n  value: true\\n});\\nexports.App = void 0;\\n\\nconst jsx_runtime_1 = require(\\"react/jsx-runtime\\");\\n\\nconst React = __importStar(require(\\"react\\"));\\n\\nvar App = props => {\\n  return (0, jsx_runtime_1.jsx)(\\"div\\", {\\n    style: {\\n      width: '100%',\\n      height: '100%',\\n      backgroundColor: '#FFFFFF',\\n      position: 'relative'\\n    }\\n  });\\n};\\n\\nexports.App = App; //# sourceMappingURL=app.js.map",
    "sourceMap": {
      "version": 3,
      "sources": [
        "../../src/app.js"
      ],
      "names": [],
      "mappings": ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;AACA,MAAA,KAAA,GAAA,YAAA,CAAA,OAAA,CAAA,OAAA,CAAA,CAAA;;AACO,IAAI,GAAG,GAAI,KAAD,IAAU;AACzB,SACE,CAAA,GAAA,aAAA,CAAA,GAAA,EAAA,KAAA,EAAA;AACE,IAAA,KAAK,EAAE;AAAE,MAAA,KAAK,EAAE,MAAT;AAAiB,MAAA,MAAM,EAAE,MAAzB;AAAiC,MAAA,eAAe,EAAE,SAAlD;AAA6D,MAAA,QAAQ,EAAE;AAAvE;AADT,GAAA,CADF;AAKD,CANM;;AAAI,OAAA,CAAA,GAAA,GAAG,GAAH,C",
      "sourcesContent": [
        "\\nimport * as React from 'react'\\nexport var App = (props) => {\\n  return (\\n    <div\\n      style={{ width: '100%', height: '100%', backgroundColor: '#FFFFFF', position: 'relative' }}\\n    />\\n  )\\n}"
      ],
      "sourceRoot": "",
      "file": "app.js"
    }
  },
  "/src/index.js": {
    "errors": [],
    "transpiledCode": "\\"use strict\\";\\n\\nvar __createBinding = this && this.__createBinding || (Object.create ? function (o, m, k, k2) {\\n  if (k2 === undefined) k2 = k;\\n  var desc = Object.getOwnPropertyDescriptor(m, k);\\n\\n  if (!desc || (\\"get\\" in desc ? !m.__esModule : desc.writable || desc.configurable)) {\\n    desc = {\\n      enumerable: true,\\n      get: function () {\\n        return m[k];\\n      }\\n    };\\n  }\\n\\n  Object.defineProperty(o, k2, desc);\\n} : function (o, m, k, k2) {\\n  if (k2 === undefined) k2 = k;\\n  o[k2] = m[k];\\n});\\n\\nvar __setModuleDefault = this && this.__setModuleDefault || (Object.create ? function (o, v) {\\n  Object.defineProperty(o, \\"default\\", {\\n    enumerable: true,\\n    value: v\\n  });\\n} : function (o, v) {\\n  o[\\"default\\"] = v;\\n});\\n\\nvar __importStar = this && this.__importStar || function (mod) {\\n  if (mod && mod.__esModule) return mod;\\n  var result = {};\\n  if (mod != null) for (var k in mod) if (k !== \\"default\\" && Object.prototype.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);\\n\\n  __setModuleDefault(result, mod);\\n\\n  return result;\\n};\\n\\nObject.defineProperty(exports, \\"__esModule\\", {\\n  value: true\\n});\\n\\nconst jsx_runtime_1 = require(\\"react/jsx-runtime\\");\\n\\nconst React = __importStar(require(\\"react\\"));\\n\\nconst ReactDOM = __importStar(require(\\"react-dom\\"));\\n\\nconst app_1 = require(\\"../src/app\\");\\n\\nconst root = document.getElementById(\\"root\\");\\n\\nif (root != null) {\\n  ReactDOM.render((0, jsx_runtime_1.jsx)(app_1.App, {\\n    \\"data-uid\\": 'preview-app'\\n  }), root);\\n} //# sourceMappingURL=index.js.map",
    "sourceMap": {
      "version": 3,
      "sources": [
        "../../src/index.js"
      ],
      "names": [],
      "mappings": ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;AAAA,MAAA,KAAA,GAAA,YAAA,CAAA,OAAA,CAAA,OAAA,CAAA,CAAA;;AACA,MAAA,QAAA,GAAA,YAAA,CAAA,OAAA,CAAA,WAAA,CAAA,CAAA;;AACA,MAAA,KAAA,GAAA,OAAA,CAAA,YAAA,CAAA;;AAEA,MAAM,IAAI,GAAG,QAAQ,CAAC,cAAT,CAAwB,MAAxB,CAAb;;AACA,IAAI,IAAI,IAAI,IAAZ,EAAkB;AAChB,EAAA,QAAQ,CAAC,MAAT,CAAgB,CAAA,GAAA,aAAA,CAAA,GAAA,EAAC,KAAA,CAAA,GAAD,EAAI;AAAA,gBAAU;AAAV,GAAJ,CAAhB,EAAgD,IAAhD;AACD,C",
      "sourcesContent": [
        "import * as React from \\"react\\";\\nimport * as ReactDOM from \\"react-dom\\";\\nimport { App } from \\"../src/app\\";\\n\\nconst root = document.getElementById(\\"root\\");\\nif (root != null) {\\n  ReactDOM.render(<App data-uid='preview-app' />, root);\\n}"
      ],
      "sourceRoot": "",
      "file": "index.js"
    }
  },
  "/utopia/storyboard.js": {
    "errors": [],
    "transpiledCode": "\\"use strict\\";\\n\\nvar __createBinding = this && this.__createBinding || (Object.create ? function (o, m, k, k2) {\\n  if (k2 === undefined) k2 = k;\\n  var desc = Object.getOwnPropertyDescriptor(m, k);\\n\\n  if (!desc || (\\"get\\" in desc ? !m.__esModule : desc.writable || desc.configurable)) {\\n    desc = {\\n      enumerable: true,\\n      get: function () {\\n        return m[k];\\n      }\\n    };\\n  }\\n\\n  Object.defineProperty(o, k2, desc);\\n} : function (o, m, k, k2) {\\n  if (k2 === undefined) k2 = k;\\n  o[k2] = m[k];\\n});\\n\\nvar __setModuleDefault = this && this.__setModuleDefault || (Object.create ? function (o, v) {\\n  Object.defineProperty(o, \\"default\\", {\\n    enumerable: true,\\n    value: v\\n  });\\n} : function (o, v) {\\n  o[\\"default\\"] = v;\\n});\\n\\nvar __importStar = this && this.__importStar || function (mod) {\\n  if (mod && mod.__esModule) return mod;\\n  var result = {};\\n  if (mod != null) for (var k in mod) if (k !== \\"default\\" && Object.prototype.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);\\n\\n  __setModuleDefault(result, mod);\\n\\n  return result;\\n};\\n\\nObject.defineProperty(exports, \\"__esModule\\", {\\n  value: true\\n});\\nexports.storyboard = void 0;\\n\\nconst jsx_runtime_1 = require(\\"react/jsx-runtime\\");\\n\\nconst React = __importStar(require(\\"react\\"));\\n\\nconst utopia_api_1 = require(\\"utopia-api\\");\\n\\nconst app_js_1 = require(\\"/src/app.js\\");\\n\\nexports.storyboard = (0, jsx_runtime_1.jsx)(utopia_api_1.Storyboard, {\\n  \\"data-uid\\": 'sample-storyboard',\\n  children: (0, jsx_runtime_1.jsx)(utopia_api_1.Scene, {\\n    \\"data-uid\\": 'sample-scene',\\n    style: {\\n      position: 'absolute',\\n      left: 0,\\n      top: 0,\\n      width: 375,\\n      height: 812\\n    },\\n    children: (0, jsx_runtime_1.jsx)(app_js_1.App, {\\n      \\"data-uid\\": 'sample-app'\\n    })\\n  })\\n}); //# sourceMappingURL=storyboard.js.map",
    "sourceMap": {
      "version": 3,
      "sources": [
        "../../utopia/storyboard.js"
      ],
      "names": [],
      "mappings": ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;AACA,MAAA,KAAA,GAAA,YAAA,CAAA,OAAA,CAAA,OAAA,CAAA,CAAA;;AACA,MAAA,YAAA,GAAA,OAAA,CAAA,YAAA,CAAA;;AAIA,MAAA,QAAA,GAAA,OAAA,CAAA,aAAA,CAAA;;AACW,OAAA,CAAA,UAAA,GACT,CAAA,GAAA,aAAA,CAAA,GAAA,EAAC,YAAA,CAAA,UAAD,EAAW;AAAA,cAAU,mBAAV;AAA6B,EAAA,QAAA,EACtC,CAAA,GAAA,aAAA,CAAA,GAAA,EAAC,YAAA,CAAA,KAAD,EAAM;AAAA,gBACK,cADL;AAEJ,IAAA,KAAK,EAAE;AAAE,MAAA,QAAQ,EAAE,UAAZ;AAAwB,MAAA,IAAI,EAAE,CAA9B;AAAiC,MAAA,GAAG,EAAE,CAAtC;AAAyC,MAAA,KAAK,EAAE,GAAhD;AAAqD,MAAA,MAAM,EAAE;AAA7D,KAFH;AAEqE,IAAA,QAAA,EAEzE,CAAA,GAAA,aAAA,CAAA,GAAA,EAAC,QAAA,CAAA,GAAD,EAAI;AAAA,kBAAU;AAAV,KAAJ;AAJI,GAAN;AADS,CAAX,CADS,C",
      "sourcesContent": [
        "\\nimport * as React from 'react'\\nimport Utopia, {\\n  Scene,\\n  Storyboard,\\n} from 'utopia-api'\\nimport { App } from '/src/app.js'\\nexport var storyboard = (\\n  <Storyboard data-uid='sample-storyboard'>\\n    <Scene\\n      data-uid='sample-scene'\\n      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}\\n    >\\n      <App data-uid='sample-app' />\\n    </Scene>\\n  </Storyboard>\\n)\\n\\n"
      ],
      "sourceRoot": "",
      "file": "storyboard.js"
    }
  }
}`)

export const SampleFileBundledExportsInfo: Array<ExportsInfo> = JSON.parse(`[
  {
    "filename": "/src/app.js",
    "code": "\\nimport * as React from 'react'\\nexport var App = (props) => {\\n  return (\\n    <div\\n      style={{ width: '100%', height: '100%', backgroundColor: '#FFFFFF', position: 'relative' }}\\n    />\\n  )\\n}",
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
      }
    }
  },
  {
    "filename": "/src/index.js",
    "code": "import * as React from \\"react\\";\\nimport * as ReactDOM from \\"react-dom\\";\\nimport { App } from \\"../src/app\\";\\n\\nconst root = document.getElementById(\\"root\\");\\nif (root != null) {\\n  ReactDOM.render(<App data-uid='preview-app' />, root);\\n}",
    "exportTypes": {}
  },
  {
    "filename": "/utopia/storyboard.js",
    "code": "\\nimport * as React from 'react'\\nimport Utopia, {\\n  Scene,\\n  Storyboard,\\n} from 'utopia-api'\\nimport { App } from '/src/app.js'\\nexport var storyboard = (\\n  <Storyboard data-uid='sample-storyboard'>\\n    <Scene\\n      data-uid='sample-scene'\\n      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}\\n    >\\n      <App data-uid='sample-app' />\\n    </Scene>\\n  </Storyboard>\\n)\\n\\n",
    "exportTypes": {
      "storyboard": {
        "type": "Element",
        "functionInfo": null,
        "reactClassInfo": null
      }
    }
  }
]`)
