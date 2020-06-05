import { ExportsInfo } from '../core/workers/ts/ts-worker'

// If you wish to update this auto-generated file, go to codeBundle.spec.ts and find the test called
// 'Update Saved Bundle' and change the test function from xit(... to it(... to enable it

export const SampleFileBuildResult = JSON.parse(`{
  "/src/app.ui.js": {
    "errors": [],
    "transpiledCode": "System.register([\\"utopia-api\\"], function (exports_1, context_1) {\\n  \\"use strict\\";\\n\\n  var utopia_api_1, App, storyboard;\\n\\n  var __moduleName = context_1 && context_1.id;\\n\\n  return {\\n    setters: [function (utopia_api_1_1) {\\n      utopia_api_1 = utopia_api_1_1;\\n    }],\\n    execute: function execute() {\\n      exports_1(\\"App\\", App = function App(props) {\\n        return utopia_api_1.jsx(utopia_api_1.View, {\\n          style: Object.assign(Object.assign({}, props.style || {}), {\\n            backgroundColor: '#FFFFFF'\\n          }),\\n          layout: {\\n            layoutSystem: 'pinSystem'\\n          }\\n        });\\n      });\\n      exports_1(\\"storyboard\\", storyboard = utopia_api_1.jsx(utopia_api_1.Storyboard, {\\n        layout: {\\n          layoutSystem: 'pinSystem'\\n        }\\n      }, utopia_api_1.jsx(utopia_api_1.Scene, {\\n        component: App,\\n        props: {\\n          style: {\\n            top: 0,\\n            left: 0,\\n            bottom: 0,\\n            right: 0\\n          }\\n        },\\n        style: {\\n          left: 0,\\n          top: 0,\\n          width: 375,\\n          height: 812\\n        },\\n        layout: {\\n          layoutSystem: 'pinSystem'\\n        }\\n      })));\\n    }\\n  };\\n}); //# sourceMappingURL=app.ui.js.map",
    "sourceMap": {
      "version": 3,
      "sources": [
        "../../src/app.ui.js"
      ],
      "names": [],
      "mappings": ";;;;;;;;;;;;AAGA,MAAA,SAAA,CAAA,KAAA,EAAW,GAAG,GAAG,aAAC,KAAD,EAAU;AACzB,eACE,YAAA,CAAA,GAAA,CAAC,YAAA,CAAA,IAAD,EAAK;AACH,UAAA,KAAK,EAAA,MAAA,CAAA,MAAA,CAAA,MAAA,CAAA,MAAA,CAAA,EAAA,EAAQ,KAAK,CAAC,KAAN,IAAe,EAAvB,CAAA,EAA0B;AAAE,YAAA,eAAe,EAAE;AAAnB,WAA1B,CADF;AAEH,UAAA,MAAM,EAAE;AAAE,YAAA,YAAY,EAAE;AAAhB;AAFL,SAAL,CADF;AAMD,OAPD,CAAA;AAQA,MAAA,SAAA,CAAA,YAAA,EAAW,UAAU,GACnB,YAAA,CAAA,GAAA,CAAC,YAAA,CAAA,UAAD,EAAW;AAAC,QAAA,MAAM,EAAE;AAAE,UAAA,YAAY,EAAE;AAAhB;AAAT,OAAX,EACE,YAAA,CAAA,GAAA,CAAC,YAAA,CAAA,KAAD,EAAM;AACJ,QAAA,SAAS,EAAE,GADP;AAEJ,QAAA,KAAK,EAAE;AAAE,UAAA,KAAK,EAAE;AAAE,YAAA,GAAG,EAAE,CAAP;AAAU,YAAA,IAAI,EAAE,CAAhB;AAAmB,YAAA,MAAM,EAAE,CAA3B;AAA8B,YAAA,KAAK,EAAE;AAArC;AAAT,SAFH;AAGJ,QAAA,KAAK,EAAE;AAAE,UAAA,IAAI,EAAE,CAAR;AAAW,UAAA,GAAG,EAAE,CAAhB;AAAmB,UAAA,KAAK,EAAE,GAA1B;AAA+B,UAAA,MAAM,EAAE;AAAvC,SAHH;AAIJ,QAAA,MAAM,EAAE;AAAE,UAAA,YAAY,EAAE;AAAhB;AAJJ,OAAN,CADF,CADF,CAAA;AAWC",
      "sourcesContent": [
        "/** @jsx jsx */\\nimport * as React from 'react'\\nimport { Scene, Storyboard, View, jsx } from 'utopia-api'\\nexport var App = (props) => {\\n  return (\\n    <View\\n      style={{ ...(props.style || {}), backgroundColor: '#FFFFFF' }}\\n      layout={{ layoutSystem: 'pinSystem' }}\\n    />\\n  )\\n}\\nexport var storyboard = (\\n  <Storyboard layout={{ layoutSystem: 'pinSystem' }}>\\n    <Scene\\n      component={App}\\n      props={{ style: { top: 0, left: 0, bottom: 0, right: 0 } }}\\n      style={{ left: 0, top: 0, width: 375, height: 812 }}\\n      layout={{ layoutSystem: 'pinSystem' }}\\n    />\\n  </Storyboard>\\n)\\n\\n"
      ],
      "sourceRoot": "",
      "file": "app.ui.js"
    }
  },
  "/public/preview.jsx": {
    "errors": [],
    "transpiledCode": "System.register([\\"react\\", \\"react-dom\\", \\"../src/app.ui\\"], function (exports_1, context_1) {\\n  \\"use strict\\";\\n\\n  var React, ReactDOM, app_ui_1, root;\\n\\n  var __moduleName = context_1 && context_1.id;\\n\\n  return {\\n    setters: [function (React_1) {\\n      React = React_1;\\n    }, function (ReactDOM_1) {\\n      ReactDOM = ReactDOM_1;\\n    }, function (app_ui_1_1) {\\n      app_ui_1 = app_ui_1_1;\\n    }],\\n    execute: function execute() {\\n      root = document.getElementById(\\"root\\");\\n\\n      if (root != null) {\\n        ReactDOM.render(React.createElement(app_ui_1.App, null), root);\\n      }\\n    }\\n  };\\n}); //# sourceMappingURL=preview.js.map",
    "sourceMap": {
      "version": 3,
      "sources": [
        "../src/index.js"
      ],
      "names": [],
      "mappings": ";;;;;;;;;;;;;;;;AAIM,MAAA,IAAI,GAAG,QAAQ,CAAC,cAAT,CAAwB,MAAxB,CAAP;;AACN,UAAI,IAAI,IAAI,IAAZ,EAAkB;AAChB,QAAA,QAAQ,CAAC,MAAT,CAAgB,KAAA,CAAA,aAAA,CAAC,QAAA,CAAA,GAAD,EAAI,IAAJ,CAAhB,EAAyB,IAAzB;AACD;AAAC",
      "sourcesContent": [
        "import * as React from \\"react\\";\\nimport * as ReactDOM from \\"react-dom\\";\\nimport { App } from \\"../src/app.ui\\";\\n\\nconst root = document.getElementById(\\"root\\");\\nif (root != null) {\\n  ReactDOM.render(<App />, root);\\n}"
      ],
      "sourceRoot": "",
      "file": "index.js"
    }
  }
}`)

export const SampleFileBundledExportsInfo: Array<ExportsInfo> = JSON.parse(`[
  {
    "filename": "/src/app.ui.js",
    "code": "/** @jsx jsx */\\nimport * as React from 'react'\\nimport { Scene, Storyboard, View, jsx } from 'utopia-api'\\nexport var App = (props) => {\\n  return (\\n    <View\\n      style={{ ...(props.style || {}), backgroundColor: '#FFFFFF' }}\\n      layout={{ layoutSystem: 'pinSystem' }}\\n    />\\n  )\\n}\\nexport var storyboard = (\\n  <Storyboard layout={{ layoutSystem: 'pinSystem' }}>\\n    <Scene\\n      component={App}\\n      props={{ style: { top: 0, left: 0, bottom: 0, right: 0 } }}\\n      style={{ left: 0, top: 0, width: 375, height: 812 }}\\n      layout={{ layoutSystem: 'pinSystem' }}\\n    />\\n  </Storyboard>\\n)\\n\\n",
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
