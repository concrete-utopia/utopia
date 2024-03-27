// Extracted by querying https://SERVER/v1/project/PROJECT_ID/contents.json/projectContents
// Once prettier formats the JSON to a JavaScript value then enums are replaced.

import type { ProjectContentTreeRoot } from '../components/assets'
import { RevisionsState } from '../core/shared/project-file-types'

export const LargeProjectContents: ProjectContentTreeRoot = {
  'package.json': {
    content: {
      fileContents: {
        code: '{\n  "name": "Utopia Project",\n  "version": "0.1.0",\n  "utopia": {\n    "main-ui": "utopia/storyboard.js",\n    "html": "public/index.html",\n    "js": "src/index.js"\n  },\n  "dependencies": {\n    "react": "16.13.1",\n    "react-dom": "16.13.1",\n    "utopia-api": "0.4.1"\n  }\n}',
        revisionsState: RevisionsState.CodeAhead,
        parsed: {
          type: 'UNPARSED',
        },
      },
      lastSavedContents: null,
      versionNumber: 0,
      type: 'TEXT_FILE',
      lastParseSuccess: null,
    },
    type: 'PROJECT_CONTENT_FILE',
    fullPath: '/package.json',
  },
  utopia: {
    children: {
      'storyboard.js': {
        content: {
          fileContents: {
            code: "import * as React from 'react'\nimport Utopia, {\n  Scene,\n  Storyboard,\n} from 'utopia-api'\nimport {\n  App,\n  AppTheSecond,\n  AppTheThird,\n  AppTheFourth,\n  AppTheFifth,\n  AppTheSixth,\n  AppTheSeventh,\n} from '/src/app.js'\n\nimport '/public/style.css'\n\nexport var storyboard = (\n  <Storyboard className='storyboard'>\n    <div style={{ display: 'flex', gap: 600 }}>\n      <Scene style={{ width: 1080, height: 2340 }}>\n        <AppTheSecond />\n      </Scene>\n      <Scene style={{ width: 1080, height: 2340 }}>\n        <AppTheThird />\n      </Scene>\n      <Scene style={{ width: 1080, height: 2340 }}>\n        <AppTheFourth />\n      </Scene>\n      <Scene style={{ width: 1080, height: 2340 }}>\n        <AppTheFifth />\n      </Scene>\n      <Scene style={{ width: 1080, height: 2340 }}>\n        <AppTheSixth />\n      </Scene>\n      <Scene style={{ width: 1080, height: 2340 }}>\n        <App />\n      </Scene>\n\n      <Scene style={{ width: 1080, height: 2340 }}>\n        <AppTheSeventh />\n      </Scene>\n    </div>\n  </Storyboard>\n)\n",
            revisionsState: RevisionsState.CodeAhead,
            parsed: {
              type: 'UNPARSED',
            },
          },
          lastSavedContents: null,
          versionNumber: 0,
          type: 'TEXT_FILE',
          lastParseSuccess: null,
        },
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/utopia/storyboard.js',
      },
    },
    type: 'PROJECT_CONTENT_DIRECTORY',
    directory: {
      type: 'DIRECTORY',
    },
    fullPath: '/utopia',
  },
  src: {
    children: {
      'app.js': {
        content: {
          fileContents: {
            code: "import * as React from 'react'\nimport {\n  SampleCard,\n  SampleButton,\n  SampleStory,\n  SampleApp1,\n  SampleApp2,\n  SampleAppMini,\n  SamplePost,\n  SampleCard2,\n  SampleCard3,\n} from './sample.js'\nimport '/public/style.css'\n\nexport var App = (props) => {\n  return (\n    <div className='scene'>\n      <div\n        className='navBar'\n        style={{\n          display: 'flex',\n          justifyContent: 'center',\n          alignItems: 'center',\n          flexGrow: 1,\n        }}\n      >\n        <div\n          className='sampleSearchBar'\n          style={{ fontSize: '50px' }}\n        >\n          🔎\n        </div>\n      </div>\n      <div\n        className='bodycontainer'\n        style={{\n          flexGrow: 20,\n          padding: 50,\n          justifyContent: 'start',\n          flexDirection: 'column',\n          overflow: 'hidden',\n        }}\n      >\n        <SampleCard />\n\n        <div\n          style={{\n            display: 'flex',\n            flexDirection: 'row',\n            justifyContent: 'space-between',\n            gap: 30,\n            flexWrap: 'wrap',\n          }}\n        >\n          <SampleApp2 />\n          <SampleApp2 />\n          <SampleApp2 />\n        </div>\n      </div>\n      <div\n        className='footer'\n        style={{\n          width: '100%',\n          display: 'flex',\n          flexDirection: 'row',\n          justifyContent: 'space-between',\n          alignSelf: 'flex-end',\n          flexGrow: 1,\n        }}\n      >\n        <SampleButton />\n        <SampleButton />\n        <SampleButton />\n        <SampleButton />\n        <SampleButton />\n      </div>\n    </div>\n  )\n}\n\nexport var AppTheSecond = (props) => {\n  return (\n    <div className='scene'>\n      <div\n        className='navBar'\n        style={{ height: '8%', padding: 20, gap: 20 }}\n      >\n        <h1\n          style={{\n            width: '250%',\n            display: 'flex',\n            alignItems: 'center',\n            justifyContent: 'center',\n          }}\n        >\n          Logo\n        </h1>\n        <SampleButton />\n        <SampleButton />\n        <SampleButton />\n      </div>\n      <div className='stories'>\n        <SampleStory />\n        <SampleStory />\n        <SampleStory />\n        <SampleStory />\n        <SampleStory />\n        <SampleStory />\n        <SampleStory />\n        <SampleStory />\n      </div>\n      <div className='bodycontainer'>\n        <SampleCard />\n        <SampleCard />\n      </div>\n      <div\n        className='footer'\n        style={{ height: '8%', padding: 20, gap: 20 }}\n      >\n        <SampleButton />\n        <SampleButton />\n        <SampleButton />\n        <SampleButton />\n        <SampleButton />\n      </div>\n    </div>\n  )\n}\n\nexport var AppTheThird = (props) => {\n  return (\n    <div className='scene'>\n      <div\n        className='a3header'\n        style={{\n          width: '100%',\n          padding: 20,\n          display: 'flex',\n          flexDirection: 'row',\n          justifyContent: 'space-between',\n          fontSize: '30pt',\n        }}\n      >\n        <div style={{ width: '30%' }}>AT&T</div>\n        <div style={{ width: '30%', textAlign: 'center' }}>\n          12:30\n        </div>\n        <div style={{ width: '30%', textAlign: 'right' }}>\n          83%\n        </div>\n      </div>\n      <div\n        className='a3 bodycontainer'\n        style={{ justifyContent: 'flex-start' }}\n      >\n        <SampleApp1 />\n        <SampleApp1 />\n        <SampleApp1 />\n        <SampleApp1 />\n        <SampleApp1 />\n        <SampleApp1 />\n        <SampleApp1 />\n        <SampleApp1 />\n        <SampleApp1 />\n        <SampleApp1 />\n        <SampleApp1 />\n        <SampleApp1 />\n        <SampleApp1 />\n        <SampleApp1 />\n        <SampleApp1 />\n        <SampleApp1 />\n        <SampleApp1 />\n        <SampleApp1 />\n        <SampleApp1 />\n        <SampleApp1 />\n        <SampleApp1 />\n        <SampleApp1 />\n        <SampleApp1 />\n        <div className='appFolder sampleApp'>\n          <SampleAppMini />\n          <SampleAppMini />\n          <SampleAppMini />\n        </div>\n        <div className='appFolder sampleApp'>\n          <SampleAppMini />\n          <SampleAppMini />\n          <SampleAppMini />\n          <SampleAppMini />\n          <SampleAppMini />\n          <SampleAppMini />\n          <SampleAppMini />\n        </div>\n        <div className='appFolder sampleApp'>\n          <SampleAppMini />\n          <SampleAppMini />\n          <SampleAppMini />\n          <SampleAppMini />\n          <SampleAppMini />\n          <SampleAppMini />\n          <SampleAppMini />\n          <SampleAppMini />\n          <SampleAppMini />\n        </div>\n      </div>\n      <div className='a3bottomnav'>\n        <SampleApp1 />\n        <SampleApp1 />\n        <SampleApp1 />\n        <SampleApp1 />\n      </div>\n    </div>\n  )\n}\n\nexport var AppTheFourth = (props) => {\n  return (\n    <div className='scene'>\n      <div className='navBar'>\n        <div\n          style={{\n            display: 'flex',\n            alignItems: 'center',\n            width: '50%',\n            justifyContent: 'center',\n          }}\n        >\n          <div style={{ fontSize: '100pt' }}>Logo</div>\n        </div>\n        <div\n          style={{ display: 'flex', gap: 0, width: '50%' }}\n        >\n          <SampleButton />\n          <SampleButton />\n          <SampleButton />\n        </div>\n      </div>\n      <div\n        className='card-container'\n        style={{ height: '80%' }}\n      >\n        <SamplePost />\n        <SamplePost />\n        <SamplePost style={{ gap: 0 }} />\n        <SamplePost />\n        <SamplePost />\n        <SamplePost />\n      </div>\n      <div className='footer'>\n        <SampleButton />\n        <SampleButton />\n        <SampleButton />\n        <SampleButton />\n        <SampleButton />\n      </div>\n    </div>\n  )\n}\n\nexport var AppTheFifth = (props) => {\n  return (\n    <div className='scene'>\n      <div\n        className='navBar'\n        style={{\n          display: 'flex',\n          alignItems: 'center',\n          justifyContent: 'center',\n        }}\n      >\n        <h1>Time</h1>\n      </div>\n      <div\n        className='card-container'\n        style={{ overflow: 'hidden', height: '90%' }}\n      >\n        <SampleCard2 />\n        <SampleCard2 />\n        <SampleCard2 />\n        <SampleCard2 />\n        <SampleCard2 />\n      </div>\n    </div>\n  )\n}\n\nexport var AppTheSixth = (props) => {\n  return (\n    <div className='scene'>\n      <div className='navBar'>\n        <SampleButton />\n        <SampleButton />\n        <SampleButton />\n      </div>\n      <div\n        className='card-container'\n        style={{ overflow: 'hidden', height: '90%' }}\n      >\n        <SampleCard3 />\n        <SampleCard3 />\n        <SampleCard3 />\n        <SampleCard3 />\n      </div>\n    </div>\n  )\n}\n\nexport var AppTheSeventh = (props) => {\n  return (\n    <div className='scene'>\n      <div className='navBar'>\n        <SampleButton />\n        <SampleButton />\n        <SampleButton />\n      </div>\n      <div\n        className='card-container'\n        style={{ overflow: 'hidden', height: '90%' }}\n      >\n        <SampleCard3 />\n        <SampleCard3 />\n        <SampleCard3 />\n        <SampleCard3 />\n      </div>\n    </div>\n  )\n}\n",
            revisionsState: RevisionsState.CodeAhead,
            parsed: {
              type: 'UNPARSED',
            },
          },
          lastSavedContents: null,
          versionNumber: 0,
          type: 'TEXT_FILE',
          lastParseSuccess: null,
        },
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/src/app.js',
      },
      'sample.js': {
        content: {
          fileContents: {
            code: "import * as React from 'react'\n\nconst SampleImage = () => (\n  <img\n    className='sampleImage sampleImage100'\n    src='https://images.pexels.com/photos/1450372/pexels-photo-1450372.jpeg?auto=compress&cs=tinysrgb&dpr=2&h=750&w=1260'\n    alt='beach'\n  />\n)\n\nconst SampleImage50 = () => (\n  <img\n    className='sampleImage sampleImage50'\n    src='https://images.pexels.com/photos/1450372/pexels-photo-1450372.jpeg?auto=compress&cs=tinysrgb&dpr=2&h=750&w=1260'\n    alt='beach'\n  />\n)\n\nexport var SampleCard = (props) => {\n  return (\n    <div className='sampleCard'>\n      <div\n        style={{\n          display: 'flex',\n          justifyContent: 'space-between',\n          alignItems: 'baseline',\n          padding: '10px',\n        }}\n      >\n        <h1>Title</h1>\n        <h2>February 11, 2022</h2>\n      </div>\n      <SampleImage />\n    </div>\n  )\n}\n\nexport var SampleButton = (props) => {\n  return (\n    <div className='sampleButton'>\n      <h1>X</h1>\n    </div>\n  )\n}\n\nexport var SampleStory = (props) => {\n  return <div className='sampleStory' />\n}\n\nexport var SampleApp1 = (props) => {\n  return <div className='sampleApp sampleApp1'></div>\n}\n\nexport var SampleApp2 = (props) => {\n  return (\n    <div className='sampleApp sampleApp2'>\n      <SampleApp1 />\n      <SampleApp1 />\n      <SampleApp1 />\n      <SampleApp1 />\n    </div>\n  )\n}\n\nexport var SampleAppMini = (props) => {\n  return <div className='sampleAppMini'></div>\n}\n\nexport var SamplePost = (props) => {\n  return (\n    <div className='card'>\n      <SampleImage />\n      <br />\n      <div>Title</div>\n      <br />\n      <div>\n        description description description description\n      </div>\n      <br />\n      <div className='action-buttons'>\n        <SampleButton />\n        <SampleButton />\n        <SampleButton />\n      </div>\n    </div>\n  )\n}\n\nexport var SampleCard2 = (props) => {\n  return (\n    <div className='sampleCard2'>\n      <SampleImage50 />\n      <div\n        style={{\n          padding: 40,\n          display: 'flex',\n          flexDirection: 'column',\n          width: '100%',\n          height: 'auto',\n          justifyContent: 'space-between',\n        }}\n      >\n        <div>Title</div>\n        <div>\n          description description description description\n          description description description description\n          description description description description\n          description description description description\n          description description description description\n          description\n        </div>\n        <div className='action-buttons'>\n          <SampleButton />\n          <SampleButton />\n          <SampleButton />\n          <SampleButton />\n        </div>\n      </div>\n    </div>\n  )\n}\n\nexport var SampleCard3 = (props) => {\n  return (\n    <div\n      className='sampleCard3'\n      style={{\n        flexDirection: 'column',\n        width: '100%',\n        height: 'auto',\n        justifyContent: 'space-between',\n      }}\n    >\n      <div style={{ paddingBottom: 20 }}>\n        Wednesday February 16, 2022\n      </div>\n      <div style={{ paddingBottom: 20 }}>\n        Turks and Caicos Islands\n      </div>\n      <div\n        style={{\n          display: 'flex',\n          justifyContent: 'space-between',\n        }}\n      >\n        <SampleImage50 />\n        <div\n          style={{\n            display: 'flex',\n            width: '65%',\n            background: 'pink',\n            padding: 20,\n            gap: 10,\n            alignItems: 'start',\n            justifyContent: 'flex-start',\n            flexWrap: 'wrap',\n          }}\n        >\n          <SampleTime />\n          <SampleTime />\n          <SampleTime />\n          <SampleTime />\n          <SampleTime />\n          <SampleTime />\n          <SampleTime />\n\n          <SampleTime style={{ marginRight: 'auto' }} />\n        </div>\n      </div>\n    </div>\n  )\n}\n\nexport var SampleTime = (props) => {\n  return (\n    <div\n      style={{\n        display: 'flex',\n        flexGrow: 1,\n        flexWrap: 'wrap',\n        height: '100px',\n        background: 'white',\n        justifyContent: 'center',\n        alignItems: 'center',\n      }}\n    >\n      10:30\n    </div>\n  )\n}\n",
            revisionsState: RevisionsState.CodeAhead,
            parsed: {
              type: 'UNPARSED',
            },
          },
          lastSavedContents: null,
          versionNumber: 0,
          type: 'TEXT_FILE',
          lastParseSuccess: null,
        },
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/src/sample.js',
      },
      'index.js': {
        content: {
          fileContents: {
            code: 'import * as React from "react";\nimport * as ReactDOM from "react-dom";\nimport { App } from "../src/app";\n\nconst root = document.getElementById("root");\nif (root != null) {\n  ReactDOM.render(<App />, root);\n}',
            revisionsState: RevisionsState.CodeAhead,
            parsed: {
              type: 'UNPARSED',
            },
          },
          lastSavedContents: null,
          versionNumber: 0,
          type: 'TEXT_FILE',
          lastParseSuccess: null,
        },
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/src/index.js',
      },
    },
    type: 'PROJECT_CONTENT_DIRECTORY',
    directory: {
      type: 'DIRECTORY',
    },
    fullPath: '/src',
  },
  assets: {
    children: {},
    type: 'PROJECT_CONTENT_DIRECTORY',
    directory: {
      type: 'DIRECTORY',
    },
    fullPath: '/assets',
  },
  public: {
    children: {
      'index.html': {
        content: {
          fileContents: {
            code: '<!DOCTYPE html>\n<html lang="en">\n  <head>\n    <meta charset="utf-8">\n    <title>Utopia React App</title>\n    <!-- Begin Generated Utopia External Links -->\n    <!-- End Generated Utopia External Links -->\n  </head>\n  <body>\n    <div id="root"></div>\n  </body>\n</html>',
            revisionsState: RevisionsState.CodeAhead,
            parsed: {
              type: 'UNPARSED',
            },
          },
          lastSavedContents: null,
          versionNumber: 0,
          type: 'TEXT_FILE',
          lastParseSuccess: null,
        },
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/public/index.html',
      },
      'style.css': {
        content: {
          fileContents: {
            code: ':root {\n    /* postcard */\n    --scene: #fffdf8; \n\n    /* caribbean */\n    --parent: #5de5ee;\n\n    /* idea */\n    --child: #fff49c;\n\n    /* sunset */\n    --grandchild: #ffa47c;\n\n    /* flamingo */\n    --greatgrandchild: #fe9ecd;\n\n    --sand: #fff7da;\n    --dust: #444444;\n  }\n\n.scene {\n    font-family: Arial, Helvetica, sans-serif;\n    \n    width: 100%;\n    height: 100%;\n\n    display: flex;\n        flex-direction: column;\n        gap: 10;\n        justify-content: space-between;\n        position: relative;\n        flex-wrap: wrap;\n        align-items: center;\n\n    background-color: var(--scene); \n    color: var(--dust);\n   \n}\n\n.sampleImage {\n    object-fit: cover;\n    \n}\n\n.sampleImage100 {\n    height: 100%;\n    width: 100%;\n}\n\n.sampleImage50 {\n    height: 50%;\n    width: 50%;\n}\n\n.navBar {\n    width: 100%;\n    height: 10%;\n    display: flex;\n    justify-content: space-between;\n    background-color: var(--parent)\n}\n\n.footer {\n    width: 100%;\n    height: 10%;\n    display: flex;\n    justify-content: space-between;\n    /* gap: 10px; */\n    background-color: var(--parent)\n\n}\n\n.card-container {\n  display: flex;\n  flex-direction: row;\n  flex-wrap: wrap;\n\n  border: 1px solid var(--dust);\n  overflow-y: scroll;\n\n  justify-content: center;\n  align-items: center;\n  background-color: var(--parent);\n}\n\n.card {\n    display: flex;\n    flex-direction: column;\n    \n    width: 40%;\n    background-color: var(--child);\n    padding: 10px;\n    margin: 20px;\n}\n\n.action-buttons {\n    display: flex;\n    flex-direction: row;\n    justify-content: space-between;\n\n}\n\n.sampleButton {\n    background-color: var(--grandchild);\n    border: 1px solid var(--dust);\n    display: flex;\n    flex-direction: column;\n\n    width: 100%;\n\n    justify-content: center;\n    align-items: center;\n}\n\n.sampleCard {\n    display: flex;\n    flex-direction: column;\n}\n\n.sampleCard2 {\n    background-color: var(--child);\n    display: flex;\n    margin: 40px;\n    min-height: 0;\n}\n\n.sampleCard3 {\n    background-color: var(--child);\n    display: flex;\n    \n    margin: 40px;\n    padding: 40px;\n    min-height: 0;\n}\n\n.a3bottomnav {\n    display: flex;\n    width: 100%;\n    flex-direction: row;\n    justify-content: space-between;\n    padding: 50px;\n    background-color: var(--parent);\n}\n\n.bodycontainer {\n    display: flex;\n    flex-direction: row;\n    justify-content: flex-start;\n    align-items: center;\n    flex-wrap: wrap;\n    gap: 60px;\n    overflow: hidden;\n    background-color: var(--parent);\n    border: 1px solid var(--dust);\n}\n\n.a3 { \n    padding: 40px;\n}\n\n.a3header {\n    background-color: var(--parent);\n}\n\n.sampleApp {\n    background-color: var(--child);\n    border: 3px solid rgb(0, 0, 0, 1);\n}\n\n.appFolder, .sampleApp1 {\n    height: 200px;\n    width: 200px;\n    border-radius: 30px;\n    display: flex;\n    align-items: flex-start;\n    flex-wrap: wrap;\n    padding: 10px;\n    gap: 10px;\n}\n\n.sampleApp2 {\n    height: 460px;\n    width: 460px;\n    border-radius: 60px;\n    padding: 2px;\n    display: flex;\n    flex-direction: row;\n    align-items: flex-start;\n    flex-wrap: wrap;\n    padding: 20px;\n    gap: 10px;\n}\n\n.sampleAppMini {\n    background-color: var(--grandchild);\n    border: 3px solid rgb(0, 0, 0, 1);\n    height: 50px;\n    width: 50px;\n    border-radius: 15px;\n}\n\n.stories {\n    background-color: var(--parent);\n    display: flex;\n    flex-direction: row;\n    padding: 15px;\n    width: 100%;\n    height: auto;\n    overflow-x: scroll;\n}\n\n.sampleStory {\n    background-color: var(--child);\n    border: 1px solid var(--dust);\n    height: 100px;\n    width: 100px;\n    border-radius: 50px;\n    margin: 15px;\n}\n\n.sampleSearchBar {\n    width: 100%;\n    height: 50%;\n    background-color: var(--child);\n    border-radius: 70px;\n    display: flex;\n    align-items: center;\n    justify-content: end;\n    margin: 50px;\n    padding: 50px;\n}',
            revisionsState: RevisionsState.CodeAhead,
            parsed: {
              type: 'UNPARSED',
            },
          },
          lastSavedContents: null,
          versionNumber: 0,
          type: 'TEXT_FILE',
          lastParseSuccess: null,
        },
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/public/style.css',
      },
    },
    type: 'PROJECT_CONTENT_DIRECTORY',
    directory: {
      type: 'DIRECTORY',
    },
    fullPath: '/public',
  },
}
