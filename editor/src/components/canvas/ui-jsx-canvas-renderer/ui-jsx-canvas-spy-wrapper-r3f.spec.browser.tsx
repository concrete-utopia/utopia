import * as ReactThreeFiber from '@react-three/fiber'
import * as editorPackageJSON from '../../../../package.json'

jest.mock('../../../core/es-modules/package-manager/built-in-dependencies-list', () => ({
  BuiltInDependencies: (mode: 'preview' | 'canvas'): Array<any> => [
    ...(jest.requireActual(
      '../../../core/es-modules/package-manager/built-in-dependencies-list',
    ) as any)['BuiltInDependencies']('canvas'),
    {
      moduleName: '@react-three/fiber',
      nodeModule: {
        ...ReactThreeFiber,
        default: ReactThreeFiber,
      },
      version: editorPackageJSON.devDependencies['@react-three/fiber'],
    },
  ],
}))

import { renderTestEditorWithModel } from '../ui-jsx.test-utils'
import {
  ParsedTextFile,
  isParseFailure,
  textFile,
  textFileContents,
  RevisionsState,
} from '../../../core/shared/project-file-types'
import { emptySet } from '../../../core/shared/set-utils'
import * as EP from '../../../core/shared/element-path'
import { lintAndParse } from '../../../core/workers/parser-printer/parser-printer'
import { defaultProject } from '../../../sample-projects/sample-project-utils'
import {
  wait,
  simplifiedMetadataMap,
  domWalkerMetadataToSimplifiedMetadataMap,
} from '../../../utils/utils.test-utils'
import { addFileToProjectContents } from '../../assets'
import { StoryboardFilePath } from '../../editor/store/editor-state'

const exampleFiles = {
  [StoryboardFilePath]: `
  import * as React from "react";
  import { Scene, Storyboard } from "utopia-api";
  import { CanvasApp } from "/src/app";

  export var App = (props) => {
    return (
      <div
        data-uid='app-root'
        style={{
          width: '100%',
          height: '100%',
          backgroundColor: '#FFFFFF',
          position: 'relative',
        }}
      >
        <div data-uid='app-inner-div'>hello other file app</div>
      </div>
    )
  };
  
  export var storyboard = (
    <Storyboard data-uid="storyboard">
      <Scene
        data-uid="scene-1"
        style={{ position: "absolute", left: 0, top: 0, width: 375, height: 812 }}
      >
        <CanvasApp data-uid="canvas-app" />
      </Scene>
      <Scene
        data-uid="scene-2"
        style={{ position: "absolute", left: 400, top: 0, width: 375, height: 812 }}
      >
        <App data-uid="app" />
      </Scene>
    </Storyboard>
  );
  `,
  '/src/app.js': `import * as React from "react";
  import { Canvas } from '@react-three/fiber';

  export const CanvasApp = () => {
    return (
      <Canvas data-uid='canvas-app-div'>
        <mesh
          visible
          userData={{ test: 'TestData' }}
          position={[0, 0, 0]}
          rotation={[0, 0, 0]}
          castShadow
          data-uid='test-mesh'
        >
          <sphereGeometry attach='geometry' args={[1, 16, 200]} data-uid='test-sphereGeometry' />
          <meshStandardMaterial
            attach='material'
            color='#7222D3'
            data-uid='test-meshStandardMaterial'
          />
        </mesh>
        <ambientLight intensity={0.5} data-uid='test-ambientLight' />
        <directionalLight position={[10, 10, 5]} intensity={1} data-uid='test-directionalLight' />
        <pointLight position={[0, -10, 5]} intensity={1} data-uid='test-pointLight' />
      </Canvas>
    );
  };
  `,
}

function createTestProject() {
  const baseModel = defaultProject()

  const updatedProject = Object.keys(exampleFiles).reduce((workingProject, modifiedFilename) => {
    const parsedFile = lintAndParse(
      modifiedFilename,
      exampleFiles[modifiedFilename],
      null,
      emptySet(),
    ) as ParsedTextFile
    if (isParseFailure(parsedFile)) {
      fail('The test file parse failed')
    }

    const updatedProjectContents = addFileToProjectContents(
      workingProject.projectContents,
      modifiedFilename,
      textFile(
        textFileContents(exampleFiles[modifiedFilename], parsedFile, RevisionsState.BothMatch),
        null,
        Date.now(),
      ),
    )

    return {
      ...baseModel,
      projectContents: updatedProjectContents,
    }
  }, baseModel)
  return renderTestEditorWithModel(updatedProject)
}

describe('Spy Wrapper Tests For React Three Fiber', () => {
  it('a simple @react-three/fiber Canvas in a scene', async () => {
    // Code kept commented for any future person who needs it.
    // const currentWindow = require('electron').remote.getCurrentWindow()
    // currentWindow.show()
    // currentWindow.setPosition(500, 200)
    // currentWindow.setSize(2200, 1000)
    // currentWindow.openDevTools()
    // await wait(20000)

    const { getEditorState } = await createTestProject()
    await wait(20000)
    const spiedMetadata = getEditorState().editor.spyMetadata
    const sanitizedSpyData = simplifiedMetadataMap(spiedMetadata)
    expect(sanitizedSpyData).toMatchInlineSnapshot(`
      Object {
        "storyboard": Object {
          "children": Array [
            "storyboard/scene-1",
            "storyboard/scene-2",
          ],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        "storyboard/scene-1": Object {
          "children": Array [
            "storyboard/scene-1/canvas-app",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-1/canvas-app": Object {
          "children": Array [],
          "name": "CanvasApp",
          "rootElements": Array [],
        },
        "storyboard/scene-1/canvas-app:canvas-app-div": Object {
          "children": Array [
            "storyboard/scene-1/canvas-app:canvas-app-div/test-mesh",
            "storyboard/scene-1/canvas-app:canvas-app-div/test-ambientLight",
            "storyboard/scene-1/canvas-app:canvas-app-div/test-directionalLight",
            "storyboard/scene-1/canvas-app:canvas-app-div/test-pointLight",
          ],
          "name": "Canvas",
          "rootElements": Array [],
        },
        "storyboard/scene-1/canvas-app:canvas-app-div/test-ambientLight": Object {
          "children": Array [],
          "name": "ambientLight",
          "rootElements": Array [],
        },
        "storyboard/scene-1/canvas-app:canvas-app-div/test-directionalLight": Object {
          "children": Array [],
          "name": "directionalLight",
          "rootElements": Array [],
        },
        "storyboard/scene-1/canvas-app:canvas-app-div/test-mesh": Object {
          "children": Array [
            "storyboard/scene-1/canvas-app:canvas-app-div/test-mesh/test-sphereGeometry",
            "storyboard/scene-1/canvas-app:canvas-app-div/test-mesh/test-meshStandardMaterial",
          ],
          "name": "mesh",
          "rootElements": Array [],
        },
        "storyboard/scene-1/canvas-app:canvas-app-div/test-mesh/test-meshStandardMaterial": Object {
          "children": Array [],
          "name": "meshStandardMaterial",
          "rootElements": Array [],
        },
        "storyboard/scene-1/canvas-app:canvas-app-div/test-mesh/test-sphereGeometry": Object {
          "children": Array [],
          "name": "sphereGeometry",
          "rootElements": Array [],
        },
        "storyboard/scene-1/canvas-app:canvas-app-div/test-pointLight": Object {
          "children": Array [],
          "name": "pointLight",
          "rootElements": Array [],
        },
        "storyboard/scene-2": Object {
          "children": Array [
            "storyboard/scene-2/app",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app": Object {
          "children": Array [],
          "name": "App",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app:app-root": Object {
          "children": Array [
            "storyboard/scene-2/app:app-root/app-inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app:app-root/app-inner-div": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
      }
    `)
    const domMetadata = getEditorState().editor.domMetadata
    const sanitizedDomMetadata = domWalkerMetadataToSimplifiedMetadataMap(domMetadata)
    expect(sanitizedDomMetadata).toMatchInlineSnapshot(`
      Object {
        "storyboard": Object {
          "children": Array [],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        "storyboard/scene-1": Object {
          "children": Array [
            "storyboard/scene-1/canvas-app",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/canvas-app": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene-1/canvas-app:canvas-app-div",
          ],
        },
        "storyboard/scene-1/canvas-app:canvas-app-div": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2": Object {
          "children": Array [
            "storyboard/scene-2/app",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene-2/app:app-root",
          ],
        },
        "storyboard/scene-2/app:app-root": Object {
          "children": Array [
            "storyboard/scene-2/app:app-root/app-inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app:app-root/app-inner-div": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
      }
    `)
    const jsxMetadata = getEditorState().editor.jsxMetadata
    const sanitizedJsxMetadata = simplifiedMetadataMap(jsxMetadata)
    expect(sanitizedJsxMetadata).toMatchInlineSnapshot(`
      Object {
        "storyboard": Object {
          "children": Array [
            "storyboard/scene-1",
            "storyboard/scene-2",
          ],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        "storyboard/scene-1": Object {
          "children": Array [
            "storyboard/scene-1/canvas-app",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-1/canvas-app": Object {
          "children": Array [],
          "name": "CanvasApp",
          "rootElements": Array [
            "storyboard/scene-1/canvas-app:canvas-app-div",
          ],
        },
        "storyboard/scene-1/canvas-app:canvas-app-div": Object {
          "children": Array [
            "storyboard/scene-1/canvas-app:canvas-app-div/test-mesh",
            "storyboard/scene-1/canvas-app:canvas-app-div/test-ambientLight",
            "storyboard/scene-1/canvas-app:canvas-app-div/test-directionalLight",
            "storyboard/scene-1/canvas-app:canvas-app-div/test-pointLight",
          ],
          "name": "Canvas",
          "rootElements": Array [],
        },
        "storyboard/scene-1/canvas-app:canvas-app-div/test-ambientLight": Object {
          "children": Array [],
          "name": "ambientLight",
          "rootElements": Array [],
        },
        "storyboard/scene-1/canvas-app:canvas-app-div/test-directionalLight": Object {
          "children": Array [],
          "name": "directionalLight",
          "rootElements": Array [],
        },
        "storyboard/scene-1/canvas-app:canvas-app-div/test-mesh": Object {
          "children": Array [
            "storyboard/scene-1/canvas-app:canvas-app-div/test-mesh/test-sphereGeometry",
            "storyboard/scene-1/canvas-app:canvas-app-div/test-mesh/test-meshStandardMaterial",
          ],
          "name": "mesh",
          "rootElements": Array [],
        },
        "storyboard/scene-1/canvas-app:canvas-app-div/test-mesh/test-meshStandardMaterial": Object {
          "children": Array [],
          "name": "meshStandardMaterial",
          "rootElements": Array [],
        },
        "storyboard/scene-1/canvas-app:canvas-app-div/test-mesh/test-sphereGeometry": Object {
          "children": Array [],
          "name": "sphereGeometry",
          "rootElements": Array [],
        },
        "storyboard/scene-1/canvas-app:canvas-app-div/test-pointLight": Object {
          "children": Array [],
          "name": "pointLight",
          "rootElements": Array [],
        },
        "storyboard/scene-2": Object {
          "children": Array [
            "storyboard/scene-2/app",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app": Object {
          "children": Array [],
          "name": "App",
          "rootElements": Array [
            "storyboard/scene-2/app:app-root",
          ],
        },
        "storyboard/scene-2/app:app-root": Object {
          "children": Array [
            "storyboard/scene-2/app:app-root/app-inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app:app-root/app-inner-div": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
      }
    `)
  })
})
