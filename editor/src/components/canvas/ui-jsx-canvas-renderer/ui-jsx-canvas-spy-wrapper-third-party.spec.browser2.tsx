/* eslint-disable jest/expect-expect */
import * as MockReactThreeFiber from '@react-three/fiber'
import * as mockWithEditorPackageJSON from '../../../../package.json'

import { DefaultStartingFeatureSwitches, renderTestEditorWithModel } from '../ui-jsx.test-utils'
import type { ParsedTextFile } from '../../../core/shared/project-file-types'
import {
  textFile,
  textFileContents,
  RevisionsState,
  isParseSuccess,
} from '../../../core/shared/project-file-types'
import { emptySet } from '../../../core/shared/set-utils'
import * as EP from '../../../core/shared/element-path'
import { lintAndParse } from '../../../core/workers/parser-printer/parser-printer'
import { complexDefaultProject } from '../../../sample-projects/sample-project-utils'
import { wait, simplifiedMetadataMap } from '../../../utils/utils.test-utils'
import { addFileToProjectContents } from '../../assets'
import type { EditorStorePatched } from '../../editor/store/editor-state'
import { StoryboardFilePath } from '../../editor/store/editor-state'
import { matchInlineSnapshotBrowser } from '../../../../test/karma-snapshots'
import { createBuiltInDependenciesList } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { NO_OP } from '../../../core/shared/utils'
import CanvasActions from '../canvas-actions'
import type { CanvasVector } from '../../../core/shared/math-utils'

const builtInDependencies = createBuiltInDependenciesList(null)
builtInDependencies.push({
  moduleName: '@react-three/fiber',
  nodeModule: {
    ...MockReactThreeFiber,
    default: MockReactThreeFiber,
  },
  version: mockWithEditorPackageJSON.devDependencies['@react-three/fiber'],
})

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

async function renderTestProject() {
  const baseModel = complexDefaultProject()

  const updatedProject = Object.keys(exampleFiles).reduce((workingProject, modifiedFilename) => {
    const parsedFile = lintAndParse(
      modifiedFilename,
      [],
      exampleFiles[modifiedFilename],
      null,
      emptySet(),
      'trim-bounds',
      'do-not-apply-steganography',
    ) as ParsedTextFile
    if (!isParseSuccess(parsedFile)) {
      throw new Error('The test file parse failed')
    }

    const updatedProjectContents = addFileToProjectContents(
      workingProject.projectContents,
      modifiedFilename,
      textFile(
        textFileContents(exampleFiles[modifiedFilename], parsedFile, RevisionsState.BothMatch),
        null,
        parsedFile,
        0,
      ),
    )

    return {
      ...baseModel,
      projectContents: updatedProjectContents,
    }
  }, baseModel)
  const renderTestResult = await renderTestEditorWithModel(
    updatedProject,
    'await-first-dom-report',
    DefaultStartingFeatureSwitches,
    builtInDependencies,
  )
  // Pause to let R3F do what it needs to do.
  await wait(500)
  // This is a kludge to get the DOM walker to run after processing the action.
  await renderTestResult.dispatch(
    [CanvasActions.scrollCanvas({ x: 1, y: 0 } as CanvasVector)],
    true,
  )
  return renderTestResult
}

async function waitForFullMetadata(getEditorState: () => EditorStorePatched): Promise<true> {
  let foundMetadata = false
  let totalWaitTime = 0
  do {
    const WaitTime = 50
    // eslint-disable-next-line no-await-in-loop
    await wait(WaitTime)
    totalWaitTime += WaitTime
    foundMetadata =
      getEditorState().editor.spyMetadata[
        'storyboard/scene-1/canvas-app:canvas-app-div/test-mesh/test-meshStandardMaterial'
      ] != null
    if (foundMetadata) {
      return true
    }
    if (totalWaitTime > 5000) {
      throw new Error('The React Three Fiber test timed out.')
    }
  } while (!foundMetadata)
  throw new Error('heat death of the universe')
}

// This test has started failing with `Error creating WebGL context`, and causes other tests to fail
xdescribe('Spy Wrapper Tests For React Three Fiber', () => {
  it('a simple Canvas element in a scene where spy and jsx metadata has extra elements', async () => {
    // Code kept commented for any future person who needs it.
    // const currentWindow = require('electron').remote.getCurrentWindow()
    // currentWindow.show()
    // currentWindow.setPosition(500, 200)
    // currentWindow.setSize(2200, 1000)
    // currentWindow.openDevTools()
    // await wait(20000)

    const { getEditorState } = await renderTestProject()
    // React Three Fiber seems to have some second pass render that appears to run
    // after the regular React render and this appears to give it a chance to be triggered.
    await waitForFullMetadata(getEditorState)
    const spiedMetadata = getEditorState().editor.spyMetadata
    const sanitizedSpyData = simplifiedMetadataMap(spiedMetadata)
    matchInlineSnapshotBrowser(
      sanitizedSpyData,
      `
      Object {
        "storyboard": Object {
          "name": "Storyboard",
        },
        "storyboard/scene-1": Object {
          "name": "Scene",
        },
        "storyboard/scene-1/canvas-app": Object {
          "name": "CanvasApp",
        },
        "storyboard/scene-1/canvas-app:canvas-app-div": Object {
          "name": "Canvas",
        },
        "storyboard/scene-1/canvas-app:canvas-app-div/test-ambientLight": Object {
          "name": "ambientLight",
        },
        "storyboard/scene-1/canvas-app:canvas-app-div/test-directionalLight": Object {
          "name": "directionalLight",
        },
        "storyboard/scene-1/canvas-app:canvas-app-div/test-mesh": Object {
          "name": "mesh",
        },
        "storyboard/scene-1/canvas-app:canvas-app-div/test-mesh/test-meshStandardMaterial": Object {
          "name": "meshStandardMaterial",
        },
        "storyboard/scene-1/canvas-app:canvas-app-div/test-mesh/test-sphereGeometry": Object {
          "name": "sphereGeometry",
        },
        "storyboard/scene-1/canvas-app:canvas-app-div/test-pointLight": Object {
          "name": "pointLight",
        },
        "storyboard/scene-2": Object {
          "name": "Scene",
        },
        "storyboard/scene-2/app": Object {
          "name": "App",
        },
        "storyboard/scene-2/app:app-root": Object {
          "name": "div",
        },
        "storyboard/scene-2/app:app-root/app-inner-div": Object {
          "name": "div",
        },
      }
    `,
    )
    const domMetadata = getEditorState().editor.domMetadata
    const sanitizedDomMetadata = simplifiedMetadataMap(domMetadata)
    matchInlineSnapshotBrowser(
      sanitizedDomMetadata,
      `
      Object {
        "storyboard": Object {
          "name": "Storyboard",
        },
        "storyboard/scene-1": Object {
          "name": "div",
        },
        "storyboard/scene-1/canvas-app": Object {
          "name": "div",
        },
        "storyboard/scene-1/canvas-app:canvas-app-div": Object {
          "name": "div",
        },
        "storyboard/scene-2": Object {
          "name": "div",
        },
        "storyboard/scene-2/app": Object {
          "name": "div",
        },
        "storyboard/scene-2/app:app-root": Object {
          "name": "div",
        },
        "storyboard/scene-2/app:app-root/app-inner-div": Object {
          "name": "div",
        },
      }
    `,
    )
    const jsxMetadata = getEditorState().editor.jsxMetadata
    const sanitizedJsxMetadata = simplifiedMetadataMap(jsxMetadata)
    matchInlineSnapshotBrowser(
      sanitizedJsxMetadata,
      `
      Object {
        "storyboard": Object {
          "name": "Storyboard",
        },
        "storyboard/scene-1": Object {
          "name": "Scene",
        },
        "storyboard/scene-1/canvas-app": Object {
          "name": "CanvasApp",
        },
        "storyboard/scene-1/canvas-app:canvas-app-div": Object {
          "name": "Canvas",
        },
        "storyboard/scene-1/canvas-app:canvas-app-div/test-ambientLight": Object {
          "name": "ambientLight",
        },
        "storyboard/scene-1/canvas-app:canvas-app-div/test-directionalLight": Object {
          "name": "directionalLight",
        },
        "storyboard/scene-1/canvas-app:canvas-app-div/test-mesh": Object {
          "name": "mesh",
        },
        "storyboard/scene-1/canvas-app:canvas-app-div/test-mesh/test-meshStandardMaterial": Object {
          "name": "meshStandardMaterial",
        },
        "storyboard/scene-1/canvas-app:canvas-app-div/test-mesh/test-sphereGeometry": Object {
          "name": "sphereGeometry",
        },
        "storyboard/scene-1/canvas-app:canvas-app-div/test-pointLight": Object {
          "name": "pointLight",
        },
        "storyboard/scene-2": Object {
          "name": "Scene",
        },
        "storyboard/scene-2/app": Object {
          "name": "App",
        },
        "storyboard/scene-2/app:app-root": Object {
          "name": "div",
        },
        "storyboard/scene-2/app:app-root/app-inner-div": Object {
          "name": "div",
        },
      }
    `,
    )
  })
})
