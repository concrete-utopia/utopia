import { createModifiedProject } from '../../../sample-projects/sample-project-utils.test-utils'
import {
  setFeatureForBrowserTestsUseInDescribeBlockOnly,
  wait,
} from '../../../utils/utils.test-utils'
import { StoryboardFilePath } from '../../editor/store/editor-state'
import { REMIX_CONTAINER_TESTID } from '../ui-jsx-canvas-renderer/remix-container-component'
import { renderTestEditorWithModel } from '../ui-jsx.test-utils'
import { createRouteManifestFromProjectContents } from './remix-utils'

const storyboardFileContent = `
import * as React from 'react';
import Utopia, {
  Scene,
  Storyboard,
  RemixContainer,
} from 'utopia-api';


export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <Scene
      data-uid='scene'
      style={{ position: 'absolute', left: 400, top: 0, width: 375, height: 812 }}
    >
      <RemixContainer />
    </Scene>
  </Storyboard>
);
`

describe('Remix utils', () => {
  setFeatureForBrowserTestsUseInDescribeBlockOnly('Remix support', true)
  it('Parses the route manifest from the files', async () => {
    const project = createModifiedProject({
      [StoryboardFilePath]: storyboardFileContent,
      ['src/root.js']: `import * as React from 'react'
import { Outlet } from '@remix-run/react'

export default function App() {
  return (
    <div
      style={{
        backgroundColor: '#a5c0db',
        width: '100%',
        height: '100%',
        contain: 'layout',
      }}
      data-uid='bf5'
    >
      <div
        data-uid='d4d'
        style={{
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'flex-end',
          fontSize: '8px',
          fontWeight: 700,
          paddingTop: 0,
          paddingRight: 0,
          paddingBottom: 0,
          paddingLeft: 0,
        }}
      >
        Root.js
      </div>
      <Outlet data-uid='11c' />
    </div>
  )
}
`,
      ['src/routes/_index.js']: `import React from 'react'
import { Link } from '@remix-run/react'

export default function Index() {
  return (
    <div
      style={{
        display: 'flex',
        flexDirection: 'column',
        gap: 24,
        padding: '0px 8px',
        height: '100%',
      }}
      data-path='0cd/add/597:rrr'
      data-uid='f95'
    >
      <span
        style={{
          fontSize: '40px',
          fontWeight: 700,
          fontStyle: 'normal',
          color: 'rgb(0, 0, 0, 1)',
        }}
        data-path='0cd/add/597:rrr/ttt'
        data-uid='ttt'
      >
        Beaches
      </span>
      <div
        style={{
          backgroundColor: '#e6e6e6',
          width: '100%',
          height: 79,
          display: 'flex',
          flexDirection: 'row',
          alignItems: 'center',
          justifyContent: 'center',
          borderRadius: 26,
        }}
        data-path='0cd/add/597:rrr/419'
        data-uid='419'
      >
        <Link
          style={{
            wordBreak: 'break-word',
            color: 'rgb(255, 251, 251, 1)',
            contain: 'layout',
            fontSize: '20px',
            width: 'max-content',
            height: 'max-content',
            fontWeight: 700,
          }}
          to='/posts'
          data-uid='af0'
        >
          Check avaliable beaches
        </Link>
      </div>
    </div>
  )
}
`,
      ['src/routes/posts.$postId.js']: `import React from 'react'
import { useParams } from '@remix-run/react'
import { json, useLoaderData } from 'react-router'

async function wait(ms) {
  return new Promise((res) => setTimeout(res, ms))
}

export async function loader({ params }) {
  // await wait(1000)
  if (params.postId === '1') {
    return json({
      name: 'La Digue',
      src:
        'https://source.unsplash.com/jPmurJKSL_0/600x800',
      description:
        'Seychelles generally has warm temperatures throughout the year. They get frequent and sometimes heavy rainfall. On La Digue, rainfall can be very heavy, but it usually lasts for one hour or less. Daytime temperatures on La Digue normally range from 24 °C (75 °F) to 32 °C (90 °F); nighttime temperatures are slightly colder. The months with the heaviest rainfall are October to March, with monthly precipitation of 402.6 mm (15.85 in) in January. ',
    })
  }
  if (params.postId === '2') {
    return json({
      name: 'McWay Falls',
      src:
        'https://source.unsplash.com/07mSKrzKiRw/600x800',
      description:
        'In 1983, Big Sur experienced one of the wettest years on record with 88.85 inches (2,257 mm) of rain. Up to this time, McWay Falls fell directly into the ocean. The huge rainfall resulted in several landslides and mudflows,[9] including an extremely large mudslide immediately north of Julia Pfeiffer Burns State Park on March 1. The mudflow entered the ocean immediately to the north of the falls, and Highway 1 was closed for a year while the road was repaired.',
    })
  }
  return json({ error: 'not found' })
}

export default function PostForId() {
  const { name, src, description } = useLoaderData()

  return (
    <div
      style={{
        width: '100%',
        height: '100%',
        display: 'flex',
        flexDirection: 'column',
        gap: 21,
        backgroundColor: 'white',
        alignItems: 'center',
        justifyContent: 'flex-start',
        paddingTop: 10,
        paddingRight: 10,
        paddingBottom: 10,
        paddingLeft: 10,
        overflowY: 'scroll',
      }}
      data-uid='a8e'
    >
      <img
        style={{
          backgroundColor: '#aaaaaa33',
          width: '100%',
          height: 246.5,
          objectFit: 'cover',
          contain: 'layout',
          borderRadius: 20,
        }}
        src={src}
        data-uid='aag'
      />
      <div
        style={{
          contain: 'layout',
          width: '100%',
          height: 'max-content',
        }}
        data-uid='3d0'
      >
        <span
          style={{
            wordBreak: 'break-word',
            width: 99,
            contain: 'layout',
            fontSize: '26px',
            height: 30.5,
          }}
          data-uid='4c5'
        >
          {name}
        </span>
      </div>
      <div
        style={{
          width: '100%',
          height: 19,
          contain: 'layout',
        }}
        data-uid='6e3'
      >
        <span
          style={{
            wordBreak: 'break-word',
            width: '100%',
            height: 'max-content',
            contain: 'layout',
          }}
          data-uid='1a3'
        >
          {description}
        </span>
      </div>
    </div>
  )
}
`,
      ['src/routes/posts._index.js']: `import React from 'react'
import { Link } from '@remix-run/react'
import { json, useLoaderData } from 'react-router'

export function loader() {
  return json({
    beaches: [
      {
        id: 1,
        name: 'La Digue',
        src:
          'https://source.unsplash.com/jPmurJKSL_0/600x800',
      },
      {
        id: 2,
        name: 'McWay Falls',
        src:
          'https://source.unsplash.com/07mSKrzKiRw/600x800',
      },
    ],
  })
}

export default function Posts() {
  const { beaches } = useLoaderData()
  return (
    <div
      style={{
        padding: '10px',
        backgroundColor: 'white',
        height: '100%',
      }}
      data-uid='289'
    >
      <span
        style={{
          fontSize: '30px',
          fontWeight: 700,
          fontStyle: 'normal',
          marginBottom: 20,
          display: 'inline-block',
        }}
        data-uid='8b1'
      >
        Beaches near you
      </span>
      {beaches.map(({ id, name, src }) => (
        <div
          style={{
            width: '100%',
            height: 'max-content',
            display: 'flex',
            flexDirection: 'row',
            gap: 26,
            padding: '15px',
            alignItems: 'center',
            justifyContent: 'center',
            borderRadius: 20,
            border: '1px solid black',
            marginBottom: '10px',
          }}
          data-uid='1b9'
        >
          <img
            style={{
              backgroundColor: '#aaaaaa33',
              width: 77,
              height: 75,
              contain: 'layout',
              borderRadius: 20,
            }}
            src={src}
            data-uid='824'
          />
          <span
            style={{
              wordBreak: 'break-word',
              width: 110,
              height: 33,
              contain: 'layout',
            }}
            data-uid='97f'
          >
            <Link to={1} data-uid='5bb'>
              {name}
            </Link>
          </span>
        </div>
      ))}
    </div>
  )
}
`,
    })
    const renderResult = await renderTestEditorWithModel(project, 'await-first-dom-report')
    const remixManifest = createRouteManifestFromProjectContents(
      renderResult.getEditorState().editor.projectContents,
    )

    expect(remixManifest).toEqual({})
  })
})
