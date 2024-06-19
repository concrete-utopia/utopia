import type { ProjectContentTreeRoot } from '../../components/assets'
import { addFileToProjectContents } from '../../components/assets'
import type {
  ComponentDescriptor,
  PropertyControlsInfo,
} from '../../components/custom-code/code-file'
import { componentDescriptorFromDescriptorFile } from '../../components/custom-code/code-file'
import { simpleDefaultProject } from '../../sample-projects/sample-project-utils'
import { parseProjectContents } from '../../sample-projects/sample-project-utils.test-utils'
import { codeFile } from '../shared/project-file-types'
import { assertNever } from '../shared/utils'
import { getComponentDescriptorForTarget } from './property-controls-utils'
import * as EP from '../shared/element-path'

describe('getComponentDescriptorForTarget', () => {
  function getProjectContents(mappedPath: 'mapped-path' | 'regular-path'): ProjectContentTreeRoot {
    let baseProjectContents: ProjectContentTreeRoot = simpleDefaultProject().projectContents
    baseProjectContents = addFileToProjectContents(
      baseProjectContents,
      '/jsconfig.json',
      codeFile(
        `{
  "compilerOptions": {
    "checkJs": true,
    "jsx": "react-jsx",
    "target": "ES2022",
    "module": "ES2022",
    "moduleResolution": "Bundler",
    "baseUrl": ".",
    "paths": {
      "~/*": ["app/*"]
    }
  },
  "include": ["./**/*.d.ts", "./**/*.js", "./**/*.jsx"]
}
`,
        null,
      ),
    )
    baseProjectContents = addFileToProjectContents(
      baseProjectContents,
      '/app/components.js',
      codeFile(
        `
import * as React from 'react'
export const Component = () => {
  return <div data-uid='component-div' />
}
`,
        null,
      ),
    )
    switch (mappedPath) {
      case 'mapped-path':
        baseProjectContents = addFileToProjectContents(
          baseProjectContents,
          '/src/app.js',
          codeFile(
            `
import * as React from 'react'
import { Component } from '~/components'
export const App = (props) => {
  return (
    <div
      data-uid='div'
      style={{ width: '100%', height: '100%', backgroundcolor: '#ffffff', position: 'relative' }}
    >
      <Component data-uid='component' />
    </div>
  )
}`,
            null,
          ),
        )
        break
      case 'regular-path':
        baseProjectContents = addFileToProjectContents(
          baseProjectContents,
          '/src/app.js',
          codeFile(
            `
import * as react from 'react'
import { Component } from '/app/components'
export const App = (props) => {
  return (
    <div
      data-uid='div'
      style={{ width: '100%', height: '100%', backgroundcolor: '#ffffff', position: 'relative' }}
    >
      <Component data-uid='component' />
    </div>
  )
}`,
            null,
          ),
        )
        break
      default:
        assertNever(mappedPath)
    }

    return parseProjectContents(baseProjectContents)
  }
  const componentDescriptor: ComponentDescriptor = {
    properties: {},
    supportsChildren: true,
    preferredChildComponents: [],
    variants: [],
    source: componentDescriptorFromDescriptorFile('/components.utopia.js', null),
    focus: 'default',
    inspector: { type: 'hidden' },
    emphasis: 'regular',
    icon: 'component',
    label: 'Component',
  }
  const propertyControlsInfo: PropertyControlsInfo = {
    ['/app/components']: {
      ['Component']: componentDescriptor,
    },
  }
  it('works with the regular path', () => {
    const projectContents = getProjectContents('regular-path')
    const actualResult = getComponentDescriptorForTarget(
      EP.fromString(`sample-storyboard/sample-scene/sample-app:div/component`),
      propertyControlsInfo,
      projectContents,
    )
    expect(actualResult).toEqual(componentDescriptor)
  })
  it('works with a mapped path', () => {
    const projectContents = getProjectContents('mapped-path')
    const actualResult = getComponentDescriptorForTarget(
      EP.fromString(`sample-storyboard/sample-scene/sample-app:div/component`),
      propertyControlsInfo,
      projectContents,
    )
    expect(actualResult).toEqual(componentDescriptor)
  })
})
