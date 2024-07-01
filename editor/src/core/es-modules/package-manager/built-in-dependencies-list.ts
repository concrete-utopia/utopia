import * as EmotionReact from '@emotion/react'
import * as EmotionStyled from '@emotion/styled'
import * as RemixRunReact from '@remix-run/react'
import * as RemixServerRuntime from '@remix-run/server-runtime'
import * as Hydrogen from '@shopify/hydrogen'
import React from 'react' // this is imported like this so that the monkey patching will run
import * as ReactDOM from 'react-dom'
import * as ReactRouter from 'react-router'
import * as ReactJsxRuntime from 'react/jsx-runtime'
import * as UtopiaAPI from 'utopia-api'
import * as TTYStub from './node-builtin-shims/tty-stub'
import * as UUIUI from '../../../uuiui'
import * as UUIUIDeps from '../../../uuiui-deps'
import * as RemixServerBuild from './built-in-third-party-dependencies/remix-server-build'
import { SafeLink, SafeOutlet } from './canvas-safe-remix'
import { UtopiaApiGroup } from './group-component'

import utopiaAPIPackageJSON from '../../../../../utopia-api/package.json'
import editorPackageJSON from '../../../../package.json'
import { applyUIDMonkeyPatch } from '../../../utils/canvas-react-utils'
import type { UtopiaTsWorkers } from '../../workers/common/worker-types'

const Stub = {}

applyUIDMonkeyPatch()

export interface BuiltInDependency {
  moduleName: string
  nodeModule: any
  version: string
}

export type BuiltInDependencies = Array<BuiltInDependency>

function builtInDependency(
  moduleName: string,
  baseModule: any,
  version: string,
): BuiltInDependency {
  if (baseModule.default != null) {
    return {
      moduleName: moduleName,
      nodeModule: baseModule,
      version: version,
    }
  } else {
    // if the module does not have a default, spread one in there to simulate Synthetic Defaults
    // IMPORTANT this object spread means that the default points to the real base module, which previously
    // caused issues with the monkey patching of react, hence the explicit patching above
    return {
      moduleName: moduleName,
      nodeModule: {
        ...baseModule,
        default: baseModule,
      },
      version: version,
    }
  }
}

export function createBuiltInDependenciesList(
  workers: UtopiaTsWorkers | null,
): BuiltInDependencies {
  const UtopiaAPISpecial: typeof UtopiaAPI & { Group: any } = {
    ...UtopiaAPI,
    Group: UtopiaApiGroup,
  }

  // Ensure this is kept up to date with:
  // server/src/Utopia/Web/Packager/NPM.hs
  return [
    builtInDependency('utopia-api', UtopiaAPISpecial, utopiaAPIPackageJSON.version),
    builtInDependency('uuiui', UUIUI, editorPackageJSON.version),
    builtInDependency('uuiui-deps', UUIUIDeps, editorPackageJSON.version),
    builtInDependency('react/jsx-runtime', ReactJsxRuntime, editorPackageJSON.dependencies.react),
    builtInDependency('react', React, editorPackageJSON.dependencies.react),
    builtInDependency('react-dom', ReactDOM, editorPackageJSON.dependencies['react-dom']),
    builtInDependency(
      '@emotion/react',
      EmotionReact,
      editorPackageJSON.dependencies['@emotion/react'],
    ),
    builtInDependency(
      '@emotion/core',
      EmotionReact,
      editorPackageJSON.dependencies['@emotion/react'],
    ),
    builtInDependency(
      '@emotion/styled',
      EmotionStyled,
      editorPackageJSON.dependencies['@emotion/styled'],
    ),

    // Remix support
    builtInDependency(
      '@remix-run/react',
      {
        ...RemixRunReact,
        Link: SafeLink,
        Outlet: SafeOutlet,
      },
      editorPackageJSON.dependencies['@remix-run/react'],
    ),
    builtInDependency('react-router', ReactRouter, editorPackageJSON.dependencies['react-router']),

    // Dependencies to make loading hydrogen projects faster
    builtInDependency(
      '@remix-run/server-runtime',
      RemixServerRuntime,
      editorPackageJSON.dependencies['@remix-run/server-runtime'],
    ),
    builtInDependency(
      '@shopify/hydrogen',
      Hydrogen,
      editorPackageJSON.dependencies['@shopify/hydrogen'],
    ),

    // Stubs for the hydrogen projects. this is here to prevent our backend from downloading a massive amount of files into the editor
    builtInDependency('@remix-run/dev/server-build', RemixServerBuild, '2.1.0'),
    builtInDependency('@remix-run/dev', Stub, '2.1.0'),
    // FIXME this shouldn't be stubbed when we support configurable eslint
    builtInDependency('@remix-run/eslint-config', Stub, '2.1.0'),
    builtInDependency('@shopify/cli', Stub, '3.50.0'),
    builtInDependency('@shopify/cli-hydrogen', Stub, '6.0.0'),
    // Node builtins
    builtInDependency('tty', TTYStub, '*'),
  ]
}
