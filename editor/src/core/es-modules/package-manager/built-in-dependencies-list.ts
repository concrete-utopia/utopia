import * as UtopiaAPI from 'utopia-api'
import * as UUIUI from '../../../uuiui'
import * as UUIUIDeps from '../../../uuiui-deps'
import * as ReactJsxRuntime from 'react/jsx-runtime'
import React from 'react' // this is imported like this so that the monkey patching will run
import * as ReactDOM from 'react-dom'
import * as EmotionReact from '@emotion/react'
import * as EmotionStyled from '@emotion/styled'

import editorPackageJSON from '../../../../package.json'
import utopiaAPIPackageJSON from '../../../../../utopia-api/package.json'
import { applyUIDMonkeyPatch } from '../../../utils/canvas-react-utils'
import { createRegisterModuleFunction } from '../../property-controls/property-controls-local'
import type { EditorDispatch } from '../../../components/editor/action-types'
import type { EditorState } from '../../../components/editor/store/editor-state'
import { UtopiaTsWorkers } from '../../workers/common/worker-types'
import { UtopiaApiGroup } from './group-component'

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
    registerModule: createRegisterModuleFunction(workers),
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
  ]
}
