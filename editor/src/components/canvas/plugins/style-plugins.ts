import type { ElementPath } from 'utopia-shared/src/types'
import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import type {
  InteractionLifecycle,
  StyleInfoFactory,
} from '../canvas-strategies/canvas-strategy-types'
import { InlineStylePlugin } from './inline-style-plugin'
import { TailwindPlugin } from './tailwind-style-plugin'
import {
  getTailwindConfigCached,
  isTailwindEnabled,
} from '../../../core/tailwind/tailwind-compilation'
import { isFeatureEnabled } from '../../../utils/feature-switches'
import { assertNever } from '../../../core/shared/utils'
import * as EP from '../../../core/shared/element-path'
import type { ValueAtPath } from '../../../core/shared/jsx-attributes'
import type { EditorStateWithPatch } from '../commands/utils/property-utils'
import { applyValuesAtPath } from '../commands/utils/property-utils'
import * as PP from '../../../core/shared/property-path'
import { emptyComments, jsExpressionValue } from '../../../core/shared/element-template'
import type { StyleInfo } from '../canvas-types'

export interface UpdateCSSProp {
  type: 'set'
  property: string
  value: string | number
}

interface DeleteCSSProp {
  type: 'delete'
  property: string
}

export type StyleUpdate = UpdateCSSProp | DeleteCSSProp

export interface StylePlugin {
  name: string
  styleInfoFactory: StyleInfoFactory
  updateStyles: (
    editorState: EditorState,
    elementPath: ElementPath,
    updates: StyleUpdate[],
  ) => EditorStateWithPatch
}

export function getActivePlugin(editorState: EditorState): StylePlugin {
  if (isFeatureEnabled('Tailwind') && isTailwindEnabled()) {
    return TailwindPlugin(getTailwindConfigCached(editorState))
  }
  return InlineStylePlugin
}

interface StylePropsUpdatedDuringInteraction {
  propertiesUpdated: string[]
  propertiesDeleted: string[]
}

export const UpdatedPropertiesGlobal: {
  current: { [elementPathString: string]: StylePropsUpdatedDuringInteraction }
} = {
  current: {},
}

export function resetUpdatedPropertiesGlobal() {
  UpdatedPropertiesGlobal.current = {}
}

function ensureElementPathInUpdatedPropertiesGlobal(elementPath: ElementPath) {
  const elementPathString = EP.toString(elementPath)
  if (!(elementPathString in UpdatedPropertiesGlobal.current)) {
    UpdatedPropertiesGlobal.current[elementPathString] = {
      propertiesDeleted: [],
      propertiesUpdated: [],
    }
  }
}

function runStyleUpdateMidInteraction(
  editorState: EditorState,
  elementPath: ElementPath,
  updates: StyleUpdate[],
) {
  ensureElementPathInUpdatedPropertiesGlobal(elementPath)
  for (const update of updates) {
    switch (update.type) {
      case 'delete':
        UpdatedPropertiesGlobal.current[EP.toString(elementPath)].propertiesDeleted.push(
          update.property,
        )
        break
      case 'set':
        UpdatedPropertiesGlobal.current[EP.toString(elementPath)].propertiesUpdated.push(
          update.property,
        )
        break
      default:
        assertNever(update)
    }
  }
  return InlineStylePlugin.updateStyles(editorState, elementPath, updates)
}

const PaddingLonghands = ['paddingTop', 'paddingBottom', 'paddingLeft', 'paddingRight']

const makeZeroProp = (cssProp: string, zeroValue: string = '0px'): ValueAtPath => {
  return {
    path: PP.create('style', cssProp),
    value: jsExpressionValue(zeroValue, emptyComments),
  }
}

interface PropPatcher {
  matches: (prop: string) => boolean
  patch: (
    prop: string, // TODO: this could be more strongly typed
    styleInfo: StyleInfo | null,
    updatedProperties: StylePropsUpdatedDuringInteraction,
  ) => ValueAtPath[]
}

const genericPropPatcher =
  (zeroValue: string) =>
  (
    prop: string,
    styleInfo: StyleInfo | null,
    updatedProperties: StylePropsUpdatedDuringInteraction,
  ) => {
    const propIsSetOnElement =
      (styleInfo as Record<string, { type: string }> | null)?.[prop]?.type === 'property' // TODO type
    const propIsSetFromCommand = updatedProperties.propertiesUpdated.includes(prop)
    if (!propIsSetOnElement || propIsSetFromCommand) {
      return []
    }
    return [makeZeroProp(prop, zeroValue)]
  }

const patchers: PropPatcher[] = [
  { matches: (p) => p === 'gap', patch: genericPropPatcher('0px') },
  {
    matches: (p) => p === 'padding',
    patch: (_, styleInfo, updatedProperties) => {
      const propIsSetOnElement = styleInfo?.padding != null

      if (!propIsSetOnElement) {
        return []
      }

      return PaddingLonghands.filter((p) => !updatedProperties.propertiesUpdated.includes(p)).map(
        (p) => makeZeroProp(p),
      )
    },
  },
  { matches: (p) => p.startsWith('padding'), patch: genericPropPatcher('0px') },
]

function getPropertiesToZero(
  styleInfo: StyleInfo | null,
  updatedProperties: StylePropsUpdatedDuringInteraction,
): ValueAtPath[] {
  return updatedProperties.propertiesDeleted.flatMap((prop): ValueAtPath[] => {
    const match = patchers.find((p) => p.matches(prop))
    if (match == null) {
      return []
    }
    return match.patch(prop, styleInfo, updatedProperties)
  })
}

export function patchRemovedProperties(editorState: EditorState): EditorState {
  const styleInfoReader = getActivePlugin(editorState).styleInfoFactory({
    projectContents: editorState.projectContents,
    metadata: editorState.jsxMetadata,
    elementPathTree: editorState.elementPathTree,
  })

  return Object.entries(UpdatedPropertiesGlobal.current).reduce(
    (acc, [elementPathString, updatedProperties]) => {
      const elementPath = EP.fromString(elementPathString)
      const styleInfo = styleInfoReader(elementPath)
      const propsToZero = getPropertiesToZero(styleInfo, updatedProperties)
      if (propsToZero.length === 0) {
        return acc
      } else {
        return applyValuesAtPath(acc, elementPath, propsToZero).editorStateWithChanges
      }
    },
    editorState,
  )
}

export function runStyleUpdateForStrategy(
  commandLifecycle: InteractionLifecycle,
  editorState: EditorState,
  elementPath: ElementPath,
  updates: StyleUpdate[],
) {
  switch (commandLifecycle) {
    case 'mid-interaction':
      return runStyleUpdateMidInteraction(editorState, elementPath, updates)
    case 'end-interaction':
      return getActivePlugin(editorState).updateStyles(editorState, elementPath, updates)
    default:
      assertNever(commandLifecycle)
  }
}
