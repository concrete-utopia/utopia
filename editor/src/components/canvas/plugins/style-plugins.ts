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
import { isStyleInfoKey, type StyleInfo } from '../canvas-types'

export interface UpdateCSSProp {
  type: 'set'
  property: string
  value: string | number
}

interface DeleteCSSProp {
  type: 'delete'
  property: string
}

export function updateCSSProp(property: string, value: string | number): UpdateCSSProp {
  return {
    type: 'set',
    property: property,
    value: value,
  }
}

export function deleteCSSProp(property: string): DeleteCSSProp {
  return {
    type: 'delete',
    property: property,
  }
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

export interface StylePropsUpdatedDuringInteraction {
  propertiesUpdated: string[]
  propertiesDeleted: string[]
}

export interface UpdatedProperties {
  [elementPathString: string]: StylePropsUpdatedDuringInteraction
}

export function resetUpdatedProperties(editorState: EditorState): EditorState {
  return { ...editorState, propertiesUpdatedDuringInteraction: {} }
}

function getPropertiesUpdatedDuringInteraction(editorState: EditorState) {
  return editorState.propertiesUpdatedDuringInteraction
}

function ensureElementPathInUpdatedPropertiesGlobal(
  elementPath: ElementPath,
  updatedProperties: UpdatedProperties,
) {
  const updatedPropertiesToExtend = { ...updatedProperties }
  const elementPathString = EP.toString(elementPath)
  if (!(elementPathString in updatedPropertiesToExtend)) {
    updatedPropertiesToExtend[elementPathString] = {
      propertiesDeleted: [],
      propertiesUpdated: [],
    }
  }

  return updatedPropertiesToExtend
}

interface EditorStateWithPatches {
  editorStateWithChanges: EditorState
  editorStatePatches: EditorStatePatch[]
}

function runStyleUpdateMidInteraction(
  editorState: EditorState,
  elementPath: ElementPath,
  updates: StyleUpdate[],
): EditorStateWithPatches {
  const updatedProperties = ensureElementPathInUpdatedPropertiesGlobal(
    elementPath,
    getPropertiesUpdatedDuringInteraction(editorState),
  )
  for (const update of updates) {
    switch (update.type) {
      case 'delete':
        updatedProperties[EP.toString(elementPath)].propertiesDeleted.push(update.property)
        break
      case 'set':
        updatedProperties[EP.toString(elementPath)].propertiesUpdated.push(update.property)
        break
      default:
        assertNever(update)
    }
  }
  const { editorStatePatch, editorStateWithChanges } = InlineStylePlugin.updateStyles(
    editorState,
    elementPath,
    updates,
  )
  return {
    editorStateWithChanges: editorStateWithChanges,
    editorStatePatches: [
      editorStatePatch,
      { propertiesUpdatedDuringInteraction: { $set: updatedProperties } },
    ],
  }
}

const makeZeroProp = (cssProp: string, zeroValue: string = '0px'): ValueAtPath => {
  return {
    path: PP.create('style', cssProp),
    value: jsExpressionValue(zeroValue, emptyComments),
  }
}

interface PropPatcher {
  matches: (prop: string) => boolean
  patch: (
    prop: keyof StyleInfo,
    styleInfo: StyleInfo | null,
    updatedProperties: StylePropsUpdatedDuringInteraction,
  ) => ValueAtPath[]
}

const genericPropPatcher =
  (zeroValue: string) =>
  (
    prop: keyof StyleInfo,
    styleInfo: StyleInfo | null,
    updatedProperties: StylePropsUpdatedDuringInteraction,
  ) => {
    const propIsSetOnElement = styleInfo?.[prop] != null
    const propIsSetFromCommand = updatedProperties.propertiesUpdated.includes(prop)
    if (!propIsSetOnElement || propIsSetFromCommand) {
      return []
    }
    return [makeZeroProp(prop, zeroValue)]
  }

const patchers: PropPatcher[] = [{ matches: (p) => p === 'gap', patch: genericPropPatcher('0px') }]

function getPropertiesToZero(
  styleInfo: StyleInfo | null,
  updatedProperties: StylePropsUpdatedDuringInteraction,
): ValueAtPath[] {
  return updatedProperties.propertiesDeleted.flatMap((prop): ValueAtPath[] => {
    if (!isStyleInfoKey(prop)) {
      console.error("Trying to zero prop that's not a handled by StyleInfo:", prop)
      return []
    }

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

  const propertiesUpdatedDuringInteraction = getPropertiesUpdatedDuringInteraction(editorState)

  return Object.entries(propertiesUpdatedDuringInteraction).reduce(
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
): EditorStateWithPatches {
  switch (commandLifecycle) {
    case 'mid-interaction':
      return runStyleUpdateMidInteraction(editorState, elementPath, updates)
    case 'end-interaction':
      const { editorStatePatch, editorStateWithChanges } = getActivePlugin(
        editorState,
      ).updateStyles(editorState, elementPath, updates)
      return {
        editorStateWithChanges: editorStateWithChanges,
        editorStatePatches: [editorStatePatch],
      }
    default:
      assertNever(commandLifecycle)
  }
}
