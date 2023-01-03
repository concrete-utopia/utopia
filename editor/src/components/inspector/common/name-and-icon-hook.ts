import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import {
  JSXElementName,
  ElementInstanceMetadataMap,
  ElementInstanceMetadata,
} from '../../../core/shared/element-template'
import { ElementPath } from '../../../core/shared/project-file-types'
import { useEditorState } from '../../editor/store/store-hook'
import { IcnProps } from '../../../uuiui'
import { createComponentOrElementIconProps } from '../../navigator/layout-element-icons'
import {
  NameAndIconResultArrayKeepDeepEquality,
  NameAndIconResultKeepDeepEquality,
} from '../../../utils/deep-equality-instances'
import { createSelector } from 'reselect'
import {
  AllElementProps,
  EditorStorePatched,
  MetadataSubstate,
} from '../../editor/store/editor-state'
import React from 'react'
import { objectValues } from '../../../core/shared/object-utils'
import { eitherToMaybe } from '../../../core/shared/either'

export interface NameAndIconResult {
  path: ElementPath
  name: JSXElementName | null
  label: string
  iconProps: IcnProps
}

export function useMetadata(): ElementInstanceMetadataMap {
  return useEditorState('metadata')((store) => store.editor.jsxMetadata, 'useMetadata')
}

const namesAndIconsAllPathsResultSelector = createSelector(
  (store: MetadataSubstate) => store.editor.jsxMetadata,
  (store: MetadataSubstate) => store.editor.allElementProps,
  (metadata, allElementProps) => {
    let result: Array<NameAndIconResult> = []
    for (const metadataElement of objectValues(metadata)) {
      const nameAndIconResult = getNameAndIconResult(allElementProps, metadataElement)
      result.push(nameAndIconResult)
    }
    return result
  },
)

export function useNamesAndIconsAllPaths(): NameAndIconResult[] {
  const selector = React.useMemo(() => namesAndIconsAllPathsResultSelector, [])
  return useEditorState('metadata')(
    selector,
    'useNamesAndIconsAllPaths',
    (oldResult, newResult) => {
      return NameAndIconResultArrayKeepDeepEquality(oldResult, newResult).areEqual
    },
  )
}

function getNameAndIconResult(
  allElementProps: AllElementProps,
  metadata: ElementInstanceMetadata,
): NameAndIconResult {
  const elementName = MetadataUtils.getJSXElementName(eitherToMaybe(metadata.element))
  return {
    path: metadata.elementPath,
    name: elementName,
    label: MetadataUtils.getElementLabelFromMetadata(allElementProps, metadata),
    iconProps: createComponentOrElementIconProps(metadata),
  }
}
