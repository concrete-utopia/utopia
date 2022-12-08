import { MetadataUtils } from '../../core/model/element-metadata-utils'
import {
  ElementInstanceMetadataMap,
  emptyComments,
  jsxAttributeValue,
} from '../../core/shared/element-template'
import { ElementPath } from '../../core/shared/project-file-types'
import * as PP from '../../core/shared/property-path'
import { EditorAction } from '../editor/action-types'
import { setProperty } from '../editor/actions/action-creators'
import { FlexAlignment, FlexJustifyContent } from './inspector-common'

export type InspectorStrategy = (
  metadata: ElementInstanceMetadataMap,
  selectedElementPaths: Array<ElementPath>,
) => Array<EditorAction> | null

export const setFlexAlignJustifyContentStrategies = (
  flexAlignment: FlexAlignment,
  justifyContent: FlexJustifyContent,
): Array<InspectorStrategy> => [
  (metadata, elementPaths) => {
    const elements = elementPaths.filter((e) =>
      MetadataUtils.isFlexLayoutedContainer(MetadataUtils.findElementByElementPath(metadata, e)),
    )

    if (elements.length === 0) {
      return null
    }

    return elements.flatMap((path) => [
      setProperty(
        path,
        PP.create(['style', 'alignItems']),
        jsxAttributeValue(flexAlignment, emptyComments),
      ),
      setProperty(
        path,
        PP.create(['style', 'justifyContent']),
        jsxAttributeValue(justifyContent, emptyComments),
      ),
    ])
  },
]
