import { getLayoutProperty } from '../../../core/layout/getLayoutProperty'
import type { StyleLayoutProp } from '../../../core/layout/layout-helpers-new'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { defaultEither, isLeft, mapEither, right } from '../../../core/shared/either'
import type { JSXElement } from '../../../core/shared/element-template'
import { isJSXElement } from '../../../core/shared/element-template'
import { typedObjectKeys } from '../../../core/shared/object-utils'
import * as PP from '../../../core/shared/property-path'
import { styleStringInArray } from '../../../utils/common-constants'
import type { ParsedCSSProperties } from '../../inspector/common/css-utils'
import { withPropertyTag, type WithPropertyTag } from '../canvas-types'
import { foldAndApplyCommandsSimple } from '../commands/commands'
import { deleteProperties } from '../commands/delete-properties-command'
import type { StylePlugin } from './style-plugins'

function getPropertyFromInstance<P extends StyleLayoutProp, T = ParsedCSSProperties[P]>(
  prop: P,
  element: JSXElement,
): WithPropertyTag<T> | null {
  return defaultEither(
    null,
    mapEither(withPropertyTag, getLayoutProperty(prop, right(element.props), styleStringInArray)),
  ) as WithPropertyTag<T> | null
}

export const InlineStylePlugin: StylePlugin = {
  name: 'Inline Style',
  styleInfoFactory:
    ({ metadata }) =>
    (elementPath) => {
      const instance = MetadataUtils.findElementByElementPath(metadata, elementPath)
      if (instance == null || isLeft(instance.element) || !isJSXElement(instance.element.value)) {
        return null
      }

      const gap = getPropertyFromInstance('gap', instance.element.value)
      const flexDirection = getPropertyFromInstance('flexDirection', instance.element.value)

      return {
        gap: gap,
        flexDirection: flexDirection,
      }
    },
  normalizeFromInlineStyle: (editor, elementsToNormalize) => {
    return foldAndApplyCommandsSimple(
      editor,
      typedObjectKeys(editor.canvas.propertiesToUnset).flatMap((p) =>
        elementsToNormalize.map((element) =>
          deleteProperties('on-complete', element, [PP.create('style', p)]),
        ),
      ),
    )
  },
}
