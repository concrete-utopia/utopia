import {
  jsxAttribute,
  jsxExpressionContainer,
  jsxIdentifier,
  jsxOpeningElement,
} from '@babel/types'
import { generateUidWithExistingComponents } from '../../../../core/model/element-template-utils'
import {
  isJSXTextBlock,
  jsxAttributeNestedObjectSimple,
  jsxAttributeValue,
  jsxElement,
  jsxTextBlock,
  JSXTextBlock,
} from '../../../../core/shared/element-template'
import { InstancePath } from '../../../../core/shared/project-file-types'
import { EditorModel } from '../../action-types'
import {
  getOpenUtopiaJSXComponentsFromState,
  modifyOpenJsxElementAtPath,
} from '../../store/editor-state'

interface WrapTextInStyledSpan {
  action: 'WRAP_TEXT_IN_STYLED_SPAN'
  target: InstancePath
  selection: Selection
}

export type TextEditorActions = WrapTextInStyledSpan

// WARNING: this assumes LTR direction
// Text ranges normalise anchor/focus positions in a selection to a range with a LTR start and end
// Assumes selection is made in same node
interface SimpleTextRange {
  direction: 'LTR'
  startIndex: number
  finishIndex: number
}

function getSimpleTextRange({ focusOffset, anchorOffset }: Selection): SimpleTextRange {
  return {
    direction: 'LTR',
    startIndex: anchorOffset < focusOffset ? anchorOffset : focusOffset,
    finishIndex: anchorOffset > focusOffset ? anchorOffset : focusOffset,
  }
}

export const TextEditorActions = {
  WRAP_TEXT_IN_STYLED_SPAN: (action: WrapTextInStyledSpan, editor: EditorModel): EditorModel => {
    return modifyOpenJsxElementAtPath(
      action.target,
      (element) => {
        // WARNING: EXPERIMENT
        // we only support elements with one text block as a child
        if (element.children.length === 1) {
          const child = element.children[0]
          if (isJSXTextBlock(child)) {
            const { startIndex, finishIndex } = getSimpleTextRange(action.selection)
            const leftTextNode = child.text.substring(0, startIndex)
            const centerTextNode = child.text.substring(startIndex, finishIndex)
            const rightTextNode = child.text.substring(finishIndex)
            return {
              ...element,
              children: [
                jsxTextBlock(leftTextNode),
                jsxElement(
                  'span',
                  {
                    style: jsxAttributeNestedObjectSimple({ fontWeight: jsxAttributeValue(700) }),
                    'data-uid': jsxAttributeValue(
                      generateUidWithExistingComponents(
                        getOpenUtopiaJSXComponentsFromState(editor),
                      ),
                    ),
                  },
                  [jsxTextBlock(centerTextNode)],
                ),
                jsxTextBlock(rightTextNode),
              ],
            }
          }
        }
        throw new Error('We only support elements with one text node child')
      },
      editor,
    )
  },
}

export function wrapTextInStyledSpan(
  target: InstancePath,
  selection: Selection,
): WrapTextInStyledSpan {
  return {
    action: 'WRAP_TEXT_IN_STYLED_SPAN',
    target,
    selection,
  }
}
