import {
  ContentBlock,
  ContentState,
  convertFromRaw,
  Editor as DraftEditor,
  RawDraftContentState,
  convertToRaw,
} from 'draft-js'
import createStyles from 'draft-js-custom-styles'
import * as R from 'ramda'
import { jsxAttributeValue } from '../core/shared/element-template'
import {emptyComments} from "../core/workers/parser-printer/parser-printer-comments";

export function createContentState(text: RawDraftContentState | string): ContentState {
  const safeText = R.defaultTo('', text)
  switch (typeof safeText) {
    case 'number':
    case 'boolean':
      return ContentState.createFromText('' + safeText)
    case 'string':
      return ContentState.createFromText(safeText)
    default:
      return convertFromRaw(safeText)
  }
}

export function getNewSizeFromContent(
  content: ContentState,
  scale: number,
  draftEditor: DraftEditor,
): { width: number | null; height: number | null } {
  const newWidth = getDraftJSWidth(content, scale, draftEditor)
  const newHeight = getDraftJSHeight(content, scale, draftEditor)

  return { width: newWidth, height: newHeight }
}

export function getDraftJSWidth(
  content: ContentState,
  scale: number,
  draftEditor: DraftEditor,
): number | null {
  const blocks = content.getBlocksAsArray()

  // the width is the width of the widest block (longest line)
  const blockWidths = blocks.map((block: ContentBlock) => {
    let node = getDomNodeOfDraftJSBlock(block, 'span', draftEditor)
    let width: number | null = null
    while (node != null) {
      const toAppend = node.getBoundingClientRect().width
      width = width == null ? toAppend : width + toAppend
      node = node.nextElementSibling
    }
    return width
  })
  const blockWidthsNotNull = blockWidths.filter((w) => w != null) as number[]
  return blockWidthsNotNull.length > 0 ? Math.max(...blockWidthsNotNull) / scale : null
}

export function getDraftJSHeight(
  content: ContentState,
  scale: number,
  draftEditor: DraftEditor,
): number | null {
  const firstBlockNode = getDomNodeOfDraftJSBlock(content.getFirstBlock(), 'div', draftEditor)
  const lastBlockNode = getDomNodeOfDraftJSBlock(content.getLastBlock(), 'div', draftEditor)
  if (firstBlockNode != null && lastBlockNode != null) {
    const top = firstBlockNode.getBoundingClientRect().top
    const bottom = lastBlockNode.getBoundingClientRect().bottom
    return (bottom - top) / scale
  }
  return null
}

// There are multiple embedded DOM nodes for a draft.js block. Inside there is a `span`, which
// has the correct width, but the height is not full. Outside of that there is a `div`, which
// has the correct height, but width always fills the parent, so it is not suitable for text width
// measurement. So for height measurement, use `div` nodeType, for width measurement, use `span`
// nodeType.
export function getDomNodeOfDraftJSBlock(
  block: ContentBlock,
  nodeType: 'span' | 'div',
  draftEditor: DraftEditor,
): any {
  const editorElement = (draftEditor as any).editor
  return editorElement.querySelector(`${nodeType}[data-offset-key="${block.getKey()}-0-0"]`)
}

type StyleMap = { [key: string]: Record<string, any> }

export function createDraftStyles(): { styles: any; customStyleFn: any } {
  const styles: StyleMap = {
    BOLD: {
      'font-weight': 600,
    },
    ITALIC: {
      'font-style': 'italic',
    },
    UNDERLINE: {
      'text-decoration': 'underline',
    },
  }

  return createStyles(
    ['font-family', 'font-size', 'line-height', 'letter-spacing'],
    'CUSTOM_',
    styles,
  )
}

export function draftContentToJsxAttributeValue(content: ContentState) {
  const rawContent = convertToRaw(content)
  if (isRawContentFormatted(rawContent)) {
    return jsxAttributeValue(rawContent, emptyComments)
  } else {
    return jsxAttributeValue(content.getPlainText(), emptyComments)
  }
}

function isRawContentFormatted(rawContent: RawDraftContentState): boolean {
  const rawBlocks = rawContent.blocks
  return rawBlocks.some((block) => block.inlineStyleRanges.length > 0)
}
