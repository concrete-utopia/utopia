import * as csstree from 'css-tree'
import { CanvasContainerID } from '../../components/canvas/canvas-types'

const SelectorTypes = ['ClassSelector', 'IdSelector', 'TypeSelector']
const SelectorsToSkip = [
  // general case type selectors to skip
  'html',
  'head',

  // keyframe specific type selectors
  'from',
  'to',
]
const PseudoClassSelectorsToWalk = ['is', 'where']

export function rescopeCSSToTargetCanvasOnly(input: string): string {
  let ast = csstree.parse(input)

  csstree.walk(ast, (node) => {
    if (node.type === 'PseudoClassSelector' && !PseudoClassSelectorsToWalk.includes(node.name)) {
      // We don't want to walk pseudo classes with some exceptions
      // We can return the special value csstree.walk.skip to achieve this, though
      // the types seem to believe this doesn't exist, it does
      // https://github.com/csstree/csstree/blob/ba6dfd8bb0e33055c05f13803d04825d98dd2d8d/docs/traversal.md#walkast-options
      return (csstree.walk as any).skip
    }

    // We want to find all selectors, and prepend '#canvas-container ' (i.e. the canvas-container
    // ID Selector and a ' ' combinator) so that they will only apply to descendents of the canvas
    if (node.type === 'Selector') {
      const firstChild = node.children.first

      if (firstChild == null) {
        return
      }

      if (!SelectorTypes.includes(firstChild.type)) {
        return
      }

      if (firstChild.type === 'TypeSelector' && SelectorsToSkip.includes(firstChild.name)) {
        // Skip special selectors
        return
      }

      if (firstChild.type === 'TypeSelector' && firstChild.name === 'body') {
        // The closest analogy to the body here is the #canvas-container itself,
        // so let's just replace it
        node.children.shift()
        node.children.prependData(
          csstree.fromPlainObject({
            type: 'IdSelector',
            name: CanvasContainerID,
          }),
        )
      } else {
        // For everything else we want to prepent '#canvas-container '
        node.children.prependData(
          csstree.fromPlainObject({
            type: 'Combinator',
            name: ' ',
          }),
        )

        node.children.prependData(
          csstree.fromPlainObject({
            type: 'IdSelector',
            name: CanvasContainerID,
          }),
        )
      }
    }
  })

  return csstree.generate(ast)
}
