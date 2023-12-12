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

export function rescopeCSSToTargetCanvasOnly(input: string): string {
  let ast = csstree.parse(input)

  csstree.walk(ast, (node) => {
    // We want to find all selectors, and prepend '#canvas-container ' (i.e. the canvas-container
    // ID Selector and a ' ' combinator) so that they will only apply to descendents of the canvas
    if (node.type === 'Selector') {
      const firstChild = node.children.first()

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
