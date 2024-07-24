import * as csstree from 'css-tree'

export function parseCssTreeNodeValue(str: string): csstree.CssNode {
  return csstree.parse(str, { context: 'value' })
}

export function cssTreeNodeList(nodes: csstree.CssNode[]): csstree.List<csstree.CssNode> {
  let list = new csstree.List<csstree.CssNode>()
  nodes.forEach((node) => list.appendData(node))
  return list
}

export function cssTreeNodeValue(children: csstree.List<csstree.CssNode>): csstree.Value {
  return {
    type: 'Value',
    children: children,
  }
}

export function expandCssTreeNodeValue(node: csstree.CssNode): csstree.Value {
  return cssTreeNodeValue(expandNode(node))
}

function expandNode(node: csstree.CssNode): csstree.List<csstree.CssNode> {
  if (node.type === 'Function' && node.name === 'repeat') {
    // specific expansion for repeat functions
    return expandRepeatFunction(node)
  } else if (node.type === 'Value') {
    // recursively expand children of Value nodes
    const children = node.children.toArray()
    const expanded = children.flatMap((child) => expandNode(child).toArray())
    return cssTreeNodeList(expanded)
  } else {
    // fallback to just the verbatim node
    return cssTreeNodeList([node])
  }
}

function expandRepeatFunction(fnNode: csstree.FunctionNode): csstree.List<csstree.CssNode> {
  // The node should have 3+ children, because it should be [Number + Operator (,) + Value(s)]
  // TODO this should be extended to support non-numeric, keyword repeaters
  const children = fnNode.children.toArray()
  if (children.length < 3) {
    // just return the original children if the format is not supported
    return fnNode.children
  }

  // 1. parse the repeat number
  const repeatNumber = children[0]
  if (repeatNumber.type !== 'Number') {
    return fnNode.children
  }
  const times = parseInt(repeatNumber.value)

  // 2. grab ALL the values to repeat (rightside of the comma), wrap them in a new Value node,
  // and expand them so they support nested repeats
  const valuesToRepeat = children.slice(2)
  const nodeToRepeat = cssTreeNodeValue(cssTreeNodeList(valuesToRepeat))
  const expandedValues = expandNode(nodeToRepeat)

  // 3. append the expanded values N times, where N is the number of repeats parsed earlier
  let result = new csstree.List<csstree.CssNode>()
  for (let i = 0; i < times; i++) {
    expandedValues.forEach((v) => result.appendData(v))
  }

  return result
}
