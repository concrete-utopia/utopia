import {
  Identifier,
  createPropertyAccess,
  createIdentifier,
  Expression,
  createCall,
  LiteralLikeNode,
  createStringLiteral,
  BinaryOperator,
  createBinary,
  NodeFlags,
  SyntaxKind,
  createTrue,
  createFalse,
} from 'typescript'

// eslint-disable-next-line @typescript-eslint/ban-types
function getEnumMembers<T extends Object>(enumObject: T) {
  const result: [number, string][] = []
  for (const name in enumObject) {
    const value = enumObject[name]
    if (typeof value === 'number') {
      result.push([value, name])
    }
  }

  return result.sort((a, b) => a[0] - b[0])
}

// eslint-disable-next-line @typescript-eslint/ban-types
function formatFlags<T extends Object>(value: number = 0, enumObject: T): string[] {
  const members = getEnumMembers(enumObject)
  if (value === 0) {
    return members.length > 0 && members[0][0] === 0 ? [members[0][1]] : ([] as string[])
  }

  const result: string[] = []
  let remainingFlags = value
  for (let i = members.length - 1; i >= 0 && remainingFlags !== 0; i--) {
    const [enumValue, enumName] = members[i]
    if (enumValue !== 0 && (remainingFlags & enumValue) === enumValue) {
      remainingFlags &= ~enumValue
      result.push(enumName)
    }
  }
  return result
}

export function createTsAccess(id: Identifier) {
  return createPropertyAccess(createIdentifier('ts'), id)
}

export function createTsCall(id: string, args?: Expression[]) {
  return createCall(createTsAccess(createIdentifier(id)), undefined, args)
}

export function createLiteralCall(node: LiteralLikeNode, func: string) {
  return createTsCall(func, [createStringLiteral(node.text)])
}

function connectBinary(op: BinaryOperator, nodes: Expression[]): Expression {
  if (nodes.length === 0) {
    return createIdentifier('undefined')
  }
  if (nodes.length === 1) {
    return nodes[0]
  }
  return createBinary(nodes[0], op, connectBinary(op, nodes.slice(1)))
}

// NodeFlags.PossiblyContainsDynamicImport, NodeFlags.PossiblyContainsImportMeta, NodeFlags.Ambient, NodeFlags.InWithStatement
const internalFlags: number = (1 << 19) | (1 << 20) | (1 << 22) | (1 << 23)
function filterInternalFlags(flags: NodeFlags): NodeFlags {
  // eslint-disable-next-line no-param-reassign
  return (flags &= ~internalFlags)
}

export function transformInternalSyntaxKind(syntaxKind: string) {
  switch (syntaxKind) {
    case 'FirstContextualKeyword':
      return 'AbstractKeyword'
    case 'LastContextualKeyword':
      return 'OfKeyword'
    default:
      return syntaxKind
  }
}

export function createNodeFlags(flags: NodeFlags) {
  const formattedFlags = formatFlags(filterInternalFlags(flags), NodeFlags).map((f) =>
    createPropertyAccess(createTsAccess(createIdentifier('NodeFlags')), createIdentifier(f)),
  )
  return connectBinary(SyntaxKind.BarToken, formattedFlags)
}

/** @internal */
export function createBooleanLiteral(bool: boolean | undefined) {
  if (bool === undefined) {
    return createIdentifier('undefined')
  }
  return bool ? createTrue() : createFalse()
}
