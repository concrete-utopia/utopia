import * as TS from 'typescript-for-the-editor'

export function jsonToExpression(json: any): TS.Expression {
  switch (typeof json) {
    case 'object':
      if (json == null) {
        return TS.createNull()
      }
      if (Array.isArray(json)) {
        const arrayExpressions: Array<TS.Expression> = json.map(jsonToExpression)
        return TS.createArrayLiteral(arrayExpressions)
      } else {
        const keys = Object.keys(json)
        const objectPropertyExpressions: Array<TS.ObjectLiteralElementLike> = keys.map((key) => {
          return TS.createPropertyAssignment(
            TS.createStringLiteral(key),
            jsonToExpression(json[key]),
          )
        })
        return TS.createObjectLiteral(objectPropertyExpressions)
      }
    case 'string':
      return TS.createStringLiteral(json)
    case 'boolean':
      return json ? TS.createTrue() : TS.createFalse()
    case 'number':
      if (json < 0) {
        return TS.createPrefix(TS.SyntaxKind.MinusToken, TS.createLiteral(json * -1))
      } else {
        return TS.createLiteral(json)
      }
    case 'undefined':
      return TS.createIdentifier('undefined')
    default:
      throw new Error(`Unsure how to handle ${JSON.stringify(json)}`)
  }
}
