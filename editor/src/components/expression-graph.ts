import * as G from 'graphlib'
import { Graph } from 'graphlib'

export type Dependency<T> = {
  token: string
  id: string
  path: T
}

export type EvaluatedDependency = {
  token: string
  value: any
}

export type Expression<T> = {
  id: string
  path: T
  expression: string
  dependencies: Array<Dependency<T>>
}

export type ExpressionGraph<T, U> = {
  graph: Graph
  node: (id: T) => U | undefined
}

export type EvaluateExpressionsCircularReference<T> = {
  type: 'circularreference'
  elements: Array<T>
}

export type EvaluateExpressionsExceptionThrown = {
  type: 'exception'
  error: Error
}

export type EvaluateExpressionsFailure<T> =
  | EvaluateExpressionsCircularReference<T>
  | EvaluateExpressionsExceptionThrown

export type EvaluateExpressionResult<T> = {
  reference: T
  result: any
}

export type EvaluateExpressionsSuccess<T> = {
  type: 'success'
  results: Array<EvaluateExpressionResult<T>>
}

export type EvaluateExpressionsResult<T> =
  | EvaluateExpressionsSuccess<T>
  | EvaluateExpressionsFailure<T>

export type Evaluator<T> = (
  expression: string,
  id: T,
  dependencies: Array<EvaluatedDependency>,
) => any

export type ExpressionNode<T> = {
  expression: Expression<T>
  value: any | undefined
}

export type InvokeWithUncacheable = (...params: Array<any>) => any

export type PrebuildEvaluator<T> = (
  expression: string,
  dependencies: Array<Dependency<T>>,
) => InvokeWithUncacheable

export type PrebuiltExpressionFunction<T> = {
  id: string
  path: T
  dependencies: Array<Dependency<T>>
  func: InvokeWithUncacheable
}

export type PrebuildExpressionsSuccess<T> = {
  type: 'success'
  prebuiltExpressions: Array<PrebuiltExpressionFunction<T>>
}

export type PrebuildExpressionsCircularReference<T> = {
  type: 'circularreference'
  elements: Array<T>
}

export type PrebuildExpressionsExceptionThrown = {
  type: 'exception'
  error: Error
}

export type PrebuildExpressionsFailure<T> =
  | PrebuildExpressionsCircularReference<T>
  | PrebuildExpressionsExceptionThrown

export type PrebuildExpressionsResult<T> =
  | PrebuildExpressionsFailure<T>
  | PrebuildExpressionsSuccess<T>

export function createGraph<T, U>(
  expressions: Array<Expression<T>>,
  valueForNode: (value: Expression<T>) => U,
): ExpressionGraph<string, U> {
  const graph = new Graph()
  for (const expression of expressions) {
    const nodeId = expression.id
    graph.setNode(nodeId, valueForNode(expression))
    for (const dependency of expression.dependencies) {
      const depNodeId = dependency.id
      graph.setEdge(nodeId, depNodeId)
    }
  }
  return {
    graph: graph,
    node: (id: string) => {
      return graph.node(id)
    },
  }
}

export function prebuildExpressions<T>(
  expressions: Array<Expression<T>>,
  idToString: (id: T) => string,
  idFromString: (id: string) => T,
  evaluator: PrebuildEvaluator<T>,
): PrebuildExpressionsResult<T> {
  try {
    const graph = createGraph<T, Expression<T>>(expressions, (v) => v)

    // Check for cycles in the graph, as that means we can't evaluate the graph.
    const hasCycles = !G.alg.isAcyclic(graph.graph)
    if (hasCycles) {
      const cycles = G.alg.findCycles(graph.graph)
      return {
        type: 'circularreference',
        elements: cycles[0].map(idFromString),
      }
    } else {
      // Creates a traversal for the expressions from the bottom up.
      const graphProcessingOrder = G.alg.postorder(
        graph.graph,
        expressions.map((e) => e.id),
      )

      const prebuiltExpressions: Array<PrebuiltExpressionFunction<T>> =
        graphProcessingOrder.flatMap((graphNode) => {
          const expression = graph.node(graphNode)
          if (expression == null) {
            return []
          } else {
            const func = evaluator(expression.expression, expression.dependencies)
            return [
              {
                id: expression.id,
                path: expression.path,
                dependencies: expression.dependencies,
                func: func,
              },
            ]
          }
        })

      return {
        type: 'success',
        prebuiltExpressions: prebuiltExpressions,
      }
    }
  } catch (e: any) {
    return {
      type: 'exception',
      error: e,
    }
  }
}

export function evaluateExpressions<T>(
  expressions: Array<Expression<T>>,
  idToString: (id: T) => string,
  idFromString: (id: string) => T,
  valueLookup: (id: T) => any,
  evaluator: Evaluator<T>,
): EvaluateExpressionsResult<T> {
  try {
    const valueForNode = (expression: Expression<T>) => {
      return {
        expression: expression,
        value: undefined,
      }
    }
    const graph = createGraph<T, ExpressionNode<T>>(expressions, valueForNode)
    // Check for cycles in the graph, as that means we can't evaluate the graph.
    const cycles = G.alg.findCycles(graph.graph)
    if (cycles != null && cycles.length > 0) {
      return {
        type: 'circularreference',
        elements: cycles[0].map(idFromString),
      }
    } else {
      // Creates a traversal for the expressions from the bottom up.
      const graphProcessingOrder = G.alg.postorder(
        graph.graph,
        expressions.map((e) => e.id),
      )
      for (const graphNode of graphProcessingOrder) {
        const expressionNode: ExpressionNode<T> | undefined = graph.graph.node(graphNode)
        // If this is undefined, it means we've hit something that we'll use the value lookup for later.
        // As we won't have inserted a node for this.
        if (expressionNode != null) {
          const expressionToEvaluate = expressionNode.expression
          // Build Dependencies object with resolved values assigned.
          var evaluatorDependencies: Array<EvaluatedDependency> = []
          const dependencies = expressionToEvaluate.dependencies
          for (const dependency of dependencies) {
            // Determine if this is a reference.
            const possibleNode = graph.node(dependency.id)
            let value: any
            if (possibleNode == null) {
              // Doesn't refer to a recursive expression.
              value = valueLookup(dependency.path)
            } else {
              // Refers to an expression that we should have the value of in the graph already.
              value = possibleNode.value
            }
            evaluatorDependencies.push({
              token: dependency.token,
              value: value,
            })
          }
          // Evaluate expression with the context, squirreling away the result into the graph.
          expressionNode.value = evaluator(
            expressionToEvaluate.expression,
            expressionNode.expression.path,
            evaluatorDependencies,
          )
        }
      }

      const results = expressions.map((e) => {
        return {
          reference: e.path,
          result: graph.graph.node(e.id).value,
        }
      })

      return {
        type: 'success',
        results: results,
      }
    }
  } catch (e: any) {
    return {
      type: 'exception',
      error: e,
    }
  }
}
