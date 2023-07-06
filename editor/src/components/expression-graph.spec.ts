// disaling jest expect rules because this file uses Chai.expect
/* eslint-disable jest/valid-expect */
import * as Chai from 'chai'
const expect = Chai.expect
import Utils from '../utils/utils'

import * as EG from './expression-graph'
import type {
  Expression,
  Evaluator,
  EvaluatedDependency,
  EvaluateExpressionsResult,
  EvaluateExpressionResult,
} from './expression-graph'
import { replaceAll } from '../core/shared/string-utils'

function sortResults(
  results: EvaluateExpressionsResult<string>,
): EvaluateExpressionsResult<string> {
  function sortArray(
    array: Array<EvaluateExpressionResult<string>>,
  ): Array<EvaluateExpressionResult<string>> {
    let result = [...array]
    result.sort((firstElem, secondElem) => firstElem.reference.localeCompare(secondElem.reference))
    return result
  }
  if (results.type == 'success') {
    return {
      ...results,
      results: sortArray(results.results),
    }
  } else {
    return results
  }
}

describe('evaluateExpressions', function () {
  it('evaluates two expressions with dependencies on each other', function () {
    const expression1: Expression<string> = {
      id: 'expression1',
      path: 'expression1',
      expression: '@@expression2@@ + 100',
      dependencies: [
        {
          token: '@@expression2@@',
          id: 'expression2',
          path: 'expression2',
        },
      ],
    }
    const expression2: Expression<string> = {
      id: 'expression2',
      path: 'expression2',
      expression: '@@expression1@@ + 100',
      dependencies: [
        {
          token: '@@expression1@@',
          id: 'expression1',
          path: 'expression1',
        },
      ],
    }
    const idToString = (id: string) => id
    const idFromString = (id: string) => id
    const valueLookup = (id: string) => {
      throw new Error('Should not be triggered.')
    }
    const evaluator: Evaluator<string> = (
      expression: string,
      id: string,
      dependencies: Array<EvaluatedDependency>,
    ) => {
      var context: any = {}
      for (const dependency of dependencies) {
        const dependencyVarName: string = replaceAll(dependency.token, '@', '')
        context[dependencyVarName] = dependency.value
      }
      const fixedExpression = replaceAll(expression, '@', '')
      return Utils.SafeFunction(
        false,
        context,
        'test.js',
        'return ' + fixedExpression,
        null,
        [],
        (e) => {
          throw e
        },
      )(null)
    }
    const expectedResult: EvaluateExpressionsResult<string> = {
      type: 'circularreference',
      elements: ['expression2', 'expression1'],
    }
    const expressions = [expression1, expression2]
    const actualResult = EG.evaluateExpressions(
      expressions,
      idToString,
      idFromString,
      valueLookup,
      evaluator,
    )

    expect(sortResults(actualResult)).to.deep.equal(expectedResult)
  })
  it('evaluates a web of expressions', function () {
    const expression1: Expression<string> = {
      id: 'expression1',
      path: 'expression1',
      expression: '@@expression2@@ + @@expression3@@ + 100',
      dependencies: [
        {
          token: '@@expression2@@',
          id: 'expression2',
          path: 'expression2',
        },
        {
          token: '@@expression3@@',
          id: 'expression3',
          path: 'expression3',
        },
      ],
    }
    const expression2: Expression<string> = {
      id: 'expression2',
      path: 'expression2',
      expression: '@@expression4@@ + @@expression5@@',
      dependencies: [
        {
          token: '@@expression4@@',
          id: 'expression4',
          path: 'expression4',
        },
        {
          token: '@@expression5@@',
          id: 'expression5',
          path: 'expression5',
        },
      ],
    }
    const expression3: Expression<string> = {
      id: 'expression3',
      path: 'expression3',
      expression: '@@expression6@@ + @@expression7@@',
      dependencies: [
        {
          token: '@@expression6@@',
          id: 'expression6',
          path: 'expression6',
        },
        {
          token: '@@expression7@@',
          id: 'expression7',
          path: 'expression7',
        },
      ],
    }
    const expression4: Expression<string> = {
      id: 'expression4',
      path: 'expression4',
      expression: '100',
      dependencies: [],
    }
    const expression5: Expression<string> = {
      id: 'expression5',
      path: 'expression5',
      expression: '103',
      dependencies: [],
    }
    const expression6: Expression<string> = {
      id: 'expression6',
      path: 'expression6',
      expression: '234',
      dependencies: [],
    }
    const expression7: Expression<string> = {
      id: 'expression7',
      path: 'expression7',
      expression: '99',
      dependencies: [],
    }
    const idToString = (id: string) => id
    const idFromString = (id: string) => id
    const valueLookup = (id: string) => {
      throw new Error('Should not be triggered.')
    }
    const evaluator: Evaluator<string> = (
      expression: string,
      id: string,
      dependencies: Array<EvaluatedDependency>,
    ) => {
      var context: any = {}
      for (const dependency of dependencies) {
        const dependencyVarName: string = replaceAll(dependency.token, '@', '')
        context[dependencyVarName] = dependency.value
      }
      const fixedExpression = replaceAll(expression, '@', '')
      return Utils.SafeFunction(
        false,
        context,
        'test.js',
        'return ' + fixedExpression,
        null,
        [],
        (e) => {
          throw e
        },
      )(null)
    }
    const expectedResult: EvaluateExpressionsResult<string> = {
      type: 'success',
      results: [
        {
          reference: 'expression1',
          result: 636,
        },
        {
          reference: 'expression2',
          result: 203,
        },
        {
          reference: 'expression3',
          result: 333,
        },
        {
          reference: 'expression4',
          result: 100,
        },
        {
          reference: 'expression5',
          result: 103,
        },
        {
          reference: 'expression6',
          result: 234,
        },
        {
          reference: 'expression7',
          result: 99,
        },
      ],
    }
    const expressions = [
      expression6,
      expression4,
      expression1,
      expression2,
      expression5,
      expression7,
      expression3,
    ]
    const actualResult = EG.evaluateExpressions(
      expressions,
      idToString,
      idFromString,
      valueLookup,
      evaluator,
    )

    expect(sortResults(actualResult)).to.deep.equal(expectedResult)
  })
  it('evaluates two expressions with a dependency from one to the other', function () {
    const expression1: Expression<string> = {
      id: 'expression1',
      path: 'expression1',
      expression: '25',
      dependencies: [],
    }
    const expression2: Expression<string> = {
      id: 'expression2',
      path: 'expression2',
      expression: '@@expression1@@ + 100',
      dependencies: [
        {
          token: '@@expression1@@',
          id: 'expression1',
          path: 'expression1',
        },
      ],
    }
    const idToString = (id: string) => id
    const idFromString = (id: string) => id
    const valueLookup = (id: string) => {
      throw new Error('Should not be triggered.')
    }
    const evaluator: Evaluator<string> = (
      expression: string,
      id: string,
      dependencies: Array<EvaluatedDependency>,
    ) => {
      var context: any = {}
      for (const dependency of dependencies) {
        const dependencyVarName: string = replaceAll(dependency.token, '@', '')
        context[dependencyVarName] = dependency.value
      }
      const fixedExpression = replaceAll(expression, '@', '')
      return Utils.SafeFunction(
        false,
        context,
        'test.js',
        'return ' + fixedExpression,
        null,
        [],
        (e) => {
          throw e
        },
      )(null)
    }
    const expectedResult: EvaluateExpressionsResult<string> = {
      type: 'success',
      results: [
        {
          reference: 'expression1',
          result: 25,
        },
        {
          reference: 'expression2',
          result: 125,
        },
      ],
    }
    const expressions1First = [expression1, expression2]
    const expressions2First = [expression2, expression1]
    const actualResult1First = EG.evaluateExpressions(
      expressions1First,
      idToString,
      idFromString,
      valueLookup,
      evaluator,
    )
    const actualResult2First = EG.evaluateExpressions(
      expressions2First,
      idToString,
      idFromString,
      valueLookup,
      evaluator,
    )

    expect(sortResults(actualResult1First)).to.deep.equal(expectedResult)
    expect(sortResults(actualResult2First)).to.deep.equal(expectedResult)
  })
  it('evaluates two independent expressions', function () {
    const expression1: Expression<string> = {
      id: 'expression1',
      path: 'expression1',
      expression: '@@expression1value@@',
      dependencies: [
        {
          token: '@@expression1value@@',
          id: 'expression1value',
          path: 'expression1value',
        },
      ],
    }
    const expression2: Expression<string> = {
      id: 'expression2',
      path: 'expression2',
      expression: '@@expression2value@@',
      dependencies: [
        {
          token: '@@expression2value@@',
          id: 'expression2value',
          path: 'expression2value',
        },
      ],
    }
    const idToString = (id: string) => id
    const idFromString = (id: string) => id
    const valueLookup = (id: string) => {
      switch (id) {
        case 'expression1value':
          return 101
        case 'expression2value':
          return 246
        default:
          throw new Error('Should not be triggered.')
      }
    }
    const evaluator: Evaluator<string> = (
      expression: string,
      id: string,
      dependencies: Array<EvaluatedDependency>,
    ) => {
      var context: any = {}
      for (const dependency of dependencies) {
        const dependencyVarName: string = replaceAll(dependency.token, '@', '')
        context[dependencyVarName] = dependency.value
      }
      const fixedExpression = replaceAll(expression, '@', '')
      return Utils.SafeFunction(
        false,
        context,
        'test.js',
        'return ' + fixedExpression,
        null,
        [],
        (e) => {
          throw e
        },
      )(null)
    }
    const expectedResult: EvaluateExpressionsResult<string> = {
      type: 'success',
      results: [
        {
          reference: 'expression1',
          result: 101,
        },
        {
          reference: 'expression2',
          result: 246,
        },
      ],
    }
    const expressions = [expression1, expression2]
    const actualResult = EG.evaluateExpressions(
      expressions,
      idToString,
      idFromString,
      valueLookup,
      evaluator,
    )

    expect(sortResults(actualResult)).to.deep.equal(expectedResult)
  })
  it('evaluates an expression that depends on a value', function () {
    const expression2: Expression<string> = {
      id: 'expression2',
      path: 'expression2',
      expression: '@@expression1@@ + 100',
      dependencies: [
        {
          token: '@@expression1@@',
          id: 'expression1',
          path: 'expression1',
        },
      ],
    }
    const idToString = (id: string) => id
    const idFromString = (id: string) => id
    const valueLookup = (id: string) => {
      if (id == 'expression1') {
        return 99
      } else {
        throw new Error(`Invalid id: ${id}`)
      }
    }
    const evaluator: Evaluator<string> = (
      expression: string,
      id: string,
      dependencies: Array<EvaluatedDependency>,
    ) => {
      var context: any = {}
      for (const dependency of dependencies) {
        const dependencyVarName: string = replaceAll(dependency.token, '@', '')
        context[dependencyVarName] = dependency.value
      }
      const fixedExpression = replaceAll(expression, '@', '')
      return Utils.SafeFunction(
        false,
        context,
        'test.js',
        'return ' + fixedExpression,
        null,
        [],
        (e) => {
          throw e
        },
      )(null)
    }
    const expectedResult: EvaluateExpressionsResult<string> = {
      type: 'success',
      results: [
        {
          reference: 'expression2',
          result: 199,
        },
      ],
    }
    const expressions = [expression2]
    const actualResults = EG.evaluateExpressions(
      expressions,
      idToString,
      idFromString,
      valueLookup,
      evaluator,
    )

    expect(sortResults(actualResults)).to.deep.equal(expectedResult)
  })
})
