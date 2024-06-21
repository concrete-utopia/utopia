import type {
  JSXElement,
  JSXElementChildren,
  JSExpression,
} from '../../core/shared/element-template'
import {
  jsxElement,
  jsExpressionValue,
  jsxElementName,
  jsxAttributesFromMap,
  emptyComments,
  jsExpressionWithPrintBehavior,
  jsxAttributesEntry,
} from '../../core/shared/element-template'
import type { NormalisedFrame } from 'utopia-api/core'
import { defaultImageAttributes } from '../shared/project-components'
import type { PrintBehavior } from 'utopia-shared/src/types'

export function dataUIDExpression(
  uid: string,
  printBehavior: PrintBehavior = 'skip-printing',
): JSExpression {
  return jsExpressionWithPrintBehavior(jsExpressionValue(uid, emptyComments), printBehavior)
}

export function defaultSceneElement(
  uid: string,
  frame: NormalisedFrame,
  label: string,
  children: JSXElementChildren,
): JSXElement {
  const props = jsxAttributesFromMap({
    'data-uid': dataUIDExpression(uid),
    'data-label': jsExpressionValue(label, emptyComments),
    style: defaultSceneElementStyle(frame),
  })

  return jsxElement(jsxElementName('Scene', []), uid, props, children)
}

export function defaultSceneElementStyle(frame: NormalisedFrame | null): JSExpression {
  return jsExpressionValue(
    {
      position: 'absolute',
      ...frame,
    },
    emptyComments,
  )
}

export function defaultElementStyle(): JSExpression {
  return jsExpressionValue(
    {
      backgroundColor: '#aaaaaa33',
      position: 'absolute',
    },
    emptyComments,
  )
}

export function defaultAnimatedDivElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('animated', ['div']),
    uid,
    jsxAttributesFromMap({
      style: jsExpressionValue(
        {
          backgroundColor: '#aaaaaa33',
        },
        emptyComments,
      ),
      'data-uid': dataUIDExpression(uid),
    }),
    [],
  )
}

export function defaultUnstyledDivElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('div', []),
    uid,
    jsxAttributesFromMap({
      'data-uid': dataUIDExpression(uid),
    }),
    [],
  )
}

export function defaultTextElementStyle(): JSExpression {
  return jsExpressionValue(
    {
      fontSize: 16,
    },
    emptyComments,
  )
}

export function defaultRectangleElementStyle(): JSExpression {
  return jsExpressionValue(
    {
      backgroundColor: '#FF69B4AB',
      position: 'absolute',
    },
    emptyComments,
  )
}

export function defaultRectangleElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('Rectangle', []),
    uid,
    jsxAttributesFromMap({
      style: defaultRectangleElementStyle(),
      'data-uid': dataUIDExpression(uid),
    }),
    [],
  )
}

export function defaultEllipseElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('Ellipse', []),
    uid,
    jsxAttributesFromMap({
      style: defaultElementStyle(),
      'data-uid': dataUIDExpression(uid),
    }),
    [],
  )
}

export function defaultDivElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('div', []),
    uid,
    jsxAttributesFromMap({
      style: defaultElementStyle(),
      'data-uid': dataUIDExpression(uid),
    }),
    [],
  )
}

export function defaultSpanElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('span', []),
    uid,
    jsxAttributesFromMap({
      style: jsExpressionValue(
        {
          position: 'absolute',
          wordBreak: 'break-word',
        },
        emptyComments,
      ),
      'data-uid': dataUIDExpression(uid),
    }),
    [],
  )
}

export function defaultImgElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('img', []),
    uid,
    [
      ...defaultImageAttributes(),
      jsxAttributesEntry(
        'data-uid',
        jsExpressionValue('data-uid', emptyComments),
        emptyComments,
        'skip-printing',
      ),
    ],
    [],
  )
}

export function defaultButtonElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('button', []),
    uid,
    jsxAttributesFromMap({
      'data-uid': dataUIDExpression(uid),
      style: jsExpressionValue({ position: 'absolute' }, emptyComments),
    }),
    [],
  )
}

export function defaultFlexRowOrColStyle(): JSExpression {
  return jsExpressionValue(
    {
      position: 'absolute',
    },
    emptyComments,
  )
}
