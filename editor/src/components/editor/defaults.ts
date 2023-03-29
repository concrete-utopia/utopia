import {
  JSXElement,
  jsxElement,
  jsExpressionValue,
  jsxElementName,
  jsxAttributesFromMap,
  JSXElementChildren,
  emptyComments,
  jsxTextBlock,
  JSExpression,
  jsxAttributesEntry,
  simpleAttribute,
} from '../../core/shared/element-template'
import { NormalisedFrame } from 'utopia-api/core'
import { defaultImageAttributes } from '../shared/project-components'

export function defaultSceneElement(
  uid: string,
  frame: NormalisedFrame,
  label: string,
  children: JSXElementChildren,
): JSXElement {
  const props = jsxAttributesFromMap({
    'data-uid': jsExpressionValue(uid, emptyComments),
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

export function defaultViewElementStyle(): JSExpression {
  return jsExpressionValue(
    {
      backgroundColor: '#aaaaaa33',
      position: 'absolute',
    },
    emptyComments,
  )
}

export function defaultViewElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('View', []),
    uid,
    jsxAttributesFromMap({
      style: defaultViewElementStyle(),
      'data-uid': jsExpressionValue(uid, emptyComments),
    }),
    [],
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
      'data-uid': jsExpressionValue(uid, emptyComments),
    }),
    [],
  )
}

export function defaultTransparentViewElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('div', []),
    uid,
    jsxAttributesFromMap({
      style: jsExpressionValue(
        {
          position: 'absolute',
        },
        emptyComments,
      ),
      'data-uid': jsExpressionValue(uid, emptyComments),
    }),
    [],
  )
}

export function defaultUnstyledDivElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('div', []),
    uid,
    jsxAttributesFromMap({
      'data-uid': jsExpressionValue(uid, emptyComments),
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
      'data-uid': jsExpressionValue(uid, emptyComments),
    }),
    [],
  )
}

export function defaultEllipseElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('Ellipse', []),
    uid,
    jsxAttributesFromMap({
      style: defaultViewElementStyle(),
      'data-uid': jsExpressionValue(uid, emptyComments),
    }),
    [],
  )
}

export function defaultDivElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('div', []),
    uid,
    jsxAttributesFromMap({
      style: defaultViewElementStyle(),
      'data-uid': jsExpressionValue(uid, emptyComments),
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
      'data-uid': jsExpressionValue(uid, emptyComments),
    }),
    [],
  )
}

export function defaultImgElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('img', []),
    uid,
    [...defaultImageAttributes, simpleAttribute('data-uid', uid)],
    [],
  )
}

export function defaultButtonElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('button', []),
    uid,
    jsxAttributesFromMap({
      'data-uid': jsExpressionValue(uid, emptyComments),
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
