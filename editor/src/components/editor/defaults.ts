import {
  JSXElement,
  jsxElement,
  jsxAttributeValue,
  jsxElementName,
  jsxAttributesFromMap,
  JSXElementChildren,
  emptyComments,
  jsxTextBlock,
} from '../../core/shared/element-template'
import { NormalisedFrame } from 'utopia-api'

export function defaultSceneElement(
  uid: string,
  frame: NormalisedFrame,
  label: string,
  children: JSXElementChildren,
): JSXElement {
  const props = jsxAttributesFromMap({
    'data-uid': jsxAttributeValue(uid, emptyComments),
    'data-label': jsxAttributeValue(label, emptyComments),
    style: jsxAttributeValue(
      {
        position: 'absolute',
        ...frame,
      },
      emptyComments,
    ),
  })

  return jsxElement(jsxElementName('Scene', []), uid, props, children)
}

export function defaultViewElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('View', []),
    uid,
    jsxAttributesFromMap({
      style: jsxAttributeValue(
        {
          backgroundColor: '#0091FFAA',
          position: 'absolute',
        },
        emptyComments,
      ),
      'data-uid': jsxAttributeValue(uid, emptyComments),
    }),
    [],
  )
}

export function defaultAnimatedDivElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('animated', ['div']),
    uid,
    jsxAttributesFromMap({
      style: jsxAttributeValue(
        {
          backgroundColor: '#0091FFAA',
        },
        emptyComments,
      ),
      'data-uid': jsxAttributeValue(uid, emptyComments),
    }),
    [],
  )
}

export function defaultTransparentViewElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('div', []),
    uid,
    jsxAttributesFromMap({
      style: jsxAttributeValue(
        {
          position: 'absolute',
        },
        emptyComments,
      ),
      'data-uid': jsxAttributeValue(uid, emptyComments),
    }),
    [],
  )
}

export function defaultTextElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('Text', []),
    uid,
    jsxAttributesFromMap({
      style: jsxAttributeValue(
        {
          fontSize: 16,
        },
        emptyComments,
      ),
      'data-uid': jsxAttributeValue(uid, emptyComments),
    }),
    [jsxTextBlock('Text')],
  )
}

export function defaultRectangleElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('Rectangle', []),
    uid,
    jsxAttributesFromMap({
      style: jsxAttributeValue(
        {
          backgroundColor: '#0091FFAA',
        },
        emptyComments,
      ),
      'data-uid': jsxAttributeValue(uid, emptyComments),
    }),
    [],
  )
}

export function defaultEllipseElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('Ellipse', []),
    uid,
    jsxAttributesFromMap({
      style: jsxAttributeValue(
        {
          backgroundColor: '#0091FFAA',
        },
        emptyComments,
      ),
      'data-uid': jsxAttributeValue(uid, emptyComments),
    }),
    [],
  )
}

export function defaultDivElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('div', []),
    uid,
    jsxAttributesFromMap({
      style: jsxAttributeValue(
        {
          backgroundColor: '#0091FFAA',
          position: 'absolute',
        },
        emptyComments,
      ),
      'data-uid': jsxAttributeValue(uid, emptyComments),
    }),
    [],
  )
}
