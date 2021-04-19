import {
  JSXElement,
  jsxElement,
  jsxAttributeValue,
  jsxElementName,
  jsxAttributesFromMap,
  JSXElementChildren,
} from '../../core/shared/element-template'
import { NormalisedFrame } from 'utopia-api'
import { emptyComments } from '../../core/workers/parser-printer/parser-printer-comments'

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

  return jsxElement(jsxElementName('Scene', []), props, children)
}

export function defaultViewElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('View', []),
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
    jsxElementName('View', []),
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
    jsxAttributesFromMap({
      style: jsxAttributeValue(
        {
          fontSize: 16,
        },
        emptyComments,
      ),
      text: jsxAttributeValue('Text', emptyComments),
      'data-uid': jsxAttributeValue(uid, emptyComments),
    }),
    [],
  )
}

export function defaultRectangleElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('Rectangle', []),
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
