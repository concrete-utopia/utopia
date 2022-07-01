import {
  JSXElement,
  jsxElement,
  jsxAttributeValue,
  jsxElementName,
  jsxAttributesFromMap,
  JSXElementChildren,
  emptyComments,
  jsxTextBlock,
  JSXAttribute,
} from '../../core/shared/element-template'
import { NormalisedFrame } from 'utopia-api/core'

export function defaultSceneElement(
  uid: string,
  frame: NormalisedFrame,
  label: string,
  children: JSXElementChildren,
): JSXElement {
  const props = jsxAttributesFromMap({
    'data-uid': jsxAttributeValue(uid, emptyComments),
    'data-label': jsxAttributeValue(label, emptyComments),
    style: defaultSceneElementStyle(frame),
  })

  return jsxElement(jsxElementName('Scene', []), uid, props, children)
}

export function defaultSceneElementStyle(frame: NormalisedFrame | null): JSXAttribute {
  return jsxAttributeValue(
    {
      position: 'absolute',
      ...frame,
    },
    emptyComments,
  )
}

export function defaultViewElementStyle(): JSXAttribute {
  return jsxAttributeValue(
    {
      backgroundColor: '#0091FFAA',
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

export function defaultTextElementStyle(): JSXAttribute {
  return jsxAttributeValue(
    {
      fontSize: 16,
    },
    emptyComments,
  )
}

export function defaultTextElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('Text', []),
    uid,
    jsxAttributesFromMap({
      style: defaultTextElementStyle(),
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
      style: defaultViewElementStyle(),
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
      style: defaultViewElementStyle(),
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
      style: defaultViewElementStyle(),
      'data-uid': jsxAttributeValue(uid, emptyComments),
    }),
    [],
  )
}

export function defaultFlexRowOrColStyle(): JSXAttribute {
  return jsxAttributeValue(
    {
      position: 'absolute',
    },
    emptyComments,
  )
}
