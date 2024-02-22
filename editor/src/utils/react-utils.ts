import React from 'react'
import type {
  Attributes,
  CElement,
  ClassAttributes,
  ClassicComponent,
  ClassicComponentClass,
  ClassType,
  Component,
  ComponentClass,
  ComponentState,
  DetailedReactHTMLElement,
  DOMAttributes,
  DOMElement,
  HTMLAttributes,
  Key,
  ReactElement,
  ReactHTML,
  ReactNode,
  ReactSVG,
  ReactSVGElement,
  FC,
  FunctionComponentElement,
  SVGAttributes,
} from 'react'

class RU {
  // Utility function to interact with react. Provides the following advantages:
  // - forces the use of a key
  // - by having key and children as separate properties, the attrs param has the correct shape for the component model
  // - it's shorter than createElement :p

  // DOM Elements
  static create<P extends HTMLAttributes<T>, T extends HTMLElement>(
    type: keyof ReactHTML,
    props?: { key: Key } & ClassAttributes<T> & P,
    ...children: ReactNode[]
  ): DetailedReactHTMLElement<P, T>
  static create<P extends SVGAttributes<T>, T extends SVGElement>(
    type: keyof ReactSVG,
    props?: { key: Key } & ClassAttributes<T> & P,
    ...children: ReactNode[]
  ): ReactSVGElement
  static create<P extends DOMAttributes<T>, T extends HTMLElement>(
    type: string,
    props?: { key: Key } & ClassAttributes<T> & P,
    ...children: ReactNode[]
  ): DOMElement<P, T>

  // Custom components
  static create<P>(
    type: FC<React.PropsWithChildren<P>>,
    props?: { key: Key } & Attributes & P,
    ...children: ReactNode[]
  ): FunctionComponentElement<P>
  static create<P>(
    type: ClassType<P, ClassicComponent<P, ComponentState>, ClassicComponentClass<P>>,
    props?: { key: Key } & ClassAttributes<ClassicComponent<P, ComponentState>> & P,
    ...children: ReactNode[]
  ): CElement<P, ClassicComponent<P, ComponentState>>
  static create<P, T extends Component<P, ComponentState>, C extends ComponentClass<P>>(
    type: ClassType<P, T, C>,
    props?: { key: Key } & ClassAttributes<T> & P,
    ...children: ReactNode[]
  ): CElement<P, T>
  static create<P>(
    type: ComponentClass<P>,
    props?: { key: Key } & Attributes & P,
    ...children: ReactNode[]
  ): ReactElement<P>

  static create(type: any, attrs: any, ...children: any[]): ReactElement<any> {
    if (attrs != null && attrs.key == null) {
      const elemName = type.name ?? 'unknown'
      throw new Error(`Please provide a key for ${elemName} in RU.create props!`)
    }
    return React.createElement(type, attrs, ...children)
  }
  // This function allows us to extract children from the model in a Flow friendly way
  // We need to do this because React will merge the children into the model at runtime
  static children(attrs: any): Array<ReactElement<any>> | null {
    return attrs.children
  }
}

export function isValidReactNode(node: unknown) {
  return (
    React.isValidElement(node) ||
    typeof node === 'string' ||
    typeof node === 'number' ||
    node === null
  )
}

export default RU
