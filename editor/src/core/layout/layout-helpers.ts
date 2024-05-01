import type { FlexLength, FlexStretch } from 'utopia-api/core'
import { getUnstretchedWidthHeight } from 'utopia-api/core'
import type { UtopiaComponentProps } from 'utopia-api'
import type { FullFrame } from '../../components/frame'
import type { Either } from '../shared/either'
import {
  applicative2Either,
  flatMapEither,
  foldEither,
  isRight,
  joinEither,
  left,
  mapEither,
  reduceWithEither,
  right,
  eitherToMaybe,
  eitherFromMaybe,
} from '../shared/either'
import type { ElementInstanceMetadata, JSXAttributes, JSXElement } from '../shared/element-template'
import { isJSXElement, jsExpressionValue, emptyComments } from '../shared/element-template'
import type { ValueAtPath } from '../shared/jsx-attributes'
import { setJSXValuesAtPaths, unsetJSXValuesAtPaths } from '../shared/jsx-attributes'
import type { PropertyPath, ElementPath } from '../shared/project-file-types'
import { getLayoutProperty } from './getLayoutProperty'
import type { PropsOrJSXAttributes } from '../model/element-metadata-utils'
import { getSimpleAttributeAtPath } from '../model/element-metadata-utils'
import type { EdgePosition } from '../../components/canvas/canvas-types'
import {
  getPropertyControlsForTarget,
  hasStyleControls,
} from '../property-controls/property-controls-utils'
import type { PropertyControlsInfo } from '../../components/custom-code/code-file'
import type { ProjectContentTreeRoot } from '../../components/assets'
import { stylePropPathMappingFn } from '../../components/inspector/common/property-path-hooks'
import { setJSXValueAtPath } from '../shared/jsx-attribute-utils'

export function targetRespectsLayout(
  target: ElementPath,
  propertyControlsInfo: PropertyControlsInfo,
  projectContents: ProjectContentTreeRoot,
): boolean {
  const propControls = getPropertyControlsForTarget(target, propertyControlsInfo, projectContents)

  return propControls != null && hasStyleControls(propControls)
}

export const PinLayoutHelpers = {
  setLayoutPropsToPinsWithFrame(
    props: JSXAttributes,
    frame: Partial<FullFrame>,
    propertyTarget: Array<string>,
  ): Either<string, JSXAttributes> {
    return reduceWithEither(
      (workingProps, frameProp: keyof FullFrame) => {
        if (frameProp === 'centerX' || frameProp === 'centerY') {
          return right(workingProps)
        } else {
          return setJSXValueAtPath(
            workingProps,
            stylePropPathMappingFn(frameProp, propertyTarget),
            jsExpressionValue(frame[frameProp], emptyComments),
          )
        }
      },
      props,
      Object.keys(frame) as Array<keyof FullFrame>,
    )
  },
}

export type FixedAndBases = {
  flexBasis: number | null
  width: number | null
  height: number | null
}

export type TopLeftWidthHeight = Pick<FullFrame, 'top' | 'left' | 'width' | 'height'>

export const FlexLayoutHelpers = {
  updateLayoutPropsToFlexWithFrame(
    parentProps: PropsOrJSXAttributes,
    elementProps: JSXAttributes,
    width: number,
    height: number,
    propertyTarget: ReadonlyArray<string>,
  ): Either<string, JSXAttributes> {
    const flexBasisPath = stylePropPathMappingFn('flexBasis', propertyTarget)
    // Remove all of the properties so that old values don't mess with this.
    const withoutLayoutProps = unsetJSXValuesAtPaths(elementProps, [flexBasisPath])

    return joinEither(
      applicative2Either(
        (props, { flexBasis, width: widthToSet, height: heightToSet }) => {
          // Build the new properties.
          let propsToSet: Array<ValueAtPath> = []
          function addPropToSet(path: PropertyPath, value: string | number | undefined): void {
            if (value != null) {
              propsToSet.push({ path: path, value: jsExpressionValue(value, emptyComments) })
            }
          }
          if (flexBasis != null) {
            addPropToSet(flexBasisPath, flexBasis)
          }
          if (widthToSet != null) {
            addPropToSet(stylePropPathMappingFn('width', propertyTarget), widthToSet)
          }
          if (heightToSet != null) {
            addPropToSet(stylePropPathMappingFn('height', propertyTarget), heightToSet)
          }

          // Assign the new properties
          return setJSXValuesAtPaths(props, propsToSet)
        },
        withoutLayoutProps,
        FlexLayoutHelpers.convertWidthHeightToFlex(
          width,
          height,
          elementProps,
          parentProps,
          eitherToMaybe(FlexLayoutHelpers.getMainAxis(propertyTarget, parentProps)),
          null,
          propertyTarget,
        ),
      ),
    )
  },
  getMainAxis(
    propertyTarget: ReadonlyArray<string>,
    props: PropsOrJSXAttributes,
  ): Either<string, 'horizontal' | 'vertical'> {
    return mapEither((fd) => {
      if (fd === 'column' || fd === 'column-reverse') {
        return 'vertical'
      } else {
        return 'horizontal'
      }
    }, getSimpleAttributeAtPath(props, stylePropPathMappingFn('flexDirection', propertyTarget)))
  },
  getCrossAxis(
    propertyTarget: Array<string>,
    props: PropsOrJSXAttributes,
  ): Either<string, 'horizontal' | 'vertical'> {
    return mapEither(
      (fd) => (fd === 'vertical' ? 'horizontal' : 'vertical'),
      this.getMainAxis(propertyTarget, props),
    )
  },
  convertWidthHeightToFlex: (
    width: number,
    height: number,
    props: JSXAttributes,
    parentProps: PropsOrJSXAttributes,
    possibleMainAxis: 'horizontal' | 'vertical' | null,
    edgePosition: EdgePosition | null,
    propertyTarget: ReadonlyArray<string>,
  ): Either<string, FixedAndBases> => {
    const flexBasisPath = stylePropPathMappingFn('flexBasis', propertyTarget)
    const currentFlexBasis = eitherToMaybe(getSimpleAttributeAtPath(right(props), flexBasisPath))
    const currentWidth = eitherToMaybe(
      getSimpleAttributeAtPath(right(props), stylePropPathMappingFn('width', propertyTarget)),
    )
    const currentHeight = eitherToMaybe(
      getSimpleAttributeAtPath(right(props), stylePropPathMappingFn('height', propertyTarget)),
    )
    return mapEither((mainAxis) => {
      const flexBasis = mainAxis === 'horizontal' ? width : height
      const crossBasis = mainAxis === 'vertical' ? height : width
      let shouldSetFlexBasis = true
      let shouldSetCrossBasis = true
      if (edgePosition != null) {
        shouldSetFlexBasis =
          mainAxis === 'horizontal' ? edgePosition.y === 0.5 : edgePosition.x === 0.5
        shouldSetCrossBasis =
          mainAxis === 'vertical' ? edgePosition.y === 0.5 : edgePosition.x === 0.5
      }
      return {
        flexBasis: currentFlexBasis != null || shouldSetFlexBasis ? flexBasis : null,
        width:
          mainAxis === 'vertical' && (currentWidth != null || shouldSetCrossBasis) ? width : null,
        height:
          mainAxis === 'horizontal' && (currentHeight != null || shouldSetCrossBasis)
            ? height
            : null,
      }
    }, eitherFromMaybe('no main axis provided', possibleMainAxis))
  },
  getUnstretchedWidthHeight: (
    props: UtopiaComponentProps,
    parentProps: UtopiaComponentProps,
  ): { width: FlexLength; height: FlexLength } => {
    return getUnstretchedWidthHeight((props.style as any) ?? {}, (parentProps.style as any) ?? {})
  },
}

export const LayoutHelpers = {
  updateLayoutPropsWithFrame(
    parentIsFlex: boolean,
    parentProps: PropsOrJSXAttributes | null,
    elementProps: JSXAttributes,
    frame: TopLeftWidthHeight,
    propertyTarget: Array<string>,
  ): Either<string, JSXAttributes> {
    if (parentIsFlex && parentProps != null) {
      return FlexLayoutHelpers.updateLayoutPropsToFlexWithFrame(
        parentProps,
        elementProps,
        frame.width,
        frame.height,
        propertyTarget,
      )
    } else {
      return PinLayoutHelpers.setLayoutPropsToPinsWithFrame(elementProps, frame, propertyTarget)
    }
  },
  stretchesChild(
    parentProps: PropsOrJSXAttributes,
    childProps: JSXAttributes,
    propertyTarget: Array<string>,
  ): Either<string, boolean> {
    return applicative2Either(
      (alignSelf, alignItems) => {
        const childrenStretchesSelf = alignSelf === 'stretch'
        const childrenAlignsSelfAuto = alignSelf === 'auto' || alignSelf == null
        const parentStretchesChildren = alignItems === 'stretch'
        return childrenStretchesSelf || (parentStretchesChildren && childrenAlignsSelfAuto)
      },
      getLayoutProperty('alignSelf', right(childProps), propertyTarget),
      getLayoutProperty('alignItems', parentProps, propertyTarget),
    )
  },
  getFlexStretchForChild(
    parent: ElementInstanceMetadata,
    child: ElementInstanceMetadata,
    propertyTarget: Array<string>,
  ): Either<string, FlexStretch> {
    const parentScenePropsOrElementAttributes: PropsOrJSXAttributes | null = foldEither(
      (_) => null,
      (success) => {
        if (isJSXElement(success)) {
          return right(success.props)
        } else {
          return null
        }
      },
      parent.element,
    )
    if (parentScenePropsOrElementAttributes == null) {
      return left('Unknown parent attributes.')
    } else {
      if (isRight(child.element) && isJSXElement(child.element.value)) {
        const childJSXElement: JSXElement = child.element.value
        return flatMapEither((stretched) => {
          if (stretched) {
            return FlexLayoutHelpers.getCrossAxis(
              propertyTarget,
              parentScenePropsOrElementAttributes,
            )
          } else {
            return right('none')
          }
        }, this.stretchesChild(parentScenePropsOrElementAttributes, childJSXElement.props, propertyTarget))
      } else {
        return left('Unknown child attributes.')
      }
    }
  },
  getElementSizePropertyPaths(
    element: ElementInstanceMetadata,
    propertyTarget: Array<string>,
  ): {
    horizontal: PropertyPath
    vertical: PropertyPath
  } {
    if (element.specialSizeMeasurements.parentLayoutSystem === 'flex') {
      if (element.specialSizeMeasurements.parentFlexDirection === 'row') {
        return {
          horizontal: stylePropPathMappingFn('flexBasis', propertyTarget),
          vertical: stylePropPathMappingFn('height', propertyTarget),
        }
      } else {
        return {
          horizontal: stylePropPathMappingFn('width', propertyTarget),
          vertical: stylePropPathMappingFn('flexBasis', propertyTarget),
        }
      }
    } else {
      return {
        horizontal: stylePropPathMappingFn('width', propertyTarget),
        vertical: stylePropPathMappingFn('height', propertyTarget),
      }
    }
  },
}
