import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import type { ElementInstanceMetadata } from '../../../../core/shared/element-template'
import {
  isJSXElement,
  type ElementInstanceMetadataMap,
} from '../../../../core/shared/element-template'
import type { CanvasRectangle } from '../../../../core/shared/math-utils'
import {
  boundingRectangleArray,
  canvasRectangle,
  isInfinityRectangle,
  nullIfInfinity,
} from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { assertNever } from '../../../../core/shared/utils'
import { when } from '../../../../utils/react-conditionals'
import { useColorTheme } from '../../../../uuiui'
import { getMetadata } from '../../../editor/store/editor-state'
import { Substores, useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import type { DetectedFillHugFixedState, FixedHugFill } from '../../../inspector/inspector-common'
import { detectFillHugFixedState } from '../../../inspector/inspector-common'
import { getAllTargetsUnderAreaAABB } from '../../dom-lookup'
import { treatElementAsGroupLike } from '../../canvas-strategies/strategies/group-helpers'
import { defaultEither } from '../../../../core/shared/either'
import type { CSSNumber } from '../../../inspector/common/css-utils'
import { isCSSNumber, printCSSNumber } from '../../../inspector/common/css-utils'
import { toFirst } from '../../../../core/shared/optics/optic-utilities'
import {
  eitherRight,
  fromField,
  fromTypeGuard,
  notNull,
} from '../../../../core/shared/optics/optic-creators'
import { useBoundingBox } from '../bounding-box-hooks'
import { controlForStrategy } from '../../canvas-strategies/canvas-strategy-types'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import { isZeroSizedElement } from '../outline-utils'

export const SizeLabelID = 'size-label'
export const SizeLabelTestId = 'size-label'

const sizeLabel = (state: FixedHugFill['type'] | CSSNumber, actualSize: number): string => {
  if (isCSSNumber(state)) {
    const printed = printCSSNumber(state, 'px')
    return typeof printed === 'number' ? `${printed}` : printed
  } else {
    switch (state) {
      case 'fill':
        return 'Fill'
      case 'hug':
      case 'hug-group':
        return 'Hug'
      case 'squeeze':
        return 'Squeeze'
      case 'collapsed':
        return 'Collapsed'
      case 'fixed':
      case 'scaled':
      case 'detected':
      case 'computed':
        return `${actualSize}`
      case 'stretch':
        return 'Stretch'
      default:
        assertNever(state)
    }
  }
}

export type SizeLabelSize = {
  type: 'SIZE_LABEL_WITH_DIMENSIONS'
  h: string
  v: string
}

function sizeLabelWithDimensions(h: string, v: string): SizeLabelSize {
  return {
    type: 'SIZE_LABEL_WITH_DIMENSIONS',
    h: h,
    v: v,
  }
}

export type SizeLabelGroup = {
  type: 'SIZE_LABEL_GROUP'
}

function sizeLabelGroup(): SizeLabelGroup {
  return {
    type: 'SIZE_LABEL_GROUP',
  }
}

export type SizeLabelChildren = {
  type: 'SIZE_LABEL_CHILDREN'
  h: string
  v: string
}

function sizeLabelChildren(h: string, v: string): SizeLabelChildren {
  return {
    type: 'SIZE_LABEL_CHILDREN',
    h: h,
    v: v,
  }
}

export type SizeLabelContents = SizeLabelSize | SizeLabelGroup | SizeLabelChildren

function detectedStateToOutputValue(
  detectedState: DetectedFillHugFixedState | null,
): FixedHugFill['type'] | CSSNumber | null {
  if (detectedState == null || detectedState.fixedHugFill == null) {
    return null
  }
  switch (detectedState.fixedHugFill.type) {
    case 'hug':
    case 'squeeze':
    case 'collapsed':
    case 'stretch':
      return detectedState.fixedHugFill.type
    case 'hug-group':
      return 'hug'
    case 'fixed':
    case 'fill':
    case 'computed':
    case 'detected':
    case 'scaled':
      return detectedState.fixedHugFill.value
    default:
      assertNever(detectedState.fixedHugFill)
  }
}

function sizeLabelContents(
  metadata: ElementInstanceMetadataMap,
  selectedElements: Array<ElementPath>,
  boundingBox: CanvasRectangle | null,
  pathsWereReplaced: boolean,
): SizeLabelContents | null {
  const selectedElement = selectedElements.at(0)
  if (selectedElement == null || selectedElements.length === 0) {
    return null
  }

  if (selectedElements.length === 1) {
    if (treatElementAsGroupLike(metadata, selectedElement)) {
      return sizeLabelGroup()
    }

    const elementMetadata = MetadataUtils.findElementByElementPath(metadata, selectedElement)
    if (elementMetadata == null) {
      return null
    }
    const element = defaultEither(
      null,
      toFirst(
        notNull<ElementInstanceMetadata>()
          .compose(fromField('element'))
          .compose(eitherRight())
          .compose(fromTypeGuard(isJSXElement)),
        elementMetadata,
      ),
    )
    if (element == null) {
      return null
    }

    const globalFrame = elementMetadata.globalFrame
    if (globalFrame == null || isInfinityRectangle(globalFrame)) {
      return null
    }

    const horizontalState = detectFillHugFixedState('horizontal', metadata, selectedElement)
    const verticalState = detectFillHugFixedState('vertical', metadata, selectedElement)

    const horizontal = detectedStateToOutputValue(horizontalState) ?? 'fixed'
    const vertical = detectedStateToOutputValue(verticalState) ?? 'fixed'

    if (pathsWereReplaced) {
      return sizeLabelChildren(
        sizeLabel(horizontal, globalFrame.width),
        sizeLabel(vertical, globalFrame.height),
      )
    }

    return sizeLabelWithDimensions(
      sizeLabel(horizontal, globalFrame.width),
      sizeLabel(vertical, globalFrame.height),
    )
  }

  if (boundingBox != null) {
    return sizeLabelWithDimensions(`${boundingBox.width}`, `${boundingBox.height}`)
  }

  return null
}

interface SizeLabelProps {
  targets: Array<ElementPath>
  pathsWereReplaced: boolean
}

const FontSize = 11
const PaddingV = 0
const PaddingH = 2
const ExplicitHeightHacked = 20
const BorderRadius = 2
const SizeLabelMarginTop = 8

function getLabelText(label: SizeLabelContents | null): string | null {
  if (label == null) {
    return null
  }
  switch (label.type) {
    case 'SIZE_LABEL_GROUP':
      return 'Group'
    case 'SIZE_LABEL_WITH_DIMENSIONS':
      return `${label.h} × ${label.v}`
    case 'SIZE_LABEL_CHILDREN':
      return `(Children) ${label.h} × ${label.v}`
    default:
      assertNever(label)
  }
}

export const SizeLabel = React.memo<SizeLabelProps>(({ targets, pathsWereReplaced }) => {
  const resizeRef = useBoundingBox(targets, (ref, boundingBox) => {
    ref.current.style.top = boundingBox.height + 'px'
    ref.current.style.left = 0 + 'px'
    ref.current.style.width = boundingBox.width + 'px'
  })

  const controlRef = useBoundingBox(targets, (ref, safeGappedBoundingBox, realBoundingBox) => {
    if (isZeroSizedElement(realBoundingBox)) {
      ref.current.style.display = 'none'
    } else {
      ref.current.style.display = 'block'
      ref.current.style.left = safeGappedBoundingBox.x + 'px'
      ref.current.style.top = safeGappedBoundingBox.y + 'px'
      ref.current.style.width = safeGappedBoundingBox.width + 'px'
      ref.current.style.height = safeGappedBoundingBox.height + 'px'
    }
  })

  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'Resizelabel scale',
  )
  const colorTheme = useColorTheme()
  const metadata = useEditorState(
    Substores.metadata,
    (store) => getMetadata(store.editor),
    'ResizeLabel metadata',
  )

  const boundingBox = boundingRectangleArray(
    targets.map((t) => nullIfInfinity(MetadataUtils.getFrameInCanvasCoords(t, metadata))),
  )

  const label = sizeLabelContents(metadata, targets, boundingBox, pathsWereReplaced)
  const labelText = getLabelText(label)

  const [dimmed, setDimmed] = React.useState(false)

  const editorRef = useRefEditorState((store) => ({
    scale: store.editor.canvas.scale,
    offset: store.editor.canvas.roundedCanvasOffset,
    jsxMetadata: store.editor.jsxMetadata,
    elementPathTree: store.editor.elementPathTree,
    allElementProps: store.editor.allElementProps,
    hiddenInstances: store.editor.hiddenInstances,
  }))

  const onMouseEnter = React.useCallback(() => {
    const distanceBetweenBoxAndLabel = 10 // px
    const labelRect = document.getElementById(SizeLabelID)?.getBoundingClientRect()
    if (boundingBox != null && labelRect != null) {
      const area = canvasRectangle({
        x: boundingBox.x + (boundingBox.width - labelRect.width) / 2,
        y:
          boundingBox.y + boundingBox.height + distanceBetweenBoxAndLabel / editorRef.current.scale,
        width: labelRect.width,
        height: labelRect.height,
      })
      const elementsUnderLabel = getAllTargetsUnderAreaAABB(
        editorRef.current.jsxMetadata,
        [],
        editorRef.current.hiddenInstances,
        'no-filter',
        area,
        editorRef.current.elementPathTree,
        editorRef.current.allElementProps,
        true,
      )
      setDimmed(elementsUnderLabel.length > 0)
    }
  }, [editorRef, boundingBox])

  const onMouseLeave = React.useCallback(() => {
    setDimmed(false)
  }, [])

  return (
    <CanvasOffsetWrapper>
      <div
        data-testid={`${SizeLabelTestId}-wrapper`}
        ref={controlRef}
        style={{
          position: 'absolute',
          pointerEvents: 'none',
        }}
      >
        <div
          ref={resizeRef}
          style={{
            position: 'absolute',
            display: 'flex',
            justifyContent: 'center',
            pointerEvents: 'none',
          }}
          data-testid='parent-resize-label'
        >
          {when(
            labelText != null,
            <div
              id={SizeLabelID}
              data-testid={SizeLabelTestId}
              style={{
                display: 'flex',
                alignItems: 'center',
                marginTop: SizeLabelMarginTop / scale,
                padding: `${PaddingV}px ${PaddingH / scale}px`,
                borderRadius: BorderRadius / scale,
                color: colorTheme.white.value,
                backgroundColor: colorTheme.primary.value,
                fontSize: FontSize / scale,
                height: ExplicitHeightHacked / scale,
                opacity: dimmed ? 0.075 : 1,
                transition: '0.1s',
                pointerEvents: 'initial',
              }}
              onMouseEnter={onMouseEnter}
              onMouseLeave={onMouseLeave}
            >
              {labelText}
            </div>,
          )}
        </div>
      </div>
    </CanvasOffsetWrapper>
  )
})

export const StrategySizeLabel = controlForStrategy<SizeLabelProps>(SizeLabel)
