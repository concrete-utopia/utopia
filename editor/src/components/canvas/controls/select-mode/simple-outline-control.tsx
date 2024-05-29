import React from 'react'
import * as EP from '../../../../core/shared/element-path'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { useColorTheme } from '../../../../uuiui'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { useBoundingBox } from '../bounding-box-hooks'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import { isZeroSizedElement } from '../outline-utils'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapDropNulls, uniqBy } from '../../../../core/shared/array-utils'
import { getElementFromProjectContents } from '../../../editor/store/editor-state'

export const MultiSelectOutlineTestId = 'multiselect-outline'

export function getMultiSelectElementOutlineTestId(p: ElementPath) {
  return `multiselect-element-outline-${EP.toString(p)}`
}

interface MultiSelectOutlineControlProps {
  localSelectedElements: Array<ElementPath>
}

export const MultiSelectOutlineControl = React.memo<MultiSelectOutlineControlProps>(
  (props: MultiSelectOutlineControlProps) => {
    const hiddenInstances = useEditorState(
      Substores.restOfEditor,
      (store) => store.editor.hiddenInstances,
      'MultiSelectOutlineControl hiddenInstances',
    )
    const localSelectedElements = props.localSelectedElements.filter(
      (sv) =>
        hiddenInstances.find((hiddenInstance) => EP.pathsEqual(sv, hiddenInstance)) == null &&
        !EP.isStoryboardPath(sv),
    )

    const showMultiselectOutline = !EP.multiplePathsAllWithTheSameUID(localSelectedElements)

    return (
      <CanvasOffsetWrapper>
        {[
          ...(showMultiselectOutline
            ? [
                <OutlineControl
                  testId={MultiSelectOutlineTestId}
                  key='multiselect-outline'
                  targets={localSelectedElements}
                  color='multiselect-bounds'
                  outlineStyle='solid'
                />,
              ]
            : []),
          ...localSelectedElements.map((path) => {
            const outlineId = getMultiSelectElementOutlineTestId(path)
            return (
              <OutlineControl
                testId={outlineId}
                key={outlineId}
                targets={[path]}
                color='primary'
                outlineStyle='solid'
              />
            )
          }),
        ]}
      </CanvasOffsetWrapper>
    )
  },
)

interface OutlineControlProps {
  testId: string
  targets: ReadonlyArray<ElementPath>
  color: 'primary' | 'multiselect-bounds'
  outlineStyle: 'solid' | 'dotted'
}

export const OutlineControl = React.memo<OutlineControlProps>((props) => {
  const colorTheme = useColorTheme()
  const targets = props.targets
  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'OutlineControl scale',
  )

  const outlineRef = useBoundingBox(
    targets,
    (ref, safeGappedBoundingBox, realBoundingBox, canvasScale) => {
      if (isZeroSizedElement(realBoundingBox)) {
        ref.current.style.display = 'none'
      } else {
        ref.current.style.display = 'block'
        ref.current.style.left = `${safeGappedBoundingBox.x - 0.5 / canvasScale}px`
        ref.current.style.top = `${safeGappedBoundingBox.y - 0.5 / canvasScale}px`
        ref.current.style.width = `${safeGappedBoundingBox.width + 1 / canvasScale}px`
        ref.current.style.height = `${safeGappedBoundingBox.height + 1 / canvasScale}px`
      }
    },
  )

  if (targets.length > 0) {
    return (
      <div
        data-testid={props.testId}
        ref={outlineRef}
        className='role-outline'
        style={{
          position: 'absolute',
          borderColor: colorTheme.primary.value,
          borderWidth: `${1 / scale}px`,
          borderStyle: props.outlineStyle,
          pointerEvents: 'none',
        }}
      />
    )
  }
  return null
})

export const DataReferenceParentOutline = React.memo((props: { targetViews: ElementPath[] }) => {
  const dataReferenceParentPaths = useEditorState(
    Substores.projectContentsAndMetadata,
    (store) => {
      return uniqBy(
        mapDropNulls((path) => {
          const element = getElementFromProjectContents(path, store.editor.projectContents)
          const isDataReference = element != null && MetadataUtils.isElementDataReference(element)
          return isDataReference ? EP.parentPath(path) : null
        }, props.targetViews),
        EP.pathsEqual,
      )
    },
    'DataReferenceParentOutline dataReferenceParentPaths',
  )

  return (
    <CanvasOffsetWrapper>
      {dataReferenceParentPaths.map((path) => (
        <OutlineControl
          testId='data-reference-parents-outline'
          key={`data-reference-parents-outline-${EP.toString(path)}`}
          targets={[path]}
          color='primary'
          outlineStyle='dotted'
        />
      ))}
    </CanvasOffsetWrapper>
  )
})
DataReferenceParentOutline.displayName = 'DataReferenceParentOutline'
