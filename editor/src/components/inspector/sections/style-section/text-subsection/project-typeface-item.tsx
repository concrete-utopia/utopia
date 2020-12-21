import * as React from 'react'
import {
  ExternalResources,
  useExternalResources,
  GoogleFontsResource,
} from '../../../../../printer-parsers/html/external-resources-parser'
import { FlexRow, Icons } from '../../../../../uuiui'
import { betterReactMemo } from '../../../../../uuiui-deps'
import { ProjectTypeface } from './font-family-select-popup'

export function updateRemoveFontFamily(
  fontFamilyToDelete: string,
  oldValue: ExternalResources,
): ExternalResources {
  const googleFontsResources: Array<GoogleFontsResource> = (() => {
    let workingGoogleFontsResources = [...oldValue.googleFontsResources]
    const familyIndex = workingGoogleFontsResources.findIndex(
      (resource) => resource.fontFamily === fontFamilyToDelete,
    )
    if (familyIndex > -1) {
      workingGoogleFontsResources.splice(familyIndex, 1)
    }
    return workingGoogleFontsResources
  })()
  return {
    ...oldValue,
    googleFontsResources,
  }
}

interface ProjectTypefaceItemProps {
  typeface: ProjectTypeface['typeface']
  selected: boolean
  updateSizes: () => void
}

export const ProjectTypefaceItem = betterReactMemo<ProjectTypefaceItemProps>(
  'ProjectTypefaceItem',
  ({ typeface, selected, updateSizes }) => {
    const [hovered, setHovered] = React.useState(false)
    const onMouseOver = React.useCallback(() => {
      setHovered(true)
    }, [])
    const onMouseLeave = React.useCallback(() => {
      setHovered(false)
    }, [])
    const { useSubmitValueFactory } = useExternalResources()
    const [removeFontFamily] = useSubmitValueFactory(updateRemoveFontFamily)
    const onCrossClick = React.useCallback(
      (e: React.MouseEvent) => {
        e.stopPropagation()
        removeFontFamily(typeface.name)
        updateSizes()
      },
      [removeFontFamily, typeface.name, updateSizes],
    )
    return (
      <>
        <FlexRow style={{ alignItems: 'start' }}>
          <div style={{ flexGrow: 1 }}>{typeface.name}</div>
          <div onMouseOver={onMouseOver} onMouseLeave={onMouseLeave} style={{ cursor: 'pointer' }}>
            {hovered ? (
              <Icons.Cross
                onClick={onCrossClick}
                color='white'
                tooltipText={`Remove ${typeface.name} from the project`}
              />
            ) : (
              <Icons.Checkmark color={selected ? 'white' : undefined} />
            )}
          </div>
        </FlexRow>
      </>
    )
  },
)
