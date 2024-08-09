import React from 'react'
import { Button, colorTheme, FlexColumn, FlexRow, Section } from '../../../uuiui'
import { when } from '../../../utils/react-conditionals'
import { atom, useAtom, useSetAtom } from 'jotai'
import { UIGridRow } from '../../inspector/widgets/ui-grid-row'
import { atomWithStorage } from 'jotai/utils'
import { IS_TEST_ENVIRONMENT } from '../../../common/env-vars'
import { Ellipsis } from './github-pane/github-file-changes-list'

const sections = ['Grid'] as const
type Section = (typeof sections)[number]

type GridFeatures = {
  dragLockedToCenter: boolean
  dragVerbatim: boolean
  dragMagnetic: boolean
  dragRatio: boolean
  animateShadowSnap: boolean
  dotgrid: boolean
  shadow: boolean
  adaptiveOpacity: boolean
  activeGridColor: string
  dotgridColor: string
  inactiveGridColor: string
  opacityBaseline: number
  activeGridBackground: string
}

type RollYourOwnFeaturesTypes = {
  Grid: GridFeatures
}

type RollYourOwnFeatures = {
  [K in Section]: RollYourOwnFeaturesTypes[K]
}

const defaultRollYourOwnFeatures: RollYourOwnFeatures = {
  Grid: {
    dragLockedToCenter: false,
    dragVerbatim: false,
    dragMagnetic: false,
    dragRatio: true,
    animateShadowSnap: false,
    dotgrid: true,
    shadow: true,
    adaptiveOpacity: true,
    activeGridColor: '#0099ff77',
    activeGridBackground: colorTheme.primary10.value,
    dotgridColor: '#0099ffaa',
    inactiveGridColor: '#00000033',
    opacityBaseline: 0.25,
  },
}

const ROLL_YOUR_OWN_FEATURES_KEY: string = 'roll-your-own-features'

const rollYourOwnFeaturesAtom = IS_TEST_ENVIRONMENT
  ? atom(defaultRollYourOwnFeatures)
  : atomWithStorage(ROLL_YOUR_OWN_FEATURES_KEY, defaultRollYourOwnFeatures)

export function useRollYourOwnFeatures() {
  const [features] = useAtom(rollYourOwnFeaturesAtom)
  const merged: RollYourOwnFeatures = {
    Grid: {
      ...defaultRollYourOwnFeatures.Grid,
      ...features.Grid,
    },
  }
  return merged
}

export const RollYourOwnFeaturesPane = React.memo(() => {
  const [currentSection, setCurrentSection] = React.useState<Section | null>(null)

  const onChangeSection = React.useCallback((e: React.ChangeEvent<HTMLSelectElement>) => {
    const maybeSectionValue = e.target.value as Section
    if (sections.includes(maybeSectionValue)) {
      setCurrentSection(maybeSectionValue)
    } else {
      setCurrentSection(null)
    }
  }, [])

  return (
    <FlexColumn
      id='leftPaneRollYourOwn'
      key='leftPaneRollYourOwn'
      style={{
        display: 'relative',
        alignItems: 'stretch',
        paddingBottom: 50,
        overflowY: 'scroll',
        alignSelf: 'stretch',
      }}
    >
      <Section>
        <FlexRow
          style={{
            margin: 8,
            gap: 12,
            height: 22,
          }}
        >
          <span style={{ fontWeight: 600 }}>Roll Your Own</span>
        </FlexRow>
        <FlexRow style={{ gap: 12, margin: 8 }}>
          <select
            value={currentSection ?? undefined}
            style={{ flex: 1 }}
            onChange={onChangeSection}
          >
            <option value=''>–</option>
            {sections.map((section) => {
              return (
                <option key={`section-${section}`} value={section}>
                  {section}
                </option>
              )
            })}
          </select>
        </FlexRow>

        {when(currentSection === 'Grid', <GridSection />)}
      </Section>
    </FlexColumn>
  )
})
RollYourOwnFeaturesPane.displayName = 'RollYourOwnFeaturesPane'

function getNewFeatureValueOrNull(currentValue: any, e: React.ChangeEvent<HTMLInputElement>) {
  switch (typeof currentValue) {
    case 'boolean':
      return e.target.checked
    case 'string':
      return e.target.value
    case 'number':
      return parseFloat(e.target.value)
    default:
      return null
  }
}

const GridSection = React.memo(() => {
  const features = useRollYourOwnFeatures()
  const setFeatures = useSetAtom(rollYourOwnFeaturesAtom)

  const onChange = React.useCallback(
    (feat: keyof GridFeatures) => (e: React.ChangeEvent<HTMLInputElement>) => {
      const newValue = getNewFeatureValueOrNull(features.Grid[feat], e)
      if (newValue != null) {
        setFeatures({
          ...features,
          Grid: {
            ...features.Grid,
            [feat]: newValue,
          },
        })
      }
    },
    [features, setFeatures],
  )

  const resetDefaults = React.useCallback(() => {
    setFeatures(defaultRollYourOwnFeatures)
  }, [setFeatures])

  return (
    <FlexColumn style={{ gap: 10 }}>
      <UIGridRow padded variant='<-------------1fr------------->'>
        <Button highlight spotlight onClick={resetDefaults}>
          Reset defaults
        </Button>
      </UIGridRow>
      {Object.keys(defaultRollYourOwnFeatures.Grid).map((key) => {
        const feat = key as keyof GridFeatures
        const value = features.Grid[feat] ?? defaultRollYourOwnFeatures.Grid[feat]
        return (
          <UIGridRow padded variant='<----------1fr---------><-auto->' key={`feat-${feat}`}>
            <Ellipsis title={feat}>{feat}</Ellipsis>
            {typeof value === 'boolean' ? (
              <input type='checkbox' checked={value} onChange={onChange(feat)} />
            ) : typeof value === 'string' ? (
              <input type='text' value={value} onChange={onChange(feat)} />
            ) : typeof value === 'number' ? (
              <input type='number' value={value} onChange={onChange(feat)} />
            ) : null}
          </UIGridRow>
        )
      })}
    </FlexColumn>
  )
})
GridSection.displayName = 'GridSection'
