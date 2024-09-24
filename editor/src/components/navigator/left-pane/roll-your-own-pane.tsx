import React from 'react'
import { Button, colorTheme, FlexColumn, FlexRow, Section } from '../../../uuiui'
import { when } from '../../../utils/react-conditionals'
import { atom, useAtom, useSetAtom } from 'jotai'
import { UIGridRow } from '../../inspector/widgets/ui-grid-row'
import { atomWithStorage } from 'jotai/utils'
import { IS_TEST_ENVIRONMENT } from '../../../common/env-vars'
import { Ellipsis } from './github-pane/github-file-changes-list'
import { deleteParseCache } from '../../../core/shared/parse-cache-utils'
import { useRefEditorState } from '../../../components/editor/store/store-hook'
import type { FeatureName } from '../../../utils/feature-switches'
import { isFeatureEnabled, setFeatureEnabled } from '../../../utils/feature-switches'

const sections = ['Grid', 'Performance'] as const
type Section = (typeof sections)[number]

type GridFeatures = {
  dragVerbatim: boolean
  dragMagnetic: boolean
  dragRatio: boolean
  dotgrid: boolean
  shadow: boolean
  activeGridColor: string
  dotgridColor: string
  inactiveGridColor: string
  shadowOpacity: number
  activeGridBackground: string
}

type PerformanceFeatures = {
  parseCache: boolean
  verboseLogCache: boolean
}

type RollYourOwnFeaturesTypes = {
  Grid: GridFeatures
  Performance: PerformanceFeatures
}

type RollYourOwnFeatures = {
  [K in Section]: RollYourOwnFeaturesTypes[K]
}

const featureToFeatureFlagMap: Record<keyof Partial<PerformanceFeatures>, FeatureName> = {
  parseCache: 'Use Parsing Cache',
  verboseLogCache: 'Verbose Log Cache',
}

const defaultRollYourOwnFeatures: () => RollYourOwnFeatures = () => ({
  Grid: {
    dragVerbatim: false,
    dragMagnetic: false,
    dragRatio: true,
    dotgrid: true,
    shadow: true,
    activeGridColor: '#0099ff77',
    activeGridBackground: colorTheme.primary10.value,
    dotgridColor: '#0099ffaa',
    inactiveGridColor: '#00000033',
    shadowOpacity: 0.1,
  },
  Performance: {
    parseCache: getFeatureFlagValue('parseCache', true),
    verboseLogCache: getFeatureFlagValue('verboseLogCache', true),
  },
})

const ROLL_YOUR_OWN_FEATURES_KEY: string = 'roll-your-own-features'

let rollYourOwnFeaturesAtom:
  | ReturnType<typeof atom<RollYourOwnFeatures>>
  | ReturnType<typeof atomWithStorage<RollYourOwnFeatures>>
  | null = null

function getRollYourOwnFeaturesAtom() {
  if (rollYourOwnFeaturesAtom == null) {
    rollYourOwnFeaturesAtom = IS_TEST_ENVIRONMENT
      ? atom(defaultRollYourOwnFeatures())
      : atomWithStorage(ROLL_YOUR_OWN_FEATURES_KEY, defaultRollYourOwnFeatures())
  }
  return rollYourOwnFeaturesAtom
}

export function useRollYourOwnFeatures() {
  const [features] = useAtom(getRollYourOwnFeaturesAtom())
  const defaultFeatures = defaultRollYourOwnFeatures()
  const merged: RollYourOwnFeatures = {
    Grid: {
      ...defaultFeatures.Grid,
      ...features.Grid,
    },
    Performance: {
      ...defaultFeatures.Performance,
      ...syncWithFeatureFlags(features.Performance),
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
        {when(currentSection === 'Performance', <PerformanceSection />)}
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

/** GRID SECTION */
const GridSection = React.memo(() => {
  return (
    <FlexColumn style={{ gap: 10 }}>
      <ResetDefaultsButton subsection='Grid' />
      <SimpleFeatureControls subsection='Grid' />
    </FlexColumn>
  )
})
GridSection.displayName = 'GridSection'

/** PERFORMANCE SECTION */
const PerformanceSection = React.memo(() => {
  const workersRef = useRefEditorState((store) => {
    return store.workers
  })
  const handleDeleteParseCache = React.useCallback(() => {
    deleteParseCache(workersRef.current)
  }, [workersRef])
  return (
    <FlexColumn style={{ gap: 10 }}>
      <SimpleFeatureControls subsection='Performance' />
      <UIGridRow padded variant='<-------------1fr------------->'>
        <Button highlight spotlight onClick={handleDeleteParseCache}>
          Clear cache
        </Button>
      </UIGridRow>
    </FlexColumn>
  )
})
PerformanceSection.displayName = 'PerformanceSection'

/** GENERAL COMPONENTS */

const ResetDefaultsButton = React.memo(({ subsection }: { subsection: Section }) => {
  const setFeatures = useSetAtom(getRollYourOwnFeaturesAtom())
  const defaultFeatures = defaultRollYourOwnFeatures()
  const resetDefaults = React.useCallback(() => {
    setFeatures((prevFeatures) => ({
      ...prevFeatures,
      [subsection]: defaultFeatures[subsection],
    }))
  }, [defaultFeatures, setFeatures, subsection])
  return (
    <UIGridRow padded variant='<-------------1fr------------->'>
      <Button highlight spotlight onClick={resetDefaults}>
        Reset defaults
      </Button>
    </UIGridRow>
  )
})
ResetDefaultsButton.displayName = 'ResetDefaultsButton'

const SimpleFeatureControls = React.memo(({ subsection }: { subsection: Section }) => {
  const features = useRollYourOwnFeatures()
  const setFeatures = useSetAtom(getRollYourOwnFeaturesAtom())
  const onChange = React.useCallback(
    (feat: keyof RollYourOwnFeaturesTypes[Section]) => (e: React.ChangeEvent<HTMLInputElement>) => {
      const newValue = getNewFeatureValueOrNull(features[subsection][feat], e)
      if (newValue != null) {
        setFeatures({
          ...features,
          [subsection]: {
            ...features[subsection],
            [feat]: newValue,
          },
        })
        if (typeof newValue === 'boolean') {
          syncFeatureFlagIfExists(feat, newValue)
        }
      }
    },
    [features, setFeatures, subsection],
  )
  const defaultFeatures = defaultRollYourOwnFeatures()
  return (
    <React.Fragment>
      {Object.keys(defaultFeatures[subsection]).map((key) => {
        const feat = key as keyof RollYourOwnFeaturesTypes[Section]
        const value = features[subsection][feat] ?? defaultFeatures[subsection][feat]
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
    </React.Fragment>
  )
})
SimpleFeatureControls.displayName = 'SimpleFeatureControls'

function getFeatureFlagValue(
  featureName: keyof typeof featureToFeatureFlagMap,
  defaultValue: boolean,
): boolean {
  const featureFlag = featureToFeatureFlagMap[featureName]
  if (featureFlag == null) {
    return defaultValue
  }
  return isFeatureEnabled(featureFlag)
}

function syncFeatureFlagIfExists(
  featureName: keyof typeof featureToFeatureFlagMap,
  value: boolean,
) {
  const featureFlag = featureToFeatureFlagMap[featureName]
  if (featureFlag == null) {
    return
  }
  setFeatureEnabled(featureFlag, value)
}

function syncWithFeatureFlags(features: Record<string, any>) {
  return Object.fromEntries(
    Object.entries(features).map(([key, value]) => {
      if (typeof value === 'boolean' && key in featureToFeatureFlagMap) {
        return [key, getFeatureFlagValue(key as keyof typeof featureToFeatureFlagMap, value)]
      }
      return [key, value]
    }),
  )
}
