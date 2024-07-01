import React from 'react'
import { FlexColumn, FlexRow, Section } from '../../../uuiui'
import { when } from '../../../utils/react-conditionals'
import { useAtom } from 'jotai'
import { UIGridRow } from '../../inspector/widgets/ui-grid-row'
import { atomWithStorage } from 'jotai/utils'

const sections = ['Grid'] as const
type Section = (typeof sections)[number]

type GridFeatures = {
  foo: boolean
}

type RollYourOwnFeaturesTypes = {
  Grid: GridFeatures
}

type RollYourOwnFeatures = {
  [K in Section]: RollYourOwnFeaturesTypes[K]
}

let defaultRollYourOwnFeatures: RollYourOwnFeatures = {
  Grid: {
    foo: true,
  },
}

const ROLL_YOUR_OWN_FEATURES_KEY: string = 'roll-your-own-features'

export const rollYourOwnFeatures = atomWithStorage(
  ROLL_YOUR_OWN_FEATURES_KEY,
  defaultRollYourOwnFeatures,
)

export const RollYourOwnFeaturesPane = React.memo(() => {
  const [currentSection, setCurrentSection] = React.useState<Section | null>(null)
  const [features, setFeatures] = useAtom(rollYourOwnFeatures)

  const onChange = React.useCallback(
    (base: RollYourOwnFeatures) => async (update: Partial<RollYourOwnFeatures>) => {
      setFeatures({ ...base, ...update })
    },
    [setFeatures],
  )

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

        {when(
          currentSection === 'Grid',
          <GridSection features={features.Grid} onChange={onChange(features)} />,
        )}
      </Section>
    </FlexColumn>
  )
})
RollYourOwnFeaturesPane.displayName = 'RollYourOwnFeaturesPane'

const GridSection = React.memo(
  (props: { features: GridFeatures; onChange: (update: Partial<RollYourOwnFeatures>) => void }) => {
    const onChange = React.useCallback(
      (feat: keyof GridFeatures) => (e: React.ChangeEvent<HTMLInputElement>) => {
        return props.onChange({ Grid: { ...props.features, [feat]: e.target.checked } })
      },
      [props],
    )

    return (
      <FlexColumn style={{ gap: 10 }}>
        {Object.entries(props.features).map(([feat, value]) => {
          return (
            <UIGridRow padded variant='<--1fr--><--1fr-->' key={`feat-${feat}`}>
              <div>{feat}</div>
              {when(
                typeof value === 'boolean',
                <input
                  type='checkbox'
                  checked={value}
                  onChange={onChange(feat as keyof GridFeatures)}
                />,
              )}
            </UIGridRow>
          )
        })}
      </FlexColumn>
    )
  },
)
GridSection.displayName = 'GridSection'
