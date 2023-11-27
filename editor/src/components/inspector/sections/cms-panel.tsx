import React from 'react'
import { FlexRow, H2, FlexColumn, StringInput, Button } from '../../../uuiui'
import { JURASSIC_CMS_URL } from '../../../common/env-vars'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { PropertyLabel } from '../widgets/property-label'
import { NO_OP } from '../../../core/shared/utils'
import { updateJurassicCMS } from '../../editor/jurassic-cms'
import invariant from '../../../third-party/remix/invariant'

type ConfigData = Array<{ key: string; value: string }>

export const JurassicCMSPanel = React.memo(() => {
  const projectId = useEditorState(
    Substores.fullStore,
    (store) => store.editor.id,
    'JurassicCMSPanel projectId',
  )

  invariant(projectId, 'Project id cannot be null')

  const [configData, setConfigData] = React.useState<ConfigData | null>(null)
  const [optimisticUpdateCache, setOptimisticUpdateCache] = React.useState<Record<string, string>>(
    {},
  )

  const mergedData: ConfigData | null = React.useMemo(() => {
    if (configData == null) {
      return null
    }
    const keysToValues: Record<string, string> = {}
    const keysInOrder: string[] = []
    for (const { key, value } of configData) {
      keysToValues[key] = value
      keysInOrder.push(key)
    }

    for (const [key, value] of Object.entries(optimisticUpdateCache)) {
      keysToValues[key] = value
    }

    return keysInOrder.map((key) => ({ key: key, value: keysToValues[key] }))
  }, [configData, optimisticUpdateCache])

  const fetchConfig = React.useCallback(async () => {
    const rawResults = await fetch(`${JURASSIC_CMS_URL}/api/${projectId}/keys`).then((resp) =>
      resp.json(),
    )
    const cleanResults: ConfigData = mapDropNulls((result: unknown) => {
      if (
        typeof result !== 'object' ||
        result === null ||
        !('key' in result) ||
        !('value' in result) ||
        typeof result.key !== 'string' ||
        typeof result.value !== 'string'
      ) {
        return null
      }
      return { key: result.key, value: result.value }
    }, rawResults)
    setConfigData(cleanResults)
  }, [projectId])

  React.useEffect(() => {
    void fetchConfig()
  }, [fetchConfig])

  // todo: debounce
  const updateConfig = React.useCallback(
    (key: string) => (e: React.ChangeEvent<HTMLInputElement>) => {
      setOptimisticUpdateCache((cache) => ({
        ...cache,
        [key]: e.target.value,
      }))

      void updateJurassicCMS({ key: key, updated: e.target.value, project_id: projectId }).catch(
        (error) => {
          console.error(error)
          setOptimisticUpdateCache((cache) => {
            const next = { ...cache }
            delete next[key]
            return next
          })
        },
      )
    },
    [projectId],
  )

  const refresh = React.useCallback(() => {
    setOptimisticUpdateCache({})
    setConfigData(null)
    void fetchConfig()
  }, [fetchConfig])

  return (
    <FlexColumn
      style={{
        paddingLeft: 12,
        paddingRight: 12,
      }}
    >
      <FlexRow style={{ marginTop: 8, marginBottom: 12, paddingLeft: 8 }}>
        <H2>Jurassic CMS</H2>
      </FlexRow>
      {mergedData == null ? (
        'Fetching data from CMS'
      ) : (
        <FlexColumn style={{ paddingLeft: 8, paddingRight: 8, gap: 4 }}>
          {mergedData.map(({ key, value }, idx) => (
            <FlexRow style={{ justifyContent: 'space-between' }} key={key}>
              <PropertyLabel target={[]}>{key}</PropertyLabel>
              <StringInput
                testId=''
                value={value}
                onKeyDown={NO_OP}
                onChange={updateConfig(key)}
                onBlur={NO_OP}
              />
            </FlexRow>
          ))}
        </FlexColumn>
      )}
      <Button outline={false} highlight spotlight onClick={refresh} style={{ marginTop: 12 }}>
        Refresh
      </Button>
    </FlexColumn>
  )
})
