import React from 'react'
import {
  FlexRow,
  H2,
  FlexColumn,
  StringInput,
  Button,
  Dialog,
  FormButton,
  SquareButton,
  Icons,
} from '../../../uuiui'
import { JURASSIC_CMS_URL } from '../../../common/env-vars'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { PropertyLabel } from '../widgets/property-label'
import { NO_OP } from '../../../core/shared/utils'
import {
  JURASSIC_CMS_UPDATE_GLOBAL,
  deleteJurassicCMSKey,
  updateJurassicCMSKey,
} from '../../editor/jurassic-cms'
import invariant from '../../../third-party/remix/invariant'
import type { EditorDispatch } from '../../editor/action-types'
import { hideModal, showModal } from '../../editor/actions/action-creators'
import { useDispatch } from '../../editor/store/dispatch-context'
import { atom, useAtom, useSetAtom } from 'jotai'
import { useAtomCallback } from 'jotai/utils'

type ConfigData = Array<{ key: string; value: string }>

const CMSPanelOptimisticUpdateCache = atom<Record<string, string>>({})
export function useUpdateCMSCache() {
  const cacheRef = useAtomCallback(
    React.useCallback((get) => get(CMSPanelOptimisticUpdateCache), []),
  )
  const setOptimisticUpdateCache = useSetAtom(CMSPanelOptimisticUpdateCache)
  return React.useCallback(
    (key: string, value: string) => {
      setOptimisticUpdateCache((c) => ({ ...c, [key]: value }))
      return cacheRef()[key]
    },
    [cacheRef, setOptimisticUpdateCache],
  )
}

export const JurassicCMSPanel = React.memo(() => {
  const projectId = useEditorState(
    Substores.fullStore,
    (store) => store.editor.id,
    'JurassicCMSPanel projectId',
  )

  invariant(projectId, 'Project id cannot be null')

  const dispatch = useDispatch()

  const [configData, setConfigData] = React.useState<ConfigData | null>(null)
  const [optimisticUpdateCache, setOptimisticUpdateCache] = useAtom(CMSPanelOptimisticUpdateCache)

  const mergedData: ConfigData | null = React.useMemo(() => {
    const keysToValues: Record<string, string> = {}
    if (configData != null) {
      for (const { key, value } of configData) {
        keysToValues[key] = value
      }
    }

    for (const [key, value] of Object.entries(optimisticUpdateCache)) {
      keysToValues[key] = value
    }

    return Object.entries(keysToValues)
      .map(([key, value]) => ({ key: key, value: value }))
      .sort((left, right) => left.key.localeCompare(right.key))
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

  // TODO: debounce
  const updateConfig = React.useCallback(
    (key: string) => (e: React.ChangeEvent<HTMLInputElement>) => {
      const updated = e.target.value
      setOptimisticUpdateCache((cache) => ({
        ...cache,
        [key]: updated,
      }))

      // TODO: save original key
      JURASSIC_CMS_UPDATE_GLOBAL[key]?.(updated)

      void updateJurassicCMSKey({ key: key, updated: updated, project_id: projectId }).catch(
        (error) => {
          showError(error)
          setOptimisticUpdateCache((cache) => {
            const next = { ...cache }
            delete next[key]
            return next
          })
        },
      )
    },
    [projectId, setOptimisticUpdateCache],
  )

  const deleteKey = React.useCallback(
    (key: string, original: string) => {
      setOptimisticUpdateCache((cache) => {
        const next = { ...cache }
        delete next[key]
        return next
      })
      setConfigData((data) => (data == null ? null : data.filter((kv) => kv.key !== key)))
      void deleteJurassicCMSKey({ project_id: projectId, key: key }).catch((e) => {
        showError(e)
        setOptimisticUpdateCache((cache) => ({ ...cache, [key]: original }))
      })
    },
    [projectId, setOptimisticUpdateCache],
  )

  const showConfirmDeleteModal = React.useCallback(
    (key: string, original: string) => () =>
      dispatch([
        showModal({
          type: 'confirm-cms-key-delete',
          key: key,
          onDeleteClick: () => deleteKey(key, original),
        }),
      ]),
    [deleteKey, dispatch],
  )

  const refresh = React.useCallback(() => {
    setOptimisticUpdateCache({})
    setConfigData(null)
    void fetchConfig()
  }, [fetchConfig, setOptimisticUpdateCache])

  const [newKey, setNewKey] = React.useState<string>('')
  const [newValue, setNewValue] = React.useState<string>('')

  const updateNewKey = React.useCallback(
    (e: React.ChangeEvent<HTMLInputElement>) => setNewKey(e.target.value),
    [],
  )
  const updateNewValue = React.useCallback(
    (e: React.ChangeEvent<HTMLInputElement>) => setNewValue(e.target.value),
    [],
  )

  const addNewKey = React.useCallback(() => {
    if (newKey === '' || newValue === '') {
      return
    }

    setConfigData((data) => (data == null ? null : [...data, { key: newKey, value: newValue }]))
    setOptimisticUpdateCache((cache) => ({
      ...cache,
      [newKey]: newValue,
    }))

    void updateJurassicCMSKey({
      key: newKey,
      updated: newValue,
      project_id: projectId,
    }).finally(() => {
      setNewKey('')
      setNewValue('')
    })
  }, [newKey, newValue, projectId, setOptimisticUpdateCache])

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
              <FlexRow style={{ justifyContent: 'flex-end' }}>
                <StringInput
                  testId=''
                  value={value}
                  onKeyDown={NO_OP}
                  onChange={updateConfig(key)}
                  onBlur={NO_OP}
                />
                <SquareButton
                  style={{ height: 18, width: 18 }}
                  highlight
                  onClick={showConfirmDeleteModal(key, value)}
                >
                  <Icons.CrossSmall />
                </SquareButton>
              </FlexRow>
            </FlexRow>
          ))}
        </FlexColumn>
      )}
      <Button
        outline={false}
        highlight
        spotlight
        onClick={refresh}
        style={{ marginTop: 12, marginLeft: 8, marginRight: 8 }}
      >
        Refresh
      </Button>
      <FlexRow style={{ marginTop: 12, marginLeft: 8, marginRight: 8, gap: 12 }}>
        <StringInput
          testId=''
          value={newKey}
          onKeyDown={NO_OP}
          onChange={updateNewKey}
          onBlur={NO_OP}
          placeholder='Key'
        />
        <StringInput
          testId=''
          value={newValue}
          onKeyDown={NO_OP}
          onChange={updateNewValue}
          onBlur={NO_OP}
          placeholder='Value'
        />
      </FlexRow>
      <Button
        outline={false}
        highlight
        spotlight
        onClick={addNewKey}
        style={{ marginTop: 12, marginLeft: 8, marginRight: 8 }}
      >
        Add new key
      </Button>
    </FlexColumn>
  )
})

interface ConfirmCMSKeyDeleteDialogProps {
  dispatch: EditorDispatch
  key: string
  onDeleteClick: () => void
}

export const ConfirmCMSKeyDeleteDialog: React.FunctionComponent<
  React.PropsWithChildren<ConfirmCMSKeyDeleteDialogProps>
> = (props) => {
  const hide = React.useCallback(() => {
    props.dispatch([hideModal()], 'everyone')
  }, [props])
  return (
    <Dialog
      title='Delete key'
      content={<DialogBody {...props} />}
      defaultButton={<AcceptButton {...props} />}
      secondaryButton={<CancelButton {...props} />}
      closeCallback={hide}
    />
  )
}

const DialogBody: React.FunctionComponent<
  React.PropsWithChildren<ConfirmCMSKeyDeleteDialogProps>
> = (props) => (
  <React.Fragment>
    <p>
      Are you sure you want to delete <span>{props.key}</span>?
    </p>
    <p>Deleted files are permanently removed.</p>
  </React.Fragment>
)

const AcceptButton: React.FunctionComponent<
  React.PropsWithChildren<ConfirmCMSKeyDeleteDialogProps>
> = ({ dispatch, onDeleteClick }) => {
  const clickButton = React.useCallback(() => {
    onDeleteClick()
    dispatch([hideModal()], 'everyone')
  }, [dispatch, onDeleteClick])

  return (
    <FormButton primary danger onClick={clickButton}>
      Delete
    </FormButton>
  )
}

const CancelButton: React.FunctionComponent<
  React.PropsWithChildren<ConfirmCMSKeyDeleteDialogProps>
> = ({ dispatch }) => {
  const clickButton = React.useCallback(() => {
    dispatch([hideModal()], 'everyone')
  }, [dispatch])

  return <FormButton onClick={clickButton}>Cancel</FormButton>
}

function showError(e: any) {
  // TODO: pop a toast
  console.error(e)
}
