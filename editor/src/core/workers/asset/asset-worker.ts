import { assertNever } from '../../shared/utils'
import { OutgoingWorkerMessage } from '../common/worker-types'

export type IncomingWorkerMessage = StoreAssetMessage | EvictAssetMessage

interface StoreAssetMessage {
  type: 'STORE_ASSET_MESSAGE'
  base64: string
  path: string
}

interface EvictAssetMessage {
  type: 'EVICT_ASSET_MESSAGE'
  path: string
}

async function storeAsset({ base64, path }: { base64: string; path: string }): Promise<void> {
  /* TODO */
}

async function evictAsset(path: string): Promise<void> {
  /* TODO */
}

export async function handleMessage(
  workerMessage: IncomingWorkerMessage,
  sendMessage: (content: OutgoingWorkerMessage) => void,
) {
  switch (workerMessage.type) {
    case 'STORE_ASSET_MESSAGE':
      await storeAsset({ base64: workerMessage.base64, path: workerMessage.path })
      sendMessage({ type: 'updateprocessed', jobID: '1' })
      break
    case 'EVICT_ASSET_MESSAGE':
      break
    default:
      assertNever(workerMessage)
  }
}
