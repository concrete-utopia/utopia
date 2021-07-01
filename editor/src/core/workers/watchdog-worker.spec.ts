import localforage from 'localforage'
import {
  handleMessage,
  createWatchdogInitMessage,
  createHeartbeatResponseMessage,
  createWatchdogTerminateMessage,
} from './watchdog-worker'
import { getProjectLockedKey } from '../shared/utils'

const projectId = 'projectid'

xdescribe('Watchdog worker heartbeat', () => {
  it('watchdog sends heartbeat request after init', (done) => {
    let heartbeatReceived = false
    let setIntervalId: NodeJS.Timer | null = null
    handleMessage(createWatchdogInitMessage(projectId, 10, 20), (msg) => {
      switch (msg.type) {
        case 'heartbeatrequest':
          heartbeatReceived = true
          break
        case 'watchdoginitresponse':
          setIntervalId = msg.setIntervalId
          break
      }
    })
    setTimeout(() => {
      expect(heartbeatReceived).toBeTruthy()
      expect(setIntervalId).not.toBeNull()
      handleMessage(createWatchdogTerminateMessage(setIntervalId!), () => {})
      done()
    }, 50)
  })
  it('watchdog sets to locked up if no heartbeat response', (done) => {
    let setIntervalId: NodeJS.Timer | null = null
    handleMessage(createWatchdogInitMessage(projectId, 10, 20), (msg) => {
      switch (msg.type) {
        case 'watchdoginitresponse':
          setIntervalId = msg.setIntervalId
          break
      }
    })
    setTimeout(async () => {
      const isLockedUp = await localforage.getItem(getProjectLockedKey(projectId))
      expect(isLockedUp).toEqual(true)
      expect(setIntervalId).not.toBeNull()
      handleMessage(createWatchdogTerminateMessage(setIntervalId!), () => {})
      done()
    }, 100)
  })
  it('watchdog does not set to locked up if there is heartbeat response', (done) => {
    let setIntervalId: NodeJS.Timer | null = null
    handleMessage(createWatchdogInitMessage(projectId, 1000, 2000), (msg) => {
      switch (msg.type) {
        case 'watchdoginitresponse':
          setIntervalId = msg.setIntervalId
          break
        case 'heartbeatrequest':
          handleMessage(createHeartbeatResponseMessage(msg.id, msg.projectId, false), () => {})
          break
      }
    })
    setTimeout(async () => {
      const isLockedUp = await localforage.getItem(getProjectLockedKey(projectId))
      expect(isLockedUp).toEqual(false)
      expect(setIntervalId).not.toBeNull()
      handleMessage(createWatchdogTerminateMessage(setIntervalId!), () => {})
      done()
    }, 50)
  })
  it('watchdog sets back non locked up if there is heartbeat response', async (done) => {
    let setIntervalId: NodeJS.Timer | null = null
    await localforage.setItem(getProjectLockedKey(projectId), true)
    handleMessage(createWatchdogInitMessage(projectId, 1000, 2000), (msg) => {
      switch (msg.type) {
        case 'watchdoginitresponse':
          setIntervalId = msg.setIntervalId
          break
        case 'heartbeatrequest':
          handleMessage(createHeartbeatResponseMessage(msg.id, msg.projectId, false), () => {})
          break
      }
    })
    setTimeout(async () => {
      const isLockedUp = await localforage.getItem(getProjectLockedKey(projectId))
      expect(isLockedUp).toEqual(false)
      expect(setIntervalId).not.toBeNull()
      handleMessage(createWatchdogTerminateMessage(setIntervalId!), () => {})
      done()
    }, 50)
  }),
    it('watchdog does not set back to non locked up if there is heartbeat response', async (done) => {
      let setIntervalId: NodeJS.Timer | null = null
      await localforage.setItem(getProjectLockedKey(projectId), true)
      handleMessage(createWatchdogInitMessage(projectId, 1000, 2000), (msg) => {
        switch (msg.type) {
          case 'watchdoginitresponse':
            setIntervalId = msg.setIntervalId
            break
          case 'heartbeatrequest':
            handleMessage(createHeartbeatResponseMessage(msg.id, msg.projectId, true), () => {})
            break
        }
      })
      setTimeout(async () => {
        const isLockedUp = await localforage.getItem(getProjectLockedKey(projectId))
        expect(isLockedUp).toEqual(true)
        expect(setIntervalId).not.toBeNull()
        handleMessage(createWatchdogTerminateMessage(setIntervalId!), () => {})
        done()
      }, 50)
    })
})
