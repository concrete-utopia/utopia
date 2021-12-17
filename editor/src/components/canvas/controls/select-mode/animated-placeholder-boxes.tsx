import * as React from 'react'
import { useEditorState } from '../../../editor/store/store-hook'

function placeholderId(uid: string): string {
  return `animated-placeholder-${uid}`
}

export const AnimatedPlaceholderBoxes = () => {
  const animatedPlaceholderTargetUids = useEditorState(
    (store) =>
      store.derived.canvas.transientState.editorStatePatch.canvas?.animatedPlaceholderTargetUids ??
      [],
    'AnimatedPlaceholderBoxes animatedPlaceholderTargetUids',
  )

  React.useEffect(() => {
    animatedPlaceholderTargetUids.forEach((uid) => {
      const container = document.getElementById('animated-placeholder-boxes-container')
      const foundTarget = document.querySelector(`[data-uid='${uid}']`)
      const foundPlaceholder = document.getElementById(placeholderId(uid))
      if (container != null && foundTarget != null && foundPlaceholder != null) {
        const targetPosition = foundTarget.getBoundingClientRect()
        const containerPosition = container.getBoundingClientRect()
        const currentPosition = foundPlaceholder.getBoundingClientRect()
        if (currentPosition.width !== 0 || currentPosition.height !== 0) {
          // if the width and height are 0, we are still running the initial placement – we don't want to animate that.
          foundPlaceholder.style.transition = 'all 0.4s'
        }
        foundPlaceholder.style.left = `${targetPosition.x - containerPosition.x}px`
        foundPlaceholder.style.top = `${targetPosition.y - containerPosition.y}px`
        foundPlaceholder.style.width = `${targetPosition.width}px`
        foundPlaceholder.style.height = `${targetPosition.height}px`
      }
    })

    return function cleanup() {}
    // eslint-disable-next-line react-hooks/exhaustive-deps
  })

  return (
    <div id='animated-placeholder-boxes-container' style={{ position: 'absolute' }}>
      {animatedPlaceholderTargetUids.map((targetUid) => {
        return (
          <div
            key={targetUid}
            id={placeholderId(targetUid)}
            style={{
              position: 'absolute',
              top: 0,
              left: 0,
              width: 0,
              height: 0,
              backgroundColor: 'pink',
            }}
          />
        )
      })}
    </div>
  )
}
