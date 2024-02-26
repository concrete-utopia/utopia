const roomIdPrefix = `project-room-`

export function projectIdToRoomId(projectId: string): string {
  return `${roomIdPrefix}${projectId}`
}

export function isRoomId(s: string): boolean {
  return s.startsWith(roomIdPrefix)
}
