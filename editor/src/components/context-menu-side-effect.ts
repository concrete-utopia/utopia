import { contextMenu } from 'react-contexify'

export function openMenu(id: string, nativeEvent: MouseEvent) {
  contextMenu.show({
    id: id,
    event: nativeEvent,
  })
}
