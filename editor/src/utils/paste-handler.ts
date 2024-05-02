export const DataPasteHandler = 'data-pastehandler'

export function dataPasteHandler(value: boolean | undefined) {
  return { [DataPasteHandler]: value }
}

export function isPasteHandler(target: EventTarget | null): boolean {
  return (
    target != null &&
    target instanceof HTMLElement &&
    target.getAttribute(DataPasteHandler) === 'true'
  )
}
