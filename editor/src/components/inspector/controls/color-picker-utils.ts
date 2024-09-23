let colorPickerElement: Node | null = null

export function setColorPickerElement(node: Node | null): void {
  colorPickerElement = node
}

export function clearColorPickerElement(): void {
  colorPickerElement = null
}

export function isInsideColorPicker(eventTarget: EventTarget | null): boolean {
  return (
    eventTarget != null &&
    colorPickerElement != null &&
    eventTarget instanceof Node &&
    colorPickerElement.contains(eventTarget)
  )
}
