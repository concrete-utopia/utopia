// minimal PNG image
// https://stackoverflow.com/a/36610159
export const imgBase641x1 = `data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVQYV2NgYAAAAAMAAWgmWQ0AAAAASUVORK5CYII=`

// https://png-pixel.com/
export const imgBase642x2 = `data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAIAAAACCAYAAABytg0kAAAAEklEQVR42mP8z/D/PwMQMMIYAEDsBf08YSRIAAAAAElFTkSuQmCC`

// https://stackoverflow.com/a/47497249
export const makeImageFile = (base64: string, name: string) =>
  fetch(base64)
    .then((res) => res.blob())
    .then((blob) => new File([blob], name, { type: 'image/png' }))
