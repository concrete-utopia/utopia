import type { ActionFunctionArgs } from '@remix-run/node'
import { json } from '@remix-run/node'
import { handle } from '../util/api.server'
import { ALLOW } from '../handlers/validators'
import fs from 'fs'
import * as child_process from 'child_process'
import util from 'util'

const exec = util.promisify(child_process.exec)

interface RequestBody {
  cssContent: string
  tailwindConfigContents: any
}

/**
 * Plan for unhardcoding this:
 * - get the project through the contents api
 * - generate project structure from the json
 * - `yarn install`
 * - `npx tailwind -o build.css'
 * - return the contents of build.css
 */

// TODO: this is all super hardcoded, unhardcode it
// the preferred way to go about this is to install the whole project and run tailwind there

async function handleGenerateTailwindStyles(request: Request) {
  const tailwindConfigPath = '/tmp/project/tailwind.config.js'
  const appCssPath = '/tmp/project/app.css'

  try {
    const { cssContent, tailwindConfigContents }: RequestBody = await request.json()

    const dir = await fs.promises.lstat('/tmp/project/')
    if (!dir.isDirectory()) {
      await fs.promises.mkdir('/tmp/project/')
    }

    await fs.promises.writeFile(tailwindConfigPath, tailwindConfigContents)
    await fs.promises.writeFile(appCssPath, cssContent)
    await exec(
      `cd /tmp/project/ && pnpm install tailwindcss @tailwindcss/forms @tailwindcss/typography`,
    )
    await exec(
      `cd /tmp/project/ && npx tailwindcss -i ${appCssPath} -o /tmp/project/app.out.css --config ${tailwindConfigPath}`,
    )

    const result = await fs.promises.readFile('/tmp/project/app.out.css', 'utf8')

    return json({ generatedCSS: result })
  } catch (error) {
    console.error(error)
    return json({ error: error }, { status: 500 })
  } finally {
    await fs.promises.unlink(tailwindConfigPath)
  }
}

export async function action(args: ActionFunctionArgs) {
  return handle(args, {
    POST: {
      validator: ALLOW,
      handler: handleGenerateTailwindStyles,
    },
  })
}
