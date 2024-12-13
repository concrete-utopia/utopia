import { TailwindConfigPath } from '../../../core/tailwind/tailwind-config'
import { createModifiedProject } from '../../../sample-projects/sample-project-utils.test-utils'
import { StoryboardFilePath } from '../../editor/store/editor-state'

export const JustifyContentClassMapping = {
  'flex-start': 'justify-start',
  center: 'justify-center',
  'flex-end': 'justify-end',
} as const

export const AlignItemsClassMapping = {
  'flex-start': 'items-start',
  center: 'items-center',
  'flex-end': 'items-end',
} as const

export const TailwindProject = (classes: string) =>
  createModifiedProject({
    [StoryboardFilePath]: `
  import React from 'react'
  import { Scene, Storyboard } from 'utopia-api'
  export var storyboard = (
    <Storyboard data-uid='sb'>
      <Scene
        id='scene'
        commentId='scene'
        data-uid='scene'
        style={{
          width: 700,
          height: 759,
          position: 'absolute',
          left: 212,
          top: 128,
        }}
      >
        <div
          data-uid='mydiv'
          data-testid='mydiv'
          className='top-10 left-10 w-64 h-64 bg-slate-100 absolute ${classes}'
        >
          <div className='bg-red-500 w-10 h-10' data-uid='child-1' />
          <div className='bg-red-500 w-10 h-10' data-uid='child-2' />
        </div>  
      </Scene>
    </Storyboard>
  )
  
  `,
    [TailwindConfigPath]: `
      const TailwindConfig = { }
      export default TailwindConfig
  `,
    'app.css': `
      @tailwind base;
      @tailwind components;
      @tailwind utilities;`,
  })
