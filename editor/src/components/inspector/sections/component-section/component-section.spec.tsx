import React from 'react'
import { render, act } from '@testing-library/react'

import * as EP from '../../../../core/shared/element-path'

import { getStoreHook, TestInspectorContextProvider } from '../../common/inspector.test-utils'
import type { CanvasVector } from '../../../../core/shared/math-utils'
import {
  setupReactWhyDidYouRender,
  enableWhyDidYouRenderOnComponent,
} from '../../../../utils/react-memoize.test-utils'
import { ComponentSection } from './component-section'
import { ScenePathForTestUiJsFile } from '../../../../core/model/test-ui-js-file.test-utils'

describe('Component Section', () => {
  enableWhyDidYouRenderOnComponent(ComponentSection)

  it('make sure whyDidYouRender is enabled', () => {
    expect((ComponentSection as any).whyDidYouRender).toBeTruthy()
  })
})
