import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../core/shared/element-template'
import { left } from '../../core/shared/either'
import { BakedInStoryboardUID } from '../../core/model/scene-utils'
import { mapValues, pick } from '../../core/shared/object-utils'
import { renderTestEditorWithCode, TestAppUID, TestSceneUID } from './ui-jsx.test-utils'
import { disableStoredStateforTests } from '../editor/stored-state'
import { matchInlineSnapshotBrowser } from '../../../test/karma-snapshots'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import * as EP from '../../core/shared/element-path'

disableStoredStateforTests()

function sanitizeElementMetadata(element: ElementInstanceMetadata): ElementInstanceMetadata {
  return {
    ...element,
    element: left('REMOVED_FROM_TEST'),
  }
}

function sanitizeJsxMetadata(metadata: ElementInstanceMetadataMap) {
  return mapValues(sanitizeElementMetadata, metadata)
}

describe('DOM Walker tests', () => {
  it('Simple Project with one child View', async () => {
    const renderResult = await renderTestEditorWithCode(
      `
      import * as React from 'react'
      import {
        View,
        Scene,
        Storyboard,
      } from 'utopia-api'
      export var App = (props) => {
        return (
          <View style={{ ...props.style, backgroundColor: '#FFFFFF'}} data-uid={'05c'}>
            <View
              style={{ backgroundColor: '#DDDDDD', position: 'absolute', left: 55, top: 98, width: 266, height: 124  }}
              data-uid={'ef0'}
            >
              <View
                style={{ backgroundColor: '#DDDDDD', position: 'absolute', left: 71, top: 27, width: 125, height: 70 }}
                data-uid={'488'}
              />
            </View>
          </View>
        )
      }
      export var storyboard = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ position: 'relative', left: 0, top: 0, width: 375, height: 812 }}
              data-uid={'${TestSceneUID}'}
            >
              <App
                data-uid='${TestAppUID}' 
                style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
              />
            </Scene>
          </Storyboard>
        )
      }
      `,
      'await-first-dom-report',
    )
    const sanitizedMetadata = sanitizeJsxMetadata(renderResult.getEditorState().editor.jsxMetadata)
    matchInlineSnapshotBrowser(
      sanitizedMetadata,
      `
    Object {
      "utopia-storyboard-uid": Object {
        "attributeMetadatada": Object {},
        "componentInstance": true,
        "computedStyle": Object {},
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": Infinity,
          "width": Infinity,
          "x": -Infinity,
          "y": -Infinity,
        },
        "importInfo": Object {
          "type": "RIGHT",
          "value": Object {
            "originalName": "Storyboard",
            "path": "utopia-api",
            "variableName": "Storyboard",
          },
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": Infinity,
          "width": Infinity,
          "x": -Infinity,
          "y": -Infinity,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 0,
          "clientWidth": 0,
          "closestOffsetParentPath": Object {
            "parts": Array [],
            "type": "elementpath",
          },
          "coordinateSystemBounds": Object {
            "height": 0,
            "width": 0,
            "x": 0,
            "y": 0,
          },
          "display": "initial",
          "flexDirection": null,
          "float": "none",
          "globalContentBox": null,
          "hasPositionOffset": false,
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 0,
            "width": 0,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {},
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {},
          "parentFlexDirection": null,
          "parentFlexGap": 0,
          "parentLayoutSystem": "flow",
          "position": "static",
          "providesBoundsForAbsoluteChildren": false,
          "renderedChildrenCount": 0,
          "textDirection": "initial",
          "usesParentBounds": false,
        },
      },
      "utopia-storyboard-uid/scene-aaa": Object {
        "attributeMetadatada": null,
        "componentInstance": true,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "RIGHT",
          "value": Object {
            "originalName": "Scene",
            "path": "utopia-api",
            "variableName": "Scene",
          },
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 812,
          "clientWidth": 375,
          "closestOffsetParentPath": Object {
            "parts": Array [
              Array [
                "utopia-storyboard-uid",
                "scene-aaa",
              ],
            ],
            "type": "elementpath",
          },
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "block",
          "flexDirection": "row",
          "float": "none",
          "globalContentBox": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "hasPositionOffset": false,
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentFlexGap": 0,
          "parentLayoutSystem": "flow",
          "position": "relative",
          "providesBoundsForAbsoluteChildren": true,
          "renderedChildrenCount": 1,
          "textDirection": "ltr",
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity": Object {
        "attributeMetadatada": null,
        "componentInstance": true,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "LEFT",
          "value": "NOT_IMPORTED",
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 812,
          "clientWidth": 375,
          "closestOffsetParentPath": Object {
            "parts": Array [
              Array [
                "utopia-storyboard-uid",
                "scene-aaa",
              ],
            ],
            "type": "elementpath",
          },
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "block",
          "flexDirection": "row",
          "float": "none",
          "globalContentBox": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "hasPositionOffset": false,
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentFlexGap": 0,
          "parentLayoutSystem": "flow",
          "position": "absolute",
          "providesBoundsForAbsoluteChildren": true,
          "renderedChildrenCount": 1,
          "textDirection": "ltr",
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity:05c": Object {
        "attributeMetadatada": null,
        "componentInstance": true,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
            Array [
              "05c",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "RIGHT",
          "value": Object {
            "originalName": "View",
            "path": "utopia-api",
            "variableName": "View",
          },
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 812,
          "clientWidth": 375,
          "closestOffsetParentPath": Object {
            "parts": Array [
              Array [
                "utopia-storyboard-uid",
                "scene-aaa",
              ],
            ],
            "type": "elementpath",
          },
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "block",
          "flexDirection": "row",
          "float": "none",
          "globalContentBox": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "hasPositionOffset": false,
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentFlexGap": 0,
          "parentLayoutSystem": "flow",
          "position": "absolute",
          "providesBoundsForAbsoluteChildren": true,
          "renderedChildrenCount": 1,
          "textDirection": "ltr",
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity:05c/ef0": Object {
        "attributeMetadatada": null,
        "componentInstance": true,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
            Array [
              "05c",
              "ef0",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 124,
          "width": 266,
          "x": 55,
          "y": 98,
        },
        "importInfo": Object {
          "type": "RIGHT",
          "value": Object {
            "originalName": "View",
            "path": "utopia-api",
            "variableName": "View",
          },
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 124,
          "width": 266,
          "x": 55,
          "y": 98,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 124,
          "clientWidth": 266,
          "closestOffsetParentPath": Object {
            "parts": Array [
              Array [
                "utopia-storyboard-uid",
                "scene-aaa",
                "app-entity",
              ],
              Array [
                "05c",
              ],
            ],
            "type": "elementpath",
          },
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "block",
          "flexDirection": "row",
          "float": "none",
          "globalContentBox": Object {
            "height": 124,
            "width": 266,
            "x": 55,
            "y": 98,
          },
          "hasPositionOffset": true,
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 55,
            "y": 98,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentFlexGap": 0,
          "parentLayoutSystem": "flow",
          "position": "absolute",
          "providesBoundsForAbsoluteChildren": true,
          "renderedChildrenCount": 1,
          "textDirection": "ltr",
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity:05c/ef0/488": Object {
        "attributeMetadatada": null,
        "componentInstance": true,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
            Array [
              "05c",
              "ef0",
              "488",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 70,
          "width": 125,
          "x": 126,
          "y": 125,
        },
        "importInfo": Object {
          "type": "RIGHT",
          "value": Object {
            "originalName": "View",
            "path": "utopia-api",
            "variableName": "View",
          },
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 70,
          "width": 125,
          "x": 71,
          "y": 27,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 70,
          "clientWidth": 125,
          "closestOffsetParentPath": Object {
            "parts": Array [
              Array [
                "utopia-storyboard-uid",
                "scene-aaa",
                "app-entity",
              ],
              Array [
                "05c",
                "ef0",
              ],
            ],
            "type": "elementpath",
          },
          "coordinateSystemBounds": Object {
            "height": 124,
            "width": 266,
            "x": 55,
            "y": 98,
          },
          "display": "block",
          "flexDirection": "row",
          "float": "none",
          "globalContentBox": Object {
            "height": 70,
            "width": 125,
            "x": 126,
            "y": 125,
          },
          "hasPositionOffset": true,
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 124,
            "width": 266,
            "x": 55,
            "y": 98,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 71,
            "y": 27,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentFlexGap": 0,
          "parentLayoutSystem": "flow",
          "position": "absolute",
          "providesBoundsForAbsoluteChildren": true,
          "renderedChildrenCount": 0,
          "textDirection": "ltr",
          "usesParentBounds": true,
        },
      },
    }
    `,
    )
  })

  it('Simple Project with divs', async () => {
    const renderResult = await renderTestEditorWithCode(
      `
      import * as React from 'react'
      import {
        View,
        Scene,
        Storyboard,
      } from 'utopia-api'
      export var App = (props) => {
        return (
          <div style={{ ...props.style, backgroundColor: '#FFFFFF' }} data-uid={'05c'}>
            <div
              style={{ backgroundColor: '#DDDDDD', position: 'fixed', padding: 20, left: 55, top: 98, width: 266, height: 124 }}
              data-uid={'ef0'}
            >
              <div
                style={{ backgroundColor: '#DDDDDD', position: 'absolute', left: 71, top: 27, width: 125, height: 70 }}
                data-uid={'488'}
              />
            </div>
          </div>
        )
      }
      export var storyboard = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ position: 'relative', left: 0, top: 0, width: 375, height: 812 }}
              data-uid={'${TestSceneUID}'}
            >
              <App
                data-uid='${TestAppUID}' 
                style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
              />
            </Scene>
          </Storyboard>
        )
      }
      `,
      'await-first-dom-report',
    )
    const sanitizedMetadata = sanitizeJsxMetadata(renderResult.getEditorState().editor.jsxMetadata)
    matchInlineSnapshotBrowser(
      sanitizedMetadata,
      `
    Object {
      "utopia-storyboard-uid": Object {
        "attributeMetadatada": Object {},
        "componentInstance": true,
        "computedStyle": Object {},
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": Infinity,
          "width": Infinity,
          "x": -Infinity,
          "y": -Infinity,
        },
        "importInfo": Object {
          "type": "RIGHT",
          "value": Object {
            "originalName": "Storyboard",
            "path": "utopia-api",
            "variableName": "Storyboard",
          },
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": Infinity,
          "width": Infinity,
          "x": -Infinity,
          "y": -Infinity,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 0,
          "clientWidth": 0,
          "closestOffsetParentPath": Object {
            "parts": Array [],
            "type": "elementpath",
          },
          "coordinateSystemBounds": Object {
            "height": 0,
            "width": 0,
            "x": 0,
            "y": 0,
          },
          "display": "initial",
          "flexDirection": null,
          "float": "none",
          "globalContentBox": null,
          "hasPositionOffset": false,
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 0,
            "width": 0,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {},
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {},
          "parentFlexDirection": null,
          "parentFlexGap": 0,
          "parentLayoutSystem": "flow",
          "position": "static",
          "providesBoundsForAbsoluteChildren": false,
          "renderedChildrenCount": 0,
          "textDirection": "initial",
          "usesParentBounds": false,
        },
      },
      "utopia-storyboard-uid/scene-aaa": Object {
        "attributeMetadatada": null,
        "componentInstance": true,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "RIGHT",
          "value": Object {
            "originalName": "Scene",
            "path": "utopia-api",
            "variableName": "Scene",
          },
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 812,
          "clientWidth": 375,
          "closestOffsetParentPath": Object {
            "parts": Array [
              Array [
                "utopia-storyboard-uid",
                "scene-aaa",
              ],
            ],
            "type": "elementpath",
          },
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "block",
          "flexDirection": "row",
          "float": "none",
          "globalContentBox": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "hasPositionOffset": false,
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentFlexGap": 0,
          "parentLayoutSystem": "flow",
          "position": "relative",
          "providesBoundsForAbsoluteChildren": true,
          "renderedChildrenCount": 1,
          "textDirection": "ltr",
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity": Object {
        "attributeMetadatada": null,
        "componentInstance": true,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "LEFT",
          "value": "NOT_IMPORTED",
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 812,
          "clientWidth": 375,
          "closestOffsetParentPath": Object {
            "parts": Array [
              Array [
                "utopia-storyboard-uid",
                "scene-aaa",
              ],
            ],
            "type": "elementpath",
          },
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "block",
          "flexDirection": "row",
          "float": "none",
          "globalContentBox": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "hasPositionOffset": false,
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentFlexGap": 0,
          "parentLayoutSystem": "flow",
          "position": "absolute",
          "providesBoundsForAbsoluteChildren": true,
          "renderedChildrenCount": 1,
          "textDirection": "ltr",
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity:05c": Object {
        "attributeMetadatada": null,
        "componentInstance": false,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
            Array [
              "05c",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "LEFT",
          "value": "NOT_IMPORTED",
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 812,
          "clientWidth": 375,
          "closestOffsetParentPath": Object {
            "parts": Array [
              Array [
                "utopia-storyboard-uid",
                "scene-aaa",
              ],
            ],
            "type": "elementpath",
          },
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "block",
          "flexDirection": "row",
          "float": "none",
          "globalContentBox": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "hasPositionOffset": false,
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentFlexGap": 0,
          "parentLayoutSystem": "flow",
          "position": "absolute",
          "providesBoundsForAbsoluteChildren": true,
          "renderedChildrenCount": 1,
          "textDirection": "ltr",
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity:05c/ef0": Object {
        "attributeMetadatada": null,
        "componentInstance": false,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
            Array [
              "05c",
              "ef0",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 164,
          "width": 306,
          "x": 55,
          "y": 98,
        },
        "importInfo": Object {
          "type": "LEFT",
          "value": "NOT_IMPORTED",
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 164,
          "width": 306,
          "x": 55,
          "y": 98,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 164,
          "clientWidth": 306,
          "closestOffsetParentPath": Object {
            "parts": Array [
              Array [
                "utopia-storyboard-uid",
                "scene-aaa",
                "app-entity",
              ],
              Array [
                "05c",
              ],
            ],
            "type": "elementpath",
          },
          "coordinateSystemBounds": null,
          "display": "block",
          "flexDirection": "row",
          "float": "none",
          "globalContentBox": Object {
            "height": 164,
            "width": 306,
            "x": 55,
            "y": 98,
          },
          "hasPositionOffset": true,
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": false,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 55,
            "y": 98,
          },
          "padding": Object {
            "bottom": 20,
            "left": 20,
            "right": 20,
            "top": 20,
          },
          "parentFlexDirection": "row",
          "parentFlexGap": 0,
          "parentLayoutSystem": "flow",
          "position": "fixed",
          "providesBoundsForAbsoluteChildren": true,
          "renderedChildrenCount": 1,
          "textDirection": "ltr",
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity:05c/ef0/488": Object {
        "attributeMetadatada": null,
        "componentInstance": false,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
            Array [
              "05c",
              "ef0",
              "488",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 70,
          "width": 125,
          "x": 126,
          "y": 125,
        },
        "importInfo": Object {
          "type": "LEFT",
          "value": "NOT_IMPORTED",
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 70,
          "width": 125,
          "x": 71,
          "y": 27,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 70,
          "clientWidth": 125,
          "closestOffsetParentPath": Object {
            "parts": Array [
              Array [
                "utopia-storyboard-uid",
                "scene-aaa",
                "app-entity",
              ],
              Array [
                "05c",
                "ef0",
              ],
            ],
            "type": "elementpath",
          },
          "coordinateSystemBounds": Object {
            "height": 164,
            "width": 306,
            "x": 55,
            "y": 98,
          },
          "display": "block",
          "flexDirection": "row",
          "float": "none",
          "globalContentBox": Object {
            "height": 70,
            "width": 125,
            "x": 126,
            "y": 125,
          },
          "hasPositionOffset": true,
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 164,
            "width": 306,
            "x": 55,
            "y": 98,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 71,
            "y": 27,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentFlexGap": 0,
          "parentLayoutSystem": "flow",
          "position": "absolute",
          "providesBoundsForAbsoluteChildren": true,
          "renderedChildrenCount": 0,
          "textDirection": "ltr",
          "usesParentBounds": true,
        },
      },
    }
    `,
    )
  })

  it('Simple Project with flex parent', async () => {
    const renderResult = await renderTestEditorWithCode(
      `
      import * as React from 'react'
      import {
        View,
        Scene,
        Storyboard,
      } from 'utopia-api'
      export var App = (props) => {
        return (
          <div style={{ ...props.style, backgroundColor: '#FFFFFF', display: 'flex' }} data-uid={'05c'}>
            <div
              style={{ backgroundColor: '#DDDDDD', position: 'fixed', padding: 20, left: 55, top: 98, width: 266, height: 124 }}
              data-uid={'ef0'}
            >
              <div
                style={{ backgroundColor: '#DDDDDD', position: 'absolute', left: 71, top: 27, width: 125, height: 70 }}
                data-uid={'488'}
              />
            </div>
          </div>
        )
      }
      export var storyboard = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ position: 'relative', left: 0, top: 0, width: 375, height: 812 }}
              data-uid={'${TestSceneUID}'}
            >
              <App
                data-uid='${TestAppUID}' 
                style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
              />
            </Scene>
          </Storyboard>
        )
      }
      `,
      'await-first-dom-report',
    )
    const sanitizedMetadata = sanitizeJsxMetadata(renderResult.getEditorState().editor.jsxMetadata)
    matchInlineSnapshotBrowser(
      sanitizedMetadata,
      `
    Object {
      "utopia-storyboard-uid": Object {
        "attributeMetadatada": Object {},
        "componentInstance": true,
        "computedStyle": Object {},
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": Infinity,
          "width": Infinity,
          "x": -Infinity,
          "y": -Infinity,
        },
        "importInfo": Object {
          "type": "RIGHT",
          "value": Object {
            "originalName": "Storyboard",
            "path": "utopia-api",
            "variableName": "Storyboard",
          },
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": Infinity,
          "width": Infinity,
          "x": -Infinity,
          "y": -Infinity,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 0,
          "clientWidth": 0,
          "closestOffsetParentPath": Object {
            "parts": Array [],
            "type": "elementpath",
          },
          "coordinateSystemBounds": Object {
            "height": 0,
            "width": 0,
            "x": 0,
            "y": 0,
          },
          "display": "initial",
          "flexDirection": null,
          "float": "none",
          "globalContentBox": null,
          "hasPositionOffset": false,
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 0,
            "width": 0,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {},
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {},
          "parentFlexDirection": null,
          "parentFlexGap": 0,
          "parentLayoutSystem": "flow",
          "position": "static",
          "providesBoundsForAbsoluteChildren": false,
          "renderedChildrenCount": 0,
          "textDirection": "initial",
          "usesParentBounds": false,
        },
      },
      "utopia-storyboard-uid/scene-aaa": Object {
        "attributeMetadatada": null,
        "componentInstance": true,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "RIGHT",
          "value": Object {
            "originalName": "Scene",
            "path": "utopia-api",
            "variableName": "Scene",
          },
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 812,
          "clientWidth": 375,
          "closestOffsetParentPath": Object {
            "parts": Array [
              Array [
                "utopia-storyboard-uid",
                "scene-aaa",
              ],
            ],
            "type": "elementpath",
          },
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "block",
          "flexDirection": "row",
          "float": "none",
          "globalContentBox": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "hasPositionOffset": false,
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentFlexGap": 0,
          "parentLayoutSystem": "flow",
          "position": "relative",
          "providesBoundsForAbsoluteChildren": true,
          "renderedChildrenCount": 1,
          "textDirection": "ltr",
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity": Object {
        "attributeMetadatada": null,
        "componentInstance": true,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "LEFT",
          "value": "NOT_IMPORTED",
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 812,
          "clientWidth": 375,
          "closestOffsetParentPath": Object {
            "parts": Array [
              Array [
                "utopia-storyboard-uid",
                "scene-aaa",
              ],
            ],
            "type": "elementpath",
          },
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "flex",
          "flexDirection": "row",
          "float": "none",
          "globalContentBox": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "hasPositionOffset": false,
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flex",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentFlexGap": 0,
          "parentLayoutSystem": "flow",
          "position": "absolute",
          "providesBoundsForAbsoluteChildren": true,
          "renderedChildrenCount": 1,
          "textDirection": "ltr",
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity:05c": Object {
        "attributeMetadatada": null,
        "componentInstance": false,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
            Array [
              "05c",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "LEFT",
          "value": "NOT_IMPORTED",
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 812,
          "clientWidth": 375,
          "closestOffsetParentPath": Object {
            "parts": Array [
              Array [
                "utopia-storyboard-uid",
                "scene-aaa",
              ],
            ],
            "type": "elementpath",
          },
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "flex",
          "flexDirection": "row",
          "float": "none",
          "globalContentBox": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "hasPositionOffset": false,
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flex",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentFlexGap": 0,
          "parentLayoutSystem": "flow",
          "position": "absolute",
          "providesBoundsForAbsoluteChildren": true,
          "renderedChildrenCount": 1,
          "textDirection": "ltr",
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity:05c/ef0": Object {
        "attributeMetadatada": null,
        "componentInstance": false,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
            Array [
              "05c",
              "ef0",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 164,
          "width": 306,
          "x": 55,
          "y": 98,
        },
        "importInfo": Object {
          "type": "LEFT",
          "value": "NOT_IMPORTED",
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 164,
          "width": 306,
          "x": 55,
          "y": 98,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 164,
          "clientWidth": 306,
          "closestOffsetParentPath": Object {
            "parts": Array [
              Array [
                "utopia-storyboard-uid",
                "scene-aaa",
                "app-entity",
              ],
              Array [
                "05c",
              ],
            ],
            "type": "elementpath",
          },
          "coordinateSystemBounds": null,
          "display": "block",
          "flexDirection": "row",
          "float": "none",
          "globalContentBox": Object {
            "height": 164,
            "width": 306,
            "x": 55,
            "y": 98,
          },
          "hasPositionOffset": true,
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": false,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 55,
            "y": 98,
          },
          "padding": Object {
            "bottom": 20,
            "left": 20,
            "right": 20,
            "top": 20,
          },
          "parentFlexDirection": "row",
          "parentFlexGap": 0,
          "parentLayoutSystem": "flex",
          "position": "fixed",
          "providesBoundsForAbsoluteChildren": true,
          "renderedChildrenCount": 1,
          "textDirection": "ltr",
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity:05c/ef0/488": Object {
        "attributeMetadatada": null,
        "componentInstance": false,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
            Array [
              "05c",
              "ef0",
              "488",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 70,
          "width": 125,
          "x": 126,
          "y": 125,
        },
        "importInfo": Object {
          "type": "LEFT",
          "value": "NOT_IMPORTED",
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 70,
          "width": 125,
          "x": 71,
          "y": 27,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 70,
          "clientWidth": 125,
          "closestOffsetParentPath": Object {
            "parts": Array [
              Array [
                "utopia-storyboard-uid",
                "scene-aaa",
                "app-entity",
              ],
              Array [
                "05c",
                "ef0",
              ],
            ],
            "type": "elementpath",
          },
          "coordinateSystemBounds": Object {
            "height": 164,
            "width": 306,
            "x": 55,
            "y": 98,
          },
          "display": "block",
          "flexDirection": "row",
          "float": "none",
          "globalContentBox": Object {
            "height": 70,
            "width": 125,
            "x": 126,
            "y": 125,
          },
          "hasPositionOffset": true,
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 164,
            "width": 306,
            "x": 55,
            "y": 98,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 71,
            "y": 27,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentFlexGap": 0,
          "parentLayoutSystem": "flow",
          "position": "absolute",
          "providesBoundsForAbsoluteChildren": true,
          "renderedChildrenCount": 0,
          "textDirection": "ltr",
          "usesParentBounds": true,
        },
      },
    }
    `,
    )
  })

  it('Label carried through for normal elements', async () => {
    const renderResult = await renderTestEditorWithCode(
      `
      import * as React from 'react'
      import {
        View,
        Scene,
        Storyboard,
      } from 'utopia-api'
      export var App = (props) => {
        return <div style={{ ...props.style}} data-uid={'aaa'} data-label={'Hat'} />
      }
      export var storyboard = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ position: 'relative', left: 0, top: 0, width: 375, height: 812 }}
              data-uid={'${TestSceneUID}'}
            >
              <App
                data-uid='${TestAppUID}' 
                style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
              />
            </Scene>
          </Storyboard>
        )
      }
      `,
      'await-first-dom-report',
    )
    const sanitizedMetadata = sanitizeJsxMetadata(renderResult.getEditorState().editor.jsxMetadata)
    matchInlineSnapshotBrowser(
      sanitizedMetadata,
      `
    Object {
      "utopia-storyboard-uid": Object {
        "attributeMetadatada": Object {},
        "componentInstance": true,
        "computedStyle": Object {},
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": Infinity,
          "width": Infinity,
          "x": -Infinity,
          "y": -Infinity,
        },
        "importInfo": Object {
          "type": "RIGHT",
          "value": Object {
            "originalName": "Storyboard",
            "path": "utopia-api",
            "variableName": "Storyboard",
          },
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": Infinity,
          "width": Infinity,
          "x": -Infinity,
          "y": -Infinity,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 0,
          "clientWidth": 0,
          "closestOffsetParentPath": Object {
            "parts": Array [],
            "type": "elementpath",
          },
          "coordinateSystemBounds": Object {
            "height": 0,
            "width": 0,
            "x": 0,
            "y": 0,
          },
          "display": "initial",
          "flexDirection": null,
          "float": "none",
          "globalContentBox": null,
          "hasPositionOffset": false,
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 0,
            "width": 0,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {},
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {},
          "parentFlexDirection": null,
          "parentFlexGap": 0,
          "parentLayoutSystem": "flow",
          "position": "static",
          "providesBoundsForAbsoluteChildren": false,
          "renderedChildrenCount": 0,
          "textDirection": "initial",
          "usesParentBounds": false,
        },
      },
      "utopia-storyboard-uid/scene-aaa": Object {
        "attributeMetadatada": null,
        "componentInstance": true,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "RIGHT",
          "value": Object {
            "originalName": "Scene",
            "path": "utopia-api",
            "variableName": "Scene",
          },
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 812,
          "clientWidth": 375,
          "closestOffsetParentPath": Object {
            "parts": Array [
              Array [
                "utopia-storyboard-uid",
                "scene-aaa",
              ],
            ],
            "type": "elementpath",
          },
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "block",
          "flexDirection": "row",
          "float": "none",
          "globalContentBox": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "hasPositionOffset": false,
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentFlexGap": 0,
          "parentLayoutSystem": "flow",
          "position": "relative",
          "providesBoundsForAbsoluteChildren": true,
          "renderedChildrenCount": 1,
          "textDirection": "ltr",
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity": Object {
        "attributeMetadatada": null,
        "componentInstance": true,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "LEFT",
          "value": "NOT_IMPORTED",
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 812,
          "clientWidth": 375,
          "closestOffsetParentPath": Object {
            "parts": Array [
              Array [
                "utopia-storyboard-uid",
                "scene-aaa",
              ],
            ],
            "type": "elementpath",
          },
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "block",
          "flexDirection": "row",
          "float": "none",
          "globalContentBox": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "hasPositionOffset": false,
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentFlexGap": 0,
          "parentLayoutSystem": "flow",
          "position": "absolute",
          "providesBoundsForAbsoluteChildren": true,
          "renderedChildrenCount": 0,
          "textDirection": "ltr",
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity:aaa": Object {
        "attributeMetadatada": null,
        "componentInstance": false,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
            Array [
              "aaa",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "LEFT",
          "value": "NOT_IMPORTED",
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 812,
          "clientWidth": 375,
          "closestOffsetParentPath": Object {
            "parts": Array [
              Array [
                "utopia-storyboard-uid",
                "scene-aaa",
              ],
            ],
            "type": "elementpath",
          },
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "block",
          "flexDirection": "row",
          "float": "none",
          "globalContentBox": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "hasPositionOffset": false,
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentFlexGap": 0,
          "parentLayoutSystem": "flow",
          "position": "absolute",
          "providesBoundsForAbsoluteChildren": true,
          "renderedChildrenCount": 0,
          "textDirection": "ltr",
          "usesParentBounds": true,
        },
      },
    }
    `,
    )
  })

  it('Label carried through for generated elements', async () => {
    const renderResult = await renderTestEditorWithCode(
      `
      import * as React from 'react'
      import {
        View,
        Scene,
        Storyboard,
      } from 'utopia-api'
      export var App = (props) => {
        return <div style={{ ...props.style}} data-uid={'aaa'}>
          {[1, 2, 3].map(n => {
            return <div data-uid={'bbb'} data-label={'Plane'} />
          })}
        </div>
      }
      export var storyboard = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ position: 'relative', left: 0, top: 0, width: 375, height: 812 }}
              data-uid={'${TestSceneUID}'}
            >
              <App
                data-uid='${TestAppUID}' 
                style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
              />
            </Scene>
          </Storyboard>
        )
      }
      `,
      'await-first-dom-report',
    )
    const sanitizedMetadata = sanitizeJsxMetadata(renderResult.getEditorState().editor.jsxMetadata)
    matchInlineSnapshotBrowser(
      sanitizedMetadata,
      `
    Object {
      "utopia-storyboard-uid": Object {
        "attributeMetadatada": Object {},
        "componentInstance": true,
        "computedStyle": Object {},
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": Infinity,
          "width": Infinity,
          "x": -Infinity,
          "y": -Infinity,
        },
        "importInfo": Object {
          "type": "RIGHT",
          "value": Object {
            "originalName": "Storyboard",
            "path": "utopia-api",
            "variableName": "Storyboard",
          },
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": Infinity,
          "width": Infinity,
          "x": -Infinity,
          "y": -Infinity,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 0,
          "clientWidth": 0,
          "closestOffsetParentPath": Object {
            "parts": Array [],
            "type": "elementpath",
          },
          "coordinateSystemBounds": Object {
            "height": 0,
            "width": 0,
            "x": 0,
            "y": 0,
          },
          "display": "initial",
          "flexDirection": null,
          "float": "none",
          "globalContentBox": null,
          "hasPositionOffset": false,
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 0,
            "width": 0,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {},
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {},
          "parentFlexDirection": null,
          "parentFlexGap": 0,
          "parentLayoutSystem": "flow",
          "position": "static",
          "providesBoundsForAbsoluteChildren": false,
          "renderedChildrenCount": 0,
          "textDirection": "initial",
          "usesParentBounds": false,
        },
      },
      "utopia-storyboard-uid/scene-aaa": Object {
        "attributeMetadatada": null,
        "componentInstance": true,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "RIGHT",
          "value": Object {
            "originalName": "Scene",
            "path": "utopia-api",
            "variableName": "Scene",
          },
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 812,
          "clientWidth": 375,
          "closestOffsetParentPath": Object {
            "parts": Array [
              Array [
                "utopia-storyboard-uid",
                "scene-aaa",
              ],
            ],
            "type": "elementpath",
          },
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "block",
          "flexDirection": "row",
          "float": "none",
          "globalContentBox": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "hasPositionOffset": false,
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentFlexGap": 0,
          "parentLayoutSystem": "flow",
          "position": "relative",
          "providesBoundsForAbsoluteChildren": true,
          "renderedChildrenCount": 1,
          "textDirection": "ltr",
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity": Object {
        "attributeMetadatada": null,
        "componentInstance": true,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "LEFT",
          "value": "NOT_IMPORTED",
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 812,
          "clientWidth": 375,
          "closestOffsetParentPath": Object {
            "parts": Array [
              Array [
                "utopia-storyboard-uid",
                "scene-aaa",
              ],
            ],
            "type": "elementpath",
          },
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "block",
          "flexDirection": "row",
          "float": "none",
          "globalContentBox": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "hasPositionOffset": false,
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentFlexGap": 0,
          "parentLayoutSystem": "flow",
          "position": "absolute",
          "providesBoundsForAbsoluteChildren": true,
          "renderedChildrenCount": 3,
          "textDirection": "ltr",
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity:aaa": Object {
        "attributeMetadatada": null,
        "componentInstance": false,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
            Array [
              "aaa",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "LEFT",
          "value": "NOT_IMPORTED",
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 812,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 812,
          "clientWidth": 375,
          "closestOffsetParentPath": Object {
            "parts": Array [
              Array [
                "utopia-storyboard-uid",
                "scene-aaa",
              ],
            ],
            "type": "elementpath",
          },
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "block",
          "flexDirection": "row",
          "float": "none",
          "globalContentBox": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "hasPositionOffset": false,
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentFlexGap": 0,
          "parentLayoutSystem": "flow",
          "position": "absolute",
          "providesBoundsForAbsoluteChildren": true,
          "renderedChildrenCount": 3,
          "textDirection": "ltr",
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb~~~1": Object {
        "attributeMetadatada": null,
        "componentInstance": false,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
            Array [
              "aaa",
              "bbb~~~1",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 0,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "LEFT",
          "value": "NOT_IMPORTED",
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 0,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 0,
          "clientWidth": 375,
          "closestOffsetParentPath": Object {
            "parts": Array [
              Array [
                "utopia-storyboard-uid",
                "scene-aaa",
                "app-entity",
              ],
              Array [
                "aaa",
              ],
            ],
            "type": "elementpath",
          },
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "block",
          "flexDirection": "row",
          "float": "none",
          "globalContentBox": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "hasPositionOffset": false,
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentFlexGap": 0,
          "parentLayoutSystem": "flow",
          "position": "static",
          "providesBoundsForAbsoluteChildren": false,
          "renderedChildrenCount": 0,
          "textDirection": "ltr",
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb~~~2": Object {
        "attributeMetadatada": null,
        "componentInstance": false,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
            Array [
              "aaa",
              "bbb~~~2",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 0,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "LEFT",
          "value": "NOT_IMPORTED",
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 0,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 0,
          "clientWidth": 375,
          "closestOffsetParentPath": Object {
            "parts": Array [
              Array [
                "utopia-storyboard-uid",
                "scene-aaa",
                "app-entity",
              ],
              Array [
                "aaa",
              ],
            ],
            "type": "elementpath",
          },
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "block",
          "flexDirection": "row",
          "float": "none",
          "globalContentBox": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "hasPositionOffset": false,
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentFlexGap": 0,
          "parentLayoutSystem": "flow",
          "position": "static",
          "providesBoundsForAbsoluteChildren": false,
          "renderedChildrenCount": 0,
          "textDirection": "ltr",
          "usesParentBounds": true,
        },
      },
      "utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb~~~3": Object {
        "attributeMetadatada": null,
        "componentInstance": false,
        "computedStyle": null,
        "element": Object {
          "type": "LEFT",
          "value": "REMOVED_FROM_TEST",
        },
        "elementPath": Object {
          "parts": Array [
            Array [
              "utopia-storyboard-uid",
              "scene-aaa",
              "app-entity",
            ],
            Array [
              "aaa",
              "bbb~~~3",
            ],
          ],
          "type": "elementpath",
        },
        "globalFrame": Object {
          "height": 0,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "importInfo": Object {
          "type": "LEFT",
          "value": "NOT_IMPORTED",
        },
        "isEmotionOrStyledComponent": false,
        "label": null,
        "localFrame": Object {
          "height": 0,
          "width": 375,
          "x": 0,
          "y": 0,
        },
        "specialSizeMeasurements": Object {
          "clientHeight": 0,
          "clientWidth": 375,
          "closestOffsetParentPath": Object {
            "parts": Array [
              Array [
                "utopia-storyboard-uid",
                "scene-aaa",
                "app-entity",
              ],
              Array [
                "aaa",
              ],
            ],
            "type": "elementpath",
          },
          "coordinateSystemBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "display": "block",
          "flexDirection": "row",
          "float": "none",
          "globalContentBox": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "hasPositionOffset": false,
          "htmlElementName": "div",
          "immediateParentBounds": Object {
            "height": 812,
            "width": 375,
            "x": 0,
            "y": 0,
          },
          "immediateParentProvidesLayout": true,
          "layoutSystemForChildren": "flow",
          "margin": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "naturalHeight": null,
          "naturalWidth": null,
          "offset": Object {
            "x": 0,
            "y": 0,
          },
          "padding": Object {
            "bottom": 0,
            "left": 0,
            "right": 0,
            "top": 0,
          },
          "parentFlexDirection": "row",
          "parentFlexGap": 0,
          "parentLayoutSystem": "flow",
          "position": "static",
          "providesBoundsForAbsoluteChildren": false,
          "renderedChildrenCount": 0,
          "textDirection": "ltr",
          "usesParentBounds": true,
        },
      },
    }
    `,
    )
  })
})

describe('Capturing closest offset parent', () => {
  it('offset comes from inside the parent component', async () => {
    const renderResult = await renderTestEditorWithCode(
      `
      import * as React from 'react'
      import { Scene, Storyboard } from 'utopia-api'
      
      export var App = (props) => {
        return (
          <div data-uid='app-root'>
            <div
              data-uid='inner-div'
              style={{ position: 'absolute', top: 100 }}
            >
              <div data-uid='immediate-parent'>
                {props.children}
              </div>
            </div>
          </div>
        )
      }
      
      export var storyboard = (
        <Storyboard data-uid='sb'>
          <Scene
            data-uid='scene'
            style={{
              position: 'absolute',
              width: 375,
              height: 812,
            }}
          >
            <App data-uid='app'>
              <div
                data-uid='child'
                style={{
                  position: 'absolute',
                  width: 200,
                  height: 200,
                  backgroundColor: '#d3d3d3',
                }}
              />
            </App>
          </Scene>
        </Storyboard>
      )
      `,
      'await-first-dom-report',
    )

    const metadataOfInnerElement =
      renderResult.getEditorState().editor.jsxMetadata['sb/scene/app/child']
    const expectedClosestOffsetParent = 'sb/scene/app:app-root/inner-div'
    expect(
      EP.toString(metadataOfInnerElement.specialSizeMeasurements.closestOffsetParentPath),
    ).toBe(expectedClosestOffsetParent)
  })
})
