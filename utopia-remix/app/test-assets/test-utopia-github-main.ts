import type { ProjectContentTreeRoot } from 'utopia-shared/src/types'

export const TestProjectUtopiaGithubMain: ProjectContentTreeRoot = {
  assets: {
    type: 'PROJECT_CONTENT_DIRECTORY',
    fullPath: '/assets',
    directory: {
      type: 'DIRECTORY',
    },
    children: {
      'Curaçao.png': {
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/assets/Curaçao.png',
        content: {
          type: 'IMAGE_FILE',
          hash: 0,
          gitBlobSha: '1fd337ce073c90c8d3c351b79bc99b6c1c8dd9de',
        },
      },
      'deer.JPG': {
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/assets/deer.JPG',
        content: {
          type: 'IMAGE_FILE',
          hash: 0,
          gitBlobSha: 'bdc443699f698d8f2cbd8b9b5e56b2e9be12331f',
        },
      },
      'lanterns.png': {
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/assets/lanterns.png',
        content: {
          type: 'IMAGE_FILE',
          hash: 0,
          gitBlobSha: '20d240eb3a6b4511f75d8c3c7bbff8efb452128b',
        },
      },
      'pyramids.png': {
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/assets/pyramids.png',
        content: {
          type: 'IMAGE_FILE',
          hash: 0,
          gitBlobSha: 'cee88af40faedf8384e2a5e3eb6b75ba39d8b4ff',
        },
      },
    },
  },
  public: {
    type: 'PROJECT_CONTENT_DIRECTORY',
    fullPath: '/public',
    directory: {
      type: 'DIRECTORY',
    },
    children: {
      'data.js': {
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/public/data.js',
        content: {
          type: 'TEXT_FILE',
          fileContents: {
            code: "export const activities = [\n  {\n    id: 0,\n    name: 'Attend The Floating Lantern Festival',\n    imageUrl: './assets/lanterns.png',\n    rating: 3,\n    date: '06 05 2022',\n    description:\n      'In the warmth of the moonlight, thousands of glowing lanterns drift gracefully into the sky, creating the magic and serenity of Thailands enchanting floating lantern festival.',\n    categories: ['aquatic', 'colorful'],\n  },\n  {\n    id: 1,\n    name: 'Feed a Wild Deer',\n    imageUrl: './assets/deer.JPG',\n    rating: 4,\n    date: '07 17 2019',\n    description:\n      'With a polite nudge, the wild deer of Nara nibbled treats from an outstretched hand, forging an unforgettable moment of connection in the magical city of Nara, Japan.',\n    categories: ['nearby', 'risky'],\n  },\n  {\n    id: 2,\n    name: 'Drink Curaçao in Curaçao ',\n    imageUrl: './assets/Curaçao.png',\n    rating: 5,\n    date: '04 19 2011',\n    description:\n      'Sipping a vibrant blue cocktail on sun-kissed shores in the Caribbean, I felt the islands warmth and spirit infuse every delicious, refreshing sip.',\n    categories: ['mixological', 'tropical', 'aquatic'],\n  },\n  {\n    id: 3,\n    name: 'See The Pyramids of Giza',\n    imageUrl: './assets/pyramids.png',\n    rating: 2,\n    date: '08 28 2021',\n    description:\n      'Standing before the awe-inspiring Pyramids of Giza, I felt humbled by their sheer scale and the magnificent history they represented.',\n    categories: ['wonderous', 'rare'],\n  },\n]\n\nexport const categories = [\n  'rare',\n  'colorful',\n  'odd',\n  'trending',\n  'risky',\n  'snowy',\n  'mixological',\n  'aquatic',\n  'wonderous',\n  'tropical',\n]\n",
            parsed: {
              type: 'UNPARSED',
            },
            revisionsState: 'CODE_AHEAD',
          },
          lastParseSuccess: null,
          lastSavedContents: null,
          versionNumber: 0,
        },
      },
      'globals.css': {
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/public/globals.css',
        content: {
          type: 'TEXT_FILE',
          fileContents: {
            code: "@font-face {\n    font-family: primary;\n    src: url(https://cdn.utopia.pizza/editor/sample-assets/stretchpro.woff);\n}\n\n@font-face {\n    font-family: primary-basic;\n    src: url(https://cdn.utopia.pizza/editor/sample-assets/stretchpro-basic.woff);\n}\n \n:root {\n    --off-white: #ece6b0;\n    --purple: #7cf08c;\n    --orange: #0f7e6b;\n    --yellow: #dd4a76;\n\n    --primary: primary;\n    --primary-basic: primary-basic;\n    --secondary: 'Roboto Mono';\n    --safety: 'sans-serif';\n}\n\n#my-thing {\n    view-transition-name: main-header;\n}\n\n.my-class {\n    view-transition-name: main-header;\n    contain: layout;\n  }\n\n@keyframes fade-in {\n    from { opacity: 0; }\n}\n  \n@keyframes fade-out {\n    to { opacity: 0; }\n}\n  \n@keyframes slide-from-right {\n    from { transform: translateX(30px); }\n}\n  \n@keyframes slide-to-left {\n    to { transform: translateX(-30px); }\n}\n  \n::view-transition-old(root) {\n    animation: 90ms cubic-bezier(0.4, 0, 1, 1) both fade-out,\n      300ms cubic-bezier(0.4, 0, 0.2, 1) both slide-to-left;\n}\n  \n::view-transition-new(root) {\n    animation: 210ms cubic-bezier(0, 0, 0.2, 1) 90ms both fade-in,\n      300ms cubic-bezier(0.4, 0, 0.2, 1) both slide-from-right;\n}",
            parsed: {
              type: 'UNPARSED',
            },
            revisionsState: 'CODE_AHEAD',
          },
          lastParseSuccess: null,
          lastSavedContents: null,
          versionNumber: 0,
        },
      },
      'index.html': {
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/public/index.html',
        content: {
          type: 'TEXT_FILE',
          fileContents: {
            code: '<!DOCTYPE html>\n<html lang="en">\n  <head>\n    <meta charset="utf-8">\n    <title>Utopia React App</title>\n    <!-- Begin Generated Utopia External Links -->\n    <link href="https://fonts.googleapis.com/css2?family=Alegreya+SC:ital,wght@0,700" rel="stylesheet">\n    <link href="https://fonts.googleapis.com/css2?family=Cormorant+Garamond:ital,wght@0,700" rel="stylesheet">\n    <!-- End Generated Utopia External Links -->\n  </head>\n  <body>\n    <div id="root"></div>\n  </body>\n</html>',
            parsed: {
              type: 'UNPARSED',
            },
            revisionsState: 'CODE_AHEAD',
          },
          lastParseSuccess: null,
          lastSavedContents: null,
          versionNumber: 0,
        },
      },
    },
  },
  'package.json': {
    type: 'PROJECT_CONTENT_FILE',
    fullPath: '/package.json',
    content: {
      type: 'TEXT_FILE',
      fileContents: {
        code: '{\n  "name": "Utopia Project",\n  "version": "0.1.0",\n  "utopia": {\n    "main-ui": "utopia/storyboard.js",\n    "html": "public/index.html",\n    "js": "src/index.js"\n  },\n  "dependencies": {\n    "react": "16.13.1",\n    "react-dom": "16.13.1",\n    "utopia-api": "0.4.1",\n    "react-spring": "8.0.27",\n    "@heroicons/react": "1.0.1",\n    "@emotion/react": "11.9.3",\n    "@remix-run/react": "1.19.3"\n  }\n}',
        parsed: {
          type: 'UNPARSED',
        },
        revisionsState: 'CODE_AHEAD',
      },
      lastParseSuccess: null,
      lastSavedContents: null,
      versionNumber: 0,
    },
  },
  src: {
    type: 'PROJECT_CONTENT_DIRECTORY',
    fullPath: '/src',
    directory: {
      type: 'DIRECTORY',
    },
    children: {
      'app.js': {
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/src/app.js',
        content: {
          type: 'TEXT_FILE',
          fileContents: {
            code: "import * as React from 'react'\nimport '../public/globals.css'\nimport { FlexCol } from './utils'\n\nexport var App = () => {\n  return (\n    <FlexCol\n      style={{\n        width: '100%',\n        height: '100%',\n        background: 'white',\n        justifyContent: 'center',\n        alignItems: 'center',\n      }}\n    >\n      <img\n        src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.png?raw=true'\n        alt='Utopia logo'\n        style={{ height: '40%' }}\n      />\n    </FlexCol>\n  )\n}\n",
            parsed: {
              type: 'UNPARSED',
            },
            revisionsState: 'CODE_AHEAD',
          },
          lastParseSuccess: null,
          lastSavedContents: null,
          versionNumber: 0,
        },
      },
      'card-complete.js': {
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/src/card-complete.js',
        content: {
          type: 'TEXT_FILE',
          fileContents: {
            code: "import * as React from 'react'\nimport '../public/globals.css'\n\nexport var CardComplete = (props) => {\n  return (\n    <div\n      style={{\n        width: 834,\n        height: 267,\n        backgroundColor: props.color,\n        display: 'flex',\n        flexDirection: 'row',\n        overflow: 'hidden',\n        borderTop: `3px solid ${props.color}`,\n        ...props.style,\n      }}\n    >\n      <div\n        style={{\n          display: 'flex',\n          flexDirection: 'column',\n          justifyContent: 'space-between',\n          padding: 20,\n          color: 'var(--purple)',\n          backgroundColor: props.color,\n          gap: 20,\n          width: 679,\n          height: '100%',\n        }}\n      >\n        <div\n          style={{\n            fontFamily: 'primary-basic',\n            fontSize: '28px',\n            textAlign: 'left',\n            lineHeight: '1.2em',\n          }}\n        >\n          {props.activity.name}\n        </div>\n        <div\n          style={{\n            fontFamily: 'var(--secondary)',\n            fontSize: '12px',\n            textAlign: 'left',\n          }}\n        >\n          {props.activity.description}\n          <br />\n          <br />\n          <div\n            style={{\n              display: 'flex',\n              flexDirection: 'row',\n              gap: 5,\n            }}\n          >\n            {props.activity.categories.map((category) => (\n              <div\n                style={{\n                  backgroundColor: 'var(--purple)',\n                  color: props.color,\n                  padding: '2px 6px',\n                  borderRadius: 3,\n                }}\n              >\n                {category}\n              </div>\n            ))}\n          </div>\n        </div>\n      </div>\n      <div\n        style={{\n          height: '100%',\n          width: '100%',\n          backgroundImage: `url(${props.activity.imageUrl})`,\n          backgroundSize: '100%',\n          backgroundPosition: '50%',\n          display: 'flex',\n          alignItems: 'flex-end',\n          justifyContent: 'flex-end',\n        }}\n      >\n        <div\n          style={{\n            height: 'min-content',\n            display: 'flex',\n            justifyContent: 'center',\n            alignItems: 'center',\n            padding: 20,\n            fontFamily: 'var(--secondary)',\n            fontSize: '20px',\n            fontWeight: '600',\n            color: props.color,\n            zIndex: 100,\n          }}\n        >\n          {props.activity.date}\n        </div>\n      </div>\n    </div>\n  )\n}\n",
            parsed: {
              type: 'UNPARSED',
            },
            revisionsState: 'CODE_AHEAD',
          },
          lastParseSuccess: null,
          lastSavedContents: null,
          versionNumber: 0,
        },
      },
      'card-incomplete.js': {
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/src/card-incomplete.js',
        content: {
          type: 'TEXT_FILE',
          fileContents: {
            code: "import * as React from 'react'\nimport '../public/globals.css'\n\nexport var CardIncomplete = (props) => {\n  return (\n    <div\n      style={{\n        width: 834,\n        height: 267,\n        overflow: 'hidden',\n        backgroundImage: `url(${props.activity.imageUrl})`,\n        backgroundSize: '100%',\n        backgroundPosition: '50%',\n        borderTop: `3px solid ${props.color}`,\n      }}\n    >\n      <div\n        style={{\n          display: 'flex',\n          flexDirection: 'column',\n          padding: '20px 40px 20px 20px',\n          backgroundColor: props.color,\n          gap: 10,\n          position: 'absolute',\n          borderBottomRightRadius: 20,\n          zIndex: 100,\n        }}\n      >\n        <div\n          style={{\n            color: 'var(--purple)',\n            fontFamily: 'primary-basic',\n            fontSize: '28px',\n            textAlign: 'left',\n            lineHeight: '1.2em',\n          }}\n        >\n          {props.activity.name}\n        </div>\n      </div>\n      <div\n        style={{\n          width: '120%',\n          marginLeft: '-10%',\n          height: '160%',\n          marginTop: '-10%',\n          backgroundImage: `url(${props.activity.imageUrl})`,\n          backgroundSize: '100%',\n          backgroundPosition: '50%',\n          filter: 'blur(1rem)',\n        }}\n      />\n    </div>\n  )\n}\n",
            parsed: {
              type: 'UNPARSED',
            },
            revisionsState: 'CODE_AHEAD',
          },
          lastParseSuccess: null,
          lastSavedContents: null,
          versionNumber: 0,
        },
      },
      cardcomponents: {
        type: 'PROJECT_CONTENT_DIRECTORY',
        fullPath: '/src/cardcomponents',
        directory: {
          type: 'DIRECTORY',
        },
        children: {
          'decorations.js': {
            type: 'PROJECT_CONTENT_FILE',
            fullPath: '/src/cardcomponents/decorations.js',
            content: {
              type: 'TEXT_FILE',
              fileContents: {
                code: "import * as React from 'react'\nexport var DecorativeLines = (props) => {\n  const runCallback = (cb) => {\n    return cb()\n  }\n\n  return (\n    <div\n      style={{\n        display: 'flex',\n        flexDirection: 'column',\n        gap: 7,\n        paddingBottom: 10,\n        position: 'relative',\n        zIndex: 20,\n        ...props.style,\n      }}\n    >\n      {runCallback(() => {\n        const lines = []\n        for (var i = 0; i < 6; i++) {\n          lines.push(\n            <div\n              style={{\n                width: '100%',\n                height: '3px',\n                background: props.color\n                  ? props.color\n                  : 'var(--purple)',\n              }}\n              key={i}\n            />,\n          )\n        }\n        return lines\n      })}\n    </div>\n  )\n}\n\nexport var Checkerboard = (props) => {\n  return (\n    <div\n      style={{\n        background: props.color1\n          ? props.color1\n          : 'var(--white)',\n        backgroundImage:\n          props.color1 && props.color2\n            ? `repeating-linear-gradient(45deg, ${props.color2} 25%, transparent 25%, transparent 75%, ${props.color2} 75%, ${props.color2}), repeating-linear-gradient(45deg, ${props.color2} 25%, ${props.color1} 25%, ${props.color1} 75%, ${props.color2} 75%, ${props.color2})`\n            : 'repeating-linear-gradient(45deg, var(--orange) 25%, transparent 25%, transparent 75%, var(--orange) 75%, var(--orange)), repeating-linear-gradient(45deg, var(--orange) 25%, var(--white) 25%, var(--white) 75%, var(--orange) 75%, var(--orange))',\n        backgroundPosition: '0 0, 10px 10px',\n        backgroundSize: '20px 20px',\n        width: '100%',\n        height: 60,\n        ...props.style,\n      }}\n    >\n      {props.children}\n    </div>\n  )\n}\n",
                parsed: {
                  type: 'UNPARSED',
                },
                revisionsState: 'CODE_AHEAD',
              },
              lastParseSuccess: null,
              lastSavedContents: null,
              versionNumber: 0,
            },
          },
          'imagecontainer.js': {
            type: 'PROJECT_CONTENT_FILE',
            fullPath: '/src/cardcomponents/imagecontainer.js',
            content: {
              type: 'TEXT_FILE',
              fileContents: {
                code: "//import * as React from 'react'\n\nexport var ImageContainer = (props) => {\n  return (\n    <div\n      className='imageContainer'\n      style={{\n        width: 878,\n        height: 580,\n        background: 'pink',\n        overflow: 'hidden',\n        display: 'flex',\n        alignItems: 'center',\n        justifyContent: 'center',\n        zIndex: '0',\n        ...props.style,\n      }}\n    >\n      <img\n        src={props.imageUrl}\n        width='100%'\n        alt='awesome activity'\n      />\n    </div>\n  )\n}\n",
                parsed: {
                  type: 'UNPARSED',
                },
                revisionsState: 'CODE_AHEAD',
              },
              lastParseSuccess: null,
              lastSavedContents: null,
              versionNumber: 0,
            },
          },
        },
      },
      'card.js': {
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/src/card.js',
        content: {
          type: 'TEXT_FILE',
          fileContents: {
            code: "import * as React from 'react'\n/** @jsx jsx */\nimport { jsx } from '@emotion/react'\nimport '../public/globals.css'\nimport { useState, useCallback } from 'react'\nimport { CardComplete } from '/src/card-complete.js'\nimport { CardIncomplete } from '/src/card-incomplete.js'\n\nexport var Card = (props) => {\n  const [completed, setCompleted] = useState(false)\n  const toggleComplete = useCallback(\n    () => setCompleted((completed) => !completed),\n    [],\n  )\n\n  const color =\n    props.activity.id % 2\n      ? 'var(--yellow)'\n      : 'var(--orange)'\n\n  return (\n    <div\n      data-label='Activity Card'\n      onClick={toggleComplete}\n      style={{ overflow: 'hidden', ...props.style }}\n      css={{ '&:hover': { cursor: 'pointer' } }}\n    >\n      {!completed ? (\n        <CardComplete\n          activity={props.activity}\n          color={color}\n        />\n      ) : (\n        <CardIncomplete\n          activity={props.activity}\n          color={color}\n        />\n      )}\n    </div>\n  )\n}\n",
            parsed: {
              type: 'UNPARSED',
            },
            revisionsState: 'CODE_AHEAD',
          },
          lastParseSuccess: null,
          lastSavedContents: null,
          versionNumber: 0,
        },
      },
      'category-filters.js': {
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/src/category-filters.js',
        content: {
          type: 'TEXT_FILE',
          fileContents: {
            code: "import * as React from 'react'\nimport '../public/globals.css'\nimport {\n  CategoryItem,\n  ClearCategoriesButton,\n} from './category-item.js'\nimport { categories } from '/public/data.js'\n\nexport var CategoryFilters = (props) => {\n  return (\n    <div\n      className='category-container'\n      style={{\n        display: 'flex',\n        flexDirection: 'row',\n        alignItems: 'center',\n        padding: '0px 0px 0px 20px',\n        gap: 20,\n        width: '100%',\n        height: '69.5px',\n        overflowX: 'scroll',\n        ...props.style,\n      }}\n    >\n      <ClearCategoriesButton\n        style={{\n          borderRadizus: '50px',\n          padding: '0px 18px',\n          borderRadius: 22,\n        }}\n      />\n      {categories.map((category) => (\n        <CategoryItem\n          selected={false}\n          name={category}\n          style={{}}\n        />\n      ))}\n    </div>\n  )\n}\n",
            parsed: {
              type: 'UNPARSED',
            },
            revisionsState: 'CODE_AHEAD',
          },
          lastParseSuccess: null,
          lastSavedContents: null,
          versionNumber: 0,
        },
      },
      'category-item.js': {
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/src/category-item.js',
        content: {
          type: 'TEXT_FILE',
          fileContents: {
            code: "/** @jsx jsx */\nimport * as React from 'react'\nimport { jsx } from '@emotion/react'\nimport { useState } from 'react'\n\nexport var CategoryItem = (props) => {\n  const [selected, setSelected] = useState()\n\n  return (\n    <div\n      onClick={() => setSelected(!selected)}\n      css={{ '&:hover': { opacity: 0.7 } }}\n      style={{\n        display: 'flex',\n        flexDirection: 'row',\n        justifyContent: 'center',\n        alignItems: 'center',\n        height: '45px',\n        fontFamily: 'var(--secondary)',\n        fontSize: '16px',\n        fontWeight: '700',\n        textTransform: 'uppercase',\n        textAlign: 'center',\n        boxShadow: '0px 6px 1px rgba(0, 0, 0, 0.25)',\n        border: '4px solid var(--yellow)',\n        padding: '13px 27px',\n        borderRadius: 50,\n        backgroundColor: selected\n          ? 'var(--off-white)'\n          : 'var(--yellow)',\n        color: selected\n          ? 'var(--yellow)'\n          : 'var(--off-white)',\n        ...props.style,\n      }}\n    >\n      {props.name}\n    </div>\n  )\n}\n\nexport var ClearCategoriesButton = (props) => {\n  return (\n    <div\n      css={{ '&:hover': { opacity: 0.7 } }}\n      style={{\n        display: 'flex',\n        flexDirection: 'row',\n        justifyContent: 'center',\n        alignItems: 'center',\n        fontFamily: 'helvetica',\n        textTransform: 'uppercase',\n        textAlign: 'center',\n        boxShadow: '0px 6px 1px rgba(0, 0, 0, 0.25)',\n        border: '4px solid var(--yellow)',\n        backgroundColor: 'var(--off-white)',\n        color: 'var(--purple)',\n        fontSize: '4em',\n        fontWeight: '200',\n        overflow: 'hidden',\n        width: '45px',\n        height: '45px',\n        ...props.style,\n      }}\n    >\n      <svg\n        width='45'\n        height='45'\n        viewBox='0 0 45 45'\n        fill='none'\n        xmlns='http://www.w3.org/2000/svg'\n      >\n        <path\n          fill-rule='evenodd'\n          clip-rule='evenodd'\n          d='M22.4531 26.3826L-4.28696 53.1226L-7.5 50.7401L19.6553 23.5848L-4.54693 -0.617489L-1.33389 -3L22.4531 20.787L46.2401 -3L49.4532 -0.617489L25.2509 23.5848L52.4062 50.7401L49.1932 53.1226L22.4531 26.3826Z'\n          fill='var(--yellow)'\n        />\n      </svg>\n    </div>\n  )\n}\n",
            parsed: {
              type: 'UNPARSED',
            },
            revisionsState: 'CODE_AHEAD',
          },
          lastParseSuccess: null,
          lastSavedContents: null,
          versionNumber: 0,
        },
      },
      'index.js': {
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/src/index.js',
        content: {
          type: 'TEXT_FILE',
          fileContents: {
            code: "import * as React from 'react'\nimport * as ReactDOM from 'react-dom'\nimport { App } from '../src/app'\n\nconst root = document.getElementById('root')\nif (root != null) {\n  ReactDOM.render(<App />, root)\n}\n",
            parsed: {
              type: 'UNPARSED',
            },
            revisionsState: 'CODE_AHEAD',
          },
          lastParseSuccess: null,
          lastSavedContents: null,
          versionNumber: 0,
        },
      },
      'mood-board.js': {
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/src/mood-board.js',
        content: {
          type: 'TEXT_FILE',
          fileContents: {
            code: "import * as React from 'react'\nimport {\n  DecorativeLines,\n  Checkerboard,\n} from '/src/cardcomponents/decorations.js'\nimport { ImageContainer } from '/src/cardcomponents/imagecontainer.js'\nimport { Wonderer, Cryophile } from '/src/stickers.js'\nimport { Mixologist } from '/src/stickers.js'\nimport { Tropicologist } from '/src/stickers.js'\nimport { AquaMan } from '/src/stickers.js'\nimport { activities } from '/public/data.js'\nimport { Title } from '/src/title.js'\nimport { CategoryFilters } from '/src/category-filters.js'\nimport { Card } from '/src/card.js'\n\nexport var MoodBoard = () => {\n  return (\n    <div\n      style={{\n        width: '100%',\n        height: '100%',\n        background: 'var(--off-white)',\n        color: 'var(--purple)',\n        fontFamily: 'primary-basic',\n        position: 'relative',\n      }}\n    >\n      <Title\n        style={{\n          position: 'absolute',\n          left: 376,\n          top: 81,\n          width: 956,\n          height: 180,\n          zIndex: 100,\n        }}\n      />\n      <Card\n        activity={activities[2]}\n        style={{\n          position: 'absolute',\n          top: 1268,\n          left: 961,\n        }}\n      />\n      <Checkerboard\n        style={{\n          position: 'absolute',\n          height: 479,\n          width: 404,\n          left: 203,\n          top: 260,\n        }}\n        color1='var(--purple)'\n        color2='var(--orange)'\n      />\n      <Tropicologist\n        style={{\n          position: 'absolute',\n          left: 325,\n          top: 741,\n          width: 100,\n          height: 100,\n          zIndex: 100,\n        }}\n      />\n      <AquaMan\n        style={{\n          position: 'absolute',\n          left: 152,\n          top: 522,\n          width: 100,\n          height: 100,\n          zIndex: 100,\n        }}\n      />\n      <ImageContainer\n        imageUrl={activities[2].imageUrl}\n        style={{\n          position: 'absolute',\n          left: 647,\n          top: 521.5,\n        }}\n      />\n      <DecorativeLines\n        style={{\n          width: 1022,\n          zIndex: 100,\n          contain: 'layout',\n          height: 64,\n          left: 1439,\n          top: 940,\n          position: 'absolute',\n          transform: 'rotate(90deg)',\n        }}\n        color='var(--orange)'\n      />\n      <div\n        style={{\n          backgroundColor: 'lch(57 100 16)',\n          display: 'flex',\n          flexDirection: 'row',\n          gap: 50,\n          padding: 59,\n          contain: 'layout',\n          zIndex: 100,\n          top: 920,\n          left: 98,\n          position: 'absolute',\n          alignItems: 'center',\n          justifyContent: 'flex-start',\n          width: 786,\n          height: 482,\n          border: '0px solid #DD4A76FF',\n        }}\n      >\n        <span\n          style={{\n            wordBreak: 'break-word',\n            fontSize: '24px',\n            fontFamily: 'var(--secondary)',\n            lineHeight: '140%',\n            contain: 'layout',\n            width: 298,\n            display: 'block',\n            color: 'var(--purple)',\n            zIndex: 100,\n            height: 'max-content',\n          }}\n        >\n          {activities[3].description}\n        </span>\n        <img\n          data-aspect-ratio-locked\n          src={activities[3].imageUrl}\n          style={{\n            width: 240,\n            height: 180,\n            contain: 'layout',\n          }}\n          alt='cool activity'\n        />\n        <Wonderer\n          style={{\n            width: 122,\n            height: 122,\n            transform: 'rotate(-14deg)',\n            zIndex: 100,\n            contain: 'layout',\n          }}\n        />\n      </div>\n      <img\n        data-aspect-ratio-locked\n        src='./assets/lanterns.png'\n        alt='cool activity'\n        style={{\n          position: 'absolute',\n          width: 540,\n          height: 540,\n          top: 169,\n          left: 1705,\n        }}\n      />\n      <Mixologist\n        style={{\n          position: 'absolute',\n          left: 1654,\n          top: 1101,\n          width: 100,\n          height: 100,\n        }}\n      />\n      <Cryophile\n        style={{\n          position: 'absolute',\n          left: 1574,\n          top: 972,\n          width: 100,\n          height: 100,\n        }}\n      />\n      <Wonderer\n        style={{\n          position: 'absolute',\n          left: 1704,\n          top: 872,\n          width: 100,\n          height: 100,\n        }}\n      />\n      <span\n        style={{\n          position: 'absolute',\n          wordBreak: 'break-word',\n          fontSize: '44px',\n          height: 42,\n          color: 'var(--yellow)',\n          left: 753,\n          top: 285,\n          width: 872,\n        }}\n      >\n        {activities[2].name}\n      </span>\n      <CategoryFilters\n        style={{\n          position: 'absolute',\n          top: 330,\n          left: 84,\n          width: 1540,\n          height: 172,\n        }}\n      />\n    </div>\n  )\n}\n",
            parsed: {
              type: 'UNPARSED',
            },
            revisionsState: 'CODE_AHEAD',
          },
          lastParseSuccess: null,
          lastSavedContents: null,
          versionNumber: 0,
        },
      },
      'playground.js': {
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/src/playground.js',
        content: {
          type: 'TEXT_FILE',
          fileContents: {
            code: "import * as React from 'react'\nimport { View } from 'utopia-api'\nimport '../public/globals.css'\n\nexport var Playground = () => {\n  return (\n    <div\n      style={{\n        height: '100%',\n        width: '100%',\n        contain: 'layout',\n      }}\n    >\n      <div\n        style={{\n          height: 150,\n          position: 'absolute',\n          left: 154,\n          top: 134,\n        }}\n      >\n        <img\n          src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.png?raw=true'\n          alt='Utopia logo'\n          style={{ height: '100%' }}\n        />\n      </div>\n    </div>\n  )\n}\n",
            parsed: {
              type: 'UNPARSED',
            },
            revisionsState: 'CODE_AHEAD',
          },
          lastParseSuccess: null,
          lastSavedContents: null,
          versionNumber: 0,
        },
      },
      routes: {
        type: 'PROJECT_CONTENT_DIRECTORY',
        fullPath: '/src/routes',
        directory: {
          type: 'DIRECTORY',
        },
        children: {
          '_index.js': {
            type: 'PROJECT_CONTENT_FILE',
            fullPath: '/src/routes/_index.js',
            content: {
              type: 'TEXT_FILE',
              fileContents: {
                code: "import * as React from 'react'\nimport { Link } from '@remix-run/react'\nimport { Title } from '/src/title.js'\nimport { Card } from '/src/card.js'\nimport { activities } from '/public/data.js'\nimport { CategoryFilters } from '/src/category-filters.js'\nimport '/public/globals.css'\n\nexport default function Index() {\n  return (\n    <div\n      className='my-class'\n      style={{\n        width: '100%',\n        height: '100%',\n        background: 'var(--off-white)',\n        zIndex: '100',\n        display: 'flex',\n        flexDirection: 'column',\n        alignItems: 'center',\n        padding: '0px 0px 25px',\n        gap: 5,\n        boxShadow: '0px 2px 33px var(--yellow)',\n        transition: 'all 3s ease-out',\n      }}\n    >\n      <Title />\n      <div\n        style={{\n          background: '#dd4a76',\n          width: '80%',\n          height: 80,\n          display: 'flex',\n          flexDirection: 'row',\n          alignItems: 'center',\n          justifyContent: 'center',\n          borderRadius: 100,\n        }}\n      >\n        <Link\n          style={{\n            wordBreak: 'break-word',\n            color: 'var(--off-white)',\n            contain: 'layout',\n            width: 'max-content',\n            height: 'max-content',\n            fontWeight: 700,\n            fontFamily: 'var(--secondary)',\n            fontSize: '28px',\n            textTransform: 'uppercase',\n            textAlign: 'center',\n          }}\n          to='/posts'\n        >\n          Things I want to do\n        </Link>\n      </div>\n    </div>\n  )\n}\n",
                parsed: {
                  type: 'UNPARSED',
                },
                revisionsState: 'CODE_AHEAD',
              },
              lastParseSuccess: null,
              lastSavedContents: null,
              versionNumber: 0,
            },
          },
          'posts.$postId.js': {
            type: 'PROJECT_CONTENT_FILE',
            fullPath: '/src/routes/posts.$postId.js',
            content: {
              type: 'TEXT_FILE',
              fileContents: {
                code: "import React from 'react'\nimport { Link } from '@remix-run/react'\nimport { useParams } from '@remix-run/react'\nimport { json, useLoaderData } from 'react-router'\nimport { Title } from '/src/title.js'\nimport { activities } from '/public/data.js'\nimport { CategoryItem } from '/src/category-item.js'\n\nasync function wait(ms) {\n  return new Promise((res) => setTimeout(res, ms))\n}\n\nexport async function loader({ params }) {\n  // await wait(1000)\n  if (params.postId === '0') {\n    return json(activities[0])\n  }\n  if (params.postId === '1') {\n    return json(activities[1])\n  }\n  if (params.postId === '2') {\n    return json(activities[2])\n  }\n  if (params.postId === '3') {\n    return json(activities[3])\n  }\n  return json({ error: 'not found' })\n}\n\nexport default function PostForId() {\n  const {\n    name,\n    imageUrl,\n    rating,\n    date,\n    description,\n    categories,\n  } = useLoaderData()\n\n  return (\n    <div\n      className='my-class'\n      style={{\n        width: '100%',\n        height: '100%',\n        display: 'flex',\n        flexDirection: 'column',\n        backgroundColor: 'var(--off-white)',\n        alignItems: 'flex-start',\n        justifyContent: 'flex-start',\n        overflowY: 'scroll',\n      }}\n    >\n      <Title />\n      <span\n        style={{\n          width: '100%',\n          fontSize: '26px',\n          padding: 30,\n          fontFamily: 'primary-basic',\n          fontSize: '32px',\n          textAlign: 'left',\n          lineHeight: '1.2em',\n          color: 'var(--purple)',\n          backgroundColor: 'var(--yellow)',\n        }}\n      >\n        {name}\n      </span>\n\n      <div\n        style={{\n          height: 600,\n          width: '100%',\n          backgroundImage: `url(${imageUrl})`,\n          backgroundSize: '100%',\n          backgroundPosition: '50%',\n          display: 'flex',\n          alignItems: 'flex-end',\n          justifyContent: 'flex-end',\n        }}\n      >\n        <div\n          style={{\n            height: 'min-content',\n            display: 'flex',\n            justifyContent: 'center',\n            alignItems: 'center',\n            padding: 20,\n            fontFamily: 'var(--secondary)',\n            fontSize: '20px',\n            fontWeight: '600',\n            color: 'var(--purple)',\n            zIndex: 100,\n            mixBlendMode: 'screen',\n          }}\n        >\n          {date}\n        </div>\n      </div>\n      <div\n        style={{\n          width: '100%',\n        }}\n      >\n        <div\n          style={{\n            fontFamily: 'var(--secondary)',\n            fontSize: '18px',\n            textAlign: 'left',\n            color: 'var(--orange)',\n            padding: 30,\n          }}\n        >\n          {description}\n          <br />\n          <br />\n          <div\n            style={{\n              display: 'flex',\n              flexDirection: 'row',\n              gap: 5,\n            }}\n          >\n            {categories.map((category) => (\n              <div\n                style={{\n                  backgroundColor: 'var(--purple)',\n                  color: 'var(--orange)',\n                  padding: '4px 10px',\n                  borderRadius: 3,\n                }}\n              >\n                {category}\n              </div>\n            ))}\n          </div>\n        </div>\n      </div>\n      <div\n        style={{\n          position: 'absolute',\n          left: 30,\n          bottom: 30,\n        }}\n      >\n        <Link to='/posts'>\n          <CategoryItem name='Back' />\n        </Link>\n      </div>\n    </div>\n  )\n}\n",
                parsed: {
                  type: 'UNPARSED',
                },
                revisionsState: 'CODE_AHEAD',
              },
              lastParseSuccess: null,
              lastSavedContents: null,
              versionNumber: 0,
            },
          },
          'posts._index.js': {
            type: 'PROJECT_CONTENT_FILE',
            fullPath: '/src/routes/posts._index.js',
            content: {
              type: 'TEXT_FILE',
              fileContents: {
                code: "import React from 'react'\nimport { Link } from '@remix-run/react'\nimport { json, useLoaderData } from 'react-router'\nimport { Title } from '/src/title.js'\nimport { CategoryFilters } from '/src/category-filters.js'\nimport { activities } from '/public/data.js'\nimport { Card } from '/src/card.js'\n\nexport function loader() {\n  return json({\n    activities: activities,\n  })\n}\n\nexport default function Posts() {\n  const { activities } = useLoaderData()\n\n  return (\n    <div\n      className='my-class'\n      style={{\n        backgroundColor: 'var(--off-white)',\n        height: '100%',\n        transition: 'all 0.5s ease-out',\n      }}\n    >\n      <Title />\n      <CategoryFilters />\n      {activities.map(\n        ({\n          id,\n          name,\n          imageUrl,\n          rating,\n          date,\n          description,\n          categories,\n        }) => (\n          <div\n            style={{\n              width: '100%',\n              overflowY: 'scroll',\n              display: 'flex',\n              flexDirection: 'column',\n              gap: 0,\n              flexWrap: 'nowrap',\n              padding: '0px 0px',\n            }}\n          >\n            <Link to={`${id}`}>\n              <div\n                style={{\n                  width: 834,\n                  height: 267,\n                  display: 'flex',\n                  flexDirection: 'row',\n                  overflow: 'hidden',\n                  borderTop:\n                    id % 2\n                      ? '3px solid var(--yellow)'\n                      : '3px solid var(--orange)',\n                }}\n              >\n                <div\n                  style={{\n                    display: 'flex',\n                    flexDirection: 'column',\n                    justifyContent: 'space-between',\n                    padding: 20,\n                    color: 'var(--purple)',\n                    backgroundColor:\n                      id % 2\n                        ? 'var(--yellow)'\n                        : 'var(--orange)',\n                    gap: 20,\n                    width: 679,\n                    height: '100%',\n                  }}\n                >\n                  <div\n                    style={{\n                      fontFamily: 'primary-basic',\n                      fontSize: '28px',\n                      textAlign: 'left',\n                      lineHeight: '1.2em',\n                    }}\n                  >\n                    {name}\n                  </div>\n                  <div\n                    style={{\n                      fontFamily: 'var(--secondary)',\n                      fontSize: '12px',\n                      textAlign: 'left',\n                    }}\n                  >\n                    {description}\n                    <br />\n                    <br />\n                    <div\n                      style={{\n                        display: 'flex',\n                        flexDirection: 'row',\n                        gap: 5,\n                      }}\n                    >\n                      {categories.map((category) => (\n                        <div\n                          style={{\n                            backgroundColor:\n                              'var(--purple)',\n                            color:\n                              id % 2\n                                ? 'var(--yellow)'\n                                : 'var(--orange)',\n                            padding: '2px 6px',\n                            borderRadius: 3,\n                          }}\n                        >\n                          {category}\n                        </div>\n                      ))}\n                    </div>\n                  </div>\n                </div>\n                <div\n                  style={{\n                    height: '100%',\n                    width: '100%',\n                    backgroundImage: `url(${imageUrl})`,\n                    backgroundSize: '100%',\n                    backgroundPosition: '50%',\n                    display: 'flex',\n                    alignItems: 'flex-end',\n                    justifyContent: 'flex-end',\n                  }}\n                >\n                  <div\n                    style={{\n                      height: 'min-content',\n                      display: 'flex',\n                      justifyContent: 'center',\n                      alignItems: 'center',\n                      padding: 20,\n                      fontFamily: 'var(--secondary)',\n                      fontSize: '20px',\n                      fontWeight: '600',\n                      color: 'var(--purple)',\n                      zIndex: 100,\n                      mixBlendMode: 'screen',\n                    }}\n                  >\n                    {date}\n                  </div>\n                </div>\n              </div>\n            </Link>\n          </div>\n        ),\n      )}\n    </div>\n  )\n}\n",
                parsed: {
                  type: 'UNPARSED',
                },
                revisionsState: 'CODE_AHEAD',
              },
              lastParseSuccess: null,
              lastSavedContents: null,
              versionNumber: 0,
            },
          },
        },
      },
      'root.js': {
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/src/root.js',
        content: {
          type: 'TEXT_FILE',
          fileContents: {
            code: "// import * as React from 'react'\nimport { Outlet } from '@remix-run/react'\n\nexport default function App() {\n  return (\n    <div\n      className='my-class'\n      style={{\n        width: '100%',\n        height: '100%',\n        contain: 'layout',\n        transition: 'all 3s ease-out',\n      }}\n    >\n      <Outlet style={{ transition: 'all 3s ease-out' }} />\n    </div>\n  )\n}\n",
            parsed: {
              type: 'UNPARSED',
            },
            revisionsState: 'CODE_AHEAD',
          },
          lastParseSuccess: null,
          lastSavedContents: null,
          versionNumber: 0,
        },
      },
      'stickers.js': {
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/src/stickers.js',
        content: {
          type: 'TEXT_FILE',
          fileContents: {
            code: "//import * as React from 'react'\nimport {\n  Birb,\n  Drink,\n  PalmTree,\n  RiskMan,\n  Snowflake,\n} from './svgs.js'\n\nexport var AquaMan = (props) => {\n  return (\n    <div\n      style={{\n        border: '16px double var(--off-white)',\n        background: 'var(--off-white)',\n        backgroundImage:\n          'url(https://cdn.utopia.app/editor/sample-assets/flamingo.jpg)',\n        backgroundSize: '300px',\n        backgroundPosition: '5% 60%',\n        boxShadow: '0px 2px 4px 5px rgb(0, 0, 0, 0.12)',\n        width: 100,\n        height: 100,\n        ...props.style,\n      }}\n    />\n  )\n}\n\nexport var Wonderer = (props) => {\n  return (\n    <div\n      style={{\n        display: 'flex',\n        border: '3px solid var(--off-white)',\n        background: 'var(--orange)',\n        backgroundImage:\n          'url(https://cdn.utopia.app/editor/sample-assets/northernlights.jpg)',\n        backgroundSize: '250px',\n        backgroundPosition: '55% 30%',\n        flexDirection: 'column',\n        alignItems: 'center',\n        justifyContent: 'center',\n        color: 'var(--off-white)',\n        fontFamily: 'primary',\n        fontSize: '.6em',\n        textAlign: 'center',\n        boxShadow: '0px 2px 4px 5px rgb(0, 0, 0, 0.12)',\n        width: 100,\n        height: 100,\n        ...props.style,\n      }}\n    >\n      <div style={{ fontSize: '8em', marginTop: -5 }}>\n        7\n      </div>\n      <div style={{ marginTop: -5 }}>Wonders</div>\n    </div>\n  )\n}\n\nexport var BirbWatcher = (props) => {\n  return (\n    <div\n      style={{\n        display: 'flex',\n        border: '5px groove var(--off-white)',\n        backgroundImage:\n          'url(https://images.pexels.com/photos/3289880/pexels-photo-3289880.jpeg?auto=compress&cs=tinysrgb&w=1260&h=750&dpr=2)',\n        backgroundSize: 200,\n        boxShadow: '0px 2px 4px 5px rgb(0, 0, 0, 0.12)',\n        height: 100,\n        width: 113,\n        ...props.style,\n      }}\n    >\n      <Birb color='var(--off-white)' />\n      <div\n        style={{\n          color: 'var(--off-white)',\n          textAlign: 'right',\n          width: '51.11517289066%',\n          alignSelf: 'end',\n          height: 50,\n          marginLeft: -10,\n        }}\n      >\n        Birb Watcher\n      </div>\n    </div>\n  )\n}\n\nexport var Mixologist = (props) => {\n  return (\n    <div\n      style={{\n        border: '2px solid var(--off-white)',\n        backgroundColor: 'purple',\n        backgroundImage:\n          'url(https://images.pexels.com/photos/239466/pexels-photo-239466.jpeg?auto=compress&cs=tinysrgb&w=1260&h=750&dpr=2)',\n        backgroundSize: '500px',\n        backgroundPosition: '45% 50%',\n        backgroundBlendMode: 'darken',\n        boxShadow: '0px 2px 4px 5px rgb(0, 0, 0, 0.12)',\n        padding: '5px 0',\n        width: 100,\n        height: 100,\n        ...props.style,\n      }}\n    >\n      <Drink color='var(--off-white)' />\n      <div\n        style={{\n          textAlign: 'center',\n          color: 'var(--off-white)',\n          fontFamily: 'Futura',\n          fontSize: '.8em',\n          padding: 5,\n        }}\n      >\n        MIXOLOGIST\n      </div>\n    </div>\n  )\n}\n\nexport var Tropicologist = (props) => {\n  return (\n    <div\n      style={{\n        border: '6px double var(--off-white)',\n        backgroundImage:\n          'url(https://images.pexels.com/photos/2237348/pexels-photo-2237348.jpeg?auto=compress&cs=tinysrgb&w=1260&h=750&dpr=2)',\n        backgroundColor: '#5C885C',\n        backgroundSize: '100px',\n        backgroundPosition: '45% 50%',\n        backgroundBlendMode: 'darken',\n        boxShadow: '0px 2px 4px 5px rgb(0, 0, 0, 0.12)',\n        paddingTop: 5,\n        width: 100,\n        height: 100,\n        display: 'flex',\n        alignItems: 'center',\n        justifyContent: 'center',\n        ...props.style,\n      }}\n    >\n      <PalmTree color='var(--off-white)' />\n    </div>\n  )\n}\n\nexport var RiskTaker = (props) => {\n  return (\n    <div\n      style={{\n        border: '2px solid var(--off-white)',\n        background: 'var(--purple)',\n        backgroundImage:\n          'url(https://images.pexels.com/photos/73873/star-clusters-rosette-nebula-star-galaxies-73873.jpeg?auto=compress&cs=tinysrgb&w=1260&h=750&dpr=2)',\n        backgroundSize: '120px',\n        backgroundBlendMode: 'linear-burn',\n        boxShadow: '0px 2px 4px 5px rgb(0, 0, 0, 0.12)',\n        width: 75,\n        height: 75,\n        overflow: 'hidden',\n        alignSelf: 'center',\n        margin: '0 12px',\n        ...props.style,\n      }}\n    >\n      <RiskMan\n        color='var(--off-white)'\n        style={{ transform: 'rotate(-45deg)' }}\n      />\n    </div>\n  )\n}\n\nexport var Cryophile = (props) => {\n  return (\n    <div\n      style={{\n        border: '2px solid var(--off-white)',\n        backgroundImage:\n          'url(https://images.pexels.com/photos/1717212/pexels-photo-1717212.jpeg?auto=compress&cs=tinysrgb&w=1260&h=750&dpr=2)',\n        backgroundSize: '220%',\n        backgroundPosition: '0% 12%',\n        boxShadow: '0px 2px 4px 5px rgb(0, 0, 0, 0.12)',\n        width: 100,\n        height: 100,\n        ...props.style,\n      }}\n    >\n      <Snowflake color='var(--off-white)' width={4} />\n    </div>\n  )\n}\n",
            parsed: {
              type: 'UNPARSED',
            },
            revisionsState: 'CODE_AHEAD',
          },
          lastParseSuccess: null,
          lastSavedContents: null,
          versionNumber: 0,
        },
      },
      'svgs.js': {
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/src/svgs.js',
        content: {
          type: 'TEXT_FILE',
          fileContents: {
            code: "import * as React from 'react'\nexport var Birb = (props) => {\n  return (\n    <div style={{ ...props.style }}>\n      <svg\n        width='50'\n        height='80'\n        viewBox='0 270 350 10'\n        fill='none'\n        xmlns='http://www.w3.org/2000/svg'\n      >\n        <path\n          fill-rule='evenodd'\n          clip-rule='evenodd'\n          d='M266 24C309.6 24 359.833 43.3333 379.5 53H283H221C252.2 58.6 248 91.6667 242 107.5C244 139.5 202.833 167.833 182 176.5C175.436 207.632 159.089 234.998 140.742 256.969L151.474 330.137L151.508 330.37L151.498 330.604L146.664 446.612L193.502 426.208L195.498 430.792L155.998 448H205.5V453H152.868L185.306 472.868L182.694 477.132L143.176 452.927L115.238 449.986L115.762 445.014L141.613 447.735L146.492 330.63L136.427 262C129.125 270.286 121.609 277.722 114.347 284.211L107.497 355.655L104.072 476H157V481H118.19L164.233 495.11L162.767 499.89L107.944 483.09L130.519 514.026L126.481 516.974L100.778 481.752L68.206 499.69L65.794 495.31L99.0417 477L102.501 355.429L102.503 355.345L102.511 355.261L108.868 288.973C98.6777 297.583 89.2589 304.189 82 308.5C59.2 367.3 25.8333 413.667 12 429.5C9.41869 418.437 14.8547 395.476 18.5925 382.946C15.6316 390.241 7.60528 398.128 3.5 401.5C2.00232 393.263 7.68937 370.364 11.2934 357.9C8.90309 364.199 2.96982 368.515 0 370C42.4 114 121 56.6667 155 60C170.6 -26.8 235.5 -0.166654 266 24Z'\n          fill={props.color}\n        />\n      </svg>\n    </div>\n  )\n}\n\nexport var Drink = (props) => {\n  return (\n    <div style={{ ...props.style }}>\n      <svg\n        width='53'\n        height='64'\n        viewBox='0 0 40 500'\n        fill='none'\n        xmlns='http://www.w3.org/2000/svg'\n      >\n        <path\n          fill-rule='evenodd'\n          clip-rule='evenodd'\n          d='M43.9873 0L28 8.27087L91.3643 130.75H0.871094L179 327.606V507.145C145.733 508.291 120 516.072 120 525.5C120 535.717 150.221 544 187.5 544C224.779 544 255 535.717 255 525.5C255 516.166 229.781 508.447 197 507.182V326.501L329.31 180.282C339.595 186.452 351.633 190 364.5 190C402.331 190 433 159.332 433 121.5C433 83.6685 402.331 53 364.5 53C326.669 53 296 83.6685 296 121.5C296 124.637 296.211 127.725 296.619 130.75H111.63L43.9873 0Z'\n          fill={props.color}\n        />\n      </svg>\n    </div>\n  )\n}\n\nexport var PalmTree = (props) => {\n  return (\n    <div style={{ ...props.style }}>\n      <svg\n        width='58'\n        height='78'\n        viewBox='0 0 58 78'\n        fill='none'\n        xmlns='http://www.w3.org/2000/svg'\n      >\n        <path\n          d='M24.0597 76.0408C19.6645 67.5422 14.1725 46.1387 25.7121 23.3161C24.7592 23.935 21.0159 28.4091 16.0849 29.226C15.4812 29.3395 13.8242 29.6195 13.1936 30.6803C12.1451 32.4439 10.1435 31.9695 8.90436 32.1036C7.95119 32.2067 6.32444 32.6729 5.53649 34.9502C5.06543 36.3117 3.43952 37.5741 2.16864 37.178C0.580025 36.683 0.760087 32.8508 1.81918 30.6803C2.4231 29.4426 7.63346 19.6341 17.324 20.1291C20.7239 20.3028 25.0131 21.0265 26.0296 20.1291C24.3562 20.2735 20.641 20.0116 19.1668 17.8085C17.6925 15.6055 15.4815 16.4182 14.5916 17.3753C13.882 18.1386 11.8464 19.1019 9.38093 16.8493C6.91541 14.5968 4.61538 15.6071 3.50308 16.0758C2.86763 16.3435 0.732547 16.5399 2.86763 12.8269C5.00272 9.11389 12.5687 8.0825 16.0849 8.03093C18.8176 7.99085 22.9286 9.52232 24.6316 13.5076C25.4646 15.4569 26.8178 16.5028 27.9362 16.8493C26.8665 14.8691 26.0296 12.5175 25.2988 8.95918C24.7866 6.46517 22.2054 6.51478 20.9463 4.56546C20.1202 3.28653 19.425 0.850994 23.4878 1.00716C28.3175 1.19281 31.3991 2.8946 32.384 5.46277C33.369 8.03093 33.5926 9.39238 33.083 11.1251C31.3994 16.8493 29.3977 19.6031 38.103 14.7143C44.0009 11.4022 47.0768 11.8059 50.9707 12.8269C55.1011 13.9099 56.2239 15.8592 56.6897 17.3753C57.1936 19.0152 57.3379 21.1873 55.1011 20.717C52.8644 20.2467 50.3565 20.5211 49.3821 20.717C48.2069 20.9534 46.9802 21.0203 44.3621 20.1291C41.7441 19.238 34.0365 20.1291 32.003 20.717C35.4665 22.6973 49.0962 25.4635 51.7651 40.365C52.0298 41.943 51.9239 45.062 49.3821 44.9134C46.8404 44.7649 45.8025 42.335 45.6012 41.1386C45.5165 40.3856 44.6227 38.7622 41.725 38.2919C38.103 37.704 40.6618 34.0285 38.103 33.0628C35.5615 32.1036 29.9381 22.6354 29.3977 22.0166C25.4686 32.743 20.5457 60.341 34.0679 77'\n          stroke={props.color}\n          stroke-width='2'\n        />\n      </svg>\n    </div>\n  )\n}\n\nexport var RiskMan = (props) => {\n  return (\n    <div style={{ ...props.style }}>\n      <svg\n        width='71'\n        height='110'\n        viewBox='15 -20 41 244'\n        fill={props.color}\n        xmlns='http://www.w3.org/2000/svg'\n      >\n        <path\n          d='M48.5027 87.7844C45.9464 75.3731 52.6111 67.7074 56.2629 65.4259V45.3488C47.133 43.1586 44.851 39.2648 44.8512 37.5917C39.6777 35.4623 30.2459 30.7473 27.0505 32.1162C25.3722 32.8351 20.6598 28.3137 17.4644 26.1843C14.9083 19.6136 19.8969 19.9482 22.94 19.7961L27.9613 28.4658L43.3708 28.7471L45.8164 31.2036H50.7854H66.3059V25.728C61.1932 21.3475 65.0886 16.2979 67.6753 14.3206C75.3442 9.94012 79.696 16.1458 80.9133 19.7961L77.7179 31.2036C98.5335 31.2036 104.042 16.9063 104.194 9.7576C99.4465 1.72677 106.172 0.631659 110.128 1.08796C107.772 18.954 106.957 22.2184 101.25 29.5632C95.9912 31.1873 91.4679 36.092 88.217 41.6984C77.9918 41.6984 72.3923 61.7755 70.8707 71.814C75.9833 71.449 84.5652 75.0081 88.217 76.8333C95.8859 74.278 97.1945 83.0694 96.8902 87.7844C105.655 101.656 107.846 108.166 107.846 109.687L113.78 106.036L116.975 100.104C122.453 100.835 119.258 108.318 116.975 111.968C114.784 113.063 111.498 119.117 110.128 122.007C106.111 122.372 102.368 115.466 100.999 111.968H96.8902C95.5229 106.036 96.1294 104.515 94.6078 101.473C94.6078 101.473 89.1278 98.7356 88.217 94.1726C87.3062 89.6096 85.326 87.7844 83.6522 87.7844H64.4799C59.7325 99.1006 46.3728 108.622 40.2863 111.968C20.5663 125.475 14.48 125.903 14.0235 124.23C13.8895 123.739 11.0736 122.007 8.79123 126.113L6.50881 133.414L3.31344 142.996C0.54053 143.303 1.03743 123.989 1.03084 123.832C0.98129 121.782 10.8395 120.805 11.9833 119.269C13.6162 117.075 12.1641 113.264 12.8971 113.337H20.2009C25.6786 108.227 32.5259 104.82 35.2648 103.755C37.0907 95.359 44.8509 89.6096 48.5027 87.7844Z'\n          stroke={props.color}\n          stroke-width='2'\n        />\n      </svg>\n    </div>\n  )\n}\n\nexport var Snowflake = (props) => {\n  return (\n    <div style={{ ...props.style }}>\n      <svg\n        width='74'\n        height='74'\n        viewBox='-13 0 100 72'\n        fill='none'\n        xmlns='http://www.w3.org/2000/svg'\n      >\n        <path\n          d='M54 2C54 0.895431 53.1046 0 52 0C50.8954 0 50 0.895431 50 2H54ZM50 52V54H54V52H50ZM50 2V52H54V2H50Z'\n          fill={props.color}\n        />\n        <path\n          d='M41.8672 5.88258L51.9998 15.5417L62.1323 5.88258'\n          stroke={props.color}\n          stroke-width={props.width}\n          stroke-linecap='round'\n        />\n        <path\n          d='M41.8672 20.5606L51.9998 30.2197L62.1323 20.5606'\n          stroke={props.color}\n          stroke-width={props.width}\n          stroke-linecap='round'\n        />\n        <path\n          d='M54 102C54 103.105 53.1046 104 52 104C50.8954 104 50 103.105 50 102H54ZM50 52V50H54V52H50ZM50 102V52H54V102H50Z'\n          fill={props.color}\n        />\n        <path\n          d='M41.8672 98.1174L51.9998 88.4583L62.1323 98.1174'\n          stroke={props.color}\n          stroke-width={props.width}\n          stroke-linecap='round'\n        />\n        <path\n          d='M41.8672 83.4394L51.9998 73.7803L62.1323 83.4394'\n          stroke={props.color}\n          stroke-width={props.width}\n          stroke-linecap='round'\n        />\n        <path\n          d='M2 50C0.895431 50 0 50.8954 0 52C0 53.1046 0.895431 54 2 54L2 50ZM52 54H54V50H52V54ZM2 54L52 54V50L2 50L2 54Z'\n          fill={props.color}\n        />\n        <path\n          d='M5.88281 62.1326L15.5419 52L5.88281 41.8674'\n          stroke={props.color}\n          stroke-width={props.width}\n          stroke-linecap='round'\n        />\n        <path\n          d='M20.561 62.1326L30.2201 52L20.561 41.8674'\n          stroke={props.color}\n          stroke-width={props.width}\n          stroke-linecap='round'\n        />\n        <path\n          d='M102 50C103.105 50 104 50.8954 104 52C104 53.1046 103.105 54 102 54V50ZM52 54H50V50H52V54ZM102 54L52 54V50L102 50V54Z'\n          fill={props.color}\n        />\n        <path\n          d='M98.1172 62.1326L88.4581 52L98.1172 41.8674'\n          stroke={props.color}\n          stroke-width={props.width}\n          stroke-linecap='round'\n        />\n        <path\n          d='M83.439 62.1326L73.7799 52L83.439 41.8674'\n          stroke={props.color}\n          stroke-width={props.width}\n          stroke-linecap='round'\n        />\n        <path\n          d='M18.2055 15.0952C17.4276 14.3111 16.1612 14.306 15.3771 15.084C14.5929 15.8619 14.5879 17.1282 15.3658 17.9124L18.2055 15.0952ZM50.5799 53.4084L51.9884 54.8282L54.8281 52.0111L53.4196 50.5913L50.5799 53.4084ZM15.3658 17.9124L50.5799 53.4084L53.4196 50.5913L18.2055 15.0952L15.3658 17.9124Z'\n          fill={props.color}\n        />\n        <path\n          d='M12.3271 26.3963L26.3232 26.1173L26.7138 12.1239'\n          stroke={props.color}\n          stroke-width={props.width}\n          stroke-linecap='round'\n        />\n        <path\n          d='M22.6646 36.8166L36.6606 36.5376L37.0512 22.5442'\n          stroke={props.color}\n          stroke-width={props.width}\n          stroke-linecap='round'\n        />\n        <path\n          d='M88.6342 86.0873C89.4121 86.8715 89.4071 88.1378 88.6229 88.9157C87.8388 89.6937 86.5724 89.6886 85.7945 88.9045L88.6342 86.0873ZM50.5804 53.4084L49.1719 51.9886L52.0116 49.1715L53.4201 50.5913L50.5804 53.4084ZM85.7945 88.9045L50.5804 53.4084L53.4201 50.5913L88.6342 86.0873L85.7945 88.9045Z'\n          fill={props.color}\n        />\n        <path\n          d='M77.2866 91.8757L77.6772 77.8824L91.6733 77.6034'\n          stroke={props.color}\n          stroke-width={props.width}\n          stroke-linecap='round'\n        />\n        <path\n          d='M66.9487 81.4555L67.3393 67.4621L81.3354 67.1831'\n          stroke={props.color}\n          stroke-width={props.width}\n          stroke-linecap='round'\n        />\n        <path\n          d='M15.0953 85.7942C14.3112 86.5722 14.3061 87.8385 15.0841 88.6226C15.862 89.4068 17.1283 89.4119 17.9125 88.6339L15.0953 85.7942ZM53.4085 53.4198L54.8283 52.0113L52.0112 49.1716L50.5914 50.5802L53.4085 53.4198ZM17.9125 88.6339L53.4085 53.4198L50.5914 50.5802L15.0953 85.7942L17.9125 88.6339Z'\n          fill={props.color}\n        />\n        <path\n          d='M26.3965 91.673L26.1175 77.6769L12.1241 77.2863'\n          stroke={props.color}\n          stroke-width={props.width}\n          stroke-linecap='round'\n        />\n        <path\n          d='M36.8164 81.3355L36.5374 67.3394L22.544 66.9488'\n          stroke={props.color}\n          stroke-width={props.width}\n          stroke-linecap='round'\n        />\n        <path\n          d='M86.0875 15.3661C86.8717 14.5881 88.138 14.5932 88.9159 15.3774C89.6939 16.1615 89.6888 17.4278 88.9047 18.2058L86.0875 15.3661ZM53.4086 53.4198L51.9888 54.8284L49.1717 51.9887L50.5915 50.5802L53.4086 53.4198ZM88.9047 18.2058L53.4086 53.4198L50.5915 50.5802L86.0875 15.3661L88.9047 18.2058Z'\n          fill={props.color}\n        />\n        <path\n          d='M91.876 26.7137L77.8826 26.3231L77.6036 12.327'\n          stroke={props.color}\n          stroke-width={props.width}\n          stroke-linecap='round'\n        />\n        <path\n          d='M81.4556 37.0511L67.4622 36.6605L67.1832 22.6645'\n          stroke={props.color}\n          stroke-width={props.width}\n          stroke-linecap='round'\n        />\n      </svg>\n    </div>\n  )\n}\n\nexport var Foo = (props) => {\n  return <div style={{ ...props.style }} />\n}\n",
            parsed: {
              type: 'UNPARSED',
            },
            revisionsState: 'CODE_AHEAD',
          },
          lastParseSuccess: null,
          lastSavedContents: null,
          versionNumber: 0,
        },
      },
      'title.js': {
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/src/title.js',
        content: {
          type: 'TEXT_FILE',
          fileContents: {
            code: "export var Title = (props) => {\n  return (\n    <div\n      style={{\n        width: '100%',\n        zIndex: 100,\n        display: 'flex',\n        flexDirection: 'column',\n        alignItems: 'center',\n        padding: '44px 0px 25px',\n        gap: '26px',\n        ...props.style,\n      }}\n    >\n      <div style={{ width: '130%' }}>\n        <div\n          style={{\n            color: 'var(--orange)',\n            fontFamily: 'primary',\n            textAlign: 'center',\n            fontWeight: 400,\n            fontStyle: 'normal',\n            border: '0px solid rgb(255, 174, 52, 1)',\n            fontSize: '72px',\n            lineHeight: '125%',\n            textShadow:\n              '0px 1px 1px #64646425, 0px 8px 1px rgba(0, 0, 0, 0.35)',\n          }}\n        >\n          BBeffore I Go\n        </div>\n      </div>\n    </div>\n  )\n}\n",
            parsed: {
              type: 'UNPARSED',
            },
            revisionsState: 'CODE_AHEAD',
          },
          lastParseSuccess: null,
          lastSavedContents: null,
          versionNumber: 0,
        },
      },
      'utils.js': {
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/src/utils.js',
        content: {
          type: 'TEXT_FILE',
          fileContents: {
            code: "import * as React from 'react'\n\nexport function FlexRow({ children, style, ...props }) {\n  return (\n    <div\n      {...props}\n      style={{\n        position: 'relative',\n        display: 'flex',\n        flexDirection: 'row',\n        ...style,\n      }}\n    >\n      {children}\n    </div>\n  )\n}\n\nexport function FlexCol({ children, style, ...props }) {\n  return (\n    <div\n      {...props}\n      style={{\n        position: 'relative',\n        display: 'flex',\n        flexDirection: 'column',\n        ...style,\n      }}\n    >\n      {children}\n    </div>\n  )\n}\n\nexport function TwoColumnGrid({\n  children,\n  style,\n  ...props\n}) {\n  return (\n    <div\n      {...props}\n      style={{\n        position: 'relative',\n        display: 'grid',\n        gridTemplateColumns: 'repeat(2, 1fr)',\n        ...style,\n      }}\n    >\n      {children}\n    </div>\n  )\n}\n\nexport function ThreeColumnGrid({\n  children,\n  style,\n  ...props\n}) {\n  return (\n    <div\n      {...props}\n      style={{\n        position: 'relative',\n        display: 'grid',\n        gridTemplateColumns: '1fr 1fr 1fr',\n        width: '100%',\n        hieght: '100%',\n        ...style,\n      }}\n    >\n      {children}\n    </div>\n  )\n}\n",
            parsed: {
              type: 'UNPARSED',
            },
            revisionsState: 'CODE_AHEAD',
          },
          lastParseSuccess: null,
          lastSavedContents: null,
          versionNumber: 0,
        },
      },
    },
  },
  'remix.config.js': {
    type: 'PROJECT_CONTENT_FILE',
    fullPath: '/remix.config.js',
    content: {
      type: 'TEXT_FILE',
      fileContents: {
        code: "/** @type {import('@remix-run/dev').AppConfig} */\nmodule.exports = {\n  appDirectory: 'src',\n  assetsBuildDirectory: 'public/build',\n  ignoredRouteFiles: ['**/.*'],\n  publicPath: '/build/',\n  routes(defineRoutes) {\n    return defineRoutes((route) => {\n      route('/somewhere/cool/*', 'catchall.tsx')\n    })\n  },\n  serverBuildPath: 'build/index.js',\n}\n",
        parsed: {
          type: 'UNPARSED',
        },
        revisionsState: 'CODE_AHEAD',
      },
      lastParseSuccess: null,
      lastSavedContents: null,
      versionNumber: 0,
    },
  },
  utopia: {
    type: 'PROJECT_CONTENT_DIRECTORY',
    fullPath: '/utopia',
    directory: {
      type: 'DIRECTORY',
    },
    children: {
      'storyboard.js': {
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/utopia/storyboard.js',
        content: {
          type: 'TEXT_FILE',
          fileContents: {
            code: "import * as React from 'react'\nimport { Scene, Storyboard, RemixScene } from 'utopia-api'\nimport { App } from '/src/app.js'\nimport { Playground } from '/src/playground.js'\nimport Index from '/src/routes/_index'\nimport { Group } from 'utopia-api'\nimport { MoodBoard } from '/src/mood-board.js'\n\nexport var storyboard = (\n  <Storyboard>\n    <RemixScene\n      className='my-class'\n      style={{\n        position: 'absolute',\n        width: 834,\n        height: 1328,\n        left: 8,\n        top: -24,\n        overflow: 'hidden',\n      }}\n      data-label='Mood Board'\n    />\n    <RemixScene\n      className='my-class'\n      style={{\n        position: 'absolute',\n        width: 834,\n        height: 1328,\n        left: 916,\n        top: -24,\n        overflow: 'hidden',\n      }}\n      data-label='Mood Board'\n    />\n    <Scene\n      style={{\n        position: 'absolute',\n        left: -2464,\n        width: 2368,\n        height: 1656,\n        top: -24,\n      }}\n      data-label='Mood Board'\n    >\n      <MoodBoard />\n    </Scene>\n  </Storyboard>\n)\n",
            parsed: {
              type: 'UNPARSED',
            },
            revisionsState: 'CODE_AHEAD',
          },
          lastParseSuccess: null,
          lastSavedContents: null,
          versionNumber: 0,
        },
      },
    },
  },
}
