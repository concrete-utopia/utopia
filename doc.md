```jsx
return (
  <Storyboard data-uid='storyboard1'>
    <Scene data-uid='scene1'>
      <App data-uid='app1' />
    </Scene>
  </Storyboard>
)
```

- Storyboard
  - data-uid= '**storyboard1**'
- Scene
  - data-uid= '**scene1**'
- App
  - data-uid= '**app1**'

.
Storyboard, being the root element, gets a special `data-paths` called `data-utopia-root-element-path`. Which just contains it's uid. In this case, `storyboard1`

In same render function: each child gets a compounded data-paths with its parent

A forward-slash ( `/` ), is added between the parent/child of each node in the render function.

So the data-uids in the one render function become data-paths

- Storyboard
  - data-uid= '**storyboard1**'
  - data-utopia-root-element-path= 'storyboard1'
- Scene
  - data-uid= '**scene1**'
  - data-paths= '**storyboard1/scene1**'
- App
  - data-uid= '**parent app1**'
  - data-paths= '**storyboard1/scene1/app1:parent storyboard1/scene1/app1**'
