import * as React from 'react'
import { Canvas, useFrame, useThree } from 'react-three-fiber'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { useEditorState } from '../editor/store/store-hook'

export const OutlineControlR3f = (props: any) => {
  // This reference will give us direct access to the mesh
  const mesh = React.useRef()
  // Set up state for the hovered and active state
  const [hovered, setHover] = React.useState(false)
  const [active, setActive] = React.useState(false)
  // // Rotate mesh every frame, this is outside of React without overhead
  // useFrame(
  //   () =>
  //     (((mesh as any).current as any).rotation.x = ((mesh as any)
  //       .current as any).rotation.y += 0.01),
  // )
  // const { viewport } = useThree()

  const outlineSize = 1
  return (
    <>
      <mesh
        position={[props.frame.x, props.frame.y, 0]}
        ref={mesh}
        scale={[1, 1, 1]}
        onClick={(e) => setActive(!active)}
        onPointerOver={(e) => setHover(true)}
        onPointerOut={(e) => setHover(false)}
      >
        <boxBufferGeometry args={[props.frame.width, props.frame.height, 0]} />
        <meshStandardMaterial color={hovered ? 'black' : 'blue'} />
      </mesh>
      <mesh
        position={[props.frame.x, props.frame.y, 0]}
        onClick={(e) => setActive(!active)}
        onPointerOver={(e) => setHover(true)}
        onPointerOut={(e) => setHover(false)}
      >
        <boxBufferGeometry args={[outlineSize, props.frame.height, 0]} />
        <meshStandardMaterial color={hovered ? 'hotpink' : 'orange'} />
      </mesh>
      <mesh
        position={[props.frame.x, props.frame.y, 0]}
        onClick={(e) => setActive(!active)}
        onPointerOver={(e) => setHover(true)}
        onPointerOut={(e) => setHover(false)}
      >
        <boxBufferGeometry args={[props.frame.width, outlineSize, 0]} />
        <meshStandardMaterial color={hovered ? 'hotpink' : 'orange'} />
      </mesh>
      <mesh
        position={[props.frame.x + props.frame.width, props.frame.y, 0]}
        onClick={(e) => setActive(!active)}
        onPointerOver={(e) => setHover(true)}
        onPointerOut={(e) => setHover(false)}
      >
        <boxBufferGeometry args={[outlineSize, props.frame.height, 0]} />
        <meshStandardMaterial color={hovered ? 'hotpink' : 'orange'} />
      </mesh>
      <mesh
        position={[props.frame.x, props.frame.y + props.frame.height, 0]}
        onClick={(e) => setActive(!active)}
        onPointerOver={(e) => setHover(true)}
        onPointerOut={(e) => setHover(false)}
      >
        <boxBufferGeometry args={[props.frame.width, outlineSize, 0]} />
        <meshStandardMaterial color={hovered ? 'hotpink' : 'orange'} />
      </mesh>
    </>
  )
}

export const Wireframe3d = () => {
  const { componentMetadata } = useEditorState((store) => ({
    componentMetadata: store.editor.jsxMetadataKILLME,
  }))
  const allElements = MetadataUtils.getAllPaths(componentMetadata)
  return (
    <Canvas colorManagement>
      <ambientLight intensity={0.2} />
      <spotLight position={[10, 10, 10]} angle={0.15} penumbra={1} />
      <pointLight position={[-10, -10, -10]} />
      {allElements.map((path, i) => {
        const frame = MetadataUtils.getFrameInCanvasCoords(path, componentMetadata)
        const updatedFrame = {
          x: frame?.x || 0,
          y: frame?.y || 0,
          width: frame?.width || 0,
          height: frame?.height || 0,
        }
        return <OutlineControlR3f key={i} path={path} frame={updatedFrame} />
      })}
    </Canvas>
  )
}
