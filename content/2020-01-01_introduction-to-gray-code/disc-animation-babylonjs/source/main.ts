import {
  Animation,
  ArcRotateCamera,
  Axis,
  Color3,
  Color4,
  CSG,
  Engine,
  GlowLayer,
  HemisphericLight,
  MeshBuilder,
  PBRMaterial,
  Scene,
  VideoRecorder,
  Space,
  SpotLight,
  StandardMaterial,
  Tools,
  TransformNode,
  Vector3,
  Viewport,
} from "@babylonjs/core"
import "@babylonjs/inspector"
import { animations } from "./disc_animations.json"

interface Canvas extends HTMLElement {
  width: number
  height: number
}

const canvas = <HTMLCanvasElement>document.getElementById("renderCanvas")

if (!canvas) {
  throw new Error("Canvas not found")
}

// Set size of canvas to full HD
canvas.width = 1920 / 2
canvas.height = 1080 / 2

let sceneToRender: Scene | null = null
let engine: Engine | null = null

function startRenderLoop(theEngine: Engine, canvas: HTMLCanvasElement) {
  theEngine.runRenderLoop(() => {
    if (sceneToRender && sceneToRender.activeCamera) {
      sceneToRender.render()
    }
  })
}

function createDefaultEngine() {
  return new Engine(canvas, true, {
    preserveDrawingBuffer: true,
    stencil: true,
    disableWebGL2Support: false,
  })
}

async function runAnimation(scene: Scene, newMesh: TransformNode) {
  const frameRate = 60
  newMesh.animations = animations.map((animation: Record<string, unknown>) =>
    Animation.Parse(animation)
  )

  scene.beginAnimation(newMesh, 0, 3 * frameRate, true)
}

function recordAnimation() {
  const duration = 2 // seconds
  const codec = "opus" // Had the best visual results during testing

  if (!engine) {
    throw new Error("Engine not found")
  }

  const recorder = new VideoRecorder(engine, {
    fps: 60,
    // @ts-expect-error duration does not exist in the type
    duration: 2,
    mimeType: `video/webm;codecs=${codec}`,
  })
  recorder.startRecording(`a-video-${codec}.webm`, duration)
}

function createRingSection(
  scene: Scene,
  position: number,
  material: StandardMaterial,
  arcStartRad: number,
  arcEndRad: number
) {
  const outerCylinder = MeshBuilder.CreateCylinder(
    "outer",
    {
      diameter: position * 2 + 1,
      height: 0.5,
      tessellation: 60,
      arc: arcEndRad - arcStartRad,
      enclose: true,
    },
    scene
  )
  const outerCSG = CSG.FromMesh(outerCylinder)

  const innerCylinder = MeshBuilder.CreateCylinder(
    "inner",
    { diameter: (position - 1) * 2 + 1, height: 1, tessellation: 60 },
    scene
  )
  const innerCSG = CSG.FromMesh(innerCylinder)
  const ringCSG = outerCSG.subtract(innerCSG)

  const ringMesh = ringCSG.toMesh("ring", outerCylinder.material, scene, true)
  ringMesh.material = material

  outerCylinder.dispose()
  innerCylinder.dispose()

  ringMesh.rotate(Axis.Y, arcStartRad * 2 * Math.PI, Space.LOCAL)

  return ringMesh
}

function createSensorLight(scene: Scene, position: number) {
  // Create a red bulb
  const bulbRed1 = MeshBuilder.CreateCylinder(
    "bulb red 1",
    {
      diameter: 0.4,
      height: 0.4,
      tessellation: 60,
      enclose: true,
      faceColors: [
        new Color4(0.5, 0.5, 0.5, 1),
        new Color4(0.5, 0.5, 0.5, 1),
        new Color4(1, 0, 0, 1),
      ],
    },
    scene
  )
  bulbRed1.position.x = -position
  bulbRed1.position.y = -1

  // Create a light cone emitting from the bulb
  const height = 3
  const lightCone1 = MeshBuilder.CreateCylinder(
    "light cone",
    {
      diameterTop: 0.4,
      diameterBottom: 0.2,
      height,
      tessellation: 60,
      enclose: true,
    },
    scene
  )
  lightCone1.position.y = height / 2
  lightCone1.parent = bulbRed1
  // Trasulcent red light color
  lightCone1.material = new StandardMaterial("light cone 1 mat", scene)
  // @ts-expect-error Property 'diffuseColor' does not exist
  lightCone1.material.diffuseColor = new Color3(0, 0, 0)
  // @ts-expect-error Property 'emissiveColor' does not exist
  lightCone1.material.emissiveColor = new Color3(1, 0, 0)
  // @ts-expect-error Property 'specularColor' does not exist
  lightCone1.material.specularColor = new Color3(1, 1, 1)
  lightCone1.material.alpha = 0.3

  // Red spotlight pointing upwards
  const spotLight = new SpotLight(
    "spotlight red 1",
    Vector3.Zero(), // position
    new Vector3(0, 1, 0), // direction
    Tools.ToRadians(30),
    1,
    scene
  )
  spotLight.intensity = 0.5
  spotLight.diffuse = new Color3(1, 0, 0)
  spotLight.specular = new Color3(1, 0, 0)

  spotLight.parent = bulbRed1
}

async function createDefaultScene() {
  if (!engine) {
    throw new Error("Engine not found")
  }
  const scene = new Scene(engine)

  const defaultEnv = scene.createDefaultEnvironment({
    createGround: false,
  })

  if (!defaultEnv) {
    throw new Error("Default environment not found")
  }

  const woodBrown = new Color3(0.6, 0.3, 0.2)
  defaultEnv.setMainColor(woodBrown)

  scene.debugLayer.show() // Show inspector

  // Light from above
  const lightDown = new HemisphericLight(
    "light-down",
    new Vector3(0, 1, 0),
    scene
  )
  lightDown.intensity = 0.7

  // Light from below
  const lightUp = new HemisphericLight("light-up", new Vector3(0, -1, 0), scene)
  lightUp.intensity = 0.3

  return scene
}

// Semi transparent glass material
function getGlassMaterial(scene: Scene) {
  const matGlass = new PBRMaterial("glass", scene)
  matGlass.alpha = 0.5
  matGlass.metallic = 0
  matGlass.roughness = 1
  matGlass.indexOfRefraction = 2
  matGlass.directIntensity = 1
  matGlass.cameraExposure = 0.66
  matGlass.cameraContrast = 1.66
  matGlass.microSurface = 1
  return matGlass
}

async function createClip1(scene: Scene) {
  const camera = new ArcRotateCamera(
    "camera",
    Tools.ToRadians(45),
    Tools.ToRadians(50),
    15,
    Vector3.Zero(),
    scene
  )
  camera.attachControl(canvas, true)

  createSensorLight(scene, 1)
  createSensorLight(scene, 2)
  createSensorLight(scene, 3)

  const matBlack = new StandardMaterial("mat", scene)
  matBlack.diffuseColor = new Color3(0.2, 0.2, 0.2)

  const matWhite = new StandardMaterial("mat", scene)
  matWhite.diffuseColor = new Color3(1, 1, 1)

  const matGlass = getGlassMaterial(scene)

  let disc = new TransformNode("disc", scene)

  let r1Black = createRingSection(scene, 1, matBlack, 0, 0.5)
  r1Black.parent = disc
  let r1White = createRingSection(scene, 1, matGlass, 0.5, 1)
  r1White.parent = disc

  let r2Black = createRingSection(scene, 2, matGlass, -0.25, 0.25)
  r2Black.parent = disc
  let r2White = createRingSection(scene, 2, matBlack, 0.25, 0.75)
  r2White.parent = disc

  let r3aBlack = createRingSection(scene, 3, matBlack, 0.125, 0.375)
  r3aBlack.parent = disc
  let r3aWhite = createRingSection(scene, 3, matGlass, 0.375, 0.625)
  r3aWhite.parent = disc
  let r3bBlack = createRingSection(scene, 3, matBlack, 0.625, 0.875)
  r3bBlack.parent = disc
  let r3bWhite = createRingSection(scene, 3, matGlass, 0.875, 1.125)
  r3bWhite.parent = disc

  // Make red lights glow
  new GlowLayer("glow", scene)

  await runAnimation(scene, disc)
  // recordAnimation()

  return scene
}

async function createClip2(scene: Scene) {
  const camera = new ArcRotateCamera(
    "camera",
    Tools.ToRadians(45),
    Tools.ToRadians(50),
    15,
    Vector3.Zero(),
    scene
  )
  camera.attachControl(canvas, true)
  camera.viewport = new Viewport(0, 0.5, 1, 1)
  scene.activeCameras!.push(camera)

  const sensorCamera = new ArcRotateCamera(
    "sensorCamera",
    0,
    0,
    10,
    new Vector3(0, 0, 0),
    scene
  )
  sensorCamera.attachControl(canvas, true)
  sensorCamera.viewport = new Viewport(0, 0, 0.5, 0.4)
  scene.activeCameras!.push(sensorCamera)

  createSensorLight(scene, 1)
  createSensorLight(scene, 2)
  createSensorLight(scene, 3)

  const matBlack = new StandardMaterial("mat", scene)
  matBlack.diffuseColor = new Color3(0.2, 0.2, 0.2)

  const matWhite = new StandardMaterial("mat", scene)
  matWhite.diffuseColor = new Color3(1, 1, 1)

  const matGlass = getGlassMaterial(scene)

  let disc = new TransformNode("disc", scene)

  let r1Black = createRingSection(scene, 1, matBlack, 0, 0.5)
  r1Black.parent = disc
  let r1White = createRingSection(scene, 1, matGlass, 0.5, 1)
  r1White.parent = disc

  let r2Black = createRingSection(scene, 2, matGlass, -0.25, 0.25)
  r2Black.parent = disc
  let r2White = createRingSection(scene, 2, matBlack, 0.25, 0.75)
  r2White.parent = disc

  let r3aBlack = createRingSection(scene, 3, matBlack, 0.125, 0.375)
  r3aBlack.parent = disc
  let r3aWhite = createRingSection(scene, 3, matGlass, 0.375, 0.625)
  r3aWhite.parent = disc
  let r3bBlack = createRingSection(scene, 3, matBlack, 0.625, 0.875)
  r3bBlack.parent = disc
  let r3bWhite = createRingSection(scene, 3, matGlass, 0.875, 1.125)
  r3bWhite.parent = disc

  // Make red lights glow
  new GlowLayer("glow", scene)

  await runAnimation(scene, disc)
  // recordAnimation()
}

async function init() {
  const asyncEngineCreation = async function () {
    try {
      return createDefaultEngine()
    } catch (e) {
      console.log(
        "The available createEngine function failed. " +
          "Creating the default engine instead"
      )
      return createDefaultEngine()
    }
  }

  engine = await asyncEngineCreation()
  startRenderLoop(engine, canvas)
  const scene = await createDefaultScene()
  await createClip1(scene)
  sceneToRender = scene
}

init()

window.addEventListener("resize", () => {
  if (engine) {
    engine.resize()
  }
})
