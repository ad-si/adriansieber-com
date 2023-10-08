const canvas = document.getElementById("renderCanvas")

// Set size of canvas to full HD
canvas.width = 1920 / 2
canvas.height = 1080 / 2

let sceneToRender = null

function startRenderLoop(theEngine, canvas) {
  theEngine.runRenderLoop(function () {
    if (sceneToRender && sceneToRender.activeCamera) {
      sceneToRender.render()
    }
  })
}

function createDefaultEngine() {
  return new BABYLON.Engine(canvas, true, {
    preserveDrawingBuffer: true,
    stencil: true,
    disableWebGL2Support: false,
  })
}

// function getAnimation() {
//   const anim = new BABYLON.Animation(
//     "xSlide",
//     "position.x",
//     frameRate,
//     BABYLON.Animation.ANIMATIONTYPE_FLOAT,
//     BABYLON.Animation.ANIMATIONLOOPMODE_CYCLE
//   )

//   const keyFrames = [
//     {
//       frame: 0,
//       value: 2,
//     },
//     {
//       frame: frameRate,
//       value: -2,
//     },
//     {
//       frame: 2 * frameRate,
//       value: 2,
//     },
//   ]

//   anim.setKeys(keyFrames)

//   return anim
// }

async function runAnimation(scene, newMesh) {
  const frameRate = 60

  // const anim = getAnimation()

  const animations = await BABYLON.Animation.ParseFromFileAsync(
    null,
    "disc_animations.json"
  )

  newMesh.animations = animations

  scene.beginAnimation(newMesh, 0, 2 * frameRate, true)
}

function recordAnimation() {
  const duration = 2 // seconds
  const codec = "opus"  // Had the best visual results during testing

  const recorder = new BABYLON.VideoRecorder(engine, {
    fps: 60,
    duration: 2,
    mimeType: `video/webm;codecs=${codec}`,
  })
  recorder.startRecording(`a-video-${codec}.webm`, duration)
}

function createRing() {
  const square = [
    new BABYLON.Vector3(1, 0, 0),
    new BABYLON.Vector3(2, 0, 0),
    new BABYLON.Vector3(2, 1, 0),
    new BABYLON.Vector3(1, 1, 0),
  ]

  const lathe = BABYLON.MeshBuilder.CreateLathe("lathe", {
    shape: square,
    radius: 0.2,
    tessellation: 60,
    sideOrientation: BABYLON.Mesh.DOUBLESIDE,
  })
  lathe.convertToFlatShadedMesh()
  return lathe
}

async function createScene() {
  // This creates a basic Babylon Scene object (non-mesh)
  const scene = new BABYLON.Scene(engine)

  scene.debugLayer.show()

  const camera = new BABYLON.ArcRotateCamera(
    "camera",
    BABYLON.Tools.ToRadians(90),
    BABYLON.Tools.ToRadians(65),
    10,
    BABYLON.Vector3.Zero(),
    scene
  )

  // This attaches the camera to the canvas
  camera.attachControl(canvas, true)

  // This creates a light, aiming 0,1,0 - to the sky (non-mesh)
  const light = new BABYLON.HemisphericLight(
    "light",
    new BABYLON.Vector3(0, 1, 0),
    scene
  )

  // Default intensity is 1. Dims the light a small amount.
  light.intensity = 0.7

  let ring = createRing()

  await runAnimation(scene, ring)
  // recordAnimation()

  return scene
}

window.initFunction = async function () {
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

  window.engine = await asyncEngineCreation()
  startRenderLoop(window.engine, canvas)
  window.scene = await createScene()
}

initFunction().then(() => {
  sceneToRender = scene
})

// Resize
window.addEventListener("resize", function () {
  engine.resize()
})
