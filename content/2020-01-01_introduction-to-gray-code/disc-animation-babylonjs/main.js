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

async function runAnimation(scene, newMesh) {
  const frameRate = 60
  const animations = await BABYLON.Animation.ParseFromFileAsync(
    null,
    "disc_animations.json"
  )

  newMesh.animations = animations
  scene.beginAnimation(newMesh, 0, 3 * frameRate, true)
}

function recordAnimation() {
  const duration = 2 // seconds
  const codec = "opus" // Had the best visual results during testing

  const recorder = new BABYLON.VideoRecorder(engine, {
    fps: 60,
    duration: 2,
    mimeType: `video/webm;codecs=${codec}`,
  })
  recorder.startRecording(`a-video-${codec}.webm`, duration)
}

function createRingSection(scene, position, material, arcStartRad, arcEndRad) {
  const outerCylinder = BABYLON.MeshBuilder.CreateCylinder(
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
  const outerCSG = BABYLON.CSG.FromMesh(outerCylinder)

  const innerCylinder = BABYLON.MeshBuilder.CreateCylinder(
    "inner",
    { diameter: (position - 1) * 2 + 1, height: 1, tessellation: 60 },
    scene
  )
  const innerCSG = BABYLON.CSG.FromMesh(innerCylinder)
  const ringCSG = outerCSG.subtract(innerCSG)

  const ringMesh = ringCSG.toMesh("ring", outerCylinder.material, scene, true)
  ringMesh.material = material

  outerCylinder.dispose()
  innerCylinder.dispose()

  ringMesh.rotate(
    BABYLON.Axis.Y,
    arcStartRad * 2 * Math.PI,
    BABYLON.Space.LOCAL
  )

  return ringMesh
}

function createSensorLight(scene, position) {
  // Create a red bulb
  const bulbRed1 = BABYLON.MeshBuilder.CreateCylinder(
    "bulb red 1",
    {
      diameter: 0.4,
      height: 0.4,
      tessellation: 60,
      enclose: true,
      faceColors: [
        new BABYLON.Color3(0.5, 0.5, 0.5),
        new BABYLON.Color3(0.5, 0.5, 0.5),
        new BABYLON.Color3(1, 0, 0),
      ],
    },
    scene
  )
  bulbRed1.position.x = -position
  bulbRed1.position.y = -1

  // Create a light cone emitting from the bulb
  const height = 3
  const lightCone1 = BABYLON.MeshBuilder.CreateCylinder(
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
  lightCone1.material = new BABYLON.StandardMaterial("light cone 1 mat", scene)
  lightCone1.material.diffuseColor = new BABYLON.Color3(0, 0, 0)
  lightCone1.material.emissiveColor = new BABYLON.Color3(1, 0, 0)
  lightCone1.material.specularColor = new BABYLON.Color3(1, 1, 1)
  lightCone1.material.alpha = 0.3

  // Red spotlight pointing upwards
  const spotLight = new BABYLON.SpotLight(
    "spotlight red 1",
    new BABYLON.Vector3.Zero(), // position
    new BABYLON.Vector3(0, 1, 0), // direction
    BABYLON.Tools.ToRadians(30),
    1,
    scene
  )
  spotLight.intensity = 0.5
  spotLight.diffuse = new BABYLON.Color3(1, 0, 0)
  spotLight.specular = new BABYLON.Color3(1, 0, 0)

  spotLight.parent = bulbRed1
}

async function createScene() {
  const scene = new BABYLON.Scene(engine)

  const defaultEnv = scene.createDefaultEnvironment({
    createGround: false,
  })
  const woodBrown = new BABYLON.Color3(0.6, 0.3, 0.2)
  defaultEnv.setMainColor(woodBrown)

  scene.debugLayer.show() // Show inspector

  const camera = new BABYLON.ArcRotateCamera(
    "camera",
    BABYLON.Tools.ToRadians(45),
    BABYLON.Tools.ToRadians(50),
    15,
    BABYLON.Vector3.Zero(),
    scene
  )

  // This attaches the camera to the canvas
  camera.attachControl(canvas, true)

  // Light from above
  const lightDown = new BABYLON.HemisphericLight(
    "light-down",
    new BABYLON.Vector3(0, 1, 0),
    scene
  )
  lightDown.intensity = 0.7

  // Light from below
  const lightUp = new BABYLON.HemisphericLight(
    "light-up",
    new BABYLON.Vector3(0, -1, 0),
    scene
  )
  lightUp.intensity = 0.3

  createSensorLight(scene, 1)
  createSensorLight(scene, 2)
  createSensorLight(scene, 3)

  const matBlack = new BABYLON.StandardMaterial("mat", scene)
  matBlack.diffuseColor = new BABYLON.Color3(0.2, 0.2, 0.2)

  const matWhite = new BABYLON.StandardMaterial("mat", scene)
  matWhite.diffuseColor = new BABYLON.Color3(1, 1, 1)

  // Semi transparent glass material
  const matGlass = new BABYLON.PBRMaterial("glass", scene)
  matGlass.alpha = 0.5
  matGlass.metallic = 0
  matGlass.roughness = 1
  matGlass.indexOfRefraction = 2
  matGlass.directIntensity = 1
  matGlass.cameraExposure = 0.66
  matGlass.cameraContrast = 1.66
  matGlass.microSurface = 1

  let disc = new BABYLON.TransformNode("disc", scene)

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

  const gl = new BABYLON.GlowLayer("glow", scene)

  await runAnimation(scene, disc)
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
