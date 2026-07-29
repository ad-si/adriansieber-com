// I wasn't able to close of then ends of the half-ring
function createRingLathe(scene) {
  const square = [
    new BABYLON.Vector3(1, 0, 0),
    new BABYLON.Vector3(2, 0, 0),
    new BABYLON.Vector3(2, 1, 0),
    new BABYLON.Vector3(1, 1, 0),
    new BABYLON.Vector3(1, 0, 0),
  ]

  const lathe = BABYLON.MeshBuilder.CreateLathe("lathe", {
    shape: square,
    radius: 2,
    tessellation: 60,
    arc: 0.45,
    sideOrientation: BABYLON.Mesh.DOUBLESIDE,
    closed: false,
  })
  lathe.convertToFlatShadedMesh()

  // const nodeMaterial = new BABYLON.NodeMaterial("binary_pattern", scene, {
  //   emitComments: true,
  // })

  // let mat = new BABYLON.StandardMaterial("mat", scene)
  // mat.diffuseTexture = new BABYLON.Texture("rbc_pattern.png", scene)
  // mat.diffuseTexture.hasAlpha = true
  // mat.backFaceCulling = false
  // lathe.material = mat

  // const pbr = new BABYLON.PBRMaterial("pbr", scene)
  // lathe.material = pbr
  // pbr.backFaceCulling = false
  // pbr.metallic = 0

  // pbr.albedoTexture = new BABYLON.Texture("rbc_pattern.png", scene)
  // pbr.opacityTexture = new BABYLON.Texture("rbc_pattern.png", scene)
  // // pbr.opacityTexture.getAlphaFromRGB = true

  return lathe
}

function getAnimation() {
  const anim = new BABYLON.Animation(
    "xSlide",
    "position.x",
    frameRate,
    BABYLON.Animation.ANIMATIONTYPE_FLOAT,
    BABYLON.Animation.ANIMATIONLOOPMODE_CYCLE
  )

  const keyFrames = [
    {
      frame: 0,
      value: 2,
    },
    {
      frame: frameRate,
      value: -2,
    },
    {
      frame: 2 * frameRate,
      value: 2,
    },
  ]

  anim.setKeys(keyFrames)

  return anim
}
