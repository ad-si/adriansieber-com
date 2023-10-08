@tool
extends Node3D


# Called when the node enters the scene tree for the first time.
func _ready():
	# Create a new CSG Box
	var box = CSGBox3D.new()
	# Set the size of the box
	box.size = Vector3(0.5, 0.5, 0.5)
	# Set the material of the box
	box.material = StandardMaterial3D.new()
	# Set the color of the box
	box.material.albedo_color = Color(0.5, 0.5, 0.5)
	# Set the box to be visible in the editor
	box.visible = true
	# Add the box as a child of this node
	add_child(box)


# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(_delta):
	pass
