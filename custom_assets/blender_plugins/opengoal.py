bl_info = {
    "name": "OpenGOAL Mesh",
    "author": "water111",
    "version": (0, 0, 3),
    "blender": (2, 83, 0),
    "location": "3D View",
    "description": "OpenGOAL Mesh tools",
    "category": "Development"
}

import bpy
import colorsys
import bmesh

from bpy.props import (StringProperty,
                       BoolProperty,
                       IntProperty,
                       FloatProperty,
                       FloatVectorProperty,
                       EnumProperty,
                       PointerProperty,
                       )
from bpy.types import (Panel,
                       Menu,
                       Operator,
                       PropertyGroup,
                       )


import bpy

pat_surfaces = [
  ("stone", "stone", "", 0),
  ("ice", "ice", "", 1),
  ("quicksand", "quicksand", "", 2),
  ("waterbottom", "waterbottom", "", 3),
  ("tar", "tar", "", 4),
  ("sand", "sand", "", 5),
  ("wood", "wood", "", 6),
  ("grass", "grass", "", 7),
  ("pcmetal", "pcmetal", "", 8),
  ("snow", "snow", "", 9),
  ("deepsnow", "deepsnow", "", 10),
  ("hotcoals", "hotcoals", "", 11),
  ("lava", "lava", "", 12),
  ("crwood", "crwood", "", 13),
  ("gravel", "gravel", "", 14),
  ("dirt", "dirt", "", 15),
  ("metal", "metal", "", 16),
  ("straw", "straw", "", 17),
  ("tube", "tube", "", 18),
  ("swamp", "swamp", "", 19),
  ("stopproj", "stopproj", "", 20),
  ("rotate", "rotate", "", 21),
  ("neutral", "neutral", "", 22),
]

pat_events = [
  ("none", "none", "", 0),
  ("deadly", "deadly", "", 1),
  ("endlessfall", "endlessfall", "", 2),
  ("burn", "burn", "", 3),
  ("deadlyup", "deadlyup", "", 4),
  ("burnup", "burnup", "", 5),
  ("melt", "melt", "", 6),
]

pat_modes = [
  ("ground", "ground", "", 0),
  ("wall", "wall", "", 1),
  ("obstacle", "obstacle", "", 2),
]

def draw_func(self, context):
    layout = self.layout
    ob = context.object
    layout.prop(ob.active_material, "set_invisible")
    layout.prop(ob.active_material, "set_collision")
    if (ob.active_material.set_collision):
        layout.prop(ob.active_material, "ignore")
        layout.prop(ob.active_material, "collide_mode")
        layout.prop(ob.active_material, "collide_material")
        layout.prop(ob.active_material, "collide_event")
        layout.prop(ob.active_material, "noedge")
        layout.prop(ob.active_material, "noentity")
        layout.prop(ob.active_material, "nolineofsight")
        layout.prop(ob.active_material, "nocamera")

def draw_func_ob(self, context):
    layout = self.layout
    ob = context.object
    layout.prop(ob, "set_invisible")
    layout.prop(ob, "set_collision")
    if (ob.set_collision):
        layout.prop(ob, "ignore")
        layout.prop(ob, "collide_mode")
        layout.prop(ob, "collide_material")
        layout.prop(ob, "collide_event")
        layout.prop(ob, "noedge")
        layout.prop(ob, "noentity")
        layout.prop(ob, "nolineofsight")
        layout.prop(ob, "nocamera")

def register():
    bpy.types.Material.set_invisible = bpy.props.BoolProperty(name="Invisible")
    bpy.types.Material.set_collision = bpy.props.BoolProperty(name="Apply Collision Properties")
    bpy.types.Material.ignore = bpy.props.BoolProperty(name="ignore")
    bpy.types.Material.noedge = bpy.props.BoolProperty(name="No-Edge")
    bpy.types.Material.noentity = bpy.props.BoolProperty(name="No-Entity")
    bpy.types.Material.nolineofsight = bpy.props.BoolProperty(name="No-LOS")
    bpy.types.Material.nocamera = bpy.props.BoolProperty(name="No-Camera")
    bpy.types.Material.collide_material = bpy.props.EnumProperty(items = pat_surfaces, name = "Material")
    bpy.types.Material.collide_event = bpy.props.EnumProperty(items = pat_events, name = "Event")
    bpy.types.Material.collide_mode = bpy.props.EnumProperty(items = pat_modes, name = "Mode")
    bpy.types.MATERIAL_PT_custom_props.prepend(draw_func)

    bpy.types.Object.set_invisible = bpy.props.BoolProperty(name="Invisible")
    bpy.types.Object.set_collision = bpy.props.BoolProperty(name="Apply Collision Properties")
    bpy.types.Object.ignore = bpy.props.BoolProperty(name="ignore")
    bpy.types.Object.noedge = bpy.props.BoolProperty(name="No-Edge")
    bpy.types.Object.noentity = bpy.props.BoolProperty(name="No-Entity")
    bpy.types.Object.nolineofsight = bpy.props.BoolProperty(name="No-LOS")
    bpy.types.Object.nocamera = bpy.props.BoolProperty(name="No-Camera")
    bpy.types.Object.collide_material = bpy.props.EnumProperty(items = pat_surfaces, name = "Material")
    bpy.types.Object.collide_event = bpy.props.EnumProperty(items = pat_events, name = "Event")
    bpy.types.Object.collide_mode = bpy.props.EnumProperty(items = pat_modes, name = "Mode")
    bpy.types.OBJECT_PT_custom_props.prepend(draw_func_ob)

def unregister():
    bpy.types.MATERIAL_PT_custom_props.remove(draw_func)

if __name__ == "__main__":
    register()