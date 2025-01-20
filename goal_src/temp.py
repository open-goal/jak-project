op_names_2arg = [
    ".mov.vf",
    ".abs.vf",
]

op_names_3arg = [
    ".xor.vf",
    ".blend.vf",
    ".max.vf",
    ".max.x.vf",
    ".max.y.vf",
    ".max.z.vf",
    ".max.w.vf",
    ".min.vf",
    ".min.x.vf",
    ".min.y.vf",
    ".min.z.vf",
    ".min.w.vf",
    ".add.vf",
    ".add.x.vf",
    ".add.y.vf",
    ".add.z.vf",
    ".add.w.vf",
    ".sub.vf",
    ".sub.x.vf",
    ".sub.y.vf",
    ".sub.z.vf",
    ".sub.w.vf",
    ".mul.vf",
    ".mul.x.vf",
    ".mul.y.vf",
    ".mul.z.vf",
    ".mul.w.vf",
]

op_names_4arg = [
    ".add.mul.vf",
    ".add.mul.x.vf",
    ".add.mul.y.vf",
    ".add.mul.z.vf",
    ".add.mul.w.vf",
    ".sub.mul.vf",
    ".sub.mul.x.vf",
    ".sub.mul.y.vf",
    ".sub.mul.z.vf",
    ".sub.mul.w.vf",
]

masks = {
    "x": "#b0001",
    "y": "#b0010",
    "z": "#b0100",
    "w": "#b1000",
    "xy": "#b0011",
    "xz": "#b0101",
    "xw": "#b1001",
    "yz": "#b0110",
    "yw": "#b1010",
    "zw": "#b1100",
    "xyz": "#b0111",
    "xyw": "#b1011",
    "xzw": "#b1101",
    "yzw": "#b1110",
    "xyzw": "#b1111",
}


for op_name in op_names_2arg:
    for op_addition, mask in masks.items():
        print(f"(defmacro {op_name}.{op_addition} (dest src)\n  \"Performs `{op_name}` on the provided arguments with a `:mask` of `{mask}`\"\n  `({op_name} ,dest ,src :mask {mask}))\n")

for op_name in op_names_3arg:
    for op_addition, mask in masks.items():
        print(f"(defmacro {op_name}.{op_addition} (dest src1 src2)\n  \"Performs `{op_name}` on the provided arguments with a `:mask` of `{mask}`\"\n  `({op_name} ,dest ,src1 ,src2 :mask {mask}))\n")

for op_name in op_names_4arg:
    for op_addition, mask in masks.items():
        print(f"(defmacro {op_name}.{op_addition} (dest src1 src2 src3)\n  \"Performs `{op_name}` on the provided arguments with a `:mask` of `{mask}`\"\n  `({op_name} ,dest ,src1 ,src2 ,src3 :mask {mask}))\n")