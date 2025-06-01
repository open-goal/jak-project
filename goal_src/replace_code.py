import glob
import re

masks = {
    "#b0001": "x",
    "#b0010": "y",
    "#b0100": "z",
    "#b1000": "w",
    "#b0011": "xy",
    "#b0101": "xz",
    "#b1001": "xw",
    "#b0110": "yz",
    "#b1010": "yw",
    "#b1100": "zw",
    "#b0111": "xyz",
    "#b1011": "xyw",
    "#b1101": "xzw",
    "#b1110": "yzw",
    "#b1111": "xyzw",
}

relevant_files = glob.glob("./jak3/**/*.gc", recursive=True)
mask_regex = re.compile(r"(:mask #b[0,1]+)")
op_regex = re.compile(r"\(.*(\.vf)\s")

for file in relevant_files:
    print(file)
    new_lines = []
    with open(file, mode="r", encoding="utf-8") as f:
        lines = f.readlines()
        for line in lines:
            if ":mask #b" in line:
                # find the operation name in the line and the mask value
                mask_result = mask_regex.search(line)
                mask = mask_result.group(0)
                op_result = op_regex.search(line)
                op = op_result.group(0)
                if line.count(":mask #b") > 1 or len(mask_result.groups()) > 1 or len(op_result.groups()) > 1:
                    new_lines.append(line)
                    continue
                # handle padding on the mask
                mask_val = mask.removeprefix(":mask #b")
                mask_val = "#b{}".format(mask_val.zfill(4))
                mask = ":mask {}".format(mask_val)
                new_line = line[:op_result.span(0)[0]] + op.rstrip() + "." + masks[mask_val] + " " + line[op_result.span(0)[1]:mask_result.span(0)[0] - 1] + line[mask_result.span(0)[1]:]
                new_lines.append(new_line)
            else:
                new_lines.append(line)
    with open(file, mode="w+", encoding="utf-8") as f:
        f.writelines(new_lines)