import json

levels_added = set()
level_info_sql_lines = []


def convert_lights_to_sql():
    sql_lines = []
    with open("fixture-lights.json", "r", encoding="utf-8") as f:
        fixtures = json.load(f)
    for level_name, lights in fixtures.items():
        if level_name not in levels_added:
            levels_added.add(level_name)
            # since we get all the levels from the final game, the translate offset can always be 0
            level_info_sql_lines.append(
                f"INSERT INTO level_info (name, translate_x, translate_y, translate_z, last_update, sample_point_update) VALUES ('{level_name}', 0, 0, 0, NULL, NULL);\n"
            )
        for light in lights:
            sql_lines.append(
                f"INSERT INTO light (name, level_name, pos_x, pos_y, pos_z, r, dir_x, dir_y, dir_z, color0_r, color0_g, color0_b, color0_a, decay_start, ambient_point_ratio, brightness) VALUES ('{light['name']}', '{level_name}', {light['bsphere'][0]}, {light['bsphere'][1]}, {light['bsphere'][2]}, {light['bsphere'][3]}, {light['direction'][0]}, {light['direction'][1]}, {light['direction'][2]}, {light['color'][0]}, {light['color'][1]}, {light['color'][2]}, {light['color'][3]}, {light['decay_start']}, {light['ambient-point-ratio']}, {light['brightness']});\n"
            )
    return sql_lines


lights_sql_lines = convert_lights_to_sql()

# write out the SQL files
# with open("fixture-level_info.sql", "w", encoding="utf-8") as f:
#   f.writelines(level_info_sql_lines)

# with open("fixture-light.sql", "w", encoding="utf-8") as f:
#   f.writelines(lights_sql_lines)

def convert_regions_to_sql():
    sql_lines = []
    with open("fixture-regions.json", "r", encoding="utf-8") as f:
        fixtures = json.load(f)
    region_face_id = 0
    for level_name, regions in fixtures.items():
        if level_name not in levels_added:
            levels_added.add(level_name)
            # since we get all the levels from the final game, the translate offset can always be 0
            level_info_sql_lines.append(
                f"INSERT INTO level_info (name, translate_x, translate_y, translate_z, last_update, sample_point_update) VALUES ('{level_name}', 0, 0, 0, NULL, NULL);\n"
            )
        for region in regions:
            on_enter = region["on-enter"].replace("'", "''")
            if on_enter == "#f":
                on_enter = ""
            on_exit = region["on-exit"].replace("'", "''")
            if on_exit == "#f":
                on_exit = ""
            on_inside = region["on-inside"].replace("'", "''")
            if on_inside == "#f":
                on_inside = ""
            sql_lines.append(
                f"INSERT INTO `region` (`region_id`, `level_name`, `tree`, `on_enter`, `on_exit`, `on_inside`) values ({region['id']}, '{level_name}', '{region['region-tree']}', '{on_enter}', '{on_exit}', '{on_inside}');\n"
            )
            if "sphere" in region:
                sql_lines.append(f"INSERT INTO `region_sphere` (`region_id`, `x`, `y`, `z`, `r`) values ({region['id']}, {region['sphere'][0]}, {region['sphere'][1]}, {region['sphere'][2]}, {region['sphere'][3]});\n")
            elif region["kind"] == "face":
                for face in region["faces"]:
                    sql_lines.append(
                        f"INSERT INTO `region_face` (`region_face_id`, `region_id`, `kind`, `flags`, `normal_x`, `normal_y`, `normal_z`, `normal_w`, `pos_x`, `pos_y`, `pos_z`, `pos_w`) values ({region_face_id}, {region['id']}, 'face', '', {face['normal'][0]}, {face['normal'][1]}, {face['normal'][2]}, {face['normal'][3]}, {face['position'][0]}, {face['position'][1]}, {face['position'][2]}, {face['position'][3]});\n"
                    )
                    point_idx = 0
                    for point in face['points']:
                        sql_lines.append(
                            f"INSERT INTO `region_point` (`region_face_id`, `idx`, `x`, `y`, `z`, `w`) values ({region_face_id}, {point_idx}, {point[0]}, {point[1]}, {point[2]}, {point[3]});\n"
                        )
                        point_idx += 1
                    region_face_id += 1
            elif region["kind"] == "plane":
                sql_lines.append(
                    f"INSERT INTO `region_face` (`region_face_id`, `region_id`, `kind`, `flags`, `normal_x`, `normal_y`, `normal_z`, `normal_w`, `pos_x`, `pos_y`, `pos_z`, `pos_w`) values ({region_face_id}, {region['id']}, 'plane', '', {region['normal'][0]}, {region['normal'][1]}, {region['normal'][2]}, {region['normal'][3]}, {region['position'][0]}, {region['position'][1]}, {region['position'][2]}, {region['position'][3]});\n"
                )
                point_idx = 0
                for point in region['points']:
                    sql_lines.append(
                        f"INSERT INTO `region_point` (`region_face_id`, `idx`, `x`, `y`, `z`, `w`) values ({region_face_id}, {point_idx}, {point[0]}, {point[1]}, {point[2]}, {point[3]});\n"
                    )
                    point_idx += 1
                region_face_id += 1
    return sql_lines

with open("fixture-region.sql", "w", encoding="utf-8") as f:
    f.writelines(convert_regions_to_sql())
