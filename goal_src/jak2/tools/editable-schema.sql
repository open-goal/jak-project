-- They stored floats in SQL as strings, or atleast parse them as such, likely to get around the lack of IEEE support on the PS2
-- `string->float` is used to convert results
-- Interestingly, ints are handled the same way with `string->int`
-- This means the kernel implementation is very simple and just returns an array of strings essentially

-- They also use implicit joins :(

-- All Queries / Partial Queries:
-- select translate_x,translate_y,translate_z,level_info_id from level_info where name='~S'
-- select light_id,name,pos_x,pos_y,pos_z,r,dir_x,dir_y,dir_z,color0_r,color0_g,color0_b,color0_a,decay_start,ambient_point_ratio,brightness from light where level_name='~S'
-- select x,y,z from sample_point where level_info_id='~D' and source='manual'
-- select region_sphere.region_id,x,y,z,r from region,region_sphere where level_name='~S' and region.region_id=region_sphere.region_id
-- select region_face.region_face_id,idx,x,y,z from region,region_face,region_point where level_name='~S' and region.region_id=region_face.region_id and region_face.region_face_id=region_point.region_face_id
-- select region_face.region_id,region_face_id,kind,flags,radius from region,region_face where level_name='~S' and region.region_id=region_face.region_id
-- select region_id,level_name,tree,on_enter,on_exit,on_inside from region where level_name='~S'
-- select region_face_id from region_face where region_id=~D
-- select nav_edge_id,nav_node_id,nav_graph_id from nav_visible_nodes limit ~D,~D
-- select nav_edge_id,nav_node_id_1,nav_node_id_2,directionality,speed_limit,density,traffic_edge_flag,nav_clock_mask,nav_clock_type,width,minimap_edge_flag,nav_graph_id from nav_edge limit ~D,~D
-- select nav_node_id,x,y,z,level_name,angle,radius,nav_node_flag,nav_mesh_id,nav_graph_id from nav_node
-- select LAST_INSERT_ID()
-- select nav_graph_id from nav_graph where name='~S'
-- select nav_graph_id from nav_graph where name='~S'
-- insert into sample_point set level_info_id=~D,x=~f,y=~f,z=~f,source='manual'
-- insert into region_face set kind='plane',region_id=~D,radius=~f
-- insert into region_face set kind='face',region_id=~D
-- insert into light set level_name='~S'
-- insert into sample_point set level_info_id=~D,x=~f,y=~f,z=~f,source='manual'
-- insert into region_sphere set region_id=~D,x=~f,y=~f,z=~f,r=~f
-- insert into region_point set region_face_id=LAST_INSERT_ID(),idx=~D,x=~f,y=~f,z=~f
-- insert into region set level_name='~S',tree='~S'
-- insert nav_visible_nodes set
-- insert nav_edge set
-- insert nav_node set
-- insert into race_path set
-- update level_info set last_update=last_update, sample_point_update=NULL where level_info_id=~D
-- update light set level_name='~S', name='~S'
-- update region set level_name='~S',tree='~S'
-- update level_info set last_update=last_update, sample_point_update=NULL where level_info_id=~D
-- update nav_visible_nodes set
-- update nav_edge set
-- update nav_node set
-- delete from light where light_id=~D
-- delete from sample_point where level_info_id=~D and source='manual'
-- delete from region where region_id=~D
-- delete from region_face where region_id=~D
-- delete from region_point where region_face_id in (
-- delete from region_sphere where region_id=~D
-- delete from nav_visible_nodes where
-- delete from nav_edge where
-- delete from nav_node where
-- delete from race_path where race='~s' and path=~d~%

-- NOTE this is SQLite3, NOT MySQL

CREATE TABLE IF NOT EXISTS 'level_info' (
	'level_info_id'	INTEGER,
	'name'	TEXT UNIQUE,
	'translate_x'	REAL,
	'translate_y'	REAL,
	'translate_z'	REAL,
	'last_update'	TEXT, -- never changed by the ingame editor
	'sample_point_update'	TEXT, -- always set to NULL by the ingame editor
	PRIMARY KEY('level_info_id' AUTOINCREMENT)
);

CREATE TABLE IF NOT EXISTS 'light' (
	'light_id'	INTEGER,
	'name'	TEXT,
	'level_name'	TEXT,
	'pos_x'	REAL, -- stored as meters
	'pos_y'	REAL, -- stored as meters
	'pos_z'	REAL, -- stored as meters
	'r'	REAL,
	'dir_x'	REAL,
	'dir_y'	REAL,
	'dir_z'	REAL,
	'color0_r'	REAL,
	'color0_g'	REAL,
	'color0_b'	REAL,
	'color0_a'	REAL,
	'decay_start'	REAL,
	'ambient_point_ratio'	REAL,
	'brightness'	REAL,
	PRIMARY KEY('light_id' AUTOINCREMENT),
	FOREIGN KEY('level_name') REFERENCES 'level_info'('name')
);

CREATE TABLE IF NOT EXISTS 'region' (
	'region_id'	INTEGER NOT NULL,
	'level_name'	TEXT,
	-- Can be:
		-- target
		-- camera
		-- data
		-- water
		-- city_vis
		-- sample
		-- light
		-- entity
	'tree'	TEXT,
	'on_enter'	TEXT,
	'on_exit'	TEXT,
	'on_inside'	TEXT,
	PRIMARY KEY('region_id' AUTOINCREMENT),
	FOREIGN KEY('level_name') REFERENCES 'level_info'('name')
);

-- TODO - don't know how to properly derive radius yet but it controls as you'd expect the width of the plane
-- TODO in the game, planes are defined with 4 points but in the editor
-- it only uses 2.
-- Either there is a bug or the editor is wrong, as these two points define the proper plane
-- but when it's filled in, it gets drawn on the first point, perpendicular.
--
-- There is a bunch of complicated vector math that converts from 2 points to 4 points, and this
-- is probably the source of the problem.
CREATE TABLE IF NOT EXISTS 'region_face' (
	'region_face_id'	INTEGER NOT NULL,
	'region_id'	INTEGER NOT NULL,
	-- Can be:
	  -- plane
		-- face
	'kind'	TEXT,
	-- Can be:
	  -- orient (face the opposite direction of the normal)
	'flags'	TEXT,
	-- 'radius'	REAL, removed, radius only allows for square planes, not good enough.
	-- Added by us so that the regions can actually be displayed as they are
	-- in the final game
	'normal_x' REAL,
	'normal_y' REAL,
	'normal_z' REAL,
	'normal_w' REAL,
	'pos_x'	REAL,
	'pos_y'	REAL,
	'pos_z'	REAL,
	'pos_w'	REAL,
	FOREIGN KEY('region_id') REFERENCES 'region'('region_id'),
	PRIMARY KEY('region_face_id' AUTOINCREMENT)
);

CREATE TABLE IF NOT EXISTS 'region_point' (
	'region_point_id'	INTEGER,
	'region_face_id'	INTEGER NOT NULL,
	'idx'	INTEGER,
	'x'	REAL,
	'y'	REAL,
	'z'	REAL,
	-- also added, doesn't hurt
	'w' REAL,
	FOREIGN KEY('region_face_id') REFERENCES 'region_face'('region_face_id'),
	PRIMARY KEY('region_point_id' AUTOINCREMENT)
);

CREATE TABLE IF NOT EXISTS 'region_sphere' (
	'region_sphere_id'	INTEGER,
	'region_id'	INTEGER,
	'x'	REAL,
	'y'	REAL,
	'z'	REAL,
	'r'	REAL,
	FOREIGN KEY('region_id') REFERENCES 'region'('region_id'),
	PRIMARY KEY('region_sphere_id' AUTOINCREMENT)
);

CREATE TABLE IF NOT EXISTS 'nav_edge' (
	'nav_edge_id'	INTEGER NOT NULL,
	'nav_graph_id'	INTEGER NOT NULL,
	'nav_node_id_1'	INTEGER,
	'nav_node_id_2'	INTEGER,
	'directionality'	TEXT,
	'speed_limit'	NUMERIC,
	'density'	NUMERIC,
	'traffic_edge_flag'	NUMERIC,
	'nav_clock_mask'	NUMERIC,
	'nav_clock_type'	TEXT,
	'width'	NUMERIC,
	'minimap_edge_flag'	NUMERIC,
	FOREIGN KEY('nav_node_id_2') REFERENCES 'nav_node'('nav_node_id'),
	FOREIGN KEY('nav_graph_id') REFERENCES 'nav_graph'('nav_graph_id'),
	FOREIGN KEY('nav_node_id_1') REFERENCES 'nav_node'('nav_node_id'),
	PRIMARY KEY('nav_edge_id' AUTOINCREMENT)
);

CREATE TABLE IF NOT EXISTS 'nav_graph' (
	'nav_graph_id'	INTEGER,
	'name'	TEXT,
	PRIMARY KEY('nav_graph_id' AUTOINCREMENT)
);

CREATE TABLE IF NOT EXISTS 'nav_mesh' (
	'nav_mesh_id'	INTEGER,
	PRIMARY KEY('nav_mesh_id' AUTOINCREMENT)
);

CREATE TABLE IF NOT EXISTS 'nav_node' (
	'nav_node_id'	INTEGER NOT NULL,
	'nav_graph_id'	INTEGER NOT NULL,
	'nav_mesh_id'	INTEGER NOT NULL,
	'x'	REAL,
	'y'	REAL,
	'z'	REAL,
	'level_name'	TEXT,
	'angle'	REAL,
	'radius'	REAL,
	'nav_node_flag'	NUMERIC,
	FOREIGN KEY('nav_mesh_id') REFERENCES 'nav_mesh'('nav_mesh_id'),
	FOREIGN KEY('nav_graph_id') REFERENCES 'nav_graph'('nav_graph_id'),
	PRIMARY KEY('nav_node_id' AUTOINCREMENT)
);

CREATE TABLE IF NOT EXISTS 'nav_visible_nodes' (
	'nav_node_id'	INTEGER NOT NULL,
	'nav_graph_id'	INTEGER NOT NULL,
	'nav_edge_id'	INTEGER NOT NULL,
	FOREIGN KEY('nav_edge_id') REFERENCES 'nav_mesh'('nav_mesh_id'),
	FOREIGN KEY('nav_graph_id') REFERENCES 'nav_graph'('nav_graph_id'),
	PRIMARY KEY('nav_node_id' AUTOINCREMENT)
);

CREATE TABLE IF NOT EXISTS 'race_path' (
	'race_path_id'	INTEGER,
	'race'	TEXT,
	'path'	INTEGER,
	PRIMARY KEY('race_path_id' AUTOINCREMENT)
);

-- I believe these are related to the camera tracking-spline sample points
-- but these were all "manual" and i don't think they are stored in the level files at all
-- though who knows, the bsp-header is not 100% filled out
CREATE TABLE IF NOT EXISTS 'sample_point' (
	'sample_point_id'	INTEGER,
	'level_info_id'	INTEGER NOT NULL,
	'source'	TEXT,
	'x'	REAL,
	'y'	REAL,
	'z'	REAL,
	FOREIGN KEY('level_info_id') REFERENCES 'level_info'('level_info_id'),
	PRIMARY KEY('sample_point_id' AUTOINCREMENT)
);
