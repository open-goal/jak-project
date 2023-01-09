let project = new Project('sqlite3');

project.addIncludeDir(__dirname);
project.addFiles(__dirname + '/**');

project.addDefine("KINC_STATIC_COMPILE");
project.isStaticLib = true;
//Uncomment for DLL
//project.addDefine("KINC_DYNAMIC_COMPILE");
// project.addDefine("REPLXX_BUILDING_DLL");
// project.isDynamicLib = true;
project.setDebugDir('Deployment');

project.addCFlag("-Wimplicit-fallthrough=0");
project.addCFlag("-Wno-cast-function-type");

resolve(project);