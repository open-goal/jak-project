let project = new Project('stb_image');

project.addIncludeDir(__dirname);
project.addFiles(__dirname + '/stb_image.cpp');

project.addDefine("KINC_STATIC_COMPILE");
project.isStaticLib = true;
//Uncomment for DLL
//project.addDefine("KINC_DYNAMIC_COMPILE");
// project.addDefine("REPLXX_BUILDING_DLL");
// project.isDynamicLib = true;
project.setDebugDir('Deployment');

resolve(project);