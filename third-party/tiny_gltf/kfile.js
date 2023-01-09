let project = new Project('tiny_gltf');

project.addIncludeDir(__dirname);
project.addIncludeDir(__dirname+'/../stb_image');
project.addFiles(__dirname + '/tiny_gltf.cpp');

await project.addProject(__dirname+'/../stb_image');

project.addDefine("KINC_STATIC_COMPILE");
project.isStaticLib = true;
//Uncomment for DLL
//project.addDefine("KINC_DYNAMIC_COMPILE");
// project.addDefine("REPLXX_BUILDING_DLL");
// project.isDynamicLib = true;
project.setDebugDir('Deployment');

resolve(project);