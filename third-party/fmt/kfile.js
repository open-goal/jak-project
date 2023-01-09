let project = new Project('fmt');

project.addIncludeDir(__dirname+'/..');
project.addFiles(__dirname + '/format.cc');

project.addDefine("KINC_STATIC_COMPILE");
project.isStaticLib = true;
//Uncomment for DLL
//project.addDefine("FMT_EXPORT");
// project.addDefine("FMT_SHARED");
// project.isDynamicLib = true;
project.setDebugDir('Deployment');

resolve(project);