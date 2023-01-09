let project = new Project('Zydis');

project.addIncludeDir(__dirname+'/dependencies/zycore/include');
project.addFiles(__dirname + '/dependencies/zycore/src/**');

project.addIncludeDir(__dirname+'/include');
project.addIncludeDir(__dirname + '/src');
project.addFiles(__dirname + '/src/**');

project.addDefine("KINC_STATIC_COMPILE");
project.addDefine("ZYDIS_STATIC_BUILD");
project.isStaticLib = true;
//Uncomment for DLL
//project.addDefine("KINC_DYNAMIC_COMPILE");
// project.addDefine("ZYDIS_BUILD_SHARED_LIB");
// project.isDynamicLib = true;
project.setDebugDir('Deployment');

resolve(project);