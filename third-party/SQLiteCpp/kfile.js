let project = new Project('SQLiteCpp');

project.addIncludeDir(__dirname+'/include');
project.addFiles(__dirname + '/src/*');

await project.addProject(__dirname+'/sqlite3');

// project.setCppStd('c++11');

project.addDefine("KINC_STATIC_COMPILE");
project.isStaticLib = true;
//Uncomment for DLL
//project.addDefine("KINC_DYNAMIC_COMPILE");
// project.addDefine("REPLXX_BUILDING_DLL");
// project.isDynamicLib = true;
project.setDebugDir('Deployment');

resolve(project);