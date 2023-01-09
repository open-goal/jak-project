let project = new Project('goalc');

// We need the libraries: common(DONE) Zydis(DONE) tiny_gltf(DONE) sqlite3(DONE) SQLiteCpp(DONE)
//common library needs: fmt(DONE) lzokay(DONE) replxx(DONE) libzstd_static(DONE)
await project.addProject(__dirname+'/../common');
await project.addProject(__dirname+'/../third-party/SQLiteCpp');
await project.addProject(__dirname+'/../third-party/zydis');

project.addIncludeDir(__dirname+'/..');
project.addExclude(__dirname+'/main.cpp');
project.addFile(__dirname+'/**');

project.setDebugDir('Deployment');

project.addDefine("KINC_STATIC_COMPILE");
project.isStaticLib = true;
//Uncomment for DLL
//project.addDefine("KINC_DYNAMIC_COMPILE");
// project.isDynamicLib = true;

resolve(project);
