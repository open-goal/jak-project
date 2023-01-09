let project = new Project('zstd_static');

project.addIncludeDir(__dirname+'/lib');
project.addIncludeDir(__dirname+'/lib/common');
project.addFiles(__dirname + '/lib/**');

project.addDefine("KINC_STATIC_COMPILE");
project.isStaticLib = true;
project.setDebugDir('Deployment');

resolve(project);