const system = platform === Platform.Windows ? "win32" :
			   platform === Platform.Linux   ? "linux" :
			   platform === Platform.OSX     ? "macos" :
			   platform === Platform.HTML5   ? "html5" :
			   platform === Platform.Android ? "android" :
			   platform === Platform.iOS     ? "ios" :
			   								   "unknown";

let project = new Project('common');

project.addIncludeDir(__dirname+'/..');
project.addFiles(__dirname + '/**');

await project.addProject(__dirname+'/../third-party/replxx');
await project.addProject(__dirname+'/../third-party/lzokay');
await project.addProject(__dirname+'/../third-party/fmt');
await project.addProject(__dirname+'/../third-party/zstd');
await project.addProject(__dirname+'/../third-party/tiny_gltf');

project.setCppStd('c++17');
if(system === "win32"){
    project.addLib('wsock32');
    project.addLib('ws2_32');
    project.addLib('windowsapp');
}
else if(system === "linux"){
    project.addLib('stdc++fs');
}

project.addCppFlag('-mavx');
project.addDefine("KINC_STATIC_COMPILE");
project.isStaticLib = true;
project.setDebugDir('Deployment');

resolve(project);