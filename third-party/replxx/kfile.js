const fs = require('fs');
const path = require('path');
function getFileNames(dir) {
    // Use the fs.readdirSync() method to read the contents of the directory
    const files = fs.readdirSync(path.resolve(dir));
  
    // Add the names of the files to the array
    const fileNames = [];
    files.forEach(file => {
      fileNames.push(dir+'/'+file);
    });
  
    // Return the fileNames array
    return fileNames;
}
let project = new Project('replxx');

project.addIncludeDir('src');
project.addIncludeDir('include');
let list_of_src_files = getFileNames(__dirname+'/src');

for(i =0; i < list_of_src_files.length;++i){
    let str = list_of_src_files[i]
    if(str.indexOf('.cxx') > -1){
        str = str.replace('.cxx','.cpp');
        fs.renameSync(list_of_src_files[i],str);
    }

    list_of_src_files[i] = str;
}

project.addFiles(...list_of_src_files,'include/**','examples/util.h','examples/util.c');
// project.setCppStd('c++11');

project.addDefine("KINC_STATIC_COMPILE");
project.addDefine('REPLXX_STATIC');
project.isStaticLib = true;
//Uncomment for DLL
//project.addDefine("KINC_DYNAMIC_COMPILE");
// project.addDefine("REPLXX_BUILDING_DLL");
// project.isDynamicLib = true;
project.setDebugDir('Deployment');

resolve(project);

process.on('exit', (code) => {
    for(i =0; i < list_of_src_files.length;++i){
        let str = list_of_src_files[i]
        if(str.indexOf('.cpp') > -1 && str.indexOf('ConvertUTF') ===-1 && str.indexOf('wcwidth') ===-1){
            str = str.replace('.cpp','.cxx');
            fs.renameSync(list_of_src_files[i],str);
        }
    }
});
