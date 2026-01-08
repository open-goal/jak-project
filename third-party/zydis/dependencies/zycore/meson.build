project(
  'Zycore',
  'c',
  version: '1.5.2',
  license: 'MIT',
  license_files: 'LICENSE',
  meson_version: '>=1.3',
  default_options: [
    'c_std=c11',
    'cpp_std=c++17,vc++17',
    'warning_level=3',
  ],
)

cc = meson.get_compiler('c')

if cc.get_argument_syntax() == 'msvc'
  if get_option('b_lto')
    add_project_arguments(
      '/GL', # -flto
      language: ['c', 'cpp'],
    )
    add_project_link_arguments(
      '/LTCG',
      language: ['c', 'cpp'],
    )
  endif
endif

inc = include_directories('include')

dep = []

hdrs_api = files(
  # API
  'include/Zycore/API/Memory.h',
  'include/Zycore/API/Process.h',
  'include/Zycore/API/Synchronization.h',
  'include/Zycore/API/Terminal.h',
  'include/Zycore/API/Thread.h',
)

hdrs_common = files(
  # Common
  'include/Zycore/Allocator.h',
  'include/Zycore/ArgParse.h',
  'include/Zycore/Atomic.h',
  'include/Zycore/Bitset.h',
  'include/Zycore/Comparison.h',
  'include/Zycore/Defines.h',
  'include/Zycore/Format.h',
  'include/Zycore/LibC.h',
  'include/Zycore/List.h',
  'include/Zycore/Object.h',
  'include/Zycore/Status.h',
  'include/Zycore/String.h',
  'include/Zycore/Types.h',
  'include/Zycore/Vector.h',
  'include/Zycore/Zycore.h',
)

hdrs_internal = files(
  'include/Zycore/Internal/AtomicGNU.h',
  'include/Zycore/Internal/AtomicMSVC.h',
)

hdrs = hdrs_api + hdrs_common + hdrs_internal

src = files(
  # API
  'src/API/Memory.c',
  'src/API/Process.c',
  'src/API/Synchronization.c',
  'src/API/Terminal.c',
  'src/API/Thread.c',
  # Common
  'src/Allocator.c',
  'src/ArgParse.c',
  'src/Bitset.c',
  'src/Format.c',
  'src/List.c',
  'src/String.c',
  'src/Vector.c',
  'src/Zycore.c',
)

if host_machine.system() == 'windows'
  windows = import('windows')
  src += windows.compile_resources('resources/VersionInfo.rc')
endif

nolibc = get_option('nolibc')

examples = get_option('examples')
doc = get_option('doc')
tests = get_option('tests')

# Extra targets
examples = examples.require(not nolibc)
tests = tests.require(not nolibc)

doc = doc.disable_auto_if(meson.is_subproject())
examples = examples.disable_auto_if(meson.is_subproject())
tests = tests.disable_auto_if(meson.is_subproject())

if nolibc
  add_project_arguments(
    '-DZYAN_NO_LIBC',
    '-fno-stack-protector',
    language: 'c',
  )
elif host_machine.system() == 'linux'
  add_project_arguments(
    '-D_GNU_SOURCE',
    language: 'c',
  )
  dep += dependency('threads')
endif

zycore_lib = library(
  'Zycore',
  src + hdrs,
  c_static_args: ['-DZYCORE_STATIC_BUILD'],
  c_shared_args: ['-DZYCORE_SHOULD_EXPORT'],
  link_args: (nolibc and cc.get_linker_id().startswith('ld.')) ? ['-nostdlib', '-nodefaultlibs'] : [],
  gnu_symbol_visibility: 'hidden',
  include_directories: inc,
  implicit_include_directories: false,
  dependencies: dep,
  version: meson.project_version(),
  install: true,
)

install_headers(hdrs_common, subdir: 'Zycore')
install_headers(hdrs_api, subdir: 'Zycore/API')
install_headers(hdrs_internal, subdir: 'Zycore/Internal')

extra_cflags = nolibc ? ['-DZYAN_NO_LIBC'] : []

# Note: on MSVC, define ZYCORE_STATIC_BUILD accordingly in the user project.
zycore_dep = declare_dependency(
  include_directories: inc,
  link_with: zycore_lib,
  compile_args: extra_cflags,
)

subdir('examples')
subdir('tests')

pkg = import('pkgconfig')
pkg.generate(
  zycore_lib,
  name: 'zycore',
  description: 'Zyan Core Library for C',
  url: 'https://github.com/zyantific/zycore-c',
  extra_cflags: extra_cflags,
)

meson.override_dependency('zycore', zycore_dep)

doxygen_exe = find_program('doxygen', required: doc)
doc_req = doxygen_exe.found()
if doc_req
  cdata = configuration_data()
  cdata.set('VERSION', meson.project_version())
  cdata.set('TOP_SRCDIR', meson.project_source_root())
  cdata.set('TOP_BUILDDIR', meson.project_build_root())

  dot_exe = find_program('dot', required: false)
  if dot_exe.found()
    cdata.set('HAVE_DOT', 'YES')
    cdata.set('DOT_PATH', dot_exe.full_path())
    cdata.set(
      'HAVE_DOT_1_8_10',
      dot_exe.version().version_compare('>=1.8.10') ? 'YES' : 'NO',
    )
  else
    cdata.set('HAVE_DOT', 'NO')
  endif

  if find_program('pdf2svg', required: false).found() or find_program('inkscape', required: false).found()
    cdata.set('HTML_FORMULA_FORMAT', 'svg')
  else
    cdata.set('HTML_FORMULA_FORMAT', 'png')
  endif

  cdata.set('PREDEFINED', nolibc ? 'ZYAN_NO_LIBC' : '')

  doxyfile = configure_file(
    input: 'Doxyfile.in',
    output: 'Doxyfile',
    configuration: cdata,
    install: false,
  )

  datadir = join_paths(get_option('datadir'), 'doc', 'Zycore')
  custom_target(
    'ZycoreDoc',
    input: doxyfile,
    output: 'doc',
    command: [doxygen_exe, doxyfile],
    depend_files: [hdrs],
    install: true,
    install_dir: datadir,
  )

  summary(
    {
      'dot': cdata.get('HAVE_DOT') == 'YES',
      'formula format': cdata.get('HTML_FORMULA_FORMAT'),
    },
    section: 'Doxygen',
  )
endif

summary(
  {'doc': doc_req, 'nolibc': nolibc},
  section: 'Features',
)
