# =============================================================================================== #
# Exported functions                                                                              #
# =============================================================================================== #

function (zyan_set_common_flags target)
    if (MSVC)
        # MSVC support for C11 is still pretty lacking, so we instead just disable the warnings
        # about using non-standard C extensions.
        target_compile_options("${target}" PUBLIC "/wd4201")
    else ()
        # For the more civilized compilers, we go with C11.
        set_target_properties("${target}" PROPERTIES C_STANDARD 11)
    endif ()

    if (ZYAN_DEV_MODE)
        # If in developer mode, be pedantic.
        if (MSVC)
            target_compile_options("${target}" PUBLIC "/WX" "/W4")
        else ()
            target_compile_options("${target}" PUBLIC "-Wall" "-pedantic" "-Wextra" "-Werror")
        endif ()
    endif ()
endfunction ()

function (zyan_set_source_group target)
    if (ZYAN_DEV_MODE)
        if (((CMAKE_MAJOR_VERSION GREATER 3) OR (CMAKE_MAJOR_VERSION EQUAL 3)) AND
            ((CMAKE_MINOR_VERSION GREATER 8) OR (CMAKE_MINOR_VERSION EQUAL 8)))
            # Mirror directory structure in project files
            get_property("TARGET_SOURCE_FILES" TARGET "${target}" PROPERTY SOURCES)
            source_group(TREE "${CMAKE_CURRENT_LIST_DIR}" FILES ${TARGET_SOURCE_FILES})
        endif ()
    endif ()
endfunction ()

function (zyan_maybe_enable_wpo target)
    if (ZYAN_WHOLE_PROGRAM_OPTIMIZATION)
        set_property(TARGET "${target}" PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)
    endif ()
endfunction ()

# deprecated alias for `zyan_maybe_enable_wpo`, for backward compatibility.
function (zyan_maybe_enable_wpo_for_lib target)
    zyan_maybe_enable_wpo("${target}")
endfunction ()
