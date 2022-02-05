//========================================================================
// GLFW 3.4 Wayland - www.glfw.org
//------------------------------------------------------------------------
// Copyright (c) 2014 Jonas Ådahl <jadahl@gmail.com>
//
// This software is provided 'as-is', without any express or implied
// warranty. In no event will the authors be held liable for any damages
// arising from the use of this software.
//
// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it
// freely, subject to the following restrictions:
//
// 1. The origin of this software must not be misrepresented; you must not
//    claim that you wrote the original software. If you use this software
//    in a product, an acknowledgment in the product documentation would
//    be appreciated but is not required.
//
// 2. Altered source versions must be plainly marked as such, and must not
//    be misrepresented as being the original software.
//
// 3. This notice may not be removed or altered from any source
//    distribution.
//
//========================================================================

#include <wayland-client-core.h>
#include <xkbcommon/xkbcommon.h>
#include <xkbcommon/xkbcommon-compose.h>

typedef VkFlags VkWaylandSurfaceCreateFlagsKHR;

typedef struct VkWaylandSurfaceCreateInfoKHR
{
    VkStructureType                 sType;
    const void*                     pNext;
    VkWaylandSurfaceCreateFlagsKHR  flags;
    struct wl_display*              display;
    struct wl_surface*              surface;
} VkWaylandSurfaceCreateInfoKHR;

typedef VkResult (APIENTRY *PFN_vkCreateWaylandSurfaceKHR)(VkInstance,const VkWaylandSurfaceCreateInfoKHR*,const VkAllocationCallbacks*,VkSurfaceKHR*);
typedef VkBool32 (APIENTRY *PFN_vkGetPhysicalDeviceWaylandPresentationSupportKHR)(VkPhysicalDevice,uint32_t,struct wl_display*);

#include "xkb_unicode.h"

typedef int (* PFN_wl_display_flush)(struct wl_display *display);
typedef void (* PFN_wl_display_cancel_read)(struct wl_display *display);
typedef int (* PFN_wl_display_dispatch_pending)(struct wl_display *display);
typedef int (* PFN_wl_display_read_events)(struct wl_display *display);
typedef struct wl_display* (* PFN_wl_display_connect)(const char*);
typedef void (* PFN_wl_display_disconnect)(struct wl_display*);
typedef int (* PFN_wl_display_roundtrip)(struct wl_display*);
typedef int (* PFN_wl_display_get_fd)(struct wl_display*);
typedef int (* PFN_wl_display_prepare_read)(struct wl_display*);
typedef void (* PFN_wl_proxy_marshal)(struct wl_proxy*,uint32_t,...);
typedef int (* PFN_wl_proxy_add_listener)(struct wl_proxy*,void(**)(void),void*);
typedef void (* PFN_wl_proxy_destroy)(struct wl_proxy*);
typedef struct wl_proxy* (* PFN_wl_proxy_marshal_constructor)(struct wl_proxy*,uint32_t,const struct wl_interface*,...);
typedef struct wl_proxy* (* PFN_wl_proxy_marshal_constructor_versioned)(struct wl_proxy*,uint32_t,const struct wl_interface*,uint32_t,...);
typedef void* (* PFN_wl_proxy_get_user_data)(struct wl_proxy*);
typedef void (* PFN_wl_proxy_set_user_data)(struct wl_proxy*,void*);
typedef uint32_t (* PFN_wl_proxy_get_version)(struct wl_proxy*);
typedef struct wl_proxy* (* PFN_wl_proxy_marshal_flags)(struct wl_proxy*,uint32_t,const struct wl_interface*,uint32_t,uint32_t,...);
#define wl_display_flush _glfw.wl.client.display_flush
#define wl_display_cancel_read _glfw.wl.client.display_cancel_read
#define wl_display_dispatch_pending _glfw.wl.client.display_dispatch_pending
#define wl_display_read_events _glfw.wl.client.display_read_events
#define wl_display_disconnect _glfw.wl.client.display_disconnect
#define wl_display_roundtrip _glfw.wl.client.display_roundtrip
#define wl_display_get_fd _glfw.wl.client.display_get_fd
#define wl_display_prepare_read _glfw.wl.client.display_prepare_read
#define wl_proxy_marshal _glfw.wl.client.proxy_marshal
#define wl_proxy_add_listener _glfw.wl.client.proxy_add_listener
#define wl_proxy_destroy _glfw.wl.client.proxy_destroy
#define wl_proxy_marshal_constructor _glfw.wl.client.proxy_marshal_constructor
#define wl_proxy_marshal_constructor_versioned _glfw.wl.client.proxy_marshal_constructor_versioned
#define wl_proxy_get_user_data _glfw.wl.client.proxy_get_user_data
#define wl_proxy_set_user_data _glfw.wl.client.proxy_set_user_data
#define wl_proxy_get_version _glfw.wl.client.proxy_get_version
#define wl_proxy_marshal_flags _glfw.wl.client.proxy_marshal_flags

struct wl_shm;

#define wl_display_interface _glfw_wl_display_interface
#define wl_subcompositor_interface _glfw_wl_subcompositor_interface
#define wl_compositor_interface _glfw_wl_compositor_interface
#define wl_shm_interface _glfw_wl_shm_interface
#define wl_data_device_manager_interface _glfw_wl_data_device_manager_interface
#define wl_shell_interface _glfw_wl_shell_interface
#define wl_buffer_interface _glfw_wl_buffer_interface
#define wl_callback_interface _glfw_wl_callback_interface
#define wl_data_device_interface _glfw_wl_data_device_interface
#define wl_data_offer_interface _glfw_wl_data_offer_interface
#define wl_data_source_interface _glfw_wl_data_source_interface
#define wl_keyboard_interface _glfw_wl_keyboard_interface
#define wl_output_interface _glfw_wl_output_interface
#define wl_pointer_interface _glfw_wl_pointer_interface
#define wl_region_interface _glfw_wl_region_interface
#define wl_registry_interface _glfw_wl_registry_interface
#define wl_seat_interface _glfw_wl_seat_interface
#define wl_shell_surface_interface _glfw_wl_shell_surface_interface
#define wl_shm_pool_interface _glfw_wl_shm_pool_interface
#define wl_subsurface_interface _glfw_wl_subsurface_interface
#define wl_surface_interface _glfw_wl_surface_interface
#define wl_touch_interface _glfw_wl_touch_interface
#define zwp_idle_inhibitor_v1_interface _glfw_zwp_idle_inhibitor_v1_interface
#define zwp_idle_inhibit_manager_v1_interface _glfw_zwp_idle_inhibit_manager_v1_interface
#define zwp_confined_pointer_v1_interface _glfw_zwp_confined_pointer_v1_interface
#define zwp_locked_pointer_v1_interface _glfw_zwp_locked_pointer_v1_interface
#define zwp_pointer_constraints_v1_interface _glfw_zwp_pointer_constraints_v1_interface
#define zwp_relative_pointer_v1_interface _glfw_zwp_relative_pointer_v1_interface
#define zwp_relative_pointer_manager_v1_interface _glfw_zwp_relative_pointer_manager_v1_interface
#define wp_viewport_interface _glfw_wp_viewport_interface
#define wp_viewporter_interface _glfw_wp_viewporter_interface
#define xdg_toplevel_interface _glfw_xdg_toplevel_interface
#define zxdg_toplevel_decoration_v1_interface _glfw_zxdg_toplevel_decoration_v1_interface
#define zxdg_decoration_manager_v1_interface _glfw_zxdg_decoration_manager_v1_interface
#define xdg_popup_interface _glfw_xdg_popup_interface
#define xdg_positioner_interface _glfw_xdg_positioner_interface
#define xdg_surface_interface _glfw_xdg_surface_interface
#define xdg_toplevel_interface _glfw_xdg_toplevel_interface
#define xdg_wm_base_interface _glfw_xdg_wm_base_interface

#define GLFW_WAYLAND_WINDOW_STATE         _GLFWwindowWayland  wl;
#define GLFW_WAYLAND_LIBRARY_WINDOW_STATE _GLFWlibraryWayland wl;
#define GLFW_WAYLAND_MONITOR_STATE        _GLFWmonitorWayland wl;
#define GLFW_WAYLAND_CURSOR_STATE         _GLFWcursorWayland  wl;

struct wl_cursor_image {
    uint32_t width;
    uint32_t height;
    uint32_t hotspot_x;
    uint32_t hotspot_y;
    uint32_t delay;
};
struct wl_cursor {
    unsigned int image_count;
    struct wl_cursor_image** images;
    char* name;
};
typedef struct wl_cursor_theme* (* PFN_wl_cursor_theme_load)(const char*, int, struct wl_shm*);
typedef void (* PFN_wl_cursor_theme_destroy)(struct wl_cursor_theme*);
typedef struct wl_cursor* (* PFN_wl_cursor_theme_get_cursor)(struct wl_cursor_theme*, const char*);
typedef struct wl_buffer* (* PFN_wl_cursor_image_get_buffer)(struct wl_cursor_image*);
#define wl_cursor_theme_load _glfw.wl.cursor.theme_load
#define wl_cursor_theme_destroy _glfw.wl.cursor.theme_destroy
#define wl_cursor_theme_get_cursor _glfw.wl.cursor.theme_get_cursor
#define wl_cursor_image_get_buffer _glfw.wl.cursor.image_get_buffer

typedef struct wl_egl_window* (* PFN_wl_egl_window_create)(struct wl_surface*, int, int);
typedef void (* PFN_wl_egl_window_destroy)(struct wl_egl_window*);
typedef void (* PFN_wl_egl_window_resize)(struct wl_egl_window*, int, int, int, int);
#define wl_egl_window_create _glfw.wl.egl.window_create
#define wl_egl_window_destroy _glfw.wl.egl.window_destroy
#define wl_egl_window_resize _glfw.wl.egl.window_resize

typedef struct xkb_context* (* PFN_xkb_context_new)(enum xkb_context_flags);
typedef void (* PFN_xkb_context_unref)(struct xkb_context*);
typedef struct xkb_keymap* (* PFN_xkb_keymap_new_from_string)(struct xkb_context*, const char*, enum xkb_keymap_format, enum xkb_keymap_compile_flags);
typedef void (* PFN_xkb_keymap_unref)(struct xkb_keymap*);
typedef xkb_mod_index_t (* PFN_xkb_keymap_mod_get_index)(struct xkb_keymap*, const char*);
typedef int (* PFN_xkb_keymap_key_repeats)(struct xkb_keymap*, xkb_keycode_t);
typedef int (* PFN_xkb_keymap_key_get_syms_by_level)(struct xkb_keymap*,xkb_keycode_t,xkb_layout_index_t,xkb_level_index_t,const xkb_keysym_t**);
typedef struct xkb_state* (* PFN_xkb_state_new)(struct xkb_keymap*);
typedef void (* PFN_xkb_state_unref)(struct xkb_state*);
typedef int (* PFN_xkb_state_key_get_syms)(struct xkb_state*, xkb_keycode_t, const xkb_keysym_t**);
typedef enum xkb_state_component (* PFN_xkb_state_update_mask)(struct xkb_state*, xkb_mod_mask_t, xkb_mod_mask_t, xkb_mod_mask_t, xkb_layout_index_t, xkb_layout_index_t, xkb_layout_index_t);
typedef xkb_mod_mask_t (* PFN_xkb_state_serialize_mods)(struct xkb_state*, enum xkb_state_component);
typedef xkb_layout_index_t (* PFN_xkb_state_key_get_layout)(struct xkb_state*,xkb_keycode_t);
#define xkb_context_new _glfw.wl.xkb.context_new
#define xkb_context_unref _glfw.wl.xkb.context_unref
#define xkb_keymap_new_from_string _glfw.wl.xkb.keymap_new_from_string
#define xkb_keymap_unref _glfw.wl.xkb.keymap_unref
#define xkb_keymap_mod_get_index _glfw.wl.xkb.keymap_mod_get_index
#define xkb_keymap_key_repeats _glfw.wl.xkb.keymap_key_repeats
#define xkb_keymap_key_get_syms_by_level _glfw.wl.xkb.keymap_key_get_syms_by_level
#define xkb_state_new _glfw.wl.xkb.state_new
#define xkb_state_unref _glfw.wl.xkb.state_unref
#define xkb_state_key_get_syms _glfw.wl.xkb.state_key_get_syms
#define xkb_state_update_mask _glfw.wl.xkb.state_update_mask
#define xkb_state_serialize_mods _glfw.wl.xkb.state_serialize_mods
#define xkb_state_key_get_layout _glfw.wl.xkb.state_key_get_layout

typedef struct xkb_compose_table* (* PFN_xkb_compose_table_new_from_locale)(struct xkb_context*, const char*, enum xkb_compose_compile_flags);
typedef void (* PFN_xkb_compose_table_unref)(struct xkb_compose_table*);
typedef struct xkb_compose_state* (* PFN_xkb_compose_state_new)(struct xkb_compose_table*, enum xkb_compose_state_flags);
typedef void (* PFN_xkb_compose_state_unref)(struct xkb_compose_state*);
typedef enum xkb_compose_feed_result (* PFN_xkb_compose_state_feed)(struct xkb_compose_state*, xkb_keysym_t);
typedef enum xkb_compose_status (* PFN_xkb_compose_state_get_status)(struct xkb_compose_state*);
typedef xkb_keysym_t (* PFN_xkb_compose_state_get_one_sym)(struct xkb_compose_state*);
#define xkb_compose_table_new_from_locale _glfw.wl.xkb.compose_table_new_from_locale
#define xkb_compose_table_unref _glfw.wl.xkb.compose_table_unref
#define xkb_compose_state_new _glfw.wl.xkb.compose_state_new
#define xkb_compose_state_unref _glfw.wl.xkb.compose_state_unref
#define xkb_compose_state_feed _glfw.wl.xkb.compose_state_feed
#define xkb_compose_state_get_status _glfw.wl.xkb.compose_state_get_status
#define xkb_compose_state_get_one_sym _glfw.wl.xkb.compose_state_get_one_sym

#define _GLFW_DECORATION_WIDTH 4
#define _GLFW_DECORATION_TOP 24
#define _GLFW_DECORATION_VERTICAL (_GLFW_DECORATION_TOP + _GLFW_DECORATION_WIDTH)
#define _GLFW_DECORATION_HORIZONTAL (2 * _GLFW_DECORATION_WIDTH)

typedef enum _GLFWdecorationSideWayland
{
    mainWindow,
    topDecoration,
    leftDecoration,
    rightDecoration,
    bottomDecoration,
} _GLFWdecorationSideWayland;

typedef struct _GLFWdecorationWayland
{
    struct wl_surface*          surface;
    struct wl_subsurface*       subsurface;
    struct wp_viewport*         viewport;
} _GLFWdecorationWayland;

// Wayland-specific per-window data
//
typedef struct _GLFWwindowWayland
{
    int                         width, height;
    GLFWbool                    visible;
    GLFWbool                    maximized;
    GLFWbool                    hovered;
    GLFWbool                    transparent;
    struct wl_surface*          surface;
    struct wl_egl_window*       native;
    struct wl_callback*         callback;

    struct {
        struct xdg_surface*     surface;
        struct xdg_toplevel*    toplevel;
        struct zxdg_toplevel_decoration_v1* decoration;
    } xdg;

    _GLFWcursor*                currentCursor;
    double                      cursorPosX, cursorPosY;

    char*                       title;

    // We need to track the monitors the window spans on to calculate the
    // optimal scaling factor.
    int                         scale;
    _GLFWmonitor**              monitors;
    int                         monitorsCount;
    int                         monitorsSize;

    struct {
        struct zwp_relative_pointer_v1*    relativePointer;
        struct zwp_locked_pointer_v1*      lockedPointer;
    } pointerLock;

    struct zwp_idle_inhibitor_v1*          idleInhibitor;

    GLFWbool                    wasFullscreen;

    struct {
        GLFWbool                           serverSide;
        struct wl_buffer*                  buffer;
        _GLFWdecorationWayland             top, left, right, bottom;
        int                                focus;
    } decorations;
} _GLFWwindowWayland;

// Wayland-specific global data
//
typedef struct _GLFWlibraryWayland
{
    struct wl_display*          display;
    struct wl_registry*         registry;
    struct wl_compositor*       compositor;
    struct wl_subcompositor*    subcompositor;
    struct wl_shm*              shm;
    struct wl_seat*             seat;
    struct wl_pointer*          pointer;
    struct wl_keyboard*         keyboard;
    struct wl_data_device_manager*          dataDeviceManager;
    struct wl_data_device*      dataDevice;
    struct wl_data_offer*       dataOffer;
    struct wl_data_source*      dataSource;
    struct xdg_wm_base*         wmBase;
    struct zxdg_decoration_manager_v1*      decorationManager;
    struct wp_viewporter*       viewporter;
    struct zwp_relative_pointer_manager_v1* relativePointerManager;
    struct zwp_pointer_constraints_v1*      pointerConstraints;
    struct zwp_idle_inhibit_manager_v1*     idleInhibitManager;

    int                         compositorVersion;
    int                         seatVersion;

    struct wl_cursor_theme*     cursorTheme;
    struct wl_cursor_theme*     cursorThemeHiDPI;
    struct wl_surface*          cursorSurface;
    const char*                 cursorPreviousName;
    int                         cursorTimerfd;
    uint32_t                    serial;
    uint32_t                    pointerEnterSerial;

    int32_t                     keyboardRepeatRate;
    int32_t                     keyboardRepeatDelay;
    int                         keyboardLastKey;
    int                         keyboardLastScancode;
    char*                       clipboardString;
    size_t                      clipboardSize;
    char*                       clipboardSendString;
    size_t                      clipboardSendSize;
    int                         timerfd;
    short int                   keycodes[256];
    short int                   scancodes[GLFW_KEY_LAST + 1];
    char                        keynames[GLFW_KEY_LAST + 1][5];

    struct {
        void*                   handle;
        struct xkb_context*     context;
        struct xkb_keymap*      keymap;
        struct xkb_state*       state;

        struct xkb_compose_state* composeState;

        xkb_mod_mask_t          controlMask;
        xkb_mod_mask_t          altMask;
        xkb_mod_mask_t          shiftMask;
        xkb_mod_mask_t          superMask;
        xkb_mod_mask_t          capsLockMask;
        xkb_mod_mask_t          numLockMask;
        unsigned int            modifiers;

        PFN_xkb_context_new context_new;
        PFN_xkb_context_unref context_unref;
        PFN_xkb_keymap_new_from_string keymap_new_from_string;
        PFN_xkb_keymap_unref keymap_unref;
        PFN_xkb_keymap_mod_get_index keymap_mod_get_index;
        PFN_xkb_keymap_key_repeats keymap_key_repeats;
        PFN_xkb_keymap_key_get_syms_by_level keymap_key_get_syms_by_level;
        PFN_xkb_state_new state_new;
        PFN_xkb_state_unref state_unref;
        PFN_xkb_state_key_get_syms state_key_get_syms;
        PFN_xkb_state_update_mask state_update_mask;
        PFN_xkb_state_serialize_mods state_serialize_mods;
        PFN_xkb_state_key_get_layout state_key_get_layout;

        PFN_xkb_compose_table_new_from_locale compose_table_new_from_locale;
        PFN_xkb_compose_table_unref compose_table_unref;
        PFN_xkb_compose_state_new compose_state_new;
        PFN_xkb_compose_state_unref compose_state_unref;
        PFN_xkb_compose_state_feed compose_state_feed;
        PFN_xkb_compose_state_get_status compose_state_get_status;
        PFN_xkb_compose_state_get_one_sym compose_state_get_one_sym;
    } xkb;

    _GLFWwindow*                pointerFocus;
    _GLFWwindow*                keyboardFocus;

    struct {
        void*                                       handle;
        PFN_wl_display_flush                        display_flush;
        PFN_wl_display_cancel_read                  display_cancel_read;
        PFN_wl_display_dispatch_pending             display_dispatch_pending;
        PFN_wl_display_read_events                  display_read_events;
        PFN_wl_display_disconnect                   display_disconnect;
        PFN_wl_display_roundtrip                    display_roundtrip;
        PFN_wl_display_get_fd                       display_get_fd;
        PFN_wl_display_prepare_read                 display_prepare_read;
        PFN_wl_proxy_marshal                        proxy_marshal;
        PFN_wl_proxy_add_listener                   proxy_add_listener;
        PFN_wl_proxy_destroy                        proxy_destroy;
        PFN_wl_proxy_marshal_constructor            proxy_marshal_constructor;
        PFN_wl_proxy_marshal_constructor_versioned  proxy_marshal_constructor_versioned;
        PFN_wl_proxy_get_user_data                  proxy_get_user_data;
        PFN_wl_proxy_set_user_data                  proxy_set_user_data;
        PFN_wl_proxy_get_version                    proxy_get_version;
        PFN_wl_proxy_marshal_flags                  proxy_marshal_flags;
    } client;

    struct {
        void*                   handle;

        PFN_wl_cursor_theme_load theme_load;
        PFN_wl_cursor_theme_destroy theme_destroy;
        PFN_wl_cursor_theme_get_cursor theme_get_cursor;
        PFN_wl_cursor_image_get_buffer image_get_buffer;
    } cursor;

    struct {
        void*                   handle;

        PFN_wl_egl_window_create window_create;
        PFN_wl_egl_window_destroy window_destroy;
        PFN_wl_egl_window_resize window_resize;
    } egl;
} _GLFWlibraryWayland;

// Wayland-specific per-monitor data
//
typedef struct _GLFWmonitorWayland
{
    struct wl_output*           output;
    uint32_t                    name;
    int                         currentMode;

    int                         x;
    int                         y;
    int                         scale;
} _GLFWmonitorWayland;

// Wayland-specific per-cursor data
//
typedef struct _GLFWcursorWayland
{
    struct wl_cursor*           cursor;
    struct wl_cursor*           cursorHiDPI;
    struct wl_buffer*           buffer;
    int                         width, height;
    int                         xhot, yhot;
    int                         currentImage;
} _GLFWcursorWayland;

GLFWbool _glfwConnectWayland(int platformID, _GLFWplatform* platform);
int _glfwInitWayland(void);
void _glfwTerminateWayland(void);

int _glfwCreateWindowWayland(_GLFWwindow* window, const _GLFWwndconfig* wndconfig, const _GLFWctxconfig* ctxconfig, const _GLFWfbconfig* fbconfig);
void _glfwDestroyWindowWayland(_GLFWwindow* window);
void _glfwSetWindowTitleWayland(_GLFWwindow* window, const char* title);
void _glfwSetWindowIconWayland(_GLFWwindow* window, int count, const GLFWimage* images);
void _glfwGetWindowPosWayland(_GLFWwindow* window, int* xpos, int* ypos);
void _glfwSetWindowPosWayland(_GLFWwindow* window, int xpos, int ypos);
void _glfwGetWindowSizeWayland(_GLFWwindow* window, int* width, int* height);
void _glfwSetWindowSizeWayland(_GLFWwindow* window, int width, int height);
void _glfwSetWindowSizeLimitsWayland(_GLFWwindow* window, int minwidth, int minheight, int maxwidth, int maxheight);
void _glfwSetWindowAspectRatioWayland(_GLFWwindow* window, int numer, int denom);
void _glfwGetFramebufferSizeWayland(_GLFWwindow* window, int* width, int* height);
void _glfwGetWindowFrameSizeWayland(_GLFWwindow* window, int* left, int* top, int* right, int* bottom);
void _glfwGetWindowContentScaleWayland(_GLFWwindow* window, float* xscale, float* yscale);
void _glfwIconifyWindowWayland(_GLFWwindow* window);
void _glfwRestoreWindowWayland(_GLFWwindow* window);
void _glfwMaximizeWindowWayland(_GLFWwindow* window);
void _glfwShowWindowWayland(_GLFWwindow* window);
void _glfwHideWindowWayland(_GLFWwindow* window);
void _glfwRequestWindowAttentionWayland(_GLFWwindow* window);
void _glfwFocusWindowWayland(_GLFWwindow* window);
void _glfwSetWindowMonitorWayland(_GLFWwindow* window, _GLFWmonitor* monitor, int xpos, int ypos, int width, int height, int refreshRate);
int _glfwWindowFocusedWayland(_GLFWwindow* window);
int _glfwWindowIconifiedWayland(_GLFWwindow* window);
int _glfwWindowVisibleWayland(_GLFWwindow* window);
int _glfwWindowMaximizedWayland(_GLFWwindow* window);
int _glfwWindowHoveredWayland(_GLFWwindow* window);
int _glfwFramebufferTransparentWayland(_GLFWwindow* window);
void _glfwSetWindowResizableWayland(_GLFWwindow* window, GLFWbool enabled);
void _glfwSetWindowDecoratedWayland(_GLFWwindow* window, GLFWbool enabled);
void _glfwSetWindowFloatingWayland(_GLFWwindow* window, GLFWbool enabled);
float _glfwGetWindowOpacityWayland(_GLFWwindow* window);
void _glfwSetWindowOpacityWayland(_GLFWwindow* window, float opacity);
void _glfwSetWindowMousePassthroughWayland(_GLFWwindow* window, GLFWbool enabled);

void _glfwSetRawMouseMotionWayland(_GLFWwindow *window, GLFWbool enabled);
GLFWbool _glfwRawMouseMotionSupportedWayland(void);

void _glfwPollEventsWayland(void);
void _glfwWaitEventsWayland(void);
void _glfwWaitEventsTimeoutWayland(double timeout);
void _glfwPostEmptyEventWayland(void);

void _glfwGetCursorPosWayland(_GLFWwindow* window, double* xpos, double* ypos);
void _glfwSetCursorPosWayland(_GLFWwindow* window, double xpos, double ypos);
void _glfwSetCursorModeWayland(_GLFWwindow* window, int mode);
const char* _glfwGetScancodeNameWayland(int scancode);
int _glfwGetKeyScancodeWayland(int key);
int _glfwCreateCursorWayland(_GLFWcursor* cursor, const GLFWimage* image, int xhot, int yhot);
int _glfwCreateStandardCursorWayland(_GLFWcursor* cursor, int shape);
void _glfwDestroyCursorWayland(_GLFWcursor* cursor);
void _glfwSetCursorWayland(_GLFWwindow* window, _GLFWcursor* cursor);
void _glfwSetClipboardStringWayland(const char* string);
const char* _glfwGetClipboardStringWayland(void);

EGLenum _glfwGetEGLPlatformWayland(EGLint** attribs);
EGLNativeDisplayType _glfwGetEGLNativeDisplayWayland(void);
EGLNativeWindowType _glfwGetEGLNativeWindowWayland(_GLFWwindow* window);

void _glfwGetRequiredInstanceExtensionsWayland(char** extensions);
int _glfwGetPhysicalDevicePresentationSupportWayland(VkInstance instance, VkPhysicalDevice device, uint32_t queuefamily);
VkResult _glfwCreateWindowSurfaceWayland(VkInstance instance, _GLFWwindow* window, const VkAllocationCallbacks* allocator, VkSurfaceKHR* surface);

void _glfwFreeMonitorWayland(_GLFWmonitor* monitor);
void _glfwGetMonitorPosWayland(_GLFWmonitor* monitor, int* xpos, int* ypos);
void _glfwGetMonitorContentScaleWayland(_GLFWmonitor* monitor, float* xscale, float* yscale);
void _glfwGetMonitorWorkareaWayland(_GLFWmonitor* monitor, int* xpos, int* ypos, int* width, int* height);
GLFWvidmode* _glfwGetVideoModesWayland(_GLFWmonitor* monitor, int* count);
void _glfwGetVideoModeWayland(_GLFWmonitor* monitor, GLFWvidmode* mode);
GLFWbool _glfwGetGammaRampWayland(_GLFWmonitor* monitor, GLFWgammaramp* ramp);
void _glfwSetGammaRampWayland(_GLFWmonitor* monitor, const GLFWgammaramp* ramp);

void _glfwAddOutputWayland(uint32_t name, uint32_t version);
GLFWbool _glfwInputTextWayland(_GLFWwindow* window, uint32_t scancode);

