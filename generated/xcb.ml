type xid = int

type file_descr = int

type ('flags, 'vals) mask = F of 'flags list | V of 'vals

type ('enum, 't) alt = E of 'enum | T of 't

let ( let* ) = Option.bind

let decode f buf ~at ~size =
  if Bytes.length buf < at + size - 1 then None else Some (f buf at, at + size)

let decode_char buf ~at = decode Bytes.get buf ~at ~size:1

let decode_uint8 buf ~at = decode Bytes.get_uint8 buf ~at ~size:1

let decode_int8 buf ~at = decode Bytes.get_int8 buf ~at ~size:1

let decode_bool buf ~at =
  decode_uint8 buf ~at |> Option.map (fun (n, at) -> (n <> 0, at))

let decode_uint16 buf ~at = decode Bytes.get_uint16_le buf ~at ~size:2

let decode_int16 buf ~at = decode Bytes.get_int16_le buf ~at ~size:2

let decode_int32 buf ~at = decode Bytes.get_int32_le buf ~at ~size:4

let decode_int64 buf ~at = decode Bytes.get_int64_le buf ~at ~size:8

let decode_float buf ~at =
  decode_int64 buf ~at
  |> Option.map (fun (n, at) -> (Int64.float_of_bits n, at))

let decode_file_descr buf ~at =
  decode Bytes.get_int16_le buf ~at ~size:2
  |> Option.map (fun (n, at) -> ((Obj.magic n : Unix.file_descr), at))

let decode_xid = decode_int16

let decode_enum decode to_int of_int buf ~at =
  match decode buf ~at with
  | None -> None
  | Some (n, at) -> (
      match of_int (to_int n) with None -> None | Some e -> Some (e, at))

let decode_list decode_item len buf ~at =
  let rec loop items at len =
    if len = 0 then Some (List.rev items, at)
    else
      match decode_item buf ~at with
      | None -> None
      | Some (item, at) -> loop (item :: items) at (len - 1)
  in
  loop [] at len

module Bigreq = struct
  type enable_reply = { maximum_request_length : int32 }

  let enable () : enable_reply Lwt.t = failwith "not implemented"
end
[@@warning "-27"]

module Xproto = struct
  type char2b = { byte1 : int; byte2 : int }

  let decode_char2b buf ~at : (char2b * int) option =
    let orig = at in
    let* byte1, at = decode_uint8 buf ~at in
    let* byte2, at = decode_uint8 buf ~at in
    ignore orig;
    Some ({ byte1; byte2 }, at)

  type window = xid

  let decode_window = decode_xid

  type pixmap = xid

  let decode_pixmap = decode_xid

  type cursor = xid

  let decode_cursor = decode_xid

  type font = xid

  let decode_font = decode_xid

  type gcontext = xid

  let decode_gcontext = decode_xid

  type colormap = xid

  let decode_colormap = decode_xid

  type atom = xid

  let decode_atom = decode_xid

  type drawable = xid

  let decode_drawable = decode_xid

  type fontable = xid

  let decode_fontable = decode_xid

  type bool32 = int32

  let decode_bool32 = decode_int32

  type visualid = int32

  let decode_visualid = decode_int32

  type timestamp = int32

  let decode_timestamp = decode_int32

  type keysym = int32

  let decode_keysym = decode_int32

  type keycode = int

  let decode_keycode = decode_uint8

  type keycode32 = int32

  let decode_keycode32 = decode_int32

  type button = int

  let decode_button = decode_uint8

  type point = { x : int; y : int }

  let decode_point buf ~at : (point * int) option =
    let orig = at in
    let* x, at = decode_int16 buf ~at in
    let* y, at = decode_int16 buf ~at in
    ignore orig;
    Some ({ x; y }, at)

  type rectangle = { x : int; y : int; width : int; height : int }

  let decode_rectangle buf ~at : (rectangle * int) option =
    let orig = at in
    let* x, at = decode_int16 buf ~at in
    let* y, at = decode_int16 buf ~at in
    let* width, at = decode_uint16 buf ~at in
    let* height, at = decode_uint16 buf ~at in
    ignore orig;
    Some ({ x; y; width; height }, at)

  type arc = {
    x : int;
    y : int;
    width : int;
    height : int;
    angle1 : int;
    angle2 : int;
  }

  let decode_arc buf ~at : (arc * int) option =
    let orig = at in
    let* x, at = decode_int16 buf ~at in
    let* y, at = decode_int16 buf ~at in
    let* width, at = decode_uint16 buf ~at in
    let* height, at = decode_uint16 buf ~at in
    let* angle1, at = decode_int16 buf ~at in
    let* angle2, at = decode_int16 buf ~at in
    ignore orig;
    Some ({ x; y; width; height; angle1; angle2 }, at)

  type format = { depth : int; bits_per_pixel : int; scanline_pad : int }

  let decode_format buf ~at : (format * int) option =
    let orig = at in
    let* depth, at = decode_uint8 buf ~at in
    let* bits_per_pixel, at = decode_uint8 buf ~at in
    let* scanline_pad, at = decode_uint8 buf ~at in
    let at = at + 5 in
    ignore orig;
    Some ({ depth; bits_per_pixel; scanline_pad }, at)

  type visual_class_enum =
    [ `Static_gray
    | `Gray_scale
    | `Static_color
    | `Pseudo_color
    | `True_color
    | `Direct_color ]

  let visual_class_enum_of_int : int -> visual_class_enum option = function
    | 0 -> Some `Static_gray
    | 1 -> Some `Gray_scale
    | 2 -> Some `Static_color
    | 3 -> Some `Pseudo_color
    | 4 -> Some `True_color
    | 5 -> Some `Direct_color
    | _ -> None

  type visualtype = {
    visual_id : visualid;
    class_ : visual_class_enum;
    bits_per_rgb_value : int;
    colormap_entries : int;
    red_mask : int32;
    green_mask : int32;
    blue_mask : int32;
  }

  let decode_visualtype buf ~at : (visualtype * int) option =
    let orig = at in
    let* visual_id, at = decode_visualid buf ~at in
    let* class_, at =
      decode_enum decode_uint8 (fun x -> x) visual_class_enum_of_int buf ~at
    in
    let* bits_per_rgb_value, at = decode_uint8 buf ~at in
    let* colormap_entries, at = decode_uint16 buf ~at in
    let* red_mask, at = decode_int32 buf ~at in
    let* green_mask, at = decode_int32 buf ~at in
    let* blue_mask, at = decode_int32 buf ~at in
    let at = at + 4 in
    ignore orig;
    Some
      ( {
          visual_id;
          class_;
          bits_per_rgb_value;
          colormap_entries;
          red_mask;
          green_mask;
          blue_mask;
        },
        at )

  type depth = { depth : int; visuals : visualtype list }

  let decode_depth buf ~at : (depth * int) option =
    let orig = at in
    let* depth, at = decode_uint8 buf ~at in
    let at = at + 1 in
    let* visuals_len, at = decode_uint16 buf ~at in
    let visuals_len = visuals_len in
    let at = at + 4 in
    let* visuals, at = decode_list decode_visualtype visuals_len buf ~at in
    ignore orig;
    Some ({ depth; visuals }, at)

  type event_mask_mask =
    ( [ `Key_press
      | `Key_release
      | `Button_press
      | `Button_release
      | `Enter_window
      | `Leave_window
      | `Pointer_motion
      | `Pointer_motion_hint
      | `Button1_motion
      | `Button2_motion
      | `Button3_motion
      | `Button4_motion
      | `Button5_motion
      | `Button_motion
      | `Keymap_state
      | `Exposure
      | `Visibility_change
      | `Structure_notify
      | `Resize_redirect
      | `Substructure_notify
      | `Substructure_redirect
      | `Focus_change
      | `Property_change
      | `Color_map_change
      | `Owner_grab_button ],
      [ `No_event ] )
    mask

  type backing_store_enum = [ `Not_useful | `When_mapped | `Always ]

  let backing_store_enum_of_int : int -> backing_store_enum option = function
    | 0 -> Some `Not_useful
    | 1 -> Some `When_mapped
    | 2 -> Some `Always
    | _ -> None

  type screen = {
    root : window;
    default_colormap : colormap;
    white_pixel : int32;
    black_pixel : int32;
    current_input_masks : event_mask_mask;
    width_in_pixels : int;
    height_in_pixels : int;
    width_in_millimeters : int;
    height_in_millimeters : int;
    min_installed_maps : int;
    max_installed_maps : int;
    root_visual : visualid;
    backing_stores : backing_store_enum;
    save_unders : bool;
    root_depth : int;
    allowed_depths : depth list;
  }

  let decode_screen buf ~at : (screen * int) option =
    let orig = at in
    let* root, at = decode_window buf ~at in
    let* default_colormap, at = decode_colormap buf ~at in
    let* white_pixel, at = decode_int32 buf ~at in
    let* black_pixel, at = decode_int32 buf ~at in
    let* current_input_masks, at = decode_int32 buf ~at in
    let* width_in_pixels, at = decode_uint16 buf ~at in
    let* height_in_pixels, at = decode_uint16 buf ~at in
    let* width_in_millimeters, at = decode_uint16 buf ~at in
    let* height_in_millimeters, at = decode_uint16 buf ~at in
    let* min_installed_maps, at = decode_uint16 buf ~at in
    let* max_installed_maps, at = decode_uint16 buf ~at in
    let* root_visual, at = decode_visualid buf ~at in
    let* backing_stores, at =
      decode_enum decode_char Char.code backing_store_enum_of_int buf ~at
    in
    let* save_unders, at = decode_bool buf ~at in
    let* root_depth, at = decode_uint8 buf ~at in
    let* allowed_depths_len, at = decode_uint8 buf ~at in
    let allowed_depths_len = allowed_depths_len in
    let* allowed_depths, at =
      decode_list decode_depth allowed_depths_len buf ~at
    in
    ignore orig;
    Some
      ( {
          root;
          default_colormap;
          white_pixel;
          black_pixel;
          current_input_masks = assert false;
          width_in_pixels;
          height_in_pixels;
          width_in_millimeters;
          height_in_millimeters;
          min_installed_maps;
          max_installed_maps;
          root_visual;
          backing_stores;
          save_unders;
          root_depth;
          allowed_depths;
        },
        at )

  type setup_request = {
    byte_order : int;
    protocol_major_version : int;
    protocol_minor_version : int;
    authorization_protocol_name : char list;
    authorization_protocol_data : char list;
  }

  let decode_setup_request buf ~at : (setup_request * int) option =
    let orig = at in
    let* byte_order, at = decode_uint8 buf ~at in
    let at = at + 1 in
    let* protocol_major_version, at = decode_uint16 buf ~at in
    let* protocol_minor_version, at = decode_uint16 buf ~at in
    let* authorization_protocol_name_len, at = decode_uint16 buf ~at in
    let authorization_protocol_name_len = authorization_protocol_name_len in
    let* authorization_protocol_data_len, at = decode_uint16 buf ~at in
    let authorization_protocol_data_len = authorization_protocol_data_len in
    let at = at + 2 in
    let* authorization_protocol_name, at =
      decode_list decode_char authorization_protocol_name_len buf ~at
    in
    let at = at + ((at - orig) mod 4) in
    let* authorization_protocol_data, at =
      decode_list decode_char authorization_protocol_data_len buf ~at
    in
    let at = at + ((at - orig) mod 4) in
    ignore orig;
    Some
      ( {
          byte_order;
          protocol_major_version;
          protocol_minor_version;
          authorization_protocol_name;
          authorization_protocol_data;
        },
        at )

  type setup_failed = {
    status : int;
    protocol_major_version : int;
    protocol_minor_version : int;
    length : int;
    reason : char list;
  }

  let decode_setup_failed buf ~at : (setup_failed * int) option =
    let orig = at in
    let* status, at = decode_uint8 buf ~at in
    let* reason_len, at = decode_uint8 buf ~at in
    let reason_len = reason_len in
    let* protocol_major_version, at = decode_uint16 buf ~at in
    let* protocol_minor_version, at = decode_uint16 buf ~at in
    let* length, at = decode_uint16 buf ~at in
    let* reason, at = decode_list decode_char reason_len buf ~at in
    ignore orig;
    Some
      ( {
          status;
          protocol_major_version;
          protocol_minor_version;
          length;
          reason;
        },
        at )

  type setup_authenticate = { status : int; length : int; reason : char list }

  let decode_setup_authenticate buf ~at : (setup_authenticate * int) option =
    let orig = at in
    let* status, at = decode_uint8 buf ~at in
    let at = at + 5 in
    let* length, at = decode_uint16 buf ~at in
    let* reason, at = decode_list decode_char (length * 4) buf ~at in
    ignore orig;
    Some ({ status; length; reason }, at)

  type image_order_enum = [ `Lsb_first | `Msb_first ]

  let image_order_enum_of_int : int -> image_order_enum option = function
    | 0 -> Some `Lsb_first
    | 1 -> Some `Msb_first
    | _ -> None

  type setup = {
    status : int;
    protocol_major_version : int;
    protocol_minor_version : int;
    length : int;
    release_number : int32;
    resource_id_base : int32;
    resource_id_mask : int32;
    motion_buffer_size : int32;
    maximum_request_length : int;
    image_byte_order : image_order_enum;
    bitmap_format_bit_order : image_order_enum;
    bitmap_format_scanline_unit : int;
    bitmap_format_scanline_pad : int;
    min_keycode : keycode;
    max_keycode : keycode;
    vendor : char list;
    pixmap_formats : format list;
    roots : screen list;
  }

  let decode_setup buf ~at : (setup * int) option =
    let orig = at in
    let* status, at = decode_uint8 buf ~at in
    let at = at + 1 in
    let* protocol_major_version, at = decode_uint16 buf ~at in
    let* protocol_minor_version, at = decode_uint16 buf ~at in
    let* length, at = decode_uint16 buf ~at in
    let* release_number, at = decode_int32 buf ~at in
    let* resource_id_base, at = decode_int32 buf ~at in
    let* resource_id_mask, at = decode_int32 buf ~at in
    let* motion_buffer_size, at = decode_int32 buf ~at in
    let* vendor_len, at = decode_uint16 buf ~at in
    let vendor_len = vendor_len in
    let* maximum_request_length, at = decode_uint16 buf ~at in
    let* roots_len, at = decode_uint8 buf ~at in
    let roots_len = roots_len in
    let* pixmap_formats_len, at = decode_uint8 buf ~at in
    let pixmap_formats_len = pixmap_formats_len in
    let* image_byte_order, at =
      decode_enum decode_uint8 (fun x -> x) image_order_enum_of_int buf ~at
    in
    let* bitmap_format_bit_order, at =
      decode_enum decode_uint8 (fun x -> x) image_order_enum_of_int buf ~at
    in
    let* bitmap_format_scanline_unit, at = decode_uint8 buf ~at in
    let* bitmap_format_scanline_pad, at = decode_uint8 buf ~at in
    let* min_keycode, at = decode_keycode buf ~at in
    let* max_keycode, at = decode_keycode buf ~at in
    let at = at + 4 in
    let* vendor, at = decode_list decode_char vendor_len buf ~at in
    let at = at + ((at - orig) mod 4) in
    let* pixmap_formats, at =
      decode_list decode_format pixmap_formats_len buf ~at
    in
    let* roots, at = decode_list decode_screen roots_len buf ~at in
    ignore orig;
    Some
      ( {
          status;
          protocol_major_version;
          protocol_minor_version;
          length;
          release_number;
          resource_id_base;
          resource_id_mask;
          motion_buffer_size;
          maximum_request_length;
          image_byte_order;
          bitmap_format_bit_order;
          bitmap_format_scanline_unit;
          bitmap_format_scanline_pad;
          min_keycode;
          max_keycode;
          vendor;
          pixmap_formats;
          roots;
        },
        at )

  type mod_mask_mask =
    [ `Shift | `Lock | `Control | `D1 | `D2 | `D3 | `D4 | `D5 | `Any ] list

  type key_but_mask_mask =
    [ `Shift
    | `Lock
    | `Control
    | `Mod1
    | `Mod2
    | `Mod3
    | `Mod4
    | `Mod5
    | `Button1
    | `Button2
    | `Button3
    | `Button4
    | `Button5 ]
    list

  type window_enum = [ `None ]

  let window_enum_of_int : int -> window_enum option = function
    | 0 -> Some `None
    | _ -> None

  type key_press_event = {
    detail : keycode;
    time : timestamp;
    root : window;
    event : window;
    child : (window_enum, window) alt;
    root_x : int;
    root_y : int;
    event_x : int;
    event_y : int;
    state : key_but_mask_mask;
    same_screen : bool;
  }

  type key_release_event = key_press_event

  type button_mask_mask = [ `D1 | `D2 | `D3 | `D4 | `D5 | `Any ] list

  type button_press_event = {
    detail : button;
    time : timestamp;
    root : window;
    event : window;
    child : (window_enum, window) alt;
    root_x : int;
    root_y : int;
    event_x : int;
    event_y : int;
    state : key_but_mask_mask;
    same_screen : bool;
  }

  type button_release_event = button_press_event

  type motion_enum = [ `Normal | `Hint ]

  let motion_enum_of_int : int -> motion_enum option = function
    | 0 -> Some `Normal
    | 1 -> Some `Hint
    | _ -> None

  type motion_notify_event = {
    detail : motion_enum;
    time : timestamp;
    root : window;
    event : window;
    child : (window_enum, window) alt;
    root_x : int;
    root_y : int;
    event_x : int;
    event_y : int;
    state : key_but_mask_mask;
    same_screen : bool;
  }

  type notify_detail_enum =
    [ `Ancestor
    | `Virtual
    | `Inferior
    | `Nonlinear
    | `Nonlinear_virtual
    | `Pointer
    | `Pointer_root
    | `None ]

  let notify_detail_enum_of_int : int -> notify_detail_enum option = function
    | 0 -> Some `Ancestor
    | 1 -> Some `Virtual
    | 2 -> Some `Inferior
    | 3 -> Some `Nonlinear
    | 4 -> Some `Nonlinear_virtual
    | 5 -> Some `Pointer
    | 6 -> Some `Pointer_root
    | 7 -> Some `None
    | _ -> None

  type notify_mode_enum = [ `Normal | `Grab | `Ungrab | `While_grabbed ]

  let notify_mode_enum_of_int : int -> notify_mode_enum option = function
    | 0 -> Some `Normal
    | 1 -> Some `Grab
    | 2 -> Some `Ungrab
    | 3 -> Some `While_grabbed
    | _ -> None

  type enter_notify_event = {
    detail : notify_detail_enum;
    time : timestamp;
    root : window;
    event : window;
    child : (window_enum, window) alt;
    root_x : int;
    root_y : int;
    event_x : int;
    event_y : int;
    state : key_but_mask_mask;
    mode : notify_mode_enum;
    same_screen_focus : char;
  }

  type leave_notify_event = enter_notify_event

  type focus_in_event = {
    detail : notify_detail_enum;
    event : window;
    mode : notify_mode_enum;
  }

  type focus_out_event = focus_in_event

  type keymap_notify_event = { keys : int list }

  type expose_event = {
    window : window;
    x : int;
    y : int;
    width : int;
    height : int;
    count : int;
  }

  type graphics_exposure_event = {
    drawable : drawable;
    x : int;
    y : int;
    width : int;
    height : int;
    minor_opcode : int;
    count : int;
    major_opcode : int;
  }

  type no_exposure_event = {
    drawable : drawable;
    minor_opcode : int;
    major_opcode : int;
  }

  type visibility_enum = [ `Unobscured | `Partially_obscured | `Fully_obscured ]

  let visibility_enum_of_int : int -> visibility_enum option = function
    | 0 -> Some `Unobscured
    | 1 -> Some `Partially_obscured
    | 2 -> Some `Fully_obscured
    | _ -> None

  type visibility_notify_event = { window : window; state : visibility_enum }

  type create_notify_event = {
    parent : window;
    window : window;
    x : int;
    y : int;
    width : int;
    height : int;
    border_width : int;
    override_redirect : bool;
  }

  type destroy_notify_event = { event : window; window : window }

  type unmap_notify_event = {
    event : window;
    window : window;
    from_configure : bool;
  }

  type map_notify_event = {
    event : window;
    window : window;
    override_redirect : bool;
  }

  type map_request_event = { parent : window; window : window }

  type reparent_notify_event = {
    event : window;
    window : window;
    parent : window;
    x : int;
    y : int;
    override_redirect : bool;
  }

  type configure_notify_event = {
    event : window;
    window : window;
    above_sibling : (window_enum, window) alt;
    x : int;
    y : int;
    width : int;
    height : int;
    border_width : int;
    override_redirect : bool;
  }

  type stack_mode_enum = [ `Above | `Below | `Top_if | `Bottom_if | `Opposite ]

  let stack_mode_enum_of_int : int -> stack_mode_enum option = function
    | 0 -> Some `Above
    | 1 -> Some `Below
    | 2 -> Some `Top_if
    | 3 -> Some `Bottom_if
    | 4 -> Some `Opposite
    | _ -> None

  type config_window_mask =
    [ `X | `Y | `Width | `Height | `Border_width | `Sibling | `Stack_mode ] list

  type configure_request_event = {
    stack_mode : stack_mode_enum;
    parent : window;
    window : window;
    sibling : (window_enum, window) alt;
    x : int;
    y : int;
    width : int;
    height : int;
    border_width : int;
    value_mask : config_window_mask;
  }

  type gravity_notify_event = {
    event : window;
    window : window;
    x : int;
    y : int;
  }

  type resize_request_event = { window : window; width : int; height : int }

  type place_enum = [ `On_top | `On_bottom ]

  let place_enum_of_int : int -> place_enum option = function
    | 0 -> Some `On_top
    | 1 -> Some `On_bottom
    | _ -> None

  type circulate_notify_event = {
    event : window;
    window : window;
    place : place_enum;
  }

  type circulate_request_event = circulate_notify_event

  type property_enum = [ `New_value | `Delete ]

  let property_enum_of_int : int -> property_enum option = function
    | 0 -> Some `New_value
    | 1 -> Some `Delete
    | _ -> None

  type property_notify_event = {
    window : window;
    atom : atom;
    time : timestamp;
    state : property_enum;
  }

  type selection_clear_event = {
    time : timestamp;
    owner : window;
    selection : atom;
  }

  type time_enum = [ `Current_time ]

  let time_enum_of_int : int -> time_enum option = function
    | 0 -> Some `Current_time
    | _ -> None

  type atom_enum =
    [ `None_any
    | `Primary
    | `Secondary
    | `Arc
    | `Atom
    | `Bitmap
    | `Cardinal
    | `Colormap
    | `Cursor
    | `Cut_buffer0
    | `Cut_buffer1
    | `Cut_buffer2
    | `Cut_buffer3
    | `Cut_buffer4
    | `Cut_buffer5
    | `Cut_buffer6
    | `Cut_buffer7
    | `Drawable
    | `Font
    | `Integer
    | `Pixmap
    | `Point
    | `Rectangle
    | `Resource_manager
    | `Rgb_color_map
    | `Rgb_best_map
    | `Rgb_blue_map
    | `Rgb_default_map
    | `Rgb_gray_map
    | `Rgb_green_map
    | `Rgb_red_map
    | `String
    | `Visualid
    | `Window
    | `Wm_command
    | `Wm_hints
    | `Wm_client_machine
    | `Wm_icon_name
    | `Wm_icon_size
    | `Wm_name
    | `Wm_normal_hints
    | `Wm_size_hints
    | `Wm_zoom_hints
    | `Min_space
    | `Norm_space
    | `Max_space
    | `End_space
    | `Superscript_x
    | `Superscript_y
    | `Subscript_x
    | `Subscript_y
    | `Underline_position
    | `Underline_thickness
    | `Strikeout_ascent
    | `Strikeout_descent
    | `Italic_angle
    | `X_height
    | `Quad_width
    | `Weight
    | `Point_size
    | `Resolution
    | `Copyright
    | `Notice
    | `Font_name
    | `Family_name
    | `Full_name
    | `Cap_height
    | `Wm_class
    | `Wm_transient_for ]

  let atom_enum_of_int : int -> atom_enum option = function
    | 0 -> Some `None_any
    | 1 -> Some `Primary
    | 2 -> Some `Secondary
    | 3 -> Some `Arc
    | 4 -> Some `Atom
    | 5 -> Some `Bitmap
    | 6 -> Some `Cardinal
    | 7 -> Some `Colormap
    | 8 -> Some `Cursor
    | 9 -> Some `Cut_buffer0
    | 10 -> Some `Cut_buffer1
    | 11 -> Some `Cut_buffer2
    | 12 -> Some `Cut_buffer3
    | 13 -> Some `Cut_buffer4
    | 14 -> Some `Cut_buffer5
    | 15 -> Some `Cut_buffer6
    | 16 -> Some `Cut_buffer7
    | 17 -> Some `Drawable
    | 18 -> Some `Font
    | 19 -> Some `Integer
    | 20 -> Some `Pixmap
    | 21 -> Some `Point
    | 22 -> Some `Rectangle
    | 23 -> Some `Resource_manager
    | 24 -> Some `Rgb_color_map
    | 25 -> Some `Rgb_best_map
    | 26 -> Some `Rgb_blue_map
    | 27 -> Some `Rgb_default_map
    | 28 -> Some `Rgb_gray_map
    | 29 -> Some `Rgb_green_map
    | 30 -> Some `Rgb_red_map
    | 31 -> Some `String
    | 32 -> Some `Visualid
    | 33 -> Some `Window
    | 34 -> Some `Wm_command
    | 35 -> Some `Wm_hints
    | 36 -> Some `Wm_client_machine
    | 37 -> Some `Wm_icon_name
    | 38 -> Some `Wm_icon_size
    | 39 -> Some `Wm_name
    | 40 -> Some `Wm_normal_hints
    | 41 -> Some `Wm_size_hints
    | 42 -> Some `Wm_zoom_hints
    | 43 -> Some `Min_space
    | 44 -> Some `Norm_space
    | 45 -> Some `Max_space
    | 46 -> Some `End_space
    | 47 -> Some `Superscript_x
    | 48 -> Some `Superscript_y
    | 49 -> Some `Subscript_x
    | 50 -> Some `Subscript_y
    | 51 -> Some `Underline_position
    | 52 -> Some `Underline_thickness
    | 53 -> Some `Strikeout_ascent
    | 54 -> Some `Strikeout_descent
    | 55 -> Some `Italic_angle
    | 56 -> Some `X_height
    | 57 -> Some `Quad_width
    | 58 -> Some `Weight
    | 59 -> Some `Point_size
    | 60 -> Some `Resolution
    | 61 -> Some `Copyright
    | 62 -> Some `Notice
    | 63 -> Some `Font_name
    | 64 -> Some `Family_name
    | 65 -> Some `Full_name
    | 66 -> Some `Cap_height
    | 67 -> Some `Wm_class
    | 68 -> Some `Wm_transient_for
    | _ -> None

  type selection_request_event = {
    time : (time_enum, timestamp) alt;
    owner : window;
    requestor : window;
    selection : atom;
    target : atom;
    property : (atom_enum, atom) alt;
  }

  type selection_notify_event = {
    time : (time_enum, timestamp) alt;
    requestor : window;
    selection : atom;
    target : atom;
    property : (atom_enum, atom) alt;
  }

  type colormap_state_enum = [ `Uninstalled | `Installed ]

  let colormap_state_enum_of_int : int -> colormap_state_enum option = function
    | 0 -> Some `Uninstalled
    | 1 -> Some `Installed
    | _ -> None

  type colormap_enum = [ `None ]

  let colormap_enum_of_int : int -> colormap_enum option = function
    | 0 -> Some `None
    | _ -> None

  type colormap_notify_event = {
    window : window;
    colormap : (colormap_enum, colormap) alt;
    new_ : bool;
    state : colormap_state_enum;
  }

  type client_message_data_format_enum = [ `Data8 | `Data16 | `Data32 ]

  let client_message_data_format_enum_of_int :
      int -> client_message_data_format_enum option = function
    | 8 -> Some `Data8
    | 16 -> Some `Data16
    | 32 -> Some `Data32
    | _ -> None

  type client_message_data_format_variant =
    | Data8 of { data8 : int list }
    | Data16 of { data16 : int list }
    | Data32 of { data32 : int32 list }

  type client_message_event = {
    window : window;
    type_ : atom;
    data : client_message_data_format_variant;
  }

  type mapping_enum = [ `Modifier | `Keyboard | `Pointer ]

  let mapping_enum_of_int : int -> mapping_enum option = function
    | 0 -> Some `Modifier
    | 1 -> Some `Keyboard
    | 2 -> Some `Pointer
    | _ -> None

  type mapping_notify_event = {
    request : mapping_enum;
    first_keycode : keycode;
    count : int;
  }

  type ge_generic_event = unit

  type request_error = {
    bad_value : int32;
    minor_opcode : int;
    major_opcode : int;
  }

  type value_error = {
    bad_value : int32;
    minor_opcode : int;
    major_opcode : int;
  }

  type window_error = value_error

  type pixmap_error = value_error

  type atom_error = value_error

  type cursor_error = value_error

  type font_error = value_error

  type match_error = request_error

  type drawable_error = value_error

  type access_error = request_error

  type alloc_error = request_error

  type colormap_error = value_error

  type g_context_error = value_error

  type id_choice_error = value_error

  type name_error = request_error

  type length_error = request_error

  type implementation_error = request_error

  type window_class_enum = [ `Copy_from_parent | `Input_output | `Input_only ]

  let window_class_enum_of_int : int -> window_class_enum option = function
    | 0 -> Some `Copy_from_parent
    | 1 -> Some `Input_output
    | 2 -> Some `Input_only
    | _ -> None

  type cw_mask =
    [ `Back_pixmap
    | `Back_pixel
    | `Border_pixmap
    | `Border_pixel
    | `Bit_gravity
    | `Win_gravity
    | `Backing_store
    | `Backing_planes
    | `Backing_pixel
    | `Override_redirect
    | `Save_under
    | `Event_mask
    | `Dont_propagate
    | `Colormap
    | `Cursor ]
    list

  type back_pixmap_enum = [ `None | `Parent_relative ]

  let back_pixmap_enum_of_int : int -> back_pixmap_enum option = function
    | 0 -> Some `None
    | 1 -> Some `Parent_relative
    | _ -> None

  type gravity_enum =
    [ `Bit_forget_win_unmap
    | `North_west
    | `North
    | `North_east
    | `West
    | `Center
    | `East
    | `South_west
    | `South
    | `South_east
    | `Static ]

  let gravity_enum_of_int : int -> gravity_enum option = function
    | 0 -> Some `Bit_forget_win_unmap
    | 1 -> Some `North_west
    | 2 -> Some `North
    | 3 -> Some `North_east
    | 4 -> Some `West
    | 5 -> Some `Center
    | 6 -> Some `East
    | 7 -> Some `South_west
    | 8 -> Some `South
    | 9 -> Some `South_east
    | 10 -> Some `Static
    | _ -> None

  type pixmap_enum = [ `None ]

  let pixmap_enum_of_int : int -> pixmap_enum option = function
    | 0 -> Some `None
    | _ -> None

  type cursor_enum = [ `None ]

  let cursor_enum_of_int : int -> cursor_enum option = function
    | 0 -> Some `None
    | _ -> None

  let create_window ~(depth : int) ~(wid : window) ~(parent : window) ~(x : int)
      ~(y : int) ~(width : int) ~(height : int) ~(border_width : int)
      ~(class_ : window_class_enum) ~(visual : visualid)
      ?(background_pixmap : (back_pixmap_enum, pixmap) alt option)
      ?(background_pixel : int32 option)
      ?(border_pixmap : (pixmap_enum, pixmap) alt option)
      ?(border_pixel : int32 option) ?(bit_gravity : gravity_enum option)
      ?(win_gravity : gravity_enum option)
      ?(backing_store : backing_store_enum option)
      ?(backing_planes : int32 option) ?(backing_pixel : int32 option)
      ?(override_redirect : bool32 option) ?(save_under : bool32 option)
      ?(event_mask : event_mask_mask option)
      ?(do_not_propogate_mask : event_mask_mask option)
      ?(colormap : (colormap_enum, colormap) alt option)
      ?(cursor : (cursor_enum, cursor) alt option) () : unit Lwt.t =
    failwith "not implemented"

  let change_window_attributes ~(window : window)
      ?(background_pixmap : (back_pixmap_enum, pixmap) alt option)
      ?(background_pixel : int32 option)
      ?(border_pixmap : (pixmap_enum, pixmap) alt option)
      ?(border_pixel : int32 option) ?(bit_gravity : gravity_enum option)
      ?(win_gravity : gravity_enum option)
      ?(backing_store : backing_store_enum option)
      ?(backing_planes : int32 option) ?(backing_pixel : int32 option)
      ?(override_redirect : bool32 option) ?(save_under : bool32 option)
      ?(event_mask : event_mask_mask option)
      ?(do_not_propogate_mask : event_mask_mask option)
      ?(colormap : (colormap_enum, colormap) alt option)
      ?(cursor : (cursor_enum, cursor) alt option) () : unit Lwt.t =
    failwith "not implemented"

  type map_state_enum = [ `Unmapped | `Unviewable | `Viewable ]

  let map_state_enum_of_int : int -> map_state_enum option = function
    | 0 -> Some `Unmapped
    | 1 -> Some `Unviewable
    | 2 -> Some `Viewable
    | _ -> None

  type get_window_attributes_reply = {
    backing_store : backing_store_enum;
    visual : visualid;
    class_ : window_class_enum;
    bit_gravity : gravity_enum;
    win_gravity : gravity_enum;
    backing_planes : int32;
    backing_pixel : int32;
    save_under : bool;
    map_is_installed : bool;
    map_state : map_state_enum;
    override_redirect : bool;
    colormap : (colormap_enum, colormap) alt;
    all_event_masks : event_mask_mask;
    your_event_mask : event_mask_mask;
    do_not_propagate_mask : event_mask_mask;
  }

  let get_window_attributes ~(window : window) () :
      get_window_attributes_reply Lwt.t =
    failwith "not implemented"

  let destroy_window ~(window : window) () : unit Lwt.t =
    failwith "not implemented"

  let destroy_subwindows ~(window : window) () : unit Lwt.t =
    failwith "not implemented"

  type set_mode_enum = [ `Insert | `Delete ]

  let set_mode_enum_of_int : int -> set_mode_enum option = function
    | 0 -> Some `Insert
    | 1 -> Some `Delete
    | _ -> None

  let change_save_set ~(mode : set_mode_enum) ~(window : window) () : unit Lwt.t
      =
    failwith "not implemented"

  let reparent_window ~(window : window) ~(parent : window) ~(x : int)
      ~(y : int) () : unit Lwt.t =
    failwith "not implemented"

  let map_window ~(window : window) () : unit Lwt.t = failwith "not implemented"

  let map_subwindows ~(window : window) () : unit Lwt.t =
    failwith "not implemented"

  let unmap_window ~(window : window) () : unit Lwt.t =
    failwith "not implemented"

  let unmap_subwindows ~(window : window) () : unit Lwt.t =
    failwith "not implemented"

  let configure_window ~(window : window) ?(x : int32 option)
      ?(y : int32 option) ?(width : int32 option) ?(height : int32 option)
      ?(border_width : int32 option)
      ?(sibling : (window_enum, window) alt option)
      ?(stack_mode : stack_mode_enum option) () : unit Lwt.t =
    failwith "not implemented"

  type circulate_enum = [ `Raise_lowest | `Lower_highest ]

  let circulate_enum_of_int : int -> circulate_enum option = function
    | 0 -> Some `Raise_lowest
    | 1 -> Some `Lower_highest
    | _ -> None

  let circulate_window ~(direction : circulate_enum) ~(window : window) () :
      unit Lwt.t =
    failwith "not implemented"

  type get_geometry_reply = {
    depth : int;
    root : window;
    x : int;
    y : int;
    width : int;
    height : int;
    border_width : int;
  }

  let get_geometry ~(drawable : drawable) () : get_geometry_reply Lwt.t =
    failwith "not implemented"

  type query_tree_reply = {
    root : window;
    parent : (window_enum, window) alt;
    children : window list;
  }

  let query_tree ~(window : window) () : query_tree_reply Lwt.t =
    failwith "not implemented"

  type intern_atom_reply = { atom : (atom_enum, atom) alt }

  let intern_atom ~(only_if_exists : bool) ~(name : char list) () :
      intern_atom_reply Lwt.t =
    failwith "not implemented"

  type get_atom_name_reply = { name : char list }

  let get_atom_name ~(atom : atom) () : get_atom_name_reply Lwt.t =
    failwith "not implemented"

  type prop_mode_enum = [ `Replace | `Prepend | `Append ]

  let prop_mode_enum_of_int : int -> prop_mode_enum option = function
    | 0 -> Some `Replace
    | 1 -> Some `Prepend
    | 2 -> Some `Append
    | _ -> None

  let change_property ~(mode : prop_mode_enum) ~(window : window)
      ~(property : atom) ~(type_ : atom) ~(format : int) ~(data_len : int32)
      ~(data : char list) () : unit Lwt.t =
    failwith "not implemented"

  let delete_property ~(window : window) ~(property : atom) () : unit Lwt.t =
    failwith "not implemented"

  type get_property_type_enum = [ `Any ]

  let get_property_type_enum_of_int : int -> get_property_type_enum option =
    function
    | 0 -> Some `Any
    | _ -> None

  type get_property_reply = {
    format : int;
    type_ : atom;
    bytes_after : int32;
    value_len : int32;
    value : char list;
  }

  let get_property ~(delete : bool) ~(window : window) ~(property : atom)
      ~(type_ : (get_property_type_enum, atom) alt) ~(long_offset : int32)
      ~(long_length : int32) () : get_property_reply Lwt.t =
    failwith "not implemented"

  type list_properties_reply = { atoms : atom list }

  let list_properties ~(window : window) () : list_properties_reply Lwt.t =
    failwith "not implemented"

  let set_selection_owner ~(owner : (window_enum, window) alt)
      ~(selection : atom) ~(time : (time_enum, timestamp) alt) () : unit Lwt.t =
    failwith "not implemented"

  type get_selection_owner_reply = { owner : (window_enum, window) alt }

  let get_selection_owner ~(selection : atom) () :
      get_selection_owner_reply Lwt.t =
    failwith "not implemented"

  let convert_selection ~(requestor : window) ~(selection : atom)
      ~(target : atom) ~(property : (atom_enum, atom) alt)
      ~(time : (time_enum, timestamp) alt) () : unit Lwt.t =
    failwith "not implemented"

  type send_event_dest_enum = [ `Pointer_window | `Item_focus ]

  let send_event_dest_enum_of_int : int -> send_event_dest_enum option =
    function
    | 0 -> Some `Pointer_window
    | 1 -> Some `Item_focus
    | _ -> None

  let send_event ~(propagate : bool)
      ~(destination : (send_event_dest_enum, window) alt)
      ~(event_mask : event_mask_mask) ~(event : char list) () : unit Lwt.t =
    failwith "not implemented"

  type grab_mode_enum = [ `Sync | `Async ]

  let grab_mode_enum_of_int : int -> grab_mode_enum option = function
    | 0 -> Some `Sync
    | 1 -> Some `Async
    | _ -> None

  type grab_status_enum =
    [ `Success | `Already_grabbed | `Invalid_time | `Not_viewable | `Frozen ]

  let grab_status_enum_of_int : int -> grab_status_enum option = function
    | 0 -> Some `Success
    | 1 -> Some `Already_grabbed
    | 2 -> Some `Invalid_time
    | 3 -> Some `Not_viewable
    | 4 -> Some `Frozen
    | _ -> None

  type grab_pointer_reply = { status : grab_status_enum }

  let grab_pointer ~(owner_events : bool) ~(grab_window : window)
      ~(event_mask : event_mask_mask) ~(pointer_mode : grab_mode_enum)
      ~(keyboard_mode : grab_mode_enum)
      ~(confine_to : (window_enum, window) alt)
      ~(cursor : (cursor_enum, cursor) alt) ~(time : (time_enum, timestamp) alt)
      () : grab_pointer_reply Lwt.t =
    failwith "not implemented"

  let ungrab_pointer ~(time : (time_enum, timestamp) alt) () : unit Lwt.t =
    failwith "not implemented"

  type button_index_enum = [ `Any | `D1 | `D2 | `D3 | `D4 | `D5 ]

  let button_index_enum_of_int : int -> button_index_enum option = function
    | 0 -> Some `Any
    | 1 -> Some `D1
    | 2 -> Some `D2
    | 3 -> Some `D3
    | 4 -> Some `D4
    | 5 -> Some `D5
    | _ -> None

  let grab_button ~(owner_events : bool) ~(grab_window : window)
      ~(event_mask : event_mask_mask) ~(pointer_mode : grab_mode_enum)
      ~(keyboard_mode : grab_mode_enum)
      ~(confine_to : (window_enum, window) alt)
      ~(cursor : (cursor_enum, cursor) alt) ~(button : button_index_enum)
      ~(modifiers : mod_mask_mask) () : unit Lwt.t =
    failwith "not implemented"

  let ungrab_button ~(button : button_index_enum) ~(grab_window : window)
      ~(modifiers : mod_mask_mask) () : unit Lwt.t =
    failwith "not implemented"

  let change_active_pointer_grab ~(cursor : (cursor_enum, cursor) alt)
      ~(time : (time_enum, timestamp) alt) ~(event_mask : event_mask_mask) () :
      unit Lwt.t =
    failwith "not implemented"

  type grab_keyboard_reply = { status : grab_status_enum }

  let grab_keyboard ~(owner_events : bool) ~(grab_window : window)
      ~(time : (time_enum, timestamp) alt) ~(pointer_mode : grab_mode_enum)
      ~(keyboard_mode : grab_mode_enum) () : grab_keyboard_reply Lwt.t =
    failwith "not implemented"

  let ungrab_keyboard ~(time : (time_enum, timestamp) alt) () : unit Lwt.t =
    failwith "not implemented"

  type grab_enum = [ `Any ]

  let grab_enum_of_int : int -> grab_enum option = function
    | 0 -> Some `Any
    | _ -> None

  let grab_key ~(owner_events : bool) ~(grab_window : window)
      ~(modifiers : mod_mask_mask) ~(key : (grab_enum, keycode) alt)
      ~(pointer_mode : grab_mode_enum) ~(keyboard_mode : grab_mode_enum) () :
      unit Lwt.t =
    failwith "not implemented"

  let ungrab_key ~(key : (grab_enum, keycode) alt) ~(grab_window : window)
      ~(modifiers : mod_mask_mask) () : unit Lwt.t =
    failwith "not implemented"

  type allow_enum =
    [ `Async_pointer
    | `Sync_pointer
    | `Replay_pointer
    | `Async_keyboard
    | `Sync_keyboard
    | `Replay_keyboard
    | `Async_both
    | `Sync_both ]

  let allow_enum_of_int : int -> allow_enum option = function
    | 0 -> Some `Async_pointer
    | 1 -> Some `Sync_pointer
    | 2 -> Some `Replay_pointer
    | 3 -> Some `Async_keyboard
    | 4 -> Some `Sync_keyboard
    | 5 -> Some `Replay_keyboard
    | 6 -> Some `Async_both
    | 7 -> Some `Sync_both
    | _ -> None

  let allow_events ~(mode : allow_enum) ~(time : (time_enum, timestamp) alt) ()
      : unit Lwt.t =
    failwith "not implemented"

  let grab_server () : unit Lwt.t = failwith "not implemented"

  let ungrab_server () : unit Lwt.t = failwith "not implemented"

  type query_pointer_reply = {
    same_screen : bool;
    root : window;
    child : (window_enum, window) alt;
    root_x : int;
    root_y : int;
    win_x : int;
    win_y : int;
    mask : key_but_mask_mask;
  }

  let query_pointer ~(window : window) () : query_pointer_reply Lwt.t =
    failwith "not implemented"

  type timecoord = { time : timestamp; x : int; y : int }

  let decode_timecoord buf ~at : (timecoord * int) option =
    let orig = at in
    let* time, at = decode_timestamp buf ~at in
    let* x, at = decode_int16 buf ~at in
    let* y, at = decode_int16 buf ~at in
    ignore orig;
    Some ({ time; x; y }, at)

  type get_motion_events_reply = { events : timecoord list }

  let get_motion_events ~(window : window) ~(start : (time_enum, timestamp) alt)
      ~(stop : (time_enum, timestamp) alt) () : get_motion_events_reply Lwt.t =
    failwith "not implemented"

  type translate_coordinates_reply = {
    same_screen : bool;
    child : (window_enum, window) alt;
    dst_x : int;
    dst_y : int;
  }

  let translate_coordinates ~(src_window : window) ~(dst_window : window)
      ~(src_x : int) ~(src_y : int) () : translate_coordinates_reply Lwt.t =
    failwith "not implemented"

  let warp_pointer ~(src_window : (window_enum, window) alt)
      ~(dst_window : (window_enum, window) alt) ~(src_x : int) ~(src_y : int)
      ~(src_width : int) ~(src_height : int) ~(dst_x : int) ~(dst_y : int) () :
      unit Lwt.t =
    failwith "not implemented"

  type input_focus_enum = [ `None | `Pointer_root | `Parent | `Follow_keyboard ]

  let input_focus_enum_of_int : int -> input_focus_enum option = function
    | 0 -> Some `None
    | 1 -> Some `Pointer_root
    | 2 -> Some `Parent
    | 3 -> Some `Follow_keyboard
    | _ -> None

  let set_input_focus ~(revert_to : input_focus_enum)
      ~(focus : (input_focus_enum, window) alt)
      ~(time : (time_enum, timestamp) alt) () : unit Lwt.t =
    failwith "not implemented"

  type get_input_focus_reply = {
    revert_to : input_focus_enum;
    focus : (input_focus_enum, window) alt;
  }

  let get_input_focus () : get_input_focus_reply Lwt.t =
    failwith "not implemented"

  type query_keymap_reply = { keys : int list }

  let query_keymap () : query_keymap_reply Lwt.t = failwith "not implemented"

  let open_font ~(fid : font) ~(name : char list) () : unit Lwt.t =
    failwith "not implemented"

  let close_font ~(font : font) () : unit Lwt.t = failwith "not implemented"

  type font_draw_enum = [ `Left_to_right | `Right_to_left ]

  let font_draw_enum_of_int : int -> font_draw_enum option = function
    | 0 -> Some `Left_to_right
    | 1 -> Some `Right_to_left
    | _ -> None

  type fontprop = { name : atom; value : int32 }

  let decode_fontprop buf ~at : (fontprop * int) option =
    let orig = at in
    let* name, at = decode_atom buf ~at in
    let* value, at = decode_int32 buf ~at in
    ignore orig;
    Some ({ name; value }, at)

  type charinfo = {
    left_side_bearing : int;
    right_side_bearing : int;
    character_width : int;
    ascent : int;
    descent : int;
    attributes : int;
  }

  let decode_charinfo buf ~at : (charinfo * int) option =
    let orig = at in
    let* left_side_bearing, at = decode_int16 buf ~at in
    let* right_side_bearing, at = decode_int16 buf ~at in
    let* character_width, at = decode_int16 buf ~at in
    let* ascent, at = decode_int16 buf ~at in
    let* descent, at = decode_int16 buf ~at in
    let* attributes, at = decode_uint16 buf ~at in
    ignore orig;
    Some
      ( {
          left_side_bearing;
          right_side_bearing;
          character_width;
          ascent;
          descent;
          attributes;
        },
        at )

  type query_font_reply = {
    min_bounds : charinfo;
    max_bounds : charinfo;
    min_char_or_byte2 : int;
    max_char_or_byte2 : int;
    default_char : int;
    draw_direction : font_draw_enum;
    min_byte1 : int;
    max_byte1 : int;
    all_chars_exist : bool;
    font_ascent : int;
    font_descent : int;
    properties : fontprop list;
    char_infos : charinfo list;
  }

  let query_font ~(font : fontable) () : query_font_reply Lwt.t =
    failwith "not implemented"

  type query_text_extents_reply = {
    draw_direction : font_draw_enum;
    font_ascent : int;
    font_descent : int;
    overall_ascent : int;
    overall_descent : int;
    overall_width : int32;
    overall_left : int32;
    overall_right : int32;
  }

  let query_text_extents ~(font : fontable) ~(string : char2b list) () :
      query_text_extents_reply Lwt.t =
    failwith "not implemented"

  type str = { name : char list }

  let decode_str buf ~at : (str * int) option =
    let orig = at in
    let* name_len, at = decode_uint8 buf ~at in
    let name_len = name_len in
    let* name, at = decode_list decode_char name_len buf ~at in
    ignore orig;
    Some ({ name }, at)

  type list_fonts_reply = { names : str list }

  let list_fonts ~(max_names : int) ~(pattern : char list) () :
      list_fonts_reply Lwt.t =
    failwith "not implemented"

  type list_fonts_with_info_reply = {
    min_bounds : charinfo;
    max_bounds : charinfo;
    min_char_or_byte2 : int;
    max_char_or_byte2 : int;
    default_char : int;
    draw_direction : font_draw_enum;
    min_byte1 : int;
    max_byte1 : int;
    all_chars_exist : bool;
    font_ascent : int;
    font_descent : int;
    replies_hint : int32;
    properties : fontprop list;
    name : char list;
  }

  let list_fonts_with_info ~(max_names : int) ~(pattern : char list) () :
      list_fonts_with_info_reply Lwt.t =
    failwith "not implemented"

  let set_font_path ~(font : str list) () : unit Lwt.t =
    failwith "not implemented"

  type get_font_path_reply = { path : str list }

  let get_font_path () : get_font_path_reply Lwt.t = failwith "not implemented"

  let create_pixmap ~(depth : int) ~(pid : pixmap) ~(drawable : drawable)
      ~(width : int) ~(height : int) () : unit Lwt.t =
    failwith "not implemented"

  let free_pixmap ~(pixmap : pixmap) () : unit Lwt.t =
    failwith "not implemented"

  type gc_mask =
    [ `Function
    | `Plane_mask
    | `Foreground
    | `Background
    | `Line_width
    | `Line_style
    | `Cap_style
    | `Join_style
    | `Fill_style
    | `Fill_rule
    | `Tile
    | `Stipple
    | `Tile_stipple_origin_x
    | `Tile_stipple_origin_y
    | `Font
    | `Subwindow_mode
    | `Graphics_exposures
    | `Clip_origin_x
    | `Clip_origin_y
    | `Clip_mask
    | `Dash_offset
    | `Dash_list
    | `Arc_mode ]
    list

  type gx_enum =
    [ `Clear
    | `And
    | `And_reverse
    | `Copy
    | `And_inverted
    | `Noop
    | `Xor
    | `Or
    | `Nor
    | `Equiv
    | `Invert
    | `Or_reverse
    | `Copy_inverted
    | `Or_inverted
    | `Nand
    | `Set ]

  let gx_enum_of_int : int -> gx_enum option = function
    | 0 -> Some `Clear
    | 1 -> Some `And
    | 2 -> Some `And_reverse
    | 3 -> Some `Copy
    | 4 -> Some `And_inverted
    | 5 -> Some `Noop
    | 6 -> Some `Xor
    | 7 -> Some `Or
    | 8 -> Some `Nor
    | 9 -> Some `Equiv
    | 10 -> Some `Invert
    | 11 -> Some `Or_reverse
    | 12 -> Some `Copy_inverted
    | 13 -> Some `Or_inverted
    | 14 -> Some `Nand
    | 15 -> Some `Set
    | _ -> None

  type line_style_enum = [ `Solid | `On_off_dash | `Double_dash ]

  let line_style_enum_of_int : int -> line_style_enum option = function
    | 0 -> Some `Solid
    | 1 -> Some `On_off_dash
    | 2 -> Some `Double_dash
    | _ -> None

  type cap_style_enum = [ `Not_last | `Butt | `Round | `Projecting ]

  let cap_style_enum_of_int : int -> cap_style_enum option = function
    | 0 -> Some `Not_last
    | 1 -> Some `Butt
    | 2 -> Some `Round
    | 3 -> Some `Projecting
    | _ -> None

  type join_style_enum = [ `Miter | `Round | `Bevel ]

  let join_style_enum_of_int : int -> join_style_enum option = function
    | 0 -> Some `Miter
    | 1 -> Some `Round
    | 2 -> Some `Bevel
    | _ -> None

  type fill_style_enum = [ `Solid | `Tiled | `Stippled | `Opaque_stippled ]

  let fill_style_enum_of_int : int -> fill_style_enum option = function
    | 0 -> Some `Solid
    | 1 -> Some `Tiled
    | 2 -> Some `Stippled
    | 3 -> Some `Opaque_stippled
    | _ -> None

  type fill_rule_enum = [ `Even_odd | `Winding ]

  let fill_rule_enum_of_int : int -> fill_rule_enum option = function
    | 0 -> Some `Even_odd
    | 1 -> Some `Winding
    | _ -> None

  type subwindow_mode_enum = [ `Clip_by_children | `Include_inferiors ]

  let subwindow_mode_enum_of_int : int -> subwindow_mode_enum option = function
    | 0 -> Some `Clip_by_children
    | 1 -> Some `Include_inferiors
    | _ -> None

  type arc_mode_enum = [ `Chord | `Pie_slice ]

  let arc_mode_enum_of_int : int -> arc_mode_enum option = function
    | 0 -> Some `Chord
    | 1 -> Some `Pie_slice
    | _ -> None

  type font_enum = [ `None ]

  let font_enum_of_int : int -> font_enum option = function
    | 0 -> Some `None
    | _ -> None

  let create_gc ~(cid : gcontext) ~(drawable : drawable)
      ?(function_ : gx_enum option) ?(plane_mask : int32 option)
      ?(foreground : int32 option) ?(background : int32 option)
      ?(line_width : int32 option) ?(line_style : line_style_enum option)
      ?(cap_style : cap_style_enum option)
      ?(join_style : join_style_enum option)
      ?(fill_style : fill_style_enum option)
      ?(fill_rule : fill_rule_enum option)
      ?(tile : (pixmap_enum, pixmap) alt option)
      ?(stipple : (pixmap_enum, pixmap) alt option)
      ?(tile_stipple_x_origin : int32 option)
      ?(tile_stipple_y_origin : int32 option)
      ?(font : (font_enum, font) alt option)
      ?(subwindow_mode : subwindow_mode_enum option)
      ?(graphics_exposures : bool32 option) ?(clip_x_origin : int32 option)
      ?(clip_y_origin : int32 option)
      ?(clip_mask : (pixmap_enum, pixmap) alt option)
      ?(dash_offset : int32 option) ?(dashes : int32 option)
      ?(arc_mode : arc_mode_enum option) () : unit Lwt.t =
    failwith "not implemented"

  let change_gc ~(gc : gcontext) ?(function_ : gx_enum option)
      ?(plane_mask : int32 option) ?(foreground : int32 option)
      ?(background : int32 option) ?(line_width : int32 option)
      ?(line_style : line_style_enum option)
      ?(cap_style : cap_style_enum option)
      ?(join_style : join_style_enum option)
      ?(fill_style : fill_style_enum option)
      ?(fill_rule : fill_rule_enum option)
      ?(tile : (pixmap_enum, pixmap) alt option)
      ?(stipple : (pixmap_enum, pixmap) alt option)
      ?(tile_stipple_x_origin : int32 option)
      ?(tile_stipple_y_origin : int32 option)
      ?(font : (font_enum, font) alt option)
      ?(subwindow_mode : subwindow_mode_enum option)
      ?(graphics_exposures : bool32 option) ?(clip_x_origin : int32 option)
      ?(clip_y_origin : int32 option)
      ?(clip_mask : (pixmap_enum, pixmap) alt option)
      ?(dash_offset : int32 option) ?(dashes : int32 option)
      ?(arc_mode : arc_mode_enum option) () : unit Lwt.t =
    failwith "not implemented"

  let copy_gc ~(src_gc : gcontext) ~(dst_gc : gcontext) ~(value_mask : gc_mask)
      () : unit Lwt.t =
    failwith "not implemented"

  let set_dashes ~(gc : gcontext) ~(dash_offset : int) ~(dashes : int list) () :
      unit Lwt.t =
    failwith "not implemented"

  type clip_ordering_enum = [ `Unsorted | `Y_sorted | `Yx_sorted | `Yx_banded ]

  let clip_ordering_enum_of_int : int -> clip_ordering_enum option = function
    | 0 -> Some `Unsorted
    | 1 -> Some `Y_sorted
    | 2 -> Some `Yx_sorted
    | 3 -> Some `Yx_banded
    | _ -> None

  let set_clip_rectangles ~(ordering : clip_ordering_enum) ~(gc : gcontext)
      ~(clip_x_origin : int) ~(clip_y_origin : int)
      ~(rectangles : rectangle list) () : unit Lwt.t =
    failwith "not implemented"

  let free_gc ~(gc : gcontext) () : unit Lwt.t = failwith "not implemented"

  let clear_area ~(exposures : bool) ~(window : window) ~(x : int) ~(y : int)
      ~(width : int) ~(height : int) () : unit Lwt.t =
    failwith "not implemented"

  let copy_area ~(src_drawable : drawable) ~(dst_drawable : drawable)
      ~(gc : gcontext) ~(src_x : int) ~(src_y : int) ~(dst_x : int)
      ~(dst_y : int) ~(width : int) ~(height : int) () : unit Lwt.t =
    failwith "not implemented"

  let copy_plane ~(src_drawable : drawable) ~(dst_drawable : drawable)
      ~(gc : gcontext) ~(src_x : int) ~(src_y : int) ~(dst_x : int)
      ~(dst_y : int) ~(width : int) ~(height : int) ~(bit_plane : int32) () :
      unit Lwt.t =
    failwith "not implemented"

  type coord_mode_enum = [ `Origin | `Previous ]

  let coord_mode_enum_of_int : int -> coord_mode_enum option = function
    | 0 -> Some `Origin
    | 1 -> Some `Previous
    | _ -> None

  let poly_point ~(coordinate_mode : coord_mode_enum) ~(drawable : drawable)
      ~(gc : gcontext) ~(points : point list) () : unit Lwt.t =
    failwith "not implemented"

  let poly_line ~(coordinate_mode : coord_mode_enum) ~(drawable : drawable)
      ~(gc : gcontext) ~(points : point list) () : unit Lwt.t =
    failwith "not implemented"

  type segment = { x1 : int; y1 : int; x2 : int; y2 : int }

  let decode_segment buf ~at : (segment * int) option =
    let orig = at in
    let* x1, at = decode_int16 buf ~at in
    let* y1, at = decode_int16 buf ~at in
    let* x2, at = decode_int16 buf ~at in
    let* y2, at = decode_int16 buf ~at in
    ignore orig;
    Some ({ x1; y1; x2; y2 }, at)

  let poly_segment ~(drawable : drawable) ~(gc : gcontext)
      ~(segments : segment list) () : unit Lwt.t =
    failwith "not implemented"

  let poly_rectangle ~(drawable : drawable) ~(gc : gcontext)
      ~(rectangles : rectangle list) () : unit Lwt.t =
    failwith "not implemented"

  let poly_arc ~(drawable : drawable) ~(gc : gcontext) ~(arcs : arc list) () :
      unit Lwt.t =
    failwith "not implemented"

  type poly_shape_enum = [ `Complex | `Nonconvex | `Convex ]

  let poly_shape_enum_of_int : int -> poly_shape_enum option = function
    | 0 -> Some `Complex
    | 1 -> Some `Nonconvex
    | 2 -> Some `Convex
    | _ -> None

  let fill_poly ~(drawable : drawable) ~(gc : gcontext)
      ~(shape : poly_shape_enum) ~(coordinate_mode : coord_mode_enum)
      ~(points : point list) () : unit Lwt.t =
    failwith "not implemented"

  let poly_fill_rectangle ~(drawable : drawable) ~(gc : gcontext)
      ~(rectangles : rectangle list) () : unit Lwt.t =
    failwith "not implemented"

  let poly_fill_arc ~(drawable : drawable) ~(gc : gcontext) ~(arcs : arc list)
      () : unit Lwt.t =
    failwith "not implemented"

  type image_format_enum = [ `Xy_bitmap | `Xy_pixmap | `Z_pixmap ]

  let image_format_enum_of_int : int -> image_format_enum option = function
    | 0 -> Some `Xy_bitmap
    | 1 -> Some `Xy_pixmap
    | 2 -> Some `Z_pixmap
    | _ -> None

  let put_image ~(format : image_format_enum) ~(drawable : drawable)
      ~(gc : gcontext) ~(width : int) ~(height : int) ~(dst_x : int)
      ~(dst_y : int) ~(left_pad : int) ~(depth : int) ~(data : char list) () :
      unit Lwt.t =
    failwith "not implemented"

  type get_image_reply = { depth : int; visual : visualid; data : char list }

  let get_image ~(format : image_format_enum) ~(drawable : drawable) ~(x : int)
      ~(y : int) ~(width : int) ~(height : int) ~(plane_mask : int32) () :
      get_image_reply Lwt.t =
    failwith "not implemented"

  let poly_text8 ~(drawable : drawable) ~(gc : gcontext) ~(x : int) ~(y : int)
      ~(items : char list) () : unit Lwt.t =
    failwith "not implemented"

  let poly_text16 ~(drawable : drawable) ~(gc : gcontext) ~(x : int) ~(y : int)
      ~(items : char list) () : unit Lwt.t =
    failwith "not implemented"

  let image_text8 ~(drawable : drawable) ~(gc : gcontext) ~(x : int) ~(y : int)
      ~(string : char list) () : unit Lwt.t =
    failwith "not implemented"

  let image_text16 ~(drawable : drawable) ~(gc : gcontext) ~(x : int) ~(y : int)
      ~(string : char2b list) () : unit Lwt.t =
    failwith "not implemented"

  type colormap_alloc_enum = [ `None | `All ]

  let colormap_alloc_enum_of_int : int -> colormap_alloc_enum option = function
    | 0 -> Some `None
    | 1 -> Some `All
    | _ -> None

  let create_colormap ~(alloc : colormap_alloc_enum) ~(mid : colormap)
      ~(window : window) ~(visual : visualid) () : unit Lwt.t =
    failwith "not implemented"

  let free_colormap ~(cmap : colormap) () : unit Lwt.t =
    failwith "not implemented"

  let copy_colormap_and_free ~(mid : colormap) ~(src_cmap : colormap) () :
      unit Lwt.t =
    failwith "not implemented"

  let install_colormap ~(cmap : colormap) () : unit Lwt.t =
    failwith "not implemented"

  let uninstall_colormap ~(cmap : colormap) () : unit Lwt.t =
    failwith "not implemented"

  type list_installed_colormaps_reply = { cmaps : colormap list }

  let list_installed_colormaps ~(window : window) () :
      list_installed_colormaps_reply Lwt.t =
    failwith "not implemented"

  type alloc_color_reply = { red : int; green : int; blue : int; pixel : int32 }

  let alloc_color ~(cmap : colormap) ~(red : int) ~(green : int) ~(blue : int)
      () : alloc_color_reply Lwt.t =
    failwith "not implemented"

  type alloc_named_color_reply = {
    pixel : int32;
    exact_red : int;
    exact_green : int;
    exact_blue : int;
    visual_red : int;
    visual_green : int;
    visual_blue : int;
  }

  let alloc_named_color ~(cmap : colormap) ~(name : char list) () :
      alloc_named_color_reply Lwt.t =
    failwith "not implemented"

  type alloc_color_cells_reply = { pixels : int32 list; masks : int32 list }

  let alloc_color_cells ~(contiguous : bool) ~(cmap : colormap) ~(colors : int)
      ~(planes : int) () : alloc_color_cells_reply Lwt.t =
    failwith "not implemented"

  type alloc_color_planes_reply = {
    red_mask : int32;
    green_mask : int32;
    blue_mask : int32;
    pixels : int32 list;
  }

  let alloc_color_planes ~(contiguous : bool) ~(cmap : colormap) ~(colors : int)
      ~(reds : int) ~(greens : int) ~(blues : int) () :
      alloc_color_planes_reply Lwt.t =
    failwith "not implemented"

  let free_colors ~(cmap : colormap) ~(plane_mask : int32)
      ~(pixels : int32 list) () : unit Lwt.t =
    failwith "not implemented"

  type color_flag_mask = [ `Red | `Green | `Blue ] list

  type coloritem = {
    pixel : int32;
    red : int;
    green : int;
    blue : int;
    flags : color_flag_mask;
  }

  let decode_coloritem buf ~at : (coloritem * int) option =
    let orig = at in
    let* pixel, at = decode_int32 buf ~at in
    let* red, at = decode_uint16 buf ~at in
    let* green, at = decode_uint16 buf ~at in
    let* blue, at = decode_uint16 buf ~at in
    let* flags, at = decode_char buf ~at in
    let at = at + 1 in
    ignore orig;
    Some ({ pixel; red; green; blue; flags = assert false }, at)

  let store_colors ~(cmap : colormap) ~(items : coloritem list) () : unit Lwt.t
      =
    failwith "not implemented"

  let store_named_color ~(flags : color_flag_mask) ~(cmap : colormap)
      ~(pixel : int32) ~(name : char list) () : unit Lwt.t =
    failwith "not implemented"

  type rgb = { red : int; green : int; blue : int }

  let decode_rgb buf ~at : (rgb * int) option =
    let orig = at in
    let* red, at = decode_uint16 buf ~at in
    let* green, at = decode_uint16 buf ~at in
    let* blue, at = decode_uint16 buf ~at in
    let at = at + 2 in
    ignore orig;
    Some ({ red; green; blue }, at)

  type query_colors_reply = { colors : rgb list }

  let query_colors ~(cmap : colormap) ~(pixels : int32 list) () :
      query_colors_reply Lwt.t =
    failwith "not implemented"

  type lookup_color_reply = {
    exact_red : int;
    exact_green : int;
    exact_blue : int;
    visual_red : int;
    visual_green : int;
    visual_blue : int;
  }

  let lookup_color ~(cmap : colormap) ~(name : char list) () :
      lookup_color_reply Lwt.t =
    failwith "not implemented"

  let create_cursor ~(cid : cursor) ~(source : pixmap)
      ~(mask : (pixmap_enum, pixmap) alt) ~(fore_red : int) ~(fore_green : int)
      ~(fore_blue : int) ~(back_red : int) ~(back_green : int)
      ~(back_blue : int) ~(x : int) ~(y : int) () : unit Lwt.t =
    failwith "not implemented"

  let create_glyph_cursor ~(cid : cursor) ~(source_font : font)
      ~(mask_font : (font_enum, font) alt) ~(source_char : int)
      ~(mask_char : int) ~(fore_red : int) ~(fore_green : int)
      ~(fore_blue : int) ~(back_red : int) ~(back_green : int)
      ~(back_blue : int) () : unit Lwt.t =
    failwith "not implemented"

  let free_cursor ~(cursor : cursor) () : unit Lwt.t =
    failwith "not implemented"

  let recolor_cursor ~(cursor : cursor) ~(fore_red : int) ~(fore_green : int)
      ~(fore_blue : int) ~(back_red : int) ~(back_green : int)
      ~(back_blue : int) () : unit Lwt.t =
    failwith "not implemented"

  type query_shape_of_enum =
    [ `Largest_cursor | `Fastest_tile | `Fastest_stipple ]

  let query_shape_of_enum_of_int : int -> query_shape_of_enum option = function
    | 0 -> Some `Largest_cursor
    | 1 -> Some `Fastest_tile
    | 2 -> Some `Fastest_stipple
    | _ -> None

  type query_best_size_reply = { width : int; height : int }

  let query_best_size ~(class_ : query_shape_of_enum) ~(drawable : drawable)
      ~(width : int) ~(height : int) () : query_best_size_reply Lwt.t =
    failwith "not implemented"

  type query_extension_reply = {
    present : bool;
    major_opcode : int;
    first_event : int;
    first_error : int;
  }

  let query_extension ~(name : char list) () : query_extension_reply Lwt.t =
    failwith "not implemented"

  type list_extensions_reply = { names : str list }

  let list_extensions () : list_extensions_reply Lwt.t =
    failwith "not implemented"

  let change_keyboard_mapping ~(keycode_count : int) ~(first_keycode : keycode)
      ~(keysyms_per_keycode : int) ~(keysyms : keysym list) () : unit Lwt.t =
    failwith "not implemented"

  type get_keyboard_mapping_reply = {
    keysyms_per_keycode : char;
    keysyms : keysym list;
  }

  let get_keyboard_mapping ~(first_keycode : keycode) ~(count : int) () :
      get_keyboard_mapping_reply Lwt.t =
    failwith "not implemented"

  type kb_mask =
    [ `Key_click_percent
    | `Bell_percent
    | `Bell_pitch
    | `Bell_duration
    | `Led
    | `Led_mode
    | `Key
    | `Auto_repeat_mode ]
    list

  type led_mode_enum = [ `Off | `On ]

  let led_mode_enum_of_int : int -> led_mode_enum option = function
    | 0 -> Some `Off
    | 1 -> Some `On
    | _ -> None

  type auto_repeat_mode_enum = [ `Off | `On | `Default ]

  let auto_repeat_mode_enum_of_int : int -> auto_repeat_mode_enum option =
    function
    | 0 -> Some `Off
    | 1 -> Some `On
    | 2 -> Some `Default
    | _ -> None

  let change_keyboard_control ?(key_click_percent : int32 option)
      ?(bell_percent : int32 option) ?(bell_pitch : int32 option)
      ?(bell_duration : int32 option) ?(led : int32 option)
      ?(led_mode : led_mode_enum option) ?(key : keycode32 option)
      ?(auto_repeat_mode : auto_repeat_mode_enum option) () : unit Lwt.t =
    failwith "not implemented"

  type get_keyboard_control_reply = {
    global_auto_repeat : auto_repeat_mode_enum;
    led_mask : int32;
    key_click_percent : int;
    bell_percent : int;
    bell_pitch : int;
    bell_duration : int;
    auto_repeats : int list;
  }

  let get_keyboard_control () : get_keyboard_control_reply Lwt.t =
    failwith "not implemented"

  let bell ~(percent : int) () : unit Lwt.t = failwith "not implemented"

  let change_pointer_control ~(acceleration_numerator : int)
      ~(acceleration_denominator : int) ~(threshold : int)
      ~(do_acceleration : bool) ~(do_threshold : bool) () : unit Lwt.t =
    failwith "not implemented"

  type get_pointer_control_reply = {
    acceleration_numerator : int;
    acceleration_denominator : int;
    threshold : int;
  }

  let get_pointer_control () : get_pointer_control_reply Lwt.t =
    failwith "not implemented"

  type blanking_enum = [ `Not_preferred | `Preferred | `Default ]

  let blanking_enum_of_int : int -> blanking_enum option = function
    | 0 -> Some `Not_preferred
    | 1 -> Some `Preferred
    | 2 -> Some `Default
    | _ -> None

  type exposures_enum = [ `Not_allowed | `Allowed | `Default ]

  let exposures_enum_of_int : int -> exposures_enum option = function
    | 0 -> Some `Not_allowed
    | 1 -> Some `Allowed
    | 2 -> Some `Default
    | _ -> None

  let set_screen_saver ~(timeout : int) ~(interval : int)
      ~(prefer_blanking : blanking_enum) ~(allow_exposures : exposures_enum) ()
      : unit Lwt.t =
    failwith "not implemented"

  type get_screen_saver_reply = {
    timeout : int;
    interval : int;
    prefer_blanking : blanking_enum;
    allow_exposures : exposures_enum;
  }

  let get_screen_saver () : get_screen_saver_reply Lwt.t =
    failwith "not implemented"

  type host_mode_enum = [ `Insert | `Delete ]

  let host_mode_enum_of_int : int -> host_mode_enum option = function
    | 0 -> Some `Insert
    | 1 -> Some `Delete
    | _ -> None

  type family_enum =
    [ `Internet | `Decnet | `Chaos | `Server_interpreted | `Internet6 ]

  let family_enum_of_int : int -> family_enum option = function
    | 0 -> Some `Internet
    | 1 -> Some `Decnet
    | 2 -> Some `Chaos
    | 5 -> Some `Server_interpreted
    | 6 -> Some `Internet6
    | _ -> None

  let change_hosts ~(mode : host_mode_enum) ~(family : family_enum)
      ~(address : char list) () : unit Lwt.t =
    failwith "not implemented"

  type host = { family : family_enum; address : char list }

  let decode_host buf ~at : (host * int) option =
    let orig = at in
    let* family, at =
      decode_enum decode_uint8 (fun x -> x) family_enum_of_int buf ~at
    in
    let at = at + 1 in
    let* address_len, at = decode_uint16 buf ~at in
    let address_len = address_len in
    let* address, at = decode_list decode_char address_len buf ~at in
    let at = at + ((at - orig) mod 4) in
    ignore orig;
    Some ({ family; address }, at)

  type access_control_enum = [ `Disable | `Enable ]

  let access_control_enum_of_int : int -> access_control_enum option = function
    | 0 -> Some `Disable
    | 1 -> Some `Enable
    | _ -> None

  type list_hosts_reply = { mode : access_control_enum; hosts : host list }

  let list_hosts () : list_hosts_reply Lwt.t = failwith "not implemented"

  let set_access_control ~(mode : access_control_enum) () : unit Lwt.t =
    failwith "not implemented"

  type close_down_enum = [ `Destroy_all | `Retain_permanent | `Retain_temporary ]

  let close_down_enum_of_int : int -> close_down_enum option = function
    | 0 -> Some `Destroy_all
    | 1 -> Some `Retain_permanent
    | 2 -> Some `Retain_temporary
    | _ -> None

  let set_close_down_mode ~(mode : close_down_enum) () : unit Lwt.t =
    failwith "not implemented"

  type kill_enum = [ `All_temporary ]

  let kill_enum_of_int : int -> kill_enum option = function
    | 0 -> Some `All_temporary
    | _ -> None

  let kill_client ~(resource : (kill_enum, int32) alt) () : unit Lwt.t =
    failwith "not implemented"

  let rotate_properties ~(window : window) ~(delta : int) ~(atoms : atom list)
      () : unit Lwt.t =
    failwith "not implemented"

  type screen_saver_enum = [ `Reset | `Active ]

  let screen_saver_enum_of_int : int -> screen_saver_enum option = function
    | 0 -> Some `Reset
    | 1 -> Some `Active
    | _ -> None

  let force_screen_saver ~(mode : screen_saver_enum) () : unit Lwt.t =
    failwith "not implemented"

  type mapping_status_enum = [ `Success | `Busy | `Failure ]

  let mapping_status_enum_of_int : int -> mapping_status_enum option = function
    | 0 -> Some `Success
    | 1 -> Some `Busy
    | 2 -> Some `Failure
    | _ -> None

  type set_pointer_mapping_reply = { status : mapping_status_enum }

  let set_pointer_mapping ~(map : int list) () : set_pointer_mapping_reply Lwt.t
      =
    failwith "not implemented"

  type get_pointer_mapping_reply = { map : int list }

  let get_pointer_mapping () : get_pointer_mapping_reply Lwt.t =
    failwith "not implemented"

  type map_index_enum =
    [ `Shift | `Lock | `Control | `D1 | `D2 | `D3 | `D4 | `D5 ]

  let map_index_enum_of_int : int -> map_index_enum option = function
    | 0 -> Some `Shift
    | 1 -> Some `Lock
    | 2 -> Some `Control
    | 3 -> Some `D1
    | 4 -> Some `D2
    | 5 -> Some `D3
    | 6 -> Some `D4
    | 7 -> Some `D5
    | _ -> None

  type set_modifier_mapping_reply = { status : mapping_status_enum }

  let set_modifier_mapping ~(keycodes_per_modifier : int)
      ~(keycodes : keycode list) () : set_modifier_mapping_reply Lwt.t =
    failwith "not implemented"

  type get_modifier_mapping_reply = {
    keycodes_per_modifier : int;
    keycodes : keycode list;
  }

  let get_modifier_mapping () : get_modifier_mapping_reply Lwt.t =
    failwith "not implemented"

  let no_operation () : unit Lwt.t = failwith "not implemented"
end
[@@warning "-27"]

module Render = struct
  type pict_type_enum = [ `Indexed | `Direct ]

  let pict_type_enum_of_int : int -> pict_type_enum option = function
    | 0 -> Some `Indexed
    | 1 -> Some `Direct
    | _ -> None

  type picture_enum = [ `None ]

  let picture_enum_of_int : int -> picture_enum option = function
    | 0 -> Some `None
    | _ -> None

  type pict_op_enum =
    [ `Clear
    | `Src
    | `Dst
    | `Over
    | `Over_reverse
    | `In
    | `In_reverse
    | `Out
    | `Out_reverse
    | `Atop
    | `Atop_reverse
    | `Xor
    | `Add
    | `Saturate
    | `Disjoint_clear
    | `Disjoint_src
    | `Disjoint_dst
    | `Disjoint_over
    | `Disjoint_over_reverse
    | `Disjoint_in
    | `Disjoint_in_reverse
    | `Disjoint_out
    | `Disjoint_out_reverse
    | `Disjoint_atop
    | `Disjoint_atop_reverse
    | `Disjoint_xor
    | `Conjoint_clear
    | `Conjoint_src
    | `Conjoint_dst
    | `Conjoint_over
    | `Conjoint_over_reverse
    | `Conjoint_in
    | `Conjoint_in_reverse
    | `Conjoint_out
    | `Conjoint_out_reverse
    | `Conjoint_atop
    | `Conjoint_atop_reverse
    | `Conjoint_xor
    | `Multiply
    | `Screen
    | `Overlay
    | `Darken
    | `Lighten
    | `Color_dodge
    | `Color_burn
    | `Hard_light
    | `Soft_light
    | `Difference
    | `Exclusion
    | `Hsl_hue
    | `Hsl_saturation
    | `Hsl_color
    | `Hsl_luminosity ]

  let pict_op_enum_of_int : int -> pict_op_enum option = function
    | 0 -> Some `Clear
    | 1 -> Some `Src
    | 2 -> Some `Dst
    | 3 -> Some `Over
    | 4 -> Some `Over_reverse
    | 5 -> Some `In
    | 6 -> Some `In_reverse
    | 7 -> Some `Out
    | 8 -> Some `Out_reverse
    | 9 -> Some `Atop
    | 10 -> Some `Atop_reverse
    | 11 -> Some `Xor
    | 12 -> Some `Add
    | 13 -> Some `Saturate
    | 16 -> Some `Disjoint_clear
    | 17 -> Some `Disjoint_src
    | 18 -> Some `Disjoint_dst
    | 19 -> Some `Disjoint_over
    | 20 -> Some `Disjoint_over_reverse
    | 21 -> Some `Disjoint_in
    | 22 -> Some `Disjoint_in_reverse
    | 23 -> Some `Disjoint_out
    | 24 -> Some `Disjoint_out_reverse
    | 25 -> Some `Disjoint_atop
    | 26 -> Some `Disjoint_atop_reverse
    | 27 -> Some `Disjoint_xor
    | 32 -> Some `Conjoint_clear
    | 33 -> Some `Conjoint_src
    | 34 -> Some `Conjoint_dst
    | 35 -> Some `Conjoint_over
    | 36 -> Some `Conjoint_over_reverse
    | 37 -> Some `Conjoint_in
    | 38 -> Some `Conjoint_in_reverse
    | 39 -> Some `Conjoint_out
    | 40 -> Some `Conjoint_out_reverse
    | 41 -> Some `Conjoint_atop
    | 42 -> Some `Conjoint_atop_reverse
    | 43 -> Some `Conjoint_xor
    | 48 -> Some `Multiply
    | 49 -> Some `Screen
    | 50 -> Some `Overlay
    | 51 -> Some `Darken
    | 52 -> Some `Lighten
    | 53 -> Some `Color_dodge
    | 54 -> Some `Color_burn
    | 55 -> Some `Hard_light
    | 56 -> Some `Soft_light
    | 57 -> Some `Difference
    | 58 -> Some `Exclusion
    | 59 -> Some `Hsl_hue
    | 60 -> Some `Hsl_saturation
    | 61 -> Some `Hsl_color
    | 62 -> Some `Hsl_luminosity
    | _ -> None

  type poly_edge_enum = [ `Sharp | `Smooth ]

  let poly_edge_enum_of_int : int -> poly_edge_enum option = function
    | 0 -> Some `Sharp
    | 1 -> Some `Smooth
    | _ -> None

  type poly_mode_enum = [ `Precise | `Imprecise ]

  let poly_mode_enum_of_int : int -> poly_mode_enum option = function
    | 0 -> Some `Precise
    | 1 -> Some `Imprecise
    | _ -> None

  type cp_mask =
    [ `Repeat
    | `Alpha_map
    | `Alpha_x_origin
    | `Alpha_y_origin
    | `Clip_x_origin
    | `Clip_y_origin
    | `Clip_mask
    | `Graphics_exposure
    | `Subwindow_mode
    | `Poly_edge
    | `Poly_mode
    | `Dither
    | `Component_alpha ]
    list

  type sub_pixel_enum =
    [ `Unknown
    | `Horizontal_rgb
    | `Horizontal_bgr
    | `Vertical_rgb
    | `Vertical_bgr
    | `None ]

  let sub_pixel_enum_of_int : int -> sub_pixel_enum option = function
    | 0 -> Some `Unknown
    | 1 -> Some `Horizontal_rgb
    | 2 -> Some `Horizontal_bgr
    | 3 -> Some `Vertical_rgb
    | 4 -> Some `Vertical_bgr
    | 5 -> Some `None
    | _ -> None

  type repeat_enum = [ `None | `Normal | `Pad | `Reflect ]

  let repeat_enum_of_int : int -> repeat_enum option = function
    | 0 -> Some `None
    | 1 -> Some `Normal
    | 2 -> Some `Pad
    | 3 -> Some `Reflect
    | _ -> None

  type glyph = int32

  let decode_glyph = decode_int32

  type glyphset = xid

  let decode_glyphset = decode_xid

  type picture = xid

  let decode_picture = decode_xid

  type pictformat = xid

  let decode_pictformat = decode_xid

  type fixed = int32

  let decode_fixed = decode_int32

  type pict_format_error = unit

  type picture_error = unit

  type pict_op_error = unit

  type glyph_set_error = unit

  type glyph_error = unit

  type directformat = {
    red_shift : int;
    red_mask : int;
    green_shift : int;
    green_mask : int;
    blue_shift : int;
    blue_mask : int;
    alpha_shift : int;
    alpha_mask : int;
  }

  let decode_directformat buf ~at : (directformat * int) option =
    let orig = at in
    let* red_shift, at = decode_uint16 buf ~at in
    let* red_mask, at = decode_uint16 buf ~at in
    let* green_shift, at = decode_uint16 buf ~at in
    let* green_mask, at = decode_uint16 buf ~at in
    let* blue_shift, at = decode_uint16 buf ~at in
    let* blue_mask, at = decode_uint16 buf ~at in
    let* alpha_shift, at = decode_uint16 buf ~at in
    let* alpha_mask, at = decode_uint16 buf ~at in
    ignore orig;
    Some
      ( {
          red_shift;
          red_mask;
          green_shift;
          green_mask;
          blue_shift;
          blue_mask;
          alpha_shift;
          alpha_mask;
        },
        at )

  type pictforminfo = {
    id : pictformat;
    type_ : pict_type_enum;
    depth : int;
    direct : directformat;
    colormap : Xproto.colormap;
  }

  let decode_pictforminfo buf ~at : (pictforminfo * int) option =
    let orig = at in
    let* id, at = decode_pictformat buf ~at in
    let* type_, at =
      decode_enum decode_uint8 (fun x -> x) pict_type_enum_of_int buf ~at
    in
    let* depth, at = decode_uint8 buf ~at in
    let at = at + 2 in
    let* direct, at = decode_directformat buf ~at in
    let* colormap, at = Xproto.decode_colormap buf ~at in
    ignore orig;
    Some ({ id; type_; depth; direct; colormap }, at)

  type pictvisual = { visual : Xproto.visualid; format : pictformat }

  let decode_pictvisual buf ~at : (pictvisual * int) option =
    let orig = at in
    let* visual, at = Xproto.decode_visualid buf ~at in
    let* format, at = decode_pictformat buf ~at in
    ignore orig;
    Some ({ visual; format }, at)

  type pictdepth = { depth : int; visuals : pictvisual list }

  let decode_pictdepth buf ~at : (pictdepth * int) option =
    let orig = at in
    let* depth, at = decode_uint8 buf ~at in
    let at = at + 1 in
    let* num_visuals, at = decode_uint16 buf ~at in
    let num_visuals = num_visuals in
    let at = at + 4 in
    let* visuals, at = decode_list decode_pictvisual num_visuals buf ~at in
    ignore orig;
    Some ({ depth; visuals }, at)

  type pictscreen = { fallback : pictformat; depths : pictdepth list }

  let decode_pictscreen buf ~at : (pictscreen * int) option =
    let orig = at in
    let* num_depths, at = decode_int32 buf ~at in
    let num_depths = Int32.to_int num_depths in
    let num_depths = num_depths in
    let* fallback, at = decode_pictformat buf ~at in
    let* depths, at = decode_list decode_pictdepth num_depths buf ~at in
    ignore orig;
    Some ({ fallback; depths }, at)

  type indexvalue = {
    pixel : int32;
    red : int;
    green : int;
    blue : int;
    alpha : int;
  }

  let decode_indexvalue buf ~at : (indexvalue * int) option =
    let orig = at in
    let* pixel, at = decode_int32 buf ~at in
    let* red, at = decode_uint16 buf ~at in
    let* green, at = decode_uint16 buf ~at in
    let* blue, at = decode_uint16 buf ~at in
    let* alpha, at = decode_uint16 buf ~at in
    ignore orig;
    Some ({ pixel; red; green; blue; alpha }, at)

  type color = { red : int; green : int; blue : int; alpha : int }

  let decode_color buf ~at : (color * int) option =
    let orig = at in
    let* red, at = decode_uint16 buf ~at in
    let* green, at = decode_uint16 buf ~at in
    let* blue, at = decode_uint16 buf ~at in
    let* alpha, at = decode_uint16 buf ~at in
    ignore orig;
    Some ({ red; green; blue; alpha }, at)

  type pointfix = { x : fixed; y : fixed }

  let decode_pointfix buf ~at : (pointfix * int) option =
    let orig = at in
    let* x, at = decode_fixed buf ~at in
    let* y, at = decode_fixed buf ~at in
    ignore orig;
    Some ({ x; y }, at)

  type linefix = { p1 : pointfix; p2 : pointfix }

  let decode_linefix buf ~at : (linefix * int) option =
    let orig = at in
    let* p1, at = decode_pointfix buf ~at in
    let* p2, at = decode_pointfix buf ~at in
    ignore orig;
    Some ({ p1; p2 }, at)

  type triangle = { p1 : pointfix; p2 : pointfix; p3 : pointfix }

  let decode_triangle buf ~at : (triangle * int) option =
    let orig = at in
    let* p1, at = decode_pointfix buf ~at in
    let* p2, at = decode_pointfix buf ~at in
    let* p3, at = decode_pointfix buf ~at in
    ignore orig;
    Some ({ p1; p2; p3 }, at)

  type trapezoid = {
    top : fixed;
    bottom : fixed;
    left : linefix;
    right : linefix;
  }

  let decode_trapezoid buf ~at : (trapezoid * int) option =
    let orig = at in
    let* top, at = decode_fixed buf ~at in
    let* bottom, at = decode_fixed buf ~at in
    let* left, at = decode_linefix buf ~at in
    let* right, at = decode_linefix buf ~at in
    ignore orig;
    Some ({ top; bottom; left; right }, at)

  type glyphinfo = {
    width : int;
    height : int;
    x : int;
    y : int;
    x_off : int;
    y_off : int;
  }

  let decode_glyphinfo buf ~at : (glyphinfo * int) option =
    let orig = at in
    let* width, at = decode_uint16 buf ~at in
    let* height, at = decode_uint16 buf ~at in
    let* x, at = decode_int16 buf ~at in
    let* y, at = decode_int16 buf ~at in
    let* x_off, at = decode_int16 buf ~at in
    let* y_off, at = decode_int16 buf ~at in
    ignore orig;
    Some ({ width; height; x; y; x_off; y_off }, at)

  type query_version_reply = { major_version : int32; minor_version : int32 }

  let query_version ~(client_major_version : int32)
      ~(client_minor_version : int32) () : query_version_reply Lwt.t =
    failwith "not implemented"

  type query_pict_formats_reply = {
    num_depths : int32;
    num_visuals : int32;
    formats : pictforminfo list;
    screens : pictscreen list;
    subpixels : sub_pixel_enum list;
  }

  let query_pict_formats () : query_pict_formats_reply Lwt.t =
    failwith "not implemented"

  type query_pict_index_values_reply = { values : indexvalue list }

  let query_pict_index_values ~(format : pictformat) () :
      query_pict_index_values_reply Lwt.t =
    failwith "not implemented"

  let create_picture ~(pid : picture) ~(drawable : Xproto.drawable)
      ~(format : pictformat) ?(repeat : repeat_enum option)
      ?(alphamap : picture option) ?(alphaxorigin : int32 option)
      ?(alphayorigin : int32 option) ?(clipxorigin : int32 option)
      ?(clipyorigin : int32 option) ?(clipmask : Xproto.pixmap option)
      ?(graphicsexposure : int32 option)
      ?(subwindowmode : Xproto.subwindow_mode_enum option)
      ?(polyedge : poly_edge_enum option) ?(polymode : poly_mode_enum option)
      ?(dither : Xproto.atom option) ?(componentalpha : int32 option) () :
      unit Lwt.t =
    failwith "not implemented"

  let change_picture ~(picture : picture) ?(repeat : repeat_enum option)
      ?(alphamap : picture option) ?(alphaxorigin : int32 option)
      ?(alphayorigin : int32 option) ?(clipxorigin : int32 option)
      ?(clipyorigin : int32 option) ?(clipmask : Xproto.pixmap option)
      ?(graphicsexposure : int32 option)
      ?(subwindowmode : Xproto.subwindow_mode_enum option)
      ?(polyedge : poly_edge_enum option) ?(polymode : poly_mode_enum option)
      ?(dither : Xproto.atom option) ?(componentalpha : int32 option) () :
      unit Lwt.t =
    failwith "not implemented"

  let set_picture_clip_rectangles ~(picture : picture) ~(clip_x_origin : int)
      ~(clip_y_origin : int) ~(rectangles : Xproto.rectangle list) () :
      unit Lwt.t =
    failwith "not implemented"

  let free_picture ~(picture : picture) () : unit Lwt.t =
    failwith "not implemented"

  let composite ~(op : pict_op_enum) ~(src : picture)
      ~(mask : (picture_enum, picture) alt) ~(dst : picture) ~(src_x : int)
      ~(src_y : int) ~(mask_x : int) ~(mask_y : int) ~(dst_x : int)
      ~(dst_y : int) ~(width : int) ~(height : int) () : unit Lwt.t =
    failwith "not implemented"

  let trapezoids ~(op : pict_op_enum) ~(src : picture) ~(dst : picture)
      ~(mask_format : pictformat) ~(src_x : int) ~(src_y : int)
      ~(traps : trapezoid list) () : unit Lwt.t =
    failwith "not implemented"

  let triangles ~(op : pict_op_enum) ~(src : picture) ~(dst : picture)
      ~(mask_format : pictformat) ~(src_x : int) ~(src_y : int)
      ~(triangles : triangle list) () : unit Lwt.t =
    failwith "not implemented"

  let tri_strip ~(op : pict_op_enum) ~(src : picture) ~(dst : picture)
      ~(mask_format : pictformat) ~(src_x : int) ~(src_y : int)
      ~(points : pointfix list) () : unit Lwt.t =
    failwith "not implemented"

  let tri_fan ~(op : pict_op_enum) ~(src : picture) ~(dst : picture)
      ~(mask_format : pictformat) ~(src_x : int) ~(src_y : int)
      ~(points : pointfix list) () : unit Lwt.t =
    failwith "not implemented"

  let create_glyph_set ~(gsid : glyphset) ~(format : pictformat) () : unit Lwt.t
      =
    failwith "not implemented"

  let reference_glyph_set ~(gsid : glyphset) ~(existing : glyphset) () :
      unit Lwt.t =
    failwith "not implemented"

  let free_glyph_set ~(glyphset : glyphset) () : unit Lwt.t =
    failwith "not implemented"

  let add_glyphs ~(glyphset : glyphset) ~(glyphids : int32 list)
      ~(glyphs : glyphinfo list) ~(data : char list) () : unit Lwt.t =
    failwith "not implemented"

  let free_glyphs ~(glyphset : glyphset) ~(glyphs : glyph list) () : unit Lwt.t
      =
    failwith "not implemented"

  let composite_glyphs8 ~(op : pict_op_enum) ~(src : picture) ~(dst : picture)
      ~(mask_format : pictformat) ~(glyphset : glyphset) ~(src_x : int)
      ~(src_y : int) ~(glyphcmds : char list) () : unit Lwt.t =
    failwith "not implemented"

  let composite_glyphs16 ~(op : pict_op_enum) ~(src : picture) ~(dst : picture)
      ~(mask_format : pictformat) ~(glyphset : glyphset) ~(src_x : int)
      ~(src_y : int) ~(glyphcmds : char list) () : unit Lwt.t =
    failwith "not implemented"

  let composite_glyphs32 ~(op : pict_op_enum) ~(src : picture) ~(dst : picture)
      ~(mask_format : pictformat) ~(glyphset : glyphset) ~(src_x : int)
      ~(src_y : int) ~(glyphcmds : char list) () : unit Lwt.t =
    failwith "not implemented"

  let fill_rectangles ~(op : pict_op_enum) ~(dst : picture) ~(color : color)
      ~(rects : Xproto.rectangle list) () : unit Lwt.t =
    failwith "not implemented"

  let create_cursor ~(cid : Xproto.cursor) ~(source : picture) ~(x : int)
      ~(y : int) () : unit Lwt.t =
    failwith "not implemented"

  type transform = {
    matrix11 : fixed;
    matrix12 : fixed;
    matrix13 : fixed;
    matrix21 : fixed;
    matrix22 : fixed;
    matrix23 : fixed;
    matrix31 : fixed;
    matrix32 : fixed;
    matrix33 : fixed;
  }

  let decode_transform buf ~at : (transform * int) option =
    let orig = at in
    let* matrix11, at = decode_fixed buf ~at in
    let* matrix12, at = decode_fixed buf ~at in
    let* matrix13, at = decode_fixed buf ~at in
    let* matrix21, at = decode_fixed buf ~at in
    let* matrix22, at = decode_fixed buf ~at in
    let* matrix23, at = decode_fixed buf ~at in
    let* matrix31, at = decode_fixed buf ~at in
    let* matrix32, at = decode_fixed buf ~at in
    let* matrix33, at = decode_fixed buf ~at in
    ignore orig;
    Some
      ( {
          matrix11;
          matrix12;
          matrix13;
          matrix21;
          matrix22;
          matrix23;
          matrix31;
          matrix32;
          matrix33;
        },
        at )

  let set_picture_transform ~(picture : picture) ~(transform : transform) () :
      unit Lwt.t =
    failwith "not implemented"

  type query_filters_reply = { aliases : int list; filters : Xproto.str list }

  let query_filters ~(drawable : Xproto.drawable) () : query_filters_reply Lwt.t
      =
    failwith "not implemented"

  let set_picture_filter ~(picture : picture) ~(filter : char list)
      ~(values : fixed list) () : unit Lwt.t =
    failwith "not implemented"

  type animcursorelt = { cursor : Xproto.cursor; delay : int32 }

  let decode_animcursorelt buf ~at : (animcursorelt * int) option =
    let orig = at in
    let* cursor, at = Xproto.decode_cursor buf ~at in
    let* delay, at = decode_int32 buf ~at in
    ignore orig;
    Some ({ cursor; delay }, at)

  let create_anim_cursor ~(cid : Xproto.cursor) ~(cursors : animcursorelt list)
      () : unit Lwt.t =
    failwith "not implemented"

  type spanfix = { l : fixed; r : fixed; y : fixed }

  let decode_spanfix buf ~at : (spanfix * int) option =
    let orig = at in
    let* l, at = decode_fixed buf ~at in
    let* r, at = decode_fixed buf ~at in
    let* y, at = decode_fixed buf ~at in
    ignore orig;
    Some ({ l; r; y }, at)

  type trap = { top : spanfix; bot : spanfix }

  let decode_trap buf ~at : (trap * int) option =
    let orig = at in
    let* top, at = decode_spanfix buf ~at in
    let* bot, at = decode_spanfix buf ~at in
    ignore orig;
    Some ({ top; bot }, at)

  let add_traps ~(picture : picture) ~(x_off : int) ~(y_off : int)
      ~(traps : trap list) () : unit Lwt.t =
    failwith "not implemented"

  let create_solid_fill ~(picture : picture) ~(color : color) () : unit Lwt.t =
    failwith "not implemented"

  let create_linear_gradient ~(picture : picture) ~(p1 : pointfix)
      ~(p2 : pointfix) ~(stops : fixed list) ~(colors : color list) () :
      unit Lwt.t =
    failwith "not implemented"

  let create_radial_gradient ~(picture : picture) ~(inner : pointfix)
      ~(outer : pointfix) ~(inner_radius : fixed) ~(outer_radius : fixed)
      ~(stops : fixed list) ~(colors : color list) () : unit Lwt.t =
    failwith "not implemented"

  let create_conical_gradient ~(picture : picture) ~(center : pointfix)
      ~(angle : fixed) ~(stops : fixed list) ~(colors : color list) () :
      unit Lwt.t =
    failwith "not implemented"
end
[@@warning "-27"]

module Shape = struct
  type op = int

  let decode_op = decode_uint8

  type kind = int

  let decode_kind = decode_uint8

  type so_enum = [ `Set | `Union | `Intersect | `Subtract | `Invert ]

  let so_enum_of_int : int -> so_enum option = function
    | 0 -> Some `Set
    | 1 -> Some `Union
    | 2 -> Some `Intersect
    | 3 -> Some `Subtract
    | 4 -> Some `Invert
    | _ -> None

  type sk_enum = [ `Bounding | `Clip | `Input ]

  let sk_enum_of_int : int -> sk_enum option = function
    | 0 -> Some `Bounding
    | 1 -> Some `Clip
    | 2 -> Some `Input
    | _ -> None

  type notify_event = {
    shape_kind : sk_enum;
    affected_window : Xproto.window;
    extents_x : int;
    extents_y : int;
    extents_width : int;
    extents_height : int;
    server_time : Xproto.timestamp;
    shaped : bool;
  }

  type query_version_reply = { major_version : int; minor_version : int }

  let query_version () : query_version_reply Lwt.t = failwith "not implemented"

  let rectangles ~(operation : so_enum) ~(destination_kind : sk_enum)
      ~(ordering : Xproto.clip_ordering_enum)
      ~(destination_window : Xproto.window) ~(x_offset : int) ~(y_offset : int)
      ~(rectangles : Xproto.rectangle list) () : unit Lwt.t =
    failwith "not implemented"

  let mask ~(operation : so_enum) ~(destination_kind : sk_enum)
      ~(destination_window : Xproto.window) ~(x_offset : int) ~(y_offset : int)
      ~(source_bitmap : (Xproto.pixmap_enum, Xproto.pixmap) alt) () : unit Lwt.t
      =
    failwith "not implemented"

  let combine ~(operation : so_enum) ~(destination_kind : sk_enum)
      ~(source_kind : sk_enum) ~(destination_window : Xproto.window)
      ~(x_offset : int) ~(y_offset : int) ~(source_window : Xproto.window) () :
      unit Lwt.t =
    failwith "not implemented"

  let offset ~(destination_kind : sk_enum) ~(destination_window : Xproto.window)
      ~(x_offset : int) ~(y_offset : int) () : unit Lwt.t =
    failwith "not implemented"

  type query_extents_reply = {
    bounding_shaped : bool;
    clip_shaped : bool;
    bounding_shape_extents_x : int;
    bounding_shape_extents_y : int;
    bounding_shape_extents_width : int;
    bounding_shape_extents_height : int;
    clip_shape_extents_x : int;
    clip_shape_extents_y : int;
    clip_shape_extents_width : int;
    clip_shape_extents_height : int;
  }

  let query_extents ~(destination_window : Xproto.window) () :
      query_extents_reply Lwt.t =
    failwith "not implemented"

  let select_input ~(destination_window : Xproto.window) ~(enable : bool) () :
      unit Lwt.t =
    failwith "not implemented"

  type input_selected_reply = { enabled : bool }

  let input_selected ~(destination_window : Xproto.window) () :
      input_selected_reply Lwt.t =
    failwith "not implemented"

  type get_rectangles_reply = {
    ordering : Xproto.clip_ordering_enum;
    rectangles : Xproto.rectangle list;
  }

  let get_rectangles ~(window : Xproto.window) ~(source_kind : sk_enum) () :
      get_rectangles_reply Lwt.t =
    failwith "not implemented"
end
[@@warning "-27"]

module Xfixes = struct
  type query_version_reply = { major_version : int32; minor_version : int32 }

  let query_version ~(client_major_version : int32)
      ~(client_minor_version : int32) () : query_version_reply Lwt.t =
    failwith "not implemented"

  type save_set_mode_enum = [ `Insert | `Delete ]

  let save_set_mode_enum_of_int : int -> save_set_mode_enum option = function
    | 0 -> Some `Insert
    | 1 -> Some `Delete
    | _ -> None

  type save_set_target_enum = [ `Nearest | `Root ]

  let save_set_target_enum_of_int : int -> save_set_target_enum option =
    function
    | 0 -> Some `Nearest
    | 1 -> Some `Root
    | _ -> None

  type save_set_mapping_enum = [ `Map | `Unmap ]

  let save_set_mapping_enum_of_int : int -> save_set_mapping_enum option =
    function
    | 0 -> Some `Map
    | 1 -> Some `Unmap
    | _ -> None

  let change_save_set ~(mode : save_set_mode_enum)
      ~(target : save_set_target_enum) ~(map : save_set_mapping_enum)
      ~(window : Xproto.window) () : unit Lwt.t =
    failwith "not implemented"

  type selection_event_enum =
    [ `Set_selection_owner
    | `Selection_window_destroy
    | `Selection_client_close ]

  let selection_event_enum_of_int : int -> selection_event_enum option =
    function
    | 0 -> Some `Set_selection_owner
    | 1 -> Some `Selection_window_destroy
    | 2 -> Some `Selection_client_close
    | _ -> None

  type selection_event_mask_mask =
    [ `Set_selection_owner
    | `Selection_window_destroy
    | `Selection_client_close ]
    list

  type selection_notify_event = {
    subtype : selection_event_enum;
    window : Xproto.window;
    owner : Xproto.window;
    selection : Xproto.atom;
    timestamp : Xproto.timestamp;
    selection_timestamp : Xproto.timestamp;
  }

  let select_selection_input ~(window : Xproto.window)
      ~(selection : Xproto.atom) ~(event_mask : selection_event_mask_mask) () :
      unit Lwt.t =
    failwith "not implemented"

  type cursor_notify_enum = [ `Display_cursor ]

  let cursor_notify_enum_of_int : int -> cursor_notify_enum option = function
    | 0 -> Some `Display_cursor
    | _ -> None

  type cursor_notify_mask_mask = [ `Display_cursor ] list

  type cursor_notify_event = {
    subtype : cursor_notify_enum;
    window : Xproto.window;
    cursor_serial : int32;
    timestamp : Xproto.timestamp;
    name : (Xproto.atom_enum, Xproto.atom) alt;
  }

  let select_cursor_input ~(window : Xproto.window)
      ~(event_mask : cursor_notify_mask_mask) () : unit Lwt.t =
    failwith "not implemented"

  type get_cursor_image_reply = {
    x : int;
    y : int;
    width : int;
    height : int;
    xhot : int;
    yhot : int;
    cursor_serial : int32;
    cursor_image : int32 list;
  }

  let get_cursor_image () : get_cursor_image_reply Lwt.t =
    failwith "not implemented"

  type region = xid

  let decode_region = decode_xid

  type bad_region_error = unit

  type region_enum = [ `None ]

  let region_enum_of_int : int -> region_enum option = function
    | 0 -> Some `None
    | _ -> None

  let create_region ~(region : region) ~(rectangles : Xproto.rectangle list) ()
      : unit Lwt.t =
    failwith "not implemented"

  let create_region_from_bitmap ~(region : region) ~(bitmap : Xproto.pixmap) ()
      : unit Lwt.t =
    failwith "not implemented"

  let create_region_from_window ~(region : region) ~(window : Xproto.window)
      ~(kind : Shape.sk_enum) () : unit Lwt.t =
    failwith "not implemented"

  let create_region_from_gc ~(region : region) ~(gc : Xproto.gcontext) () :
      unit Lwt.t =
    failwith "not implemented"

  let create_region_from_picture ~(region : region) ~(picture : Render.picture)
      () : unit Lwt.t =
    failwith "not implemented"

  let destroy_region ~(region : region) () : unit Lwt.t =
    failwith "not implemented"

  let set_region ~(region : region) ~(rectangles : Xproto.rectangle list) () :
      unit Lwt.t =
    failwith "not implemented"

  let copy_region ~(source : region) ~(destination : region) () : unit Lwt.t =
    failwith "not implemented"

  let union_region ~(source1 : region) ~(source2 : region)
      ~(destination : region) () : unit Lwt.t =
    failwith "not implemented"

  let intersect_region ~(source1 : region) ~(source2 : region)
      ~(destination : region) () : unit Lwt.t =
    failwith "not implemented"

  let subtract_region ~(source1 : region) ~(source2 : region)
      ~(destination : region) () : unit Lwt.t =
    failwith "not implemented"

  let invert_region ~(source : region) ~(bounds : Xproto.rectangle)
      ~(destination : region) () : unit Lwt.t =
    failwith "not implemented"

  let translate_region ~(region : region) ~(dx : int) ~(dy : int) () :
      unit Lwt.t =
    failwith "not implemented"

  let region_extents ~(source : region) ~(destination : region) () : unit Lwt.t
      =
    failwith "not implemented"

  type fetch_region_reply = {
    extents : Xproto.rectangle;
    rectangles : Xproto.rectangle list;
  }

  let fetch_region ~(region : region) () : fetch_region_reply Lwt.t =
    failwith "not implemented"

  let set_gc_clip_region ~(gc : Xproto.gcontext)
      ~(region : (region_enum, region) alt) ~(x_origin : int) ~(y_origin : int)
      () : unit Lwt.t =
    failwith "not implemented"

  let set_window_shape_region ~(dest : Xproto.window)
      ~(dest_kind : Shape.sk_enum) ~(x_offset : int) ~(y_offset : int)
      ~(region : (region_enum, region) alt) () : unit Lwt.t =
    failwith "not implemented"

  let set_picture_clip_region ~(picture : Render.picture)
      ~(region : (region_enum, region) alt) ~(x_origin : int) ~(y_origin : int)
      () : unit Lwt.t =
    failwith "not implemented"

  let set_cursor_name ~(cursor : Xproto.cursor) ~(name : char list) () :
      unit Lwt.t =
    failwith "not implemented"

  type get_cursor_name_reply = {
    atom : (Xproto.atom_enum, Xproto.atom) alt;
    name : char list;
  }

  let get_cursor_name ~(cursor : Xproto.cursor) () : get_cursor_name_reply Lwt.t
      =
    failwith "not implemented"

  type get_cursor_image_and_name_reply = {
    x : int;
    y : int;
    width : int;
    height : int;
    xhot : int;
    yhot : int;
    cursor_serial : int32;
    cursor_atom : (Xproto.atom_enum, Xproto.atom) alt;
    cursor_image : int32 list;
    name : char list;
  }

  let get_cursor_image_and_name () : get_cursor_image_and_name_reply Lwt.t =
    failwith "not implemented"

  let change_cursor ~(source : Xproto.cursor) ~(destination : Xproto.cursor) ()
      : unit Lwt.t =
    failwith "not implemented"

  let change_cursor_by_name ~(src : Xproto.cursor) ~(name : char list) () :
      unit Lwt.t =
    failwith "not implemented"

  let expand_region ~(source : region) ~(destination : region) ~(left : int)
      ~(right : int) ~(top : int) ~(bottom : int) () : unit Lwt.t =
    failwith "not implemented"

  let hide_cursor ~(window : Xproto.window) () : unit Lwt.t =
    failwith "not implemented"

  let show_cursor ~(window : Xproto.window) () : unit Lwt.t =
    failwith "not implemented"

  type barrier = xid

  let decode_barrier = decode_xid

  type barrier_directions_mask =
    [ `Positive_x | `Positive_y | `Negative_x | `Negative_y ] list

  let create_pointer_barrier ~(barrier : barrier) ~(window : Xproto.window)
      ~(x1 : int) ~(y1 : int) ~(x2 : int) ~(y2 : int)
      ~(directions : barrier_directions_mask) ~(devices : int list) () :
      unit Lwt.t =
    failwith "not implemented"

  let delete_pointer_barrier ~(barrier : barrier) () : unit Lwt.t =
    failwith "not implemented"
end
[@@warning "-27"]

module Composite = struct
  type redirect_enum = [ `Automatic | `Manual ]

  let redirect_enum_of_int : int -> redirect_enum option = function
    | 0 -> Some `Automatic
    | 1 -> Some `Manual
    | _ -> None

  type query_version_reply = { major_version : int32; minor_version : int32 }

  let query_version ~(client_major_version : int32)
      ~(client_minor_version : int32) () : query_version_reply Lwt.t =
    failwith "not implemented"

  let redirect_window ~(window : Xproto.window) ~(update : redirect_enum) () :
      unit Lwt.t =
    failwith "not implemented"

  let redirect_subwindows ~(window : Xproto.window) ~(update : redirect_enum) ()
      : unit Lwt.t =
    failwith "not implemented"

  let unredirect_window ~(window : Xproto.window) ~(update : redirect_enum) () :
      unit Lwt.t =
    failwith "not implemented"

  let unredirect_subwindows ~(window : Xproto.window) ~(update : redirect_enum)
      () : unit Lwt.t =
    failwith "not implemented"

  let create_region_from_border_clip ~(region : Xfixes.region)
      ~(window : Xproto.window) () : unit Lwt.t =
    failwith "not implemented"

  let name_window_pixmap ~(window : Xproto.window) ~(pixmap : Xproto.pixmap) ()
      : unit Lwt.t =
    failwith "not implemented"

  type get_overlay_window_reply = { overlay_win : Xproto.window }

  let get_overlay_window ~(window : Xproto.window) () :
      get_overlay_window_reply Lwt.t =
    failwith "not implemented"

  let release_overlay_window ~(window : Xproto.window) () : unit Lwt.t =
    failwith "not implemented"
end
[@@warning "-27"]

module Damage = struct
  type damage = xid

  let decode_damage = decode_xid

  type report_level_enum =
    [ `Raw_rectangles | `Delta_rectangles | `Bounding_box | `Non_empty ]

  let report_level_enum_of_int : int -> report_level_enum option = function
    | 0 -> Some `Raw_rectangles
    | 1 -> Some `Delta_rectangles
    | 2 -> Some `Bounding_box
    | 3 -> Some `Non_empty
    | _ -> None

  type bad_damage_error = unit

  type query_version_reply = { major_version : int32; minor_version : int32 }

  let query_version ~(client_major_version : int32)
      ~(client_minor_version : int32) () : query_version_reply Lwt.t =
    failwith "not implemented"

  let create ~(damage : damage) ~(drawable : Xproto.drawable)
      ~(level : report_level_enum) () : unit Lwt.t =
    failwith "not implemented"

  let destroy ~(damage : damage) () : unit Lwt.t = failwith "not implemented"

  let subtract ~(damage : damage)
      ~(repair : (Xfixes.region_enum, Xfixes.region) alt)
      ~(parts : (Xfixes.region_enum, Xfixes.region) alt) () : unit Lwt.t =
    failwith "not implemented"

  let add ~(drawable : Xproto.drawable) ~(region : Xfixes.region) () :
      unit Lwt.t =
    failwith "not implemented"

  type notify_event = {
    level : report_level_enum;
    drawable : Xproto.drawable;
    damage : damage;
    timestamp : Xproto.timestamp;
    area : Xproto.rectangle;
    geometry : Xproto.rectangle;
  }
end
[@@warning "-27"]

module Dpms = struct
  type get_version_reply = {
    server_major_version : int;
    server_minor_version : int;
  }

  let get_version ~(client_major_version : int) ~(client_minor_version : int) ()
      : get_version_reply Lwt.t =
    failwith "not implemented"

  type capable_reply = { capable : bool }

  let capable () : capable_reply Lwt.t = failwith "not implemented"

  type get_timeouts_reply = {
    standby_timeout : int;
    suspend_timeout : int;
    off_timeout : int;
  }

  let get_timeouts () : get_timeouts_reply Lwt.t = failwith "not implemented"

  let set_timeouts ~(standby_timeout : int) ~(suspend_timeout : int)
      ~(off_timeout : int) () : unit Lwt.t =
    failwith "not implemented"

  let enable () : unit Lwt.t = failwith "not implemented"

  let disable () : unit Lwt.t = failwith "not implemented"

  type dpms_mode_enum = [ `On | `Standby | `Suspend | `Off ]

  let dpms_mode_enum_of_int : int -> dpms_mode_enum option = function
    | 0 -> Some `On
    | 1 -> Some `Standby
    | 2 -> Some `Suspend
    | 3 -> Some `Off
    | _ -> None

  let force_level ~(power_level : dpms_mode_enum) () : unit Lwt.t =
    failwith "not implemented"

  type info_reply = { power_level : dpms_mode_enum; state : bool }

  let info () : info_reply Lwt.t = failwith "not implemented"
end
[@@warning "-27"]

module Dri2 = struct
  type attachment_enum =
    [ `Buffer_front_left
    | `Buffer_back_left
    | `Buffer_front_right
    | `Buffer_back_right
    | `Buffer_depth
    | `Buffer_stencil
    | `Buffer_accum
    | `Buffer_fake_front_left
    | `Buffer_fake_front_right
    | `Buffer_depth_stencil
    | `Buffer_hiz ]

  let attachment_enum_of_int : int -> attachment_enum option = function
    | 0 -> Some `Buffer_front_left
    | 1 -> Some `Buffer_back_left
    | 2 -> Some `Buffer_front_right
    | 3 -> Some `Buffer_back_right
    | 4 -> Some `Buffer_depth
    | 5 -> Some `Buffer_stencil
    | 6 -> Some `Buffer_accum
    | 7 -> Some `Buffer_fake_front_left
    | 8 -> Some `Buffer_fake_front_right
    | 9 -> Some `Buffer_depth_stencil
    | 10 -> Some `Buffer_hiz
    | _ -> None

  type driver_type_enum = [ `Dri | `Vdpau ]

  let driver_type_enum_of_int : int -> driver_type_enum option = function
    | 0 -> Some `Dri
    | 1 -> Some `Vdpau
    | _ -> None

  type event_type_enum = [ `Exchange_complete | `Blit_complete | `Flip_complete ]

  let event_type_enum_of_int : int -> event_type_enum option = function
    | 1 -> Some `Exchange_complete
    | 2 -> Some `Blit_complete
    | 3 -> Some `Flip_complete
    | _ -> None

  type dri2_buffer = {
    attachment : attachment_enum;
    name : int32;
    pitch : int32;
    cpp : int32;
    flags : int32;
  }

  let decode_dri2_buffer buf ~at : (dri2_buffer * int) option =
    let orig = at in
    let* attachment, at =
      decode_enum decode_int32 Int32.to_int attachment_enum_of_int buf ~at
    in
    let* name, at = decode_int32 buf ~at in
    let* pitch, at = decode_int32 buf ~at in
    let* cpp, at = decode_int32 buf ~at in
    let* flags, at = decode_int32 buf ~at in
    ignore orig;
    Some ({ attachment; name; pitch; cpp; flags }, at)

  type attach_format = { attachment : attachment_enum; format : int32 }

  let decode_attach_format buf ~at : (attach_format * int) option =
    let orig = at in
    let* attachment, at =
      decode_enum decode_int32 Int32.to_int attachment_enum_of_int buf ~at
    in
    let* format, at = decode_int32 buf ~at in
    ignore orig;
    Some ({ attachment; format }, at)

  type query_version_reply = { major_version : int32; minor_version : int32 }

  let query_version ~(major_version : int32) ~(minor_version : int32) () :
      query_version_reply Lwt.t =
    failwith "not implemented"

  type connect_reply = {
    driver_name : char list;
    alignment_pad : char list;
    device_name : char list;
  }

  let connect ~(window : Xproto.window) ~(driver_type : driver_type_enum) () :
      connect_reply Lwt.t =
    failwith "not implemented"

  type authenticate_reply = { authenticated : int32 }

  let authenticate ~(window : Xproto.window) ~(magic : int32) () :
      authenticate_reply Lwt.t =
    failwith "not implemented"

  let create_drawable ~(drawable : Xproto.drawable) () : unit Lwt.t =
    failwith "not implemented"

  let destroy_drawable ~(drawable : Xproto.drawable) () : unit Lwt.t =
    failwith "not implemented"

  type get_buffers_reply = {
    width : int32;
    height : int32;
    buffers : dri2_buffer list;
  }

  let get_buffers ~(drawable : Xproto.drawable) ~(attachments : int32 list) () :
      get_buffers_reply Lwt.t =
    failwith "not implemented"

  type copy_region_reply = unit

  let copy_region ~(drawable : Xproto.drawable) ~(region : int32)
      ~(dest : int32) ~(src : int32) () : copy_region_reply Lwt.t =
    failwith "not implemented"

  type get_buffers_with_format_reply = {
    width : int32;
    height : int32;
    buffers : dri2_buffer list;
  }

  let get_buffers_with_format ~(drawable : Xproto.drawable)
      ~(attachments : attach_format list) () :
      get_buffers_with_format_reply Lwt.t =
    failwith "not implemented"

  type swap_buffers_reply = { swap_hi : int32; swap_lo : int32 }

  let swap_buffers ~(drawable : Xproto.drawable) ~(target_msc_hi : int32)
      ~(target_msc_lo : int32) ~(divisor_hi : int32) ~(divisor_lo : int32)
      ~(remainder_hi : int32) ~(remainder_lo : int32) () :
      swap_buffers_reply Lwt.t =
    failwith "not implemented"

  type get_msc_reply = {
    ust_hi : int32;
    ust_lo : int32;
    msc_hi : int32;
    msc_lo : int32;
    sbc_hi : int32;
    sbc_lo : int32;
  }

  let get_msc ~(drawable : Xproto.drawable) () : get_msc_reply Lwt.t =
    failwith "not implemented"

  type wait_msc_reply = {
    ust_hi : int32;
    ust_lo : int32;
    msc_hi : int32;
    msc_lo : int32;
    sbc_hi : int32;
    sbc_lo : int32;
  }

  let wait_msc ~(drawable : Xproto.drawable) ~(target_msc_hi : int32)
      ~(target_msc_lo : int32) ~(divisor_hi : int32) ~(divisor_lo : int32)
      ~(remainder_hi : int32) ~(remainder_lo : int32) () : wait_msc_reply Lwt.t
      =
    failwith "not implemented"

  type wait_sbc_reply = {
    ust_hi : int32;
    ust_lo : int32;
    msc_hi : int32;
    msc_lo : int32;
    sbc_hi : int32;
    sbc_lo : int32;
  }

  let wait_sbc ~(drawable : Xproto.drawable) ~(target_sbc_hi : int32)
      ~(target_sbc_lo : int32) () : wait_sbc_reply Lwt.t =
    failwith "not implemented"

  let swap_interval ~(drawable : Xproto.drawable) ~(interval : int32) () :
      unit Lwt.t =
    failwith "not implemented"

  type get_param_reply = {
    is_param_recognized : bool;
    value_hi : int32;
    value_lo : int32;
  }

  let get_param ~(drawable : Xproto.drawable) ~(param : int32) () :
      get_param_reply Lwt.t =
    failwith "not implemented"

  type buffer_swap_complete_event = {
    event_type : event_type_enum;
    drawable : Xproto.drawable;
    ust_hi : int32;
    ust_lo : int32;
    msc_hi : int32;
    msc_lo : int32;
    sbc : int32;
  }

  type invalidate_buffers_event = { drawable : Xproto.drawable }
end
[@@warning "-27"]

module Dri3 = struct
  type query_version_reply = { major_version : int32; minor_version : int32 }

  let query_version ~(major_version : int32) ~(minor_version : int32) () :
      query_version_reply Lwt.t =
    failwith "not implemented"

  type open_reply = { nfd : int; device_fd : file_descr }

  let open_ ~(drawable : Xproto.drawable) ~(provider : int32) () :
      open_reply Lwt.t =
    failwith "not implemented"

  let pixmap_from_buffer ~(pixmap : Xproto.pixmap) ~(drawable : Xproto.drawable)
      ~(size : int32) ~(width : int) ~(height : int) ~(stride : int)
      ~(depth : int) ~(bpp : int) ~(pixmap_fd : file_descr) () : unit Lwt.t =
    failwith "not implemented"

  type buffer_from_pixmap_reply = {
    nfd : int;
    size : int32;
    width : int;
    height : int;
    stride : int;
    depth : int;
    bpp : int;
    pixmap_fd : file_descr;
  }

  let buffer_from_pixmap ~(pixmap : Xproto.pixmap) () :
      buffer_from_pixmap_reply Lwt.t =
    failwith "not implemented"

  let fence_from_fd ~(drawable : Xproto.drawable) ~(fence : int32)
      ~(initially_triggered : bool) ~(fence_fd : file_descr) () : unit Lwt.t =
    failwith "not implemented"

  type fd_from_fence_reply = { nfd : int; fence_fd : file_descr }

  let fd_from_fence ~(drawable : Xproto.drawable) ~(fence : int32) () :
      fd_from_fence_reply Lwt.t =
    failwith "not implemented"

  type get_supported_modifiers_reply = {
    window_modifiers : int64 list;
    screen_modifiers : int64 list;
  }

  let get_supported_modifiers ~(window : int32) ~(depth : int) ~(bpp : int) () :
      get_supported_modifiers_reply Lwt.t =
    failwith "not implemented"

  let pixmap_from_buffers ~(pixmap : Xproto.pixmap) ~(window : Xproto.window)
      ~(width : int) ~(height : int) ~(stride0 : int32) ~(offset0 : int32)
      ~(stride1 : int32) ~(offset1 : int32) ~(stride2 : int32)
      ~(offset2 : int32) ~(stride3 : int32) ~(offset3 : int32) ~(depth : int)
      ~(bpp : int) ~(modifier : int64) ~(buffers : file_descr list) () :
      unit Lwt.t =
    failwith "not implemented"

  type buffers_from_pixmap_reply = {
    width : int;
    height : int;
    modifier : int64;
    depth : int;
    bpp : int;
    strides : int32 list;
    offsets : int32 list;
    buffers : file_descr list;
  }

  let buffers_from_pixmap ~(pixmap : Xproto.pixmap) () :
      buffers_from_pixmap_reply Lwt.t =
    failwith "not implemented"
end
[@@warning "-27"]

module Ge = struct
  type query_version_reply = { major_version : int; minor_version : int }

  let query_version ~(client_major_version : int) ~(client_minor_version : int)
      () : query_version_reply Lwt.t =
    failwith "not implemented"
end
[@@warning "-27"]

module Glx = struct
  type pixmap = xid

  let decode_pixmap = decode_xid

  type context = xid

  let decode_context = decode_xid

  type pbuffer = xid

  let decode_pbuffer = decode_xid

  type window = xid

  let decode_window = decode_xid

  type fbconfig = xid

  let decode_fbconfig = decode_xid

  type drawable = xid

  let decode_drawable = decode_xid

  type float32 = float

  let decode_float32 = decode_float

  type float64 = float

  let decode_float64 = decode_float

  type bool32 = int32

  let decode_bool32 = decode_int32

  type context_tag = int32

  let decode_context_tag = decode_int32

  type generic_error = {
    bad_value : int32;
    minor_opcode : int;
    major_opcode : int;
  }

  type bad_context_error = generic_error

  type bad_context_state_error = generic_error

  type bad_drawable_error = generic_error

  type bad_pixmap_error = generic_error

  type bad_context_tag_error = generic_error

  type bad_current_window_error = generic_error

  type bad_render_request_error = generic_error

  type bad_large_request_error = generic_error

  type unsupported_private_request_error = generic_error

  type bad_fb_config_error = generic_error

  type bad_pbuffer_error = generic_error

  type bad_current_drawable_error = generic_error

  type bad_window_error = generic_error

  type glx_bad_profile_arb_error = generic_error

  type pbuffer_clobber_event = {
    event_type : int;
    draw_type : int;
    drawable : drawable;
    b_mask : int32;
    aux_buffer : int;
    x : int;
    y : int;
    width : int;
    height : int;
    count : int;
  }

  type buffer_swap_complete_event = {
    event_type : int;
    drawable : drawable;
    ust_hi : int32;
    ust_lo : int32;
    msc_hi : int32;
    msc_lo : int32;
    sbc : int32;
  }

  type pbcet_enum = [ `Damaged | `Saved ]

  let pbcet_enum_of_int : int -> pbcet_enum option = function
    | 32791 -> Some `Damaged
    | 32792 -> Some `Saved
    | _ -> None

  type pbcdt_enum = [ `Window | `Pbuffer ]

  let pbcdt_enum_of_int : int -> pbcdt_enum option = function
    | 32793 -> Some `Window
    | 32794 -> Some `Pbuffer
    | _ -> None

  let render ~(context_tag : context_tag) ~(data : char list) () : unit Lwt.t =
    failwith "not implemented"

  let render_large ~(context_tag : context_tag) ~(request_num : int)
      ~(request_total : int) ~(data : char list) () : unit Lwt.t =
    failwith "not implemented"

  let create_context ~(context : context) ~(visual : Xproto.visualid)
      ~(screen : int32) ~(share_list : context) ~(is_direct : bool) () :
      unit Lwt.t =
    failwith "not implemented"

  let destroy_context ~(context : context) () : unit Lwt.t =
    failwith "not implemented"

  type make_current_reply = { context_tag : context_tag }

  let make_current ~(drawable : drawable) ~(context : context)
      ~(old_context_tag : context_tag) () : make_current_reply Lwt.t =
    failwith "not implemented"

  type is_direct_reply = { is_direct : bool }

  let is_direct ~(context : context) () : is_direct_reply Lwt.t =
    failwith "not implemented"

  type query_version_reply = { major_version : int32; minor_version : int32 }

  let query_version ~(major_version : int32) ~(minor_version : int32) () :
      query_version_reply Lwt.t =
    failwith "not implemented"

  let wait_gl ~(context_tag : context_tag) () : unit Lwt.t =
    failwith "not implemented"

  let wait_x ~(context_tag : context_tag) () : unit Lwt.t =
    failwith "not implemented"

  let copy_context ~(src : context) ~(dest : context) ~(mask : int32)
      ~(src_context_tag : context_tag) () : unit Lwt.t =
    failwith "not implemented"

  type gc_mask =
    ( [ `Gl_current_bit
      | `Gl_point_bit
      | `Gl_line_bit
      | `Gl_polygon_bit
      | `Gl_polygon_stipple_bit
      | `Gl_pixel_mode_bit
      | `Gl_lighting_bit
      | `Gl_fog_bit
      | `Gl_depth_buffer_bit
      | `Gl_accum_buffer_bit
      | `Gl_stencil_buffer_bit
      | `Gl_viewport_bit
      | `Gl_transform_bit
      | `Gl_enable_bit
      | `Gl_color_buffer_bit
      | `Gl_hint_bit
      | `Gl_eval_bit
      | `Gl_list_bit
      | `Gl_texture_bit
      | `Gl_scissor_bit ],
      [ `Gl_all_attrib_bits ] )
    mask

  let swap_buffers ~(context_tag : context_tag) ~(drawable : drawable) () :
      unit Lwt.t =
    failwith "not implemented"

  let use_x_font ~(context_tag : context_tag) ~(font : Xproto.font)
      ~(first : int32) ~(count : int32) ~(list_base : int32) () : unit Lwt.t =
    failwith "not implemented"

  let create_glx_pixmap ~(screen : int32) ~(visual : Xproto.visualid)
      ~(pixmap : Xproto.pixmap) ~(glx_pixmap : pixmap) () : unit Lwt.t =
    failwith "not implemented"

  type get_visual_configs_reply = {
    num_visuals : int32;
    num_properties : int32;
    property_list : int32 list;
  }

  let get_visual_configs ~(screen : int32) () : get_visual_configs_reply Lwt.t =
    failwith "not implemented"

  let destroy_glx_pixmap ~(glx_pixmap : pixmap) () : unit Lwt.t =
    failwith "not implemented"

  let vendor_private ~(vendor_code : int32) ~(context_tag : context_tag)
      ~(data : char list) () : unit Lwt.t =
    failwith "not implemented"

  type vendor_private_with_reply_reply = {
    retval : int32;
    data1 : char list;
    data2 : char list;
  }

  let vendor_private_with_reply ~(vendor_code : int32)
      ~(context_tag : context_tag) ~(data : char list) () :
      vendor_private_with_reply_reply Lwt.t =
    failwith "not implemented"

  type query_extensions_string_reply = { n : int32 }

  let query_extensions_string ~(screen : int32) () :
      query_extensions_string_reply Lwt.t =
    failwith "not implemented"

  type query_server_string_reply = { string : char list }

  let query_server_string ~(screen : int32) ~(name : int32) () :
      query_server_string_reply Lwt.t =
    failwith "not implemented"

  let client_info ~(major_version : int32) ~(minor_version : int32)
      ~(string : char list) () : unit Lwt.t =
    failwith "not implemented"

  type get_fb_configs_reply = {
    num_f_b_configs : int32;
    num_properties : int32;
    property_list : int32 list;
  }

  let get_fb_configs ~(screen : int32) () : get_fb_configs_reply Lwt.t =
    failwith "not implemented"

  let create_pixmap ~(screen : int32) ~(fbconfig : fbconfig)
      ~(pixmap : Xproto.pixmap) ~(glx_pixmap : pixmap) ~(num_attribs : int32)
      ~(attribs : int32 list) () : unit Lwt.t =
    failwith "not implemented"

  let destroy_pixmap ~(glx_pixmap : pixmap) () : unit Lwt.t =
    failwith "not implemented"

  let create_new_context ~(context : context) ~(fbconfig : fbconfig)
      ~(screen : int32) ~(render_type : int32) ~(share_list : context)
      ~(is_direct : bool) () : unit Lwt.t =
    failwith "not implemented"

  type query_context_reply = { num_attribs : int32; attribs : int32 list }

  let query_context ~(context : context) () : query_context_reply Lwt.t =
    failwith "not implemented"

  type make_context_current_reply = { context_tag : context_tag }

  let make_context_current ~(old_context_tag : context_tag)
      ~(drawable : drawable) ~(read_drawable : drawable) ~(context : context) ()
      : make_context_current_reply Lwt.t =
    failwith "not implemented"

  let create_pbuffer ~(screen : int32) ~(fbconfig : fbconfig)
      ~(pbuffer : pbuffer) ~(num_attribs : int32) ~(attribs : int32 list) () :
      unit Lwt.t =
    failwith "not implemented"

  let destroy_pbuffer ~(pbuffer : pbuffer) () : unit Lwt.t =
    failwith "not implemented"

  type get_drawable_attributes_reply = {
    num_attribs : int32;
    attribs : int32 list;
  }

  let get_drawable_attributes ~(drawable : drawable) () :
      get_drawable_attributes_reply Lwt.t =
    failwith "not implemented"

  let change_drawable_attributes ~(drawable : drawable) ~(num_attribs : int32)
      ~(attribs : int32 list) () : unit Lwt.t =
    failwith "not implemented"

  let create_window ~(screen : int32) ~(fbconfig : fbconfig)
      ~(window : Xproto.window) ~(glx_window : window) ~(num_attribs : int32)
      ~(attribs : int32 list) () : unit Lwt.t =
    failwith "not implemented"

  let delete_window ~(glxwindow : window) () : unit Lwt.t =
    failwith "not implemented"

  let set_client_info_arb ~(major_version : int32) ~(minor_version : int32)
      ~(num_versions : int32) ~(gl_versions : int32 list)
      ~(gl_extension_string : char list) ~(glx_extension_string : char list) ()
      : unit Lwt.t =
    failwith "not implemented"

  let create_context_attribs_arb ~(context : context) ~(fbconfig : fbconfig)
      ~(screen : int32) ~(share_list : context) ~(is_direct : bool)
      ~(num_attribs : int32) ~(attribs : int32 list) () : unit Lwt.t =
    failwith "not implemented"

  let set_client_info2_arb ~(major_version : int32) ~(minor_version : int32)
      ~(num_versions : int32) ~(gl_versions : int32 list)
      ~(gl_extension_string : char list) ~(glx_extension_string : char list) ()
      : unit Lwt.t =
    failwith "not implemented"

  let new_list ~(context_tag : context_tag) ~(list : int32) ~(mode : int32) () :
      unit Lwt.t =
    failwith "not implemented"

  let end_list ~(context_tag : context_tag) () : unit Lwt.t =
    failwith "not implemented"

  let delete_lists ~(context_tag : context_tag) ~(list : int32) ~(range : int32)
      () : unit Lwt.t =
    failwith "not implemented"

  type gen_lists_reply = { ret_val : int32 }

  let gen_lists ~(context_tag : context_tag) ~(range : int32) () :
      gen_lists_reply Lwt.t =
    failwith "not implemented"

  let feedback_buffer ~(context_tag : context_tag) ~(size : int32)
      ~(type_ : int32) () : unit Lwt.t =
    failwith "not implemented"

  let select_buffer ~(context_tag : context_tag) ~(size : int32) () : unit Lwt.t
      =
    failwith "not implemented"

  type render_mode_reply = {
    ret_val : int32;
    new_mode : int32;
    data : int32 list;
  }

  let render_mode ~(context_tag : context_tag) ~(mode : int32) () :
      render_mode_reply Lwt.t =
    failwith "not implemented"

  type rm_enum = [ `Gl_render | `Gl_feedback | `Gl_select ]

  let rm_enum_of_int : int -> rm_enum option = function
    | 7168 -> Some `Gl_render
    | 7169 -> Some `Gl_feedback
    | 7170 -> Some `Gl_select
    | _ -> None

  type finish_reply = unit

  let finish ~(context_tag : context_tag) () : finish_reply Lwt.t =
    failwith "not implemented"

  let pixel_storef ~(context_tag : context_tag) ~(pname : int32)
      ~(datum : float32) () : unit Lwt.t =
    failwith "not implemented"

  let pixel_storei ~(context_tag : context_tag) ~(pname : int32)
      ~(datum : int32) () : unit Lwt.t =
    failwith "not implemented"

  type read_pixels_reply = { data : char list }

  let read_pixels ~(context_tag : context_tag) ~(x : int32) ~(y : int32)
      ~(width : int32) ~(height : int32) ~(format : int32) ~(type_ : int32)
      ~(swap_bytes : bool) ~(lsb_first : bool) () : read_pixels_reply Lwt.t =
    failwith "not implemented"

  type get_booleanv_reply = { datum : bool; data : bool list }

  let get_booleanv ~(context_tag : context_tag) ~(pname : int32) () :
      get_booleanv_reply Lwt.t =
    failwith "not implemented"

  type get_clip_plane_reply = { data : float64 list }

  let get_clip_plane ~(context_tag : context_tag) ~(plane : int32) () :
      get_clip_plane_reply Lwt.t =
    failwith "not implemented"

  type get_doublev_reply = { datum : float64; data : float64 list }

  let get_doublev ~(context_tag : context_tag) ~(pname : int32) () :
      get_doublev_reply Lwt.t =
    failwith "not implemented"

  type get_error_reply = { error : int32 }

  let get_error ~(context_tag : context_tag) () : get_error_reply Lwt.t =
    failwith "not implemented"

  type get_floatv_reply = { datum : float32; data : float32 list }

  let get_floatv ~(context_tag : context_tag) ~(pname : int32) () :
      get_floatv_reply Lwt.t =
    failwith "not implemented"

  type get_integerv_reply = { datum : int32; data : int32 list }

  let get_integerv ~(context_tag : context_tag) ~(pname : int32) () :
      get_integerv_reply Lwt.t =
    failwith "not implemented"

  type get_lightfv_reply = { datum : float32; data : float32 list }

  let get_lightfv ~(context_tag : context_tag) ~(light : int32) ~(pname : int32)
      () : get_lightfv_reply Lwt.t =
    failwith "not implemented"

  type get_lightiv_reply = { datum : int32; data : int32 list }

  let get_lightiv ~(context_tag : context_tag) ~(light : int32) ~(pname : int32)
      () : get_lightiv_reply Lwt.t =
    failwith "not implemented"

  type get_mapdv_reply = { datum : float64; data : float64 list }

  let get_mapdv ~(context_tag : context_tag) ~(target : int32) ~(query : int32)
      () : get_mapdv_reply Lwt.t =
    failwith "not implemented"

  type get_mapfv_reply = { datum : float32; data : float32 list }

  let get_mapfv ~(context_tag : context_tag) ~(target : int32) ~(query : int32)
      () : get_mapfv_reply Lwt.t =
    failwith "not implemented"

  type get_mapiv_reply = { datum : int32; data : int32 list }

  let get_mapiv ~(context_tag : context_tag) ~(target : int32) ~(query : int32)
      () : get_mapiv_reply Lwt.t =
    failwith "not implemented"

  type get_materialfv_reply = { datum : float32; data : float32 list }

  let get_materialfv ~(context_tag : context_tag) ~(face : int32)
      ~(pname : int32) () : get_materialfv_reply Lwt.t =
    failwith "not implemented"

  type get_materialiv_reply = { datum : int32; data : int32 list }

  let get_materialiv ~(context_tag : context_tag) ~(face : int32)
      ~(pname : int32) () : get_materialiv_reply Lwt.t =
    failwith "not implemented"

  type get_pixel_mapfv_reply = { datum : float32; data : float32 list }

  let get_pixel_mapfv ~(context_tag : context_tag) ~(map : int32) () :
      get_pixel_mapfv_reply Lwt.t =
    failwith "not implemented"

  type get_pixel_mapuiv_reply = { datum : int32; data : int32 list }

  let get_pixel_mapuiv ~(context_tag : context_tag) ~(map : int32) () :
      get_pixel_mapuiv_reply Lwt.t =
    failwith "not implemented"

  type get_pixel_mapusv_reply = { datum : int; data : int list }

  let get_pixel_mapusv ~(context_tag : context_tag) ~(map : int32) () :
      get_pixel_mapusv_reply Lwt.t =
    failwith "not implemented"

  type get_polygon_stipple_reply = { data : char list }

  let get_polygon_stipple ~(context_tag : context_tag) ~(lsb_first : bool) () :
      get_polygon_stipple_reply Lwt.t =
    failwith "not implemented"

  type get_string_reply = { string : char list }

  let get_string ~(context_tag : context_tag) ~(name : int32) () :
      get_string_reply Lwt.t =
    failwith "not implemented"

  type get_tex_envfv_reply = { datum : float32; data : float32 list }

  let get_tex_envfv ~(context_tag : context_tag) ~(target : int32)
      ~(pname : int32) () : get_tex_envfv_reply Lwt.t =
    failwith "not implemented"

  type get_tex_enviv_reply = { datum : int32; data : int32 list }

  let get_tex_enviv ~(context_tag : context_tag) ~(target : int32)
      ~(pname : int32) () : get_tex_enviv_reply Lwt.t =
    failwith "not implemented"

  type get_tex_gendv_reply = { datum : float64; data : float64 list }

  let get_tex_gendv ~(context_tag : context_tag) ~(coord : int32)
      ~(pname : int32) () : get_tex_gendv_reply Lwt.t =
    failwith "not implemented"

  type get_tex_genfv_reply = { datum : float32; data : float32 list }

  let get_tex_genfv ~(context_tag : context_tag) ~(coord : int32)
      ~(pname : int32) () : get_tex_genfv_reply Lwt.t =
    failwith "not implemented"

  type get_tex_geniv_reply = { datum : int32; data : int32 list }

  let get_tex_geniv ~(context_tag : context_tag) ~(coord : int32)
      ~(pname : int32) () : get_tex_geniv_reply Lwt.t =
    failwith "not implemented"

  type get_tex_image_reply = {
    width : int32;
    height : int32;
    depth : int32;
    data : char list;
  }

  let get_tex_image ~(context_tag : context_tag) ~(target : int32)
      ~(level : int32) ~(format : int32) ~(type_ : int32) ~(swap_bytes : bool)
      () : get_tex_image_reply Lwt.t =
    failwith "not implemented"

  type get_tex_parameterfv_reply = { datum : float32; data : float32 list }

  let get_tex_parameterfv ~(context_tag : context_tag) ~(target : int32)
      ~(pname : int32) () : get_tex_parameterfv_reply Lwt.t =
    failwith "not implemented"

  type get_tex_parameteriv_reply = { datum : int32; data : int32 list }

  let get_tex_parameteriv ~(context_tag : context_tag) ~(target : int32)
      ~(pname : int32) () : get_tex_parameteriv_reply Lwt.t =
    failwith "not implemented"

  type get_tex_level_parameterfv_reply = {
    datum : float32;
    data : float32 list;
  }

  let get_tex_level_parameterfv ~(context_tag : context_tag) ~(target : int32)
      ~(level : int32) ~(pname : int32) () :
      get_tex_level_parameterfv_reply Lwt.t =
    failwith "not implemented"

  type get_tex_level_parameteriv_reply = { datum : int32; data : int32 list }

  let get_tex_level_parameteriv ~(context_tag : context_tag) ~(target : int32)
      ~(level : int32) ~(pname : int32) () :
      get_tex_level_parameteriv_reply Lwt.t =
    failwith "not implemented"

  type is_enabled_reply = { ret_val : bool32 }

  let is_enabled ~(context_tag : context_tag) ~(capability : int32) () :
      is_enabled_reply Lwt.t =
    failwith "not implemented"

  type is_list_reply = { ret_val : bool32 }

  let is_list ~(context_tag : context_tag) ~(list : int32) () :
      is_list_reply Lwt.t =
    failwith "not implemented"

  let flush ~(context_tag : context_tag) () : unit Lwt.t =
    failwith "not implemented"

  type are_textures_resident_reply = { ret_val : bool32; data : bool list }

  let are_textures_resident ~(context_tag : context_tag)
      ~(textures : int32 list) () : are_textures_resident_reply Lwt.t =
    failwith "not implemented"

  let delete_textures ~(context_tag : context_tag) ~(textures : int32 list) () :
      unit Lwt.t =
    failwith "not implemented"

  type gen_textures_reply = { data : int32 list }

  let gen_textures ~(context_tag : context_tag) ~(n : int32) () :
      gen_textures_reply Lwt.t =
    failwith "not implemented"

  type is_texture_reply = { ret_val : bool32 }

  let is_texture ~(context_tag : context_tag) ~(texture : int32) () :
      is_texture_reply Lwt.t =
    failwith "not implemented"

  type get_color_table_reply = { width : int32; data : char list }

  let get_color_table ~(context_tag : context_tag) ~(target : int32)
      ~(format : int32) ~(type_ : int32) ~(swap_bytes : bool) () :
      get_color_table_reply Lwt.t =
    failwith "not implemented"

  type get_color_table_parameterfv_reply = {
    datum : float32;
    data : float32 list;
  }

  let get_color_table_parameterfv ~(context_tag : context_tag) ~(target : int32)
      ~(pname : int32) () : get_color_table_parameterfv_reply Lwt.t =
    failwith "not implemented"

  type get_color_table_parameteriv_reply = { datum : int32; data : int32 list }

  let get_color_table_parameteriv ~(context_tag : context_tag) ~(target : int32)
      ~(pname : int32) () : get_color_table_parameteriv_reply Lwt.t =
    failwith "not implemented"

  type get_convolution_filter_reply = {
    width : int32;
    height : int32;
    data : char list;
  }

  let get_convolution_filter ~(context_tag : context_tag) ~(target : int32)
      ~(format : int32) ~(type_ : int32) ~(swap_bytes : bool) () :
      get_convolution_filter_reply Lwt.t =
    failwith "not implemented"

  type get_convolution_parameterfv_reply = {
    datum : float32;
    data : float32 list;
  }

  let get_convolution_parameterfv ~(context_tag : context_tag) ~(target : int32)
      ~(pname : int32) () : get_convolution_parameterfv_reply Lwt.t =
    failwith "not implemented"

  type get_convolution_parameteriv_reply = { datum : int32; data : int32 list }

  let get_convolution_parameteriv ~(context_tag : context_tag) ~(target : int32)
      ~(pname : int32) () : get_convolution_parameteriv_reply Lwt.t =
    failwith "not implemented"

  type get_separable_filter_reply = {
    row_w : int32;
    col_h : int32;
    rows_and_cols : char list;
  }

  let get_separable_filter ~(context_tag : context_tag) ~(target : int32)
      ~(format : int32) ~(type_ : int32) ~(swap_bytes : bool) () :
      get_separable_filter_reply Lwt.t =
    failwith "not implemented"

  type get_histogram_reply = { width : int32; data : char list }

  let get_histogram ~(context_tag : context_tag) ~(target : int32)
      ~(format : int32) ~(type_ : int32) ~(swap_bytes : bool) ~(reset : bool) ()
      : get_histogram_reply Lwt.t =
    failwith "not implemented"

  type get_histogram_parameterfv_reply = {
    datum : float32;
    data : float32 list;
  }

  let get_histogram_parameterfv ~(context_tag : context_tag) ~(target : int32)
      ~(pname : int32) () : get_histogram_parameterfv_reply Lwt.t =
    failwith "not implemented"

  type get_histogram_parameteriv_reply = { datum : int32; data : int32 list }

  let get_histogram_parameteriv ~(context_tag : context_tag) ~(target : int32)
      ~(pname : int32) () : get_histogram_parameteriv_reply Lwt.t =
    failwith "not implemented"

  type get_minmax_reply = { data : char list }

  let get_minmax ~(context_tag : context_tag) ~(target : int32)
      ~(format : int32) ~(type_ : int32) ~(swap_bytes : bool) ~(reset : bool) ()
      : get_minmax_reply Lwt.t =
    failwith "not implemented"

  type get_minmax_parameterfv_reply = { datum : float32; data : float32 list }

  let get_minmax_parameterfv ~(context_tag : context_tag) ~(target : int32)
      ~(pname : int32) () : get_minmax_parameterfv_reply Lwt.t =
    failwith "not implemented"

  type get_minmax_parameteriv_reply = { datum : int32; data : int32 list }

  let get_minmax_parameteriv ~(context_tag : context_tag) ~(target : int32)
      ~(pname : int32) () : get_minmax_parameteriv_reply Lwt.t =
    failwith "not implemented"

  type get_compressed_tex_image_arb_reply = { size : int32; data : char list }

  let get_compressed_tex_image_arb ~(context_tag : context_tag)
      ~(target : int32) ~(level : int32) () :
      get_compressed_tex_image_arb_reply Lwt.t =
    failwith "not implemented"

  let delete_queries_arb ~(context_tag : context_tag) ~(ids : int32 list) () :
      unit Lwt.t =
    failwith "not implemented"

  type gen_queries_arb_reply = { data : int32 list }

  let gen_queries_arb ~(context_tag : context_tag) ~(n : int32) () :
      gen_queries_arb_reply Lwt.t =
    failwith "not implemented"

  type is_query_arb_reply = { ret_val : bool32 }

  let is_query_arb ~(context_tag : context_tag) ~(id : int32) () :
      is_query_arb_reply Lwt.t =
    failwith "not implemented"

  type get_queryiv_arb_reply = { datum : int32; data : int32 list }

  let get_queryiv_arb ~(context_tag : context_tag) ~(target : int32)
      ~(pname : int32) () : get_queryiv_arb_reply Lwt.t =
    failwith "not implemented"

  type get_query_objectiv_arb_reply = { datum : int32; data : int32 list }

  let get_query_objectiv_arb ~(context_tag : context_tag) ~(id : int32)
      ~(pname : int32) () : get_query_objectiv_arb_reply Lwt.t =
    failwith "not implemented"

  type get_query_objectuiv_arb_reply = { datum : int32; data : int32 list }

  let get_query_objectuiv_arb ~(context_tag : context_tag) ~(id : int32)
      ~(pname : int32) () : get_query_objectuiv_arb_reply Lwt.t =
    failwith "not implemented"
end
[@@warning "-27"]

module Randr = struct
  type mode = xid

  let decode_mode = decode_xid

  type crtc = xid

  let decode_crtc = decode_xid

  type output = xid

  let decode_output = decode_xid

  type provider = xid

  let decode_provider = decode_xid

  type lease = xid

  let decode_lease = decode_xid

  type bad_output_error = unit

  type bad_crtc_error = unit

  type bad_mode_error = unit

  type bad_provider_error = unit

  type rotation_mask =
    [ `Rotate_0
    | `Rotate_90
    | `Rotate_180
    | `Rotate_270
    | `Reflect_x
    | `Reflect_y ]
    list

  type screen_size = { width : int; height : int; mwidth : int; mheight : int }

  let decode_screen_size buf ~at : (screen_size * int) option =
    let orig = at in
    let* width, at = decode_uint16 buf ~at in
    let* height, at = decode_uint16 buf ~at in
    let* mwidth, at = decode_uint16 buf ~at in
    let* mheight, at = decode_uint16 buf ~at in
    ignore orig;
    Some ({ width; height; mwidth; mheight }, at)

  type refresh_rates = { rates : int list }

  let decode_refresh_rates buf ~at : (refresh_rates * int) option =
    let orig = at in
    let* n_rates, at = decode_uint16 buf ~at in
    let n_rates = n_rates in
    let* rates, at = decode_list decode_uint16 n_rates buf ~at in
    ignore orig;
    Some ({ rates }, at)

  type query_version_reply = { major_version : int32; minor_version : int32 }

  let query_version ~(major_version : int32) ~(minor_version : int32) () :
      query_version_reply Lwt.t =
    failwith "not implemented"

  type set_config_enum =
    [ `Success | `Invalid_config_time | `Invalid_time | `Failed ]

  let set_config_enum_of_int : int -> set_config_enum option = function
    | 0 -> Some `Success
    | 1 -> Some `Invalid_config_time
    | 2 -> Some `Invalid_time
    | 3 -> Some `Failed
    | _ -> None

  type set_screen_config_reply = {
    status : set_config_enum;
    new_timestamp : Xproto.timestamp;
    config_timestamp : Xproto.timestamp;
    root : Xproto.window;
    subpixel_order : Render.sub_pixel_enum;
  }

  let set_screen_config ~(window : Xproto.window)
      ~(timestamp : Xproto.timestamp) ~(config_timestamp : Xproto.timestamp)
      ~(size_id : int) ~(rotation : rotation_mask) ~(rate : int) () :
      set_screen_config_reply Lwt.t =
    failwith "not implemented"

  type notify_mask_mask =
    [ `Screen_change
    | `Crtc_change
    | `Output_change
    | `Output_property
    | `Provider_change
    | `Provider_property
    | `Resource_change
    | `Lease ]
    list

  let select_input ~(window : Xproto.window) ~(enable : notify_mask_mask) () :
      unit Lwt.t =
    failwith "not implemented"

  type get_screen_info_reply = {
    rotations : rotation_mask;
    root : Xproto.window;
    timestamp : Xproto.timestamp;
    config_timestamp : Xproto.timestamp;
    size_id : int;
    rotation : rotation_mask;
    rate : int;
    n_info : int;
    sizes : screen_size list;
    rates : refresh_rates list;
  }

  let get_screen_info ~(window : Xproto.window) () : get_screen_info_reply Lwt.t
      =
    failwith "not implemented"

  type get_screen_size_range_reply = {
    min_width : int;
    min_height : int;
    max_width : int;
    max_height : int;
  }

  let get_screen_size_range ~(window : Xproto.window) () :
      get_screen_size_range_reply Lwt.t =
    failwith "not implemented"

  let set_screen_size ~(window : Xproto.window) ~(width : int) ~(height : int)
      ~(mm_width : int32) ~(mm_height : int32) () : unit Lwt.t =
    failwith "not implemented"

  type mode_flag_mask =
    [ `Hsync_positive
    | `Hsync_negative
    | `Vsync_positive
    | `Vsync_negative
    | `Interlace
    | `Double_scan
    | `Csync
    | `Csync_positive
    | `Csync_negative
    | `Hskew_present
    | `Bcast
    | `Pixel_multiplex
    | `Double_clock
    | `Halve_clock ]
    list

  type mode_info = {
    id : int32;
    width : int;
    height : int;
    dot_clock : int32;
    hsync_start : int;
    hsync_end : int;
    htotal : int;
    hskew : int;
    vsync_start : int;
    vsync_end : int;
    vtotal : int;
    name_len : int;
    mode_flags : mode_flag_mask;
  }

  let decode_mode_info buf ~at : (mode_info * int) option =
    let orig = at in
    let* id, at = decode_int32 buf ~at in
    let* width, at = decode_uint16 buf ~at in
    let* height, at = decode_uint16 buf ~at in
    let* dot_clock, at = decode_int32 buf ~at in
    let* hsync_start, at = decode_uint16 buf ~at in
    let* hsync_end, at = decode_uint16 buf ~at in
    let* htotal, at = decode_uint16 buf ~at in
    let* hskew, at = decode_uint16 buf ~at in
    let* vsync_start, at = decode_uint16 buf ~at in
    let* vsync_end, at = decode_uint16 buf ~at in
    let* vtotal, at = decode_uint16 buf ~at in
    let* name_len, at = decode_uint16 buf ~at in
    let* mode_flags, at = decode_int32 buf ~at in
    ignore orig;
    Some
      ( {
          id;
          width;
          height;
          dot_clock;
          hsync_start;
          hsync_end;
          htotal;
          hskew;
          vsync_start;
          vsync_end;
          vtotal;
          name_len;
          mode_flags = assert false;
        },
        at )

  type get_screen_resources_reply = {
    timestamp : Xproto.timestamp;
    config_timestamp : Xproto.timestamp;
    crtcs : crtc list;
    outputs : output list;
    modes : mode_info list;
    names : char list;
  }

  let get_screen_resources ~(window : Xproto.window) () :
      get_screen_resources_reply Lwt.t =
    failwith "not implemented"

  type connection_enum = [ `Connected | `Disconnected | `Unknown ]

  let connection_enum_of_int : int -> connection_enum option = function
    | 0 -> Some `Connected
    | 1 -> Some `Disconnected
    | 2 -> Some `Unknown
    | _ -> None

  type get_output_info_reply = {
    status : set_config_enum;
    timestamp : Xproto.timestamp;
    crtc : crtc;
    mm_width : int32;
    mm_height : int32;
    connection : connection_enum;
    subpixel_order : Render.sub_pixel_enum;
    num_preferred : int;
    crtcs : crtc list;
    modes : mode list;
    clones : output list;
    name : char list;
  }

  let get_output_info ~(output : output) ~(config_timestamp : Xproto.timestamp)
      () : get_output_info_reply Lwt.t =
    failwith "not implemented"

  type list_output_properties_reply = { atoms : Xproto.atom list }

  let list_output_properties ~(output : output) () :
      list_output_properties_reply Lwt.t =
    failwith "not implemented"

  type query_output_property_reply = {
    pending : bool;
    range : bool;
    immutable : bool;
    valid_values : int32 list;
  }

  let query_output_property ~(output : output) ~(property : Xproto.atom) () :
      query_output_property_reply Lwt.t =
    failwith "not implemented"

  let configure_output_property ~(output : output) ~(property : Xproto.atom)
      ~(pending : bool) ~(range : bool) ~(values : int32 list) () : unit Lwt.t =
    failwith "not implemented"

  let change_output_property ~(output : output) ~(property : Xproto.atom)
      ~(type_ : Xproto.atom) ~(format : int) ~(mode : Xproto.prop_mode_enum)
      ~(num_units : int32) ~(data : char list) () : unit Lwt.t =
    failwith "not implemented"

  let delete_output_property ~(output : output) ~(property : Xproto.atom) () :
      unit Lwt.t =
    failwith "not implemented"

  type get_output_property_reply = {
    format : int;
    type_ : (Xproto.atom_enum, Xproto.atom) alt;
    bytes_after : int32;
    num_items : int32;
    data : char list;
  }

  let get_output_property ~(output : output) ~(property : Xproto.atom)
      ~(type_ : (Xproto.get_property_type_enum, Xproto.atom) alt)
      ~(long_offset : int32) ~(long_length : int32) ~(delete : bool)
      ~(pending : bool) () : get_output_property_reply Lwt.t =
    failwith "not implemented"

  type create_mode_reply = { mode : mode }

  let create_mode ~(window : Xproto.window) ~(mode_info : mode_info)
      ~(name : char list) () : create_mode_reply Lwt.t =
    failwith "not implemented"

  let destroy_mode ~(mode : mode) () : unit Lwt.t = failwith "not implemented"

  let add_output_mode ~(output : output) ~(mode : mode) () : unit Lwt.t =
    failwith "not implemented"

  let delete_output_mode ~(output : output) ~(mode : mode) () : unit Lwt.t =
    failwith "not implemented"

  type get_crtc_info_reply = {
    status : set_config_enum;
    timestamp : Xproto.timestamp;
    x : int;
    y : int;
    width : int;
    height : int;
    mode : mode;
    rotation : rotation_mask;
    rotations : rotation_mask;
    outputs : output list;
    possible : output list;
  }

  let get_crtc_info ~(crtc : crtc) ~(config_timestamp : Xproto.timestamp) () :
      get_crtc_info_reply Lwt.t =
    failwith "not implemented"

  type set_crtc_config_reply = {
    status : set_config_enum;
    timestamp : Xproto.timestamp;
  }

  let set_crtc_config ~(crtc : crtc) ~(timestamp : Xproto.timestamp)
      ~(config_timestamp : Xproto.timestamp) ~(x : int) ~(y : int)
      ~(mode : mode) ~(rotation : rotation_mask) ~(outputs : output list) () :
      set_crtc_config_reply Lwt.t =
    failwith "not implemented"

  type get_crtc_gamma_size_reply = { size : int }

  let get_crtc_gamma_size ~(crtc : crtc) () : get_crtc_gamma_size_reply Lwt.t =
    failwith "not implemented"

  type get_crtc_gamma_reply = {
    red : int list;
    green : int list;
    blue : int list;
  }

  let get_crtc_gamma ~(crtc : crtc) () : get_crtc_gamma_reply Lwt.t =
    failwith "not implemented"

  let set_crtc_gamma ~(crtc : crtc) ~(red : int list) ~(green : int list)
      ~(blue : int list) () : unit Lwt.t =
    failwith "not implemented"

  type get_screen_resources_current_reply = {
    timestamp : Xproto.timestamp;
    config_timestamp : Xproto.timestamp;
    crtcs : crtc list;
    outputs : output list;
    modes : mode_info list;
    names : char list;
  }

  let get_screen_resources_current ~(window : Xproto.window) () :
      get_screen_resources_current_reply Lwt.t =
    failwith "not implemented"

  type transform_mask = [ `Unit | `Scale_up | `Scale_down | `Projective ] list

  let set_crtc_transform ~(crtc : crtc) ~(transform : Render.transform)
      ~(filter_name : char list) ~(filter_params : Render.fixed list) () :
      unit Lwt.t =
    failwith "not implemented"

  type get_crtc_transform_reply = {
    pending_transform : Render.transform;
    has_transforms : bool;
    current_transform : Render.transform;
    pending_filter_name : char list;
    pending_params : Render.fixed list;
    current_filter_name : char list;
    current_params : Render.fixed list;
  }

  let get_crtc_transform ~(crtc : crtc) () : get_crtc_transform_reply Lwt.t =
    failwith "not implemented"

  type get_panning_reply = {
    status : set_config_enum;
    timestamp : Xproto.timestamp;
    left : int;
    top : int;
    width : int;
    height : int;
    track_left : int;
    track_top : int;
    track_width : int;
    track_height : int;
    border_left : int;
    border_top : int;
    border_right : int;
    border_bottom : int;
  }

  let get_panning ~(crtc : crtc) () : get_panning_reply Lwt.t =
    failwith "not implemented"

  type set_panning_reply = {
    status : set_config_enum;
    timestamp : Xproto.timestamp;
  }

  let set_panning ~(crtc : crtc) ~(timestamp : Xproto.timestamp) ~(left : int)
      ~(top : int) ~(width : int) ~(height : int) ~(track_left : int)
      ~(track_top : int) ~(track_width : int) ~(track_height : int)
      ~(border_left : int) ~(border_top : int) ~(border_right : int)
      ~(border_bottom : int) () : set_panning_reply Lwt.t =
    failwith "not implemented"

  let set_output_primary ~(window : Xproto.window) ~(output : output) () :
      unit Lwt.t =
    failwith "not implemented"

  type get_output_primary_reply = { output : output }

  let get_output_primary ~(window : Xproto.window) () :
      get_output_primary_reply Lwt.t =
    failwith "not implemented"

  type get_providers_reply = {
    timestamp : Xproto.timestamp;
    providers : provider list;
  }

  let get_providers ~(window : Xproto.window) () : get_providers_reply Lwt.t =
    failwith "not implemented"

  type provider_capability_mask =
    [ `Source_output | `Sink_output | `Source_offload | `Sink_offload ] list

  type get_provider_info_reply = {
    status : int;
    timestamp : Xproto.timestamp;
    capabilities : provider_capability_mask;
    crtcs : crtc list;
    outputs : output list;
    associated_providers : provider list;
    associated_capability : int32 list;
    name : char list;
  }

  let get_provider_info ~(provider : provider)
      ~(config_timestamp : Xproto.timestamp) () : get_provider_info_reply Lwt.t
      =
    failwith "not implemented"

  let set_provider_offload_sink ~(provider : provider)
      ~(sink_provider : provider) ~(config_timestamp : Xproto.timestamp) () :
      unit Lwt.t =
    failwith "not implemented"

  let set_provider_output_source ~(provider : provider)
      ~(source_provider : provider) ~(config_timestamp : Xproto.timestamp) () :
      unit Lwt.t =
    failwith "not implemented"

  type list_provider_properties_reply = { atoms : Xproto.atom list }

  let list_provider_properties ~(provider : provider) () :
      list_provider_properties_reply Lwt.t =
    failwith "not implemented"

  type query_provider_property_reply = {
    pending : bool;
    range : bool;
    immutable : bool;
    valid_values : int32 list;
  }

  let query_provider_property ~(provider : provider) ~(property : Xproto.atom)
      () : query_provider_property_reply Lwt.t =
    failwith "not implemented"

  let configure_provider_property ~(provider : provider)
      ~(property : Xproto.atom) ~(pending : bool) ~(range : bool)
      ~(values : int32 list) () : unit Lwt.t =
    failwith "not implemented"

  let change_provider_property ~(provider : provider) ~(property : Xproto.atom)
      ~(type_ : Xproto.atom) ~(format : int) ~(mode : int) ~(num_items : int32)
      ~(data : char list) () : unit Lwt.t =
    failwith "not implemented"

  let delete_provider_property ~(provider : provider) ~(property : Xproto.atom)
      () : unit Lwt.t =
    failwith "not implemented"

  type get_provider_property_reply = {
    format : int;
    type_ : Xproto.atom;
    bytes_after : int32;
    num_items : int32;
    data : char list;
  }

  let get_provider_property ~(provider : provider) ~(property : Xproto.atom)
      ~(type_ : Xproto.atom) ~(long_offset : int32) ~(long_length : int32)
      ~(delete : bool) ~(pending : bool) () : get_provider_property_reply Lwt.t
      =
    failwith "not implemented"

  type screen_change_notify_event = {
    rotation : rotation_mask;
    timestamp : Xproto.timestamp;
    config_timestamp : Xproto.timestamp;
    root : Xproto.window;
    request_window : Xproto.window;
    size_id : int;
    subpixel_order : Render.sub_pixel_enum;
    width : int;
    height : int;
    mwidth : int;
    mheight : int;
  }

  type notify_enum =
    [ `Crtc_change
    | `Output_change
    | `Output_property
    | `Provider_change
    | `Provider_property
    | `Resource_change
    | `Lease ]

  let notify_enum_of_int : int -> notify_enum option = function
    | 0 -> Some `Crtc_change
    | 1 -> Some `Output_change
    | 2 -> Some `Output_property
    | 3 -> Some `Provider_change
    | 4 -> Some `Provider_property
    | 5 -> Some `Resource_change
    | 6 -> Some `Lease
    | _ -> None

  type crtc_change = {
    timestamp : Xproto.timestamp;
    window : Xproto.window;
    crtc : crtc;
    mode : mode;
    rotation : rotation_mask;
    x : int;
    y : int;
    width : int;
    height : int;
  }

  let decode_crtc_change buf ~at : (crtc_change * int) option =
    let orig = at in
    let* timestamp, at = Xproto.decode_timestamp buf ~at in
    let* window, at = Xproto.decode_window buf ~at in
    let* crtc, at = decode_crtc buf ~at in
    let* mode, at = decode_mode buf ~at in
    let* rotation, at = decode_uint16 buf ~at in
    let at = at + 2 in
    let* x, at = decode_int16 buf ~at in
    let* y, at = decode_int16 buf ~at in
    let* width, at = decode_uint16 buf ~at in
    let* height, at = decode_uint16 buf ~at in
    ignore orig;
    Some ({ timestamp; window; crtc; mode; rotation = assert false; x; y; width; height }, at)

  type output_change = {
    timestamp : Xproto.timestamp;
    config_timestamp : Xproto.timestamp;
    window : Xproto.window;
    output : output;
    crtc : crtc;
    mode : mode;
    rotation : rotation_mask;
    connection : connection_enum;
    subpixel_order : Render.sub_pixel_enum;
  }

  let decode_output_change buf ~at : (output_change * int) option =
    let orig = at in
    let* timestamp, at = Xproto.decode_timestamp buf ~at in
    let* config_timestamp, at = Xproto.decode_timestamp buf ~at in
    let* window, at = Xproto.decode_window buf ~at in
    let* output, at = decode_output buf ~at in
    let* crtc, at = decode_crtc buf ~at in
    let* mode, at = decode_mode buf ~at in
    let* rotation, at = decode_uint16 buf ~at in
    let* connection, at =
      decode_enum decode_uint8 (fun x -> x) connection_enum_of_int buf ~at
    in
    let* subpixel_order, at =
      decode_enum decode_uint8 (fun x -> x) Render.sub_pixel_enum_of_int buf ~at
    in
    ignore orig;
    Some
      ( {
          timestamp;
          config_timestamp;
          window;
          output;
          crtc;
          mode;
          rotation = assert false;
          connection;
          subpixel_order;
        },
        at )

  type output_property = {
    window : Xproto.window;
    output : output;
    atom : Xproto.atom;
    timestamp : Xproto.timestamp;
    status : Xproto.property_enum;
  }

  let decode_output_property buf ~at : (output_property * int) option =
    let orig = at in
    let* window, at = Xproto.decode_window buf ~at in
    let* output, at = decode_output buf ~at in
    let* atom, at = Xproto.decode_atom buf ~at in
    let* timestamp, at = Xproto.decode_timestamp buf ~at in
    let* status, at =
      decode_enum decode_uint8 (fun x -> x) Xproto.property_enum_of_int buf ~at
    in
    let at = at + 11 in
    ignore orig;
    Some ({ window; output; atom; timestamp; status }, at)

  type provider_change = {
    timestamp : Xproto.timestamp;
    window : Xproto.window;
    provider : provider;
  }

  let decode_provider_change buf ~at : (provider_change * int) option =
    let orig = at in
    let* timestamp, at = Xproto.decode_timestamp buf ~at in
    let* window, at = Xproto.decode_window buf ~at in
    let* provider, at = decode_provider buf ~at in
    let at = at + 16 in
    ignore orig;
    Some ({ timestamp; window; provider }, at)

  type provider_property = {
    window : Xproto.window;
    provider : provider;
    atom : Xproto.atom;
    timestamp : Xproto.timestamp;
    state : int;
  }

  let decode_provider_property buf ~at : (provider_property * int) option =
    let orig = at in
    let* window, at = Xproto.decode_window buf ~at in
    let* provider, at = decode_provider buf ~at in
    let* atom, at = Xproto.decode_atom buf ~at in
    let* timestamp, at = Xproto.decode_timestamp buf ~at in
    let* state, at = decode_uint8 buf ~at in
    let at = at + 11 in
    ignore orig;
    Some ({ window; provider; atom; timestamp; state }, at)

  type resource_change = {
    timestamp : Xproto.timestamp;
    window : Xproto.window;
  }

  let decode_resource_change buf ~at : (resource_change * int) option =
    let orig = at in
    let* timestamp, at = Xproto.decode_timestamp buf ~at in
    let* window, at = Xproto.decode_window buf ~at in
    let at = at + 20 in
    ignore orig;
    Some ({ timestamp; window }, at)

  type monitor_info = {
    name : Xproto.atom;
    primary : bool;
    automatic : bool;
    x : int;
    y : int;
    width : int;
    height : int;
    width_in_millimeters : int32;
    height_in_millimeters : int32;
    outputs : output list;
  }

  let decode_monitor_info buf ~at : (monitor_info * int) option =
    let orig = at in
    let* name, at = Xproto.decode_atom buf ~at in
    let* primary, at = decode_bool buf ~at in
    let* automatic, at = decode_bool buf ~at in
    let* n_output, at = decode_uint16 buf ~at in
    let n_output = n_output in
    let* x, at = decode_int16 buf ~at in
    let* y, at = decode_int16 buf ~at in
    let* width, at = decode_uint16 buf ~at in
    let* height, at = decode_uint16 buf ~at in
    let* width_in_millimeters, at = decode_int32 buf ~at in
    let* height_in_millimeters, at = decode_int32 buf ~at in
    let* outputs, at = decode_list decode_output n_output buf ~at in
    ignore orig;
    Some
      ( {
          name;
          primary;
          automatic;
          x;
          y;
          width;
          height;
          width_in_millimeters;
          height_in_millimeters;
          outputs;
        },
        at )

  type get_monitors_reply = {
    timestamp : Xproto.timestamp;
    n_outputs : int32;
    monitors : monitor_info list;
  }

  let get_monitors ~(window : Xproto.window) ~(get_active : bool) () :
      get_monitors_reply Lwt.t =
    failwith "not implemented"

  let set_monitor ~(window : Xproto.window) ~(monitorinfo : monitor_info) () :
      unit Lwt.t =
    failwith "not implemented"

  let delete_monitor ~(window : Xproto.window) ~(name : Xproto.atom) () :
      unit Lwt.t =
    failwith "not implemented"

  type create_lease_reply = { nfd : int; master_fd : file_descr }

  let create_lease ~(window : Xproto.window) ~(lid : lease) ~(crtcs : crtc list)
      ~(outputs : output list) () : create_lease_reply Lwt.t =
    failwith "not implemented"

  let free_lease ~(lid : lease) ~(terminate : char) () : unit Lwt.t =
    failwith "not implemented"

  type lease_notify = {
    timestamp : Xproto.timestamp;
    window : Xproto.window;
    lease : lease;
    created : int;
  }

  let decode_lease_notify buf ~at : (lease_notify * int) option =
    let orig = at in
    let* timestamp, at = Xproto.decode_timestamp buf ~at in
    let* window, at = Xproto.decode_window buf ~at in
    let* lease, at = decode_lease buf ~at in
    let* created, at = decode_uint8 buf ~at in
    let at = at + 15 in
    ignore orig;
    Some ({ timestamp; window; lease; created }, at)

  type notify_variant =
    | Crtc_change of { cc : crtc_change }
    | Output_change of { oc : output_change }
    | Output_property of { op : output_property }
    | Provider_change of { pc : provider_change }
    | Provider_property of { pp : provider_property }
    | Resource_change of { rc : resource_change }
    | Lease of { lc : lease_notify }

  type notify_event = { u : notify_variant }
end
[@@warning "-27"]

module Sync = struct
  type alarm = xid

  let decode_alarm = decode_xid

  type alarmstate_enum = [ `Active | `Inactive | `Destroyed ]

  let alarmstate_enum_of_int : int -> alarmstate_enum option = function
    | 0 -> Some `Active
    | 1 -> Some `Inactive
    | 2 -> Some `Destroyed
    | _ -> None

  type counter = xid

  let decode_counter = decode_xid

  type fence = xid

  let decode_fence = decode_xid

  type testtype_enum =
    [ `Positive_transition
    | `Negative_transition
    | `Positive_comparison
    | `Negative_comparison ]

  let testtype_enum_of_int : int -> testtype_enum option = function
    | 0 -> Some `Positive_transition
    | 1 -> Some `Negative_transition
    | 2 -> Some `Positive_comparison
    | 3 -> Some `Negative_comparison
    | _ -> None

  type valuetype_enum = [ `Absolute | `Relative ]

  let valuetype_enum_of_int : int -> valuetype_enum option = function
    | 0 -> Some `Absolute
    | 1 -> Some `Relative
    | _ -> None

  type ca_mask =
    [ `Counter | `Value_type | `Value | `Test_type | `Delta | `Events ] list

  type int64 = { hi : int32; lo : int32 }

  let decode_int64 buf ~at : (int64 * int) option =
    let orig = at in
    let* hi, at = decode_int32 buf ~at in
    let* lo, at = decode_int32 buf ~at in
    ignore orig;
    Some ({ hi; lo }, at)

  type systemcounter = {
    counter : counter;
    resolution : int64;
    name : char list;
  }

  let decode_systemcounter buf ~at : (systemcounter * int) option =
    let orig = at in
    let* counter, at = decode_counter buf ~at in
    let* resolution, at = decode_int64 buf ~at in
    let* name_len, at = decode_uint16 buf ~at in
    let name_len = name_len in
    let* name, at = decode_list decode_char name_len buf ~at in
    let at = at + ((at - orig) mod 4) in
    ignore orig;
    Some ({ counter; resolution; name }, at)

  type trigger = {
    counter : counter;
    wait_type : valuetype_enum;
    wait_value : int64;
    test_type : testtype_enum;
  }

  let decode_trigger buf ~at : (trigger * int) option =
    let orig = at in
    let* counter, at = decode_counter buf ~at in
    let* wait_type, at =
      decode_enum decode_int32 Int32.to_int valuetype_enum_of_int buf ~at
    in
    let* wait_value, at = decode_int64 buf ~at in
    let* test_type, at =
      decode_enum decode_int32 Int32.to_int testtype_enum_of_int buf ~at
    in
    ignore orig;
    Some ({ counter; wait_type; wait_value; test_type }, at)

  type waitcondition = { trigger : trigger; event_threshold : int64 }

  let decode_waitcondition buf ~at : (waitcondition * int) option =
    let orig = at in
    let* trigger, at = decode_trigger buf ~at in
    let* event_threshold, at = decode_int64 buf ~at in
    ignore orig;
    Some ({ trigger; event_threshold }, at)

  type counter_error = {
    bad_counter : int32;
    minor_opcode : int;
    major_opcode : int;
  }

  type alarm_error = {
    bad_alarm : int32;
    minor_opcode : int;
    major_opcode : int;
  }

  type initialize_reply = { major_version : int; minor_version : int }

  let initialize ~(desired_major_version : int) ~(desired_minor_version : int)
      () : initialize_reply Lwt.t =
    failwith "not implemented"

  type list_system_counters_reply = { counters : systemcounter list }

  let list_system_counters () : list_system_counters_reply Lwt.t =
    failwith "not implemented"

  let create_counter ~(id : counter) ~(initial_value : int64) () : unit Lwt.t =
    failwith "not implemented"

  let destroy_counter ~(counter : counter) () : unit Lwt.t =
    failwith "not implemented"

  type query_counter_reply = { counter_value : int64 }

  let query_counter ~(counter : counter) () : query_counter_reply Lwt.t =
    failwith "not implemented"

  let await ~(wait_list : waitcondition list) () : unit Lwt.t =
    failwith "not implemented"

  let change_counter ~(counter : counter) ~(amount : int64) () : unit Lwt.t =
    failwith "not implemented"

  let set_counter ~(counter : counter) ~(value : int64) () : unit Lwt.t =
    failwith "not implemented"

  let create_alarm ~(id : alarm) ?(counter : counter option)
      ?(value_type : valuetype_enum option) ?(value : int64 option)
      ?(test_type : testtype_enum option) ?(delta : int64 option)
      ?(events : int32 option) () : unit Lwt.t =
    failwith "not implemented"

  let change_alarm ~(id : alarm) ?(counter : counter option)
      ?(value_type : valuetype_enum option) ?(value : int64 option)
      ?(test_type : testtype_enum option) ?(delta : int64 option)
      ?(events : int32 option) () : unit Lwt.t =
    failwith "not implemented"

  let destroy_alarm ~(alarm : alarm) () : unit Lwt.t =
    failwith "not implemented"

  type query_alarm_reply = {
    trigger : trigger;
    delta : int64;
    events : bool;
    state : alarmstate_enum;
  }

  let query_alarm ~(alarm : alarm) () : query_alarm_reply Lwt.t =
    failwith "not implemented"

  let set_priority ~(id : int32) ~(priority : int32) () : unit Lwt.t =
    failwith "not implemented"

  type get_priority_reply = { priority : int32 }

  let get_priority ~(id : int32) () : get_priority_reply Lwt.t =
    failwith "not implemented"

  let create_fence ~(drawable : Xproto.drawable) ~(fence : fence)
      ~(initially_triggered : bool) () : unit Lwt.t =
    failwith "not implemented"

  let trigger_fence ~(fence : fence) () : unit Lwt.t =
    failwith "not implemented"

  let reset_fence ~(fence : fence) () : unit Lwt.t = failwith "not implemented"

  let destroy_fence ~(fence : fence) () : unit Lwt.t =
    failwith "not implemented"

  type query_fence_reply = { triggered : bool }

  let query_fence ~(fence : fence) () : query_fence_reply Lwt.t =
    failwith "not implemented"

  let await_fence ~(fence_list : fence list) () : unit Lwt.t =
    failwith "not implemented"

  type counter_notify_event = {
    kind : int;
    counter : counter;
    wait_value : int64;
    counter_value : int64;
    timestamp : Xproto.timestamp;
    count : int;
    destroyed : bool;
  }

  type alarm_notify_event = {
    kind : int;
    alarm : alarm;
    counter_value : int64;
    alarm_value : int64;
    timestamp : Xproto.timestamp;
    state : alarmstate_enum;
  }
end
[@@warning "-27"]

module Present = struct
  type event_enum =
    [ `Configure_notify | `Complete_notify | `Idle_notify | `Redirect_notify ]

  let event_enum_of_int : int -> event_enum option = function
    | 0 -> Some `Configure_notify
    | 1 -> Some `Complete_notify
    | 2 -> Some `Idle_notify
    | 3 -> Some `Redirect_notify
    | _ -> None

  type event_mask_mask =
    ( [ `Configure_notify | `Complete_notify | `Idle_notify | `Redirect_notify ],
      [ `No_event ] )
    mask

  type option_mask = ([ `Async | `Copy | `Ust | `Suboptimal ], [ `None ]) mask

  type capability_mask = ([ `Async | `Fence | `Ust ], [ `None ]) mask

  type complete_kind_enum = [ `Pixmap | `Notify_msc ]

  let complete_kind_enum_of_int : int -> complete_kind_enum option = function
    | 0 -> Some `Pixmap
    | 1 -> Some `Notify_msc
    | _ -> None

  type complete_mode_enum = [ `Copy | `Flip | `Skip | `Suboptimal_copy ]

  let complete_mode_enum_of_int : int -> complete_mode_enum option = function
    | 0 -> Some `Copy
    | 1 -> Some `Flip
    | 2 -> Some `Skip
    | 3 -> Some `Suboptimal_copy
    | _ -> None

  type notify = { window : Xproto.window; serial : int32 }

  let decode_notify buf ~at : (notify * int) option =
    let orig = at in
    let* window, at = Xproto.decode_window buf ~at in
    let* serial, at = decode_int32 buf ~at in
    ignore orig;
    Some ({ window; serial }, at)

  type query_version_reply = { major_version : int32; minor_version : int32 }

  let query_version ~(major_version : int32) ~(minor_version : int32) () :
      query_version_reply Lwt.t =
    failwith "not implemented"

  let pixmap ~(window : Xproto.window) ~(pixmap : Xproto.pixmap)
      ~(serial : int32) ~(valid : Xfixes.region) ~(update : Xfixes.region)
      ~(x_off : int) ~(y_off : int) ~(target_crtc : Randr.crtc)
      ~(wait_fence : Sync.fence) ~(idle_fence : Sync.fence) ~(options : int32)
      ~(target_msc : int64) ~(divisor : int64) ~(remainder : int64)
      ~(notifies : notify list) () : unit Lwt.t =
    failwith "not implemented"

  let notify_msc ~(window : Xproto.window) ~(serial : int32)
      ~(target_msc : int64) ~(divisor : int64) ~(remainder : int64) () :
      unit Lwt.t =
    failwith "not implemented"

  type event = xid

  let decode_event = decode_xid

  let select_input ~(eid : event) ~(window : Xproto.window)
      ~(event_mask : event_mask_mask) () : unit Lwt.t =
    failwith "not implemented"

  type query_capabilities_reply = { capabilities : int32 }

  let query_capabilities ~(target : int32) () : query_capabilities_reply Lwt.t =
    failwith "not implemented"

  type generic_event = {
    extension : int;
    length : int32;
    evtype : int;
    event : event;
  }

  type configure_notify_event = {
    event : event;
    window : Xproto.window;
    x : int;
    y : int;
    width : int;
    height : int;
    off_x : int;
    off_y : int;
    pixmap_width : int;
    pixmap_height : int;
    pixmap_flags : int32;
  }

  type complete_notify_event = {
    kind : complete_kind_enum;
    mode : complete_mode_enum;
    event : event;
    window : Xproto.window;
    serial : int32;
    ust : int64;
    msc : int64;
  }

  type idle_notify_event = {
    event : event;
    window : Xproto.window;
    serial : int32;
    pixmap : Xproto.pixmap;
    idle_fence : Sync.fence;
  }

  type redirect_notify_event = {
    update_window : bool;
    event : event;
    event_window : Xproto.window;
    window : Xproto.window;
    pixmap : Xproto.pixmap;
    serial : int32;
    valid_region : Xfixes.region;
    update_region : Xfixes.region;
    valid_rect : Xproto.rectangle;
    update_rect : Xproto.rectangle;
    x_off : int;
    y_off : int;
    target_crtc : Randr.crtc;
    wait_fence : Sync.fence;
    idle_fence : Sync.fence;
    options : int32;
    target_msc : int64;
    divisor : int64;
    remainder : int64;
    notifies : notify list;
  }
end
[@@warning "-27"]

module Record = struct
  type context = xid

  let decode_context = decode_xid

  type range8 = { first : int; last : int }

  let decode_range8 buf ~at : (range8 * int) option =
    let orig = at in
    let* first, at = decode_uint8 buf ~at in
    let* last, at = decode_uint8 buf ~at in
    ignore orig;
    Some ({ first; last }, at)

  type range16 = { first : int; last : int }

  let decode_range16 buf ~at : (range16 * int) option =
    let orig = at in
    let* first, at = decode_uint16 buf ~at in
    let* last, at = decode_uint16 buf ~at in
    ignore orig;
    Some ({ first; last }, at)

  type ext_range = { major : range8; minor : range16 }

  let decode_ext_range buf ~at : (ext_range * int) option =
    let orig = at in
    let* major, at = decode_range8 buf ~at in
    let* minor, at = decode_range16 buf ~at in
    ignore orig;
    Some ({ major; minor }, at)

  type range = {
    core_requests : range8;
    core_replies : range8;
    ext_requests : ext_range;
    ext_replies : ext_range;
    delivered_events : range8;
    device_events : range8;
    errors : range8;
    client_started : bool;
    client_died : bool;
  }

  let decode_range buf ~at : (range * int) option =
    let orig = at in
    let* core_requests, at = decode_range8 buf ~at in
    let* core_replies, at = decode_range8 buf ~at in
    let* ext_requests, at = decode_ext_range buf ~at in
    let* ext_replies, at = decode_ext_range buf ~at in
    let* delivered_events, at = decode_range8 buf ~at in
    let* device_events, at = decode_range8 buf ~at in
    let* errors, at = decode_range8 buf ~at in
    let* client_started, at = decode_bool buf ~at in
    let* client_died, at = decode_bool buf ~at in
    ignore orig;
    Some
      ( {
          core_requests;
          core_replies;
          ext_requests;
          ext_replies;
          delivered_events;
          device_events;
          errors;
          client_started;
          client_died;
        },
        at )

  type element_header = int

  let decode_element_header = decode_uint8

  type h_type_mask =
    [ `From_server_time | `From_client_time | `From_client_sequence ] list

  type client_spec = int32

  let decode_client_spec = decode_int32

  type cs_enum = [ `Current_clients | `Future_clients | `All_clients ]

  let cs_enum_of_int : int -> cs_enum option = function
    | 1 -> Some `Current_clients
    | 2 -> Some `Future_clients
    | 3 -> Some `All_clients
    | _ -> None

  type client_info = { client_resource : client_spec; ranges : range list }

  let decode_client_info buf ~at : (client_info * int) option =
    let orig = at in
    let* client_resource, at = decode_client_spec buf ~at in
    let* num_ranges, at = decode_int32 buf ~at in
    let num_ranges = Int32.to_int num_ranges in
    let num_ranges = num_ranges in
    let* ranges, at = decode_list decode_range num_ranges buf ~at in
    ignore orig;
    Some ({ client_resource; ranges }, at)

  type bad_context_error = { invalid_record : int32 }

  type query_version_reply = { major_version : int; minor_version : int }

  let query_version ~(major_version : int) ~(minor_version : int) () :
      query_version_reply Lwt.t =
    failwith "not implemented"

  let create_context ~(context : context) ~(element_header : element_header)
      ~(client_specs : client_spec list) ~(ranges : range list) () : unit Lwt.t
      =
    failwith "not implemented"

  let register_clients ~(context : context) ~(element_header : element_header)
      ~(client_specs : client_spec list) ~(ranges : range list) () : unit Lwt.t
      =
    failwith "not implemented"

  let unregister_clients ~(context : context) ~(client_specs : client_spec list)
      () : unit Lwt.t =
    failwith "not implemented"

  type get_context_reply = {
    enabled : bool;
    element_header : element_header;
    intercepted_clients : client_info list;
  }

  let get_context ~(context : context) () : get_context_reply Lwt.t =
    failwith "not implemented"

  type enable_context_reply = {
    category : int;
    element_header : element_header;
    client_swapped : bool;
    xid_base : int32;
    server_time : int32;
    rec_sequence_num : int32;
    data : char list;
  }

  let enable_context ~(context : context) () : enable_context_reply Lwt.t =
    failwith "not implemented"

  let disable_context ~(context : context) () : unit Lwt.t =
    failwith "not implemented"

  let free_context ~(context : context) () : unit Lwt.t =
    failwith "not implemented"
end
[@@warning "-27"]

module Res = struct
  type client = { resource_base : int32; resource_mask : int32 }

  let decode_client buf ~at : (client * int) option =
    let orig = at in
    let* resource_base, at = decode_int32 buf ~at in
    let* resource_mask, at = decode_int32 buf ~at in
    ignore orig;
    Some ({ resource_base; resource_mask }, at)

  type type_ = { resource_type : Xproto.atom; count : int32 }

  let decode_type buf ~at : (type_ * int) option =
    let orig = at in
    let* resource_type, at = Xproto.decode_atom buf ~at in
    let* count, at = decode_int32 buf ~at in
    ignore orig;
    Some ({ resource_type; count }, at)

  type client_id_mask_mask = [ `Client_xid | `Local_client_pid ] list

  type client_id_spec = { client : int32; mask : client_id_mask_mask }

  let decode_client_id_spec buf ~at : (client_id_spec * int) option =
    let orig = at in
    let* client, at = decode_int32 buf ~at in
    let* mask, at = decode_int32 buf ~at in
    ignore orig;
    Some ({ client; mask = assert false }, at)

  type client_id_value = {
    spec : client_id_spec;
    length : int32;
    value : int32 list;
  }

  let decode_client_id_value buf ~at : (client_id_value * int) option =
    let orig = at in
    let* spec, at = decode_client_id_spec buf ~at in
    let* length, at = decode_int32 buf ~at in
    let* value, at = decode_list decode_int32 (length / 4) buf ~at in
    ignore orig;
    Some ({ spec; length; value }, at)

  type resource_id_spec = { resource : int32; type_ : int32 }

  let decode_resource_id_spec buf ~at : (resource_id_spec * int) option =
    let orig = at in
    let* resource, at = decode_int32 buf ~at in
    let* type_, at = decode_int32 buf ~at in
    ignore orig;
    Some ({ resource; type_ }, at)

  type resource_size_spec = {
    spec : resource_id_spec;
    bytes : int32;
    ref_count : int32;
    use_count : int32;
  }

  let decode_resource_size_spec buf ~at : (resource_size_spec * int) option =
    let orig = at in
    let* spec, at = decode_resource_id_spec buf ~at in
    let* bytes, at = decode_int32 buf ~at in
    let* ref_count, at = decode_int32 buf ~at in
    let* use_count, at = decode_int32 buf ~at in
    ignore orig;
    Some ({ spec; bytes; ref_count; use_count }, at)

  type resource_size_value = {
    size : resource_size_spec;
    cross_references : resource_size_spec list;
  }

  let decode_resource_size_value buf ~at : (resource_size_value * int) option =
    let orig = at in
    let* size, at = decode_resource_size_spec buf ~at in
    let* num_cross_references, at = decode_int32 buf ~at in
    let num_cross_references = Int32.to_int num_cross_references in
    let num_cross_references = num_cross_references in
    let* cross_references, at =
      decode_list decode_resource_size_spec num_cross_references buf ~at
    in
    ignore orig;
    Some ({ size; cross_references }, at)

  type query_version_reply = { server_major : int; server_minor : int }

  let query_version ~(client_major : int) ~(client_minor : int) () :
      query_version_reply Lwt.t =
    failwith "not implemented"

  type query_clients_reply = { clients : client list }

  let query_clients () : query_clients_reply Lwt.t = failwith "not implemented"

  type query_client_resources_reply = { types : type_ list }

  let query_client_resources ~(xid : int32) () :
      query_client_resources_reply Lwt.t =
    failwith "not implemented"

  type query_client_pixmap_bytes_reply = {
    bytes : int32;
    bytes_overflow : int32;
  }

  let query_client_pixmap_bytes ~(xid : int32) () :
      query_client_pixmap_bytes_reply Lwt.t =
    failwith "not implemented"

  type query_client_ids_reply = { ids : client_id_value list }

  let query_client_ids ~(specs : client_id_spec list) () :
      query_client_ids_reply Lwt.t =
    failwith "not implemented"

  type query_resource_bytes_reply = { sizes : resource_size_value list }

  let query_resource_bytes ~(client : int32) ~(specs : resource_id_spec list) ()
      : query_resource_bytes_reply Lwt.t =
    failwith "not implemented"
end
[@@warning "-27"]

module Screensaver = struct
  type kind_enum = [ `Blanked | `Internal | `External ]

  let kind_enum_of_int : int -> kind_enum option = function
    | 0 -> Some `Blanked
    | 1 -> Some `Internal
    | 2 -> Some `External
    | _ -> None

  type event_mask = [ `Notify_mask | `Cycle_mask ] list

  type state_enum = [ `Off | `On | `Cycle | `Disabled ]

  let state_enum_of_int : int -> state_enum option = function
    | 0 -> Some `Off
    | 1 -> Some `On
    | 2 -> Some `Cycle
    | 3 -> Some `Disabled
    | _ -> None

  type query_version_reply = {
    server_major_version : int;
    server_minor_version : int;
  }

  let query_version ~(client_major_version : int) ~(client_minor_version : int)
      () : query_version_reply Lwt.t =
    failwith "not implemented"

  type query_info_reply = {
    state : int;
    saver_window : Xproto.window;
    ms_until_server : int32;
    ms_since_user_input : int32;
    event_mask : int32;
    kind : kind_enum;
  }

  let query_info ~(drawable : Xproto.drawable) () : query_info_reply Lwt.t =
    failwith "not implemented"

  let select_input ~(drawable : Xproto.drawable) ~(event_mask : event_mask) () :
      unit Lwt.t =
    failwith "not implemented"

  let set_attributes ~(drawable : Xproto.drawable) ~(x : int) ~(y : int)
      ~(width : int) ~(height : int) ~(border_width : int)
      ~(class_ : Xproto.window_class_enum) ~(depth : int)
      ~(visual : Xproto.visualid)
      ?(background_pixmap : (Xproto.back_pixmap_enum, Xproto.pixmap) alt option)
      ?(background_pixel : int32 option)
      ?(border_pixmap : (Xproto.pixmap_enum, Xproto.pixmap) alt option)
      ?(border_pixel : int32 option) ?(bit_gravity : Xproto.gravity_enum option)
      ?(win_gravity : Xproto.gravity_enum option)
      ?(backing_store : Xproto.backing_store_enum option)
      ?(backing_planes : int32 option) ?(backing_pixel : int32 option)
      ?(override_redirect : Xproto.bool32 option)
      ?(save_under : Xproto.bool32 option)
      ?(event_mask : Xproto.event_mask_mask option)
      ?(do_not_propogate_mask : Xproto.event_mask_mask option)
      ?(colormap : (Xproto.colormap_enum, Xproto.colormap) alt option)
      ?(cursor : (Xproto.cursor_enum, Xproto.cursor) alt option) () : unit Lwt.t
      =
    failwith "not implemented"

  let unset_attributes ~(drawable : Xproto.drawable) () : unit Lwt.t =
    failwith "not implemented"

  let suspend ~(suspend : int32) () : unit Lwt.t = failwith "not implemented"

  type notify_event = {
    state : state_enum;
    time : Xproto.timestamp;
    root : Xproto.window;
    window : Xproto.window;
    kind : kind_enum;
    forced : bool;
  }
end
[@@warning "-27"]

module Shm = struct
  type seg = xid

  let decode_seg = decode_xid

  type completion_event = {
    drawable : Xproto.drawable;
    minor_event : int;
    major_event : char;
    shmseg : seg;
    offset : int32;
  }

  type bad_seg_error = Xproto.value_error

  type query_version_reply = {
    shared_pixmaps : bool;
    major_version : int;
    minor_version : int;
    uid : int;
    gid : int;
    pixmap_format : int;
  }

  let query_version () : query_version_reply Lwt.t = failwith "not implemented"

  let attach ~(shmseg : seg) ~(shmid : int32) ~(read_only : bool) () :
      unit Lwt.t =
    failwith "not implemented"

  let detach ~(shmseg : seg) () : unit Lwt.t = failwith "not implemented"

  let put_image ~(drawable : Xproto.drawable) ~(gc : Xproto.gcontext)
      ~(total_width : int) ~(total_height : int) ~(src_x : int) ~(src_y : int)
      ~(src_width : int) ~(src_height : int) ~(dst_x : int) ~(dst_y : int)
      ~(depth : int) ~(format : int) ~(send_event : bool) ~(shmseg : seg)
      ~(offset : int32) () : unit Lwt.t =
    failwith "not implemented"

  type get_image_reply = { depth : int; visual : Xproto.visualid; size : int32 }

  let get_image ~(drawable : Xproto.drawable) ~(x : int) ~(y : int)
      ~(width : int) ~(height : int) ~(plane_mask : int32) ~(format : int)
      ~(shmseg : seg) ~(offset : int32) () : get_image_reply Lwt.t =
    failwith "not implemented"

  let create_pixmap ~(pid : Xproto.pixmap) ~(drawable : Xproto.drawable)
      ~(width : int) ~(height : int) ~(depth : int) ~(shmseg : seg)
      ~(offset : int32) () : unit Lwt.t =
    failwith "not implemented"

  let attach_fd ~(shmseg : seg) ~(shm_fd : file_descr) ~(read_only : bool) () :
      unit Lwt.t =
    failwith "not implemented"

  type create_segment_reply = { nfd : int; shm_fd : file_descr }

  let create_segment ~(shmseg : seg) ~(size : int32) ~(read_only : bool) () :
      create_segment_reply Lwt.t =
    failwith "not implemented"
end
[@@warning "-27"]

module Xc_misc = struct
  type get_version_reply = {
    server_major_version : int;
    server_minor_version : int;
  }

  let get_version ~(client_major_version : int) ~(client_minor_version : int) ()
      : get_version_reply Lwt.t =
    failwith "not implemented"

  type get_xid_range_reply = { start_id : int32; count : int32 }

  let get_xid_range () : get_xid_range_reply Lwt.t = failwith "not implemented"

  type get_xid_list_reply = { ids : int32 list }

  let get_xid_list ~(count : int32) () : get_xid_list_reply Lwt.t =
    failwith "not implemented"
end
[@@warning "-27"]

module Xf86dri = struct
  type drm_clip_rect = { x1 : int; y1 : int; x2 : int; x3 : int }

  let decode_drm_clip_rect buf ~at : (drm_clip_rect * int) option =
    let orig = at in
    let* x1, at = decode_int16 buf ~at in
    let* y1, at = decode_int16 buf ~at in
    let* x2, at = decode_int16 buf ~at in
    let* x3, at = decode_int16 buf ~at in
    ignore orig;
    Some ({ x1; y1; x2; x3 }, at)

  type query_version_reply = {
    dri_major_version : int;
    dri_minor_version : int;
    dri_minor_patch : int32;
  }

  let query_version () : query_version_reply Lwt.t = failwith "not implemented"

  type query_direct_rendering_capable_reply = { is_capable : bool }

  let query_direct_rendering_capable ~(screen : int32) () :
      query_direct_rendering_capable_reply Lwt.t =
    failwith "not implemented"

  type open_connection_reply = {
    sarea_handle_low : int32;
    sarea_handle_high : int32;
    bus_id : char list;
  }

  let open_connection ~(screen : int32) () : open_connection_reply Lwt.t =
    failwith "not implemented"

  let close_connection ~(screen : int32) () : unit Lwt.t =
    failwith "not implemented"

  type get_client_driver_name_reply = {
    client_driver_major_version : int32;
    client_driver_minor_version : int32;
    client_driver_patch_version : int32;
    client_driver_name : char list;
  }

  let get_client_driver_name ~(screen : int32) () :
      get_client_driver_name_reply Lwt.t =
    failwith "not implemented"

  type create_context_reply = { hw_context : int32 }

  let create_context ~(screen : int32) ~(visual : int32) ~(context : int32) () :
      create_context_reply Lwt.t =
    failwith "not implemented"

  let destroy_context ~(screen : int32) ~(context : int32) () : unit Lwt.t =
    failwith "not implemented"

  type create_drawable_reply = { hw_drawable_handle : int32 }

  let create_drawable ~(screen : int32) ~(drawable : int32) () :
      create_drawable_reply Lwt.t =
    failwith "not implemented"

  let destroy_drawable ~(screen : int32) ~(drawable : int32) () : unit Lwt.t =
    failwith "not implemented"

  type get_drawable_info_reply = {
    drawable_table_index : int32;
    drawable_table_stamp : int32;
    drawable_origin_x : int;
    drawable_origin_y : int;
    drawable_size_w : int;
    drawable_size_h : int;
    back_x : int;
    back_y : int;
    clip_rects : drm_clip_rect list;
    back_clip_rects : drm_clip_rect list;
  }

  let get_drawable_info ~(screen : int32) ~(drawable : int32) () :
      get_drawable_info_reply Lwt.t =
    failwith "not implemented"

  type get_device_info_reply = {
    framebuffer_handle_low : int32;
    framebuffer_handle_high : int32;
    framebuffer_origin_offset : int32;
    framebuffer_size : int32;
    framebuffer_stride : int32;
    device_private : int32 list;
  }

  let get_device_info ~(screen : int32) () : get_device_info_reply Lwt.t =
    failwith "not implemented"

  type auth_connection_reply = { authenticated : int32 }

  let auth_connection ~(screen : int32) ~(magic : int32) () :
      auth_connection_reply Lwt.t =
    failwith "not implemented"
end
[@@warning "-27"]

module Xf86vidmode = struct
  type syncrange = int32

  let decode_syncrange = decode_int32

  type dotclock = int32

  let decode_dotclock = decode_int32

  type mode_flag_mask =
    [ `Positive_h_sync
    | `Negative_h_sync
    | `Positive_v_sync
    | `Negative_v_sync
    | `Interlace
    | `Composite_sync
    | `Positive_c_sync
    | `Negative_c_sync
    | `H_skew
    | `Broadcast
    | `Pixmux
    | `Double_clock
    | `Half_clock ]
    list

  type clock_flag_mask = [ `Programable ] list

  type permission_mask = [ `Read | `Write ] list

  type mode_info = {
    dotclock : dotclock;
    hdisplay : int;
    hsyncstart : int;
    hsyncend : int;
    htotal : int;
    hskew : int32;
    vdisplay : int;
    vsyncstart : int;
    vsyncend : int;
    vtotal : int;
    flags : mode_flag_mask;
    privsize : int32;
  }

  let decode_mode_info buf ~at : (mode_info * int) option =
    let orig = at in
    let* dotclock, at = decode_dotclock buf ~at in
    let* hdisplay, at = decode_uint16 buf ~at in
    let* hsyncstart, at = decode_uint16 buf ~at in
    let* hsyncend, at = decode_uint16 buf ~at in
    let* htotal, at = decode_uint16 buf ~at in
    let* hskew, at = decode_int32 buf ~at in
    let* vdisplay, at = decode_uint16 buf ~at in
    let* vsyncstart, at = decode_uint16 buf ~at in
    let* vsyncend, at = decode_uint16 buf ~at in
    let* vtotal, at = decode_uint16 buf ~at in
    let at = at + 4 in
    let* flags, at = decode_int32 buf ~at in
    let at = at + 12 in
    let* privsize, at = decode_int32 buf ~at in
    ignore orig;
    Some
      ( {
          dotclock;
          hdisplay;
          hsyncstart;
          hsyncend;
          htotal;
          hskew;
          vdisplay;
          vsyncstart;
          vsyncend;
          vtotal;
          flags;
          privsize;
        },
        at )

  type query_version_reply = { major_version : int; minor_version : int }

  let query_version () : query_version_reply Lwt.t = failwith "not implemented"

  type get_mode_line_reply = {
    dotclock : dotclock;
    hdisplay : int;
    hsyncstart : int;
    hsyncend : int;
    htotal : int;
    hskew : int;
    vdisplay : int;
    vsyncstart : int;
    vsyncend : int;
    vtotal : int;
    flags : mode_flag_mask;
    private_ : int list;
  }

  let get_mode_line ~(screen : int) () : get_mode_line_reply Lwt.t =
    failwith "not implemented"

  let mod_mode_line ~(screen : int32) ~(hdisplay : int) ~(hsyncstart : int)
      ~(hsyncend : int) ~(htotal : int) ~(hskew : int) ~(vdisplay : int)
      ~(vsyncstart : int) ~(vsyncend : int) ~(vtotal : int)
      ~(flags : mode_flag_mask) ~(private_ : int list) () : unit Lwt.t =
    failwith "not implemented"

  let switch_mode ~(screen : int) ~(zoom : int) () : unit Lwt.t =
    failwith "not implemented"

  type get_monitor_reply = {
    hsync : syncrange list;
    vsync : syncrange list;
    vendor : char list;
    alignment_pad : char list;
    model : char list;
  }

  let get_monitor ~(screen : int) () : get_monitor_reply Lwt.t =
    failwith "not implemented"

  let lock_mode_switch ~(screen : int) ~(lock : int) () : unit Lwt.t =
    failwith "not implemented"

  type get_all_mode_lines_reply = { modeinfo : mode_info list }

  let get_all_mode_lines ~(screen : int) () : get_all_mode_lines_reply Lwt.t =
    failwith "not implemented"

  let add_mode_line ~(screen : int32) ~(dotclock : dotclock) ~(hdisplay : int)
      ~(hsyncstart : int) ~(hsyncend : int) ~(htotal : int) ~(hskew : int)
      ~(vdisplay : int) ~(vsyncstart : int) ~(vsyncend : int) ~(vtotal : int)
      ~(flags : mode_flag_mask) ~(after_dotclock : dotclock)
      ~(after_hdisplay : int) ~(after_hsyncstart : int) ~(after_hsyncend : int)
      ~(after_htotal : int) ~(after_hskew : int) ~(after_vdisplay : int)
      ~(after_vsyncstart : int) ~(after_vsyncend : int) ~(after_vtotal : int)
      ~(after_flags : mode_flag_mask) ~(private_ : int list) () : unit Lwt.t =
    failwith "not implemented"

  let delete_mode_line ~(screen : int32) ~(dotclock : dotclock)
      ~(hdisplay : int) ~(hsyncstart : int) ~(hsyncend : int) ~(htotal : int)
      ~(hskew : int) ~(vdisplay : int) ~(vsyncstart : int) ~(vsyncend : int)
      ~(vtotal : int) ~(flags : mode_flag_mask) ~(private_ : int list) () :
      unit Lwt.t =
    failwith "not implemented"

  type validate_mode_line_reply = { status : int32 }

  let validate_mode_line ~(screen : int32) ~(dotclock : dotclock)
      ~(hdisplay : int) ~(hsyncstart : int) ~(hsyncend : int) ~(htotal : int)
      ~(hskew : int) ~(vdisplay : int) ~(vsyncstart : int) ~(vsyncend : int)
      ~(vtotal : int) ~(flags : mode_flag_mask) ~(private_ : int list) () :
      validate_mode_line_reply Lwt.t =
    failwith "not implemented"

  let switch_to_mode ~(screen : int32) ~(dotclock : dotclock) ~(hdisplay : int)
      ~(hsyncstart : int) ~(hsyncend : int) ~(htotal : int) ~(hskew : int)
      ~(vdisplay : int) ~(vsyncstart : int) ~(vsyncend : int) ~(vtotal : int)
      ~(flags : mode_flag_mask) ~(private_ : int list) () : unit Lwt.t =
    failwith "not implemented"

  type get_view_port_reply = { x : int32; y : int32 }

  let get_view_port ~(screen : int) () : get_view_port_reply Lwt.t =
    failwith "not implemented"

  let set_view_port ~(screen : int) ~(x : int32) ~(y : int32) () : unit Lwt.t =
    failwith "not implemented"

  type get_dot_clocks_reply = {
    flags : clock_flag_mask;
    clocks : int32;
    maxclocks : int32;
    clock : int32 list;
  }

  let get_dot_clocks ~(screen : int) () : get_dot_clocks_reply Lwt.t =
    failwith "not implemented"

  let set_client_version ~(major : int) ~(minor : int) () : unit Lwt.t =
    failwith "not implemented"

  let set_gamma ~(screen : int) ~(red : int32) ~(green : int32) ~(blue : int32)
      () : unit Lwt.t =
    failwith "not implemented"

  type get_gamma_reply = { red : int32; green : int32; blue : int32 }

  let get_gamma ~(screen : int) () : get_gamma_reply Lwt.t =
    failwith "not implemented"

  type get_gamma_ramp_reply = {
    size : int;
    red : int list;
    green : int list;
    blue : int list;
  }

  let get_gamma_ramp ~(screen : int) ~(size : int) () :
      get_gamma_ramp_reply Lwt.t =
    failwith "not implemented"

  let set_gamma_ramp ~(screen : int) ~(size : int) ~(red : int list)
      ~(green : int list) ~(blue : int list) () : unit Lwt.t =
    failwith "not implemented"

  type get_gamma_ramp_size_reply = { size : int }

  let get_gamma_ramp_size ~(screen : int) () : get_gamma_ramp_size_reply Lwt.t =
    failwith "not implemented"

  type get_permissions_reply = { permissions : permission_mask }

  let get_permissions ~(screen : int) () : get_permissions_reply Lwt.t =
    failwith "not implemented"

  type bad_clock_error = unit

  type bad_h_timings_error = unit

  type bad_v_timings_error = unit

  type mode_unsuitable_error = unit

  type extension_disabled_error = unit

  type client_not_local_error = unit

  type zoom_locked_error = unit
end
[@@warning "-27"]

module Xinerama = struct
  type screen_info = { x_org : int; y_org : int; width : int; height : int }

  let decode_screen_info buf ~at : (screen_info * int) option =
    let orig = at in
    let* x_org, at = decode_int16 buf ~at in
    let* y_org, at = decode_int16 buf ~at in
    let* width, at = decode_uint16 buf ~at in
    let* height, at = decode_uint16 buf ~at in
    ignore orig;
    Some ({ x_org; y_org; width; height }, at)

  type query_version_reply = { major : int; minor : int }

  let query_version ~(major : int) ~(minor : int) () : query_version_reply Lwt.t
      =
    failwith "not implemented"

  type get_state_reply = { state : char; window : Xproto.window }

  let get_state ~(window : Xproto.window) () : get_state_reply Lwt.t =
    failwith "not implemented"

  type get_screen_count_reply = { screen_count : char; window : Xproto.window }

  let get_screen_count ~(window : Xproto.window) () :
      get_screen_count_reply Lwt.t =
    failwith "not implemented"

  type get_screen_size_reply = {
    width : int32;
    height : int32;
    window : Xproto.window;
    screen : int32;
  }

  let get_screen_size ~(window : Xproto.window) ~(screen : int32) () :
      get_screen_size_reply Lwt.t =
    failwith "not implemented"

  type is_active_reply = { state : int32 }

  let is_active () : is_active_reply Lwt.t = failwith "not implemented"

  type query_screens_reply = { screen_info : screen_info list }

  let query_screens () : query_screens_reply Lwt.t = failwith "not implemented"
end
[@@warning "-27"]

module Xinput = struct
  type event_class = int32

  let decode_event_class = decode_int32

  type key_code = int

  let decode_key_code = decode_uint8

  type device_id = int

  let decode_device_id = decode_uint16

  type fp1616 = int32

  let decode_fp1616 = decode_int32

  type fp3232 = { integral : int32; frac : int32 }

  let decode_fp3232 buf ~at : (fp3232 * int) option =
    let orig = at in
    let* integral, at = decode_int32 buf ~at in
    let* frac, at = decode_int32 buf ~at in
    ignore orig;
    Some ({ integral; frac }, at)

  type get_extension_version_reply = {
    xi_reply_type : int;
    server_major : int;
    server_minor : int;
    present : bool;
  }

  let get_extension_version ~(name : char list) () :
      get_extension_version_reply Lwt.t =
    failwith "not implemented"

  type device_use_enum =
    [ `Is_x_pointer
    | `Is_x_keyboard
    | `Is_x_extension_device
    | `Is_x_extension_keyboard
    | `Is_x_extension_pointer ]

  let device_use_enum_of_int : int -> device_use_enum option = function
    | 0 -> Some `Is_x_pointer
    | 1 -> Some `Is_x_keyboard
    | 2 -> Some `Is_x_extension_device
    | 3 -> Some `Is_x_extension_keyboard
    | 4 -> Some `Is_x_extension_pointer
    | _ -> None

  type input_class_enum =
    [ `Key | `Button | `Valuator | `Feedback | `Proximity | `Focus | `Other ]

  let input_class_enum_of_int : int -> input_class_enum option = function
    | 0 -> Some `Key
    | 1 -> Some `Button
    | 2 -> Some `Valuator
    | 3 -> Some `Feedback
    | 4 -> Some `Proximity
    | 5 -> Some `Focus
    | 6 -> Some `Other
    | _ -> None

  type valuator_mode_enum = [ `Relative | `Absolute ]

  let valuator_mode_enum_of_int : int -> valuator_mode_enum option = function
    | 0 -> Some `Relative
    | 1 -> Some `Absolute
    | _ -> None

  type device_info = {
    device_type : Xproto.atom;
    device_id : int;
    num_class_info : int;
    device_use : device_use_enum;
  }

  let decode_device_info buf ~at : (device_info * int) option =
    let orig = at in
    let* device_type, at = Xproto.decode_atom buf ~at in
    let* device_id, at = decode_uint8 buf ~at in
    let* num_class_info, at = decode_uint8 buf ~at in
    let* device_use, at =
      decode_enum decode_uint8 (fun x -> x) device_use_enum_of_int buf ~at
    in
    let at = at + 1 in
    ignore orig;
    Some ({ device_type; device_id; num_class_info; device_use }, at)

  type key_info = {
    class_id : input_class_enum;
    len : int;
    min_keycode : key_code;
    max_keycode : key_code;
    num_keys : int;
  }

  let decode_key_info buf ~at : (key_info * int) option =
    let orig = at in
    let* class_id, at =
      decode_enum decode_uint8 (fun x -> x) input_class_enum_of_int buf ~at
    in
    let* len, at = decode_uint8 buf ~at in
    let* min_keycode, at = decode_key_code buf ~at in
    let* max_keycode, at = decode_key_code buf ~at in
    let* num_keys, at = decode_uint16 buf ~at in
    let at = at + 2 in
    ignore orig;
    Some ({ class_id; len; min_keycode; max_keycode; num_keys }, at)

  type button_info = {
    class_id : input_class_enum;
    len : int;
    num_buttons : int;
  }

  let decode_button_info buf ~at : (button_info * int) option =
    let orig = at in
    let* class_id, at =
      decode_enum decode_uint8 (fun x -> x) input_class_enum_of_int buf ~at
    in
    let* len, at = decode_uint8 buf ~at in
    let* num_buttons, at = decode_uint16 buf ~at in
    ignore orig;
    Some ({ class_id; len; num_buttons }, at)

  type axis_info = { resolution : int32; minimum : int32; maximum : int32 }

  let decode_axis_info buf ~at : (axis_info * int) option =
    let orig = at in
    let* resolution, at = decode_int32 buf ~at in
    let* minimum, at = decode_int32 buf ~at in
    let* maximum, at = decode_int32 buf ~at in
    ignore orig;
    Some ({ resolution; minimum; maximum }, at)

  type valuator_info = {
    class_id : input_class_enum;
    len : int;
    mode : valuator_mode_enum;
    motion_size : int32;
    axes : axis_info list;
  }

  let decode_valuator_info buf ~at : (valuator_info * int) option =
    let orig = at in
    let* class_id, at =
      decode_enum decode_uint8 (fun x -> x) input_class_enum_of_int buf ~at
    in
    let* len, at = decode_uint8 buf ~at in
    let* axes_len, at = decode_uint8 buf ~at in
    let axes_len = axes_len in
    let* mode, at =
      decode_enum decode_uint8 (fun x -> x) valuator_mode_enum_of_int buf ~at
    in
    let* motion_size, at = decode_int32 buf ~at in
    let* axes, at = decode_list decode_axis_info axes_len buf ~at in
    ignore orig;
    Some ({ class_id; len; mode; motion_size; axes }, at)

  type input_class_variant =
    | Key of { min_keycode : key_code; max_keycode : key_code; num_keys : int }
    | Button of { num_buttons : int }
    | Valuator of {
        mode : valuator_mode_enum;
        motion_size : int32;
        axes : axis_info list;
      }

  type input_info = { len : int; info : input_class_variant }

  let decode_input_info buf ~at : (input_info * int) option =
    let orig = at in
    let* len, at = decode_uint8 buf ~at in
    ignore orig;
    Some ({ len; info }, at)

  type device_name = { string : char list }

  let decode_device_name buf ~at : (device_name * int) option =
    let orig = at in
    let* len, at = decode_uint8 buf ~at in
    let len = len in
    let* string, at = decode_list decode_char len buf ~at in
    ignore orig;
    Some ({ string }, at)

  type list_input_devices_reply = {
    xi_reply_type : int;
    devices : device_info list;
    infos : input_info list;
    names : Xproto.str list;
  }

  let list_input_devices () : list_input_devices_reply Lwt.t =
    failwith "not implemented"

  type event_type_base = int

  let decode_event_type_base = decode_uint8

  type input_class_info = {
    class_id : input_class_enum;
    event_type_base : event_type_base;
  }

  let decode_input_class_info buf ~at : (input_class_info * int) option =
    let orig = at in
    let* class_id, at =
      decode_enum decode_uint8 (fun x -> x) input_class_enum_of_int buf ~at
    in
    let* event_type_base, at = decode_event_type_base buf ~at in
    ignore orig;
    Some ({ class_id; event_type_base }, at)

  type open_device_reply = {
    xi_reply_type : int;
    class_info : input_class_info list;
  }

  let open_device ~(device_id : int) () : open_device_reply Lwt.t =
    failwith "not implemented"

  let close_device ~(device_id : int) () : unit Lwt.t =
    failwith "not implemented"

  type set_device_mode_reply = {
    xi_reply_type : int;
    status : Xproto.grab_status_enum;
  }

  let set_device_mode ~(device_id : int) ~(mode : valuator_mode_enum) () :
      set_device_mode_reply Lwt.t =
    failwith "not implemented"

  let select_extension_event ~(window : Xproto.window)
      ~(classes : event_class list) () : unit Lwt.t =
    failwith "not implemented"

  type get_selected_extension_events_reply = {
    xi_reply_type : int;
    this_classes : event_class list;
    all_classes : event_class list;
  }

  let get_selected_extension_events ~(window : Xproto.window) () :
      get_selected_extension_events_reply Lwt.t =
    failwith "not implemented"

  type propagate_mode_enum = [ `Add_to_list | `Delete_from_list ]

  let propagate_mode_enum_of_int : int -> propagate_mode_enum option = function
    | 0 -> Some `Add_to_list
    | 1 -> Some `Delete_from_list
    | _ -> None

  let change_device_dont_propagate_list ~(window : Xproto.window)
      ~(mode : propagate_mode_enum) ~(classes : event_class list) () :
      unit Lwt.t =
    failwith "not implemented"

  type get_device_dont_propagate_list_reply = {
    xi_reply_type : int;
    classes : event_class list;
  }

  let get_device_dont_propagate_list ~(window : Xproto.window) () :
      get_device_dont_propagate_list_reply Lwt.t =
    failwith "not implemented"

  type device_time_coord = { time : Xproto.timestamp; axisvalues : int32 list }

  let decode_device_time_coord buf ~at : (device_time_coord * int) option =
    let orig = at in
    let* time, at = Xproto.decode_timestamp buf ~at in
    let* axisvalues, at = decode_list decode_int32 num_axes buf ~at in
    ignore orig;
    Some ({ time; axisvalues }, at)

  type get_device_motion_events_reply = {
    xi_reply_type : int;
    num_axes : int;
    device_mode : valuator_mode_enum;
    events : device_time_coord list;
  }

  let get_device_motion_events ~(start : Xproto.timestamp)
      ~(stop : (Xproto.time_enum, Xproto.timestamp) alt) ~(device_id : int) () :
      get_device_motion_events_reply Lwt.t =
    failwith "not implemented"

  type change_keyboard_device_reply = {
    xi_reply_type : int;
    status : Xproto.grab_status_enum;
  }

  let change_keyboard_device ~(device_id : int) () :
      change_keyboard_device_reply Lwt.t =
    failwith "not implemented"

  type change_pointer_device_reply = {
    xi_reply_type : int;
    status : Xproto.grab_status_enum;
  }

  let change_pointer_device ~(x_axis : int) ~(y_axis : int) ~(device_id : int)
      () : change_pointer_device_reply Lwt.t =
    failwith "not implemented"

  type grab_device_reply = {
    xi_reply_type : int;
    status : Xproto.grab_status_enum;
  }

  let grab_device ~(grab_window : Xproto.window)
      ~(time : (Xproto.time_enum, Xproto.timestamp) alt)
      ~(this_device_mode : Xproto.grab_mode_enum)
      ~(other_device_mode : Xproto.grab_mode_enum) ~(owner_events : bool)
      ~(device_id : int) ~(classes : event_class list) () :
      grab_device_reply Lwt.t =
    failwith "not implemented"

  let ungrab_device ~(time : (Xproto.time_enum, Xproto.timestamp) alt)
      ~(device_id : int) () : unit Lwt.t =
    failwith "not implemented"

  type modifier_device_enum = [ `Use_x_keyboard ]

  let modifier_device_enum_of_int : int -> modifier_device_enum option =
    function
    | 255 -> Some `Use_x_keyboard
    | _ -> None

  let grab_device_key ~(grab_window : Xproto.window)
      ~(modifiers : Xproto.mod_mask_mask)
      ~(modifier_device : (modifier_device_enum, int) alt)
      ~(grabbed_device : int) ~(key : (Xproto.grab_enum, int) alt)
      ~(this_device_mode : Xproto.grab_mode_enum)
      ~(other_device_mode : Xproto.grab_mode_enum) ~(owner_events : bool)
      ~(classes : event_class list) () : unit Lwt.t =
    failwith "not implemented"

  let ungrab_device_key ~(grab_window : Xproto.window)
      ~(modifiers : Xproto.mod_mask_mask)
      ~(modifier_device : (modifier_device_enum, int) alt)
      ~(key : (Xproto.grab_enum, int) alt) ~(grabbed_device : int) () :
      unit Lwt.t =
    failwith "not implemented"

  let grab_device_button ~(grab_window : Xproto.window) ~(grabbed_device : int)
      ~(modifier_device : (modifier_device_enum, int) alt)
      ~(modifiers : Xproto.mod_mask_mask)
      ~(this_device_mode : Xproto.grab_mode_enum)
      ~(other_device_mode : Xproto.grab_mode_enum)
      ~(button : (Xproto.grab_enum, int) alt) ~(owner_events : bool)
      ~(classes : event_class list) () : unit Lwt.t =
    failwith "not implemented"

  let ungrab_device_button ~(grab_window : Xproto.window)
      ~(modifiers : Xproto.mod_mask_mask)
      ~(modifier_device : (modifier_device_enum, int) alt)
      ~(button : (Xproto.grab_enum, int) alt) ~(grabbed_device : int) () :
      unit Lwt.t =
    failwith "not implemented"

  type device_input_mode_enum =
    [ `Async_this_device
    | `Sync_this_device
    | `Replay_this_device
    | `Async_other_devices
    | `Async_all
    | `Sync_all ]

  let device_input_mode_enum_of_int : int -> device_input_mode_enum option =
    function
    | 0 -> Some `Async_this_device
    | 1 -> Some `Sync_this_device
    | 2 -> Some `Replay_this_device
    | 3 -> Some `Async_other_devices
    | 4 -> Some `Async_all
    | 5 -> Some `Sync_all
    | _ -> None

  let allow_device_events ~(time : (Xproto.time_enum, Xproto.timestamp) alt)
      ~(mode : device_input_mode_enum) ~(device_id : int) () : unit Lwt.t =
    failwith "not implemented"

  type get_device_focus_reply = {
    xi_reply_type : int;
    focus : (Xproto.input_focus_enum, Xproto.window) alt;
    time : Xproto.timestamp;
    revert_to : Xproto.input_focus_enum;
  }

  let get_device_focus ~(device_id : int) () : get_device_focus_reply Lwt.t =
    failwith "not implemented"

  let set_device_focus ~(focus : (Xproto.input_focus_enum, Xproto.window) alt)
      ~(time : (Xproto.time_enum, Xproto.timestamp) alt)
      ~(revert_to : Xproto.input_focus_enum) ~(device_id : int) () : unit Lwt.t
      =
    failwith "not implemented"

  type feedback_class_enum =
    [ `Keyboard | `Pointer | `String | `Integer | `Led | `Bell ]

  let feedback_class_enum_of_int : int -> feedback_class_enum option = function
    | 0 -> Some `Keyboard
    | 1 -> Some `Pointer
    | 2 -> Some `String
    | 3 -> Some `Integer
    | 4 -> Some `Led
    | 5 -> Some `Bell
    | _ -> None

  type kbd_feedback_state = {
    class_id : feedback_class_enum;
    feedback_id : int;
    len : int;
    pitch : int;
    duration : int;
    led_mask : int32;
    led_values : int32;
    global_auto_repeat : bool;
    click : int;
    percent : int;
    auto_repeats : int list;
  }

  let decode_kbd_feedback_state buf ~at : (kbd_feedback_state * int) option =
    let orig = at in
    let* class_id, at =
      decode_enum decode_uint8 (fun x -> x) feedback_class_enum_of_int buf ~at
    in
    let* feedback_id, at = decode_uint8 buf ~at in
    let* len, at = decode_uint16 buf ~at in
    let* pitch, at = decode_uint16 buf ~at in
    let* duration, at = decode_uint16 buf ~at in
    let* led_mask, at = decode_int32 buf ~at in
    let* led_values, at = decode_int32 buf ~at in
    let* global_auto_repeat, at = decode_bool buf ~at in
    let* click, at = decode_uint8 buf ~at in
    let* percent, at = decode_uint8 buf ~at in
    let at = at + 1 in
    let* auto_repeats, at = decode_list decode_uint8 32 buf ~at in
    ignore orig;
    Some
      ( {
          class_id;
          feedback_id;
          len;
          pitch;
          duration;
          led_mask;
          led_values;
          global_auto_repeat;
          click;
          percent;
          auto_repeats;
        },
        at )

  type ptr_feedback_state = {
    class_id : feedback_class_enum;
    feedback_id : int;
    len : int;
    accel_num : int;
    accel_denom : int;
    threshold : int;
  }

  let decode_ptr_feedback_state buf ~at : (ptr_feedback_state * int) option =
    let orig = at in
    let* class_id, at =
      decode_enum decode_uint8 (fun x -> x) feedback_class_enum_of_int buf ~at
    in
    let* feedback_id, at = decode_uint8 buf ~at in
    let* len, at = decode_uint16 buf ~at in
    let at = at + 2 in
    let* accel_num, at = decode_uint16 buf ~at in
    let* accel_denom, at = decode_uint16 buf ~at in
    let* threshold, at = decode_uint16 buf ~at in
    ignore orig;
    Some ({ class_id; feedback_id; len; accel_num; accel_denom; threshold }, at)

  type integer_feedback_state = {
    class_id : feedback_class_enum;
    feedback_id : int;
    len : int;
    resolution : int32;
    min_value : int32;
    max_value : int32;
  }

  let decode_integer_feedback_state buf ~at :
      (integer_feedback_state * int) option =
    let orig = at in
    let* class_id, at =
      decode_enum decode_uint8 (fun x -> x) feedback_class_enum_of_int buf ~at
    in
    let* feedback_id, at = decode_uint8 buf ~at in
    let* len, at = decode_uint16 buf ~at in
    let* resolution, at = decode_int32 buf ~at in
    let* min_value, at = decode_int32 buf ~at in
    let* max_value, at = decode_int32 buf ~at in
    ignore orig;
    Some ({ class_id; feedback_id; len; resolution; min_value; max_value }, at)

  type string_feedback_state = {
    class_id : feedback_class_enum;
    feedback_id : int;
    len : int;
    max_symbols : int;
    keysyms : Xproto.keysym list;
  }

  let decode_string_feedback_state buf ~at :
      (string_feedback_state * int) option =
    let orig = at in
    let* class_id, at =
      decode_enum decode_uint8 (fun x -> x) feedback_class_enum_of_int buf ~at
    in
    let* feedback_id, at = decode_uint8 buf ~at in
    let* len, at = decode_uint16 buf ~at in
    let* max_symbols, at = decode_uint16 buf ~at in
    let* num_keysyms, at = decode_uint16 buf ~at in
    let num_keysyms = num_keysyms in
    let* keysyms, at = decode_list Xproto.decode_keysym num_keysyms buf ~at in
    ignore orig;
    Some ({ class_id; feedback_id; len; max_symbols; keysyms }, at)

  type bell_feedback_state = {
    class_id : feedback_class_enum;
    feedback_id : int;
    len : int;
    percent : int;
    pitch : int;
    duration : int;
  }

  let decode_bell_feedback_state buf ~at : (bell_feedback_state * int) option =
    let orig = at in
    let* class_id, at =
      decode_enum decode_uint8 (fun x -> x) feedback_class_enum_of_int buf ~at
    in
    let* feedback_id, at = decode_uint8 buf ~at in
    let* len, at = decode_uint16 buf ~at in
    let* percent, at = decode_uint8 buf ~at in
    let at = at + 3 in
    let* pitch, at = decode_uint16 buf ~at in
    let* duration, at = decode_uint16 buf ~at in
    ignore orig;
    Some ({ class_id; feedback_id; len; percent; pitch; duration }, at)

  type led_feedback_state = {
    class_id : feedback_class_enum;
    feedback_id : int;
    len : int;
    led_mask : int32;
    led_values : int32;
  }

  let decode_led_feedback_state buf ~at : (led_feedback_state * int) option =
    let orig = at in
    let* class_id, at =
      decode_enum decode_uint8 (fun x -> x) feedback_class_enum_of_int buf ~at
    in
    let* feedback_id, at = decode_uint8 buf ~at in
    let* len, at = decode_uint16 buf ~at in
    let* led_mask, at = decode_int32 buf ~at in
    let* led_values, at = decode_int32 buf ~at in
    ignore orig;
    Some ({ class_id; feedback_id; len; led_mask; led_values }, at)

  type feedback_class_variant =
    | Keyboard of {
        pitch : int;
        duration : int;
        led_mask : int32;
        led_values : int32;
        global_auto_repeat : bool;
        click : int;
        percent : int;
        auto_repeats : int list;
      }
    | Pointer of { accel_num : int; accel_denom : int; threshold : int }
    | String of { max_symbols : int; keysyms : Xproto.keysym list }
    | Integer of { resolution : int32; min_value : int32; max_value : int32 }
    | Led of { led_mask : int32; led_values : int32 }
    | Bell of { percent : int; pitch : int; duration : int }

  type feedback_state = {
    feedback_id : int;
    len : int;
    data : feedback_class_variant;
  }

  let decode_feedback_state buf ~at : (feedback_state * int) option =
    let orig = at in
    let* feedback_id, at = decode_uint8 buf ~at in
    let* len, at = decode_uint16 buf ~at in
    ignore orig;
    Some ({ feedback_id; len; data }, at)

  type get_feedback_control_reply = {
    xi_reply_type : int;
    feedbacks : feedback_state list;
  }

  let get_feedback_control ~(device_id : int) () :
      get_feedback_control_reply Lwt.t =
    failwith "not implemented"

  type kbd_feedback_ctl = {
    class_id : feedback_class_enum;
    feedback_id : int;
    len : int;
    key : key_code;
    auto_repeat_mode : int;
    key_click_percent : int;
    bell_percent : int;
    bell_pitch : int;
    bell_duration : int;
    led_mask : int32;
    led_values : int32;
  }

  let decode_kbd_feedback_ctl buf ~at : (kbd_feedback_ctl * int) option =
    let orig = at in
    let* class_id, at =
      decode_enum decode_uint8 (fun x -> x) feedback_class_enum_of_int buf ~at
    in
    let* feedback_id, at = decode_uint8 buf ~at in
    let* len, at = decode_uint16 buf ~at in
    let* key, at = decode_key_code buf ~at in
    let* auto_repeat_mode, at = decode_uint8 buf ~at in
    let* key_click_percent, at = decode_int8 buf ~at in
    let* bell_percent, at = decode_int8 buf ~at in
    let* bell_pitch, at = decode_int16 buf ~at in
    let* bell_duration, at = decode_int16 buf ~at in
    let* led_mask, at = decode_int32 buf ~at in
    let* led_values, at = decode_int32 buf ~at in
    ignore orig;
    Some
      ( {
          class_id;
          feedback_id;
          len;
          key;
          auto_repeat_mode;
          key_click_percent;
          bell_percent;
          bell_pitch;
          bell_duration;
          led_mask;
          led_values;
        },
        at )

  type ptr_feedback_ctl = {
    class_id : feedback_class_enum;
    feedback_id : int;
    len : int;
    num : int;
    denom : int;
    threshold : int;
  }

  let decode_ptr_feedback_ctl buf ~at : (ptr_feedback_ctl * int) option =
    let orig = at in
    let* class_id, at =
      decode_enum decode_uint8 (fun x -> x) feedback_class_enum_of_int buf ~at
    in
    let* feedback_id, at = decode_uint8 buf ~at in
    let* len, at = decode_uint16 buf ~at in
    let at = at + 2 in
    let* num, at = decode_int16 buf ~at in
    let* denom, at = decode_int16 buf ~at in
    let* threshold, at = decode_int16 buf ~at in
    ignore orig;
    Some ({ class_id; feedback_id; len; num; denom; threshold }, at)

  type integer_feedback_ctl = {
    class_id : feedback_class_enum;
    feedback_id : int;
    len : int;
    int_to_display : int32;
  }

  let decode_integer_feedback_ctl buf ~at : (integer_feedback_ctl * int) option
      =
    let orig = at in
    let* class_id, at =
      decode_enum decode_uint8 (fun x -> x) feedback_class_enum_of_int buf ~at
    in
    let* feedback_id, at = decode_uint8 buf ~at in
    let* len, at = decode_uint16 buf ~at in
    let* int_to_display, at = decode_int32 buf ~at in
    ignore orig;
    Some ({ class_id; feedback_id; len; int_to_display }, at)

  type string_feedback_ctl = {
    class_id : feedback_class_enum;
    feedback_id : int;
    len : int;
    keysyms : Xproto.keysym list;
  }

  let decode_string_feedback_ctl buf ~at : (string_feedback_ctl * int) option =
    let orig = at in
    let* class_id, at =
      decode_enum decode_uint8 (fun x -> x) feedback_class_enum_of_int buf ~at
    in
    let* feedback_id, at = decode_uint8 buf ~at in
    let* len, at = decode_uint16 buf ~at in
    let at = at + 2 in
    let* num_keysyms, at = decode_uint16 buf ~at in
    let num_keysyms = num_keysyms in
    let* keysyms, at = decode_list Xproto.decode_keysym num_keysyms buf ~at in
    ignore orig;
    Some ({ class_id; feedback_id; len; keysyms }, at)

  type bell_feedback_ctl = {
    class_id : feedback_class_enum;
    feedback_id : int;
    len : int;
    percent : int;
    pitch : int;
    duration : int;
  }

  let decode_bell_feedback_ctl buf ~at : (bell_feedback_ctl * int) option =
    let orig = at in
    let* class_id, at =
      decode_enum decode_uint8 (fun x -> x) feedback_class_enum_of_int buf ~at
    in
    let* feedback_id, at = decode_uint8 buf ~at in
    let* len, at = decode_uint16 buf ~at in
    let* percent, at = decode_int8 buf ~at in
    let at = at + 3 in
    let* pitch, at = decode_int16 buf ~at in
    let* duration, at = decode_int16 buf ~at in
    ignore orig;
    Some ({ class_id; feedback_id; len; percent; pitch; duration }, at)

  type led_feedback_ctl = {
    class_id : feedback_class_enum;
    feedback_id : int;
    len : int;
    led_mask : int32;
    led_values : int32;
  }

  let decode_led_feedback_ctl buf ~at : (led_feedback_ctl * int) option =
    let orig = at in
    let* class_id, at =
      decode_enum decode_uint8 (fun x -> x) feedback_class_enum_of_int buf ~at
    in
    let* feedback_id, at = decode_uint8 buf ~at in
    let* len, at = decode_uint16 buf ~at in
    let* led_mask, at = decode_int32 buf ~at in
    let* led_values, at = decode_int32 buf ~at in
    ignore orig;
    Some ({ class_id; feedback_id; len; led_mask; led_values }, at)

  type feedback_ctl = {
    feedback_id : int;
    len : int;
    data : feedback_class_variant;
  }

  let decode_feedback_ctl buf ~at : (feedback_ctl * int) option =
    let orig = at in
    let* feedback_id, at = decode_uint8 buf ~at in
    let* len, at = decode_uint16 buf ~at in
    ignore orig;
    Some ({ feedback_id; len; data }, at)

  type change_feedback_control_mask_mask =
    [ `Key_click_percent
    | `Percent
    | `Pitch
    | `Duration
    | `Led
    | `Led_mode
    | `Key
    | `Auto_repeat_mode
    | `String
    | `Integer
    | `Accel_num
    | `Accel_denom
    | `Threshold ]
    list

  let change_feedback_control ~(mask : change_feedback_control_mask_mask)
      ~(device_id : int) ~(feedback_id : int) ~(feedback : feedback_ctl) () :
      unit Lwt.t =
    failwith "not implemented"

  type get_device_key_mapping_reply = {
    xi_reply_type : int;
    keysyms_per_keycode : int;
    keysyms : Xproto.keysym list;
  }

  let get_device_key_mapping ~(device_id : int) ~(first_keycode : key_code)
      ~(count : int) () : get_device_key_mapping_reply Lwt.t =
    failwith "not implemented"

  let change_device_key_mapping ~(device_id : int) ~(first_keycode : key_code)
      ~(keysyms_per_keycode : int) ~(keycode_count : int)
      ~(keysyms : Xproto.keysym list) () : unit Lwt.t =
    failwith "not implemented"

  type get_device_modifier_mapping_reply = {
    xi_reply_type : int;
    keycodes_per_modifier : int;
    keymaps : int list;
  }

  let get_device_modifier_mapping ~(device_id : int) () :
      get_device_modifier_mapping_reply Lwt.t =
    failwith "not implemented"

  type set_device_modifier_mapping_reply = {
    xi_reply_type : int;
    status : Xproto.mapping_status_enum;
  }

  let set_device_modifier_mapping ~(device_id : int)
      ~(keycodes_per_modifier : int) ~(keymaps : int list) () :
      set_device_modifier_mapping_reply Lwt.t =
    failwith "not implemented"

  type get_device_button_mapping_reply = { xi_reply_type : int; map : int list }

  let get_device_button_mapping ~(device_id : int) () :
      get_device_button_mapping_reply Lwt.t =
    failwith "not implemented"

  type set_device_button_mapping_reply = {
    xi_reply_type : int;
    status : Xproto.mapping_status_enum;
  }

  let set_device_button_mapping ~(device_id : int) ~(map : int list) () :
      set_device_button_mapping_reply Lwt.t =
    failwith "not implemented"

  type key_state = {
    class_id : input_class_enum;
    len : int;
    num_keys : int;
    keys : int list;
  }

  let decode_key_state buf ~at : (key_state * int) option =
    let orig = at in
    let* class_id, at =
      decode_enum decode_uint8 (fun x -> x) input_class_enum_of_int buf ~at
    in
    let* len, at = decode_uint8 buf ~at in
    let* num_keys, at = decode_uint8 buf ~at in
    let at = at + 1 in
    let* keys, at = decode_list decode_uint8 32 buf ~at in
    ignore orig;
    Some ({ class_id; len; num_keys; keys }, at)

  type button_state = {
    class_id : input_class_enum;
    len : int;
    num_buttons : int;
    buttons : int list;
  }

  let decode_button_state buf ~at : (button_state * int) option =
    let orig = at in
    let* class_id, at =
      decode_enum decode_uint8 (fun x -> x) input_class_enum_of_int buf ~at
    in
    let* len, at = decode_uint8 buf ~at in
    let* num_buttons, at = decode_uint8 buf ~at in
    let at = at + 1 in
    let* buttons, at = decode_list decode_uint8 32 buf ~at in
    ignore orig;
    Some ({ class_id; len; num_buttons; buttons }, at)

  type valuator_state_mode_mask_mask =
    [ `Device_mode_absolute | `Out_of_proximity ] list

  type valuator_state = {
    class_id : input_class_enum;
    len : int;
    mode : valuator_state_mode_mask_mask;
    valuators : int32 list;
  }

  let decode_valuator_state buf ~at : (valuator_state * int) option =
    let orig = at in
    let* class_id, at =
      decode_enum decode_uint8 (fun x -> x) input_class_enum_of_int buf ~at
    in
    let* len, at = decode_uint8 buf ~at in
    let* num_valuators, at = decode_uint8 buf ~at in
    let num_valuators = num_valuators in
    let* mode, at = decode_uint8 buf ~at in
    let* valuators, at = decode_list decode_int32 num_valuators buf ~at in
    ignore orig;
    Some ({ class_id; len; mode; valuators }, at)

  type input_state = { len : int; data : input_class_variant }

  let decode_input_state buf ~at : (input_state * int) option =
    let orig = at in
    let* len, at = decode_uint8 buf ~at in
    ignore orig;
    Some ({ len; data }, at)

  type query_device_state_reply = {
    xi_reply_type : int;
    classes : input_state list;
  }

  let query_device_state ~(device_id : int) () : query_device_state_reply Lwt.t
      =
    failwith "not implemented"

  let device_bell ~(device_id : int) ~(feedback_id : int)
      ~(feedback_class : int) ~(percent : int) () : unit Lwt.t =
    failwith "not implemented"

  type set_device_valuators_reply = {
    xi_reply_type : int;
    status : Xproto.grab_status_enum;
  }

  let set_device_valuators ~(device_id : int) ~(first_valuator : int)
      ~(valuators : int32 list) () : set_device_valuators_reply Lwt.t =
    failwith "not implemented"

  type device_control_enum =
    [ `Resolution | `Abs_calib | `Core | `Enable | `Abs_area ]

  let device_control_enum_of_int : int -> device_control_enum option = function
    | 1 -> Some `Resolution
    | 2 -> Some `Abs_calib
    | 3 -> Some `Core
    | 4 -> Some `Enable
    | 5 -> Some `Abs_area
    | _ -> None

  type device_resolution_state = {
    control_id : device_control_enum;
    len : int;
    resolution_values : int32 list;
    resolution_min : int32 list;
    resolution_max : int32 list;
  }

  let decode_device_resolution_state buf ~at :
      (device_resolution_state * int) option =
    let orig = at in
    let* control_id, at =
      decode_enum decode_uint16 (fun x -> x) device_control_enum_of_int buf ~at
    in
    let* len, at = decode_uint16 buf ~at in
    let* num_valuators, at = decode_int32 buf ~at in
    let num_valuators = Int32.to_int num_valuators in
    let num_valuators = num_valuators in
    let* resolution_values, at =
      decode_list decode_int32 num_valuators buf ~at
    in
    let* resolution_min, at = decode_list decode_int32 num_valuators buf ~at in
    let* resolution_max, at = decode_list decode_int32 num_valuators buf ~at in
    ignore orig;
    Some
      ( { control_id; len; resolution_values; resolution_min; resolution_max },
        at )

  type device_abs_calib_state = {
    control_id : device_control_enum;
    len : int;
    min_x : int32;
    max_x : int32;
    min_y : int32;
    max_y : int32;
    flip_x : int32;
    flip_y : int32;
    rotation : int32;
    button_threshold : int32;
  }

  let decode_device_abs_calib_state buf ~at :
      (device_abs_calib_state * int) option =
    let orig = at in
    let* control_id, at =
      decode_enum decode_uint16 (fun x -> x) device_control_enum_of_int buf ~at
    in
    let* len, at = decode_uint16 buf ~at in
    let* min_x, at = decode_int32 buf ~at in
    let* max_x, at = decode_int32 buf ~at in
    let* min_y, at = decode_int32 buf ~at in
    let* max_y, at = decode_int32 buf ~at in
    let* flip_x, at = decode_int32 buf ~at in
    let* flip_y, at = decode_int32 buf ~at in
    let* rotation, at = decode_int32 buf ~at in
    let* button_threshold, at = decode_int32 buf ~at in
    ignore orig;
    Some
      ( {
          control_id;
          len;
          min_x;
          max_x;
          min_y;
          max_y;
          flip_x;
          flip_y;
          rotation;
          button_threshold;
        },
        at )

  type device_abs_area_state = {
    control_id : device_control_enum;
    len : int;
    offset_x : int32;
    offset_y : int32;
    width : int32;
    height : int32;
    screen : int32;
    following : int32;
  }

  let decode_device_abs_area_state buf ~at :
      (device_abs_area_state * int) option =
    let orig = at in
    let* control_id, at =
      decode_enum decode_uint16 (fun x -> x) device_control_enum_of_int buf ~at
    in
    let* len, at = decode_uint16 buf ~at in
    let* offset_x, at = decode_int32 buf ~at in
    let* offset_y, at = decode_int32 buf ~at in
    let* width, at = decode_int32 buf ~at in
    let* height, at = decode_int32 buf ~at in
    let* screen, at = decode_int32 buf ~at in
    let* following, at = decode_int32 buf ~at in
    ignore orig;
    Some
      ( { control_id; len; offset_x; offset_y; width; height; screen; following },
        at )

  type device_core_state = {
    control_id : device_control_enum;
    len : int;
    status : int;
    iscore : int;
  }

  let decode_device_core_state buf ~at : (device_core_state * int) option =
    let orig = at in
    let* control_id, at =
      decode_enum decode_uint16 (fun x -> x) device_control_enum_of_int buf ~at
    in
    let* len, at = decode_uint16 buf ~at in
    let* status, at = decode_uint8 buf ~at in
    let* iscore, at = decode_uint8 buf ~at in
    let at = at + 2 in
    ignore orig;
    Some ({ control_id; len; status; iscore }, at)

  type device_enable_state = {
    control_id : device_control_enum;
    len : int;
    enable : int;
  }

  let decode_device_enable_state buf ~at : (device_enable_state * int) option =
    let orig = at in
    let* control_id, at =
      decode_enum decode_uint16 (fun x -> x) device_control_enum_of_int buf ~at
    in
    let* len, at = decode_uint16 buf ~at in
    let* enable, at = decode_uint8 buf ~at in
    let at = at + 3 in
    ignore orig;
    Some ({ control_id; len; enable }, at)

  type device_control_variant =
    | Resolution of {
        resolution_values : int32 list;
        resolution_min : int32 list;
        resolution_max : int32 list;
      }
    | Abs_calib of {
        min_x : int32;
        max_x : int32;
        min_y : int32;
        max_y : int32;
        flip_x : int32;
        flip_y : int32;
        rotation : int32;
        button_threshold : int32;
      }
    | Core of { status : int; iscore : int }
    | Enable of { enable : int }
    | Abs_area of {
        offset_x : int32;
        offset_y : int32;
        width : int32;
        height : int32;
        screen : int32;
        following : int32;
      }

  type device_state = { len : int; data : device_control_variant }

  let decode_device_state buf ~at : (device_state * int) option =
    let orig = at in
    let* len, at = decode_uint16 buf ~at in
    ignore orig;
    Some ({ len; data }, at)

  type get_device_control_reply = {
    xi_reply_type : int;
    status : (Xproto.grab_status_enum, int) alt;
    control : device_state;
  }

  let get_device_control ~(control_id : device_control_enum) ~(device_id : int)
      () : get_device_control_reply Lwt.t =
    failwith "not implemented"

  type device_resolution_ctl = {
    control_id : device_control_enum;
    len : int;
    first_valuator : int;
    resolution_values : int32 list;
  }

  let decode_device_resolution_ctl buf ~at :
      (device_resolution_ctl * int) option =
    let orig = at in
    let* control_id, at =
      decode_enum decode_uint16 (fun x -> x) device_control_enum_of_int buf ~at
    in
    let* len, at = decode_uint16 buf ~at in
    let* first_valuator, at = decode_uint8 buf ~at in
    let* num_valuators, at = decode_uint8 buf ~at in
    let num_valuators = num_valuators in
    let at = at + 2 in
    let* resolution_values, at =
      decode_list decode_int32 num_valuators buf ~at
    in
    ignore orig;
    Some ({ control_id; len; first_valuator; resolution_values }, at)

  type device_abs_calib_ctl = {
    control_id : device_control_enum;
    len : int;
    min_x : int32;
    max_x : int32;
    min_y : int32;
    max_y : int32;
    flip_x : int32;
    flip_y : int32;
    rotation : int32;
    button_threshold : int32;
  }

  let decode_device_abs_calib_ctl buf ~at : (device_abs_calib_ctl * int) option
      =
    let orig = at in
    let* control_id, at =
      decode_enum decode_uint16 (fun x -> x) device_control_enum_of_int buf ~at
    in
    let* len, at = decode_uint16 buf ~at in
    let* min_x, at = decode_int32 buf ~at in
    let* max_x, at = decode_int32 buf ~at in
    let* min_y, at = decode_int32 buf ~at in
    let* max_y, at = decode_int32 buf ~at in
    let* flip_x, at = decode_int32 buf ~at in
    let* flip_y, at = decode_int32 buf ~at in
    let* rotation, at = decode_int32 buf ~at in
    let* button_threshold, at = decode_int32 buf ~at in
    ignore orig;
    Some
      ( {
          control_id;
          len;
          min_x;
          max_x;
          min_y;
          max_y;
          flip_x;
          flip_y;
          rotation;
          button_threshold;
        },
        at )

  type device_abs_area_ctrl = {
    control_id : device_control_enum;
    len : int;
    offset_x : int32;
    offset_y : int32;
    width : int32;
    height : int32;
    screen : int32;
    following : int32;
  }

  let decode_device_abs_area_ctrl buf ~at : (device_abs_area_ctrl * int) option
      =
    let orig = at in
    let* control_id, at =
      decode_enum decode_uint16 (fun x -> x) device_control_enum_of_int buf ~at
    in
    let* len, at = decode_uint16 buf ~at in
    let* offset_x, at = decode_int32 buf ~at in
    let* offset_y, at = decode_int32 buf ~at in
    let* width, at = decode_int32 buf ~at in
    let* height, at = decode_int32 buf ~at in
    let* screen, at = decode_int32 buf ~at in
    let* following, at = decode_int32 buf ~at in
    ignore orig;
    Some
      ( { control_id; len; offset_x; offset_y; width; height; screen; following },
        at )

  type device_core_ctrl = {
    control_id : device_control_enum;
    len : int;
    status : int;
  }

  let decode_device_core_ctrl buf ~at : (device_core_ctrl * int) option =
    let orig = at in
    let* control_id, at =
      decode_enum decode_uint16 (fun x -> x) device_control_enum_of_int buf ~at
    in
    let* len, at = decode_uint16 buf ~at in
    let* status, at = decode_uint8 buf ~at in
    let at = at + 3 in
    ignore orig;
    Some ({ control_id; len; status }, at)

  type device_enable_ctrl = {
    control_id : device_control_enum;
    len : int;
    enable : int;
  }

  let decode_device_enable_ctrl buf ~at : (device_enable_ctrl * int) option =
    let orig = at in
    let* control_id, at =
      decode_enum decode_uint16 (fun x -> x) device_control_enum_of_int buf ~at
    in
    let* len, at = decode_uint16 buf ~at in
    let* enable, at = decode_uint8 buf ~at in
    let at = at + 3 in
    ignore orig;
    Some ({ control_id; len; enable }, at)

  type device_ctl = { len : int; data : device_control_variant }

  let decode_device_ctl buf ~at : (device_ctl * int) option =
    let orig = at in
    let* len, at = decode_uint16 buf ~at in
    ignore orig;
    Some ({ len; data }, at)

  type change_device_control_reply = {
    xi_reply_type : int;
    status : (Xproto.grab_status_enum, int) alt;
  }

  let change_device_control ~(control_id : device_control_enum)
      ~(device_id : int) ~(control : device_ctl) () :
      change_device_control_reply Lwt.t =
    failwith "not implemented"

  type list_device_properties_reply = {
    xi_reply_type : int;
    atoms : Xproto.atom list;
  }

  let list_device_properties ~(device_id : int) () :
      list_device_properties_reply Lwt.t =
    failwith "not implemented"

  type property_format_enum = [ `D8_bits | `D16_bits | `D32_bits ]

  let property_format_enum_of_int : int -> property_format_enum option =
    function
    | 8 -> Some `D8_bits
    | 16 -> Some `D16_bits
    | 32 -> Some `D32_bits
    | _ -> None

  type property_format_variant =
    | D8_bits of { data8 : int list }
    | D16_bits of { data16 : int list }
    | D32_bits of { data32 : int32 list }

  let change_device_property ~(property : Xproto.atom) ~(type_ : Xproto.atom)
      ~(device_id : int) ~(mode : Xproto.prop_mode_enum) ~(num_items : int32)
      ~(items : property_format_variant) () : unit Lwt.t =
    failwith "not implemented"

  let delete_device_property ~(property : Xproto.atom) ~(device_id : int) () :
      unit Lwt.t =
    failwith "not implemented"

  type get_device_property_reply = {
    xi_reply_type : int;
    type_ : Xproto.atom;
    bytes_after : int32;
    num_items : int32;
    device_id : int;
    items : property_format_variant;
  }

  let get_device_property ~(property : Xproto.atom) ~(type_ : Xproto.atom)
      ~(offset : int32) ~(len : int32) ~(device_id : int) ~(delete : bool) () :
      get_device_property_reply Lwt.t =
    failwith "not implemented"

  type device_enum = [ `All | `All_master ]

  let device_enum_of_int : int -> device_enum option = function
    | 0 -> Some `All
    | 1 -> Some `All_master
    | _ -> None

  type group_info = { base : int; latched : int; locked : int; effective : int }

  let decode_group_info buf ~at : (group_info * int) option =
    let orig = at in
    let* base, at = decode_uint8 buf ~at in
    let* latched, at = decode_uint8 buf ~at in
    let* locked, at = decode_uint8 buf ~at in
    let* effective, at = decode_uint8 buf ~at in
    ignore orig;
    Some ({ base; latched; locked; effective }, at)

  type modifier_info = {
    base : int32;
    latched : int32;
    locked : int32;
    effective : int32;
  }

  let decode_modifier_info buf ~at : (modifier_info * int) option =
    let orig = at in
    let* base, at = decode_int32 buf ~at in
    let* latched, at = decode_int32 buf ~at in
    let* locked, at = decode_int32 buf ~at in
    let* effective, at = decode_int32 buf ~at in
    ignore orig;
    Some ({ base; latched; locked; effective }, at)

  type xi_query_pointer_reply = {
    root : Xproto.window;
    child : Xproto.window;
    root_x : fp1616;
    root_y : fp1616;
    win_x : fp1616;
    win_y : fp1616;
    same_screen : bool;
    mods : modifier_info;
    group : group_info;
    buttons : int32 list;
  }

  let xi_query_pointer ~(window : Xproto.window)
      ~(deviceid : (device_enum, device_id) alt) () :
      xi_query_pointer_reply Lwt.t =
    failwith "not implemented"

  let xi_warp_pointer ~(src_win : Xproto.window) ~(dst_win : Xproto.window)
      ~(src_x : fp1616) ~(src_y : fp1616) ~(src_width : int) ~(src_height : int)
      ~(dst_x : fp1616) ~(dst_y : fp1616)
      ~(deviceid : (device_enum, device_id) alt) () : unit Lwt.t =
    failwith "not implemented"

  let xi_change_cursor ~(window : Xproto.window) ~(cursor : Xproto.cursor)
      ~(deviceid : (device_enum, device_id) alt) () : unit Lwt.t =
    failwith "not implemented"

  type hierarchy_change_type_enum =
    [ `Add_master | `Remove_master | `Attach_slave | `Detach_slave ]

  let hierarchy_change_type_enum_of_int :
      int -> hierarchy_change_type_enum option = function
    | 1 -> Some `Add_master
    | 2 -> Some `Remove_master
    | 3 -> Some `Attach_slave
    | 4 -> Some `Detach_slave
    | _ -> None

  type change_mode_enum = [ `Attach | `Float ]

  let change_mode_enum_of_int : int -> change_mode_enum option = function
    | 1 -> Some `Attach
    | 2 -> Some `Float
    | _ -> None

  type add_master = {
    type_ : hierarchy_change_type_enum;
    len : int;
    send_core : bool;
    enable : bool;
    name : char list;
  }

  let decode_add_master buf ~at : (add_master * int) option =
    let orig = at in
    let* type_, at =
      decode_enum decode_uint16
        (fun x -> x)
        hierarchy_change_type_enum_of_int buf ~at
    in
    let* len, at = decode_uint16 buf ~at in
    let* name_len, at = decode_uint16 buf ~at in
    let name_len = name_len in
    let* send_core, at = decode_bool buf ~at in
    let* enable, at = decode_bool buf ~at in
    let* name, at = decode_list decode_char name_len buf ~at in
    let at = at + ((at - orig) mod 4) in
    ignore orig;
    Some ({ type_; len; send_core; enable; name }, at)

  type remove_master = {
    type_ : hierarchy_change_type_enum;
    len : int;
    deviceid : (device_enum, device_id) alt;
    return_mode : change_mode_enum;
    return_pointer : (device_enum, device_id) alt;
    return_keyboard : (device_enum, device_id) alt;
  }

  let decode_remove_master buf ~at : (remove_master * int) option =
    let orig = at in
    let* type_, at =
      decode_enum decode_uint16
        (fun x -> x)
        hierarchy_change_type_enum_of_int buf ~at
    in
    let* len, at = decode_uint16 buf ~at in
    let* deviceid, at = decode_device_id buf ~at in
    let* return_mode, at =
      decode_enum decode_uint8 (fun x -> x) change_mode_enum_of_int buf ~at
    in
    let at = at + 1 in
    let* return_pointer, at = decode_device_id buf ~at in
    let* return_keyboard, at = decode_device_id buf ~at in
    ignore orig;
    Some
      ( { type_; len; deviceid; return_mode; return_pointer; return_keyboard },
        at )

  type attach_slave = {
    type_ : hierarchy_change_type_enum;
    len : int;
    deviceid : (device_enum, device_id) alt;
    master : (device_enum, device_id) alt;
  }

  let decode_attach_slave buf ~at : (attach_slave * int) option =
    let orig = at in
    let* type_, at =
      decode_enum decode_uint16
        (fun x -> x)
        hierarchy_change_type_enum_of_int buf ~at
    in
    let* len, at = decode_uint16 buf ~at in
    let* deviceid, at = decode_device_id buf ~at in
    let* master, at = decode_device_id buf ~at in
    ignore orig;
    Some ({ type_; len; deviceid; master }, at)

  type detach_slave = {
    type_ : hierarchy_change_type_enum;
    len : int;
    deviceid : (device_enum, device_id) alt;
  }

  let decode_detach_slave buf ~at : (detach_slave * int) option =
    let orig = at in
    let* type_, at =
      decode_enum decode_uint16
        (fun x -> x)
        hierarchy_change_type_enum_of_int buf ~at
    in
    let* len, at = decode_uint16 buf ~at in
    let* deviceid, at = decode_device_id buf ~at in
    let at = at + 2 in
    ignore orig;
    Some ({ type_; len; deviceid }, at)

  type hierarchy_change_type_variant =
    | Add_master of { send_core : bool; enable : bool; name : char list }
    | Remove_master of {
        deviceid : (device_enum, device_id) alt;
        return_mode : change_mode_enum;
        return_pointer : (device_enum, device_id) alt;
        return_keyboard : (device_enum, device_id) alt;
      }
    | Attach_slave of {
        deviceid : (device_enum, device_id) alt;
        master : (device_enum, device_id) alt;
      }
    | Detach_slave of { deviceid : (device_enum, device_id) alt }

  type hierarchy_change = { len : int; data : hierarchy_change_type_variant }

  let decode_hierarchy_change buf ~at : (hierarchy_change * int) option =
    let orig = at in
    let* len, at = decode_uint16 buf ~at in
    ignore orig;
    Some ({ len; data }, at)

  let xi_change_hierarchy ~(changes : hierarchy_change list) () : unit Lwt.t =
    failwith "not implemented"

  let xi_set_client_pointer ~(window : Xproto.window)
      ~(deviceid : (device_enum, device_id) alt) () : unit Lwt.t =
    failwith "not implemented"

  type xi_get_client_pointer_reply = {
    set : bool;
    deviceid : (device_enum, device_id) alt;
  }

  let xi_get_client_pointer ~(window : Xproto.window) () :
      xi_get_client_pointer_reply Lwt.t =
    failwith "not implemented"

  type xi_event_mask_mask =
    [ `Device_changed
    | `Key_press
    | `Key_release
    | `Button_press
    | `Button_release
    | `Motion
    | `Enter
    | `Leave
    | `Focus_in
    | `Focus_out
    | `Hierarchy
    | `Property
    | `Raw_key_press
    | `Raw_key_release
    | `Raw_button_press
    | `Raw_button_release
    | `Raw_motion
    | `Touch_begin
    | `Touch_update
    | `Touch_end
    | `Touch_ownership
    | `Raw_touch_begin
    | `Raw_touch_update
    | `Raw_touch_end
    | `Barrier_hit
    | `Barrier_leave ]
    list

  type event_mask = {
    deviceid : (device_enum, device_id) alt;
    mask : xi_event_mask_mask list;
  }

  let decode_event_mask buf ~at : (event_mask * int) option =
    let orig = at in
    let* deviceid, at = decode_device_id buf ~at in
    let* mask_len, at = decode_uint16 buf ~at in
    let mask_len = mask_len in
    let* mask, at = decode_list decode_int32 mask_len buf ~at in
    ignore orig;
    Some ({ deviceid; mask }, at)

  let xi_select_events ~(window : Xproto.window) ~(masks : event_mask list) () :
      unit Lwt.t =
    failwith "not implemented"

  type xi_query_version_reply = { major_version : int; minor_version : int }

  let xi_query_version ~(major_version : int) ~(minor_version : int) () :
      xi_query_version_reply Lwt.t =
    failwith "not implemented"

  type device_class_type_enum = [ `Key | `Button | `Valuator | `Scroll | `Touch ]

  let device_class_type_enum_of_int : int -> device_class_type_enum option =
    function
    | 0 -> Some `Key
    | 1 -> Some `Button
    | 2 -> Some `Valuator
    | 3 -> Some `Scroll
    | 8 -> Some `Touch
    | _ -> None

  type device_type_enum =
    [ `Master_pointer
    | `Master_keyboard
    | `Slave_pointer
    | `Slave_keyboard
    | `Floating_slave ]

  let device_type_enum_of_int : int -> device_type_enum option = function
    | 1 -> Some `Master_pointer
    | 2 -> Some `Master_keyboard
    | 3 -> Some `Slave_pointer
    | 4 -> Some `Slave_keyboard
    | 5 -> Some `Floating_slave
    | _ -> None

  type scroll_flags_mask = [ `No_emulation | `Preferred ] list

  type scroll_type_enum = [ `Vertical | `Horizontal ]

  let scroll_type_enum_of_int : int -> scroll_type_enum option = function
    | 1 -> Some `Vertical
    | 2 -> Some `Horizontal
    | _ -> None

  type touch_mode_enum = [ `Direct | `Dependent ]

  let touch_mode_enum_of_int : int -> touch_mode_enum option = function
    | 1 -> Some `Direct
    | 2 -> Some `Dependent
    | _ -> None

  type button_class = {
    type_ : device_class_type_enum;
    len : int;
    sourceid : device_id;
    state : int32 list;
    labels : Xproto.atom list;
  }

  let decode_button_class buf ~at : (button_class * int) option =
    let orig = at in
    let* type_, at =
      decode_enum decode_uint16
        (fun x -> x)
        device_class_type_enum_of_int buf ~at
    in
    let* len, at = decode_uint16 buf ~at in
    let* sourceid, at = decode_device_id buf ~at in
    let* num_buttons, at = decode_uint16 buf ~at in
    let num_buttons = num_buttons in
    let* state, at =
      decode_list decode_int32 ((num_buttons + 31) / 32) buf ~at
    in
    let* labels, at = decode_list Xproto.decode_atom num_buttons buf ~at in
    ignore orig;
    Some ({ type_; len; sourceid; state; labels }, at)

  type key_class = {
    type_ : device_class_type_enum;
    len : int;
    sourceid : device_id;
    keys : int32 list;
  }

  let decode_key_class buf ~at : (key_class * int) option =
    let orig = at in
    let* type_, at =
      decode_enum decode_uint16
        (fun x -> x)
        device_class_type_enum_of_int buf ~at
    in
    let* len, at = decode_uint16 buf ~at in
    let* sourceid, at = decode_device_id buf ~at in
    let* num_keys, at = decode_uint16 buf ~at in
    let num_keys = num_keys in
    let* keys, at = decode_list decode_int32 num_keys buf ~at in
    ignore orig;
    Some ({ type_; len; sourceid; keys }, at)

  type scroll_class = {
    type_ : device_class_type_enum;
    len : int;
    sourceid : device_id;
    number : int;
    scroll_type : scroll_type_enum;
    flags : scroll_flags_mask;
    increment : fp3232;
  }

  let decode_scroll_class buf ~at : (scroll_class * int) option =
    let orig = at in
    let* type_, at =
      decode_enum decode_uint16
        (fun x -> x)
        device_class_type_enum_of_int buf ~at
    in
    let* len, at = decode_uint16 buf ~at in
    let* sourceid, at = decode_device_id buf ~at in
    let* number, at = decode_uint16 buf ~at in
    let* scroll_type, at =
      decode_enum decode_uint16 (fun x -> x) scroll_type_enum_of_int buf ~at
    in
    let at = at + 2 in
    let* flags, at = decode_int32 buf ~at in
    let* increment, at = decode_fp3232 buf ~at in
    ignore orig;
    Some ({ type_; len; sourceid; number; scroll_type; flags; increment }, at)

  type touch_class = {
    type_ : device_class_type_enum;
    len : int;
    sourceid : device_id;
    mode : touch_mode_enum;
    num_touches : int;
  }

  let decode_touch_class buf ~at : (touch_class * int) option =
    let orig = at in
    let* type_, at =
      decode_enum decode_uint16
        (fun x -> x)
        device_class_type_enum_of_int buf ~at
    in
    let* len, at = decode_uint16 buf ~at in
    let* sourceid, at = decode_device_id buf ~at in
    let* mode, at =
      decode_enum decode_uint8 (fun x -> x) touch_mode_enum_of_int buf ~at
    in
    let* num_touches, at = decode_uint8 buf ~at in
    ignore orig;
    Some ({ type_; len; sourceid; mode; num_touches }, at)

  type valuator_class = {
    type_ : device_class_type_enum;
    len : int;
    sourceid : device_id;
    number : int;
    label : Xproto.atom;
    min : fp3232;
    max : fp3232;
    value : fp3232;
    resolution : int32;
    mode : valuator_mode_enum;
  }

  let decode_valuator_class buf ~at : (valuator_class * int) option =
    let orig = at in
    let* type_, at =
      decode_enum decode_uint16
        (fun x -> x)
        device_class_type_enum_of_int buf ~at
    in
    let* len, at = decode_uint16 buf ~at in
    let* sourceid, at = decode_device_id buf ~at in
    let* number, at = decode_uint16 buf ~at in
    let* label, at = Xproto.decode_atom buf ~at in
    let* min, at = decode_fp3232 buf ~at in
    let* max, at = decode_fp3232 buf ~at in
    let* value, at = decode_fp3232 buf ~at in
    let* resolution, at = decode_int32 buf ~at in
    let* mode, at =
      decode_enum decode_uint8 (fun x -> x) valuator_mode_enum_of_int buf ~at
    in
    let at = at + 3 in
    ignore orig;
    Some
      ( {
          type_;
          len;
          sourceid;
          number;
          label;
          min;
          max;
          value;
          resolution;
          mode;
        },
        at )

  type device_class_type_variant =
    | Key of { keys : int32 list }
    | Button of { state : int32 list; labels : Xproto.atom list }
    | Valuator of {
        number : int;
        label : Xproto.atom;
        min : fp3232;
        max : fp3232;
        value : fp3232;
        resolution : int32;
        mode : valuator_mode_enum;
      }
    | Scroll of {
        number : int;
        scroll_type : scroll_type_enum;
        flags : scroll_flags_mask;
        increment : fp3232;
      }
    | Touch of { mode : touch_mode_enum; num_touches : int }

  type device_class = {
    len : int;
    sourceid : device_id;
    data : device_class_type_variant;
  }

  let decode_device_class buf ~at : (device_class * int) option =
    let orig = at in
    let* len, at = decode_uint16 buf ~at in
    let* sourceid, at = decode_device_id buf ~at in
    ignore orig;
    Some ({ len; sourceid; data }, at)

  type xi_device_info = {
    deviceid : (device_enum, device_id) alt;
    type_ : device_type_enum;
    attachment : (device_enum, device_id) alt;
    enabled : bool;
    name : char list;
    classes : device_class list;
  }

  let decode_xi_device_info buf ~at : (xi_device_info * int) option =
    let orig = at in
    let* deviceid, at = decode_device_id buf ~at in
    let* type_, at =
      decode_enum decode_uint16 (fun x -> x) device_type_enum_of_int buf ~at
    in
    let* attachment, at = decode_device_id buf ~at in
    let* num_classes, at = decode_uint16 buf ~at in
    let num_classes = num_classes in
    let* name_len, at = decode_uint16 buf ~at in
    let name_len = name_len in
    let* enabled, at = decode_bool buf ~at in
    let at = at + 1 in
    let* name, at = decode_list decode_char name_len buf ~at in
    let at = at + ((at - orig) mod 4) in
    let* classes, at = decode_list decode_device_class num_classes buf ~at in
    ignore orig;
    Some ({ deviceid; type_; attachment; enabled; name; classes }, at)

  type xi_query_device_reply = { infos : xi_device_info list }

  let xi_query_device ~(deviceid : (device_enum, device_id) alt) () :
      xi_query_device_reply Lwt.t =
    failwith "not implemented"

  let xi_set_focus ~(window : Xproto.window)
      ~(time : (Xproto.time_enum, Xproto.timestamp) alt)
      ~(deviceid : (device_enum, device_id) alt) () : unit Lwt.t =
    failwith "not implemented"

  type xi_get_focus_reply = { focus : Xproto.window }

  let xi_get_focus ~(deviceid : (device_enum, device_id) alt) () :
      xi_get_focus_reply Lwt.t =
    failwith "not implemented"

  type grab_owner_enum = [ `No_owner | `Owner ]

  let grab_owner_enum_of_int : int -> grab_owner_enum option = function
    | 0 -> Some `No_owner
    | 1 -> Some `Owner
    | _ -> None

  type xi_grab_device_reply = { status : Xproto.grab_status_enum }

  let xi_grab_device ~(window : Xproto.window)
      ~(time : (Xproto.time_enum, Xproto.timestamp) alt)
      ~(cursor : Xproto.cursor) ~(deviceid : (device_enum, device_id) alt)
      ~(mode : Xproto.grab_mode_enum)
      ~(paired_device_mode : Xproto.grab_mode_enum)
      ~(owner_events : grab_owner_enum) ~(mask : int32 list) () :
      xi_grab_device_reply Lwt.t =
    failwith "not implemented"

  let xi_ungrab_device ~(time : (Xproto.time_enum, Xproto.timestamp) alt)
      ~(deviceid : (device_enum, device_id) alt) () : unit Lwt.t =
    failwith "not implemented"

  type event_mode_enum =
    [ `Async_device
    | `Sync_device
    | `Replay_device
    | `Async_paired_device
    | `Async_pair
    | `Sync_pair
    | `Accept_touch
    | `Reject_touch ]

  let event_mode_enum_of_int : int -> event_mode_enum option = function
    | 0 -> Some `Async_device
    | 1 -> Some `Sync_device
    | 2 -> Some `Replay_device
    | 3 -> Some `Async_paired_device
    | 4 -> Some `Async_pair
    | 5 -> Some `Sync_pair
    | 6 -> Some `Accept_touch
    | 7 -> Some `Reject_touch
    | _ -> None

  let xi_allow_events ~(time : (Xproto.time_enum, Xproto.timestamp) alt)
      ~(deviceid : (device_enum, device_id) alt) ~(event_mode : event_mode_enum)
      ~(touchid : int32) ~(grab_window : Xproto.window) () : unit Lwt.t =
    failwith "not implemented"

  type grab_mode22_enum = [ `Sync | `Async | `Touch ]

  let grab_mode22_enum_of_int : int -> grab_mode22_enum option = function
    | 0 -> Some `Sync
    | 1 -> Some `Async
    | 2 -> Some `Touch
    | _ -> None

  type grab_type_enum =
    [ `Button | `Keycode | `Enter | `Focus_in | `Touch_begin ]

  let grab_type_enum_of_int : int -> grab_type_enum option = function
    | 0 -> Some `Button
    | 1 -> Some `Keycode
    | 2 -> Some `Enter
    | 3 -> Some `Focus_in
    | 4 -> Some `Touch_begin
    | _ -> None

  type modifier_mask_mask = [ `Any ] list

  type grab_modifier_info = {
    modifiers : (modifier_mask_mask, int32) alt;
    status : Xproto.grab_status_enum;
  }

  let decode_grab_modifier_info buf ~at : (grab_modifier_info * int) option =
    let orig = at in
    let* modifiers, at = decode_int32 buf ~at in
    let* status, at =
      decode_enum decode_uint8
        (fun x -> x)
        Xproto.grab_status_enum_of_int buf ~at
    in
    let at = at + 3 in
    ignore orig;
    Some ({ modifiers; status }, at)

  type xi_passive_grab_device_reply = { modifiers : grab_modifier_info list }

  let xi_passive_grab_device ~(time : (Xproto.time_enum, Xproto.timestamp) alt)
      ~(grab_window : Xproto.window) ~(cursor : Xproto.cursor) ~(detail : int32)
      ~(deviceid : (device_enum, device_id) alt) ~(grab_type : grab_type_enum)
      ~(grab_mode : grab_mode22_enum)
      ~(paired_device_mode : Xproto.grab_mode_enum)
      ~(owner_events : grab_owner_enum) ~(mask : int32 list)
      ~(modifiers : int32 list) () : xi_passive_grab_device_reply Lwt.t =
    failwith "not implemented"

  let xi_passive_ungrab_device ~(grab_window : Xproto.window) ~(detail : int32)
      ~(deviceid : (device_enum, device_id) alt) ~(grab_type : grab_type_enum)
      ~(modifiers : int32 list) () : unit Lwt.t =
    failwith "not implemented"

  type xi_list_properties_reply = { properties : Xproto.atom list }

  let xi_list_properties ~(deviceid : (device_enum, device_id) alt) () :
      xi_list_properties_reply Lwt.t =
    failwith "not implemented"

  let xi_change_property ~(deviceid : (device_enum, device_id) alt)
      ~(mode : Xproto.prop_mode_enum) ~(property : Xproto.atom)
      ~(type_ : Xproto.atom) ~(num_items : int32)
      ~(items : property_format_variant) () : unit Lwt.t =
    failwith "not implemented"

  let xi_delete_property ~(deviceid : (device_enum, device_id) alt)
      ~(property : Xproto.atom) () : unit Lwt.t =
    failwith "not implemented"

  type xi_get_property_reply = {
    type_ : Xproto.atom;
    bytes_after : int32;
    num_items : int32;
    items : property_format_variant;
  }

  let xi_get_property ~(deviceid : (device_enum, device_id) alt)
      ~(delete : bool) ~(property : Xproto.atom) ~(type_ : Xproto.atom)
      ~(offset : int32) ~(len : int32) () : xi_get_property_reply Lwt.t =
    failwith "not implemented"

  type xi_get_selected_events_reply = { masks : event_mask list }

  let xi_get_selected_events ~(window : Xproto.window) () :
      xi_get_selected_events_reply Lwt.t =
    failwith "not implemented"

  type barrier_release_pointer_info = {
    deviceid : device_id;
    barrier : Xfixes.barrier;
    eventid : int32;
  }

  let decode_barrier_release_pointer_info buf ~at :
      (barrier_release_pointer_info * int) option =
    let orig = at in
    let* deviceid, at = decode_device_id buf ~at in
    let at = at + 2 in
    let* barrier, at = Xfixes.decode_barrier buf ~at in
    let* eventid, at = decode_int32 buf ~at in
    ignore orig;
    Some ({ deviceid; barrier; eventid }, at)

  let xi_barrier_release_pointer ~(barriers : barrier_release_pointer_info list)
      () : unit Lwt.t =
    failwith "not implemented"

  type device_valuator_event = {
    device_id : int;
    device_state : int;
    num_valuators : int;
    first_valuator : int;
    valuators : int32 list;
  }

  type more_events_mask_mask = [ `More_events ] list

  type device_key_press_event = {
    detail : char;
    time : Xproto.timestamp;
    root : Xproto.window;
    event : Xproto.window;
    child : (Xproto.window_enum, Xproto.window) alt;
    root_x : int;
    root_y : int;
    event_x : int;
    event_y : int;
    state : Xproto.key_but_mask_mask;
    same_screen : bool;
    device_id : (more_events_mask_mask, int) alt;
  }

  type device_key_release_event = device_key_press_event

  type device_button_press_event = device_key_press_event

  type device_button_release_event = device_key_press_event

  type device_motion_notify_event = device_key_press_event

  type device_focus_in_event = {
    detail : Xproto.notify_detail_enum;
    time : Xproto.timestamp;
    window : Xproto.window;
    mode : Xproto.notify_mode_enum;
    device_id : int;
  }

  type device_focus_out_event = device_focus_in_event

  type proximity_in_event = device_key_press_event

  type proximity_out_event = device_key_press_event

  type classes_reported_mask_mask =
    [ `Out_of_proximity
    | `Device_mode_absolute
    | `Reporting_valuators
    | `Reporting_buttons
    | `Reporting_keys ]
    list

  type device_state_notify_event = {
    device_id : (more_events_mask_mask, char) alt;
    time : Xproto.timestamp;
    num_keys : int;
    num_buttons : int;
    num_valuators : int;
    classes_reported : classes_reported_mask_mask;
    buttons : int list;
    keys : int list;
    valuators : int32 list;
  }

  type device_mapping_notify_event = {
    device_id : char;
    request : Xproto.mapping_enum;
    first_keycode : key_code;
    count : int;
    time : Xproto.timestamp;
  }

  type change_device_enum = [ `New_pointer | `New_keyboard ]

  let change_device_enum_of_int : int -> change_device_enum option = function
    | 0 -> Some `New_pointer
    | 1 -> Some `New_keyboard
    | _ -> None

  type change_device_notify_event = {
    device_id : char;
    time : Xproto.timestamp;
    request : change_device_enum;
  }

  type device_key_state_notify_event = {
    device_id : (more_events_mask_mask, char) alt;
    keys : int list;
  }

  type device_button_state_notify_event = {
    device_id : (more_events_mask_mask, char) alt;
    buttons : int list;
  }

  type device_change_enum =
    [ `Added
    | `Removed
    | `Enabled
    | `Disabled
    | `Unrecoverable
    | `Control_changed ]

  let device_change_enum_of_int : int -> device_change_enum option = function
    | 0 -> Some `Added
    | 1 -> Some `Removed
    | 2 -> Some `Enabled
    | 3 -> Some `Disabled
    | 4 -> Some `Unrecoverable
    | 5 -> Some `Control_changed
    | _ -> None

  type device_presence_notify_event = {
    time : Xproto.timestamp;
    devchange : device_change_enum;
    device_id : char;
    control : int;
  }

  type device_property_notify_event = {
    state : Xproto.property_enum;
    time : Xproto.timestamp;
    property : Xproto.atom;
    device_id : int;
  }

  type change_reason_enum = [ `Slave_switch | `Device_change ]

  let change_reason_enum_of_int : int -> change_reason_enum option = function
    | 1 -> Some `Slave_switch
    | 2 -> Some `Device_change
    | _ -> None

  type device_changed_event = {
    deviceid : (device_enum, device_id) alt;
    time : (Xproto.time_enum, Xproto.timestamp) alt;
    sourceid : (device_enum, device_id) alt;
    reason : change_reason_enum;
    classes : device_class list;
  }

  type key_event_flags_mask = [ `Key_repeat ] list

  type key_press_event = {
    deviceid : (device_enum, device_id) alt;
    time : (Xproto.time_enum, Xproto.timestamp) alt;
    detail : int32;
    root : Xproto.window;
    event : Xproto.window;
    child : Xproto.window;
    root_x : fp1616;
    root_y : fp1616;
    event_x : fp1616;
    event_y : fp1616;
    sourceid : (device_enum, device_id) alt;
    flags : key_event_flags_mask;
    mods : modifier_info;
    group : group_info;
    button_mask : int32 list;
    valuator_mask : int32 list;
    axisvalues : fp3232 list;
  }

  type key_release_event = key_press_event

  type pointer_event_flags_mask = [ `Pointer_emulated ] list

  type button_press_event = {
    deviceid : (device_enum, device_id) alt;
    time : (Xproto.time_enum, Xproto.timestamp) alt;
    detail : int32;
    root : Xproto.window;
    event : Xproto.window;
    child : Xproto.window;
    root_x : fp1616;
    root_y : fp1616;
    event_x : fp1616;
    event_y : fp1616;
    sourceid : (device_enum, device_id) alt;
    flags : pointer_event_flags_mask;
    mods : modifier_info;
    group : group_info;
    button_mask : int32 list;
    valuator_mask : int32 list;
    axisvalues : fp3232 list;
  }

  type button_release_event = button_press_event

  type motion_event = button_press_event

  type notify_mode_enum =
    [ `Normal
    | `Grab
    | `Ungrab
    | `While_grabbed
    | `Passive_grab
    | `Passive_ungrab ]

  let notify_mode_enum_of_int : int -> notify_mode_enum option = function
    | 0 -> Some `Normal
    | 1 -> Some `Grab
    | 2 -> Some `Ungrab
    | 3 -> Some `While_grabbed
    | 4 -> Some `Passive_grab
    | 5 -> Some `Passive_ungrab
    | _ -> None

  type notify_detail_enum =
    [ `Ancestor
    | `Virtual
    | `Inferior
    | `Nonlinear
    | `Nonlinear_virtual
    | `Pointer
    | `Pointer_root
    | `None ]

  let notify_detail_enum_of_int : int -> notify_detail_enum option = function
    | 0 -> Some `Ancestor
    | 1 -> Some `Virtual
    | 2 -> Some `Inferior
    | 3 -> Some `Nonlinear
    | 4 -> Some `Nonlinear_virtual
    | 5 -> Some `Pointer
    | 6 -> Some `Pointer_root
    | 7 -> Some `None
    | _ -> None

  type enter_event = {
    deviceid : (device_enum, device_id) alt;
    time : (Xproto.time_enum, Xproto.timestamp) alt;
    sourceid : (device_enum, device_id) alt;
    mode : notify_mode_enum;
    detail : notify_detail_enum;
    root : Xproto.window;
    event : Xproto.window;
    child : Xproto.window;
    root_x : fp1616;
    root_y : fp1616;
    event_x : fp1616;
    event_y : fp1616;
    same_screen : bool;
    focus : bool;
    mods : modifier_info;
    group : group_info;
    buttons : int32 list;
  }

  type leave_event = enter_event

  type focus_in_event = enter_event

  type focus_out_event = enter_event

  type hierarchy_mask_mask =
    [ `Master_added
    | `Master_removed
    | `Slave_added
    | `Slave_removed
    | `Slave_attached
    | `Slave_detached
    | `Device_enabled
    | `Device_disabled ]
    list

  type hierarchy_info = {
    deviceid : (device_enum, device_id) alt;
    attachment : (device_enum, device_id) alt;
    type_ : device_type_enum;
    enabled : bool;
    flags : hierarchy_mask_mask;
  }

  let decode_hierarchy_info buf ~at : (hierarchy_info * int) option =
    let orig = at in
    let* deviceid, at = decode_device_id buf ~at in
    let* attachment, at = decode_device_id buf ~at in
    let* type_, at =
      decode_enum decode_uint8 (fun x -> x) device_type_enum_of_int buf ~at
    in
    let* enabled, at = decode_bool buf ~at in
    let at = at + 2 in
    let* flags, at = decode_int32 buf ~at in
    ignore orig;
    Some ({ deviceid; attachment; type_; enabled; flags }, at)

  type hierarchy_event = {
    deviceid : (device_enum, device_id) alt;
    time : (Xproto.time_enum, Xproto.timestamp) alt;
    flags : hierarchy_mask_mask;
    infos : hierarchy_info list;
  }

  type property_flag_enum = [ `Deleted | `Created | `Modified ]

  let property_flag_enum_of_int : int -> property_flag_enum option = function
    | 0 -> Some `Deleted
    | 1 -> Some `Created
    | 2 -> Some `Modified
    | _ -> None

  type property_event = {
    deviceid : (device_enum, device_id) alt;
    time : (Xproto.time_enum, Xproto.timestamp) alt;
    property : Xproto.atom;
    what : property_flag_enum;
  }

  type raw_key_press_event = {
    deviceid : (device_enum, device_id) alt;
    time : (Xproto.time_enum, Xproto.timestamp) alt;
    detail : int32;
    sourceid : device_id;
    flags : key_event_flags_mask;
    valuator_mask : int32 list;
    axisvalues : fp3232 list;
    axisvalues_raw : fp3232 list;
  }

  type raw_key_release_event = raw_key_press_event

  type raw_button_press_event = {
    deviceid : (device_enum, device_id) alt;
    time : (Xproto.time_enum, Xproto.timestamp) alt;
    detail : int32;
    sourceid : device_id;
    flags : pointer_event_flags_mask;
    valuator_mask : int32 list;
    axisvalues : fp3232 list;
    axisvalues_raw : fp3232 list;
  }

  type raw_button_release_event = raw_button_press_event

  type raw_motion_event = raw_button_press_event

  type touch_event_flags_mask =
    [ `Touch_pending_end | `Touch_emulating_pointer ] list

  type touch_begin_event = {
    deviceid : (device_enum, device_id) alt;
    time : (Xproto.time_enum, Xproto.timestamp) alt;
    detail : int32;
    root : Xproto.window;
    event : Xproto.window;
    child : Xproto.window;
    root_x : fp1616;
    root_y : fp1616;
    event_x : fp1616;
    event_y : fp1616;
    sourceid : (device_enum, device_id) alt;
    flags : touch_event_flags_mask;
    mods : modifier_info;
    group : group_info;
    button_mask : int32 list;
    valuator_mask : int32 list;
    axisvalues : fp3232 list;
  }

  type touch_update_event = touch_begin_event

  type touch_end_event = touch_begin_event

  type touch_ownership_flags_enum = [ `None ]

  let touch_ownership_flags_enum_of_int :
      int -> touch_ownership_flags_enum option = function
    | 0 -> Some `None
    | _ -> None

  type touch_ownership_event = {
    deviceid : (device_enum, device_id) alt;
    time : (Xproto.time_enum, Xproto.timestamp) alt;
    touchid : int32;
    root : Xproto.window;
    event : Xproto.window;
    child : Xproto.window;
    sourceid : (device_enum, device_id) alt;
    flags : touch_ownership_flags_enum;
  }

  type raw_touch_begin_event = {
    deviceid : (device_enum, device_id) alt;
    time : (Xproto.time_enum, Xproto.timestamp) alt;
    detail : int32;
    sourceid : device_id;
    flags : touch_event_flags_mask;
    valuator_mask : int32 list;
    axisvalues : fp3232 list;
    axisvalues_raw : fp3232 list;
  }

  type raw_touch_update_event = raw_touch_begin_event

  type raw_touch_end_event = raw_touch_begin_event

  type barrier_flags_mask = [ `Pointer_released | `Device_is_grabbed ] list

  type barrier_hit_event = {
    deviceid : (device_enum, device_id) alt;
    time : (Xproto.time_enum, Xproto.timestamp) alt;
    eventid : int32;
    root : Xproto.window;
    event : Xproto.window;
    barrier : Xfixes.barrier;
    dtime : int32;
    flags : barrier_flags_mask;
    sourceid : (device_enum, device_id) alt;
    root_x : fp1616;
    root_y : fp1616;
    dx : fp3232;
    dy : fp3232;
  }

  type barrier_leave_event = barrier_hit_event

  type event_for_send =
    | Device_valuator of device_valuator_event
    | Device_key_press of device_key_press_event
    | Device_key_release of device_key_release_event
    | Device_button_press of device_button_press_event
    | Device_button_release of device_button_release_event
    | Device_motion_notify of device_motion_notify_event
    | Device_focus_in of device_focus_in_event
    | Device_focus_out of device_focus_out_event
    | Proximity_in of proximity_in_event
    | Proximity_out of proximity_out_event
    | Device_state_notify of device_state_notify_event
    | Device_mapping_notify of device_mapping_notify_event
    | Change_device_notify of change_device_notify_event
    | Device_key_state_notify of device_key_state_notify_event
    | Device_button_state_notify of device_button_state_notify_event
    | Device_presence_notify of device_presence_notify_event
    | Device_property_notify of device_property_notify_event

  let send_extension_event ~(destination : Xproto.window) ~(device_id : int)
      ~(propagate : bool) ~(events : event_for_send list)
      ~(classes : event_class list) () : unit Lwt.t =
    failwith "not implemented"

  type device_error = unit

  type event_error = unit

  type mode_error = unit

  type device_busy_error = unit

  type class_error = unit
end
[@@warning "-27"]

module Xprint = struct
  type string8 = char

  let decode_string8 = decode_char

  type printer = { name : string8 list; description : string8 list }

  let decode_printer buf ~at : (printer * int) option =
    let orig = at in
    let* name_len, at = decode_int32 buf ~at in
    let name_len = Int32.to_int name_len in
    let name_len = name_len in
    let* name, at = decode_list decode_string8 name_len buf ~at in
    let at = at + ((at - orig) mod 4) in
    let* desc_len, at = decode_int32 buf ~at in
    let desc_len = Int32.to_int desc_len in
    let desc_len = desc_len in
    let* description, at = decode_list decode_string8 desc_len buf ~at in
    let at = at + ((at - orig) mod 4) in
    ignore orig;
    Some ({ name; description }, at)

  type pcontext = xid

  let decode_pcontext = decode_xid

  type get_doc_enum = [ `Finished | `Second_consumer ]

  let get_doc_enum_of_int : int -> get_doc_enum option = function
    | 0 -> Some `Finished
    | 1 -> Some `Second_consumer
    | _ -> None

  type ev_mask_mask =
    ([ `Print_mask | `Attribute_mask ], [ `No_event_mask ]) mask

  type detail_enum =
    [ `Start_job_notify
    | `End_job_notify
    | `Start_doc_notify
    | `End_doc_notify
    | `Start_page_notify
    | `End_page_notify ]

  let detail_enum_of_int : int -> detail_enum option = function
    | 1 -> Some `Start_job_notify
    | 2 -> Some `End_job_notify
    | 3 -> Some `Start_doc_notify
    | 4 -> Some `End_doc_notify
    | 5 -> Some `Start_page_notify
    | 6 -> Some `End_page_notify
    | _ -> None

  type attr_enum =
    [ `Job_attr
    | `Doc_attr
    | `Page_attr
    | `Printer_attr
    | `Server_attr
    | `Medium_attr
    | `Spooler_attr ]

  let attr_enum_of_int : int -> attr_enum option = function
    | 1 -> Some `Job_attr
    | 2 -> Some `Doc_attr
    | 3 -> Some `Page_attr
    | 4 -> Some `Printer_attr
    | 5 -> Some `Server_attr
    | 6 -> Some `Medium_attr
    | 7 -> Some `Spooler_attr
    | _ -> None

  type print_query_version_reply = { major_version : int; minor_version : int }

  let print_query_version () : print_query_version_reply Lwt.t =
    failwith "not implemented"

  type print_get_printer_list_reply = { printers : printer list }

  let print_get_printer_list ~(printer_name : string8 list)
      ~(locale : string8 list) () : print_get_printer_list_reply Lwt.t =
    failwith "not implemented"

  let print_rehash_printer_list () : unit Lwt.t = failwith "not implemented"

  let create_context ~(context_id : int32) ~(printer_name : string8 list)
      ~(locale : string8 list) () : unit Lwt.t =
    failwith "not implemented"

  let print_set_context ~(context : int32) () : unit Lwt.t =
    failwith "not implemented"

  type print_get_context_reply = { context : int32 }

  let print_get_context () : print_get_context_reply Lwt.t =
    failwith "not implemented"

  let print_destroy_context ~(context : int32) () : unit Lwt.t =
    failwith "not implemented"

  type print_get_screen_of_context_reply = { root : Xproto.window }

  let print_get_screen_of_context () : print_get_screen_of_context_reply Lwt.t =
    failwith "not implemented"

  let print_start_job ~(output_mode : int) () : unit Lwt.t =
    failwith "not implemented"

  let print_end_job ~(cancel : bool) () : unit Lwt.t =
    failwith "not implemented"

  let print_start_doc ~(driver_mode : int) () : unit Lwt.t =
    failwith "not implemented"

  let print_end_doc ~(cancel : bool) () : unit Lwt.t =
    failwith "not implemented"

  let print_put_document_data ~(drawable : Xproto.drawable) ~(data : char list)
      ~(doc_format : string8 list) ~(options : string8 list) () : unit Lwt.t =
    failwith "not implemented"

  type print_get_document_data_reply = {
    status_code : int32;
    finished_flag : int32;
    data : char list;
  }

  let print_get_document_data ~(context : pcontext) ~(max_bytes : int32) () :
      print_get_document_data_reply Lwt.t =
    failwith "not implemented"

  let print_start_page ~(window : Xproto.window) () : unit Lwt.t =
    failwith "not implemented"

  let print_end_page ~(cancel : bool) () : unit Lwt.t =
    failwith "not implemented"

  let print_select_input ~(context : pcontext) ~(event_mask : int32) () :
      unit Lwt.t =
    failwith "not implemented"

  type print_input_selected_reply = {
    event_mask : int32;
    all_events_mask : int32;
  }

  let print_input_selected ~(context : pcontext) () :
      print_input_selected_reply Lwt.t =
    failwith "not implemented"

  type print_get_attributes_reply = { attributes : string8 list }

  let print_get_attributes ~(context : pcontext) ~(pool : int) () :
      print_get_attributes_reply Lwt.t =
    failwith "not implemented"

  type print_get_one_attributes_reply = { value : string8 list }

  let print_get_one_attributes ~(context : pcontext) ~(pool : int)
      ~(name : string8 list) () : print_get_one_attributes_reply Lwt.t =
    failwith "not implemented"

  let print_set_attributes ~(context : pcontext) ~(string_len : int32)
      ~(pool : int) ~(rule : int) ~(attributes : string8 list) () : unit Lwt.t =
    failwith "not implemented"

  type print_get_page_dimensions_reply = {
    width : int;
    height : int;
    offset_x : int;
    offset_y : int;
    reproducible_width : int;
    reproducible_height : int;
  }

  let print_get_page_dimensions ~(context : pcontext) () :
      print_get_page_dimensions_reply Lwt.t =
    failwith "not implemented"

  type print_query_screens_reply = { roots : Xproto.window list }

  let print_query_screens () : print_query_screens_reply Lwt.t =
    failwith "not implemented"

  type print_set_image_resolution_reply = {
    status : bool;
    previous_resolutions : int;
  }

  let print_set_image_resolution ~(context : pcontext) ~(image_resolution : int)
      () : print_set_image_resolution_reply Lwt.t =
    failwith "not implemented"

  type print_get_image_resolution_reply = { image_resolution : int }

  let print_get_image_resolution ~(context : pcontext) () :
      print_get_image_resolution_reply Lwt.t =
    failwith "not implemented"

  type notify_event = { detail : int; context : pcontext; cancel : bool }

  type attribut_notify_event = { detail : int; context : pcontext }

  type bad_context_error = unit

  type bad_sequence_error = unit
end
[@@warning "-27"]

module Xselinux = struct
  type query_version_reply = { server_major : int; server_minor : int }

  let query_version ~(client_major : int) ~(client_minor : int) () :
      query_version_reply Lwt.t =
    failwith "not implemented"

  let set_device_create_context ~(context : char list) () : unit Lwt.t =
    failwith "not implemented"

  type get_device_create_context_reply = { context : char list }

  let get_device_create_context () : get_device_create_context_reply Lwt.t =
    failwith "not implemented"

  let set_device_context ~(device : int32) ~(context : char list) () :
      unit Lwt.t =
    failwith "not implemented"

  type get_device_context_reply = { context : char list }

  let get_device_context ~(device : int32) () : get_device_context_reply Lwt.t =
    failwith "not implemented"

  let set_window_create_context ~(context : char list) () : unit Lwt.t =
    failwith "not implemented"

  type get_window_create_context_reply = { context : char list }

  let get_window_create_context () : get_window_create_context_reply Lwt.t =
    failwith "not implemented"

  type get_window_context_reply = { context : char list }

  let get_window_context ~(window : Xproto.window) () :
      get_window_context_reply Lwt.t =
    failwith "not implemented"

  type list_item = {
    name : Xproto.atom;
    object_context : char list;
    data_context : char list;
  }

  let decode_list_item buf ~at : (list_item * int) option =
    let orig = at in
    let* name, at = Xproto.decode_atom buf ~at in
    let* object_context_len, at = decode_int32 buf ~at in
    let object_context_len = Int32.to_int object_context_len in
    let object_context_len = object_context_len in
    let* data_context_len, at = decode_int32 buf ~at in
    let data_context_len = Int32.to_int data_context_len in
    let data_context_len = data_context_len in
    let* object_context, at =
      decode_list decode_char object_context_len buf ~at
    in
    let at = at + ((at - orig) mod 4) in
    let* data_context, at = decode_list decode_char data_context_len buf ~at in
    let at = at + ((at - orig) mod 4) in
    ignore orig;
    Some ({ name; object_context; data_context }, at)

  let set_property_create_context ~(context : char list) () : unit Lwt.t =
    failwith "not implemented"

  type get_property_create_context_reply = { context : char list }

  let get_property_create_context () : get_property_create_context_reply Lwt.t =
    failwith "not implemented"

  let set_property_use_context ~(context : char list) () : unit Lwt.t =
    failwith "not implemented"

  type get_property_use_context_reply = { context : char list }

  let get_property_use_context () : get_property_use_context_reply Lwt.t =
    failwith "not implemented"

  type get_property_context_reply = { context : char list }

  let get_property_context ~(window : Xproto.window) ~(property : Xproto.atom)
      () : get_property_context_reply Lwt.t =
    failwith "not implemented"

  type get_property_data_context_reply = { context : char list }

  let get_property_data_context ~(window : Xproto.window)
      ~(property : Xproto.atom) () : get_property_data_context_reply Lwt.t =
    failwith "not implemented"

  type list_properties_reply = { properties : list_item list }

  let list_properties ~(window : Xproto.window) () : list_properties_reply Lwt.t
      =
    failwith "not implemented"

  let set_selection_create_context ~(context : char list) () : unit Lwt.t =
    failwith "not implemented"

  type get_selection_create_context_reply = { context : char list }

  let get_selection_create_context () : get_selection_create_context_reply Lwt.t
      =
    failwith "not implemented"

  let set_selection_use_context ~(context : char list) () : unit Lwt.t =
    failwith "not implemented"

  type get_selection_use_context_reply = { context : char list }

  let get_selection_use_context () : get_selection_use_context_reply Lwt.t =
    failwith "not implemented"

  type get_selection_context_reply = { context : char list }

  let get_selection_context ~(selection : Xproto.atom) () :
      get_selection_context_reply Lwt.t =
    failwith "not implemented"

  type get_selection_data_context_reply = { context : char list }

  let get_selection_data_context ~(selection : Xproto.atom) () :
      get_selection_data_context_reply Lwt.t =
    failwith "not implemented"

  type list_selections_reply = { selections : list_item list }

  let list_selections () : list_selections_reply Lwt.t =
    failwith "not implemented"

  type get_client_context_reply = { context : char list }

  let get_client_context ~(resource : int32) () : get_client_context_reply Lwt.t
      =
    failwith "not implemented"
end
[@@warning "-27"]

module Xtest = struct
  type get_version_reply = { major_version : int; minor_version : int }

  let get_version ~(major_version : int) ~(minor_version : int) () :
      get_version_reply Lwt.t =
    failwith "not implemented"

  type cursor_enum = [ `None | `Current ]

  let cursor_enum_of_int : int -> cursor_enum option = function
    | 0 -> Some `None
    | 1 -> Some `Current
    | _ -> None

  type compare_cursor_reply = { same : bool }

  let compare_cursor ~(window : Xproto.window) ~(cursor : Xproto.cursor) () :
      compare_cursor_reply Lwt.t =
    failwith "not implemented"

  let fake_input ~(type_ : char) ~(detail : char) ~(time : int32)
      ~(root : Xproto.window) ~(root_x : int) ~(root_y : int) ~(deviceid : int)
      () : unit Lwt.t =
    failwith "not implemented"

  let grab_control ~(impervious : bool) () : unit Lwt.t =
    failwith "not implemented"
end
[@@warning "-27"]

module Xv = struct
  type port = xid

  let decode_port = decode_xid

  type encoding = xid

  let decode_encoding = decode_xid

  type type_mask =
    [ `Input_mask | `Output_mask | `Video_mask | `Still_mask | `Image_mask ]
    list

  type image_format_info_type_enum = [ `Rgb | `Yuv ]

  let image_format_info_type_enum_of_int :
      int -> image_format_info_type_enum option = function
    | 0 -> Some `Rgb
    | 1 -> Some `Yuv
    | _ -> None

  type image_format_info_format_enum = [ `Packed | `Planar ]

  let image_format_info_format_enum_of_int :
      int -> image_format_info_format_enum option = function
    | 0 -> Some `Packed
    | 1 -> Some `Planar
    | _ -> None

  type attribute_flag_mask = [ `Gettable | `Settable ] list

  type video_notify_reason_enum =
    [ `Started | `Stopped | `Busy | `Preempted | `Hard_error ]

  let video_notify_reason_enum_of_int : int -> video_notify_reason_enum option =
    function
    | 0 -> Some `Started
    | 1 -> Some `Stopped
    | 2 -> Some `Busy
    | 3 -> Some `Preempted
    | 4 -> Some `Hard_error
    | _ -> None

  type scanline_order_enum = [ `Top_to_bottom | `Bottom_to_top ]

  let scanline_order_enum_of_int : int -> scanline_order_enum option = function
    | 0 -> Some `Top_to_bottom
    | 1 -> Some `Bottom_to_top
    | _ -> None

  type grab_port_status_enum =
    [ `Success
    | `Bad_extension
    | `Already_grabbed
    | `Invalid_time
    | `Bad_reply
    | `Bad_alloc ]

  let grab_port_status_enum_of_int : int -> grab_port_status_enum option =
    function
    | 0 -> Some `Success
    | 1 -> Some `Bad_extension
    | 2 -> Some `Already_grabbed
    | 3 -> Some `Invalid_time
    | 4 -> Some `Bad_reply
    | 5 -> Some `Bad_alloc
    | _ -> None

  type rational = { numerator : int32; denominator : int32 }

  let decode_rational buf ~at : (rational * int) option =
    let orig = at in
    let* numerator, at = decode_int32 buf ~at in
    let* denominator, at = decode_int32 buf ~at in
    ignore orig;
    Some ({ numerator; denominator }, at)

  type format = { visual : Xproto.visualid; depth : int }

  let decode_format buf ~at : (format * int) option =
    let orig = at in
    let* visual, at = Xproto.decode_visualid buf ~at in
    let* depth, at = decode_uint8 buf ~at in
    let at = at + 3 in
    ignore orig;
    Some ({ visual; depth }, at)

  type adaptor_info = {
    base_id : port;
    num_ports : int;
    type_ : type_mask;
    name : char list;
    formats : format list;
  }

  let decode_adaptor_info buf ~at : (adaptor_info * int) option =
    let orig = at in
    let* base_id, at = decode_port buf ~at in
    let* name_size, at = decode_uint16 buf ~at in
    let name_size = name_size in
    let* num_ports, at = decode_uint16 buf ~at in
    let* num_formats, at = decode_uint16 buf ~at in
    let num_formats = num_formats in
    let* type_, at = decode_uint8 buf ~at in
    let at = at + 1 in
    let* name, at = decode_list decode_char name_size buf ~at in
    let at = at + ((at - orig) mod 4) in
    let* formats, at = decode_list decode_format num_formats buf ~at in
    ignore orig;
    Some ({ base_id; num_ports; type_; name; formats }, at)

  type encoding_info = {
    encoding : encoding;
    width : int;
    height : int;
    rate : rational;
    name : char list;
  }

  let decode_encoding_info buf ~at : (encoding_info * int) option =
    let orig = at in
    let* encoding, at = decode_encoding buf ~at in
    let* name_size, at = decode_uint16 buf ~at in
    let name_size = name_size in
    let* width, at = decode_uint16 buf ~at in
    let* height, at = decode_uint16 buf ~at in
    let at = at + 2 in
    let* rate, at = decode_rational buf ~at in
    let* name, at = decode_list decode_char name_size buf ~at in
    let at = at + ((at - orig) mod 4) in
    ignore orig;
    Some ({ encoding; width; height; rate; name }, at)

  type image = {
    id : int32;
    width : int;
    height : int;
    pitches : int32 list;
    offsets : int32 list;
    data : int list;
  }

  let decode_image buf ~at : (image * int) option =
    let orig = at in
    let* id, at = decode_int32 buf ~at in
    let* width, at = decode_uint16 buf ~at in
    let* height, at = decode_uint16 buf ~at in
    let* data_size, at = decode_int32 buf ~at in
    let data_size = Int32.to_int data_size in
    let data_size = data_size in
    let* num_planes, at = decode_int32 buf ~at in
    let num_planes = Int32.to_int num_planes in
    let num_planes = num_planes in
    let* pitches, at = decode_list decode_int32 num_planes buf ~at in
    let* offsets, at = decode_list decode_int32 num_planes buf ~at in
    let* data, at = decode_list decode_uint8 data_size buf ~at in
    ignore orig;
    Some ({ id; width; height; pitches; offsets; data }, at)

  type attribute_info = {
    flags : attribute_flag_mask;
    min : int32;
    max : int32;
    name : char list;
  }

  let decode_attribute_info buf ~at : (attribute_info * int) option =
    let orig = at in
    let* flags, at = decode_int32 buf ~at in
    let* min, at = decode_int32 buf ~at in
    let* max, at = decode_int32 buf ~at in
    let* size, at = decode_int32 buf ~at in
    let size = Int32.to_int size in
    let size = size in
    let* name, at = decode_list decode_char size buf ~at in
    let at = at + ((at - orig) mod 4) in
    ignore orig;
    Some ({ flags; min; max; name }, at)

  type image_format_info = {
    id : int32;
    type_ : image_format_info_type_enum;
    byte_order : Xproto.image_order_enum;
    guid : int list;
    bpp : int;
    num_planes : int;
    depth : int;
    red_mask : int32;
    green_mask : int32;
    blue_mask : int32;
    format : image_format_info_format_enum;
    y_sample_bits : int32;
    u_sample_bits : int32;
    v_sample_bits : int32;
    vhorz_y_period : int32;
    vhorz_u_period : int32;
    vhorz_v_period : int32;
    vvert_y_period : int32;
    vvert_u_period : int32;
    vvert_v_period : int32;
    vcomp_order : int list;
    vscanline_order : scanline_order_enum;
  }

  let decode_image_format_info buf ~at : (image_format_info * int) option =
    let orig = at in
    let* id, at = decode_int32 buf ~at in
    let* type_, at =
      decode_enum decode_uint8
        (fun x -> x)
        image_format_info_type_enum_of_int buf ~at
    in
    let* byte_order, at =
      decode_enum decode_uint8
        (fun x -> x)
        Xproto.image_order_enum_of_int buf ~at
    in
    let at = at + 2 in
    let* guid, at = decode_list decode_uint8 16 buf ~at in
    let* bpp, at = decode_uint8 buf ~at in
    let* num_planes, at = decode_uint8 buf ~at in
    let at = at + 2 in
    let* depth, at = decode_uint8 buf ~at in
    let at = at + 3 in
    let* red_mask, at = decode_int32 buf ~at in
    let* green_mask, at = decode_int32 buf ~at in
    let* blue_mask, at = decode_int32 buf ~at in
    let* format, at =
      decode_enum decode_uint8
        (fun x -> x)
        image_format_info_format_enum_of_int buf ~at
    in
    let at = at + 3 in
    let* y_sample_bits, at = decode_int32 buf ~at in
    let* u_sample_bits, at = decode_int32 buf ~at in
    let* v_sample_bits, at = decode_int32 buf ~at in
    let* vhorz_y_period, at = decode_int32 buf ~at in
    let* vhorz_u_period, at = decode_int32 buf ~at in
    let* vhorz_v_period, at = decode_int32 buf ~at in
    let* vvert_y_period, at = decode_int32 buf ~at in
    let* vvert_u_period, at = decode_int32 buf ~at in
    let* vvert_v_period, at = decode_int32 buf ~at in
    let* vcomp_order, at = decode_list decode_uint8 32 buf ~at in
    let* vscanline_order, at =
      decode_enum decode_uint8 (fun x -> x) scanline_order_enum_of_int buf ~at
    in
    let at = at + 11 in
    ignore orig;
    Some
      ( {
          id;
          type_;
          byte_order;
          guid;
          bpp;
          num_planes;
          depth;
          red_mask;
          green_mask;
          blue_mask;
          format;
          y_sample_bits;
          u_sample_bits;
          v_sample_bits;
          vhorz_y_period;
          vhorz_u_period;
          vhorz_v_period;
          vvert_y_period;
          vvert_u_period;
          vvert_v_period;
          vcomp_order;
          vscanline_order;
        },
        at )

  type bad_port_error = unit

  type bad_encoding_error = unit

  type bad_control_error = unit

  type video_notify_event = {
    reason : video_notify_reason_enum;
    time : Xproto.timestamp;
    drawable : Xproto.drawable;
    port : port;
  }

  type port_notify_event = {
    time : Xproto.timestamp;
    port : port;
    attribute : Xproto.atom;
    value : int32;
  }

  type query_extension_reply = { major : int; minor : int }

  let query_extension () : query_extension_reply Lwt.t =
    failwith "not implemented"

  type query_adaptors_reply = { info : adaptor_info list }

  let query_adaptors ~(window : Xproto.window) () : query_adaptors_reply Lwt.t =
    failwith "not implemented"

  type query_encodings_reply = { info : encoding_info list }

  let query_encodings ~(port : port) () : query_encodings_reply Lwt.t =
    failwith "not implemented"

  type grab_port_reply = { result : grab_port_status_enum }

  let grab_port ~(port : port)
      ~(time : (Xproto.time_enum, Xproto.timestamp) alt) () :
      grab_port_reply Lwt.t =
    failwith "not implemented"

  let ungrab_port ~(port : port)
      ~(time : (Xproto.time_enum, Xproto.timestamp) alt) () : unit Lwt.t =
    failwith "not implemented"

  let put_video ~(port : port) ~(drawable : Xproto.drawable)
      ~(gc : Xproto.gcontext) ~(vid_x : int) ~(vid_y : int) ~(vid_w : int)
      ~(vid_h : int) ~(drw_x : int) ~(drw_y : int) ~(drw_w : int) ~(drw_h : int)
      () : unit Lwt.t =
    failwith "not implemented"

  let put_still ~(port : port) ~(drawable : Xproto.drawable)
      ~(gc : Xproto.gcontext) ~(vid_x : int) ~(vid_y : int) ~(vid_w : int)
      ~(vid_h : int) ~(drw_x : int) ~(drw_y : int) ~(drw_w : int) ~(drw_h : int)
      () : unit Lwt.t =
    failwith "not implemented"

  let get_video ~(port : port) ~(drawable : Xproto.drawable)
      ~(gc : Xproto.gcontext) ~(vid_x : int) ~(vid_y : int) ~(vid_w : int)
      ~(vid_h : int) ~(drw_x : int) ~(drw_y : int) ~(drw_w : int) ~(drw_h : int)
      () : unit Lwt.t =
    failwith "not implemented"

  let get_still ~(port : port) ~(drawable : Xproto.drawable)
      ~(gc : Xproto.gcontext) ~(vid_x : int) ~(vid_y : int) ~(vid_w : int)
      ~(vid_h : int) ~(drw_x : int) ~(drw_y : int) ~(drw_w : int) ~(drw_h : int)
      () : unit Lwt.t =
    failwith "not implemented"

  let stop_video ~(port : port) ~(drawable : Xproto.drawable) () : unit Lwt.t =
    failwith "not implemented"

  let select_video_notify ~(drawable : Xproto.drawable) ~(onoff : bool) () :
      unit Lwt.t =
    failwith "not implemented"

  let select_port_notify ~(port : port) ~(onoff : bool) () : unit Lwt.t =
    failwith "not implemented"

  type query_best_size_reply = { actual_width : int; actual_height : int }

  let query_best_size ~(port : port) ~(vid_w : int) ~(vid_h : int)
      ~(drw_w : int) ~(drw_h : int) ~(motion : bool) () :
      query_best_size_reply Lwt.t =
    failwith "not implemented"

  let set_port_attribute ~(port : port) ~(attribute : Xproto.atom)
      ~(value : int32) () : unit Lwt.t =
    failwith "not implemented"

  type get_port_attribute_reply = { value : int32 }

  let get_port_attribute ~(port : port) ~(attribute : Xproto.atom) () :
      get_port_attribute_reply Lwt.t =
    failwith "not implemented"

  type query_port_attributes_reply = {
    text_size : int32;
    attributes : attribute_info list;
  }

  let query_port_attributes ~(port : port) () :
      query_port_attributes_reply Lwt.t =
    failwith "not implemented"

  type list_image_formats_reply = { format : image_format_info list }

  let list_image_formats ~(port : port) () : list_image_formats_reply Lwt.t =
    failwith "not implemented"

  type query_image_attributes_reply = {
    data_size : int32;
    width : int;
    height : int;
    pitches : int32 list;
    offsets : int32 list;
  }

  let query_image_attributes ~(port : port) ~(id : int32) ~(width : int)
      ~(height : int) () : query_image_attributes_reply Lwt.t =
    failwith "not implemented"

  let put_image ~(port : port) ~(drawable : Xproto.drawable)
      ~(gc : Xproto.gcontext) ~(id : int32) ~(src_x : int) ~(src_y : int)
      ~(src_w : int) ~(src_h : int) ~(drw_x : int) ~(drw_y : int) ~(drw_w : int)
      ~(drw_h : int) ~(width : int) ~(height : int) ~(data : int list) () :
      unit Lwt.t =
    failwith "not implemented"

  let shm_put_image ~(port : port) ~(drawable : Xproto.drawable)
      ~(gc : Xproto.gcontext) ~(shmseg : Shm.seg) ~(id : int32)
      ~(offset : int32) ~(src_x : int) ~(src_y : int) ~(src_w : int)
      ~(src_h : int) ~(drw_x : int) ~(drw_y : int) ~(drw_w : int) ~(drw_h : int)
      ~(width : int) ~(height : int) ~(send_event : int) () : unit Lwt.t =
    failwith "not implemented"
end
[@@warning "-27"]

module Xvmc = struct
  type context = xid

  let decode_context = decode_xid

  type surface = xid

  let decode_surface = decode_xid

  type subpicture = xid

  let decode_subpicture = decode_xid

  type surface_info = {
    id : surface;
    chroma_format : int;
    pad0 : int;
    max_width : int;
    max_height : int;
    subpicture_max_width : int;
    subpicture_max_height : int;
    mc_type : int32;
    flags : int32;
  }

  let decode_surface_info buf ~at : (surface_info * int) option =
    let orig = at in
    let* id, at = decode_surface buf ~at in
    let* chroma_format, at = decode_uint16 buf ~at in
    let* pad0, at = decode_uint16 buf ~at in
    let* max_width, at = decode_uint16 buf ~at in
    let* max_height, at = decode_uint16 buf ~at in
    let* subpicture_max_width, at = decode_uint16 buf ~at in
    let* subpicture_max_height, at = decode_uint16 buf ~at in
    let* mc_type, at = decode_int32 buf ~at in
    let* flags, at = decode_int32 buf ~at in
    ignore orig;
    Some
      ( {
          id;
          chroma_format;
          pad0;
          max_width;
          max_height;
          subpicture_max_width;
          subpicture_max_height;
          mc_type;
          flags;
        },
        at )

  type query_version_reply = { major : int32; minor : int32 }

  let query_version () : query_version_reply Lwt.t = failwith "not implemented"

  type list_surface_types_reply = { surfaces : surface_info list }

  let list_surface_types ~(port_id : Xv.port) () :
      list_surface_types_reply Lwt.t =
    failwith "not implemented"

  type create_context_reply = {
    width_actual : int;
    height_actual : int;
    flags_return : int32;
    priv_data : int32 list;
  }

  let create_context ~(context_id : context) ~(port_id : Xv.port)
      ~(surface_id : surface) ~(width : int) ~(height : int) ~(flags : int32) ()
      : create_context_reply Lwt.t =
    failwith "not implemented"

  let destroy_context ~(context_id : context) () : unit Lwt.t =
    failwith "not implemented"

  type create_surface_reply = { priv_data : int32 list }

  let create_surface ~(surface_id : surface) ~(context_id : context) () :
      create_surface_reply Lwt.t =
    failwith "not implemented"

  let destroy_surface ~(surface_id : surface) () : unit Lwt.t =
    failwith "not implemented"

  type create_subpicture_reply = {
    width_actual : int;
    height_actual : int;
    num_palette_entries : int;
    entry_bytes : int;
    component_order : int list;
    priv_data : int32 list;
  }

  let create_subpicture ~(subpicture_id : subpicture) ~(context : context)
      ~(xvimage_id : int32) ~(width : int) ~(height : int) () :
      create_subpicture_reply Lwt.t =
    failwith "not implemented"

  let destroy_subpicture ~(subpicture_id : subpicture) () : unit Lwt.t =
    failwith "not implemented"

  type list_subpicture_types_reply = { types : Xv.image_format_info list }

  let list_subpicture_types ~(port_id : Xv.port) ~(surface_id : surface) () :
      list_subpicture_types_reply Lwt.t =
    failwith "not implemented"
end
[@@warning "-27"]
