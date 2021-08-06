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

  let event_mask_mask_flags =
    [
      (`Key_press, 0);
      (`Key_release, 1);
      (`Button_press, 2);
      (`Button_release, 3);
      (`Enter_window, 4);
      (`Leave_window, 5);
      (`Pointer_motion, 6);
      (`Pointer_motion_hint, 7);
      (`Button1_motion, 8);
      (`Button2_motion, 9);
      (`Button3_motion, 10);
      (`Button4_motion, 11);
      (`Button5_motion, 12);
      (`Button_motion, 13);
      (`Keymap_state, 14);
      (`Exposure, 15);
      (`Visibility_change, 16);
      (`Structure_notify, 17);
      (`Resize_redirect, 18);
      (`Substructure_notify, 19);
      (`Substructure_redirect, 20);
      (`Focus_change, 21);
      (`Property_change, 22);
      (`Color_map_change, 23);
      (`Owner_grab_button, 24);
    ]

  let event_mask_mask_values = [ (`No_event, 0) ]

  let decode_event_mask_mask i =
    match List.find_opt (fun (value, v) -> v = i) event_mask_mask_values with
    | Some (value, _) -> V value
    | None ->
        F
          (List.filter_map
             (fun (flag, f) -> if f land i <> 0 then Some flag else None)
             event_mask_mask_flags)

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
    let current_input_masks =
      decode_event_mask_mask (Int32.to_int current_input_masks)
    in
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
          current_input_masks;
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

  let mod_mask_mask_flags =
    [
      (`Shift, 0);
      (`Lock, 1);
      (`Control, 2);
      (`D1, 3);
      (`D2, 4);
      (`D3, 5);
      (`D4, 6);
      (`D5, 7);
      (`Any, 15);
    ]

  let decode_mod_mask_mask i =
    List.filter_map
      (fun (flag, f) -> if f land i <> 0 then Some flag else None)
      mod_mask_mask_flags

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

  let key_but_mask_mask_flags =
    [
      (`Shift, 0);
      (`Lock, 1);
      (`Control, 2);
      (`Mod1, 3);
      (`Mod2, 4);
      (`Mod3, 5);
      (`Mod4, 6);
      (`Mod5, 7);
      (`Button1, 8);
      (`Button2, 9);
      (`Button3, 10);
      (`Button4, 11);
      (`Button5, 12);
    ]

  let decode_key_but_mask_mask i =
    List.filter_map
      (fun (flag, f) -> if f land i <> 0 then Some flag else None)
      key_but_mask_mask_flags

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

  let button_mask_mask_flags =
    [ (`D1, 8); (`D2, 9); (`D3, 10); (`D4, 11); (`D5, 12); (`Any, 15) ]

  let decode_button_mask_mask i =
    List.filter_map
      (fun (flag, f) -> if f land i <> 0 then Some flag else None)
      button_mask_mask_flags

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

  let config_window_mask_flags =
    [
      (`X, 0);
      (`Y, 1);
      (`Width, 2);
      (`Height, 3);
      (`Border_width, 4);
      (`Sibling, 5);
      (`Stack_mode, 6);
    ]

  let decode_config_window_mask i =
    List.filter_map
      (fun (flag, f) -> if f land i <> 0 then Some flag else None)
      config_window_mask_flags

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

  let cw_mask_flags =
    [
      (`Back_pixmap, 0);
      (`Back_pixel, 1);
      (`Border_pixmap, 2);
      (`Border_pixel, 3);
      (`Bit_gravity, 4);
      (`Win_gravity, 5);
      (`Backing_store, 6);
      (`Backing_planes, 7);
      (`Backing_pixel, 8);
      (`Override_redirect, 9);
      (`Save_under, 10);
      (`Event_mask, 11);
      (`Dont_propagate, 12);
      (`Colormap, 13);
      (`Cursor, 14);
    ]

  let decode_cw_mask i =
    List.filter_map
      (fun (flag, f) -> if f land i <> 0 then Some flag else None)
      cw_mask_flags

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

  let gc_mask_flags =
    [
      (`Function, 0);
      (`Plane_mask, 1);
      (`Foreground, 2);
      (`Background, 3);
      (`Line_width, 4);
      (`Line_style, 5);
      (`Cap_style, 6);
      (`Join_style, 7);
      (`Fill_style, 8);
      (`Fill_rule, 9);
      (`Tile, 10);
      (`Stipple, 11);
      (`Tile_stipple_origin_x, 12);
      (`Tile_stipple_origin_y, 13);
      (`Font, 14);
      (`Subwindow_mode, 15);
      (`Graphics_exposures, 16);
      (`Clip_origin_x, 17);
      (`Clip_origin_y, 18);
      (`Clip_mask, 19);
      (`Dash_offset, 20);
      (`Dash_list, 21);
      (`Arc_mode, 22);
    ]

  let decode_gc_mask i =
    List.filter_map
      (fun (flag, f) -> if f land i <> 0 then Some flag else None)
      gc_mask_flags

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

  let color_flag_mask_flags = [ (`Red, 0); (`Green, 1); (`Blue, 2) ]

  let decode_color_flag_mask i =
    List.filter_map
      (fun (flag, f) -> if f land i <> 0 then Some flag else None)
      color_flag_mask_flags

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
    let flags = decode_color_flag_mask (Char.code flags) in
    let at = at + 1 in
    ignore orig;
    Some ({ pixel; red; green; blue; flags }, at)

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

  let kb_mask_flags =
    [
      (`Key_click_percent, 0);
      (`Bell_percent, 1);
      (`Bell_pitch, 2);
      (`Bell_duration, 3);
      (`Led, 4);
      (`Led_mode, 5);
      (`Key, 6);
      (`Auto_repeat_mode, 7);
    ]

  let decode_kb_mask i =
    List.filter_map
      (fun (flag, f) -> if f land i <> 0 then Some flag else None)
      kb_mask_flags

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
