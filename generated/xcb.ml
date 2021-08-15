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

let encode_char buf c = Buffer.add_char buf c

let encode_bool buf b =
  Buffer.add_char buf (if b then Char.chr 1 else Char.chr 0)

let encode_int8 buf i = Buffer.add_int8 buf i

let encode_int16 buf i = Buffer.add_int16_le buf i

let encode_int32 buf i = Buffer.add_int32_le buf i

let encode_int64 buf i = Buffer.add_int64_le buf i

let encode_file_descr buf fd = Buffer.add_int16_le buf (Obj.magic fd)

let encode_uint8 buf i = Buffer.add_uint8 buf i

let encode_uint16 buf i = Buffer.add_uint16_le buf i

let encode_float buf f = Buffer.add_int64_le buf (Int64.bits_of_float f)

let encode_xid = encode_int16

let encode_to_int encode to_int buf x = encode buf (to_int x)

let encode_list encode_item buf items = List.iter (encode_item buf) items

let encode_alt encode to_int encode_t buf = function
  | E e -> encode_to_int encode to_int buf e
  | T t -> encode_t buf t

module Bigreq = struct
  type enable_reply = { maximum_request_length : int32 }

  let enable () : Buffer.t =
    let buf = Buffer.create 16 in
    buf
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

  let encode_char2b buf { byte1; byte2 } =
    encode_uint8 buf byte1;
    encode_uint8 buf byte2;
    ignore buf

  type window = xid

  let decode_window = decode_xid

  let encode_window = encode_xid

  type pixmap = xid

  let decode_pixmap = decode_xid

  let encode_pixmap = encode_xid

  type cursor = xid

  let decode_cursor = decode_xid

  let encode_cursor = encode_xid

  type font = xid

  let decode_font = decode_xid

  let encode_font = encode_xid

  type gcontext = xid

  let decode_gcontext = decode_xid

  let encode_gcontext = encode_xid

  type colormap = xid

  let decode_colormap = decode_xid

  let encode_colormap = encode_xid

  type atom = xid

  let decode_atom = decode_xid

  let encode_atom = encode_xid

  type drawable = xid

  let decode_drawable = decode_xid

  let encode_drawable = encode_xid

  type fontable = xid

  let decode_fontable = decode_xid

  let encode_fontable = encode_xid

  type bool32 = int32

  let decode_bool32 = decode_int32

  let encode_bool32 = encode_int32

  type visualid = int32

  let decode_visualid = decode_int32

  let encode_visualid = encode_int32

  type timestamp = int32

  let decode_timestamp = decode_int32

  let encode_timestamp = encode_int32

  type keysym = int32

  let decode_keysym = decode_int32

  let encode_keysym = encode_int32

  type keycode = int

  let decode_keycode = decode_uint8

  let encode_keycode = encode_uint8

  type keycode32 = int32

  let decode_keycode32 = decode_int32

  let encode_keycode32 = encode_int32

  type button = int

  let decode_button = decode_uint8

  let encode_button = encode_uint8

  type point = { x : int; y : int }

  let decode_point buf ~at : (point * int) option =
    let orig = at in
    let* x, at = decode_int16 buf ~at in
    let* y, at = decode_int16 buf ~at in
    ignore orig;
    Some ({ x; y }, at)

  let encode_point buf { x; y } =
    encode_int16 buf x;
    encode_int16 buf y;
    ignore buf

  type rectangle = { x : int; y : int; width : int; height : int }

  let decode_rectangle buf ~at : (rectangle * int) option =
    let orig = at in
    let* x, at = decode_int16 buf ~at in
    let* y, at = decode_int16 buf ~at in
    let* width, at = decode_uint16 buf ~at in
    let* height, at = decode_uint16 buf ~at in
    ignore orig;
    Some ({ x; y; width; height }, at)

  let encode_rectangle buf { x; y; width; height } =
    encode_int16 buf x;
    encode_int16 buf y;
    encode_uint16 buf width;
    encode_uint16 buf height;
    ignore buf

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

  let encode_arc buf { x; y; width; height; angle1; angle2 } =
    encode_int16 buf x;
    encode_int16 buf y;
    encode_uint16 buf width;
    encode_uint16 buf height;
    encode_int16 buf angle1;
    encode_int16 buf angle2;
    ignore buf

  type format = { depth : int; bits_per_pixel : int; scanline_pad : int }

  let decode_format buf ~at : (format * int) option =
    let orig = at in
    let* depth, at = decode_uint8 buf ~at in
    let* bits_per_pixel, at = decode_uint8 buf ~at in
    let* scanline_pad, at = decode_uint8 buf ~at in
    let at = at + 5 in
    ignore orig;
    Some ({ depth; bits_per_pixel; scanline_pad }, at)

  let encode_format buf { depth; bits_per_pixel; scanline_pad } =
    encode_uint8 buf depth;
    encode_uint8 buf bits_per_pixel;
    encode_uint8 buf scanline_pad;
    ignore buf

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

  let int_of_visual_class_enum : visual_class_enum -> int = function
    | `Static_gray -> 0
    | `Gray_scale -> 1
    | `Static_color -> 2
    | `Pseudo_color -> 3
    | `True_color -> 4
    | `Direct_color -> 5

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

  let encode_visualtype buf
      {
        visual_id;
        class_;
        bits_per_rgb_value;
        colormap_entries;
        red_mask;
        green_mask;
        blue_mask;
      } =
    encode_visualid buf visual_id;
    encode_to_int encode_uint8 int_of_visual_class_enum buf class_;
    encode_uint8 buf bits_per_rgb_value;
    encode_uint16 buf colormap_entries;
    encode_int32 buf red_mask;
    encode_int32 buf green_mask;
    encode_int32 buf blue_mask;
    ignore buf

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

  let encode_depth buf { depth; visuals } =
    encode_uint8 buf depth;
    encode_uint16 buf (List.length visuals);
    encode_list encode_visualtype buf visuals;
    ignore buf

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
             (fun (flag, f) ->
               if f land (1 lsl i) <> 0 then Some flag else None)
             event_mask_mask_flags)

  let int_of_event_mask_mask = function
    | F flags ->
        List.fold_left
          (fun acc flag ->
            let code =
              match flag with
              | `Key_press -> 0
              | `Key_release -> 1
              | `Button_press -> 2
              | `Button_release -> 3
              | `Enter_window -> 4
              | `Leave_window -> 5
              | `Pointer_motion -> 6
              | `Pointer_motion_hint -> 7
              | `Button1_motion -> 8
              | `Button2_motion -> 9
              | `Button3_motion -> 10
              | `Button4_motion -> 11
              | `Button5_motion -> 12
              | `Button_motion -> 13
              | `Keymap_state -> 14
              | `Exposure -> 15
              | `Visibility_change -> 16
              | `Structure_notify -> 17
              | `Resize_redirect -> 18
              | `Substructure_notify -> 19
              | `Substructure_redirect -> 20
              | `Focus_change -> 21
              | `Property_change -> 22
              | `Color_map_change -> 23
              | `Owner_grab_button -> 24
            in
            acc lor (1 lsl code))
          0 flags
    | V `No_event -> 0

  type backing_store_enum = [ `Not_useful | `When_mapped | `Always ]

  let backing_store_enum_of_int : int -> backing_store_enum option = function
    | 0 -> Some `Not_useful
    | 1 -> Some `When_mapped
    | 2 -> Some `Always
    | _ -> None

  let int_of_backing_store_enum : backing_store_enum -> int = function
    | `Not_useful -> 0
    | `When_mapped -> 1
    | `Always -> 2

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

  let encode_screen buf
      {
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
      } =
    encode_window buf root;
    encode_colormap buf default_colormap;
    encode_int32 buf white_pixel;
    encode_int32 buf black_pixel;
    encode_to_int encode_int32
      (fun x -> Int32.of_int (int_of_event_mask_mask x))
      buf current_input_masks;
    encode_uint16 buf width_in_pixels;
    encode_uint16 buf height_in_pixels;
    encode_uint16 buf width_in_millimeters;
    encode_uint16 buf height_in_millimeters;
    encode_uint16 buf min_installed_maps;
    encode_uint16 buf max_installed_maps;
    encode_visualid buf root_visual;
    encode_to_int encode_char
      (fun x -> Char.unsafe_chr (int_of_backing_store_enum x))
      buf backing_stores;
    encode_bool buf save_unders;
    encode_uint8 buf root_depth;
    encode_uint8 buf (List.length allowed_depths);
    encode_list encode_depth buf allowed_depths;
    ignore buf

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

  let encode_setup_request buf
      {
        byte_order;
        protocol_major_version;
        protocol_minor_version;
        authorization_protocol_name;
        authorization_protocol_data;
      } =
    encode_uint8 buf byte_order;
    encode_uint16 buf protocol_major_version;
    encode_uint16 buf protocol_minor_version;
    encode_uint16 buf (List.length authorization_protocol_name);
    encode_uint16 buf (List.length authorization_protocol_data);
    encode_list encode_char buf authorization_protocol_name;
    encode_list encode_char buf authorization_protocol_data;
    ignore buf

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

  let encode_setup_failed buf
      { status; protocol_major_version; protocol_minor_version; length; reason }
      =
    encode_uint8 buf status;
    encode_uint8 buf (List.length reason);
    encode_uint16 buf protocol_major_version;
    encode_uint16 buf protocol_minor_version;
    encode_uint16 buf length;
    encode_list encode_char buf reason;
    ignore buf

  type setup_authenticate = { status : int; length : int; reason : char list }

  let decode_setup_authenticate buf ~at : (setup_authenticate * int) option =
    let orig = at in
    let* status, at = decode_uint8 buf ~at in
    let at = at + 5 in
    let* length, at = decode_uint16 buf ~at in
    let* reason, at = decode_list decode_char (length * 4) buf ~at in
    ignore orig;
    Some ({ status; length; reason }, at)

  let encode_setup_authenticate buf { status; length; reason } =
    encode_uint8 buf status;
    encode_uint16 buf length;
    ignore buf

  type image_order_enum = [ `Lsb_first | `Msb_first ]

  let image_order_enum_of_int : int -> image_order_enum option = function
    | 0 -> Some `Lsb_first
    | 1 -> Some `Msb_first
    | _ -> None

  let int_of_image_order_enum : image_order_enum -> int = function
    | `Lsb_first -> 0
    | `Msb_first -> 1

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

  let encode_setup buf
      {
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
      } =
    encode_uint8 buf status;
    encode_uint16 buf protocol_major_version;
    encode_uint16 buf protocol_minor_version;
    encode_uint16 buf length;
    encode_int32 buf release_number;
    encode_int32 buf resource_id_base;
    encode_int32 buf resource_id_mask;
    encode_int32 buf motion_buffer_size;
    encode_uint16 buf (List.length vendor);
    encode_uint16 buf maximum_request_length;
    encode_uint8 buf (List.length roots);
    encode_uint8 buf (List.length pixmap_formats);
    encode_to_int encode_uint8 int_of_image_order_enum buf image_byte_order;
    encode_to_int encode_uint8 int_of_image_order_enum buf
      bitmap_format_bit_order;
    encode_uint8 buf bitmap_format_scanline_unit;
    encode_uint8 buf bitmap_format_scanline_pad;
    encode_keycode buf min_keycode;
    encode_keycode buf max_keycode;
    encode_list encode_char buf vendor;
    encode_list encode_format buf pixmap_formats;
    encode_list encode_screen buf roots;
    ignore buf

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
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      mod_mask_mask_flags

  let int_of_mod_mask_mask flags =
    List.fold_left
      (fun acc flag ->
        let code =
          match flag with
          | `Shift -> 0
          | `Lock -> 1
          | `Control -> 2
          | `D1 -> 3
          | `D2 -> 4
          | `D3 -> 5
          | `D4 -> 6
          | `D5 -> 7
          | `Any -> 15
        in
        acc lor (1 lsl code))
      0 flags

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
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      key_but_mask_mask_flags

  let int_of_key_but_mask_mask flags =
    List.fold_left
      (fun acc flag ->
        let code =
          match flag with
          | `Shift -> 0
          | `Lock -> 1
          | `Control -> 2
          | `Mod1 -> 3
          | `Mod2 -> 4
          | `Mod3 -> 5
          | `Mod4 -> 6
          | `Mod5 -> 7
          | `Button1 -> 8
          | `Button2 -> 9
          | `Button3 -> 10
          | `Button4 -> 11
          | `Button5 -> 12
        in
        acc lor (1 lsl code))
      0 flags

  type window_enum = [ `None ]

  let window_enum_of_int : int -> window_enum option = function
    | 0 -> Some `None
    | _ -> None

  let int_of_window_enum : window_enum -> int = function `None -> 0

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
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      button_mask_mask_flags

  let int_of_button_mask_mask flags =
    List.fold_left
      (fun acc flag ->
        let code =
          match flag with
          | `D1 -> 8
          | `D2 -> 9
          | `D3 -> 10
          | `D4 -> 11
          | `D5 -> 12
          | `Any -> 15
        in
        acc lor (1 lsl code))
      0 flags

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

  let int_of_motion_enum : motion_enum -> int = function
    | `Normal -> 0
    | `Hint -> 1

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

  let int_of_notify_detail_enum : notify_detail_enum -> int = function
    | `Ancestor -> 0
    | `Virtual -> 1
    | `Inferior -> 2
    | `Nonlinear -> 3
    | `Nonlinear_virtual -> 4
    | `Pointer -> 5
    | `Pointer_root -> 6
    | `None -> 7

  type notify_mode_enum = [ `Normal | `Grab | `Ungrab | `While_grabbed ]

  let notify_mode_enum_of_int : int -> notify_mode_enum option = function
    | 0 -> Some `Normal
    | 1 -> Some `Grab
    | 2 -> Some `Ungrab
    | 3 -> Some `While_grabbed
    | _ -> None

  let int_of_notify_mode_enum : notify_mode_enum -> int = function
    | `Normal -> 0
    | `Grab -> 1
    | `Ungrab -> 2
    | `While_grabbed -> 3

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

  let int_of_visibility_enum : visibility_enum -> int = function
    | `Unobscured -> 0
    | `Partially_obscured -> 1
    | `Fully_obscured -> 2

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

  let int_of_stack_mode_enum : stack_mode_enum -> int = function
    | `Above -> 0
    | `Below -> 1
    | `Top_if -> 2
    | `Bottom_if -> 3
    | `Opposite -> 4

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
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      config_window_mask_flags

  let int_of_config_window_mask flags =
    List.fold_left
      (fun acc flag ->
        let code =
          match flag with
          | `X -> 0
          | `Y -> 1
          | `Width -> 2
          | `Height -> 3
          | `Border_width -> 4
          | `Sibling -> 5
          | `Stack_mode -> 6
        in
        acc lor (1 lsl code))
      0 flags

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

  let int_of_place_enum : place_enum -> int = function
    | `On_top -> 0
    | `On_bottom -> 1

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

  let int_of_property_enum : property_enum -> int = function
    | `New_value -> 0
    | `Delete -> 1

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

  let int_of_time_enum : time_enum -> int = function `Current_time -> 0

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

  let int_of_atom_enum : atom_enum -> int = function
    | `None_any -> 0
    | `Primary -> 1
    | `Secondary -> 2
    | `Arc -> 3
    | `Atom -> 4
    | `Bitmap -> 5
    | `Cardinal -> 6
    | `Colormap -> 7
    | `Cursor -> 8
    | `Cut_buffer0 -> 9
    | `Cut_buffer1 -> 10
    | `Cut_buffer2 -> 11
    | `Cut_buffer3 -> 12
    | `Cut_buffer4 -> 13
    | `Cut_buffer5 -> 14
    | `Cut_buffer6 -> 15
    | `Cut_buffer7 -> 16
    | `Drawable -> 17
    | `Font -> 18
    | `Integer -> 19
    | `Pixmap -> 20
    | `Point -> 21
    | `Rectangle -> 22
    | `Resource_manager -> 23
    | `Rgb_color_map -> 24
    | `Rgb_best_map -> 25
    | `Rgb_blue_map -> 26
    | `Rgb_default_map -> 27
    | `Rgb_gray_map -> 28
    | `Rgb_green_map -> 29
    | `Rgb_red_map -> 30
    | `String -> 31
    | `Visualid -> 32
    | `Window -> 33
    | `Wm_command -> 34
    | `Wm_hints -> 35
    | `Wm_client_machine -> 36
    | `Wm_icon_name -> 37
    | `Wm_icon_size -> 38
    | `Wm_name -> 39
    | `Wm_normal_hints -> 40
    | `Wm_size_hints -> 41
    | `Wm_zoom_hints -> 42
    | `Min_space -> 43
    | `Norm_space -> 44
    | `Max_space -> 45
    | `End_space -> 46
    | `Superscript_x -> 47
    | `Superscript_y -> 48
    | `Subscript_x -> 49
    | `Subscript_y -> 50
    | `Underline_position -> 51
    | `Underline_thickness -> 52
    | `Strikeout_ascent -> 53
    | `Strikeout_descent -> 54
    | `Italic_angle -> 55
    | `X_height -> 56
    | `Quad_width -> 57
    | `Weight -> 58
    | `Point_size -> 59
    | `Resolution -> 60
    | `Copyright -> 61
    | `Notice -> 62
    | `Font_name -> 63
    | `Family_name -> 64
    | `Full_name -> 65
    | `Cap_height -> 66
    | `Wm_class -> 67
    | `Wm_transient_for -> 68

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

  let int_of_colormap_state_enum : colormap_state_enum -> int = function
    | `Uninstalled -> 0
    | `Installed -> 1

  type colormap_enum = [ `None ]

  let colormap_enum_of_int : int -> colormap_enum option = function
    | 0 -> Some `None
    | _ -> None

  let int_of_colormap_enum : colormap_enum -> int = function `None -> 0

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

  let int_of_client_message_data_format_enum :
      client_message_data_format_enum -> int = function
    | `Data8 -> 8
    | `Data16 -> 16
    | `Data32 -> 32

  type client_message_data_format_variant =
    | Data8 of { data8 : int list }
    | Data16 of { data16 : int list }
    | Data32 of { data32 : int32 list }

  let decode_client_message_data_format_variant buf ~at enum :
      (client_message_data_format_variant * int) option =
    let decode_data8 buf ~at : (client_message_data_format_variant * int) option
        =
      let orig = at in
      let* data8, at = decode_list decode_uint8 20 buf ~at in
      ignore orig;
      Some (Data8 { data8 }, at)
    in
    let decode_data16 buf ~at :
        (client_message_data_format_variant * int) option =
      let orig = at in
      let* data16, at = decode_list decode_uint16 10 buf ~at in
      ignore orig;
      Some (Data16 { data16 }, at)
    in
    let decode_data32 buf ~at :
        (client_message_data_format_variant * int) option =
      let orig = at in
      let* data32, at = decode_list decode_int32 5 buf ~at in
      ignore orig;
      Some (Data32 { data32 }, at)
    in
    match enum with
    | 8 -> decode_data8 buf ~at
    | 16 -> decode_data16 buf ~at
    | 32 -> decode_data32 buf ~at
    | invalid ->
        failwith
          (Printf.sprintf "Invalid enum tag %d for client_message_data_format "
             invalid)

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

  let int_of_mapping_enum : mapping_enum -> int = function
    | `Modifier -> 0
    | `Keyboard -> 1
    | `Pointer -> 2

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

  let int_of_window_class_enum : window_class_enum -> int = function
    | `Copy_from_parent -> 0
    | `Input_output -> 1
    | `Input_only -> 2

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
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      cw_mask_flags

  let int_of_cw_mask flags =
    List.fold_left
      (fun acc flag ->
        let code =
          match flag with
          | `Back_pixmap -> 0
          | `Back_pixel -> 1
          | `Border_pixmap -> 2
          | `Border_pixel -> 3
          | `Bit_gravity -> 4
          | `Win_gravity -> 5
          | `Backing_store -> 6
          | `Backing_planes -> 7
          | `Backing_pixel -> 8
          | `Override_redirect -> 9
          | `Save_under -> 10
          | `Event_mask -> 11
          | `Dont_propagate -> 12
          | `Colormap -> 13
          | `Cursor -> 14
        in
        acc lor (1 lsl code))
      0 flags

  type back_pixmap_enum = [ `None | `Parent_relative ]

  let back_pixmap_enum_of_int : int -> back_pixmap_enum option = function
    | 0 -> Some `None
    | 1 -> Some `Parent_relative
    | _ -> None

  let int_of_back_pixmap_enum : back_pixmap_enum -> int = function
    | `None -> 0
    | `Parent_relative -> 1

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

  let int_of_gravity_enum : gravity_enum -> int = function
    | `Bit_forget_win_unmap -> 0
    | `North_west -> 1
    | `North -> 2
    | `North_east -> 3
    | `West -> 4
    | `Center -> 5
    | `East -> 6
    | `South_west -> 7
    | `South -> 8
    | `South_east -> 9
    | `Static -> 10

  type pixmap_enum = [ `None ]

  let pixmap_enum_of_int : int -> pixmap_enum option = function
    | 0 -> Some `None
    | _ -> None

  let int_of_pixmap_enum : pixmap_enum -> int = function `None -> 0

  type cursor_enum = [ `None ]

  let cursor_enum_of_int : int -> cursor_enum option = function
    | 0 -> Some `None
    | _ -> None

  let int_of_cursor_enum : cursor_enum -> int = function `None -> 0

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
      ?(cursor : (cursor_enum, cursor) alt option) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint8 buf depth;
    encode_window buf wid;
    encode_window buf parent;
    encode_int16 buf x;
    encode_int16 buf y;
    encode_uint16 buf width;
    encode_uint16 buf height;
    encode_uint16 buf border_width;
    encode_to_int encode_uint16 int_of_window_class_enum buf class_;
    encode_visualid buf visual;
    let value_mask = 0 in
    let value_mask =
      if Option.is_some background_pixmap then value_mask lor (1 lsl 0)
      else value_mask
    in
    let value_mask =
      if Option.is_some background_pixel then value_mask lor (1 lsl 1)
      else value_mask
    in
    let value_mask =
      if Option.is_some border_pixmap then value_mask lor (1 lsl 2)
      else value_mask
    in
    let value_mask =
      if Option.is_some border_pixel then value_mask lor (1 lsl 3)
      else value_mask
    in
    let value_mask =
      if Option.is_some bit_gravity then value_mask lor (1 lsl 4)
      else value_mask
    in
    let value_mask =
      if Option.is_some win_gravity then value_mask lor (1 lsl 5)
      else value_mask
    in
    let value_mask =
      if Option.is_some backing_store then value_mask lor (1 lsl 6)
      else value_mask
    in
    let value_mask =
      if Option.is_some backing_planes then value_mask lor (1 lsl 7)
      else value_mask
    in
    let value_mask =
      if Option.is_some backing_pixel then value_mask lor (1 lsl 8)
      else value_mask
    in
    let value_mask =
      if Option.is_some override_redirect then value_mask lor (1 lsl 9)
      else value_mask
    in
    let value_mask =
      if Option.is_some save_under then value_mask lor (1 lsl 10)
      else value_mask
    in
    let value_mask =
      if Option.is_some event_mask then value_mask lor (1 lsl 11)
      else value_mask
    in
    let value_mask =
      if Option.is_some do_not_propogate_mask then value_mask lor (1 lsl 12)
      else value_mask
    in
    let value_mask =
      if Option.is_some colormap then value_mask lor (1 lsl 13) else value_mask
    in
    let value_mask =
      if Option.is_some cursor then value_mask lor (1 lsl 14) else value_mask
    in
    encode_int32 buf (Int32.of_int value_mask);
    Option.iter
      (fun background_pixmap ->
        encode_alt encode_pixmap int_of_back_pixmap_enum encode_pixmap buf
          background_pixmap)
      background_pixmap;
    Option.iter
      (fun background_pixel -> encode_int32 buf background_pixel)
      background_pixel;
    Option.iter
      (fun border_pixmap ->
        encode_alt encode_pixmap int_of_pixmap_enum encode_pixmap buf
          border_pixmap)
      border_pixmap;
    Option.iter (fun border_pixel -> encode_int32 buf border_pixel) border_pixel;
    Option.iter
      (fun bit_gravity ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_gravity_enum x))
          buf bit_gravity)
      bit_gravity;
    Option.iter
      (fun win_gravity ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_gravity_enum x))
          buf win_gravity)
      win_gravity;
    Option.iter
      (fun backing_store ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_backing_store_enum x))
          buf backing_store)
      backing_store;
    Option.iter
      (fun backing_planes -> encode_int32 buf backing_planes)
      backing_planes;
    Option.iter
      (fun backing_pixel -> encode_int32 buf backing_pixel)
      backing_pixel;
    Option.iter
      (fun override_redirect -> encode_bool32 buf override_redirect)
      override_redirect;
    Option.iter (fun save_under -> encode_bool32 buf save_under) save_under;
    Option.iter
      (fun event_mask ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_event_mask_mask x))
          buf event_mask)
      event_mask;
    Option.iter
      (fun do_not_propogate_mask ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_event_mask_mask x))
          buf do_not_propogate_mask)
      do_not_propogate_mask;
    Option.iter
      (fun colormap ->
        encode_alt encode_colormap int_of_colormap_enum encode_colormap buf
          colormap)
      colormap;
    Option.iter
      (fun cursor ->
        encode_alt encode_cursor int_of_cursor_enum encode_cursor buf cursor)
      cursor;
    buf

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
      ?(cursor : (cursor_enum, cursor) alt option) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_window buf window;
    let value_mask = 0 in
    let value_mask =
      if Option.is_some background_pixmap then value_mask lor (1 lsl 0)
      else value_mask
    in
    let value_mask =
      if Option.is_some background_pixel then value_mask lor (1 lsl 1)
      else value_mask
    in
    let value_mask =
      if Option.is_some border_pixmap then value_mask lor (1 lsl 2)
      else value_mask
    in
    let value_mask =
      if Option.is_some border_pixel then value_mask lor (1 lsl 3)
      else value_mask
    in
    let value_mask =
      if Option.is_some bit_gravity then value_mask lor (1 lsl 4)
      else value_mask
    in
    let value_mask =
      if Option.is_some win_gravity then value_mask lor (1 lsl 5)
      else value_mask
    in
    let value_mask =
      if Option.is_some backing_store then value_mask lor (1 lsl 6)
      else value_mask
    in
    let value_mask =
      if Option.is_some backing_planes then value_mask lor (1 lsl 7)
      else value_mask
    in
    let value_mask =
      if Option.is_some backing_pixel then value_mask lor (1 lsl 8)
      else value_mask
    in
    let value_mask =
      if Option.is_some override_redirect then value_mask lor (1 lsl 9)
      else value_mask
    in
    let value_mask =
      if Option.is_some save_under then value_mask lor (1 lsl 10)
      else value_mask
    in
    let value_mask =
      if Option.is_some event_mask then value_mask lor (1 lsl 11)
      else value_mask
    in
    let value_mask =
      if Option.is_some do_not_propogate_mask then value_mask lor (1 lsl 12)
      else value_mask
    in
    let value_mask =
      if Option.is_some colormap then value_mask lor (1 lsl 13) else value_mask
    in
    let value_mask =
      if Option.is_some cursor then value_mask lor (1 lsl 14) else value_mask
    in
    encode_int32 buf (Int32.of_int value_mask);
    Option.iter
      (fun background_pixmap ->
        encode_alt encode_pixmap int_of_back_pixmap_enum encode_pixmap buf
          background_pixmap)
      background_pixmap;
    Option.iter
      (fun background_pixel -> encode_int32 buf background_pixel)
      background_pixel;
    Option.iter
      (fun border_pixmap ->
        encode_alt encode_pixmap int_of_pixmap_enum encode_pixmap buf
          border_pixmap)
      border_pixmap;
    Option.iter (fun border_pixel -> encode_int32 buf border_pixel) border_pixel;
    Option.iter
      (fun bit_gravity ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_gravity_enum x))
          buf bit_gravity)
      bit_gravity;
    Option.iter
      (fun win_gravity ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_gravity_enum x))
          buf win_gravity)
      win_gravity;
    Option.iter
      (fun backing_store ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_backing_store_enum x))
          buf backing_store)
      backing_store;
    Option.iter
      (fun backing_planes -> encode_int32 buf backing_planes)
      backing_planes;
    Option.iter
      (fun backing_pixel -> encode_int32 buf backing_pixel)
      backing_pixel;
    Option.iter
      (fun override_redirect -> encode_bool32 buf override_redirect)
      override_redirect;
    Option.iter (fun save_under -> encode_bool32 buf save_under) save_under;
    Option.iter
      (fun event_mask ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_event_mask_mask x))
          buf event_mask)
      event_mask;
    Option.iter
      (fun do_not_propogate_mask ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_event_mask_mask x))
          buf do_not_propogate_mask)
      do_not_propogate_mask;
    Option.iter
      (fun colormap ->
        encode_alt encode_colormap int_of_colormap_enum encode_colormap buf
          colormap)
      colormap;
    Option.iter
      (fun cursor ->
        encode_alt encode_cursor int_of_cursor_enum encode_cursor buf cursor)
      cursor;
    buf

  type map_state_enum = [ `Unmapped | `Unviewable | `Viewable ]

  let map_state_enum_of_int : int -> map_state_enum option = function
    | 0 -> Some `Unmapped
    | 1 -> Some `Unviewable
    | 2 -> Some `Viewable
    | _ -> None

  let int_of_map_state_enum : map_state_enum -> int = function
    | `Unmapped -> 0
    | `Unviewable -> 1
    | `Viewable -> 2

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

  let get_window_attributes ~(window : window) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_window buf window;
    buf

  let destroy_window ~(window : window) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_window buf window;
    buf

  let destroy_subwindows ~(window : window) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_window buf window;
    buf

  type set_mode_enum = [ `Insert | `Delete ]

  let set_mode_enum_of_int : int -> set_mode_enum option = function
    | 0 -> Some `Insert
    | 1 -> Some `Delete
    | _ -> None

  let int_of_set_mode_enum : set_mode_enum -> int = function
    | `Insert -> 0
    | `Delete -> 1

  let change_save_set ~(mode : set_mode_enum) ~(window : window) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_char
      (fun x -> Char.unsafe_chr (int_of_set_mode_enum x))
      buf mode;
    encode_window buf window;
    buf

  let reparent_window ~(window : window) ~(parent : window) ~(x : int)
      ~(y : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_window buf window;
    encode_window buf parent;
    encode_int16 buf x;
    encode_int16 buf y;
    buf

  let map_window ~(window : window) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_window buf window;
    buf

  let map_subwindows ~(window : window) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_window buf window;
    buf

  let unmap_window ~(window : window) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_window buf window;
    buf

  let unmap_subwindows ~(window : window) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_window buf window;
    buf

  let configure_window ~(window : window) ?(x : int32 option)
      ?(y : int32 option) ?(width : int32 option) ?(height : int32 option)
      ?(border_width : int32 option)
      ?(sibling : (window_enum, window) alt option)
      ?(stack_mode : stack_mode_enum option) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_window buf window;
    let value_mask = 0 in
    let value_mask =
      if Option.is_some x then value_mask lor (1 lsl 0) else value_mask
    in
    let value_mask =
      if Option.is_some y then value_mask lor (1 lsl 1) else value_mask
    in
    let value_mask =
      if Option.is_some width then value_mask lor (1 lsl 2) else value_mask
    in
    let value_mask =
      if Option.is_some height then value_mask lor (1 lsl 3) else value_mask
    in
    let value_mask =
      if Option.is_some border_width then value_mask lor (1 lsl 4)
      else value_mask
    in
    let value_mask =
      if Option.is_some sibling then value_mask lor (1 lsl 5) else value_mask
    in
    let value_mask =
      if Option.is_some stack_mode then value_mask lor (1 lsl 6) else value_mask
    in
    encode_uint16 buf value_mask;
    Option.iter (fun x -> encode_int32 buf x) x;
    Option.iter (fun y -> encode_int32 buf y) y;
    Option.iter (fun width -> encode_int32 buf width) width;
    Option.iter (fun height -> encode_int32 buf height) height;
    Option.iter (fun border_width -> encode_int32 buf border_width) border_width;
    Option.iter
      (fun sibling ->
        encode_alt encode_window int_of_window_enum encode_window buf sibling)
      sibling;
    Option.iter
      (fun stack_mode ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_stack_mode_enum x))
          buf stack_mode)
      stack_mode;
    buf

  type circulate_enum = [ `Raise_lowest | `Lower_highest ]

  let circulate_enum_of_int : int -> circulate_enum option = function
    | 0 -> Some `Raise_lowest
    | 1 -> Some `Lower_highest
    | _ -> None

  let int_of_circulate_enum : circulate_enum -> int = function
    | `Raise_lowest -> 0
    | `Lower_highest -> 1

  let circulate_window ~(direction : circulate_enum) ~(window : window) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_uint8 int_of_circulate_enum buf direction;
    encode_window buf window;
    buf

  type get_geometry_reply = {
    depth : int;
    root : window;
    x : int;
    y : int;
    width : int;
    height : int;
    border_width : int;
  }

  let get_geometry ~(drawable : drawable) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_drawable buf drawable;
    buf

  type query_tree_reply = {
    root : window;
    parent : (window_enum, window) alt;
    children : window list;
  }

  let query_tree ~(window : window) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_window buf window;
    buf

  type intern_atom_reply = { atom : (atom_enum, atom) alt }

  let intern_atom ~(only_if_exists : bool) ~(name : char list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_bool buf only_if_exists;
    encode_uint16 buf (List.length name);
    encode_list encode_char buf name;
    buf

  type get_atom_name_reply = { name : char list }

  let get_atom_name ~(atom : atom) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_atom buf atom;
    buf

  type prop_mode_enum = [ `Replace | `Prepend | `Append ]

  let prop_mode_enum_of_int : int -> prop_mode_enum option = function
    | 0 -> Some `Replace
    | 1 -> Some `Prepend
    | 2 -> Some `Append
    | _ -> None

  let int_of_prop_mode_enum : prop_mode_enum -> int = function
    | `Replace -> 0
    | `Prepend -> 1
    | `Append -> 2

  let change_property ~(mode : prop_mode_enum) ~(window : window)
      ~(property : atom) ~(type_ : atom) ~(format : int) ~(data_len : int32)
      ~(data : char list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_uint8 int_of_prop_mode_enum buf mode;
    encode_window buf window;
    encode_atom buf property;
    encode_atom buf type_;
    encode_uint8 buf format;
    encode_int32 buf data_len;
    buf

  let delete_property ~(window : window) ~(property : atom) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_window buf window;
    encode_atom buf property;
    buf

  type get_property_type_enum = [ `Any ]

  let get_property_type_enum_of_int : int -> get_property_type_enum option =
    function
    | 0 -> Some `Any
    | _ -> None

  let int_of_get_property_type_enum : get_property_type_enum -> int = function
    | `Any -> 0

  type get_property_reply = {
    format : int;
    type_ : atom;
    bytes_after : int32;
    value_len : int32;
    value : char list;
  }

  let get_property ~(delete : bool) ~(window : window) ~(property : atom)
      ~(type_ : (get_property_type_enum, atom) alt) ~(long_offset : int32)
      ~(long_length : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_bool buf delete;
    encode_window buf window;
    encode_atom buf property;
    encode_alt encode_atom int_of_get_property_type_enum encode_atom buf type_;
    encode_int32 buf long_offset;
    encode_int32 buf long_length;
    buf

  type list_properties_reply = { atoms : atom list }

  let list_properties ~(window : window) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_window buf window;
    buf

  let set_selection_owner ~(owner : (window_enum, window) alt)
      ~(selection : atom) ~(time : (time_enum, timestamp) alt) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_alt encode_window int_of_window_enum encode_window buf owner;
    encode_atom buf selection;
    encode_alt encode_timestamp
      (fun x -> Int32.of_int (int_of_time_enum x))
      encode_timestamp buf time;
    buf

  type get_selection_owner_reply = { owner : (window_enum, window) alt }

  let get_selection_owner ~(selection : atom) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_atom buf selection;
    buf

  let convert_selection ~(requestor : window) ~(selection : atom)
      ~(target : atom) ~(property : (atom_enum, atom) alt)
      ~(time : (time_enum, timestamp) alt) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_window buf requestor;
    encode_atom buf selection;
    encode_atom buf target;
    encode_alt encode_atom int_of_atom_enum encode_atom buf property;
    encode_alt encode_timestamp
      (fun x -> Int32.of_int (int_of_time_enum x))
      encode_timestamp buf time;
    buf

  type send_event_dest_enum = [ `Pointer_window | `Item_focus ]

  let send_event_dest_enum_of_int : int -> send_event_dest_enum option =
    function
    | 0 -> Some `Pointer_window
    | 1 -> Some `Item_focus
    | _ -> None

  let int_of_send_event_dest_enum : send_event_dest_enum -> int = function
    | `Pointer_window -> 0
    | `Item_focus -> 1

  let send_event ~(propagate : bool)
      ~(destination : (send_event_dest_enum, window) alt)
      ~(event_mask : event_mask_mask) ~(event : char list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_bool buf propagate;
    encode_alt encode_window int_of_send_event_dest_enum encode_window buf
      destination;
    encode_to_int encode_int32
      (fun x -> Int32.of_int (int_of_event_mask_mask x))
      buf event_mask;
    buf

  type grab_mode_enum = [ `Sync | `Async ]

  let grab_mode_enum_of_int : int -> grab_mode_enum option = function
    | 0 -> Some `Sync
    | 1 -> Some `Async
    | _ -> None

  let int_of_grab_mode_enum : grab_mode_enum -> int = function
    | `Sync -> 0
    | `Async -> 1

  type grab_status_enum =
    [ `Success | `Already_grabbed | `Invalid_time | `Not_viewable | `Frozen ]

  let grab_status_enum_of_int : int -> grab_status_enum option = function
    | 0 -> Some `Success
    | 1 -> Some `Already_grabbed
    | 2 -> Some `Invalid_time
    | 3 -> Some `Not_viewable
    | 4 -> Some `Frozen
    | _ -> None

  let int_of_grab_status_enum : grab_status_enum -> int = function
    | `Success -> 0
    | `Already_grabbed -> 1
    | `Invalid_time -> 2
    | `Not_viewable -> 3
    | `Frozen -> 4

  type grab_pointer_reply = { status : grab_status_enum }

  let grab_pointer ~(owner_events : bool) ~(grab_window : window)
      ~(event_mask : event_mask_mask) ~(pointer_mode : grab_mode_enum)
      ~(keyboard_mode : grab_mode_enum)
      ~(confine_to : (window_enum, window) alt)
      ~(cursor : (cursor_enum, cursor) alt) ~(time : (time_enum, timestamp) alt)
      () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_bool buf owner_events;
    encode_window buf grab_window;
    encode_to_int encode_uint16 int_of_event_mask_mask buf event_mask;
    encode_to_int encode_char
      (fun x -> Char.unsafe_chr (int_of_grab_mode_enum x))
      buf pointer_mode;
    encode_to_int encode_char
      (fun x -> Char.unsafe_chr (int_of_grab_mode_enum x))
      buf keyboard_mode;
    encode_alt encode_window int_of_window_enum encode_window buf confine_to;
    encode_alt encode_cursor int_of_cursor_enum encode_cursor buf cursor;
    encode_alt encode_timestamp
      (fun x -> Int32.of_int (int_of_time_enum x))
      encode_timestamp buf time;
    buf

  let ungrab_pointer ~(time : (time_enum, timestamp) alt) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_alt encode_timestamp
      (fun x -> Int32.of_int (int_of_time_enum x))
      encode_timestamp buf time;
    buf

  type button_index_enum = [ `Any | `D1 | `D2 | `D3 | `D4 | `D5 ]

  let button_index_enum_of_int : int -> button_index_enum option = function
    | 0 -> Some `Any
    | 1 -> Some `D1
    | 2 -> Some `D2
    | 3 -> Some `D3
    | 4 -> Some `D4
    | 5 -> Some `D5
    | _ -> None

  let int_of_button_index_enum : button_index_enum -> int = function
    | `Any -> 0
    | `D1 -> 1
    | `D2 -> 2
    | `D3 -> 3
    | `D4 -> 4
    | `D5 -> 5

  let grab_button ~(owner_events : bool) ~(grab_window : window)
      ~(event_mask : event_mask_mask) ~(pointer_mode : grab_mode_enum)
      ~(keyboard_mode : grab_mode_enum)
      ~(confine_to : (window_enum, window) alt)
      ~(cursor : (cursor_enum, cursor) alt) ~(button : button_index_enum)
      ~(modifiers : mod_mask_mask) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_bool buf owner_events;
    encode_window buf grab_window;
    encode_to_int encode_uint16 int_of_event_mask_mask buf event_mask;
    encode_to_int encode_uint8 int_of_grab_mode_enum buf pointer_mode;
    encode_to_int encode_uint8 int_of_grab_mode_enum buf keyboard_mode;
    encode_alt encode_window int_of_window_enum encode_window buf confine_to;
    encode_alt encode_cursor int_of_cursor_enum encode_cursor buf cursor;
    encode_to_int encode_uint8 int_of_button_index_enum buf button;
    encode_to_int encode_uint16 int_of_mod_mask_mask buf modifiers;
    buf

  let ungrab_button ~(button : button_index_enum) ~(grab_window : window)
      ~(modifiers : mod_mask_mask) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_uint8 int_of_button_index_enum buf button;
    encode_window buf grab_window;
    encode_to_int encode_uint16 int_of_mod_mask_mask buf modifiers;
    buf

  let change_active_pointer_grab ~(cursor : (cursor_enum, cursor) alt)
      ~(time : (time_enum, timestamp) alt) ~(event_mask : event_mask_mask) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_alt encode_cursor int_of_cursor_enum encode_cursor buf cursor;
    encode_alt encode_timestamp
      (fun x -> Int32.of_int (int_of_time_enum x))
      encode_timestamp buf time;
    encode_to_int encode_uint16 int_of_event_mask_mask buf event_mask;
    buf

  type grab_keyboard_reply = { status : grab_status_enum }

  let grab_keyboard ~(owner_events : bool) ~(grab_window : window)
      ~(time : (time_enum, timestamp) alt) ~(pointer_mode : grab_mode_enum)
      ~(keyboard_mode : grab_mode_enum) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_bool buf owner_events;
    encode_window buf grab_window;
    encode_alt encode_timestamp
      (fun x -> Int32.of_int (int_of_time_enum x))
      encode_timestamp buf time;
    encode_to_int encode_char
      (fun x -> Char.unsafe_chr (int_of_grab_mode_enum x))
      buf pointer_mode;
    encode_to_int encode_char
      (fun x -> Char.unsafe_chr (int_of_grab_mode_enum x))
      buf keyboard_mode;
    buf

  let ungrab_keyboard ~(time : (time_enum, timestamp) alt) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_alt encode_timestamp
      (fun x -> Int32.of_int (int_of_time_enum x))
      encode_timestamp buf time;
    buf

  type grab_enum = [ `Any ]

  let grab_enum_of_int : int -> grab_enum option = function
    | 0 -> Some `Any
    | _ -> None

  let int_of_grab_enum : grab_enum -> int = function `Any -> 0

  let grab_key ~(owner_events : bool) ~(grab_window : window)
      ~(modifiers : mod_mask_mask) ~(key : (grab_enum, keycode) alt)
      ~(pointer_mode : grab_mode_enum) ~(keyboard_mode : grab_mode_enum) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_bool buf owner_events;
    encode_window buf grab_window;
    encode_to_int encode_uint16 int_of_mod_mask_mask buf modifiers;
    encode_alt encode_keycode int_of_grab_enum encode_keycode buf key;
    encode_to_int encode_uint8 int_of_grab_mode_enum buf pointer_mode;
    encode_to_int encode_uint8 int_of_grab_mode_enum buf keyboard_mode;
    buf

  let ungrab_key ~(key : (grab_enum, keycode) alt) ~(grab_window : window)
      ~(modifiers : mod_mask_mask) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_alt encode_keycode int_of_grab_enum encode_keycode buf key;
    encode_window buf grab_window;
    encode_to_int encode_uint16 int_of_mod_mask_mask buf modifiers;
    buf

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

  let int_of_allow_enum : allow_enum -> int = function
    | `Async_pointer -> 0
    | `Sync_pointer -> 1
    | `Replay_pointer -> 2
    | `Async_keyboard -> 3
    | `Sync_keyboard -> 4
    | `Replay_keyboard -> 5
    | `Async_both -> 6
    | `Sync_both -> 7

  let allow_events ~(mode : allow_enum) ~(time : (time_enum, timestamp) alt) ()
      : Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_uint8 int_of_allow_enum buf mode;
    encode_alt encode_timestamp
      (fun x -> Int32.of_int (int_of_time_enum x))
      encode_timestamp buf time;
    buf

  let grab_server () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  let ungrab_server () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

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

  let query_pointer ~(window : window) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_window buf window;
    buf

  type timecoord = { time : timestamp; x : int; y : int }

  let decode_timecoord buf ~at : (timecoord * int) option =
    let orig = at in
    let* time, at = decode_timestamp buf ~at in
    let* x, at = decode_int16 buf ~at in
    let* y, at = decode_int16 buf ~at in
    ignore orig;
    Some ({ time; x; y }, at)

  let encode_timecoord buf { time; x; y } =
    encode_timestamp buf time;
    encode_int16 buf x;
    encode_int16 buf y;
    ignore buf

  type get_motion_events_reply = { events : timecoord list }

  let get_motion_events ~(window : window) ~(start : (time_enum, timestamp) alt)
      ~(stop : (time_enum, timestamp) alt) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_window buf window;
    encode_alt encode_timestamp
      (fun x -> Int32.of_int (int_of_time_enum x))
      encode_timestamp buf start;
    encode_alt encode_timestamp
      (fun x -> Int32.of_int (int_of_time_enum x))
      encode_timestamp buf stop;
    buf

  type translate_coordinates_reply = {
    same_screen : bool;
    child : (window_enum, window) alt;
    dst_x : int;
    dst_y : int;
  }

  let translate_coordinates ~(src_window : window) ~(dst_window : window)
      ~(src_x : int) ~(src_y : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_window buf src_window;
    encode_window buf dst_window;
    encode_int16 buf src_x;
    encode_int16 buf src_y;
    buf

  let warp_pointer ~(src_window : (window_enum, window) alt)
      ~(dst_window : (window_enum, window) alt) ~(src_x : int) ~(src_y : int)
      ~(src_width : int) ~(src_height : int) ~(dst_x : int) ~(dst_y : int) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_alt encode_window int_of_window_enum encode_window buf src_window;
    encode_alt encode_window int_of_window_enum encode_window buf dst_window;
    encode_int16 buf src_x;
    encode_int16 buf src_y;
    encode_uint16 buf src_width;
    encode_uint16 buf src_height;
    encode_int16 buf dst_x;
    encode_int16 buf dst_y;
    buf

  type input_focus_enum = [ `None | `Pointer_root | `Parent | `Follow_keyboard ]

  let input_focus_enum_of_int : int -> input_focus_enum option = function
    | 0 -> Some `None
    | 1 -> Some `Pointer_root
    | 2 -> Some `Parent
    | 3 -> Some `Follow_keyboard
    | _ -> None

  let int_of_input_focus_enum : input_focus_enum -> int = function
    | `None -> 0
    | `Pointer_root -> 1
    | `Parent -> 2
    | `Follow_keyboard -> 3

  let set_input_focus ~(revert_to : input_focus_enum)
      ~(focus : (input_focus_enum, window) alt)
      ~(time : (time_enum, timestamp) alt) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_uint8 int_of_input_focus_enum buf revert_to;
    encode_alt encode_window int_of_input_focus_enum encode_window buf focus;
    encode_alt encode_timestamp
      (fun x -> Int32.of_int (int_of_time_enum x))
      encode_timestamp buf time;
    buf

  type get_input_focus_reply = {
    revert_to : input_focus_enum;
    focus : (input_focus_enum, window) alt;
  }

  let get_input_focus () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  type query_keymap_reply = { keys : int list }

  let query_keymap () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  let open_font ~(fid : font) ~(name : char list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_font buf fid;
    encode_uint16 buf (List.length name);
    encode_list encode_char buf name;
    buf

  let close_font ~(font : font) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_font buf font;
    buf

  type font_draw_enum = [ `Left_to_right | `Right_to_left ]

  let font_draw_enum_of_int : int -> font_draw_enum option = function
    | 0 -> Some `Left_to_right
    | 1 -> Some `Right_to_left
    | _ -> None

  let int_of_font_draw_enum : font_draw_enum -> int = function
    | `Left_to_right -> 0
    | `Right_to_left -> 1

  type fontprop = { name : atom; value : int32 }

  let decode_fontprop buf ~at : (fontprop * int) option =
    let orig = at in
    let* name, at = decode_atom buf ~at in
    let* value, at = decode_int32 buf ~at in
    ignore orig;
    Some ({ name; value }, at)

  let encode_fontprop buf { name; value } =
    encode_atom buf name;
    encode_int32 buf value;
    ignore buf

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

  let encode_charinfo buf
      {
        left_side_bearing;
        right_side_bearing;
        character_width;
        ascent;
        descent;
        attributes;
      } =
    encode_int16 buf left_side_bearing;
    encode_int16 buf right_side_bearing;
    encode_int16 buf character_width;
    encode_int16 buf ascent;
    encode_int16 buf descent;
    encode_uint16 buf attributes;
    ignore buf

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

  let query_font ~(font : fontable) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_fontable buf font;
    buf

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
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_fontable buf font;
    buf

  type str = { name : char list }

  let decode_str buf ~at : (str * int) option =
    let orig = at in
    let* name_len, at = decode_uint8 buf ~at in
    let name_len = name_len in
    let* name, at = decode_list decode_char name_len buf ~at in
    ignore orig;
    Some ({ name }, at)

  let encode_str buf { name } =
    encode_uint8 buf (List.length name);
    encode_list encode_char buf name;
    ignore buf

  type list_fonts_reply = { names : str list }

  let list_fonts ~(max_names : int) ~(pattern : char list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint16 buf max_names;
    encode_uint16 buf (List.length pattern);
    encode_list encode_char buf pattern;
    buf

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
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint16 buf max_names;
    encode_uint16 buf (List.length pattern);
    encode_list encode_char buf pattern;
    buf

  let set_font_path ~(font : str list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint16 buf (List.length font);
    encode_list encode_str buf font;
    buf

  type get_font_path_reply = { path : str list }

  let get_font_path () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  let create_pixmap ~(depth : int) ~(pid : pixmap) ~(drawable : drawable)
      ~(width : int) ~(height : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint8 buf depth;
    encode_pixmap buf pid;
    encode_drawable buf drawable;
    encode_uint16 buf width;
    encode_uint16 buf height;
    buf

  let free_pixmap ~(pixmap : pixmap) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_pixmap buf pixmap;
    buf

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
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      gc_mask_flags

  let int_of_gc_mask flags =
    List.fold_left
      (fun acc flag ->
        let code =
          match flag with
          | `Function -> 0
          | `Plane_mask -> 1
          | `Foreground -> 2
          | `Background -> 3
          | `Line_width -> 4
          | `Line_style -> 5
          | `Cap_style -> 6
          | `Join_style -> 7
          | `Fill_style -> 8
          | `Fill_rule -> 9
          | `Tile -> 10
          | `Stipple -> 11
          | `Tile_stipple_origin_x -> 12
          | `Tile_stipple_origin_y -> 13
          | `Font -> 14
          | `Subwindow_mode -> 15
          | `Graphics_exposures -> 16
          | `Clip_origin_x -> 17
          | `Clip_origin_y -> 18
          | `Clip_mask -> 19
          | `Dash_offset -> 20
          | `Dash_list -> 21
          | `Arc_mode -> 22
        in
        acc lor (1 lsl code))
      0 flags

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

  let int_of_gx_enum : gx_enum -> int = function
    | `Clear -> 0
    | `And -> 1
    | `And_reverse -> 2
    | `Copy -> 3
    | `And_inverted -> 4
    | `Noop -> 5
    | `Xor -> 6
    | `Or -> 7
    | `Nor -> 8
    | `Equiv -> 9
    | `Invert -> 10
    | `Or_reverse -> 11
    | `Copy_inverted -> 12
    | `Or_inverted -> 13
    | `Nand -> 14
    | `Set -> 15

  type line_style_enum = [ `Solid | `On_off_dash | `Double_dash ]

  let line_style_enum_of_int : int -> line_style_enum option = function
    | 0 -> Some `Solid
    | 1 -> Some `On_off_dash
    | 2 -> Some `Double_dash
    | _ -> None

  let int_of_line_style_enum : line_style_enum -> int = function
    | `Solid -> 0
    | `On_off_dash -> 1
    | `Double_dash -> 2

  type cap_style_enum = [ `Not_last | `Butt | `Round | `Projecting ]

  let cap_style_enum_of_int : int -> cap_style_enum option = function
    | 0 -> Some `Not_last
    | 1 -> Some `Butt
    | 2 -> Some `Round
    | 3 -> Some `Projecting
    | _ -> None

  let int_of_cap_style_enum : cap_style_enum -> int = function
    | `Not_last -> 0
    | `Butt -> 1
    | `Round -> 2
    | `Projecting -> 3

  type join_style_enum = [ `Miter | `Round | `Bevel ]

  let join_style_enum_of_int : int -> join_style_enum option = function
    | 0 -> Some `Miter
    | 1 -> Some `Round
    | 2 -> Some `Bevel
    | _ -> None

  let int_of_join_style_enum : join_style_enum -> int = function
    | `Miter -> 0
    | `Round -> 1
    | `Bevel -> 2

  type fill_style_enum = [ `Solid | `Tiled | `Stippled | `Opaque_stippled ]

  let fill_style_enum_of_int : int -> fill_style_enum option = function
    | 0 -> Some `Solid
    | 1 -> Some `Tiled
    | 2 -> Some `Stippled
    | 3 -> Some `Opaque_stippled
    | _ -> None

  let int_of_fill_style_enum : fill_style_enum -> int = function
    | `Solid -> 0
    | `Tiled -> 1
    | `Stippled -> 2
    | `Opaque_stippled -> 3

  type fill_rule_enum = [ `Even_odd | `Winding ]

  let fill_rule_enum_of_int : int -> fill_rule_enum option = function
    | 0 -> Some `Even_odd
    | 1 -> Some `Winding
    | _ -> None

  let int_of_fill_rule_enum : fill_rule_enum -> int = function
    | `Even_odd -> 0
    | `Winding -> 1

  type subwindow_mode_enum = [ `Clip_by_children | `Include_inferiors ]

  let subwindow_mode_enum_of_int : int -> subwindow_mode_enum option = function
    | 0 -> Some `Clip_by_children
    | 1 -> Some `Include_inferiors
    | _ -> None

  let int_of_subwindow_mode_enum : subwindow_mode_enum -> int = function
    | `Clip_by_children -> 0
    | `Include_inferiors -> 1

  type arc_mode_enum = [ `Chord | `Pie_slice ]

  let arc_mode_enum_of_int : int -> arc_mode_enum option = function
    | 0 -> Some `Chord
    | 1 -> Some `Pie_slice
    | _ -> None

  let int_of_arc_mode_enum : arc_mode_enum -> int = function
    | `Chord -> 0
    | `Pie_slice -> 1

  type font_enum = [ `None ]

  let font_enum_of_int : int -> font_enum option = function
    | 0 -> Some `None
    | _ -> None

  let int_of_font_enum : font_enum -> int = function `None -> 0

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
      ?(arc_mode : arc_mode_enum option) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_gcontext buf cid;
    encode_drawable buf drawable;
    let value_mask = 0 in
    let value_mask =
      if Option.is_some function_ then value_mask lor (1 lsl 0) else value_mask
    in
    let value_mask =
      if Option.is_some plane_mask then value_mask lor (1 lsl 1) else value_mask
    in
    let value_mask =
      if Option.is_some foreground then value_mask lor (1 lsl 2) else value_mask
    in
    let value_mask =
      if Option.is_some background then value_mask lor (1 lsl 3) else value_mask
    in
    let value_mask =
      if Option.is_some line_width then value_mask lor (1 lsl 4) else value_mask
    in
    let value_mask =
      if Option.is_some line_style then value_mask lor (1 lsl 5) else value_mask
    in
    let value_mask =
      if Option.is_some cap_style then value_mask lor (1 lsl 6) else value_mask
    in
    let value_mask =
      if Option.is_some join_style then value_mask lor (1 lsl 7) else value_mask
    in
    let value_mask =
      if Option.is_some fill_style then value_mask lor (1 lsl 8) else value_mask
    in
    let value_mask =
      if Option.is_some fill_rule then value_mask lor (1 lsl 9) else value_mask
    in
    let value_mask =
      if Option.is_some tile then value_mask lor (1 lsl 10) else value_mask
    in
    let value_mask =
      if Option.is_some stipple then value_mask lor (1 lsl 11) else value_mask
    in
    let value_mask =
      if Option.is_some tile_stipple_x_origin then value_mask lor (1 lsl 12)
      else value_mask
    in
    let value_mask =
      if Option.is_some tile_stipple_y_origin then value_mask lor (1 lsl 13)
      else value_mask
    in
    let value_mask =
      if Option.is_some font then value_mask lor (1 lsl 14) else value_mask
    in
    let value_mask =
      if Option.is_some subwindow_mode then value_mask lor (1 lsl 15)
      else value_mask
    in
    let value_mask =
      if Option.is_some graphics_exposures then value_mask lor (1 lsl 16)
      else value_mask
    in
    let value_mask =
      if Option.is_some clip_x_origin then value_mask lor (1 lsl 17)
      else value_mask
    in
    let value_mask =
      if Option.is_some clip_y_origin then value_mask lor (1 lsl 18)
      else value_mask
    in
    let value_mask =
      if Option.is_some clip_mask then value_mask lor (1 lsl 19) else value_mask
    in
    let value_mask =
      if Option.is_some dash_offset then value_mask lor (1 lsl 20)
      else value_mask
    in
    let value_mask =
      if Option.is_some dashes then value_mask lor (1 lsl 21) else value_mask
    in
    let value_mask =
      if Option.is_some arc_mode then value_mask lor (1 lsl 22) else value_mask
    in
    encode_int32 buf (Int32.of_int value_mask);
    Option.iter
      (fun function_ ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_gx_enum x))
          buf function_)
      function_;
    Option.iter (fun plane_mask -> encode_int32 buf plane_mask) plane_mask;
    Option.iter (fun foreground -> encode_int32 buf foreground) foreground;
    Option.iter (fun background -> encode_int32 buf background) background;
    Option.iter (fun line_width -> encode_int32 buf line_width) line_width;
    Option.iter
      (fun line_style ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_line_style_enum x))
          buf line_style)
      line_style;
    Option.iter
      (fun cap_style ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_cap_style_enum x))
          buf cap_style)
      cap_style;
    Option.iter
      (fun join_style ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_join_style_enum x))
          buf join_style)
      join_style;
    Option.iter
      (fun fill_style ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_fill_style_enum x))
          buf fill_style)
      fill_style;
    Option.iter
      (fun fill_rule ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_fill_rule_enum x))
          buf fill_rule)
      fill_rule;
    Option.iter
      (fun tile ->
        encode_alt encode_pixmap int_of_pixmap_enum encode_pixmap buf tile)
      tile;
    Option.iter
      (fun stipple ->
        encode_alt encode_pixmap int_of_pixmap_enum encode_pixmap buf stipple)
      stipple;
    Option.iter
      (fun tile_stipple_x_origin -> encode_int32 buf tile_stipple_x_origin)
      tile_stipple_x_origin;
    Option.iter
      (fun tile_stipple_y_origin -> encode_int32 buf tile_stipple_y_origin)
      tile_stipple_y_origin;
    Option.iter
      (fun font -> encode_alt encode_font int_of_font_enum encode_font buf font)
      font;
    Option.iter
      (fun subwindow_mode ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_subwindow_mode_enum x))
          buf subwindow_mode)
      subwindow_mode;
    Option.iter
      (fun graphics_exposures -> encode_bool32 buf graphics_exposures)
      graphics_exposures;
    Option.iter
      (fun clip_x_origin -> encode_int32 buf clip_x_origin)
      clip_x_origin;
    Option.iter
      (fun clip_y_origin -> encode_int32 buf clip_y_origin)
      clip_y_origin;
    Option.iter
      (fun clip_mask ->
        encode_alt encode_pixmap int_of_pixmap_enum encode_pixmap buf clip_mask)
      clip_mask;
    Option.iter (fun dash_offset -> encode_int32 buf dash_offset) dash_offset;
    Option.iter (fun dashes -> encode_int32 buf dashes) dashes;
    Option.iter
      (fun arc_mode ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_arc_mode_enum x))
          buf arc_mode)
      arc_mode;
    buf

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
      ?(arc_mode : arc_mode_enum option) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_gcontext buf gc;
    let value_mask = 0 in
    let value_mask =
      if Option.is_some function_ then value_mask lor (1 lsl 0) else value_mask
    in
    let value_mask =
      if Option.is_some plane_mask then value_mask lor (1 lsl 1) else value_mask
    in
    let value_mask =
      if Option.is_some foreground then value_mask lor (1 lsl 2) else value_mask
    in
    let value_mask =
      if Option.is_some background then value_mask lor (1 lsl 3) else value_mask
    in
    let value_mask =
      if Option.is_some line_width then value_mask lor (1 lsl 4) else value_mask
    in
    let value_mask =
      if Option.is_some line_style then value_mask lor (1 lsl 5) else value_mask
    in
    let value_mask =
      if Option.is_some cap_style then value_mask lor (1 lsl 6) else value_mask
    in
    let value_mask =
      if Option.is_some join_style then value_mask lor (1 lsl 7) else value_mask
    in
    let value_mask =
      if Option.is_some fill_style then value_mask lor (1 lsl 8) else value_mask
    in
    let value_mask =
      if Option.is_some fill_rule then value_mask lor (1 lsl 9) else value_mask
    in
    let value_mask =
      if Option.is_some tile then value_mask lor (1 lsl 10) else value_mask
    in
    let value_mask =
      if Option.is_some stipple then value_mask lor (1 lsl 11) else value_mask
    in
    let value_mask =
      if Option.is_some tile_stipple_x_origin then value_mask lor (1 lsl 12)
      else value_mask
    in
    let value_mask =
      if Option.is_some tile_stipple_y_origin then value_mask lor (1 lsl 13)
      else value_mask
    in
    let value_mask =
      if Option.is_some font then value_mask lor (1 lsl 14) else value_mask
    in
    let value_mask =
      if Option.is_some subwindow_mode then value_mask lor (1 lsl 15)
      else value_mask
    in
    let value_mask =
      if Option.is_some graphics_exposures then value_mask lor (1 lsl 16)
      else value_mask
    in
    let value_mask =
      if Option.is_some clip_x_origin then value_mask lor (1 lsl 17)
      else value_mask
    in
    let value_mask =
      if Option.is_some clip_y_origin then value_mask lor (1 lsl 18)
      else value_mask
    in
    let value_mask =
      if Option.is_some clip_mask then value_mask lor (1 lsl 19) else value_mask
    in
    let value_mask =
      if Option.is_some dash_offset then value_mask lor (1 lsl 20)
      else value_mask
    in
    let value_mask =
      if Option.is_some dashes then value_mask lor (1 lsl 21) else value_mask
    in
    let value_mask =
      if Option.is_some arc_mode then value_mask lor (1 lsl 22) else value_mask
    in
    encode_int32 buf (Int32.of_int value_mask);
    Option.iter
      (fun function_ ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_gx_enum x))
          buf function_)
      function_;
    Option.iter (fun plane_mask -> encode_int32 buf plane_mask) plane_mask;
    Option.iter (fun foreground -> encode_int32 buf foreground) foreground;
    Option.iter (fun background -> encode_int32 buf background) background;
    Option.iter (fun line_width -> encode_int32 buf line_width) line_width;
    Option.iter
      (fun line_style ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_line_style_enum x))
          buf line_style)
      line_style;
    Option.iter
      (fun cap_style ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_cap_style_enum x))
          buf cap_style)
      cap_style;
    Option.iter
      (fun join_style ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_join_style_enum x))
          buf join_style)
      join_style;
    Option.iter
      (fun fill_style ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_fill_style_enum x))
          buf fill_style)
      fill_style;
    Option.iter
      (fun fill_rule ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_fill_rule_enum x))
          buf fill_rule)
      fill_rule;
    Option.iter
      (fun tile ->
        encode_alt encode_pixmap int_of_pixmap_enum encode_pixmap buf tile)
      tile;
    Option.iter
      (fun stipple ->
        encode_alt encode_pixmap int_of_pixmap_enum encode_pixmap buf stipple)
      stipple;
    Option.iter
      (fun tile_stipple_x_origin -> encode_int32 buf tile_stipple_x_origin)
      tile_stipple_x_origin;
    Option.iter
      (fun tile_stipple_y_origin -> encode_int32 buf tile_stipple_y_origin)
      tile_stipple_y_origin;
    Option.iter
      (fun font -> encode_alt encode_font int_of_font_enum encode_font buf font)
      font;
    Option.iter
      (fun subwindow_mode ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_subwindow_mode_enum x))
          buf subwindow_mode)
      subwindow_mode;
    Option.iter
      (fun graphics_exposures -> encode_bool32 buf graphics_exposures)
      graphics_exposures;
    Option.iter
      (fun clip_x_origin -> encode_int32 buf clip_x_origin)
      clip_x_origin;
    Option.iter
      (fun clip_y_origin -> encode_int32 buf clip_y_origin)
      clip_y_origin;
    Option.iter
      (fun clip_mask ->
        encode_alt encode_pixmap int_of_pixmap_enum encode_pixmap buf clip_mask)
      clip_mask;
    Option.iter (fun dash_offset -> encode_int32 buf dash_offset) dash_offset;
    Option.iter (fun dashes -> encode_int32 buf dashes) dashes;
    Option.iter
      (fun arc_mode ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_arc_mode_enum x))
          buf arc_mode)
      arc_mode;
    buf

  let copy_gc ~(src_gc : gcontext) ~(dst_gc : gcontext) ~(value_mask : gc_mask)
      () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_gcontext buf src_gc;
    encode_gcontext buf dst_gc;
    encode_to_int encode_int32
      (fun x -> Int32.of_int (int_of_gc_mask x))
      buf value_mask;
    buf

  let set_dashes ~(gc : gcontext) ~(dash_offset : int) ~(dashes : int list) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_gcontext buf gc;
    encode_uint16 buf dash_offset;
    encode_uint16 buf (List.length dashes);
    encode_list encode_uint8 buf dashes;
    buf

  type clip_ordering_enum = [ `Unsorted | `Y_sorted | `Yx_sorted | `Yx_banded ]

  let clip_ordering_enum_of_int : int -> clip_ordering_enum option = function
    | 0 -> Some `Unsorted
    | 1 -> Some `Y_sorted
    | 2 -> Some `Yx_sorted
    | 3 -> Some `Yx_banded
    | _ -> None

  let int_of_clip_ordering_enum : clip_ordering_enum -> int = function
    | `Unsorted -> 0
    | `Y_sorted -> 1
    | `Yx_sorted -> 2
    | `Yx_banded -> 3

  let set_clip_rectangles ~(ordering : clip_ordering_enum) ~(gc : gcontext)
      ~(clip_x_origin : int) ~(clip_y_origin : int)
      ~(rectangles : rectangle list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_char
      (fun x -> Char.unsafe_chr (int_of_clip_ordering_enum x))
      buf ordering;
    encode_gcontext buf gc;
    encode_int16 buf clip_x_origin;
    encode_int16 buf clip_y_origin;
    buf

  let free_gc ~(gc : gcontext) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_gcontext buf gc;
    buf

  let clear_area ~(exposures : bool) ~(window : window) ~(x : int) ~(y : int)
      ~(width : int) ~(height : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_bool buf exposures;
    encode_window buf window;
    encode_int16 buf x;
    encode_int16 buf y;
    encode_uint16 buf width;
    encode_uint16 buf height;
    buf

  let copy_area ~(src_drawable : drawable) ~(dst_drawable : drawable)
      ~(gc : gcontext) ~(src_x : int) ~(src_y : int) ~(dst_x : int)
      ~(dst_y : int) ~(width : int) ~(height : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_drawable buf src_drawable;
    encode_drawable buf dst_drawable;
    encode_gcontext buf gc;
    encode_int16 buf src_x;
    encode_int16 buf src_y;
    encode_int16 buf dst_x;
    encode_int16 buf dst_y;
    encode_uint16 buf width;
    encode_uint16 buf height;
    buf

  let copy_plane ~(src_drawable : drawable) ~(dst_drawable : drawable)
      ~(gc : gcontext) ~(src_x : int) ~(src_y : int) ~(dst_x : int)
      ~(dst_y : int) ~(width : int) ~(height : int) ~(bit_plane : int32) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_drawable buf src_drawable;
    encode_drawable buf dst_drawable;
    encode_gcontext buf gc;
    encode_int16 buf src_x;
    encode_int16 buf src_y;
    encode_int16 buf dst_x;
    encode_int16 buf dst_y;
    encode_uint16 buf width;
    encode_uint16 buf height;
    encode_int32 buf bit_plane;
    buf

  type coord_mode_enum = [ `Origin | `Previous ]

  let coord_mode_enum_of_int : int -> coord_mode_enum option = function
    | 0 -> Some `Origin
    | 1 -> Some `Previous
    | _ -> None

  let int_of_coord_mode_enum : coord_mode_enum -> int = function
    | `Origin -> 0
    | `Previous -> 1

  let poly_point ~(coordinate_mode : coord_mode_enum) ~(drawable : drawable)
      ~(gc : gcontext) ~(points : point list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_char
      (fun x -> Char.unsafe_chr (int_of_coord_mode_enum x))
      buf coordinate_mode;
    encode_drawable buf drawable;
    encode_gcontext buf gc;
    buf

  let poly_line ~(coordinate_mode : coord_mode_enum) ~(drawable : drawable)
      ~(gc : gcontext) ~(points : point list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_char
      (fun x -> Char.unsafe_chr (int_of_coord_mode_enum x))
      buf coordinate_mode;
    encode_drawable buf drawable;
    encode_gcontext buf gc;
    buf

  type segment = { x1 : int; y1 : int; x2 : int; y2 : int }

  let decode_segment buf ~at : (segment * int) option =
    let orig = at in
    let* x1, at = decode_int16 buf ~at in
    let* y1, at = decode_int16 buf ~at in
    let* x2, at = decode_int16 buf ~at in
    let* y2, at = decode_int16 buf ~at in
    ignore orig;
    Some ({ x1; y1; x2; y2 }, at)

  let encode_segment buf { x1; y1; x2; y2 } =
    encode_int16 buf x1;
    encode_int16 buf y1;
    encode_int16 buf x2;
    encode_int16 buf y2;
    ignore buf

  let poly_segment ~(drawable : drawable) ~(gc : gcontext)
      ~(segments : segment list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_drawable buf drawable;
    encode_gcontext buf gc;
    buf

  let poly_rectangle ~(drawable : drawable) ~(gc : gcontext)
      ~(rectangles : rectangle list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_drawable buf drawable;
    encode_gcontext buf gc;
    buf

  let poly_arc ~(drawable : drawable) ~(gc : gcontext) ~(arcs : arc list) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_drawable buf drawable;
    encode_gcontext buf gc;
    buf

  type poly_shape_enum = [ `Complex | `Nonconvex | `Convex ]

  let poly_shape_enum_of_int : int -> poly_shape_enum option = function
    | 0 -> Some `Complex
    | 1 -> Some `Nonconvex
    | 2 -> Some `Convex
    | _ -> None

  let int_of_poly_shape_enum : poly_shape_enum -> int = function
    | `Complex -> 0
    | `Nonconvex -> 1
    | `Convex -> 2

  let fill_poly ~(drawable : drawable) ~(gc : gcontext)
      ~(shape : poly_shape_enum) ~(coordinate_mode : coord_mode_enum)
      ~(points : point list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_drawable buf drawable;
    encode_gcontext buf gc;
    encode_to_int encode_uint8 int_of_poly_shape_enum buf shape;
    encode_to_int encode_uint8 int_of_coord_mode_enum buf coordinate_mode;
    buf

  let poly_fill_rectangle ~(drawable : drawable) ~(gc : gcontext)
      ~(rectangles : rectangle list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_drawable buf drawable;
    encode_gcontext buf gc;
    buf

  let poly_fill_arc ~(drawable : drawable) ~(gc : gcontext) ~(arcs : arc list)
      () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_drawable buf drawable;
    encode_gcontext buf gc;
    buf

  type image_format_enum = [ `Xy_bitmap | `Xy_pixmap | `Z_pixmap ]

  let image_format_enum_of_int : int -> image_format_enum option = function
    | 0 -> Some `Xy_bitmap
    | 1 -> Some `Xy_pixmap
    | 2 -> Some `Z_pixmap
    | _ -> None

  let int_of_image_format_enum : image_format_enum -> int = function
    | `Xy_bitmap -> 0
    | `Xy_pixmap -> 1
    | `Z_pixmap -> 2

  let put_image ~(format : image_format_enum) ~(drawable : drawable)
      ~(gc : gcontext) ~(width : int) ~(height : int) ~(dst_x : int)
      ~(dst_y : int) ~(left_pad : int) ~(depth : int) ~(data : char list) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_uint8 int_of_image_format_enum buf format;
    encode_drawable buf drawable;
    encode_gcontext buf gc;
    encode_uint16 buf width;
    encode_uint16 buf height;
    encode_int16 buf dst_x;
    encode_int16 buf dst_y;
    encode_uint8 buf left_pad;
    encode_uint8 buf depth;
    buf

  type get_image_reply = { depth : int; visual : visualid; data : char list }

  let get_image ~(format : image_format_enum) ~(drawable : drawable) ~(x : int)
      ~(y : int) ~(width : int) ~(height : int) ~(plane_mask : int32) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_uint8 int_of_image_format_enum buf format;
    encode_drawable buf drawable;
    encode_int16 buf x;
    encode_int16 buf y;
    encode_uint16 buf width;
    encode_uint16 buf height;
    encode_int32 buf plane_mask;
    buf

  let poly_text8 ~(drawable : drawable) ~(gc : gcontext) ~(x : int) ~(y : int)
      ~(items : char list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_drawable buf drawable;
    encode_gcontext buf gc;
    encode_int16 buf x;
    encode_int16 buf y;
    buf

  let poly_text16 ~(drawable : drawable) ~(gc : gcontext) ~(x : int) ~(y : int)
      ~(items : char list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_drawable buf drawable;
    encode_gcontext buf gc;
    encode_int16 buf x;
    encode_int16 buf y;
    buf

  let image_text8 ~(drawable : drawable) ~(gc : gcontext) ~(x : int) ~(y : int)
      ~(string : char list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_char buf (Char.unsafe_chr (List.length string));
    encode_drawable buf drawable;
    encode_gcontext buf gc;
    encode_int16 buf x;
    encode_int16 buf y;
    encode_list encode_char buf string;
    buf

  let image_text16 ~(drawable : drawable) ~(gc : gcontext) ~(x : int) ~(y : int)
      ~(string : char2b list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_char buf (Char.unsafe_chr (List.length string));
    encode_drawable buf drawable;
    encode_gcontext buf gc;
    encode_int16 buf x;
    encode_int16 buf y;
    encode_list encode_char2b buf string;
    buf

  type colormap_alloc_enum = [ `None | `All ]

  let colormap_alloc_enum_of_int : int -> colormap_alloc_enum option = function
    | 0 -> Some `None
    | 1 -> Some `All
    | _ -> None

  let int_of_colormap_alloc_enum : colormap_alloc_enum -> int = function
    | `None -> 0
    | `All -> 1

  let create_colormap ~(alloc : colormap_alloc_enum) ~(mid : colormap)
      ~(window : window) ~(visual : visualid) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_char
      (fun x -> Char.unsafe_chr (int_of_colormap_alloc_enum x))
      buf alloc;
    encode_colormap buf mid;
    encode_window buf window;
    encode_visualid buf visual;
    buf

  let free_colormap ~(cmap : colormap) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_colormap buf cmap;
    buf

  let copy_colormap_and_free ~(mid : colormap) ~(src_cmap : colormap) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_colormap buf mid;
    encode_colormap buf src_cmap;
    buf

  let install_colormap ~(cmap : colormap) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_colormap buf cmap;
    buf

  let uninstall_colormap ~(cmap : colormap) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_colormap buf cmap;
    buf

  type list_installed_colormaps_reply = { cmaps : colormap list }

  let list_installed_colormaps ~(window : window) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_window buf window;
    buf

  type alloc_color_reply = { red : int; green : int; blue : int; pixel : int32 }

  let alloc_color ~(cmap : colormap) ~(red : int) ~(green : int) ~(blue : int)
      () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_colormap buf cmap;
    encode_uint16 buf red;
    encode_uint16 buf green;
    encode_uint16 buf blue;
    buf

  type alloc_named_color_reply = {
    pixel : int32;
    exact_red : int;
    exact_green : int;
    exact_blue : int;
    visual_red : int;
    visual_green : int;
    visual_blue : int;
  }

  let alloc_named_color ~(cmap : colormap) ~(name : char list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_colormap buf cmap;
    encode_uint16 buf (List.length name);
    encode_list encode_char buf name;
    buf

  type alloc_color_cells_reply = { pixels : int32 list; masks : int32 list }

  let alloc_color_cells ~(contiguous : bool) ~(cmap : colormap) ~(colors : int)
      ~(planes : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_bool buf contiguous;
    encode_colormap buf cmap;
    encode_uint16 buf colors;
    encode_uint16 buf planes;
    buf

  type alloc_color_planes_reply = {
    red_mask : int32;
    green_mask : int32;
    blue_mask : int32;
    pixels : int32 list;
  }

  let alloc_color_planes ~(contiguous : bool) ~(cmap : colormap) ~(colors : int)
      ~(reds : int) ~(greens : int) ~(blues : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_bool buf contiguous;
    encode_colormap buf cmap;
    encode_uint16 buf colors;
    encode_uint16 buf reds;
    encode_uint16 buf greens;
    encode_uint16 buf blues;
    buf

  let free_colors ~(cmap : colormap) ~(plane_mask : int32)
      ~(pixels : int32 list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_colormap buf cmap;
    encode_int32 buf plane_mask;
    buf

  type color_flag_mask = [ `Red | `Green | `Blue ] list

  let color_flag_mask_flags = [ (`Red, 0); (`Green, 1); (`Blue, 2) ]

  let decode_color_flag_mask i =
    List.filter_map
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      color_flag_mask_flags

  let int_of_color_flag_mask flags =
    List.fold_left
      (fun acc flag ->
        let code = match flag with `Red -> 0 | `Green -> 1 | `Blue -> 2 in
        acc lor (1 lsl code))
      0 flags

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

  let encode_coloritem buf { pixel; red; green; blue; flags } =
    encode_int32 buf pixel;
    encode_uint16 buf red;
    encode_uint16 buf green;
    encode_uint16 buf blue;
    encode_to_int encode_char
      (fun x -> Char.unsafe_chr (int_of_color_flag_mask x))
      buf flags;
    ignore buf

  let store_colors ~(cmap : colormap) ~(items : coloritem list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_colormap buf cmap;
    buf

  let store_named_color ~(flags : color_flag_mask) ~(cmap : colormap)
      ~(pixel : int32) ~(name : char list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_uint8 int_of_color_flag_mask buf flags;
    encode_colormap buf cmap;
    encode_int32 buf pixel;
    encode_uint16 buf (List.length name);
    encode_list encode_char buf name;
    buf

  type rgb = { red : int; green : int; blue : int }

  let decode_rgb buf ~at : (rgb * int) option =
    let orig = at in
    let* red, at = decode_uint16 buf ~at in
    let* green, at = decode_uint16 buf ~at in
    let* blue, at = decode_uint16 buf ~at in
    let at = at + 2 in
    ignore orig;
    Some ({ red; green; blue }, at)

  let encode_rgb buf { red; green; blue } =
    encode_uint16 buf red;
    encode_uint16 buf green;
    encode_uint16 buf blue;
    ignore buf

  type query_colors_reply = { colors : rgb list }

  let query_colors ~(cmap : colormap) ~(pixels : int32 list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_colormap buf cmap;
    buf

  type lookup_color_reply = {
    exact_red : int;
    exact_green : int;
    exact_blue : int;
    visual_red : int;
    visual_green : int;
    visual_blue : int;
  }

  let lookup_color ~(cmap : colormap) ~(name : char list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_colormap buf cmap;
    encode_uint16 buf (List.length name);
    encode_list encode_char buf name;
    buf

  let create_cursor ~(cid : cursor) ~(source : pixmap)
      ~(mask : (pixmap_enum, pixmap) alt) ~(fore_red : int) ~(fore_green : int)
      ~(fore_blue : int) ~(back_red : int) ~(back_green : int)
      ~(back_blue : int) ~(x : int) ~(y : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_cursor buf cid;
    encode_pixmap buf source;
    encode_alt encode_pixmap int_of_pixmap_enum encode_pixmap buf mask;
    encode_uint16 buf fore_red;
    encode_uint16 buf fore_green;
    encode_uint16 buf fore_blue;
    encode_uint16 buf back_red;
    encode_uint16 buf back_green;
    encode_uint16 buf back_blue;
    encode_uint16 buf x;
    encode_uint16 buf y;
    buf

  let create_glyph_cursor ~(cid : cursor) ~(source_font : font)
      ~(mask_font : (font_enum, font) alt) ~(source_char : int)
      ~(mask_char : int) ~(fore_red : int) ~(fore_green : int)
      ~(fore_blue : int) ~(back_red : int) ~(back_green : int)
      ~(back_blue : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_cursor buf cid;
    encode_font buf source_font;
    encode_alt encode_font int_of_font_enum encode_font buf mask_font;
    encode_uint16 buf source_char;
    encode_uint16 buf mask_char;
    encode_uint16 buf fore_red;
    encode_uint16 buf fore_green;
    encode_uint16 buf fore_blue;
    encode_uint16 buf back_red;
    encode_uint16 buf back_green;
    encode_uint16 buf back_blue;
    buf

  let free_cursor ~(cursor : cursor) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_cursor buf cursor;
    buf

  let recolor_cursor ~(cursor : cursor) ~(fore_red : int) ~(fore_green : int)
      ~(fore_blue : int) ~(back_red : int) ~(back_green : int)
      ~(back_blue : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_cursor buf cursor;
    encode_uint16 buf fore_red;
    encode_uint16 buf fore_green;
    encode_uint16 buf fore_blue;
    encode_uint16 buf back_red;
    encode_uint16 buf back_green;
    encode_uint16 buf back_blue;
    buf

  type query_shape_of_enum =
    [ `Largest_cursor | `Fastest_tile | `Fastest_stipple ]

  let query_shape_of_enum_of_int : int -> query_shape_of_enum option = function
    | 0 -> Some `Largest_cursor
    | 1 -> Some `Fastest_tile
    | 2 -> Some `Fastest_stipple
    | _ -> None

  let int_of_query_shape_of_enum : query_shape_of_enum -> int = function
    | `Largest_cursor -> 0
    | `Fastest_tile -> 1
    | `Fastest_stipple -> 2

  type query_best_size_reply = { width : int; height : int }

  let query_best_size ~(class_ : query_shape_of_enum) ~(drawable : drawable)
      ~(width : int) ~(height : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_uint8 int_of_query_shape_of_enum buf class_;
    encode_drawable buf drawable;
    encode_uint16 buf width;
    encode_uint16 buf height;
    buf

  type query_extension_reply = {
    present : bool;
    major_opcode : int;
    first_event : int;
    first_error : int;
  }

  let query_extension ~(name : char list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint16 buf (List.length name);
    encode_list encode_char buf name;
    buf

  type list_extensions_reply = { names : str list }

  let list_extensions () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  let change_keyboard_mapping ~(keycode_count : int) ~(first_keycode : keycode)
      ~(keysyms_per_keycode : int) ~(keysyms : keysym list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint8 buf keycode_count;
    encode_keycode buf first_keycode;
    encode_uint8 buf keysyms_per_keycode;
    buf

  type get_keyboard_mapping_reply = {
    keysyms_per_keycode : char;
    keysyms : keysym list;
  }

  let get_keyboard_mapping ~(first_keycode : keycode) ~(count : int) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_keycode buf first_keycode;
    encode_uint8 buf count;
    buf

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
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      kb_mask_flags

  let int_of_kb_mask flags =
    List.fold_left
      (fun acc flag ->
        let code =
          match flag with
          | `Key_click_percent -> 0
          | `Bell_percent -> 1
          | `Bell_pitch -> 2
          | `Bell_duration -> 3
          | `Led -> 4
          | `Led_mode -> 5
          | `Key -> 6
          | `Auto_repeat_mode -> 7
        in
        acc lor (1 lsl code))
      0 flags

  type led_mode_enum = [ `Off | `On ]

  let led_mode_enum_of_int : int -> led_mode_enum option = function
    | 0 -> Some `Off
    | 1 -> Some `On
    | _ -> None

  let int_of_led_mode_enum : led_mode_enum -> int = function
    | `Off -> 0
    | `On -> 1

  type auto_repeat_mode_enum = [ `Off | `On | `Default ]

  let auto_repeat_mode_enum_of_int : int -> auto_repeat_mode_enum option =
    function
    | 0 -> Some `Off
    | 1 -> Some `On
    | 2 -> Some `Default
    | _ -> None

  let int_of_auto_repeat_mode_enum : auto_repeat_mode_enum -> int = function
    | `Off -> 0
    | `On -> 1
    | `Default -> 2

  let change_keyboard_control ?(key_click_percent : int32 option)
      ?(bell_percent : int32 option) ?(bell_pitch : int32 option)
      ?(bell_duration : int32 option) ?(led : int32 option)
      ?(led_mode : led_mode_enum option) ?(key : keycode32 option)
      ?(auto_repeat_mode : auto_repeat_mode_enum option) () : Buffer.t =
    let buf = Buffer.create 16 in
    let value_mask = 0 in
    let value_mask =
      if Option.is_some key_click_percent then value_mask lor (1 lsl 0)
      else value_mask
    in
    let value_mask =
      if Option.is_some bell_percent then value_mask lor (1 lsl 1)
      else value_mask
    in
    let value_mask =
      if Option.is_some bell_pitch then value_mask lor (1 lsl 2) else value_mask
    in
    let value_mask =
      if Option.is_some bell_duration then value_mask lor (1 lsl 3)
      else value_mask
    in
    let value_mask =
      if Option.is_some led then value_mask lor (1 lsl 4) else value_mask
    in
    let value_mask =
      if Option.is_some led_mode then value_mask lor (1 lsl 5) else value_mask
    in
    let value_mask =
      if Option.is_some key then value_mask lor (1 lsl 6) else value_mask
    in
    let value_mask =
      if Option.is_some auto_repeat_mode then value_mask lor (1 lsl 7)
      else value_mask
    in
    encode_int32 buf (Int32.of_int value_mask);
    Option.iter
      (fun key_click_percent -> encode_int32 buf key_click_percent)
      key_click_percent;
    Option.iter (fun bell_percent -> encode_int32 buf bell_percent) bell_percent;
    Option.iter (fun bell_pitch -> encode_int32 buf bell_pitch) bell_pitch;
    Option.iter
      (fun bell_duration -> encode_int32 buf bell_duration)
      bell_duration;
    Option.iter (fun led -> encode_int32 buf led) led;
    Option.iter
      (fun led_mode ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_led_mode_enum x))
          buf led_mode)
      led_mode;
    Option.iter (fun key -> encode_keycode32 buf key) key;
    Option.iter
      (fun auto_repeat_mode ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_auto_repeat_mode_enum x))
          buf auto_repeat_mode)
      auto_repeat_mode;
    buf

  type get_keyboard_control_reply = {
    global_auto_repeat : auto_repeat_mode_enum;
    led_mask : int32;
    key_click_percent : int;
    bell_percent : int;
    bell_pitch : int;
    bell_duration : int;
    auto_repeats : int list;
  }

  let get_keyboard_control () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  let bell ~(percent : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int8 buf percent;
    buf

  let change_pointer_control ~(acceleration_numerator : int)
      ~(acceleration_denominator : int) ~(threshold : int)
      ~(do_acceleration : bool) ~(do_threshold : bool) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int16 buf acceleration_numerator;
    encode_int16 buf acceleration_denominator;
    encode_int16 buf threshold;
    encode_bool buf do_acceleration;
    encode_bool buf do_threshold;
    buf

  type get_pointer_control_reply = {
    acceleration_numerator : int;
    acceleration_denominator : int;
    threshold : int;
  }

  let get_pointer_control () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  type blanking_enum = [ `Not_preferred | `Preferred | `Default ]

  let blanking_enum_of_int : int -> blanking_enum option = function
    | 0 -> Some `Not_preferred
    | 1 -> Some `Preferred
    | 2 -> Some `Default
    | _ -> None

  let int_of_blanking_enum : blanking_enum -> int = function
    | `Not_preferred -> 0
    | `Preferred -> 1
    | `Default -> 2

  type exposures_enum = [ `Not_allowed | `Allowed | `Default ]

  let exposures_enum_of_int : int -> exposures_enum option = function
    | 0 -> Some `Not_allowed
    | 1 -> Some `Allowed
    | 2 -> Some `Default
    | _ -> None

  let int_of_exposures_enum : exposures_enum -> int = function
    | `Not_allowed -> 0
    | `Allowed -> 1
    | `Default -> 2

  let set_screen_saver ~(timeout : int) ~(interval : int)
      ~(prefer_blanking : blanking_enum) ~(allow_exposures : exposures_enum) ()
      : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int16 buf timeout;
    encode_int16 buf interval;
    encode_to_int encode_uint8 int_of_blanking_enum buf prefer_blanking;
    encode_to_int encode_uint8 int_of_exposures_enum buf allow_exposures;
    buf

  type get_screen_saver_reply = {
    timeout : int;
    interval : int;
    prefer_blanking : blanking_enum;
    allow_exposures : exposures_enum;
  }

  let get_screen_saver () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  type host_mode_enum = [ `Insert | `Delete ]

  let host_mode_enum_of_int : int -> host_mode_enum option = function
    | 0 -> Some `Insert
    | 1 -> Some `Delete
    | _ -> None

  let int_of_host_mode_enum : host_mode_enum -> int = function
    | `Insert -> 0
    | `Delete -> 1

  type family_enum =
    [ `Internet | `Decnet | `Chaos | `Server_interpreted | `Internet6 ]

  let family_enum_of_int : int -> family_enum option = function
    | 0 -> Some `Internet
    | 1 -> Some `Decnet
    | 2 -> Some `Chaos
    | 5 -> Some `Server_interpreted
    | 6 -> Some `Internet6
    | _ -> None

  let int_of_family_enum : family_enum -> int = function
    | `Internet -> 0
    | `Decnet -> 1
    | `Chaos -> 2
    | `Server_interpreted -> 5
    | `Internet6 -> 6

  let change_hosts ~(mode : host_mode_enum) ~(family : family_enum)
      ~(address : char list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_uint8 int_of_host_mode_enum buf mode;
    encode_to_int encode_uint8 int_of_family_enum buf family;
    encode_uint16 buf (List.length address);
    encode_list encode_char buf address;
    buf

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

  let encode_host buf { family; address } =
    encode_to_int encode_uint8 int_of_family_enum buf family;
    encode_uint16 buf (List.length address);
    encode_list encode_char buf address;
    ignore buf

  type access_control_enum = [ `Disable | `Enable ]

  let access_control_enum_of_int : int -> access_control_enum option = function
    | 0 -> Some `Disable
    | 1 -> Some `Enable
    | _ -> None

  let int_of_access_control_enum : access_control_enum -> int = function
    | `Disable -> 0
    | `Enable -> 1

  type list_hosts_reply = { mode : access_control_enum; hosts : host list }

  let list_hosts () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  let set_access_control ~(mode : access_control_enum) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_uint8 int_of_access_control_enum buf mode;
    buf

  type close_down_enum = [ `Destroy_all | `Retain_permanent | `Retain_temporary ]

  let close_down_enum_of_int : int -> close_down_enum option = function
    | 0 -> Some `Destroy_all
    | 1 -> Some `Retain_permanent
    | 2 -> Some `Retain_temporary
    | _ -> None

  let int_of_close_down_enum : close_down_enum -> int = function
    | `Destroy_all -> 0
    | `Retain_permanent -> 1
    | `Retain_temporary -> 2

  let set_close_down_mode ~(mode : close_down_enum) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_uint8 int_of_close_down_enum buf mode;
    buf

  type kill_enum = [ `All_temporary ]

  let kill_enum_of_int : int -> kill_enum option = function
    | 0 -> Some `All_temporary
    | _ -> None

  let int_of_kill_enum : kill_enum -> int = function `All_temporary -> 0

  let kill_client ~(resource : (kill_enum, int32) alt) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_alt encode_int32
      (fun x -> Int32.of_int (int_of_kill_enum x))
      encode_int32 buf resource;
    buf

  let rotate_properties ~(window : window) ~(delta : int) ~(atoms : atom list)
      () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_window buf window;
    encode_uint16 buf (List.length atoms);
    encode_int16 buf delta;
    encode_list encode_atom buf atoms;
    buf

  type screen_saver_enum = [ `Reset | `Active ]

  let screen_saver_enum_of_int : int -> screen_saver_enum option = function
    | 0 -> Some `Reset
    | 1 -> Some `Active
    | _ -> None

  let int_of_screen_saver_enum : screen_saver_enum -> int = function
    | `Reset -> 0
    | `Active -> 1

  let force_screen_saver ~(mode : screen_saver_enum) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_uint8 int_of_screen_saver_enum buf mode;
    buf

  type mapping_status_enum = [ `Success | `Busy | `Failure ]

  let mapping_status_enum_of_int : int -> mapping_status_enum option = function
    | 0 -> Some `Success
    | 1 -> Some `Busy
    | 2 -> Some `Failure
    | _ -> None

  let int_of_mapping_status_enum : mapping_status_enum -> int = function
    | `Success -> 0
    | `Busy -> 1
    | `Failure -> 2

  type set_pointer_mapping_reply = { status : mapping_status_enum }

  let set_pointer_mapping ~(map : int list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint8 buf (List.length map);
    encode_list encode_uint8 buf map;
    buf

  type get_pointer_mapping_reply = { map : int list }

  let get_pointer_mapping () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

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

  let int_of_map_index_enum : map_index_enum -> int = function
    | `Shift -> 0
    | `Lock -> 1
    | `Control -> 2
    | `D1 -> 3
    | `D2 -> 4
    | `D3 -> 5
    | `D4 -> 6
    | `D5 -> 7

  type set_modifier_mapping_reply = { status : mapping_status_enum }

  let set_modifier_mapping ~(keycodes_per_modifier : int)
      ~(keycodes : keycode list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint8 buf keycodes_per_modifier;
    buf

  type get_modifier_mapping_reply = {
    keycodes_per_modifier : int;
    keycodes : keycode list;
  }

  let get_modifier_mapping () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  let no_operation () : Buffer.t =
    let buf = Buffer.create 16 in
    buf
end
[@@warning "-27"]

module Render = struct
  type pict_type_enum = [ `Indexed | `Direct ]

  let pict_type_enum_of_int : int -> pict_type_enum option = function
    | 0 -> Some `Indexed
    | 1 -> Some `Direct
    | _ -> None

  let int_of_pict_type_enum : pict_type_enum -> int = function
    | `Indexed -> 0
    | `Direct -> 1

  type picture_enum = [ `None ]

  let picture_enum_of_int : int -> picture_enum option = function
    | 0 -> Some `None
    | _ -> None

  let int_of_picture_enum : picture_enum -> int = function `None -> 0

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

  let int_of_pict_op_enum : pict_op_enum -> int = function
    | `Clear -> 0
    | `Src -> 1
    | `Dst -> 2
    | `Over -> 3
    | `Over_reverse -> 4
    | `In -> 5
    | `In_reverse -> 6
    | `Out -> 7
    | `Out_reverse -> 8
    | `Atop -> 9
    | `Atop_reverse -> 10
    | `Xor -> 11
    | `Add -> 12
    | `Saturate -> 13
    | `Disjoint_clear -> 16
    | `Disjoint_src -> 17
    | `Disjoint_dst -> 18
    | `Disjoint_over -> 19
    | `Disjoint_over_reverse -> 20
    | `Disjoint_in -> 21
    | `Disjoint_in_reverse -> 22
    | `Disjoint_out -> 23
    | `Disjoint_out_reverse -> 24
    | `Disjoint_atop -> 25
    | `Disjoint_atop_reverse -> 26
    | `Disjoint_xor -> 27
    | `Conjoint_clear -> 32
    | `Conjoint_src -> 33
    | `Conjoint_dst -> 34
    | `Conjoint_over -> 35
    | `Conjoint_over_reverse -> 36
    | `Conjoint_in -> 37
    | `Conjoint_in_reverse -> 38
    | `Conjoint_out -> 39
    | `Conjoint_out_reverse -> 40
    | `Conjoint_atop -> 41
    | `Conjoint_atop_reverse -> 42
    | `Conjoint_xor -> 43
    | `Multiply -> 48
    | `Screen -> 49
    | `Overlay -> 50
    | `Darken -> 51
    | `Lighten -> 52
    | `Color_dodge -> 53
    | `Color_burn -> 54
    | `Hard_light -> 55
    | `Soft_light -> 56
    | `Difference -> 57
    | `Exclusion -> 58
    | `Hsl_hue -> 59
    | `Hsl_saturation -> 60
    | `Hsl_color -> 61
    | `Hsl_luminosity -> 62

  type poly_edge_enum = [ `Sharp | `Smooth ]

  let poly_edge_enum_of_int : int -> poly_edge_enum option = function
    | 0 -> Some `Sharp
    | 1 -> Some `Smooth
    | _ -> None

  let int_of_poly_edge_enum : poly_edge_enum -> int = function
    | `Sharp -> 0
    | `Smooth -> 1

  type poly_mode_enum = [ `Precise | `Imprecise ]

  let poly_mode_enum_of_int : int -> poly_mode_enum option = function
    | 0 -> Some `Precise
    | 1 -> Some `Imprecise
    | _ -> None

  let int_of_poly_mode_enum : poly_mode_enum -> int = function
    | `Precise -> 0
    | `Imprecise -> 1

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

  let cp_mask_flags =
    [
      (`Repeat, 0);
      (`Alpha_map, 1);
      (`Alpha_x_origin, 2);
      (`Alpha_y_origin, 3);
      (`Clip_x_origin, 4);
      (`Clip_y_origin, 5);
      (`Clip_mask, 6);
      (`Graphics_exposure, 7);
      (`Subwindow_mode, 8);
      (`Poly_edge, 9);
      (`Poly_mode, 10);
      (`Dither, 11);
      (`Component_alpha, 12);
    ]

  let decode_cp_mask i =
    List.filter_map
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      cp_mask_flags

  let int_of_cp_mask flags =
    List.fold_left
      (fun acc flag ->
        let code =
          match flag with
          | `Repeat -> 0
          | `Alpha_map -> 1
          | `Alpha_x_origin -> 2
          | `Alpha_y_origin -> 3
          | `Clip_x_origin -> 4
          | `Clip_y_origin -> 5
          | `Clip_mask -> 6
          | `Graphics_exposure -> 7
          | `Subwindow_mode -> 8
          | `Poly_edge -> 9
          | `Poly_mode -> 10
          | `Dither -> 11
          | `Component_alpha -> 12
        in
        acc lor (1 lsl code))
      0 flags

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

  let int_of_sub_pixel_enum : sub_pixel_enum -> int = function
    | `Unknown -> 0
    | `Horizontal_rgb -> 1
    | `Horizontal_bgr -> 2
    | `Vertical_rgb -> 3
    | `Vertical_bgr -> 4
    | `None -> 5

  type repeat_enum = [ `None | `Normal | `Pad | `Reflect ]

  let repeat_enum_of_int : int -> repeat_enum option = function
    | 0 -> Some `None
    | 1 -> Some `Normal
    | 2 -> Some `Pad
    | 3 -> Some `Reflect
    | _ -> None

  let int_of_repeat_enum : repeat_enum -> int = function
    | `None -> 0
    | `Normal -> 1
    | `Pad -> 2
    | `Reflect -> 3

  type glyph = int32

  let decode_glyph = decode_int32

  let encode_glyph = encode_int32

  type glyphset = xid

  let decode_glyphset = decode_xid

  let encode_glyphset = encode_xid

  type picture = xid

  let decode_picture = decode_xid

  let encode_picture = encode_xid

  type pictformat = xid

  let decode_pictformat = decode_xid

  let encode_pictformat = encode_xid

  type fixed = int32

  let decode_fixed = decode_int32

  let encode_fixed = encode_int32

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

  let encode_directformat buf
      {
        red_shift;
        red_mask;
        green_shift;
        green_mask;
        blue_shift;
        blue_mask;
        alpha_shift;
        alpha_mask;
      } =
    encode_uint16 buf red_shift;
    encode_uint16 buf red_mask;
    encode_uint16 buf green_shift;
    encode_uint16 buf green_mask;
    encode_uint16 buf blue_shift;
    encode_uint16 buf blue_mask;
    encode_uint16 buf alpha_shift;
    encode_uint16 buf alpha_mask;
    ignore buf

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

  let encode_pictforminfo buf { id; type_; depth; direct; colormap } =
    encode_pictformat buf id;
    encode_to_int encode_uint8 int_of_pict_type_enum buf type_;
    encode_uint8 buf depth;
    encode_directformat buf direct;
    Xproto.encode_colormap buf colormap;
    ignore buf

  type pictvisual = { visual : Xproto.visualid; format : pictformat }

  let decode_pictvisual buf ~at : (pictvisual * int) option =
    let orig = at in
    let* visual, at = Xproto.decode_visualid buf ~at in
    let* format, at = decode_pictformat buf ~at in
    ignore orig;
    Some ({ visual; format }, at)

  let encode_pictvisual buf { visual; format } =
    Xproto.encode_visualid buf visual;
    encode_pictformat buf format;
    ignore buf

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

  let encode_pictdepth buf { depth; visuals } =
    encode_uint8 buf depth;
    encode_uint16 buf (List.length visuals);
    encode_list encode_pictvisual buf visuals;
    ignore buf

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

  let encode_pictscreen buf { fallback; depths } =
    encode_int32 buf (Int32.of_int (List.length depths));
    encode_pictformat buf fallback;
    encode_list encode_pictdepth buf depths;
    ignore buf

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

  let encode_indexvalue buf { pixel; red; green; blue; alpha } =
    encode_int32 buf pixel;
    encode_uint16 buf red;
    encode_uint16 buf green;
    encode_uint16 buf blue;
    encode_uint16 buf alpha;
    ignore buf

  type color = { red : int; green : int; blue : int; alpha : int }

  let decode_color buf ~at : (color * int) option =
    let orig = at in
    let* red, at = decode_uint16 buf ~at in
    let* green, at = decode_uint16 buf ~at in
    let* blue, at = decode_uint16 buf ~at in
    let* alpha, at = decode_uint16 buf ~at in
    ignore orig;
    Some ({ red; green; blue; alpha }, at)

  let encode_color buf { red; green; blue; alpha } =
    encode_uint16 buf red;
    encode_uint16 buf green;
    encode_uint16 buf blue;
    encode_uint16 buf alpha;
    ignore buf

  type pointfix = { x : fixed; y : fixed }

  let decode_pointfix buf ~at : (pointfix * int) option =
    let orig = at in
    let* x, at = decode_fixed buf ~at in
    let* y, at = decode_fixed buf ~at in
    ignore orig;
    Some ({ x; y }, at)

  let encode_pointfix buf { x; y } =
    encode_fixed buf x;
    encode_fixed buf y;
    ignore buf

  type linefix = { p1 : pointfix; p2 : pointfix }

  let decode_linefix buf ~at : (linefix * int) option =
    let orig = at in
    let* p1, at = decode_pointfix buf ~at in
    let* p2, at = decode_pointfix buf ~at in
    ignore orig;
    Some ({ p1; p2 }, at)

  let encode_linefix buf { p1; p2 } =
    encode_pointfix buf p1;
    encode_pointfix buf p2;
    ignore buf

  type triangle = { p1 : pointfix; p2 : pointfix; p3 : pointfix }

  let decode_triangle buf ~at : (triangle * int) option =
    let orig = at in
    let* p1, at = decode_pointfix buf ~at in
    let* p2, at = decode_pointfix buf ~at in
    let* p3, at = decode_pointfix buf ~at in
    ignore orig;
    Some ({ p1; p2; p3 }, at)

  let encode_triangle buf { p1; p2; p3 } =
    encode_pointfix buf p1;
    encode_pointfix buf p2;
    encode_pointfix buf p3;
    ignore buf

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

  let encode_trapezoid buf { top; bottom; left; right } =
    encode_fixed buf top;
    encode_fixed buf bottom;
    encode_linefix buf left;
    encode_linefix buf right;
    ignore buf

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

  let encode_glyphinfo buf { width; height; x; y; x_off; y_off } =
    encode_uint16 buf width;
    encode_uint16 buf height;
    encode_int16 buf x;
    encode_int16 buf y;
    encode_int16 buf x_off;
    encode_int16 buf y_off;
    ignore buf

  type query_version_reply = { major_version : int32; minor_version : int32 }

  let query_version ~(client_major_version : int32)
      ~(client_minor_version : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf client_major_version;
    encode_int32 buf client_minor_version;
    buf

  type query_pict_formats_reply = {
    num_depths : int32;
    num_visuals : int32;
    formats : pictforminfo list;
    screens : pictscreen list;
    subpixels : sub_pixel_enum list;
  }

  let query_pict_formats () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  type query_pict_index_values_reply = { values : indexvalue list }

  let query_pict_index_values ~(format : pictformat) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_pictformat buf format;
    buf

  let create_picture ~(pid : picture) ~(drawable : Xproto.drawable)
      ~(format : pictformat) ?(repeat : repeat_enum option)
      ?(alphamap : picture option) ?(alphaxorigin : int32 option)
      ?(alphayorigin : int32 option) ?(clipxorigin : int32 option)
      ?(clipyorigin : int32 option) ?(clipmask : Xproto.pixmap option)
      ?(graphicsexposure : int32 option)
      ?(subwindowmode : Xproto.subwindow_mode_enum option)
      ?(polyedge : poly_edge_enum option) ?(polymode : poly_mode_enum option)
      ?(dither : Xproto.atom option) ?(componentalpha : int32 option) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_picture buf pid;
    Xproto.encode_drawable buf drawable;
    encode_pictformat buf format;
    let value_mask = 0 in
    let value_mask =
      if Option.is_some repeat then value_mask lor (1 lsl 0) else value_mask
    in
    let value_mask =
      if Option.is_some alphamap then value_mask lor (1 lsl 1) else value_mask
    in
    let value_mask =
      if Option.is_some alphaxorigin then value_mask lor (1 lsl 2)
      else value_mask
    in
    let value_mask =
      if Option.is_some alphayorigin then value_mask lor (1 lsl 3)
      else value_mask
    in
    let value_mask =
      if Option.is_some clipxorigin then value_mask lor (1 lsl 4)
      else value_mask
    in
    let value_mask =
      if Option.is_some clipyorigin then value_mask lor (1 lsl 5)
      else value_mask
    in
    let value_mask =
      if Option.is_some clipmask then value_mask lor (1 lsl 6) else value_mask
    in
    let value_mask =
      if Option.is_some graphicsexposure then value_mask lor (1 lsl 7)
      else value_mask
    in
    let value_mask =
      if Option.is_some subwindowmode then value_mask lor (1 lsl 8)
      else value_mask
    in
    let value_mask =
      if Option.is_some polyedge then value_mask lor (1 lsl 9) else value_mask
    in
    let value_mask =
      if Option.is_some polymode then value_mask lor (1 lsl 10) else value_mask
    in
    let value_mask =
      if Option.is_some dither then value_mask lor (1 lsl 11) else value_mask
    in
    let value_mask =
      if Option.is_some componentalpha then value_mask lor (1 lsl 12)
      else value_mask
    in
    encode_int32 buf (Int32.of_int value_mask);
    Option.iter
      (fun repeat ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_repeat_enum x))
          buf repeat)
      repeat;
    Option.iter (fun alphamap -> encode_picture buf alphamap) alphamap;
    Option.iter (fun alphaxorigin -> encode_int32 buf alphaxorigin) alphaxorigin;
    Option.iter (fun alphayorigin -> encode_int32 buf alphayorigin) alphayorigin;
    Option.iter (fun clipxorigin -> encode_int32 buf clipxorigin) clipxorigin;
    Option.iter (fun clipyorigin -> encode_int32 buf clipyorigin) clipyorigin;
    Option.iter (fun clipmask -> Xproto.encode_pixmap buf clipmask) clipmask;
    Option.iter
      (fun graphicsexposure -> encode_int32 buf graphicsexposure)
      graphicsexposure;
    Option.iter
      (fun subwindowmode ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (Xproto.int_of_subwindow_mode_enum x))
          buf subwindowmode)
      subwindowmode;
    Option.iter
      (fun polyedge ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_poly_edge_enum x))
          buf polyedge)
      polyedge;
    Option.iter
      (fun polymode ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_poly_mode_enum x))
          buf polymode)
      polymode;
    Option.iter (fun dither -> Xproto.encode_atom buf dither) dither;
    Option.iter
      (fun componentalpha -> encode_int32 buf componentalpha)
      componentalpha;
    buf

  let change_picture ~(picture : picture) ?(repeat : repeat_enum option)
      ?(alphamap : picture option) ?(alphaxorigin : int32 option)
      ?(alphayorigin : int32 option) ?(clipxorigin : int32 option)
      ?(clipyorigin : int32 option) ?(clipmask : Xproto.pixmap option)
      ?(graphicsexposure : int32 option)
      ?(subwindowmode : Xproto.subwindow_mode_enum option)
      ?(polyedge : poly_edge_enum option) ?(polymode : poly_mode_enum option)
      ?(dither : Xproto.atom option) ?(componentalpha : int32 option) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_picture buf picture;
    let value_mask = 0 in
    let value_mask =
      if Option.is_some repeat then value_mask lor (1 lsl 0) else value_mask
    in
    let value_mask =
      if Option.is_some alphamap then value_mask lor (1 lsl 1) else value_mask
    in
    let value_mask =
      if Option.is_some alphaxorigin then value_mask lor (1 lsl 2)
      else value_mask
    in
    let value_mask =
      if Option.is_some alphayorigin then value_mask lor (1 lsl 3)
      else value_mask
    in
    let value_mask =
      if Option.is_some clipxorigin then value_mask lor (1 lsl 4)
      else value_mask
    in
    let value_mask =
      if Option.is_some clipyorigin then value_mask lor (1 lsl 5)
      else value_mask
    in
    let value_mask =
      if Option.is_some clipmask then value_mask lor (1 lsl 6) else value_mask
    in
    let value_mask =
      if Option.is_some graphicsexposure then value_mask lor (1 lsl 7)
      else value_mask
    in
    let value_mask =
      if Option.is_some subwindowmode then value_mask lor (1 lsl 8)
      else value_mask
    in
    let value_mask =
      if Option.is_some polyedge then value_mask lor (1 lsl 9) else value_mask
    in
    let value_mask =
      if Option.is_some polymode then value_mask lor (1 lsl 10) else value_mask
    in
    let value_mask =
      if Option.is_some dither then value_mask lor (1 lsl 11) else value_mask
    in
    let value_mask =
      if Option.is_some componentalpha then value_mask lor (1 lsl 12)
      else value_mask
    in
    encode_int32 buf (Int32.of_int value_mask);
    Option.iter
      (fun repeat ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_repeat_enum x))
          buf repeat)
      repeat;
    Option.iter (fun alphamap -> encode_picture buf alphamap) alphamap;
    Option.iter (fun alphaxorigin -> encode_int32 buf alphaxorigin) alphaxorigin;
    Option.iter (fun alphayorigin -> encode_int32 buf alphayorigin) alphayorigin;
    Option.iter (fun clipxorigin -> encode_int32 buf clipxorigin) clipxorigin;
    Option.iter (fun clipyorigin -> encode_int32 buf clipyorigin) clipyorigin;
    Option.iter (fun clipmask -> Xproto.encode_pixmap buf clipmask) clipmask;
    Option.iter
      (fun graphicsexposure -> encode_int32 buf graphicsexposure)
      graphicsexposure;
    Option.iter
      (fun subwindowmode ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (Xproto.int_of_subwindow_mode_enum x))
          buf subwindowmode)
      subwindowmode;
    Option.iter
      (fun polyedge ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_poly_edge_enum x))
          buf polyedge)
      polyedge;
    Option.iter
      (fun polymode ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_poly_mode_enum x))
          buf polymode)
      polymode;
    Option.iter (fun dither -> Xproto.encode_atom buf dither) dither;
    Option.iter
      (fun componentalpha -> encode_int32 buf componentalpha)
      componentalpha;
    buf

  let set_picture_clip_rectangles ~(picture : picture) ~(clip_x_origin : int)
      ~(clip_y_origin : int) ~(rectangles : Xproto.rectangle list) () : Buffer.t
      =
    let buf = Buffer.create 16 in
    encode_picture buf picture;
    encode_int16 buf clip_x_origin;
    encode_int16 buf clip_y_origin;
    buf

  let free_picture ~(picture : picture) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_picture buf picture;
    buf

  let composite ~(op : pict_op_enum) ~(src : picture)
      ~(mask : (picture_enum, picture) alt) ~(dst : picture) ~(src_x : int)
      ~(src_y : int) ~(mask_x : int) ~(mask_y : int) ~(dst_x : int)
      ~(dst_y : int) ~(width : int) ~(height : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_uint8 int_of_pict_op_enum buf op;
    encode_picture buf src;
    encode_alt encode_picture int_of_picture_enum encode_picture buf mask;
    encode_picture buf dst;
    encode_int16 buf src_x;
    encode_int16 buf src_y;
    encode_int16 buf mask_x;
    encode_int16 buf mask_y;
    encode_int16 buf dst_x;
    encode_int16 buf dst_y;
    encode_uint16 buf width;
    encode_uint16 buf height;
    buf

  let trapezoids ~(op : pict_op_enum) ~(src : picture) ~(dst : picture)
      ~(mask_format : pictformat) ~(src_x : int) ~(src_y : int)
      ~(traps : trapezoid list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_uint8 int_of_pict_op_enum buf op;
    encode_picture buf src;
    encode_picture buf dst;
    encode_pictformat buf mask_format;
    encode_int16 buf src_x;
    encode_int16 buf src_y;
    buf

  let triangles ~(op : pict_op_enum) ~(src : picture) ~(dst : picture)
      ~(mask_format : pictformat) ~(src_x : int) ~(src_y : int)
      ~(triangles : triangle list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_uint8 int_of_pict_op_enum buf op;
    encode_picture buf src;
    encode_picture buf dst;
    encode_pictformat buf mask_format;
    encode_int16 buf src_x;
    encode_int16 buf src_y;
    buf

  let tri_strip ~(op : pict_op_enum) ~(src : picture) ~(dst : picture)
      ~(mask_format : pictformat) ~(src_x : int) ~(src_y : int)
      ~(points : pointfix list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_uint8 int_of_pict_op_enum buf op;
    encode_picture buf src;
    encode_picture buf dst;
    encode_pictformat buf mask_format;
    encode_int16 buf src_x;
    encode_int16 buf src_y;
    buf

  let tri_fan ~(op : pict_op_enum) ~(src : picture) ~(dst : picture)
      ~(mask_format : pictformat) ~(src_x : int) ~(src_y : int)
      ~(points : pointfix list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_uint8 int_of_pict_op_enum buf op;
    encode_picture buf src;
    encode_picture buf dst;
    encode_pictformat buf mask_format;
    encode_int16 buf src_x;
    encode_int16 buf src_y;
    buf

  let create_glyph_set ~(gsid : glyphset) ~(format : pictformat) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_glyphset buf gsid;
    encode_pictformat buf format;
    buf

  let reference_glyph_set ~(gsid : glyphset) ~(existing : glyphset) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_glyphset buf gsid;
    encode_glyphset buf existing;
    buf

  let free_glyph_set ~(glyphset : glyphset) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_glyphset buf glyphset;
    buf

  let add_glyphs ~(glyphset : glyphset) ~(glyphids : int32 list)
      ~(glyphs : glyphinfo list) ~(data : char list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_glyphset buf glyphset;
    encode_int32 buf (Int32.of_int (List.length glyphids));
    encode_list encode_int32 buf glyphids;
    encode_list encode_glyphinfo buf glyphs;
    buf

  let free_glyphs ~(glyphset : glyphset) ~(glyphs : glyph list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_glyphset buf glyphset;
    buf

  let composite_glyphs8 ~(op : pict_op_enum) ~(src : picture) ~(dst : picture)
      ~(mask_format : pictformat) ~(glyphset : glyphset) ~(src_x : int)
      ~(src_y : int) ~(glyphcmds : char list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_uint8 int_of_pict_op_enum buf op;
    encode_picture buf src;
    encode_picture buf dst;
    encode_pictformat buf mask_format;
    encode_glyphset buf glyphset;
    encode_int16 buf src_x;
    encode_int16 buf src_y;
    buf

  let composite_glyphs16 ~(op : pict_op_enum) ~(src : picture) ~(dst : picture)
      ~(mask_format : pictformat) ~(glyphset : glyphset) ~(src_x : int)
      ~(src_y : int) ~(glyphcmds : char list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_uint8 int_of_pict_op_enum buf op;
    encode_picture buf src;
    encode_picture buf dst;
    encode_pictformat buf mask_format;
    encode_glyphset buf glyphset;
    encode_int16 buf src_x;
    encode_int16 buf src_y;
    buf

  let composite_glyphs32 ~(op : pict_op_enum) ~(src : picture) ~(dst : picture)
      ~(mask_format : pictformat) ~(glyphset : glyphset) ~(src_x : int)
      ~(src_y : int) ~(glyphcmds : char list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_uint8 int_of_pict_op_enum buf op;
    encode_picture buf src;
    encode_picture buf dst;
    encode_pictformat buf mask_format;
    encode_glyphset buf glyphset;
    encode_int16 buf src_x;
    encode_int16 buf src_y;
    buf

  let fill_rectangles ~(op : pict_op_enum) ~(dst : picture) ~(color : color)
      ~(rects : Xproto.rectangle list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_uint8 int_of_pict_op_enum buf op;
    encode_picture buf dst;
    encode_color buf color;
    buf

  let create_cursor ~(cid : Xproto.cursor) ~(source : picture) ~(x : int)
      ~(y : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_cursor buf cid;
    encode_picture buf source;
    encode_uint16 buf x;
    encode_uint16 buf y;
    buf

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

  let encode_transform buf
      {
        matrix11;
        matrix12;
        matrix13;
        matrix21;
        matrix22;
        matrix23;
        matrix31;
        matrix32;
        matrix33;
      } =
    encode_fixed buf matrix11;
    encode_fixed buf matrix12;
    encode_fixed buf matrix13;
    encode_fixed buf matrix21;
    encode_fixed buf matrix22;
    encode_fixed buf matrix23;
    encode_fixed buf matrix31;
    encode_fixed buf matrix32;
    encode_fixed buf matrix33;
    ignore buf

  let set_picture_transform ~(picture : picture) ~(transform : transform) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_picture buf picture;
    encode_transform buf transform;
    buf

  type query_filters_reply = { aliases : int list; filters : Xproto.str list }

  let query_filters ~(drawable : Xproto.drawable) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_drawable buf drawable;
    buf

  let set_picture_filter ~(picture : picture) ~(filter : char list)
      ~(values : fixed list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_picture buf picture;
    encode_uint16 buf (List.length filter);
    encode_list encode_char buf filter;
    buf

  type animcursorelt = { cursor : Xproto.cursor; delay : int32 }

  let decode_animcursorelt buf ~at : (animcursorelt * int) option =
    let orig = at in
    let* cursor, at = Xproto.decode_cursor buf ~at in
    let* delay, at = decode_int32 buf ~at in
    ignore orig;
    Some ({ cursor; delay }, at)

  let encode_animcursorelt buf { cursor; delay } =
    Xproto.encode_cursor buf cursor;
    encode_int32 buf delay;
    ignore buf

  let create_anim_cursor ~(cid : Xproto.cursor) ~(cursors : animcursorelt list)
      () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_cursor buf cid;
    buf

  type spanfix = { l : fixed; r : fixed; y : fixed }

  let decode_spanfix buf ~at : (spanfix * int) option =
    let orig = at in
    let* l, at = decode_fixed buf ~at in
    let* r, at = decode_fixed buf ~at in
    let* y, at = decode_fixed buf ~at in
    ignore orig;
    Some ({ l; r; y }, at)

  let encode_spanfix buf { l; r; y } =
    encode_fixed buf l;
    encode_fixed buf r;
    encode_fixed buf y;
    ignore buf

  type trap = { top : spanfix; bot : spanfix }

  let decode_trap buf ~at : (trap * int) option =
    let orig = at in
    let* top, at = decode_spanfix buf ~at in
    let* bot, at = decode_spanfix buf ~at in
    ignore orig;
    Some ({ top; bot }, at)

  let encode_trap buf { top; bot } =
    encode_spanfix buf top;
    encode_spanfix buf bot;
    ignore buf

  let add_traps ~(picture : picture) ~(x_off : int) ~(y_off : int)
      ~(traps : trap list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_picture buf picture;
    encode_int16 buf x_off;
    encode_int16 buf y_off;
    buf

  let create_solid_fill ~(picture : picture) ~(color : color) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_picture buf picture;
    encode_color buf color;
    buf

  let create_linear_gradient ~(picture : picture) ~(p1 : pointfix)
      ~(p2 : pointfix) ~(stops : fixed list) ~(colors : color list) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_picture buf picture;
    encode_pointfix buf p1;
    encode_pointfix buf p2;
    encode_int32 buf (Int32.of_int (List.length stops));
    encode_list encode_fixed buf stops;
    encode_list encode_color buf colors;
    buf

  let create_radial_gradient ~(picture : picture) ~(inner : pointfix)
      ~(outer : pointfix) ~(inner_radius : fixed) ~(outer_radius : fixed)
      ~(stops : fixed list) ~(colors : color list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_picture buf picture;
    encode_pointfix buf inner;
    encode_pointfix buf outer;
    encode_fixed buf inner_radius;
    encode_fixed buf outer_radius;
    encode_int32 buf (Int32.of_int (List.length stops));
    encode_list encode_fixed buf stops;
    encode_list encode_color buf colors;
    buf

  let create_conical_gradient ~(picture : picture) ~(center : pointfix)
      ~(angle : fixed) ~(stops : fixed list) ~(colors : color list) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_picture buf picture;
    encode_pointfix buf center;
    encode_fixed buf angle;
    encode_int32 buf (Int32.of_int (List.length stops));
    encode_list encode_fixed buf stops;
    encode_list encode_color buf colors;
    buf
end
[@@warning "-27"]

module Shape = struct
  type op = int

  let decode_op = decode_uint8

  let encode_op = encode_uint8

  type kind = int

  let decode_kind = decode_uint8

  let encode_kind = encode_uint8

  type so_enum = [ `Set | `Union | `Intersect | `Subtract | `Invert ]

  let so_enum_of_int : int -> so_enum option = function
    | 0 -> Some `Set
    | 1 -> Some `Union
    | 2 -> Some `Intersect
    | 3 -> Some `Subtract
    | 4 -> Some `Invert
    | _ -> None

  let int_of_so_enum : so_enum -> int = function
    | `Set -> 0
    | `Union -> 1
    | `Intersect -> 2
    | `Subtract -> 3
    | `Invert -> 4

  type sk_enum = [ `Bounding | `Clip | `Input ]

  let sk_enum_of_int : int -> sk_enum option = function
    | 0 -> Some `Bounding
    | 1 -> Some `Clip
    | 2 -> Some `Input
    | _ -> None

  let int_of_sk_enum : sk_enum -> int = function
    | `Bounding -> 0
    | `Clip -> 1
    | `Input -> 2

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

  let query_version () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  let rectangles ~(operation : so_enum) ~(destination_kind : sk_enum)
      ~(ordering : Xproto.clip_ordering_enum)
      ~(destination_window : Xproto.window) ~(x_offset : int) ~(y_offset : int)
      ~(rectangles : Xproto.rectangle list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_op int_of_so_enum buf operation;
    encode_to_int encode_kind int_of_sk_enum buf destination_kind;
    encode_to_int encode_char
      (fun x -> Char.unsafe_chr (Xproto.int_of_clip_ordering_enum x))
      buf ordering;
    Xproto.encode_window buf destination_window;
    encode_int16 buf x_offset;
    encode_int16 buf y_offset;
    buf

  let mask ~(operation : so_enum) ~(destination_kind : sk_enum)
      ~(destination_window : Xproto.window) ~(x_offset : int) ~(y_offset : int)
      ~(source_bitmap : (Xproto.pixmap_enum, Xproto.pixmap) alt) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_op int_of_so_enum buf operation;
    encode_to_int encode_kind int_of_sk_enum buf destination_kind;
    Xproto.encode_window buf destination_window;
    encode_int16 buf x_offset;
    encode_int16 buf y_offset;
    encode_alt Xproto.encode_pixmap Xproto.int_of_pixmap_enum
      Xproto.encode_pixmap buf source_bitmap;
    buf

  let combine ~(operation : so_enum) ~(destination_kind : sk_enum)
      ~(source_kind : sk_enum) ~(destination_window : Xproto.window)
      ~(x_offset : int) ~(y_offset : int) ~(source_window : Xproto.window) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_op int_of_so_enum buf operation;
    encode_to_int encode_kind int_of_sk_enum buf destination_kind;
    encode_to_int encode_kind int_of_sk_enum buf source_kind;
    Xproto.encode_window buf destination_window;
    encode_int16 buf x_offset;
    encode_int16 buf y_offset;
    Xproto.encode_window buf source_window;
    buf

  let offset ~(destination_kind : sk_enum) ~(destination_window : Xproto.window)
      ~(x_offset : int) ~(y_offset : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_kind int_of_sk_enum buf destination_kind;
    Xproto.encode_window buf destination_window;
    encode_int16 buf x_offset;
    encode_int16 buf y_offset;
    buf

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

  let query_extents ~(destination_window : Xproto.window) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf destination_window;
    buf

  let select_input ~(destination_window : Xproto.window) ~(enable : bool) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf destination_window;
    encode_bool buf enable;
    buf

  type input_selected_reply = { enabled : bool }

  let input_selected ~(destination_window : Xproto.window) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf destination_window;
    buf

  type get_rectangles_reply = {
    ordering : Xproto.clip_ordering_enum;
    rectangles : Xproto.rectangle list;
  }

  let get_rectangles ~(window : Xproto.window) ~(source_kind : sk_enum) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    encode_to_int encode_kind int_of_sk_enum buf source_kind;
    buf
end
[@@warning "-27"]

module Xfixes = struct
  type query_version_reply = { major_version : int32; minor_version : int32 }

  let query_version ~(client_major_version : int32)
      ~(client_minor_version : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf client_major_version;
    encode_int32 buf client_minor_version;
    buf

  type save_set_mode_enum = [ `Insert | `Delete ]

  let save_set_mode_enum_of_int : int -> save_set_mode_enum option = function
    | 0 -> Some `Insert
    | 1 -> Some `Delete
    | _ -> None

  let int_of_save_set_mode_enum : save_set_mode_enum -> int = function
    | `Insert -> 0
    | `Delete -> 1

  type save_set_target_enum = [ `Nearest | `Root ]

  let save_set_target_enum_of_int : int -> save_set_target_enum option =
    function
    | 0 -> Some `Nearest
    | 1 -> Some `Root
    | _ -> None

  let int_of_save_set_target_enum : save_set_target_enum -> int = function
    | `Nearest -> 0
    | `Root -> 1

  type save_set_mapping_enum = [ `Map | `Unmap ]

  let save_set_mapping_enum_of_int : int -> save_set_mapping_enum option =
    function
    | 0 -> Some `Map
    | 1 -> Some `Unmap
    | _ -> None

  let int_of_save_set_mapping_enum : save_set_mapping_enum -> int = function
    | `Map -> 0
    | `Unmap -> 1

  let change_save_set ~(mode : save_set_mode_enum)
      ~(target : save_set_target_enum) ~(map : save_set_mapping_enum)
      ~(window : Xproto.window) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_char
      (fun x -> Char.unsafe_chr (int_of_save_set_mode_enum x))
      buf mode;
    encode_to_int encode_char
      (fun x -> Char.unsafe_chr (int_of_save_set_target_enum x))
      buf target;
    encode_to_int encode_char
      (fun x -> Char.unsafe_chr (int_of_save_set_mapping_enum x))
      buf map;
    Xproto.encode_window buf window;
    buf

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

  let int_of_selection_event_enum : selection_event_enum -> int = function
    | `Set_selection_owner -> 0
    | `Selection_window_destroy -> 1
    | `Selection_client_close -> 2

  type selection_event_mask_mask =
    [ `Set_selection_owner
    | `Selection_window_destroy
    | `Selection_client_close ]
    list

  let selection_event_mask_mask_flags =
    [
      (`Set_selection_owner, 0);
      (`Selection_window_destroy, 1);
      (`Selection_client_close, 2);
    ]

  let decode_selection_event_mask_mask i =
    List.filter_map
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      selection_event_mask_mask_flags

  let int_of_selection_event_mask_mask flags =
    List.fold_left
      (fun acc flag ->
        let code =
          match flag with
          | `Set_selection_owner -> 0
          | `Selection_window_destroy -> 1
          | `Selection_client_close -> 2
        in
        acc lor (1 lsl code))
      0 flags

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
      Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    Xproto.encode_atom buf selection;
    encode_to_int encode_int32
      (fun x -> Int32.of_int (int_of_selection_event_mask_mask x))
      buf event_mask;
    buf

  type cursor_notify_enum = [ `Display_cursor ]

  let cursor_notify_enum_of_int : int -> cursor_notify_enum option = function
    | 0 -> Some `Display_cursor
    | _ -> None

  let int_of_cursor_notify_enum : cursor_notify_enum -> int = function
    | `Display_cursor -> 0

  type cursor_notify_mask_mask = [ `Display_cursor ] list

  let cursor_notify_mask_mask_flags = [ (`Display_cursor, 0) ]

  let decode_cursor_notify_mask_mask i =
    List.filter_map
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      cursor_notify_mask_mask_flags

  let int_of_cursor_notify_mask_mask flags =
    List.fold_left
      (fun acc flag ->
        let code = match flag with `Display_cursor -> 0 in
        acc lor (1 lsl code))
      0 flags

  type cursor_notify_event = {
    subtype : cursor_notify_enum;
    window : Xproto.window;
    cursor_serial : int32;
    timestamp : Xproto.timestamp;
    name : (Xproto.atom_enum, Xproto.atom) alt;
  }

  let select_cursor_input ~(window : Xproto.window)
      ~(event_mask : cursor_notify_mask_mask) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    encode_to_int encode_int32
      (fun x -> Int32.of_int (int_of_cursor_notify_mask_mask x))
      buf event_mask;
    buf

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

  let get_cursor_image () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  type region = xid

  let decode_region = decode_xid

  let encode_region = encode_xid

  type bad_region_error = unit

  type region_enum = [ `None ]

  let region_enum_of_int : int -> region_enum option = function
    | 0 -> Some `None
    | _ -> None

  let int_of_region_enum : region_enum -> int = function `None -> 0

  let create_region ~(region : region) ~(rectangles : Xproto.rectangle list) ()
      : Buffer.t =
    let buf = Buffer.create 16 in
    encode_region buf region;
    buf

  let create_region_from_bitmap ~(region : region) ~(bitmap : Xproto.pixmap) ()
      : Buffer.t =
    let buf = Buffer.create 16 in
    encode_region buf region;
    Xproto.encode_pixmap buf bitmap;
    buf

  let create_region_from_window ~(region : region) ~(window : Xproto.window)
      ~(kind : Shape.sk_enum) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_region buf region;
    Xproto.encode_window buf window;
    encode_to_int Shape.encode_kind Shape.int_of_sk_enum buf kind;
    buf

  let create_region_from_gc ~(region : region) ~(gc : Xproto.gcontext) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_region buf region;
    Xproto.encode_gcontext buf gc;
    buf

  let create_region_from_picture ~(region : region) ~(picture : Render.picture)
      () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_region buf region;
    Render.encode_picture buf picture;
    buf

  let destroy_region ~(region : region) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_region buf region;
    buf

  let set_region ~(region : region) ~(rectangles : Xproto.rectangle list) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_region buf region;
    buf

  let copy_region ~(source : region) ~(destination : region) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_region buf source;
    encode_region buf destination;
    buf

  let union_region ~(source1 : region) ~(source2 : region)
      ~(destination : region) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_region buf source1;
    encode_region buf source2;
    encode_region buf destination;
    buf

  let intersect_region ~(source1 : region) ~(source2 : region)
      ~(destination : region) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_region buf source1;
    encode_region buf source2;
    encode_region buf destination;
    buf

  let subtract_region ~(source1 : region) ~(source2 : region)
      ~(destination : region) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_region buf source1;
    encode_region buf source2;
    encode_region buf destination;
    buf

  let invert_region ~(source : region) ~(bounds : Xproto.rectangle)
      ~(destination : region) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_region buf source;
    Xproto.encode_rectangle buf bounds;
    encode_region buf destination;
    buf

  let translate_region ~(region : region) ~(dx : int) ~(dy : int) () : Buffer.t
      =
    let buf = Buffer.create 16 in
    encode_region buf region;
    encode_int16 buf dx;
    encode_int16 buf dy;
    buf

  let region_extents ~(source : region) ~(destination : region) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_region buf source;
    encode_region buf destination;
    buf

  type fetch_region_reply = {
    extents : Xproto.rectangle;
    rectangles : Xproto.rectangle list;
  }

  let fetch_region ~(region : region) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_region buf region;
    buf

  let set_gc_clip_region ~(gc : Xproto.gcontext)
      ~(region : (region_enum, region) alt) ~(x_origin : int) ~(y_origin : int)
      () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_gcontext buf gc;
    encode_alt encode_region int_of_region_enum encode_region buf region;
    encode_int16 buf x_origin;
    encode_int16 buf y_origin;
    buf

  let set_window_shape_region ~(dest : Xproto.window)
      ~(dest_kind : Shape.sk_enum) ~(x_offset : int) ~(y_offset : int)
      ~(region : (region_enum, region) alt) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf dest;
    encode_to_int Shape.encode_kind Shape.int_of_sk_enum buf dest_kind;
    encode_int16 buf x_offset;
    encode_int16 buf y_offset;
    encode_alt encode_region int_of_region_enum encode_region buf region;
    buf

  let set_picture_clip_region ~(picture : Render.picture)
      ~(region : (region_enum, region) alt) ~(x_origin : int) ~(y_origin : int)
      () : Buffer.t =
    let buf = Buffer.create 16 in
    Render.encode_picture buf picture;
    encode_alt encode_region int_of_region_enum encode_region buf region;
    encode_int16 buf x_origin;
    encode_int16 buf y_origin;
    buf

  let set_cursor_name ~(cursor : Xproto.cursor) ~(name : char list) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_cursor buf cursor;
    encode_uint16 buf (List.length name);
    encode_list encode_char buf name;
    buf

  type get_cursor_name_reply = {
    atom : (Xproto.atom_enum, Xproto.atom) alt;
    name : char list;
  }

  let get_cursor_name ~(cursor : Xproto.cursor) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_cursor buf cursor;
    buf

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

  let get_cursor_image_and_name () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  let change_cursor ~(source : Xproto.cursor) ~(destination : Xproto.cursor) ()
      : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_cursor buf source;
    Xproto.encode_cursor buf destination;
    buf

  let change_cursor_by_name ~(src : Xproto.cursor) ~(name : char list) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_cursor buf src;
    encode_uint16 buf (List.length name);
    encode_list encode_char buf name;
    buf

  let expand_region ~(source : region) ~(destination : region) ~(left : int)
      ~(right : int) ~(top : int) ~(bottom : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_region buf source;
    encode_region buf destination;
    encode_uint16 buf left;
    encode_uint16 buf right;
    encode_uint16 buf top;
    encode_uint16 buf bottom;
    buf

  let hide_cursor ~(window : Xproto.window) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    buf

  let show_cursor ~(window : Xproto.window) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    buf

  type barrier = xid

  let decode_barrier = decode_xid

  let encode_barrier = encode_xid

  type barrier_directions_mask =
    [ `Positive_x | `Positive_y | `Negative_x | `Negative_y ] list

  let barrier_directions_mask_flags =
    [ (`Positive_x, 0); (`Positive_y, 1); (`Negative_x, 2); (`Negative_y, 3) ]

  let decode_barrier_directions_mask i =
    List.filter_map
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      barrier_directions_mask_flags

  let int_of_barrier_directions_mask flags =
    List.fold_left
      (fun acc flag ->
        let code =
          match flag with
          | `Positive_x -> 0
          | `Positive_y -> 1
          | `Negative_x -> 2
          | `Negative_y -> 3
        in
        acc lor (1 lsl code))
      0 flags

  let create_pointer_barrier ~(barrier : barrier) ~(window : Xproto.window)
      ~(x1 : int) ~(y1 : int) ~(x2 : int) ~(y2 : int)
      ~(directions : barrier_directions_mask) ~(devices : int list) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_barrier buf barrier;
    Xproto.encode_window buf window;
    encode_uint16 buf x1;
    encode_uint16 buf y1;
    encode_uint16 buf x2;
    encode_uint16 buf y2;
    encode_to_int encode_int32
      (fun x -> Int32.of_int (int_of_barrier_directions_mask x))
      buf directions;
    encode_uint16 buf (List.length devices);
    encode_list encode_uint16 buf devices;
    buf

  let delete_pointer_barrier ~(barrier : barrier) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_barrier buf barrier;
    buf
end
[@@warning "-27"]

module Composite = struct
  type redirect_enum = [ `Automatic | `Manual ]

  let redirect_enum_of_int : int -> redirect_enum option = function
    | 0 -> Some `Automatic
    | 1 -> Some `Manual
    | _ -> None

  let int_of_redirect_enum : redirect_enum -> int = function
    | `Automatic -> 0
    | `Manual -> 1

  type query_version_reply = { major_version : int32; minor_version : int32 }

  let query_version ~(client_major_version : int32)
      ~(client_minor_version : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf client_major_version;
    encode_int32 buf client_minor_version;
    buf

  let redirect_window ~(window : Xproto.window) ~(update : redirect_enum) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    encode_to_int encode_uint8 int_of_redirect_enum buf update;
    buf

  let redirect_subwindows ~(window : Xproto.window) ~(update : redirect_enum) ()
      : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    encode_to_int encode_uint8 int_of_redirect_enum buf update;
    buf

  let unredirect_window ~(window : Xproto.window) ~(update : redirect_enum) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    encode_to_int encode_uint8 int_of_redirect_enum buf update;
    buf

  let unredirect_subwindows ~(window : Xproto.window) ~(update : redirect_enum)
      () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    encode_to_int encode_uint8 int_of_redirect_enum buf update;
    buf

  let create_region_from_border_clip ~(region : Xfixes.region)
      ~(window : Xproto.window) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xfixes.encode_region buf region;
    Xproto.encode_window buf window;
    buf

  let name_window_pixmap ~(window : Xproto.window) ~(pixmap : Xproto.pixmap) ()
      : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    Xproto.encode_pixmap buf pixmap;
    buf

  type get_overlay_window_reply = { overlay_win : Xproto.window }

  let get_overlay_window ~(window : Xproto.window) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    buf

  let release_overlay_window ~(window : Xproto.window) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    buf
end
[@@warning "-27"]

module Damage = struct
  type damage = xid

  let decode_damage = decode_xid

  let encode_damage = encode_xid

  type report_level_enum =
    [ `Raw_rectangles | `Delta_rectangles | `Bounding_box | `Non_empty ]

  let report_level_enum_of_int : int -> report_level_enum option = function
    | 0 -> Some `Raw_rectangles
    | 1 -> Some `Delta_rectangles
    | 2 -> Some `Bounding_box
    | 3 -> Some `Non_empty
    | _ -> None

  let int_of_report_level_enum : report_level_enum -> int = function
    | `Raw_rectangles -> 0
    | `Delta_rectangles -> 1
    | `Bounding_box -> 2
    | `Non_empty -> 3

  type bad_damage_error = unit

  type query_version_reply = { major_version : int32; minor_version : int32 }

  let query_version ~(client_major_version : int32)
      ~(client_minor_version : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf client_major_version;
    encode_int32 buf client_minor_version;
    buf

  let create ~(damage : damage) ~(drawable : Xproto.drawable)
      ~(level : report_level_enum) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_damage buf damage;
    Xproto.encode_drawable buf drawable;
    encode_to_int encode_uint8 int_of_report_level_enum buf level;
    buf

  let destroy ~(damage : damage) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_damage buf damage;
    buf

  let subtract ~(damage : damage)
      ~(repair : (Xfixes.region_enum, Xfixes.region) alt)
      ~(parts : (Xfixes.region_enum, Xfixes.region) alt) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_damage buf damage;
    encode_alt Xfixes.encode_region Xfixes.int_of_region_enum
      Xfixes.encode_region buf repair;
    encode_alt Xfixes.encode_region Xfixes.int_of_region_enum
      Xfixes.encode_region buf parts;
    buf

  let add ~(drawable : Xproto.drawable) ~(region : Xfixes.region) () : Buffer.t
      =
    let buf = Buffer.create 16 in
    Xproto.encode_drawable buf drawable;
    Xfixes.encode_region buf region;
    buf

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
      : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint16 buf client_major_version;
    encode_uint16 buf client_minor_version;
    buf

  type capable_reply = { capable : bool }

  let capable () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  type get_timeouts_reply = {
    standby_timeout : int;
    suspend_timeout : int;
    off_timeout : int;
  }

  let get_timeouts () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  let set_timeouts ~(standby_timeout : int) ~(suspend_timeout : int)
      ~(off_timeout : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint16 buf standby_timeout;
    encode_uint16 buf suspend_timeout;
    encode_uint16 buf off_timeout;
    buf

  let enable () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  let disable () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  type dpms_mode_enum = [ `On | `Standby | `Suspend | `Off ]

  let dpms_mode_enum_of_int : int -> dpms_mode_enum option = function
    | 0 -> Some `On
    | 1 -> Some `Standby
    | 2 -> Some `Suspend
    | 3 -> Some `Off
    | _ -> None

  let int_of_dpms_mode_enum : dpms_mode_enum -> int = function
    | `On -> 0
    | `Standby -> 1
    | `Suspend -> 2
    | `Off -> 3

  let force_level ~(power_level : dpms_mode_enum) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_uint16 int_of_dpms_mode_enum buf power_level;
    buf

  type info_reply = { power_level : dpms_mode_enum; state : bool }

  let info () : Buffer.t =
    let buf = Buffer.create 16 in
    buf
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

  let int_of_attachment_enum : attachment_enum -> int = function
    | `Buffer_front_left -> 0
    | `Buffer_back_left -> 1
    | `Buffer_front_right -> 2
    | `Buffer_back_right -> 3
    | `Buffer_depth -> 4
    | `Buffer_stencil -> 5
    | `Buffer_accum -> 6
    | `Buffer_fake_front_left -> 7
    | `Buffer_fake_front_right -> 8
    | `Buffer_depth_stencil -> 9
    | `Buffer_hiz -> 10

  type driver_type_enum = [ `Dri | `Vdpau ]

  let driver_type_enum_of_int : int -> driver_type_enum option = function
    | 0 -> Some `Dri
    | 1 -> Some `Vdpau
    | _ -> None

  let int_of_driver_type_enum : driver_type_enum -> int = function
    | `Dri -> 0
    | `Vdpau -> 1

  type event_type_enum = [ `Exchange_complete | `Blit_complete | `Flip_complete ]

  let event_type_enum_of_int : int -> event_type_enum option = function
    | 1 -> Some `Exchange_complete
    | 2 -> Some `Blit_complete
    | 3 -> Some `Flip_complete
    | _ -> None

  let int_of_event_type_enum : event_type_enum -> int = function
    | `Exchange_complete -> 1
    | `Blit_complete -> 2
    | `Flip_complete -> 3

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

  let encode_dri2_buffer buf { attachment; name; pitch; cpp; flags } =
    encode_to_int encode_int32
      (fun x -> Int32.of_int (int_of_attachment_enum x))
      buf attachment;
    encode_int32 buf name;
    encode_int32 buf pitch;
    encode_int32 buf cpp;
    encode_int32 buf flags;
    ignore buf

  type attach_format = { attachment : attachment_enum; format : int32 }

  let decode_attach_format buf ~at : (attach_format * int) option =
    let orig = at in
    let* attachment, at =
      decode_enum decode_int32 Int32.to_int attachment_enum_of_int buf ~at
    in
    let* format, at = decode_int32 buf ~at in
    ignore orig;
    Some ({ attachment; format }, at)

  let encode_attach_format buf { attachment; format } =
    encode_to_int encode_int32
      (fun x -> Int32.of_int (int_of_attachment_enum x))
      buf attachment;
    encode_int32 buf format;
    ignore buf

  type query_version_reply = { major_version : int32; minor_version : int32 }

  let query_version ~(major_version : int32) ~(minor_version : int32) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf major_version;
    encode_int32 buf minor_version;
    buf

  type connect_reply = {
    driver_name : char list;
    alignment_pad : char list;
    device_name : char list;
  }

  let connect ~(window : Xproto.window) ~(driver_type : driver_type_enum) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    encode_to_int encode_int32
      (fun x -> Int32.of_int (int_of_driver_type_enum x))
      buf driver_type;
    buf

  type authenticate_reply = { authenticated : int32 }

  let authenticate ~(window : Xproto.window) ~(magic : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    encode_int32 buf magic;
    buf

  let create_drawable ~(drawable : Xproto.drawable) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_drawable buf drawable;
    buf

  let destroy_drawable ~(drawable : Xproto.drawable) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_drawable buf drawable;
    buf

  type get_buffers_reply = {
    width : int32;
    height : int32;
    buffers : dri2_buffer list;
  }

  let get_buffers ~(drawable : Xproto.drawable) ~(attachments : int32 list) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_drawable buf drawable;
    encode_int32 buf (Int32.of_int (List.length attachments));
    encode_list encode_int32 buf attachments;
    buf

  type copy_region_reply = unit

  let copy_region ~(drawable : Xproto.drawable) ~(region : int32)
      ~(dest : int32) ~(src : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_drawable buf drawable;
    encode_int32 buf region;
    encode_int32 buf dest;
    encode_int32 buf src;
    buf

  type get_buffers_with_format_reply = {
    width : int32;
    height : int32;
    buffers : dri2_buffer list;
  }

  let get_buffers_with_format ~(drawable : Xproto.drawable)
      ~(attachments : attach_format list) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_drawable buf drawable;
    encode_int32 buf (Int32.of_int (List.length attachments));
    encode_list encode_attach_format buf attachments;
    buf

  type swap_buffers_reply = { swap_hi : int32; swap_lo : int32 }

  let swap_buffers ~(drawable : Xproto.drawable) ~(target_msc_hi : int32)
      ~(target_msc_lo : int32) ~(divisor_hi : int32) ~(divisor_lo : int32)
      ~(remainder_hi : int32) ~(remainder_lo : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_drawable buf drawable;
    encode_int32 buf target_msc_hi;
    encode_int32 buf target_msc_lo;
    encode_int32 buf divisor_hi;
    encode_int32 buf divisor_lo;
    encode_int32 buf remainder_hi;
    encode_int32 buf remainder_lo;
    buf

  type get_msc_reply = {
    ust_hi : int32;
    ust_lo : int32;
    msc_hi : int32;
    msc_lo : int32;
    sbc_hi : int32;
    sbc_lo : int32;
  }

  let get_msc ~(drawable : Xproto.drawable) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_drawable buf drawable;
    buf

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
      ~(remainder_hi : int32) ~(remainder_lo : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_drawable buf drawable;
    encode_int32 buf target_msc_hi;
    encode_int32 buf target_msc_lo;
    encode_int32 buf divisor_hi;
    encode_int32 buf divisor_lo;
    encode_int32 buf remainder_hi;
    encode_int32 buf remainder_lo;
    buf

  type wait_sbc_reply = {
    ust_hi : int32;
    ust_lo : int32;
    msc_hi : int32;
    msc_lo : int32;
    sbc_hi : int32;
    sbc_lo : int32;
  }

  let wait_sbc ~(drawable : Xproto.drawable) ~(target_sbc_hi : int32)
      ~(target_sbc_lo : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_drawable buf drawable;
    encode_int32 buf target_sbc_hi;
    encode_int32 buf target_sbc_lo;
    buf

  let swap_interval ~(drawable : Xproto.drawable) ~(interval : int32) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_drawable buf drawable;
    encode_int32 buf interval;
    buf

  type get_param_reply = {
    is_param_recognized : bool;
    value_hi : int32;
    value_lo : int32;
  }

  let get_param ~(drawable : Xproto.drawable) ~(param : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_drawable buf drawable;
    encode_int32 buf param;
    buf

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
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf major_version;
    encode_int32 buf minor_version;
    buf

  type open_reply = { nfd : int; device_fd : file_descr }

  let open_ ~(drawable : Xproto.drawable) ~(provider : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_drawable buf drawable;
    encode_int32 buf provider;
    buf

  let pixmap_from_buffer ~(pixmap : Xproto.pixmap) ~(drawable : Xproto.drawable)
      ~(size : int32) ~(width : int) ~(height : int) ~(stride : int)
      ~(depth : int) ~(bpp : int) ~(pixmap_fd : file_descr) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_pixmap buf pixmap;
    Xproto.encode_drawable buf drawable;
    encode_int32 buf size;
    encode_uint16 buf width;
    encode_uint16 buf height;
    encode_uint16 buf stride;
    encode_uint8 buf depth;
    encode_uint8 buf bpp;
    buf

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

  let buffer_from_pixmap ~(pixmap : Xproto.pixmap) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_pixmap buf pixmap;
    buf

  let fence_from_fd ~(drawable : Xproto.drawable) ~(fence : int32)
      ~(initially_triggered : bool) ~(fence_fd : file_descr) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_drawable buf drawable;
    encode_int32 buf fence;
    encode_bool buf initially_triggered;
    buf

  type fd_from_fence_reply = { nfd : int; fence_fd : file_descr }

  let fd_from_fence ~(drawable : Xproto.drawable) ~(fence : int32) () : Buffer.t
      =
    let buf = Buffer.create 16 in
    Xproto.encode_drawable buf drawable;
    encode_int32 buf fence;
    buf

  type get_supported_modifiers_reply = {
    window_modifiers : int64 list;
    screen_modifiers : int64 list;
  }

  let get_supported_modifiers ~(window : int32) ~(depth : int) ~(bpp : int) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf window;
    encode_uint8 buf depth;
    encode_uint8 buf bpp;
    buf

  let pixmap_from_buffers ~(pixmap : Xproto.pixmap) ~(window : Xproto.window)
      ~(width : int) ~(height : int) ~(stride0 : int32) ~(offset0 : int32)
      ~(stride1 : int32) ~(offset1 : int32) ~(stride2 : int32)
      ~(offset2 : int32) ~(stride3 : int32) ~(offset3 : int32) ~(depth : int)
      ~(bpp : int) ~(modifier : int64) ~(buffers : file_descr list) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_pixmap buf pixmap;
    Xproto.encode_window buf window;
    encode_uint8 buf (List.length buffers);
    encode_uint16 buf width;
    encode_uint16 buf height;
    encode_int32 buf stride0;
    encode_int32 buf offset0;
    encode_int32 buf stride1;
    encode_int32 buf offset1;
    encode_int32 buf stride2;
    encode_int32 buf offset2;
    encode_int32 buf stride3;
    encode_int32 buf offset3;
    encode_uint8 buf depth;
    encode_uint8 buf bpp;
    encode_int64 buf modifier;
    encode_list encode_file_descr buf buffers;
    buf

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

  let buffers_from_pixmap ~(pixmap : Xproto.pixmap) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_pixmap buf pixmap;
    buf
end
[@@warning "-27"]

module Ge = struct
  type query_version_reply = { major_version : int; minor_version : int }

  let query_version ~(client_major_version : int) ~(client_minor_version : int)
      () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint16 buf client_major_version;
    encode_uint16 buf client_minor_version;
    buf
end
[@@warning "-27"]

module Glx = struct
  type pixmap = xid

  let decode_pixmap = decode_xid

  let encode_pixmap = encode_xid

  type context = xid

  let decode_context = decode_xid

  let encode_context = encode_xid

  type pbuffer = xid

  let decode_pbuffer = decode_xid

  let encode_pbuffer = encode_xid

  type window = xid

  let decode_window = decode_xid

  let encode_window = encode_xid

  type fbconfig = xid

  let decode_fbconfig = decode_xid

  let encode_fbconfig = encode_xid

  type drawable = xid

  let decode_drawable = decode_xid

  let encode_drawable = encode_xid

  type float32 = float

  let decode_float32 = decode_float

  let encode_float32 = encode_float

  type float64 = float

  let decode_float64 = decode_float

  let encode_float64 = encode_float

  type bool32 = int32

  let decode_bool32 = decode_int32

  let encode_bool32 = encode_int32

  type context_tag = int32

  let decode_context_tag = decode_int32

  let encode_context_tag = encode_int32

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

  let int_of_pbcet_enum : pbcet_enum -> int = function
    | `Damaged -> 32791
    | `Saved -> 32792

  type pbcdt_enum = [ `Window | `Pbuffer ]

  let pbcdt_enum_of_int : int -> pbcdt_enum option = function
    | 32793 -> Some `Window
    | 32794 -> Some `Pbuffer
    | _ -> None

  let int_of_pbcdt_enum : pbcdt_enum -> int = function
    | `Window -> 32793
    | `Pbuffer -> 32794

  let render ~(context_tag : context_tag) ~(data : char list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    buf

  let render_large ~(context_tag : context_tag) ~(request_num : int)
      ~(request_total : int) ~(data : char list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_uint16 buf request_num;
    encode_uint16 buf request_total;
    encode_int32 buf (Int32.of_int (List.length data));
    encode_list encode_char buf data;
    buf

  let create_context ~(context : context) ~(visual : Xproto.visualid)
      ~(screen : int32) ~(share_list : context) ~(is_direct : bool) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_context buf context;
    Xproto.encode_visualid buf visual;
    encode_int32 buf screen;
    encode_context buf share_list;
    encode_bool buf is_direct;
    buf

  let destroy_context ~(context : context) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context buf context;
    buf

  type make_current_reply = { context_tag : context_tag }

  let make_current ~(drawable : drawable) ~(context : context)
      ~(old_context_tag : context_tag) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_drawable buf drawable;
    encode_context buf context;
    encode_context_tag buf old_context_tag;
    buf

  type is_direct_reply = { is_direct : bool }

  let is_direct ~(context : context) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context buf context;
    buf

  type query_version_reply = { major_version : int32; minor_version : int32 }

  let query_version ~(major_version : int32) ~(minor_version : int32) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf major_version;
    encode_int32 buf minor_version;
    buf

  let wait_gl ~(context_tag : context_tag) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    buf

  let wait_x ~(context_tag : context_tag) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    buf

  let copy_context ~(src : context) ~(dest : context) ~(mask : int32)
      ~(src_context_tag : context_tag) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context buf src;
    encode_context buf dest;
    encode_int32 buf mask;
    encode_context_tag buf src_context_tag;
    buf

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

  let gc_mask_flags =
    [
      (`Gl_current_bit, 0);
      (`Gl_point_bit, 1);
      (`Gl_line_bit, 2);
      (`Gl_polygon_bit, 3);
      (`Gl_polygon_stipple_bit, 4);
      (`Gl_pixel_mode_bit, 5);
      (`Gl_lighting_bit, 6);
      (`Gl_fog_bit, 7);
      (`Gl_depth_buffer_bit, 8);
      (`Gl_accum_buffer_bit, 9);
      (`Gl_stencil_buffer_bit, 10);
      (`Gl_viewport_bit, 11);
      (`Gl_transform_bit, 12);
      (`Gl_enable_bit, 13);
      (`Gl_color_buffer_bit, 14);
      (`Gl_hint_bit, 15);
      (`Gl_eval_bit, 16);
      (`Gl_list_bit, 17);
      (`Gl_texture_bit, 18);
      (`Gl_scissor_bit, 19);
    ]

  let gc_mask_values = [ (`Gl_all_attrib_bits, 16777215) ]

  let decode_gc_mask i =
    match List.find_opt (fun (value, v) -> v = i) gc_mask_values with
    | Some (value, _) -> V value
    | None ->
        F
          (List.filter_map
             (fun (flag, f) ->
               if f land (1 lsl i) <> 0 then Some flag else None)
             gc_mask_flags)

  let int_of_gc_mask = function
    | F flags ->
        List.fold_left
          (fun acc flag ->
            let code =
              match flag with
              | `Gl_current_bit -> 0
              | `Gl_point_bit -> 1
              | `Gl_line_bit -> 2
              | `Gl_polygon_bit -> 3
              | `Gl_polygon_stipple_bit -> 4
              | `Gl_pixel_mode_bit -> 5
              | `Gl_lighting_bit -> 6
              | `Gl_fog_bit -> 7
              | `Gl_depth_buffer_bit -> 8
              | `Gl_accum_buffer_bit -> 9
              | `Gl_stencil_buffer_bit -> 10
              | `Gl_viewport_bit -> 11
              | `Gl_transform_bit -> 12
              | `Gl_enable_bit -> 13
              | `Gl_color_buffer_bit -> 14
              | `Gl_hint_bit -> 15
              | `Gl_eval_bit -> 16
              | `Gl_list_bit -> 17
              | `Gl_texture_bit -> 18
              | `Gl_scissor_bit -> 19
            in
            acc lor (1 lsl code))
          0 flags
    | V `Gl_all_attrib_bits -> 16777215

  let swap_buffers ~(context_tag : context_tag) ~(drawable : drawable) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_drawable buf drawable;
    buf

  let use_x_font ~(context_tag : context_tag) ~(font : Xproto.font)
      ~(first : int32) ~(count : int32) ~(list_base : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    Xproto.encode_font buf font;
    encode_int32 buf first;
    encode_int32 buf count;
    encode_int32 buf list_base;
    buf

  let create_glx_pixmap ~(screen : int32) ~(visual : Xproto.visualid)
      ~(pixmap : Xproto.pixmap) ~(glx_pixmap : pixmap) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf screen;
    Xproto.encode_visualid buf visual;
    Xproto.encode_pixmap buf pixmap;
    encode_pixmap buf glx_pixmap;
    buf

  type get_visual_configs_reply = {
    num_visuals : int32;
    num_properties : int32;
    property_list : int32 list;
  }

  let get_visual_configs ~(screen : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf screen;
    buf

  let destroy_glx_pixmap ~(glx_pixmap : pixmap) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_pixmap buf glx_pixmap;
    buf

  let vendor_private ~(vendor_code : int32) ~(context_tag : context_tag)
      ~(data : char list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf vendor_code;
    encode_context_tag buf context_tag;
    buf

  type vendor_private_with_reply_reply = {
    retval : int32;
    data1 : char list;
    data2 : char list;
  }

  let vendor_private_with_reply ~(vendor_code : int32)
      ~(context_tag : context_tag) ~(data : char list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf vendor_code;
    encode_context_tag buf context_tag;
    buf

  type query_extensions_string_reply = { n : int32 }

  let query_extensions_string ~(screen : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf screen;
    buf

  type query_server_string_reply = { string : char list }

  let query_server_string ~(screen : int32) ~(name : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf screen;
    encode_int32 buf name;
    buf

  let client_info ~(major_version : int32) ~(minor_version : int32)
      ~(string : char list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf major_version;
    encode_int32 buf minor_version;
    encode_int32 buf (Int32.of_int (List.length string));
    encode_list encode_char buf string;
    buf

  type get_fb_configs_reply = {
    num_f_b_configs : int32;
    num_properties : int32;
    property_list : int32 list;
  }

  let get_fb_configs ~(screen : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf screen;
    buf

  let create_pixmap ~(screen : int32) ~(fbconfig : fbconfig)
      ~(pixmap : Xproto.pixmap) ~(glx_pixmap : pixmap) ~(num_attribs : int32)
      ~(attribs : int32 list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf screen;
    encode_fbconfig buf fbconfig;
    Xproto.encode_pixmap buf pixmap;
    encode_pixmap buf glx_pixmap;
    encode_int32 buf num_attribs;
    buf

  let destroy_pixmap ~(glx_pixmap : pixmap) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_pixmap buf glx_pixmap;
    buf

  let create_new_context ~(context : context) ~(fbconfig : fbconfig)
      ~(screen : int32) ~(render_type : int32) ~(share_list : context)
      ~(is_direct : bool) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context buf context;
    encode_fbconfig buf fbconfig;
    encode_int32 buf screen;
    encode_int32 buf render_type;
    encode_context buf share_list;
    encode_bool buf is_direct;
    buf

  type query_context_reply = { num_attribs : int32; attribs : int32 list }

  let query_context ~(context : context) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context buf context;
    buf

  type make_context_current_reply = { context_tag : context_tag }

  let make_context_current ~(old_context_tag : context_tag)
      ~(drawable : drawable) ~(read_drawable : drawable) ~(context : context) ()
      : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf old_context_tag;
    encode_drawable buf drawable;
    encode_drawable buf read_drawable;
    encode_context buf context;
    buf

  let create_pbuffer ~(screen : int32) ~(fbconfig : fbconfig)
      ~(pbuffer : pbuffer) ~(num_attribs : int32) ~(attribs : int32 list) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf screen;
    encode_fbconfig buf fbconfig;
    encode_pbuffer buf pbuffer;
    encode_int32 buf num_attribs;
    buf

  let destroy_pbuffer ~(pbuffer : pbuffer) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_pbuffer buf pbuffer;
    buf

  type get_drawable_attributes_reply = {
    num_attribs : int32;
    attribs : int32 list;
  }

  let get_drawable_attributes ~(drawable : drawable) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_drawable buf drawable;
    buf

  let change_drawable_attributes ~(drawable : drawable) ~(num_attribs : int32)
      ~(attribs : int32 list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_drawable buf drawable;
    encode_int32 buf num_attribs;
    buf

  let create_window ~(screen : int32) ~(fbconfig : fbconfig)
      ~(window : Xproto.window) ~(glx_window : window) ~(num_attribs : int32)
      ~(attribs : int32 list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf screen;
    encode_fbconfig buf fbconfig;
    Xproto.encode_window buf window;
    encode_window buf glx_window;
    encode_int32 buf num_attribs;
    buf

  let delete_window ~(glxwindow : window) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_window buf glxwindow;
    buf

  let set_client_info_arb ~(major_version : int32) ~(minor_version : int32)
      ~(num_versions : int32) ~(gl_versions : int32 list)
      ~(gl_extension_string : char list) ~(glx_extension_string : char list) ()
      : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf major_version;
    encode_int32 buf minor_version;
    encode_int32 buf num_versions;
    encode_int32 buf (Int32.of_int (List.length gl_extension_string));
    encode_int32 buf (Int32.of_int (List.length glx_extension_string));
    encode_list encode_char buf gl_extension_string;
    encode_list encode_char buf glx_extension_string;
    buf

  let create_context_attribs_arb ~(context : context) ~(fbconfig : fbconfig)
      ~(screen : int32) ~(share_list : context) ~(is_direct : bool)
      ~(num_attribs : int32) ~(attribs : int32 list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context buf context;
    encode_fbconfig buf fbconfig;
    encode_int32 buf screen;
    encode_context buf share_list;
    encode_bool buf is_direct;
    encode_int32 buf num_attribs;
    buf

  let set_client_info2_arb ~(major_version : int32) ~(minor_version : int32)
      ~(num_versions : int32) ~(gl_versions : int32 list)
      ~(gl_extension_string : char list) ~(glx_extension_string : char list) ()
      : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf major_version;
    encode_int32 buf minor_version;
    encode_int32 buf num_versions;
    encode_int32 buf (Int32.of_int (List.length gl_extension_string));
    encode_int32 buf (Int32.of_int (List.length glx_extension_string));
    encode_list encode_char buf gl_extension_string;
    encode_list encode_char buf glx_extension_string;
    buf

  let new_list ~(context_tag : context_tag) ~(list : int32) ~(mode : int32) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf list;
    encode_int32 buf mode;
    buf

  let end_list ~(context_tag : context_tag) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    buf

  let delete_lists ~(context_tag : context_tag) ~(list : int32) ~(range : int32)
      () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf list;
    encode_int32 buf range;
    buf

  type gen_lists_reply = { ret_val : int32 }

  let gen_lists ~(context_tag : context_tag) ~(range : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf range;
    buf

  let feedback_buffer ~(context_tag : context_tag) ~(size : int32)
      ~(type_ : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf size;
    encode_int32 buf type_;
    buf

  let select_buffer ~(context_tag : context_tag) ~(size : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf size;
    buf

  type render_mode_reply = {
    ret_val : int32;
    new_mode : int32;
    data : int32 list;
  }

  let render_mode ~(context_tag : context_tag) ~(mode : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf mode;
    buf

  type rm_enum = [ `Gl_render | `Gl_feedback | `Gl_select ]

  let rm_enum_of_int : int -> rm_enum option = function
    | 7168 -> Some `Gl_render
    | 7169 -> Some `Gl_feedback
    | 7170 -> Some `Gl_select
    | _ -> None

  let int_of_rm_enum : rm_enum -> int = function
    | `Gl_render -> 7168
    | `Gl_feedback -> 7169
    | `Gl_select -> 7170

  type finish_reply = unit

  let finish ~(context_tag : context_tag) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    buf

  let pixel_storef ~(context_tag : context_tag) ~(pname : int32)
      ~(datum : float32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf pname;
    encode_float32 buf datum;
    buf

  let pixel_storei ~(context_tag : context_tag) ~(pname : int32)
      ~(datum : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf pname;
    encode_int32 buf datum;
    buf

  type read_pixels_reply = { data : char list }

  let read_pixels ~(context_tag : context_tag) ~(x : int32) ~(y : int32)
      ~(width : int32) ~(height : int32) ~(format : int32) ~(type_ : int32)
      ~(swap_bytes : bool) ~(lsb_first : bool) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf x;
    encode_int32 buf y;
    encode_int32 buf width;
    encode_int32 buf height;
    encode_int32 buf format;
    encode_int32 buf type_;
    encode_bool buf swap_bytes;
    encode_bool buf lsb_first;
    buf

  type get_booleanv_reply = { datum : bool; data : bool list }

  let get_booleanv ~(context_tag : context_tag) ~(pname : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf pname;
    buf

  type get_clip_plane_reply = { data : float64 list }

  let get_clip_plane ~(context_tag : context_tag) ~(plane : int32) () : Buffer.t
      =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf plane;
    buf

  type get_doublev_reply = { datum : float64; data : float64 list }

  let get_doublev ~(context_tag : context_tag) ~(pname : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf pname;
    buf

  type get_error_reply = { error : int32 }

  let get_error ~(context_tag : context_tag) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    buf

  type get_floatv_reply = { datum : float32; data : float32 list }

  let get_floatv ~(context_tag : context_tag) ~(pname : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf pname;
    buf

  type get_integerv_reply = { datum : int32; data : int32 list }

  let get_integerv ~(context_tag : context_tag) ~(pname : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf pname;
    buf

  type get_lightfv_reply = { datum : float32; data : float32 list }

  let get_lightfv ~(context_tag : context_tag) ~(light : int32) ~(pname : int32)
      () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf light;
    encode_int32 buf pname;
    buf

  type get_lightiv_reply = { datum : int32; data : int32 list }

  let get_lightiv ~(context_tag : context_tag) ~(light : int32) ~(pname : int32)
      () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf light;
    encode_int32 buf pname;
    buf

  type get_mapdv_reply = { datum : float64; data : float64 list }

  let get_mapdv ~(context_tag : context_tag) ~(target : int32) ~(query : int32)
      () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf target;
    encode_int32 buf query;
    buf

  type get_mapfv_reply = { datum : float32; data : float32 list }

  let get_mapfv ~(context_tag : context_tag) ~(target : int32) ~(query : int32)
      () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf target;
    encode_int32 buf query;
    buf

  type get_mapiv_reply = { datum : int32; data : int32 list }

  let get_mapiv ~(context_tag : context_tag) ~(target : int32) ~(query : int32)
      () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf target;
    encode_int32 buf query;
    buf

  type get_materialfv_reply = { datum : float32; data : float32 list }

  let get_materialfv ~(context_tag : context_tag) ~(face : int32)
      ~(pname : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf face;
    encode_int32 buf pname;
    buf

  type get_materialiv_reply = { datum : int32; data : int32 list }

  let get_materialiv ~(context_tag : context_tag) ~(face : int32)
      ~(pname : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf face;
    encode_int32 buf pname;
    buf

  type get_pixel_mapfv_reply = { datum : float32; data : float32 list }

  let get_pixel_mapfv ~(context_tag : context_tag) ~(map : int32) () : Buffer.t
      =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf map;
    buf

  type get_pixel_mapuiv_reply = { datum : int32; data : int32 list }

  let get_pixel_mapuiv ~(context_tag : context_tag) ~(map : int32) () : Buffer.t
      =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf map;
    buf

  type get_pixel_mapusv_reply = { datum : int; data : int list }

  let get_pixel_mapusv ~(context_tag : context_tag) ~(map : int32) () : Buffer.t
      =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf map;
    buf

  type get_polygon_stipple_reply = { data : char list }

  let get_polygon_stipple ~(context_tag : context_tag) ~(lsb_first : bool) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_bool buf lsb_first;
    buf

  type get_string_reply = { string : char list }

  let get_string ~(context_tag : context_tag) ~(name : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf name;
    buf

  type get_tex_envfv_reply = { datum : float32; data : float32 list }

  let get_tex_envfv ~(context_tag : context_tag) ~(target : int32)
      ~(pname : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf target;
    encode_int32 buf pname;
    buf

  type get_tex_enviv_reply = { datum : int32; data : int32 list }

  let get_tex_enviv ~(context_tag : context_tag) ~(target : int32)
      ~(pname : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf target;
    encode_int32 buf pname;
    buf

  type get_tex_gendv_reply = { datum : float64; data : float64 list }

  let get_tex_gendv ~(context_tag : context_tag) ~(coord : int32)
      ~(pname : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf coord;
    encode_int32 buf pname;
    buf

  type get_tex_genfv_reply = { datum : float32; data : float32 list }

  let get_tex_genfv ~(context_tag : context_tag) ~(coord : int32)
      ~(pname : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf coord;
    encode_int32 buf pname;
    buf

  type get_tex_geniv_reply = { datum : int32; data : int32 list }

  let get_tex_geniv ~(context_tag : context_tag) ~(coord : int32)
      ~(pname : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf coord;
    encode_int32 buf pname;
    buf

  type get_tex_image_reply = {
    width : int32;
    height : int32;
    depth : int32;
    data : char list;
  }

  let get_tex_image ~(context_tag : context_tag) ~(target : int32)
      ~(level : int32) ~(format : int32) ~(type_ : int32) ~(swap_bytes : bool)
      () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf target;
    encode_int32 buf level;
    encode_int32 buf format;
    encode_int32 buf type_;
    encode_bool buf swap_bytes;
    buf

  type get_tex_parameterfv_reply = { datum : float32; data : float32 list }

  let get_tex_parameterfv ~(context_tag : context_tag) ~(target : int32)
      ~(pname : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf target;
    encode_int32 buf pname;
    buf

  type get_tex_parameteriv_reply = { datum : int32; data : int32 list }

  let get_tex_parameteriv ~(context_tag : context_tag) ~(target : int32)
      ~(pname : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf target;
    encode_int32 buf pname;
    buf

  type get_tex_level_parameterfv_reply = {
    datum : float32;
    data : float32 list;
  }

  let get_tex_level_parameterfv ~(context_tag : context_tag) ~(target : int32)
      ~(level : int32) ~(pname : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf target;
    encode_int32 buf level;
    encode_int32 buf pname;
    buf

  type get_tex_level_parameteriv_reply = { datum : int32; data : int32 list }

  let get_tex_level_parameteriv ~(context_tag : context_tag) ~(target : int32)
      ~(level : int32) ~(pname : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf target;
    encode_int32 buf level;
    encode_int32 buf pname;
    buf

  type is_enabled_reply = { ret_val : bool32 }

  let is_enabled ~(context_tag : context_tag) ~(capability : int32) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf capability;
    buf

  type is_list_reply = { ret_val : bool32 }

  let is_list ~(context_tag : context_tag) ~(list : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf list;
    buf

  let flush ~(context_tag : context_tag) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    buf

  type are_textures_resident_reply = { ret_val : bool32; data : bool list }

  let are_textures_resident ~(context_tag : context_tag)
      ~(textures : int32 list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf (Int32.of_int (List.length textures));
    encode_list encode_int32 buf textures;
    buf

  let delete_textures ~(context_tag : context_tag) ~(textures : int32 list) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf (Int32.of_int (List.length textures));
    encode_list encode_int32 buf textures;
    buf

  type gen_textures_reply = { data : int32 list }

  let gen_textures ~(context_tag : context_tag) ~(n : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf n;
    buf

  type is_texture_reply = { ret_val : bool32 }

  let is_texture ~(context_tag : context_tag) ~(texture : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf texture;
    buf

  type get_color_table_reply = { width : int32; data : char list }

  let get_color_table ~(context_tag : context_tag) ~(target : int32)
      ~(format : int32) ~(type_ : int32) ~(swap_bytes : bool) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf target;
    encode_int32 buf format;
    encode_int32 buf type_;
    encode_bool buf swap_bytes;
    buf

  type get_color_table_parameterfv_reply = {
    datum : float32;
    data : float32 list;
  }

  let get_color_table_parameterfv ~(context_tag : context_tag) ~(target : int32)
      ~(pname : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf target;
    encode_int32 buf pname;
    buf

  type get_color_table_parameteriv_reply = { datum : int32; data : int32 list }

  let get_color_table_parameteriv ~(context_tag : context_tag) ~(target : int32)
      ~(pname : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf target;
    encode_int32 buf pname;
    buf

  type get_convolution_filter_reply = {
    width : int32;
    height : int32;
    data : char list;
  }

  let get_convolution_filter ~(context_tag : context_tag) ~(target : int32)
      ~(format : int32) ~(type_ : int32) ~(swap_bytes : bool) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf target;
    encode_int32 buf format;
    encode_int32 buf type_;
    encode_bool buf swap_bytes;
    buf

  type get_convolution_parameterfv_reply = {
    datum : float32;
    data : float32 list;
  }

  let get_convolution_parameterfv ~(context_tag : context_tag) ~(target : int32)
      ~(pname : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf target;
    encode_int32 buf pname;
    buf

  type get_convolution_parameteriv_reply = { datum : int32; data : int32 list }

  let get_convolution_parameteriv ~(context_tag : context_tag) ~(target : int32)
      ~(pname : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf target;
    encode_int32 buf pname;
    buf

  type get_separable_filter_reply = {
    row_w : int32;
    col_h : int32;
    rows_and_cols : char list;
  }

  let get_separable_filter ~(context_tag : context_tag) ~(target : int32)
      ~(format : int32) ~(type_ : int32) ~(swap_bytes : bool) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf target;
    encode_int32 buf format;
    encode_int32 buf type_;
    encode_bool buf swap_bytes;
    buf

  type get_histogram_reply = { width : int32; data : char list }

  let get_histogram ~(context_tag : context_tag) ~(target : int32)
      ~(format : int32) ~(type_ : int32) ~(swap_bytes : bool) ~(reset : bool) ()
      : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf target;
    encode_int32 buf format;
    encode_int32 buf type_;
    encode_bool buf swap_bytes;
    encode_bool buf reset;
    buf

  type get_histogram_parameterfv_reply = {
    datum : float32;
    data : float32 list;
  }

  let get_histogram_parameterfv ~(context_tag : context_tag) ~(target : int32)
      ~(pname : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf target;
    encode_int32 buf pname;
    buf

  type get_histogram_parameteriv_reply = { datum : int32; data : int32 list }

  let get_histogram_parameteriv ~(context_tag : context_tag) ~(target : int32)
      ~(pname : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf target;
    encode_int32 buf pname;
    buf

  type get_minmax_reply = { data : char list }

  let get_minmax ~(context_tag : context_tag) ~(target : int32)
      ~(format : int32) ~(type_ : int32) ~(swap_bytes : bool) ~(reset : bool) ()
      : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf target;
    encode_int32 buf format;
    encode_int32 buf type_;
    encode_bool buf swap_bytes;
    encode_bool buf reset;
    buf

  type get_minmax_parameterfv_reply = { datum : float32; data : float32 list }

  let get_minmax_parameterfv ~(context_tag : context_tag) ~(target : int32)
      ~(pname : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf target;
    encode_int32 buf pname;
    buf

  type get_minmax_parameteriv_reply = { datum : int32; data : int32 list }

  let get_minmax_parameteriv ~(context_tag : context_tag) ~(target : int32)
      ~(pname : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf target;
    encode_int32 buf pname;
    buf

  type get_compressed_tex_image_arb_reply = { size : int32; data : char list }

  let get_compressed_tex_image_arb ~(context_tag : context_tag)
      ~(target : int32) ~(level : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf target;
    encode_int32 buf level;
    buf

  let delete_queries_arb ~(context_tag : context_tag) ~(ids : int32 list) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf (Int32.of_int (List.length ids));
    encode_list encode_int32 buf ids;
    buf

  type gen_queries_arb_reply = { data : int32 list }

  let gen_queries_arb ~(context_tag : context_tag) ~(n : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf n;
    buf

  type is_query_arb_reply = { ret_val : bool32 }

  let is_query_arb ~(context_tag : context_tag) ~(id : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf id;
    buf

  type get_queryiv_arb_reply = { datum : int32; data : int32 list }

  let get_queryiv_arb ~(context_tag : context_tag) ~(target : int32)
      ~(pname : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf target;
    encode_int32 buf pname;
    buf

  type get_query_objectiv_arb_reply = { datum : int32; data : int32 list }

  let get_query_objectiv_arb ~(context_tag : context_tag) ~(id : int32)
      ~(pname : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf id;
    encode_int32 buf pname;
    buf

  type get_query_objectuiv_arb_reply = { datum : int32; data : int32 list }

  let get_query_objectuiv_arb ~(context_tag : context_tag) ~(id : int32)
      ~(pname : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context_tag buf context_tag;
    encode_int32 buf id;
    encode_int32 buf pname;
    buf
end
[@@warning "-27"]

module Randr = struct
  type mode = xid

  let decode_mode = decode_xid

  let encode_mode = encode_xid

  type crtc = xid

  let decode_crtc = decode_xid

  let encode_crtc = encode_xid

  type output = xid

  let decode_output = decode_xid

  let encode_output = encode_xid

  type provider = xid

  let decode_provider = decode_xid

  let encode_provider = encode_xid

  type lease = xid

  let decode_lease = decode_xid

  let encode_lease = encode_xid

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

  let rotation_mask_flags =
    [
      (`Rotate_0, 0);
      (`Rotate_90, 1);
      (`Rotate_180, 2);
      (`Rotate_270, 3);
      (`Reflect_x, 4);
      (`Reflect_y, 5);
    ]

  let decode_rotation_mask i =
    List.filter_map
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      rotation_mask_flags

  let int_of_rotation_mask flags =
    List.fold_left
      (fun acc flag ->
        let code =
          match flag with
          | `Rotate_0 -> 0
          | `Rotate_90 -> 1
          | `Rotate_180 -> 2
          | `Rotate_270 -> 3
          | `Reflect_x -> 4
          | `Reflect_y -> 5
        in
        acc lor (1 lsl code))
      0 flags

  type screen_size = { width : int; height : int; mwidth : int; mheight : int }

  let decode_screen_size buf ~at : (screen_size * int) option =
    let orig = at in
    let* width, at = decode_uint16 buf ~at in
    let* height, at = decode_uint16 buf ~at in
    let* mwidth, at = decode_uint16 buf ~at in
    let* mheight, at = decode_uint16 buf ~at in
    ignore orig;
    Some ({ width; height; mwidth; mheight }, at)

  let encode_screen_size buf { width; height; mwidth; mheight } =
    encode_uint16 buf width;
    encode_uint16 buf height;
    encode_uint16 buf mwidth;
    encode_uint16 buf mheight;
    ignore buf

  type refresh_rates = { rates : int list }

  let decode_refresh_rates buf ~at : (refresh_rates * int) option =
    let orig = at in
    let* n_rates, at = decode_uint16 buf ~at in
    let n_rates = n_rates in
    let* rates, at = decode_list decode_uint16 n_rates buf ~at in
    ignore orig;
    Some ({ rates }, at)

  let encode_refresh_rates buf { rates } =
    encode_uint16 buf (List.length rates);
    encode_list encode_uint16 buf rates;
    ignore buf

  type query_version_reply = { major_version : int32; minor_version : int32 }

  let query_version ~(major_version : int32) ~(minor_version : int32) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf major_version;
    encode_int32 buf minor_version;
    buf

  type set_config_enum =
    [ `Success | `Invalid_config_time | `Invalid_time | `Failed ]

  let set_config_enum_of_int : int -> set_config_enum option = function
    | 0 -> Some `Success
    | 1 -> Some `Invalid_config_time
    | 2 -> Some `Invalid_time
    | 3 -> Some `Failed
    | _ -> None

  let int_of_set_config_enum : set_config_enum -> int = function
    | `Success -> 0
    | `Invalid_config_time -> 1
    | `Invalid_time -> 2
    | `Failed -> 3

  type set_screen_config_reply = {
    status : set_config_enum;
    new_timestamp : Xproto.timestamp;
    config_timestamp : Xproto.timestamp;
    root : Xproto.window;
    subpixel_order : Render.sub_pixel_enum;
  }

  let set_screen_config ~(window : Xproto.window)
      ~(timestamp : Xproto.timestamp) ~(config_timestamp : Xproto.timestamp)
      ~(size_id : int) ~(rotation : rotation_mask) ~(rate : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    Xproto.encode_timestamp buf timestamp;
    Xproto.encode_timestamp buf config_timestamp;
    encode_uint16 buf size_id;
    encode_to_int encode_uint16 int_of_rotation_mask buf rotation;
    encode_uint16 buf rate;
    buf

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

  let notify_mask_mask_flags =
    [
      (`Screen_change, 0);
      (`Crtc_change, 1);
      (`Output_change, 2);
      (`Output_property, 3);
      (`Provider_change, 4);
      (`Provider_property, 5);
      (`Resource_change, 6);
      (`Lease, 7);
    ]

  let decode_notify_mask_mask i =
    List.filter_map
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      notify_mask_mask_flags

  let int_of_notify_mask_mask flags =
    List.fold_left
      (fun acc flag ->
        let code =
          match flag with
          | `Screen_change -> 0
          | `Crtc_change -> 1
          | `Output_change -> 2
          | `Output_property -> 3
          | `Provider_change -> 4
          | `Provider_property -> 5
          | `Resource_change -> 6
          | `Lease -> 7
        in
        acc lor (1 lsl code))
      0 flags

  let select_input ~(window : Xproto.window) ~(enable : notify_mask_mask) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    encode_to_int encode_uint16 int_of_notify_mask_mask buf enable;
    buf

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

  let get_screen_info ~(window : Xproto.window) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    buf

  type get_screen_size_range_reply = {
    min_width : int;
    min_height : int;
    max_width : int;
    max_height : int;
  }

  let get_screen_size_range ~(window : Xproto.window) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    buf

  let set_screen_size ~(window : Xproto.window) ~(width : int) ~(height : int)
      ~(mm_width : int32) ~(mm_height : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    encode_uint16 buf width;
    encode_uint16 buf height;
    encode_int32 buf mm_width;
    encode_int32 buf mm_height;
    buf

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

  let mode_flag_mask_flags =
    [
      (`Hsync_positive, 0);
      (`Hsync_negative, 1);
      (`Vsync_positive, 2);
      (`Vsync_negative, 3);
      (`Interlace, 4);
      (`Double_scan, 5);
      (`Csync, 6);
      (`Csync_positive, 7);
      (`Csync_negative, 8);
      (`Hskew_present, 9);
      (`Bcast, 10);
      (`Pixel_multiplex, 11);
      (`Double_clock, 12);
      (`Halve_clock, 13);
    ]

  let decode_mode_flag_mask i =
    List.filter_map
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      mode_flag_mask_flags

  let int_of_mode_flag_mask flags =
    List.fold_left
      (fun acc flag ->
        let code =
          match flag with
          | `Hsync_positive -> 0
          | `Hsync_negative -> 1
          | `Vsync_positive -> 2
          | `Vsync_negative -> 3
          | `Interlace -> 4
          | `Double_scan -> 5
          | `Csync -> 6
          | `Csync_positive -> 7
          | `Csync_negative -> 8
          | `Hskew_present -> 9
          | `Bcast -> 10
          | `Pixel_multiplex -> 11
          | `Double_clock -> 12
          | `Halve_clock -> 13
        in
        acc lor (1 lsl code))
      0 flags

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
    let mode_flags = decode_mode_flag_mask (Int32.to_int mode_flags) in
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
          mode_flags;
        },
        at )

  let encode_mode_info buf
      {
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
        mode_flags;
      } =
    encode_int32 buf id;
    encode_uint16 buf width;
    encode_uint16 buf height;
    encode_int32 buf dot_clock;
    encode_uint16 buf hsync_start;
    encode_uint16 buf hsync_end;
    encode_uint16 buf htotal;
    encode_uint16 buf hskew;
    encode_uint16 buf vsync_start;
    encode_uint16 buf vsync_end;
    encode_uint16 buf vtotal;
    encode_uint16 buf name_len;
    encode_to_int encode_int32
      (fun x -> Int32.of_int (int_of_mode_flag_mask x))
      buf mode_flags;
    ignore buf

  type get_screen_resources_reply = {
    timestamp : Xproto.timestamp;
    config_timestamp : Xproto.timestamp;
    crtcs : crtc list;
    outputs : output list;
    modes : mode_info list;
    names : char list;
  }

  let get_screen_resources ~(window : Xproto.window) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    buf

  type connection_enum = [ `Connected | `Disconnected | `Unknown ]

  let connection_enum_of_int : int -> connection_enum option = function
    | 0 -> Some `Connected
    | 1 -> Some `Disconnected
    | 2 -> Some `Unknown
    | _ -> None

  let int_of_connection_enum : connection_enum -> int = function
    | `Connected -> 0
    | `Disconnected -> 1
    | `Unknown -> 2

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
      () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_output buf output;
    Xproto.encode_timestamp buf config_timestamp;
    buf

  type list_output_properties_reply = { atoms : Xproto.atom list }

  let list_output_properties ~(output : output) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_output buf output;
    buf

  type query_output_property_reply = {
    pending : bool;
    range : bool;
    immutable : bool;
    valid_values : int32 list;
  }

  let query_output_property ~(output : output) ~(property : Xproto.atom) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_output buf output;
    Xproto.encode_atom buf property;
    buf

  let configure_output_property ~(output : output) ~(property : Xproto.atom)
      ~(pending : bool) ~(range : bool) ~(values : int32 list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_output buf output;
    Xproto.encode_atom buf property;
    encode_bool buf pending;
    encode_bool buf range;
    buf

  let change_output_property ~(output : output) ~(property : Xproto.atom)
      ~(type_ : Xproto.atom) ~(format : int) ~(mode : Xproto.prop_mode_enum)
      ~(num_units : int32) ~(data : char list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_output buf output;
    Xproto.encode_atom buf property;
    Xproto.encode_atom buf type_;
    encode_uint8 buf format;
    encode_to_int encode_uint8 Xproto.int_of_prop_mode_enum buf mode;
    encode_int32 buf num_units;
    buf

  let delete_output_property ~(output : output) ~(property : Xproto.atom) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_output buf output;
    Xproto.encode_atom buf property;
    buf

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
      ~(pending : bool) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_output buf output;
    Xproto.encode_atom buf property;
    encode_alt Xproto.encode_atom Xproto.int_of_get_property_type_enum
      Xproto.encode_atom buf type_;
    encode_int32 buf long_offset;
    encode_int32 buf long_length;
    encode_bool buf delete;
    encode_bool buf pending;
    buf

  type create_mode_reply = { mode : mode }

  let create_mode ~(window : Xproto.window) ~(mode_info : mode_info)
      ~(name : char list) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    encode_mode_info buf mode_info;
    buf

  let destroy_mode ~(mode : mode) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_mode buf mode;
    buf

  let add_output_mode ~(output : output) ~(mode : mode) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_output buf output;
    encode_mode buf mode;
    buf

  let delete_output_mode ~(output : output) ~(mode : mode) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_output buf output;
    encode_mode buf mode;
    buf

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
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_crtc buf crtc;
    Xproto.encode_timestamp buf config_timestamp;
    buf

  type set_crtc_config_reply = {
    status : set_config_enum;
    timestamp : Xproto.timestamp;
  }

  let set_crtc_config ~(crtc : crtc) ~(timestamp : Xproto.timestamp)
      ~(config_timestamp : Xproto.timestamp) ~(x : int) ~(y : int)
      ~(mode : mode) ~(rotation : rotation_mask) ~(outputs : output list) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_crtc buf crtc;
    Xproto.encode_timestamp buf timestamp;
    Xproto.encode_timestamp buf config_timestamp;
    encode_int16 buf x;
    encode_int16 buf y;
    encode_mode buf mode;
    encode_to_int encode_uint16 int_of_rotation_mask buf rotation;
    buf

  type get_crtc_gamma_size_reply = { size : int }

  let get_crtc_gamma_size ~(crtc : crtc) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_crtc buf crtc;
    buf

  type get_crtc_gamma_reply = {
    red : int list;
    green : int list;
    blue : int list;
  }

  let get_crtc_gamma ~(crtc : crtc) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_crtc buf crtc;
    buf

  let set_crtc_gamma ~(crtc : crtc) ~(red : int list) ~(green : int list)
      ~(blue : int list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_crtc buf crtc;
    encode_uint16 buf (List.length red);
    encode_list encode_uint16 buf red;
    encode_list encode_uint16 buf green;
    encode_list encode_uint16 buf blue;
    buf

  type get_screen_resources_current_reply = {
    timestamp : Xproto.timestamp;
    config_timestamp : Xproto.timestamp;
    crtcs : crtc list;
    outputs : output list;
    modes : mode_info list;
    names : char list;
  }

  let get_screen_resources_current ~(window : Xproto.window) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    buf

  type transform_mask = [ `Unit | `Scale_up | `Scale_down | `Projective ] list

  let transform_mask_flags =
    [ (`Unit, 0); (`Scale_up, 1); (`Scale_down, 2); (`Projective, 3) ]

  let decode_transform_mask i =
    List.filter_map
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      transform_mask_flags

  let int_of_transform_mask flags =
    List.fold_left
      (fun acc flag ->
        let code =
          match flag with
          | `Unit -> 0
          | `Scale_up -> 1
          | `Scale_down -> 2
          | `Projective -> 3
        in
        acc lor (1 lsl code))
      0 flags

  let set_crtc_transform ~(crtc : crtc) ~(transform : Render.transform)
      ~(filter_name : char list) ~(filter_params : Render.fixed list) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_crtc buf crtc;
    Render.encode_transform buf transform;
    encode_uint16 buf (List.length filter_name);
    encode_list encode_char buf filter_name;
    buf

  type get_crtc_transform_reply = {
    pending_transform : Render.transform;
    has_transforms : bool;
    current_transform : Render.transform;
    pending_filter_name : char list;
    pending_params : Render.fixed list;
    current_filter_name : char list;
    current_params : Render.fixed list;
  }

  let get_crtc_transform ~(crtc : crtc) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_crtc buf crtc;
    buf

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

  let get_panning ~(crtc : crtc) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_crtc buf crtc;
    buf

  type set_panning_reply = {
    status : set_config_enum;
    timestamp : Xproto.timestamp;
  }

  let set_panning ~(crtc : crtc) ~(timestamp : Xproto.timestamp) ~(left : int)
      ~(top : int) ~(width : int) ~(height : int) ~(track_left : int)
      ~(track_top : int) ~(track_width : int) ~(track_height : int)
      ~(border_left : int) ~(border_top : int) ~(border_right : int)
      ~(border_bottom : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_crtc buf crtc;
    Xproto.encode_timestamp buf timestamp;
    encode_uint16 buf left;
    encode_uint16 buf top;
    encode_uint16 buf width;
    encode_uint16 buf height;
    encode_uint16 buf track_left;
    encode_uint16 buf track_top;
    encode_uint16 buf track_width;
    encode_uint16 buf track_height;
    encode_int16 buf border_left;
    encode_int16 buf border_top;
    encode_int16 buf border_right;
    encode_int16 buf border_bottom;
    buf

  let set_output_primary ~(window : Xproto.window) ~(output : output) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    encode_output buf output;
    buf

  type get_output_primary_reply = { output : output }

  let get_output_primary ~(window : Xproto.window) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    buf

  type get_providers_reply = {
    timestamp : Xproto.timestamp;
    providers : provider list;
  }

  let get_providers ~(window : Xproto.window) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    buf

  type provider_capability_mask =
    [ `Source_output | `Sink_output | `Source_offload | `Sink_offload ] list

  let provider_capability_mask_flags =
    [
      (`Source_output, 0);
      (`Sink_output, 1);
      (`Source_offload, 2);
      (`Sink_offload, 3);
    ]

  let decode_provider_capability_mask i =
    List.filter_map
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      provider_capability_mask_flags

  let int_of_provider_capability_mask flags =
    List.fold_left
      (fun acc flag ->
        let code =
          match flag with
          | `Source_output -> 0
          | `Sink_output -> 1
          | `Source_offload -> 2
          | `Sink_offload -> 3
        in
        acc lor (1 lsl code))
      0 flags

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
      ~(config_timestamp : Xproto.timestamp) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_provider buf provider;
    Xproto.encode_timestamp buf config_timestamp;
    buf

  let set_provider_offload_sink ~(provider : provider)
      ~(sink_provider : provider) ~(config_timestamp : Xproto.timestamp) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_provider buf provider;
    encode_provider buf sink_provider;
    Xproto.encode_timestamp buf config_timestamp;
    buf

  let set_provider_output_source ~(provider : provider)
      ~(source_provider : provider) ~(config_timestamp : Xproto.timestamp) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_provider buf provider;
    encode_provider buf source_provider;
    Xproto.encode_timestamp buf config_timestamp;
    buf

  type list_provider_properties_reply = { atoms : Xproto.atom list }

  let list_provider_properties ~(provider : provider) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_provider buf provider;
    buf

  type query_provider_property_reply = {
    pending : bool;
    range : bool;
    immutable : bool;
    valid_values : int32 list;
  }

  let query_provider_property ~(provider : provider) ~(property : Xproto.atom)
      () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_provider buf provider;
    Xproto.encode_atom buf property;
    buf

  let configure_provider_property ~(provider : provider)
      ~(property : Xproto.atom) ~(pending : bool) ~(range : bool)
      ~(values : int32 list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_provider buf provider;
    Xproto.encode_atom buf property;
    encode_bool buf pending;
    encode_bool buf range;
    buf

  let change_provider_property ~(provider : provider) ~(property : Xproto.atom)
      ~(type_ : Xproto.atom) ~(format : int) ~(mode : int) ~(num_items : int32)
      ~(data : char list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_provider buf provider;
    Xproto.encode_atom buf property;
    Xproto.encode_atom buf type_;
    encode_uint8 buf format;
    encode_uint8 buf mode;
    encode_int32 buf num_items;
    buf

  let delete_provider_property ~(provider : provider) ~(property : Xproto.atom)
      () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_provider buf provider;
    Xproto.encode_atom buf property;
    buf

  type get_provider_property_reply = {
    format : int;
    type_ : Xproto.atom;
    bytes_after : int32;
    num_items : int32;
    data : char list;
  }

  let get_provider_property ~(provider : provider) ~(property : Xproto.atom)
      ~(type_ : Xproto.atom) ~(long_offset : int32) ~(long_length : int32)
      ~(delete : bool) ~(pending : bool) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_provider buf provider;
    Xproto.encode_atom buf property;
    Xproto.encode_atom buf type_;
    encode_int32 buf long_offset;
    encode_int32 buf long_length;
    encode_bool buf delete;
    encode_bool buf pending;
    buf

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

  let int_of_notify_enum : notify_enum -> int = function
    | `Crtc_change -> 0
    | `Output_change -> 1
    | `Output_property -> 2
    | `Provider_change -> 3
    | `Provider_property -> 4
    | `Resource_change -> 5
    | `Lease -> 6

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
    let rotation = decode_rotation_mask rotation in
    let at = at + 2 in
    let* x, at = decode_int16 buf ~at in
    let* y, at = decode_int16 buf ~at in
    let* width, at = decode_uint16 buf ~at in
    let* height, at = decode_uint16 buf ~at in
    ignore orig;
    Some ({ timestamp; window; crtc; mode; rotation; x; y; width; height }, at)

  let encode_crtc_change buf
      { timestamp; window; crtc; mode; rotation; x; y; width; height } =
    Xproto.encode_timestamp buf timestamp;
    Xproto.encode_window buf window;
    encode_crtc buf crtc;
    encode_mode buf mode;
    encode_to_int encode_uint16 int_of_rotation_mask buf rotation;
    encode_int16 buf x;
    encode_int16 buf y;
    encode_uint16 buf width;
    encode_uint16 buf height;
    ignore buf

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
    let rotation = decode_rotation_mask rotation in
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
          rotation;
          connection;
          subpixel_order;
        },
        at )

  let encode_output_change buf
      {
        timestamp;
        config_timestamp;
        window;
        output;
        crtc;
        mode;
        rotation;
        connection;
        subpixel_order;
      } =
    Xproto.encode_timestamp buf timestamp;
    Xproto.encode_timestamp buf config_timestamp;
    Xproto.encode_window buf window;
    encode_output buf output;
    encode_crtc buf crtc;
    encode_mode buf mode;
    encode_to_int encode_uint16 int_of_rotation_mask buf rotation;
    encode_to_int encode_uint8 int_of_connection_enum buf connection;
    encode_to_int encode_uint8 Render.int_of_sub_pixel_enum buf subpixel_order;
    ignore buf

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

  let encode_output_property buf { window; output; atom; timestamp; status } =
    Xproto.encode_window buf window;
    encode_output buf output;
    Xproto.encode_atom buf atom;
    Xproto.encode_timestamp buf timestamp;
    encode_to_int encode_uint8 Xproto.int_of_property_enum buf status;
    ignore buf

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

  let encode_provider_change buf { timestamp; window; provider } =
    Xproto.encode_timestamp buf timestamp;
    Xproto.encode_window buf window;
    encode_provider buf provider;
    ignore buf

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

  let encode_provider_property buf { window; provider; atom; timestamp; state }
      =
    Xproto.encode_window buf window;
    encode_provider buf provider;
    Xproto.encode_atom buf atom;
    Xproto.encode_timestamp buf timestamp;
    encode_uint8 buf state;
    ignore buf

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

  let encode_resource_change buf { timestamp; window } =
    Xproto.encode_timestamp buf timestamp;
    Xproto.encode_window buf window;
    ignore buf

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

  let encode_monitor_info buf
      {
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
      } =
    Xproto.encode_atom buf name;
    encode_bool buf primary;
    encode_bool buf automatic;
    encode_uint16 buf (List.length outputs);
    encode_int16 buf x;
    encode_int16 buf y;
    encode_uint16 buf width;
    encode_uint16 buf height;
    encode_int32 buf width_in_millimeters;
    encode_int32 buf height_in_millimeters;
    encode_list encode_output buf outputs;
    ignore buf

  type get_monitors_reply = {
    timestamp : Xproto.timestamp;
    n_outputs : int32;
    monitors : monitor_info list;
  }

  let get_monitors ~(window : Xproto.window) ~(get_active : bool) () : Buffer.t
      =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    encode_bool buf get_active;
    buf

  let set_monitor ~(window : Xproto.window) ~(monitorinfo : monitor_info) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    encode_monitor_info buf monitorinfo;
    buf

  let delete_monitor ~(window : Xproto.window) ~(name : Xproto.atom) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    Xproto.encode_atom buf name;
    buf

  type create_lease_reply = { nfd : int; master_fd : file_descr }

  let create_lease ~(window : Xproto.window) ~(lid : lease) ~(crtcs : crtc list)
      ~(outputs : output list) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    encode_lease buf lid;
    encode_uint16 buf (List.length crtcs);
    encode_uint16 buf (List.length outputs);
    encode_list encode_crtc buf crtcs;
    encode_list encode_output buf outputs;
    buf

  let free_lease ~(lid : lease) ~(terminate : char) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_lease buf lid;
    encode_char buf terminate;
    buf

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

  let encode_lease_notify buf { timestamp; window; lease; created } =
    Xproto.encode_timestamp buf timestamp;
    Xproto.encode_window buf window;
    encode_lease buf lease;
    encode_uint8 buf created;
    ignore buf

  type notify_variant =
    | Crtc_change of { cc : crtc_change }
    | Output_change of { oc : output_change }
    | Output_property of { op : output_property }
    | Provider_change of { pc : provider_change }
    | Provider_property of { pp : provider_property }
    | Resource_change of { rc : resource_change }
    | Lease of { lc : lease_notify }

  let decode_notify_variant buf ~at enum : (notify_variant * int) option =
    let decode_crtc_change buf ~at : (notify_variant * int) option =
      let orig = at in
      let* cc, at = decode_crtc_change buf ~at in
      ignore orig;
      Some (Crtc_change { cc }, at)
    in
    let decode_output_change buf ~at : (notify_variant * int) option =
      let orig = at in
      let* oc, at = decode_output_change buf ~at in
      ignore orig;
      Some (Output_change { oc }, at)
    in
    let decode_output_property buf ~at : (notify_variant * int) option =
      let orig = at in
      let* op, at = decode_output_property buf ~at in
      ignore orig;
      Some (Output_property { op }, at)
    in
    let decode_provider_change buf ~at : (notify_variant * int) option =
      let orig = at in
      let* pc, at = decode_provider_change buf ~at in
      ignore orig;
      Some (Provider_change { pc }, at)
    in
    let decode_provider_property buf ~at : (notify_variant * int) option =
      let orig = at in
      let* pp, at = decode_provider_property buf ~at in
      ignore orig;
      Some (Provider_property { pp }, at)
    in
    let decode_resource_change buf ~at : (notify_variant * int) option =
      let orig = at in
      let* rc, at = decode_resource_change buf ~at in
      ignore orig;
      Some (Resource_change { rc }, at)
    in
    let decode_lease buf ~at : (notify_variant * int) option =
      let orig = at in
      let* lc, at = decode_lease_notify buf ~at in
      ignore orig;
      Some (Lease { lc }, at)
    in
    match enum with
    | 0 -> decode_crtc_change buf ~at
    | 1 -> decode_output_change buf ~at
    | 2 -> decode_output_property buf ~at
    | 3 -> decode_provider_change buf ~at
    | 4 -> decode_provider_property buf ~at
    | 5 -> decode_resource_change buf ~at
    | 6 -> decode_lease buf ~at
    | invalid ->
        failwith (Printf.sprintf "Invalid enum tag %d for notify " invalid)

  type notify_event = { u : notify_variant }
end
[@@warning "-27"]

module Sync = struct
  type alarm = xid

  let decode_alarm = decode_xid

  let encode_alarm = encode_xid

  type alarmstate_enum = [ `Active | `Inactive | `Destroyed ]

  let alarmstate_enum_of_int : int -> alarmstate_enum option = function
    | 0 -> Some `Active
    | 1 -> Some `Inactive
    | 2 -> Some `Destroyed
    | _ -> None

  let int_of_alarmstate_enum : alarmstate_enum -> int = function
    | `Active -> 0
    | `Inactive -> 1
    | `Destroyed -> 2

  type counter = xid

  let decode_counter = decode_xid

  let encode_counter = encode_xid

  type fence = xid

  let decode_fence = decode_xid

  let encode_fence = encode_xid

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

  let int_of_testtype_enum : testtype_enum -> int = function
    | `Positive_transition -> 0
    | `Negative_transition -> 1
    | `Positive_comparison -> 2
    | `Negative_comparison -> 3

  type valuetype_enum = [ `Absolute | `Relative ]

  let valuetype_enum_of_int : int -> valuetype_enum option = function
    | 0 -> Some `Absolute
    | 1 -> Some `Relative
    | _ -> None

  let int_of_valuetype_enum : valuetype_enum -> int = function
    | `Absolute -> 0
    | `Relative -> 1

  type ca_mask =
    [ `Counter | `Value_type | `Value | `Test_type | `Delta | `Events ] list

  let ca_mask_flags =
    [
      (`Counter, 0);
      (`Value_type, 1);
      (`Value, 2);
      (`Test_type, 3);
      (`Delta, 4);
      (`Events, 5);
    ]

  let decode_ca_mask i =
    List.filter_map
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      ca_mask_flags

  let int_of_ca_mask flags =
    List.fold_left
      (fun acc flag ->
        let code =
          match flag with
          | `Counter -> 0
          | `Value_type -> 1
          | `Value -> 2
          | `Test_type -> 3
          | `Delta -> 4
          | `Events -> 5
        in
        acc lor (1 lsl code))
      0 flags

  type int64 = { hi : int32; lo : int32 }

  let decode_int64 buf ~at : (int64 * int) option =
    let orig = at in
    let* hi, at = decode_int32 buf ~at in
    let* lo, at = decode_int32 buf ~at in
    ignore orig;
    Some ({ hi; lo }, at)

  let encode_int64 buf { hi; lo } =
    encode_int32 buf hi;
    encode_int32 buf lo;
    ignore buf

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

  let encode_systemcounter buf { counter; resolution; name } =
    encode_counter buf counter;
    encode_int64 buf resolution;
    encode_uint16 buf (List.length name);
    encode_list encode_char buf name;
    ignore buf

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

  let encode_trigger buf { counter; wait_type; wait_value; test_type } =
    encode_counter buf counter;
    encode_to_int encode_int32
      (fun x -> Int32.of_int (int_of_valuetype_enum x))
      buf wait_type;
    encode_int64 buf wait_value;
    encode_to_int encode_int32
      (fun x -> Int32.of_int (int_of_testtype_enum x))
      buf test_type;
    ignore buf

  type waitcondition = { trigger : trigger; event_threshold : int64 }

  let decode_waitcondition buf ~at : (waitcondition * int) option =
    let orig = at in
    let* trigger, at = decode_trigger buf ~at in
    let* event_threshold, at = decode_int64 buf ~at in
    ignore orig;
    Some ({ trigger; event_threshold }, at)

  let encode_waitcondition buf { trigger; event_threshold } =
    encode_trigger buf trigger;
    encode_int64 buf event_threshold;
    ignore buf

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
      () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint8 buf desired_major_version;
    encode_uint8 buf desired_minor_version;
    buf

  type list_system_counters_reply = { counters : systemcounter list }

  let list_system_counters () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  let create_counter ~(id : counter) ~(initial_value : int64) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_counter buf id;
    encode_int64 buf initial_value;
    buf

  let destroy_counter ~(counter : counter) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_counter buf counter;
    buf

  type query_counter_reply = { counter_value : int64 }

  let query_counter ~(counter : counter) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_counter buf counter;
    buf

  let await ~(wait_list : waitcondition list) () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  let change_counter ~(counter : counter) ~(amount : int64) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_counter buf counter;
    encode_int64 buf amount;
    buf

  let set_counter ~(counter : counter) ~(value : int64) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_counter buf counter;
    encode_int64 buf value;
    buf

  let create_alarm ~(id : alarm) ?(counter : counter option)
      ?(value_type : valuetype_enum option) ?(value : int64 option)
      ?(test_type : testtype_enum option) ?(delta : int64 option)
      ?(events : int32 option) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_alarm buf id;
    let value_mask = 0 in
    let value_mask =
      if Option.is_some counter then value_mask lor (1 lsl 0) else value_mask
    in
    let value_mask =
      if Option.is_some value_type then value_mask lor (1 lsl 1) else value_mask
    in
    let value_mask =
      if Option.is_some value then value_mask lor (1 lsl 2) else value_mask
    in
    let value_mask =
      if Option.is_some test_type then value_mask lor (1 lsl 3) else value_mask
    in
    let value_mask =
      if Option.is_some delta then value_mask lor (1 lsl 4) else value_mask
    in
    let value_mask =
      if Option.is_some events then value_mask lor (1 lsl 5) else value_mask
    in
    encode_int32 buf (Int32.of_int value_mask);
    Option.iter (fun counter -> encode_counter buf counter) counter;
    Option.iter
      (fun value_type ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_valuetype_enum x))
          buf value_type)
      value_type;
    Option.iter (fun value -> encode_int64 buf value) value;
    Option.iter
      (fun test_type ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_testtype_enum x))
          buf test_type)
      test_type;
    Option.iter (fun delta -> encode_int64 buf delta) delta;
    Option.iter (fun events -> encode_int32 buf events) events;
    buf

  let change_alarm ~(id : alarm) ?(counter : counter option)
      ?(value_type : valuetype_enum option) ?(value : int64 option)
      ?(test_type : testtype_enum option) ?(delta : int64 option)
      ?(events : int32 option) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_alarm buf id;
    let value_mask = 0 in
    let value_mask =
      if Option.is_some counter then value_mask lor (1 lsl 0) else value_mask
    in
    let value_mask =
      if Option.is_some value_type then value_mask lor (1 lsl 1) else value_mask
    in
    let value_mask =
      if Option.is_some value then value_mask lor (1 lsl 2) else value_mask
    in
    let value_mask =
      if Option.is_some test_type then value_mask lor (1 lsl 3) else value_mask
    in
    let value_mask =
      if Option.is_some delta then value_mask lor (1 lsl 4) else value_mask
    in
    let value_mask =
      if Option.is_some events then value_mask lor (1 lsl 5) else value_mask
    in
    encode_int32 buf (Int32.of_int value_mask);
    Option.iter (fun counter -> encode_counter buf counter) counter;
    Option.iter
      (fun value_type ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_valuetype_enum x))
          buf value_type)
      value_type;
    Option.iter (fun value -> encode_int64 buf value) value;
    Option.iter
      (fun test_type ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (int_of_testtype_enum x))
          buf test_type)
      test_type;
    Option.iter (fun delta -> encode_int64 buf delta) delta;
    Option.iter (fun events -> encode_int32 buf events) events;
    buf

  let destroy_alarm ~(alarm : alarm) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_alarm buf alarm;
    buf

  type query_alarm_reply = {
    trigger : trigger;
    delta : int64;
    events : bool;
    state : alarmstate_enum;
  }

  let query_alarm ~(alarm : alarm) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_alarm buf alarm;
    buf

  let set_priority ~(id : int32) ~(priority : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf id;
    encode_int32 buf priority;
    buf

  type get_priority_reply = { priority : int32 }

  let get_priority ~(id : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf id;
    buf

  let create_fence ~(drawable : Xproto.drawable) ~(fence : fence)
      ~(initially_triggered : bool) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_drawable buf drawable;
    encode_fence buf fence;
    encode_bool buf initially_triggered;
    buf

  let trigger_fence ~(fence : fence) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_fence buf fence;
    buf

  let reset_fence ~(fence : fence) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_fence buf fence;
    buf

  let destroy_fence ~(fence : fence) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_fence buf fence;
    buf

  type query_fence_reply = { triggered : bool }

  let query_fence ~(fence : fence) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_fence buf fence;
    buf

  let await_fence ~(fence_list : fence list) () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

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

  let int_of_event_enum : event_enum -> int = function
    | `Configure_notify -> 0
    | `Complete_notify -> 1
    | `Idle_notify -> 2
    | `Redirect_notify -> 3

  type event_mask_mask =
    ( [ `Configure_notify | `Complete_notify | `Idle_notify | `Redirect_notify ],
      [ `No_event ] )
    mask

  let event_mask_mask_flags =
    [
      (`Configure_notify, 0);
      (`Complete_notify, 1);
      (`Idle_notify, 2);
      (`Redirect_notify, 3);
    ]

  let event_mask_mask_values = [ (`No_event, 0) ]

  let decode_event_mask_mask i =
    match List.find_opt (fun (value, v) -> v = i) event_mask_mask_values with
    | Some (value, _) -> V value
    | None ->
        F
          (List.filter_map
             (fun (flag, f) ->
               if f land (1 lsl i) <> 0 then Some flag else None)
             event_mask_mask_flags)

  let int_of_event_mask_mask = function
    | F flags ->
        List.fold_left
          (fun acc flag ->
            let code =
              match flag with
              | `Configure_notify -> 0
              | `Complete_notify -> 1
              | `Idle_notify -> 2
              | `Redirect_notify -> 3
            in
            acc lor (1 lsl code))
          0 flags
    | V `No_event -> 0

  type option_mask = ([ `Async | `Copy | `Ust | `Suboptimal ], [ `None ]) mask

  let option_mask_flags =
    [ (`Async, 0); (`Copy, 1); (`Ust, 2); (`Suboptimal, 3) ]

  let option_mask_values = [ (`None, 0) ]

  let decode_option_mask i =
    match List.find_opt (fun (value, v) -> v = i) option_mask_values with
    | Some (value, _) -> V value
    | None ->
        F
          (List.filter_map
             (fun (flag, f) ->
               if f land (1 lsl i) <> 0 then Some flag else None)
             option_mask_flags)

  let int_of_option_mask = function
    | F flags ->
        List.fold_left
          (fun acc flag ->
            let code =
              match flag with
              | `Async -> 0
              | `Copy -> 1
              | `Ust -> 2
              | `Suboptimal -> 3
            in
            acc lor (1 lsl code))
          0 flags
    | V `None -> 0

  type capability_mask = ([ `Async | `Fence | `Ust ], [ `None ]) mask

  let capability_mask_flags = [ (`Async, 0); (`Fence, 1); (`Ust, 2) ]

  let capability_mask_values = [ (`None, 0) ]

  let decode_capability_mask i =
    match List.find_opt (fun (value, v) -> v = i) capability_mask_values with
    | Some (value, _) -> V value
    | None ->
        F
          (List.filter_map
             (fun (flag, f) ->
               if f land (1 lsl i) <> 0 then Some flag else None)
             capability_mask_flags)

  let int_of_capability_mask = function
    | F flags ->
        List.fold_left
          (fun acc flag ->
            let code =
              match flag with `Async -> 0 | `Fence -> 1 | `Ust -> 2
            in
            acc lor (1 lsl code))
          0 flags
    | V `None -> 0

  type complete_kind_enum = [ `Pixmap | `Notify_msc ]

  let complete_kind_enum_of_int : int -> complete_kind_enum option = function
    | 0 -> Some `Pixmap
    | 1 -> Some `Notify_msc
    | _ -> None

  let int_of_complete_kind_enum : complete_kind_enum -> int = function
    | `Pixmap -> 0
    | `Notify_msc -> 1

  type complete_mode_enum = [ `Copy | `Flip | `Skip | `Suboptimal_copy ]

  let complete_mode_enum_of_int : int -> complete_mode_enum option = function
    | 0 -> Some `Copy
    | 1 -> Some `Flip
    | 2 -> Some `Skip
    | 3 -> Some `Suboptimal_copy
    | _ -> None

  let int_of_complete_mode_enum : complete_mode_enum -> int = function
    | `Copy -> 0
    | `Flip -> 1
    | `Skip -> 2
    | `Suboptimal_copy -> 3

  type notify = { window : Xproto.window; serial : int32 }

  let decode_notify buf ~at : (notify * int) option =
    let orig = at in
    let* window, at = Xproto.decode_window buf ~at in
    let* serial, at = decode_int32 buf ~at in
    ignore orig;
    Some ({ window; serial }, at)

  let encode_notify buf { window; serial } =
    Xproto.encode_window buf window;
    encode_int32 buf serial;
    ignore buf

  type query_version_reply = { major_version : int32; minor_version : int32 }

  let query_version ~(major_version : int32) ~(minor_version : int32) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf major_version;
    encode_int32 buf minor_version;
    buf

  let pixmap ~(window : Xproto.window) ~(pixmap : Xproto.pixmap)
      ~(serial : int32) ~(valid : Xfixes.region) ~(update : Xfixes.region)
      ~(x_off : int) ~(y_off : int) ~(target_crtc : Randr.crtc)
      ~(wait_fence : Sync.fence) ~(idle_fence : Sync.fence) ~(options : int32)
      ~(target_msc : int64) ~(divisor : int64) ~(remainder : int64)
      ~(notifies : notify list) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    Xproto.encode_pixmap buf pixmap;
    encode_int32 buf serial;
    Xfixes.encode_region buf valid;
    Xfixes.encode_region buf update;
    encode_int16 buf x_off;
    encode_int16 buf y_off;
    Randr.encode_crtc buf target_crtc;
    Sync.encode_fence buf wait_fence;
    Sync.encode_fence buf idle_fence;
    encode_int32 buf options;
    encode_int64 buf target_msc;
    encode_int64 buf divisor;
    encode_int64 buf remainder;
    buf

  let notify_msc ~(window : Xproto.window) ~(serial : int32)
      ~(target_msc : int64) ~(divisor : int64) ~(remainder : int64) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    encode_int32 buf serial;
    encode_int64 buf target_msc;
    encode_int64 buf divisor;
    encode_int64 buf remainder;
    buf

  type event = xid

  let decode_event = decode_xid

  let encode_event = encode_xid

  let select_input ~(eid : event) ~(window : Xproto.window)
      ~(event_mask : event_mask_mask) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_event buf eid;
    Xproto.encode_window buf window;
    encode_to_int encode_int32
      (fun x -> Int32.of_int (int_of_event_mask_mask x))
      buf event_mask;
    buf

  type query_capabilities_reply = { capabilities : int32 }

  let query_capabilities ~(target : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf target;
    buf

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

  let encode_context = encode_xid

  type range8 = { first : int; last : int }

  let decode_range8 buf ~at : (range8 * int) option =
    let orig = at in
    let* first, at = decode_uint8 buf ~at in
    let* last, at = decode_uint8 buf ~at in
    ignore orig;
    Some ({ first; last }, at)

  let encode_range8 buf { first; last } =
    encode_uint8 buf first;
    encode_uint8 buf last;
    ignore buf

  type range16 = { first : int; last : int }

  let decode_range16 buf ~at : (range16 * int) option =
    let orig = at in
    let* first, at = decode_uint16 buf ~at in
    let* last, at = decode_uint16 buf ~at in
    ignore orig;
    Some ({ first; last }, at)

  let encode_range16 buf { first; last } =
    encode_uint16 buf first;
    encode_uint16 buf last;
    ignore buf

  type ext_range = { major : range8; minor : range16 }

  let decode_ext_range buf ~at : (ext_range * int) option =
    let orig = at in
    let* major, at = decode_range8 buf ~at in
    let* minor, at = decode_range16 buf ~at in
    ignore orig;
    Some ({ major; minor }, at)

  let encode_ext_range buf { major; minor } =
    encode_range8 buf major;
    encode_range16 buf minor;
    ignore buf

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

  let encode_range buf
      {
        core_requests;
        core_replies;
        ext_requests;
        ext_replies;
        delivered_events;
        device_events;
        errors;
        client_started;
        client_died;
      } =
    encode_range8 buf core_requests;
    encode_range8 buf core_replies;
    encode_ext_range buf ext_requests;
    encode_ext_range buf ext_replies;
    encode_range8 buf delivered_events;
    encode_range8 buf device_events;
    encode_range8 buf errors;
    encode_bool buf client_started;
    encode_bool buf client_died;
    ignore buf

  type element_header = int

  let decode_element_header = decode_uint8

  let encode_element_header = encode_uint8

  type h_type_mask =
    [ `From_server_time | `From_client_time | `From_client_sequence ] list

  let h_type_mask_flags =
    [
      (`From_server_time, 0); (`From_client_time, 1); (`From_client_sequence, 2);
    ]

  let decode_h_type_mask i =
    List.filter_map
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      h_type_mask_flags

  let int_of_h_type_mask flags =
    List.fold_left
      (fun acc flag ->
        let code =
          match flag with
          | `From_server_time -> 0
          | `From_client_time -> 1
          | `From_client_sequence -> 2
        in
        acc lor (1 lsl code))
      0 flags

  type client_spec = int32

  let decode_client_spec = decode_int32

  let encode_client_spec = encode_int32

  type cs_enum = [ `Current_clients | `Future_clients | `All_clients ]

  let cs_enum_of_int : int -> cs_enum option = function
    | 1 -> Some `Current_clients
    | 2 -> Some `Future_clients
    | 3 -> Some `All_clients
    | _ -> None

  let int_of_cs_enum : cs_enum -> int = function
    | `Current_clients -> 1
    | `Future_clients -> 2
    | `All_clients -> 3

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

  let encode_client_info buf { client_resource; ranges } =
    encode_client_spec buf client_resource;
    encode_int32 buf (Int32.of_int (List.length ranges));
    encode_list encode_range buf ranges;
    ignore buf

  type bad_context_error = { invalid_record : int32 }

  type query_version_reply = { major_version : int; minor_version : int }

  let query_version ~(major_version : int) ~(minor_version : int) () : Buffer.t
      =
    let buf = Buffer.create 16 in
    encode_uint16 buf major_version;
    encode_uint16 buf minor_version;
    buf

  let create_context ~(context : context) ~(element_header : element_header)
      ~(client_specs : client_spec list) ~(ranges : range list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context buf context;
    encode_element_header buf element_header;
    encode_int32 buf (Int32.of_int (List.length client_specs));
    encode_int32 buf (Int32.of_int (List.length ranges));
    encode_list encode_client_spec buf client_specs;
    encode_list encode_range buf ranges;
    buf

  let register_clients ~(context : context) ~(element_header : element_header)
      ~(client_specs : client_spec list) ~(ranges : range list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context buf context;
    encode_element_header buf element_header;
    encode_int32 buf (Int32.of_int (List.length client_specs));
    encode_int32 buf (Int32.of_int (List.length ranges));
    encode_list encode_client_spec buf client_specs;
    encode_list encode_range buf ranges;
    buf

  let unregister_clients ~(context : context) ~(client_specs : client_spec list)
      () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context buf context;
    encode_int32 buf (Int32.of_int (List.length client_specs));
    encode_list encode_client_spec buf client_specs;
    buf

  type get_context_reply = {
    enabled : bool;
    element_header : element_header;
    intercepted_clients : client_info list;
  }

  let get_context ~(context : context) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context buf context;
    buf

  type enable_context_reply = {
    category : int;
    element_header : element_header;
    client_swapped : bool;
    xid_base : int32;
    server_time : int32;
    rec_sequence_num : int32;
    data : char list;
  }

  let enable_context ~(context : context) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context buf context;
    buf

  let disable_context ~(context : context) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context buf context;
    buf

  let free_context ~(context : context) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context buf context;
    buf
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

  let encode_client buf { resource_base; resource_mask } =
    encode_int32 buf resource_base;
    encode_int32 buf resource_mask;
    ignore buf

  type type_ = { resource_type : Xproto.atom; count : int32 }

  let decode_type buf ~at : (type_ * int) option =
    let orig = at in
    let* resource_type, at = Xproto.decode_atom buf ~at in
    let* count, at = decode_int32 buf ~at in
    ignore orig;
    Some ({ resource_type; count }, at)

  let encode_type buf { resource_type; count } =
    Xproto.encode_atom buf resource_type;
    encode_int32 buf count;
    ignore buf

  type client_id_mask_mask = [ `Client_xid | `Local_client_pid ] list

  let client_id_mask_mask_flags = [ (`Client_xid, 0); (`Local_client_pid, 1) ]

  let decode_client_id_mask_mask i =
    List.filter_map
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      client_id_mask_mask_flags

  let int_of_client_id_mask_mask flags =
    List.fold_left
      (fun acc flag ->
        let code =
          match flag with `Client_xid -> 0 | `Local_client_pid -> 1
        in
        acc lor (1 lsl code))
      0 flags

  type client_id_spec = { client : int32; mask : client_id_mask_mask }

  let decode_client_id_spec buf ~at : (client_id_spec * int) option =
    let orig = at in
    let* client, at = decode_int32 buf ~at in
    let* mask, at = decode_int32 buf ~at in
    let mask = decode_client_id_mask_mask (Int32.to_int mask) in
    ignore orig;
    Some ({ client; mask }, at)

  let encode_client_id_spec buf { client; mask } =
    encode_int32 buf client;
    encode_to_int encode_int32
      (fun x -> Int32.of_int (int_of_client_id_mask_mask x))
      buf mask;
    ignore buf

  type client_id_value = {
    spec : client_id_spec;
    length : int32;
    value : int32 list;
  }

  let decode_client_id_value buf ~at : (client_id_value * int) option =
    let orig = at in
    let* spec, at = decode_client_id_spec buf ~at in
    let* length, at = decode_int32 buf ~at in
    let* value, at =
      decode_list decode_int32 (Int32.to_int length / 4) buf ~at
    in
    ignore orig;
    Some ({ spec; length; value }, at)

  let encode_client_id_value buf { spec; length; value } =
    encode_client_id_spec buf spec;
    encode_int32 buf length;
    ignore buf

  type resource_id_spec = { resource : int32; type_ : int32 }

  let decode_resource_id_spec buf ~at : (resource_id_spec * int) option =
    let orig = at in
    let* resource, at = decode_int32 buf ~at in
    let* type_, at = decode_int32 buf ~at in
    ignore orig;
    Some ({ resource; type_ }, at)

  let encode_resource_id_spec buf { resource; type_ } =
    encode_int32 buf resource;
    encode_int32 buf type_;
    ignore buf

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

  let encode_resource_size_spec buf { spec; bytes; ref_count; use_count } =
    encode_resource_id_spec buf spec;
    encode_int32 buf bytes;
    encode_int32 buf ref_count;
    encode_int32 buf use_count;
    ignore buf

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

  let encode_resource_size_value buf { size; cross_references } =
    encode_resource_size_spec buf size;
    encode_int32 buf (Int32.of_int (List.length cross_references));
    encode_list encode_resource_size_spec buf cross_references;
    ignore buf

  type query_version_reply = { server_major : int; server_minor : int }

  let query_version ~(client_major : int) ~(client_minor : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint8 buf client_major;
    encode_uint8 buf client_minor;
    buf

  type query_clients_reply = { clients : client list }

  let query_clients () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  type query_client_resources_reply = { types : type_ list }

  let query_client_resources ~(xid : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf xid;
    buf

  type query_client_pixmap_bytes_reply = {
    bytes : int32;
    bytes_overflow : int32;
  }

  let query_client_pixmap_bytes ~(xid : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf xid;
    buf

  type query_client_ids_reply = { ids : client_id_value list }

  let query_client_ids ~(specs : client_id_spec list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf (Int32.of_int (List.length specs));
    encode_list encode_client_id_spec buf specs;
    buf

  type query_resource_bytes_reply = { sizes : resource_size_value list }

  let query_resource_bytes ~(client : int32) ~(specs : resource_id_spec list) ()
      : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf client;
    encode_int32 buf (Int32.of_int (List.length specs));
    encode_list encode_resource_id_spec buf specs;
    buf
end
[@@warning "-27"]

module Screensaver = struct
  type kind_enum = [ `Blanked | `Internal | `External ]

  let kind_enum_of_int : int -> kind_enum option = function
    | 0 -> Some `Blanked
    | 1 -> Some `Internal
    | 2 -> Some `External
    | _ -> None

  let int_of_kind_enum : kind_enum -> int = function
    | `Blanked -> 0
    | `Internal -> 1
    | `External -> 2

  type event_mask = [ `Notify_mask | `Cycle_mask ] list

  let event_mask_flags = [ (`Notify_mask, 0); (`Cycle_mask, 1) ]

  let decode_event_mask i =
    List.filter_map
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      event_mask_flags

  let int_of_event_mask flags =
    List.fold_left
      (fun acc flag ->
        let code = match flag with `Notify_mask -> 0 | `Cycle_mask -> 1 in
        acc lor (1 lsl code))
      0 flags

  type state_enum = [ `Off | `On | `Cycle | `Disabled ]

  let state_enum_of_int : int -> state_enum option = function
    | 0 -> Some `Off
    | 1 -> Some `On
    | 2 -> Some `Cycle
    | 3 -> Some `Disabled
    | _ -> None

  let int_of_state_enum : state_enum -> int = function
    | `Off -> 0
    | `On -> 1
    | `Cycle -> 2
    | `Disabled -> 3

  type query_version_reply = {
    server_major_version : int;
    server_minor_version : int;
  }

  let query_version ~(client_major_version : int) ~(client_minor_version : int)
      () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint8 buf client_major_version;
    encode_uint8 buf client_minor_version;
    buf

  type query_info_reply = {
    state : int;
    saver_window : Xproto.window;
    ms_until_server : int32;
    ms_since_user_input : int32;
    event_mask : int32;
    kind : kind_enum;
  }

  let query_info ~(drawable : Xproto.drawable) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_drawable buf drawable;
    buf

  let select_input ~(drawable : Xproto.drawable) ~(event_mask : event_mask) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_drawable buf drawable;
    encode_to_int encode_int32
      (fun x -> Int32.of_int (int_of_event_mask x))
      buf event_mask;
    buf

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
      ?(cursor : (Xproto.cursor_enum, Xproto.cursor) alt option) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_drawable buf drawable;
    encode_int16 buf x;
    encode_int16 buf y;
    encode_uint16 buf width;
    encode_uint16 buf height;
    encode_uint16 buf border_width;
    encode_to_int encode_char
      (fun x -> Char.unsafe_chr (Xproto.int_of_window_class_enum x))
      buf class_;
    encode_uint8 buf depth;
    Xproto.encode_visualid buf visual;
    let value_mask = 0 in
    let value_mask =
      if Option.is_some background_pixmap then value_mask lor (1 lsl 0)
      else value_mask
    in
    let value_mask =
      if Option.is_some background_pixel then value_mask lor (1 lsl 1)
      else value_mask
    in
    let value_mask =
      if Option.is_some border_pixmap then value_mask lor (1 lsl 2)
      else value_mask
    in
    let value_mask =
      if Option.is_some border_pixel then value_mask lor (1 lsl 3)
      else value_mask
    in
    let value_mask =
      if Option.is_some bit_gravity then value_mask lor (1 lsl 4)
      else value_mask
    in
    let value_mask =
      if Option.is_some win_gravity then value_mask lor (1 lsl 5)
      else value_mask
    in
    let value_mask =
      if Option.is_some backing_store then value_mask lor (1 lsl 6)
      else value_mask
    in
    let value_mask =
      if Option.is_some backing_planes then value_mask lor (1 lsl 7)
      else value_mask
    in
    let value_mask =
      if Option.is_some backing_pixel then value_mask lor (1 lsl 8)
      else value_mask
    in
    let value_mask =
      if Option.is_some override_redirect then value_mask lor (1 lsl 9)
      else value_mask
    in
    let value_mask =
      if Option.is_some save_under then value_mask lor (1 lsl 10)
      else value_mask
    in
    let value_mask =
      if Option.is_some event_mask then value_mask lor (1 lsl 11)
      else value_mask
    in
    let value_mask =
      if Option.is_some do_not_propogate_mask then value_mask lor (1 lsl 12)
      else value_mask
    in
    let value_mask =
      if Option.is_some colormap then value_mask lor (1 lsl 13) else value_mask
    in
    let value_mask =
      if Option.is_some cursor then value_mask lor (1 lsl 14) else value_mask
    in
    encode_int32 buf (Int32.of_int value_mask);
    Option.iter
      (fun background_pixmap ->
        encode_alt Xproto.encode_pixmap Xproto.int_of_back_pixmap_enum
          Xproto.encode_pixmap buf background_pixmap)
      background_pixmap;
    Option.iter
      (fun background_pixel -> encode_int32 buf background_pixel)
      background_pixel;
    Option.iter
      (fun border_pixmap ->
        encode_alt Xproto.encode_pixmap Xproto.int_of_pixmap_enum
          Xproto.encode_pixmap buf border_pixmap)
      border_pixmap;
    Option.iter (fun border_pixel -> encode_int32 buf border_pixel) border_pixel;
    Option.iter
      (fun bit_gravity ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (Xproto.int_of_gravity_enum x))
          buf bit_gravity)
      bit_gravity;
    Option.iter
      (fun win_gravity ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (Xproto.int_of_gravity_enum x))
          buf win_gravity)
      win_gravity;
    Option.iter
      (fun backing_store ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (Xproto.int_of_backing_store_enum x))
          buf backing_store)
      backing_store;
    Option.iter
      (fun backing_planes -> encode_int32 buf backing_planes)
      backing_planes;
    Option.iter
      (fun backing_pixel -> encode_int32 buf backing_pixel)
      backing_pixel;
    Option.iter
      (fun override_redirect -> Xproto.encode_bool32 buf override_redirect)
      override_redirect;
    Option.iter
      (fun save_under -> Xproto.encode_bool32 buf save_under)
      save_under;
    Option.iter
      (fun event_mask ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (Xproto.int_of_event_mask_mask x))
          buf event_mask)
      event_mask;
    Option.iter
      (fun do_not_propogate_mask ->
        encode_to_int encode_int32
          (fun x -> Int32.of_int (Xproto.int_of_event_mask_mask x))
          buf do_not_propogate_mask)
      do_not_propogate_mask;
    Option.iter
      (fun colormap ->
        encode_alt Xproto.encode_colormap Xproto.int_of_colormap_enum
          Xproto.encode_colormap buf colormap)
      colormap;
    Option.iter
      (fun cursor ->
        encode_alt Xproto.encode_cursor Xproto.int_of_cursor_enum
          Xproto.encode_cursor buf cursor)
      cursor;
    buf

  let unset_attributes ~(drawable : Xproto.drawable) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_drawable buf drawable;
    buf

  let suspend ~(suspend : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf suspend;
    buf

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

  let encode_seg = encode_xid

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

  let query_version () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  let attach ~(shmseg : seg) ~(shmid : int32) ~(read_only : bool) () : Buffer.t
      =
    let buf = Buffer.create 16 in
    encode_seg buf shmseg;
    encode_int32 buf shmid;
    encode_bool buf read_only;
    buf

  let detach ~(shmseg : seg) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_seg buf shmseg;
    buf

  let put_image ~(drawable : Xproto.drawable) ~(gc : Xproto.gcontext)
      ~(total_width : int) ~(total_height : int) ~(src_x : int) ~(src_y : int)
      ~(src_width : int) ~(src_height : int) ~(dst_x : int) ~(dst_y : int)
      ~(depth : int) ~(format : int) ~(send_event : bool) ~(shmseg : seg)
      ~(offset : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_drawable buf drawable;
    Xproto.encode_gcontext buf gc;
    encode_uint16 buf total_width;
    encode_uint16 buf total_height;
    encode_uint16 buf src_x;
    encode_uint16 buf src_y;
    encode_uint16 buf src_width;
    encode_uint16 buf src_height;
    encode_int16 buf dst_x;
    encode_int16 buf dst_y;
    encode_uint8 buf depth;
    encode_uint8 buf format;
    encode_bool buf send_event;
    encode_seg buf shmseg;
    encode_int32 buf offset;
    buf

  type get_image_reply = { depth : int; visual : Xproto.visualid; size : int32 }

  let get_image ~(drawable : Xproto.drawable) ~(x : int) ~(y : int)
      ~(width : int) ~(height : int) ~(plane_mask : int32) ~(format : int)
      ~(shmseg : seg) ~(offset : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_drawable buf drawable;
    encode_int16 buf x;
    encode_int16 buf y;
    encode_uint16 buf width;
    encode_uint16 buf height;
    encode_int32 buf plane_mask;
    encode_uint8 buf format;
    encode_seg buf shmseg;
    encode_int32 buf offset;
    buf

  let create_pixmap ~(pid : Xproto.pixmap) ~(drawable : Xproto.drawable)
      ~(width : int) ~(height : int) ~(depth : int) ~(shmseg : seg)
      ~(offset : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_pixmap buf pid;
    Xproto.encode_drawable buf drawable;
    encode_uint16 buf width;
    encode_uint16 buf height;
    encode_uint8 buf depth;
    encode_seg buf shmseg;
    encode_int32 buf offset;
    buf

  let attach_fd ~(shmseg : seg) ~(shm_fd : file_descr) ~(read_only : bool) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_seg buf shmseg;
    encode_bool buf read_only;
    buf

  type create_segment_reply = { nfd : int; shm_fd : file_descr }

  let create_segment ~(shmseg : seg) ~(size : int32) ~(read_only : bool) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_seg buf shmseg;
    encode_int32 buf size;
    encode_bool buf read_only;
    buf
end
[@@warning "-27"]

module Xc_misc = struct
  type get_version_reply = {
    server_major_version : int;
    server_minor_version : int;
  }

  let get_version ~(client_major_version : int) ~(client_minor_version : int) ()
      : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint16 buf client_major_version;
    encode_uint16 buf client_minor_version;
    buf

  type get_xid_range_reply = { start_id : int32; count : int32 }

  let get_xid_range () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  type get_xid_list_reply = { ids : int32 list }

  let get_xid_list ~(count : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf count;
    buf
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

  let encode_drm_clip_rect buf { x1; y1; x2; x3 } =
    encode_int16 buf x1;
    encode_int16 buf y1;
    encode_int16 buf x2;
    encode_int16 buf x3;
    ignore buf

  type query_version_reply = {
    dri_major_version : int;
    dri_minor_version : int;
    dri_minor_patch : int32;
  }

  let query_version () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  type query_direct_rendering_capable_reply = { is_capable : bool }

  let query_direct_rendering_capable ~(screen : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf screen;
    buf

  type open_connection_reply = {
    sarea_handle_low : int32;
    sarea_handle_high : int32;
    bus_id : char list;
  }

  let open_connection ~(screen : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf screen;
    buf

  let close_connection ~(screen : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf screen;
    buf

  type get_client_driver_name_reply = {
    client_driver_major_version : int32;
    client_driver_minor_version : int32;
    client_driver_patch_version : int32;
    client_driver_name : char list;
  }

  let get_client_driver_name ~(screen : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf screen;
    buf

  type create_context_reply = { hw_context : int32 }

  let create_context ~(screen : int32) ~(visual : int32) ~(context : int32) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf screen;
    encode_int32 buf visual;
    encode_int32 buf context;
    buf

  let destroy_context ~(screen : int32) ~(context : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf screen;
    encode_int32 buf context;
    buf

  type create_drawable_reply = { hw_drawable_handle : int32 }

  let create_drawable ~(screen : int32) ~(drawable : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf screen;
    encode_int32 buf drawable;
    buf

  let destroy_drawable ~(screen : int32) ~(drawable : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf screen;
    encode_int32 buf drawable;
    buf

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

  let get_drawable_info ~(screen : int32) ~(drawable : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf screen;
    encode_int32 buf drawable;
    buf

  type get_device_info_reply = {
    framebuffer_handle_low : int32;
    framebuffer_handle_high : int32;
    framebuffer_origin_offset : int32;
    framebuffer_size : int32;
    framebuffer_stride : int32;
    device_private : int32 list;
  }

  let get_device_info ~(screen : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf screen;
    buf

  type auth_connection_reply = { authenticated : int32 }

  let auth_connection ~(screen : int32) ~(magic : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf screen;
    encode_int32 buf magic;
    buf
end
[@@warning "-27"]

module Xf86vidmode = struct
  type syncrange = int32

  let decode_syncrange = decode_int32

  let encode_syncrange = encode_int32

  type dotclock = int32

  let decode_dotclock = decode_int32

  let encode_dotclock = encode_int32

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

  let mode_flag_mask_flags =
    [
      (`Positive_h_sync, 0);
      (`Negative_h_sync, 1);
      (`Positive_v_sync, 2);
      (`Negative_v_sync, 3);
      (`Interlace, 4);
      (`Composite_sync, 5);
      (`Positive_c_sync, 6);
      (`Negative_c_sync, 7);
      (`H_skew, 8);
      (`Broadcast, 9);
      (`Pixmux, 10);
      (`Double_clock, 11);
      (`Half_clock, 12);
    ]

  let decode_mode_flag_mask i =
    List.filter_map
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      mode_flag_mask_flags

  let int_of_mode_flag_mask flags =
    List.fold_left
      (fun acc flag ->
        let code =
          match flag with
          | `Positive_h_sync -> 0
          | `Negative_h_sync -> 1
          | `Positive_v_sync -> 2
          | `Negative_v_sync -> 3
          | `Interlace -> 4
          | `Composite_sync -> 5
          | `Positive_c_sync -> 6
          | `Negative_c_sync -> 7
          | `H_skew -> 8
          | `Broadcast -> 9
          | `Pixmux -> 10
          | `Double_clock -> 11
          | `Half_clock -> 12
        in
        acc lor (1 lsl code))
      0 flags

  type clock_flag_mask = [ `Programable ] list

  let clock_flag_mask_flags = [ (`Programable, 0) ]

  let decode_clock_flag_mask i =
    List.filter_map
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      clock_flag_mask_flags

  let int_of_clock_flag_mask flags =
    List.fold_left
      (fun acc flag ->
        let code = match flag with `Programable -> 0 in
        acc lor (1 lsl code))
      0 flags

  type permission_mask = [ `Read | `Write ] list

  let permission_mask_flags = [ (`Read, 0); (`Write, 1) ]

  let decode_permission_mask i =
    List.filter_map
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      permission_mask_flags

  let int_of_permission_mask flags =
    List.fold_left
      (fun acc flag ->
        let code = match flag with `Read -> 0 | `Write -> 1 in
        acc lor (1 lsl code))
      0 flags

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
    let flags = decode_mode_flag_mask (Int32.to_int flags) in
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

  let encode_mode_info buf
      {
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
      } =
    encode_dotclock buf dotclock;
    encode_uint16 buf hdisplay;
    encode_uint16 buf hsyncstart;
    encode_uint16 buf hsyncend;
    encode_uint16 buf htotal;
    encode_int32 buf hskew;
    encode_uint16 buf vdisplay;
    encode_uint16 buf vsyncstart;
    encode_uint16 buf vsyncend;
    encode_uint16 buf vtotal;
    encode_to_int encode_int32
      (fun x -> Int32.of_int (int_of_mode_flag_mask x))
      buf flags;
    encode_int32 buf privsize;
    ignore buf

  type query_version_reply = { major_version : int; minor_version : int }

  let query_version () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

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

  let get_mode_line ~(screen : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint16 buf screen;
    buf

  let mod_mode_line ~(screen : int32) ~(hdisplay : int) ~(hsyncstart : int)
      ~(hsyncend : int) ~(htotal : int) ~(hskew : int) ~(vdisplay : int)
      ~(vsyncstart : int) ~(vsyncend : int) ~(vtotal : int)
      ~(flags : mode_flag_mask) ~(private_ : int list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf screen;
    encode_uint16 buf hdisplay;
    encode_uint16 buf hsyncstart;
    encode_uint16 buf hsyncend;
    encode_uint16 buf htotal;
    encode_uint16 buf hskew;
    encode_uint16 buf vdisplay;
    encode_uint16 buf vsyncstart;
    encode_uint16 buf vsyncend;
    encode_uint16 buf vtotal;
    encode_to_int encode_int32
      (fun x -> Int32.of_int (int_of_mode_flag_mask x))
      buf flags;
    encode_int32 buf (Int32.of_int (List.length private_));
    encode_list encode_uint8 buf private_;
    buf

  let switch_mode ~(screen : int) ~(zoom : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint16 buf screen;
    encode_uint16 buf zoom;
    buf

  type get_monitor_reply = {
    hsync : syncrange list;
    vsync : syncrange list;
    vendor : char list;
    alignment_pad : char list;
    model : char list;
  }

  let get_monitor ~(screen : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint16 buf screen;
    buf

  let lock_mode_switch ~(screen : int) ~(lock : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint16 buf screen;
    encode_uint16 buf lock;
    buf

  type get_all_mode_lines_reply = { modeinfo : mode_info list }

  let get_all_mode_lines ~(screen : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint16 buf screen;
    buf

  let add_mode_line ~(screen : int32) ~(dotclock : dotclock) ~(hdisplay : int)
      ~(hsyncstart : int) ~(hsyncend : int) ~(htotal : int) ~(hskew : int)
      ~(vdisplay : int) ~(vsyncstart : int) ~(vsyncend : int) ~(vtotal : int)
      ~(flags : mode_flag_mask) ~(after_dotclock : dotclock)
      ~(after_hdisplay : int) ~(after_hsyncstart : int) ~(after_hsyncend : int)
      ~(after_htotal : int) ~(after_hskew : int) ~(after_vdisplay : int)
      ~(after_vsyncstart : int) ~(after_vsyncend : int) ~(after_vtotal : int)
      ~(after_flags : mode_flag_mask) ~(private_ : int list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf screen;
    encode_dotclock buf dotclock;
    encode_uint16 buf hdisplay;
    encode_uint16 buf hsyncstart;
    encode_uint16 buf hsyncend;
    encode_uint16 buf htotal;
    encode_uint16 buf hskew;
    encode_uint16 buf vdisplay;
    encode_uint16 buf vsyncstart;
    encode_uint16 buf vsyncend;
    encode_uint16 buf vtotal;
    encode_to_int encode_int32
      (fun x -> Int32.of_int (int_of_mode_flag_mask x))
      buf flags;
    encode_int32 buf (Int32.of_int (List.length private_));
    encode_dotclock buf after_dotclock;
    encode_uint16 buf after_hdisplay;
    encode_uint16 buf after_hsyncstart;
    encode_uint16 buf after_hsyncend;
    encode_uint16 buf after_htotal;
    encode_uint16 buf after_hskew;
    encode_uint16 buf after_vdisplay;
    encode_uint16 buf after_vsyncstart;
    encode_uint16 buf after_vsyncend;
    encode_uint16 buf after_vtotal;
    encode_to_int encode_int32
      (fun x -> Int32.of_int (int_of_mode_flag_mask x))
      buf after_flags;
    encode_list encode_uint8 buf private_;
    buf

  let delete_mode_line ~(screen : int32) ~(dotclock : dotclock)
      ~(hdisplay : int) ~(hsyncstart : int) ~(hsyncend : int) ~(htotal : int)
      ~(hskew : int) ~(vdisplay : int) ~(vsyncstart : int) ~(vsyncend : int)
      ~(vtotal : int) ~(flags : mode_flag_mask) ~(private_ : int list) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf screen;
    encode_dotclock buf dotclock;
    encode_uint16 buf hdisplay;
    encode_uint16 buf hsyncstart;
    encode_uint16 buf hsyncend;
    encode_uint16 buf htotal;
    encode_uint16 buf hskew;
    encode_uint16 buf vdisplay;
    encode_uint16 buf vsyncstart;
    encode_uint16 buf vsyncend;
    encode_uint16 buf vtotal;
    encode_to_int encode_int32
      (fun x -> Int32.of_int (int_of_mode_flag_mask x))
      buf flags;
    encode_int32 buf (Int32.of_int (List.length private_));
    encode_list encode_uint8 buf private_;
    buf

  type validate_mode_line_reply = { status : int32 }

  let validate_mode_line ~(screen : int32) ~(dotclock : dotclock)
      ~(hdisplay : int) ~(hsyncstart : int) ~(hsyncend : int) ~(htotal : int)
      ~(hskew : int) ~(vdisplay : int) ~(vsyncstart : int) ~(vsyncend : int)
      ~(vtotal : int) ~(flags : mode_flag_mask) ~(private_ : int list) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf screen;
    encode_dotclock buf dotclock;
    encode_uint16 buf hdisplay;
    encode_uint16 buf hsyncstart;
    encode_uint16 buf hsyncend;
    encode_uint16 buf htotal;
    encode_uint16 buf hskew;
    encode_uint16 buf vdisplay;
    encode_uint16 buf vsyncstart;
    encode_uint16 buf vsyncend;
    encode_uint16 buf vtotal;
    encode_to_int encode_int32
      (fun x -> Int32.of_int (int_of_mode_flag_mask x))
      buf flags;
    encode_int32 buf (Int32.of_int (List.length private_));
    encode_list encode_uint8 buf private_;
    buf

  let switch_to_mode ~(screen : int32) ~(dotclock : dotclock) ~(hdisplay : int)
      ~(hsyncstart : int) ~(hsyncend : int) ~(htotal : int) ~(hskew : int)
      ~(vdisplay : int) ~(vsyncstart : int) ~(vsyncend : int) ~(vtotal : int)
      ~(flags : mode_flag_mask) ~(private_ : int list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf screen;
    encode_dotclock buf dotclock;
    encode_uint16 buf hdisplay;
    encode_uint16 buf hsyncstart;
    encode_uint16 buf hsyncend;
    encode_uint16 buf htotal;
    encode_uint16 buf hskew;
    encode_uint16 buf vdisplay;
    encode_uint16 buf vsyncstart;
    encode_uint16 buf vsyncend;
    encode_uint16 buf vtotal;
    encode_to_int encode_int32
      (fun x -> Int32.of_int (int_of_mode_flag_mask x))
      buf flags;
    encode_int32 buf (Int32.of_int (List.length private_));
    encode_list encode_uint8 buf private_;
    buf

  type get_view_port_reply = { x : int32; y : int32 }

  let get_view_port ~(screen : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint16 buf screen;
    buf

  let set_view_port ~(screen : int) ~(x : int32) ~(y : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint16 buf screen;
    encode_int32 buf x;
    encode_int32 buf y;
    buf

  type get_dot_clocks_reply = {
    flags : clock_flag_mask;
    clocks : int32;
    maxclocks : int32;
    clock : int32 list;
  }

  let get_dot_clocks ~(screen : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint16 buf screen;
    buf

  let set_client_version ~(major : int) ~(minor : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint16 buf major;
    encode_uint16 buf minor;
    buf

  let set_gamma ~(screen : int) ~(red : int32) ~(green : int32) ~(blue : int32)
      () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint16 buf screen;
    encode_int32 buf red;
    encode_int32 buf green;
    encode_int32 buf blue;
    buf

  type get_gamma_reply = { red : int32; green : int32; blue : int32 }

  let get_gamma ~(screen : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint16 buf screen;
    buf

  type get_gamma_ramp_reply = {
    size : int;
    red : int list;
    green : int list;
    blue : int list;
  }

  let get_gamma_ramp ~(screen : int) ~(size : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint16 buf screen;
    encode_uint16 buf size;
    buf

  let set_gamma_ramp ~(screen : int) ~(size : int) ~(red : int list)
      ~(green : int list) ~(blue : int list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint16 buf screen;
    encode_uint16 buf size;
    buf

  type get_gamma_ramp_size_reply = { size : int }

  let get_gamma_ramp_size ~(screen : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint16 buf screen;
    buf

  type get_permissions_reply = { permissions : permission_mask }

  let get_permissions ~(screen : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint16 buf screen;
    buf

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

  let encode_screen_info buf { x_org; y_org; width; height } =
    encode_int16 buf x_org;
    encode_int16 buf y_org;
    encode_uint16 buf width;
    encode_uint16 buf height;
    ignore buf

  type query_version_reply = { major : int; minor : int }

  let query_version ~(major : int) ~(minor : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint8 buf major;
    encode_uint8 buf minor;
    buf

  type get_state_reply = { state : char; window : Xproto.window }

  let get_state ~(window : Xproto.window) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    buf

  type get_screen_count_reply = { screen_count : char; window : Xproto.window }

  let get_screen_count ~(window : Xproto.window) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    buf

  type get_screen_size_reply = {
    width : int32;
    height : int32;
    window : Xproto.window;
    screen : int32;
  }

  let get_screen_size ~(window : Xproto.window) ~(screen : int32) () : Buffer.t
      =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    encode_int32 buf screen;
    buf

  type is_active_reply = { state : int32 }

  let is_active () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  type query_screens_reply = { screen_info : screen_info list }

  let query_screens () : Buffer.t =
    let buf = Buffer.create 16 in
    buf
end
[@@warning "-27"]

module Xinput = struct
  type event_class = int32

  let decode_event_class = decode_int32

  let encode_event_class = encode_int32

  type key_code = int

  let decode_key_code = decode_uint8

  let encode_key_code = encode_uint8

  type device_id = int

  let decode_device_id = decode_uint16

  let encode_device_id = encode_uint16

  type fp1616 = int32

  let decode_fp1616 = decode_int32

  let encode_fp1616 = encode_int32

  type fp3232 = { integral : int32; frac : int32 }

  let decode_fp3232 buf ~at : (fp3232 * int) option =
    let orig = at in
    let* integral, at = decode_int32 buf ~at in
    let* frac, at = decode_int32 buf ~at in
    ignore orig;
    Some ({ integral; frac }, at)

  let encode_fp3232 buf { integral; frac } =
    encode_int32 buf integral;
    encode_int32 buf frac;
    ignore buf

  type get_extension_version_reply = {
    xi_reply_type : int;
    server_major : int;
    server_minor : int;
    present : bool;
  }

  let get_extension_version ~(name : char list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint16 buf (List.length name);
    encode_list encode_char buf name;
    buf

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

  let int_of_device_use_enum : device_use_enum -> int = function
    | `Is_x_pointer -> 0
    | `Is_x_keyboard -> 1
    | `Is_x_extension_device -> 2
    | `Is_x_extension_keyboard -> 3
    | `Is_x_extension_pointer -> 4

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

  let int_of_input_class_enum : input_class_enum -> int = function
    | `Key -> 0
    | `Button -> 1
    | `Valuator -> 2
    | `Feedback -> 3
    | `Proximity -> 4
    | `Focus -> 5
    | `Other -> 6

  type valuator_mode_enum = [ `Relative | `Absolute ]

  let valuator_mode_enum_of_int : int -> valuator_mode_enum option = function
    | 0 -> Some `Relative
    | 1 -> Some `Absolute
    | _ -> None

  let int_of_valuator_mode_enum : valuator_mode_enum -> int = function
    | `Relative -> 0
    | `Absolute -> 1

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

  let encode_device_info buf
      { device_type; device_id; num_class_info; device_use } =
    Xproto.encode_atom buf device_type;
    encode_uint8 buf device_id;
    encode_uint8 buf num_class_info;
    encode_to_int encode_uint8 int_of_device_use_enum buf device_use;
    ignore buf

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

  let encode_key_info buf { class_id; len; min_keycode; max_keycode; num_keys }
      =
    encode_to_int encode_uint8 int_of_input_class_enum buf class_id;
    encode_uint8 buf len;
    encode_key_code buf min_keycode;
    encode_key_code buf max_keycode;
    encode_uint16 buf num_keys;
    ignore buf

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

  let encode_button_info buf { class_id; len; num_buttons } =
    encode_to_int encode_uint8 int_of_input_class_enum buf class_id;
    encode_uint8 buf len;
    encode_uint16 buf num_buttons;
    ignore buf

  type axis_info = { resolution : int32; minimum : int32; maximum : int32 }

  let decode_axis_info buf ~at : (axis_info * int) option =
    let orig = at in
    let* resolution, at = decode_int32 buf ~at in
    let* minimum, at = decode_int32 buf ~at in
    let* maximum, at = decode_int32 buf ~at in
    ignore orig;
    Some ({ resolution; minimum; maximum }, at)

  let encode_axis_info buf { resolution; minimum; maximum } =
    encode_int32 buf resolution;
    encode_int32 buf minimum;
    encode_int32 buf maximum;
    ignore buf

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

  let encode_valuator_info buf { class_id; len; mode; motion_size; axes } =
    encode_to_int encode_uint8 int_of_input_class_enum buf class_id;
    encode_uint8 buf len;
    encode_uint8 buf (List.length axes);
    encode_to_int encode_uint8 int_of_valuator_mode_enum buf mode;
    encode_int32 buf motion_size;
    encode_list encode_axis_info buf axes;
    ignore buf

  type input_class_variant =
    | Key of { min_keycode : key_code; max_keycode : key_code; num_keys : int }
    | Button of { num_buttons : int }
    | Valuator of {
        mode : valuator_mode_enum;
        motion_size : int32;
        axes : axis_info list;
      }

  let decode_input_class_variant buf ~at enum :
      (input_class_variant * int) option =
    let decode_key buf ~at : (input_class_variant * int) option =
      let orig = at in
      let* min_keycode, at = decode_key_code buf ~at in
      let* max_keycode, at = decode_key_code buf ~at in
      let* num_keys, at = decode_uint16 buf ~at in
      let at = at + 2 in
      ignore orig;
      Some (Key { min_keycode; max_keycode; num_keys }, at)
    in
    let decode_button buf ~at : (input_class_variant * int) option =
      let orig = at in
      let* num_buttons, at = decode_uint16 buf ~at in
      ignore orig;
      Some (Button { num_buttons }, at)
    in
    let decode_valuator buf ~at : (input_class_variant * int) option =
      let orig = at in
      let* axes_len, at = decode_uint8 buf ~at in
      let axes_len = axes_len in
      let* mode, at =
        decode_enum decode_uint8 (fun x -> x) valuator_mode_enum_of_int buf ~at
      in
      let* motion_size, at = decode_int32 buf ~at in
      let* axes, at = decode_list decode_axis_info axes_len buf ~at in
      ignore orig;
      Some (Valuator { mode; motion_size; axes }, at)
    in
    match enum with
    | 0 -> decode_key buf ~at
    | 1 -> decode_button buf ~at
    | 2 -> decode_valuator buf ~at
    | invalid ->
        failwith (Printf.sprintf "Invalid enum tag %d for input_class " invalid)

  type input_info = { len : int; info : input_class_variant }

  let decode_input_info buf ~at : (input_info * int) option =
    let orig = at in
    let* info, at = decode_uint8 buf ~at in
    let* len, at = decode_uint8 buf ~at in
    let* info, at = decode_input_class_variant buf ~at info in
    ignore orig;
    Some ({ len; info }, at)

  let encode_input_info buf { len; info } =
    encode_uint8 buf len;
    ignore buf

  type device_name = { string : char list }

  let decode_device_name buf ~at : (device_name * int) option =
    let orig = at in
    let* len, at = decode_uint8 buf ~at in
    let len = len in
    let* string, at = decode_list decode_char len buf ~at in
    ignore orig;
    Some ({ string }, at)

  let encode_device_name buf { string } =
    encode_uint8 buf (List.length string);
    encode_list encode_char buf string;
    ignore buf

  type list_input_devices_reply = {
    xi_reply_type : int;
    devices : device_info list;
    infos : input_info list;
    names : Xproto.str list;
  }

  let list_input_devices () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  type event_type_base = int

  let decode_event_type_base = decode_uint8

  let encode_event_type_base = encode_uint8

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

  let encode_input_class_info buf { class_id; event_type_base } =
    encode_to_int encode_uint8 int_of_input_class_enum buf class_id;
    encode_event_type_base buf event_type_base;
    ignore buf

  type open_device_reply = {
    xi_reply_type : int;
    class_info : input_class_info list;
  }

  let open_device ~(device_id : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint8 buf device_id;
    buf

  let close_device ~(device_id : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint8 buf device_id;
    buf

  type set_device_mode_reply = {
    xi_reply_type : int;
    status : Xproto.grab_status_enum;
  }

  let set_device_mode ~(device_id : int) ~(mode : valuator_mode_enum) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint8 buf device_id;
    encode_to_int encode_uint8 int_of_valuator_mode_enum buf mode;
    buf

  let select_extension_event ~(window : Xproto.window)
      ~(classes : event_class list) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    encode_uint16 buf (List.length classes);
    encode_list encode_event_class buf classes;
    buf

  type get_selected_extension_events_reply = {
    xi_reply_type : int;
    this_classes : event_class list;
    all_classes : event_class list;
  }

  let get_selected_extension_events ~(window : Xproto.window) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    buf

  type propagate_mode_enum = [ `Add_to_list | `Delete_from_list ]

  let propagate_mode_enum_of_int : int -> propagate_mode_enum option = function
    | 0 -> Some `Add_to_list
    | 1 -> Some `Delete_from_list
    | _ -> None

  let int_of_propagate_mode_enum : propagate_mode_enum -> int = function
    | `Add_to_list -> 0
    | `Delete_from_list -> 1

  let change_device_dont_propagate_list ~(window : Xproto.window)
      ~(mode : propagate_mode_enum) ~(classes : event_class list) () : Buffer.t
      =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    encode_uint16 buf (List.length classes);
    encode_to_int encode_uint8 int_of_propagate_mode_enum buf mode;
    encode_list encode_event_class buf classes;
    buf

  type get_device_dont_propagate_list_reply = {
    xi_reply_type : int;
    classes : event_class list;
  }

  let get_device_dont_propagate_list ~(window : Xproto.window) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    buf

  type device_time_coord = { time : Xproto.timestamp; axisvalues : int32 list }

  let decode_device_time_coord num_axes buf ~at : (device_time_coord * int) option =
    let orig = at in
    let* time, at = Xproto.decode_timestamp buf ~at in
    let* axisvalues, at = decode_list decode_int32 num_axes buf ~at in
    ignore orig;
    Some ({ time; axisvalues }, at)

  let encode_device_time_coord buf { time; axisvalues } =
    Xproto.encode_timestamp buf time;
    ignore buf

  type get_device_motion_events_reply = {
    xi_reply_type : int;
    num_axes : int;
    device_mode : valuator_mode_enum;
    events : device_time_coord list;
  }

  let get_device_motion_events ~(start : Xproto.timestamp)
      ~(stop : (Xproto.time_enum, Xproto.timestamp) alt) ~(device_id : int) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_timestamp buf start;
    encode_alt Xproto.encode_timestamp
      (fun x -> Int32.of_int (Xproto.int_of_time_enum x))
      Xproto.encode_timestamp buf stop;
    encode_uint8 buf device_id;
    buf

  type change_keyboard_device_reply = {
    xi_reply_type : int;
    status : Xproto.grab_status_enum;
  }

  let change_keyboard_device ~(device_id : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint8 buf device_id;
    buf

  type change_pointer_device_reply = {
    xi_reply_type : int;
    status : Xproto.grab_status_enum;
  }

  let change_pointer_device ~(x_axis : int) ~(y_axis : int) ~(device_id : int)
      () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint8 buf x_axis;
    encode_uint8 buf y_axis;
    encode_uint8 buf device_id;
    buf

  type grab_device_reply = {
    xi_reply_type : int;
    status : Xproto.grab_status_enum;
  }

  let grab_device ~(grab_window : Xproto.window)
      ~(time : (Xproto.time_enum, Xproto.timestamp) alt)
      ~(this_device_mode : Xproto.grab_mode_enum)
      ~(other_device_mode : Xproto.grab_mode_enum) ~(owner_events : bool)
      ~(device_id : int) ~(classes : event_class list) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf grab_window;
    encode_alt Xproto.encode_timestamp
      (fun x -> Int32.of_int (Xproto.int_of_time_enum x))
      Xproto.encode_timestamp buf time;
    encode_uint16 buf (List.length classes);
    encode_to_int encode_uint8 Xproto.int_of_grab_mode_enum buf this_device_mode;
    encode_to_int encode_uint8 Xproto.int_of_grab_mode_enum buf
      other_device_mode;
    encode_bool buf owner_events;
    encode_uint8 buf device_id;
    encode_list encode_event_class buf classes;
    buf

  let ungrab_device ~(time : (Xproto.time_enum, Xproto.timestamp) alt)
      ~(device_id : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_alt Xproto.encode_timestamp
      (fun x -> Int32.of_int (Xproto.int_of_time_enum x))
      Xproto.encode_timestamp buf time;
    encode_uint8 buf device_id;
    buf

  type modifier_device_enum = [ `Use_x_keyboard ]

  let modifier_device_enum_of_int : int -> modifier_device_enum option =
    function
    | 255 -> Some `Use_x_keyboard
    | _ -> None

  let int_of_modifier_device_enum : modifier_device_enum -> int = function
    | `Use_x_keyboard -> 255

  let grab_device_key ~(grab_window : Xproto.window)
      ~(modifiers : Xproto.mod_mask_mask)
      ~(modifier_device : (modifier_device_enum, int) alt)
      ~(grabbed_device : int) ~(key : (Xproto.grab_enum, int) alt)
      ~(this_device_mode : Xproto.grab_mode_enum)
      ~(other_device_mode : Xproto.grab_mode_enum) ~(owner_events : bool)
      ~(classes : event_class list) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf grab_window;
    encode_uint16 buf (List.length classes);
    encode_to_int encode_uint16 Xproto.int_of_mod_mask_mask buf modifiers;
    encode_alt encode_uint8 int_of_modifier_device_enum encode_uint8 buf
      modifier_device;
    encode_uint8 buf grabbed_device;
    encode_alt encode_uint8 Xproto.int_of_grab_enum encode_uint8 buf key;
    encode_to_int encode_uint8 Xproto.int_of_grab_mode_enum buf this_device_mode;
    encode_to_int encode_uint8 Xproto.int_of_grab_mode_enum buf
      other_device_mode;
    encode_bool buf owner_events;
    encode_list encode_event_class buf classes;
    buf

  let ungrab_device_key ~(grab_window : Xproto.window)
      ~(modifiers : Xproto.mod_mask_mask)
      ~(modifier_device : (modifier_device_enum, int) alt)
      ~(key : (Xproto.grab_enum, int) alt) ~(grabbed_device : int) () : Buffer.t
      =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf grab_window;
    encode_to_int encode_uint16 Xproto.int_of_mod_mask_mask buf modifiers;
    encode_alt encode_uint8 int_of_modifier_device_enum encode_uint8 buf
      modifier_device;
    encode_alt encode_uint8 Xproto.int_of_grab_enum encode_uint8 buf key;
    encode_uint8 buf grabbed_device;
    buf

  let grab_device_button ~(grab_window : Xproto.window) ~(grabbed_device : int)
      ~(modifier_device : (modifier_device_enum, int) alt)
      ~(modifiers : Xproto.mod_mask_mask)
      ~(this_device_mode : Xproto.grab_mode_enum)
      ~(other_device_mode : Xproto.grab_mode_enum)
      ~(button : (Xproto.grab_enum, int) alt) ~(owner_events : bool)
      ~(classes : event_class list) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf grab_window;
    encode_uint8 buf grabbed_device;
    encode_alt encode_uint8 int_of_modifier_device_enum encode_uint8 buf
      modifier_device;
    encode_uint16 buf (List.length classes);
    encode_to_int encode_uint16 Xproto.int_of_mod_mask_mask buf modifiers;
    encode_to_int encode_uint8 Xproto.int_of_grab_mode_enum buf this_device_mode;
    encode_to_int encode_uint8 Xproto.int_of_grab_mode_enum buf
      other_device_mode;
    encode_alt encode_uint8 Xproto.int_of_grab_enum encode_uint8 buf button;
    encode_bool buf owner_events;
    encode_list encode_event_class buf classes;
    buf

  let ungrab_device_button ~(grab_window : Xproto.window)
      ~(modifiers : Xproto.mod_mask_mask)
      ~(modifier_device : (modifier_device_enum, int) alt)
      ~(button : (Xproto.grab_enum, int) alt) ~(grabbed_device : int) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf grab_window;
    encode_to_int encode_uint16 Xproto.int_of_mod_mask_mask buf modifiers;
    encode_alt encode_uint8 int_of_modifier_device_enum encode_uint8 buf
      modifier_device;
    encode_alt encode_uint8 Xproto.int_of_grab_enum encode_uint8 buf button;
    encode_uint8 buf grabbed_device;
    buf

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

  let int_of_device_input_mode_enum : device_input_mode_enum -> int = function
    | `Async_this_device -> 0
    | `Sync_this_device -> 1
    | `Replay_this_device -> 2
    | `Async_other_devices -> 3
    | `Async_all -> 4
    | `Sync_all -> 5

  let allow_device_events ~(time : (Xproto.time_enum, Xproto.timestamp) alt)
      ~(mode : device_input_mode_enum) ~(device_id : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_alt Xproto.encode_timestamp
      (fun x -> Int32.of_int (Xproto.int_of_time_enum x))
      Xproto.encode_timestamp buf time;
    encode_to_int encode_uint8 int_of_device_input_mode_enum buf mode;
    encode_uint8 buf device_id;
    buf

  type get_device_focus_reply = {
    xi_reply_type : int;
    focus : (Xproto.input_focus_enum, Xproto.window) alt;
    time : Xproto.timestamp;
    revert_to : Xproto.input_focus_enum;
  }

  let get_device_focus ~(device_id : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint8 buf device_id;
    buf

  let set_device_focus ~(focus : (Xproto.input_focus_enum, Xproto.window) alt)
      ~(time : (Xproto.time_enum, Xproto.timestamp) alt)
      ~(revert_to : Xproto.input_focus_enum) ~(device_id : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_alt Xproto.encode_window Xproto.int_of_input_focus_enum
      Xproto.encode_window buf focus;
    encode_alt Xproto.encode_timestamp
      (fun x -> Int32.of_int (Xproto.int_of_time_enum x))
      Xproto.encode_timestamp buf time;
    encode_to_int encode_uint8 Xproto.int_of_input_focus_enum buf revert_to;
    encode_uint8 buf device_id;
    buf

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

  let int_of_feedback_class_enum : feedback_class_enum -> int = function
    | `Keyboard -> 0
    | `Pointer -> 1
    | `String -> 2
    | `Integer -> 3
    | `Led -> 4
    | `Bell -> 5

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

  let encode_kbd_feedback_state buf
      {
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
      } =
    encode_to_int encode_uint8 int_of_feedback_class_enum buf class_id;
    encode_uint8 buf feedback_id;
    encode_uint16 buf len;
    encode_uint16 buf pitch;
    encode_uint16 buf duration;
    encode_int32 buf led_mask;
    encode_int32 buf led_values;
    encode_bool buf global_auto_repeat;
    encode_uint8 buf click;
    encode_uint8 buf percent;
    ignore buf

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

  let encode_ptr_feedback_state buf
      { class_id; feedback_id; len; accel_num; accel_denom; threshold } =
    encode_to_int encode_uint8 int_of_feedback_class_enum buf class_id;
    encode_uint8 buf feedback_id;
    encode_uint16 buf len;
    encode_uint16 buf accel_num;
    encode_uint16 buf accel_denom;
    encode_uint16 buf threshold;
    ignore buf

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

  let encode_integer_feedback_state buf
      { class_id; feedback_id; len; resolution; min_value; max_value } =
    encode_to_int encode_uint8 int_of_feedback_class_enum buf class_id;
    encode_uint8 buf feedback_id;
    encode_uint16 buf len;
    encode_int32 buf resolution;
    encode_int32 buf min_value;
    encode_int32 buf max_value;
    ignore buf

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

  let encode_string_feedback_state buf
      { class_id; feedback_id; len; max_symbols; keysyms } =
    encode_to_int encode_uint8 int_of_feedback_class_enum buf class_id;
    encode_uint8 buf feedback_id;
    encode_uint16 buf len;
    encode_uint16 buf max_symbols;
    encode_uint16 buf (List.length keysyms);
    encode_list Xproto.encode_keysym buf keysyms;
    ignore buf

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

  let encode_bell_feedback_state buf
      { class_id; feedback_id; len; percent; pitch; duration } =
    encode_to_int encode_uint8 int_of_feedback_class_enum buf class_id;
    encode_uint8 buf feedback_id;
    encode_uint16 buf len;
    encode_uint8 buf percent;
    encode_uint16 buf pitch;
    encode_uint16 buf duration;
    ignore buf

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

  let encode_led_feedback_state buf
      { class_id; feedback_id; len; led_mask; led_values } =
    encode_to_int encode_uint8 int_of_feedback_class_enum buf class_id;
    encode_uint8 buf feedback_id;
    encode_uint16 buf len;
    encode_int32 buf led_mask;
    encode_int32 buf led_values;
    ignore buf

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

  let decode_feedback_class_variant buf ~at enum :
      (feedback_class_variant * int) option =
    let decode_keyboard buf ~at : (feedback_class_variant * int) option =
      let orig = at in
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
        ( Keyboard
            {
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
    in
    let decode_pointer buf ~at : (feedback_class_variant * int) option =
      let orig = at in
      let at = at + 2 in
      let* accel_num, at = decode_uint16 buf ~at in
      let* accel_denom, at = decode_uint16 buf ~at in
      let* threshold, at = decode_uint16 buf ~at in
      ignore orig;
      Some (Pointer { accel_num; accel_denom; threshold }, at)
    in
    let decode_string buf ~at : (feedback_class_variant * int) option =
      let orig = at in
      let* max_symbols, at = decode_uint16 buf ~at in
      let* num_keysyms, at = decode_uint16 buf ~at in
      let num_keysyms = num_keysyms in
      let* keysyms, at = decode_list Xproto.decode_keysym num_keysyms buf ~at in
      ignore orig;
      Some (String { max_symbols; keysyms }, at)
    in
    let decode_integer buf ~at : (feedback_class_variant * int) option =
      let orig = at in
      let* resolution, at = decode_int32 buf ~at in
      let* min_value, at = decode_int32 buf ~at in
      let* max_value, at = decode_int32 buf ~at in
      ignore orig;
      Some (Integer { resolution; min_value; max_value }, at)
    in
    let decode_led buf ~at : (feedback_class_variant * int) option =
      let orig = at in
      let* led_mask, at = decode_int32 buf ~at in
      let* led_values, at = decode_int32 buf ~at in
      ignore orig;
      Some (Led { led_mask; led_values }, at)
    in
    let decode_bell buf ~at : (feedback_class_variant * int) option =
      let orig = at in
      let* percent, at = decode_uint8 buf ~at in
      let at = at + 3 in
      let* pitch, at = decode_uint16 buf ~at in
      let* duration, at = decode_uint16 buf ~at in
      ignore orig;
      Some (Bell { percent; pitch; duration }, at)
    in
    match enum with
    | 0 -> decode_keyboard buf ~at
    | 1 -> decode_pointer buf ~at
    | 2 -> decode_string buf ~at
    | 3 -> decode_integer buf ~at
    | 4 -> decode_led buf ~at
    | 5 -> decode_bell buf ~at
    | invalid ->
        failwith
          (Printf.sprintf "Invalid enum tag %d for feedback_class " invalid)

  type feedback_state = {
    feedback_id : int;
    len : int;
    data : feedback_class_variant;
  }

  let decode_feedback_state buf ~at : (feedback_state * int) option =
    let orig = at in
    let* data, at = decode_uint8 buf ~at in
    let* feedback_id, at = decode_uint8 buf ~at in
    let* len, at = decode_uint16 buf ~at in
    let* data, at = decode_feedback_class_variant buf ~at data in
    ignore orig;
    Some ({ feedback_id; len; data }, at)

  let encode_feedback_state buf { feedback_id; len; data } =
    encode_uint8 buf feedback_id;
    encode_uint16 buf len;
    ignore buf

  type get_feedback_control_reply = {
    xi_reply_type : int;
    feedbacks : feedback_state list;
  }

  let get_feedback_control ~(device_id : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint8 buf device_id;
    buf

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

  let encode_kbd_feedback_ctl buf
      {
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
      } =
    encode_to_int encode_uint8 int_of_feedback_class_enum buf class_id;
    encode_uint8 buf feedback_id;
    encode_uint16 buf len;
    encode_key_code buf key;
    encode_uint8 buf auto_repeat_mode;
    encode_int8 buf key_click_percent;
    encode_int8 buf bell_percent;
    encode_int16 buf bell_pitch;
    encode_int16 buf bell_duration;
    encode_int32 buf led_mask;
    encode_int32 buf led_values;
    ignore buf

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

  let encode_ptr_feedback_ctl buf
      { class_id; feedback_id; len; num; denom; threshold } =
    encode_to_int encode_uint8 int_of_feedback_class_enum buf class_id;
    encode_uint8 buf feedback_id;
    encode_uint16 buf len;
    encode_int16 buf num;
    encode_int16 buf denom;
    encode_int16 buf threshold;
    ignore buf

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

  let encode_integer_feedback_ctl buf
      { class_id; feedback_id; len; int_to_display } =
    encode_to_int encode_uint8 int_of_feedback_class_enum buf class_id;
    encode_uint8 buf feedback_id;
    encode_uint16 buf len;
    encode_int32 buf int_to_display;
    ignore buf

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

  let encode_string_feedback_ctl buf { class_id; feedback_id; len; keysyms } =
    encode_to_int encode_uint8 int_of_feedback_class_enum buf class_id;
    encode_uint8 buf feedback_id;
    encode_uint16 buf len;
    encode_uint16 buf (List.length keysyms);
    encode_list Xproto.encode_keysym buf keysyms;
    ignore buf

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

  let encode_bell_feedback_ctl buf
      { class_id; feedback_id; len; percent; pitch; duration } =
    encode_to_int encode_uint8 int_of_feedback_class_enum buf class_id;
    encode_uint8 buf feedback_id;
    encode_uint16 buf len;
    encode_int8 buf percent;
    encode_int16 buf pitch;
    encode_int16 buf duration;
    ignore buf

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

  let encode_led_feedback_ctl buf
      { class_id; feedback_id; len; led_mask; led_values } =
    encode_to_int encode_uint8 int_of_feedback_class_enum buf class_id;
    encode_uint8 buf feedback_id;
    encode_uint16 buf len;
    encode_int32 buf led_mask;
    encode_int32 buf led_values;
    ignore buf

  type feedback_ctl = {
    feedback_id : int;
    len : int;
    data : feedback_class_variant;
  }

  let decode_feedback_ctl buf ~at : (feedback_ctl * int) option =
    let orig = at in
    let* data, at = decode_uint8 buf ~at in
    let* feedback_id, at = decode_uint8 buf ~at in
    let* len, at = decode_uint16 buf ~at in
    let* data, at = decode_feedback_class_variant buf ~at data in
    ignore orig;
    Some ({ feedback_id; len; data }, at)

  let encode_feedback_ctl buf { feedback_id; len; data } =
    encode_uint8 buf feedback_id;
    encode_uint16 buf len;
    ignore buf

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

  let change_feedback_control_mask_mask_flags =
    [
      (`Key_click_percent, 0);
      (`Percent, 1);
      (`Pitch, 2);
      (`Duration, 3);
      (`Led, 4);
      (`Led_mode, 5);
      (`Key, 6);
      (`Auto_repeat_mode, 7);
      (`String, 0);
      (`Integer, 0);
      (`Accel_num, 0);
      (`Accel_denom, 1);
      (`Threshold, 2);
    ]

  let decode_change_feedback_control_mask_mask i =
    List.filter_map
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      change_feedback_control_mask_mask_flags

  let int_of_change_feedback_control_mask_mask flags =
    List.fold_left
      (fun acc flag ->
        let code =
          match flag with
          | `Key_click_percent -> 0
          | `Percent -> 1
          | `Pitch -> 2
          | `Duration -> 3
          | `Led -> 4
          | `Led_mode -> 5
          | `Key -> 6
          | `Auto_repeat_mode -> 7
          | `String -> 0
          | `Integer -> 0
          | `Accel_num -> 0
          | `Accel_denom -> 1
          | `Threshold -> 2
        in
        acc lor (1 lsl code))
      0 flags

  let change_feedback_control ~(mask : change_feedback_control_mask_mask)
      ~(device_id : int) ~(feedback_id : int) ~(feedback : feedback_ctl) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_int32
      (fun x -> Int32.of_int (int_of_change_feedback_control_mask_mask x))
      buf mask;
    encode_uint8 buf device_id;
    encode_uint8 buf feedback_id;
    encode_feedback_ctl buf feedback;
    buf

  type get_device_key_mapping_reply = {
    xi_reply_type : int;
    keysyms_per_keycode : int;
    keysyms : Xproto.keysym list;
  }

  let get_device_key_mapping ~(device_id : int) ~(first_keycode : key_code)
      ~(count : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint8 buf device_id;
    encode_key_code buf first_keycode;
    encode_uint8 buf count;
    buf

  let change_device_key_mapping ~(device_id : int) ~(first_keycode : key_code)
      ~(keysyms_per_keycode : int) ~(keycode_count : int)
      ~(keysyms : Xproto.keysym list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint8 buf device_id;
    encode_key_code buf first_keycode;
    encode_uint8 buf keysyms_per_keycode;
    encode_uint8 buf keycode_count;
    buf

  type get_device_modifier_mapping_reply = {
    xi_reply_type : int;
    keycodes_per_modifier : int;
    keymaps : int list;
  }

  let get_device_modifier_mapping ~(device_id : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint8 buf device_id;
    buf

  type set_device_modifier_mapping_reply = {
    xi_reply_type : int;
    status : Xproto.mapping_status_enum;
  }

  let set_device_modifier_mapping ~(device_id : int)
      ~(keycodes_per_modifier : int) ~(keymaps : int list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint8 buf device_id;
    encode_uint8 buf keycodes_per_modifier;
    buf

  type get_device_button_mapping_reply = { xi_reply_type : int; map : int list }

  let get_device_button_mapping ~(device_id : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint8 buf device_id;
    buf

  type set_device_button_mapping_reply = {
    xi_reply_type : int;
    status : Xproto.mapping_status_enum;
  }

  let set_device_button_mapping ~(device_id : int) ~(map : int list) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint8 buf device_id;
    encode_uint8 buf (List.length map);
    encode_list encode_uint8 buf map;
    buf

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

  let encode_key_state buf { class_id; len; num_keys; keys } =
    encode_to_int encode_uint8 int_of_input_class_enum buf class_id;
    encode_uint8 buf len;
    encode_uint8 buf num_keys;
    ignore buf

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

  let encode_button_state buf { class_id; len; num_buttons; buttons } =
    encode_to_int encode_uint8 int_of_input_class_enum buf class_id;
    encode_uint8 buf len;
    encode_uint8 buf num_buttons;
    ignore buf

  type valuator_state_mode_mask_mask =
    [ `Device_mode_absolute | `Out_of_proximity ] list

  let valuator_state_mode_mask_mask_flags =
    [ (`Device_mode_absolute, 0); (`Out_of_proximity, 1) ]

  let decode_valuator_state_mode_mask_mask i =
    List.filter_map
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      valuator_state_mode_mask_mask_flags

  let int_of_valuator_state_mode_mask_mask flags =
    List.fold_left
      (fun acc flag ->
        let code =
          match flag with `Device_mode_absolute -> 0 | `Out_of_proximity -> 1
        in
        acc lor (1 lsl code))
      0 flags

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
    let mode = decode_valuator_state_mode_mask_mask mode in
    let* valuators, at = decode_list decode_int32 num_valuators buf ~at in
    ignore orig;
    Some ({ class_id; len; mode; valuators }, at)

  let encode_valuator_state buf { class_id; len; mode; valuators } =
    encode_to_int encode_uint8 int_of_input_class_enum buf class_id;
    encode_uint8 buf len;
    encode_uint8 buf (List.length valuators);
    encode_to_int encode_uint8 int_of_valuator_state_mode_mask_mask buf mode;
    encode_list encode_int32 buf valuators;
    ignore buf

  type input_state = { len : int; data : input_class_variant }

  let decode_input_state buf ~at : (input_state * int) option =
    let orig = at in
    let* data, at = decode_uint8 buf ~at in
    let* len, at = decode_uint8 buf ~at in
    let* data, at = decode_input_class_variant buf ~at data in
    ignore orig;
    Some ({ len; data }, at)

  let encode_input_state buf { len; data } =
    encode_uint8 buf len;
    ignore buf

  type query_device_state_reply = {
    xi_reply_type : int;
    classes : input_state list;
  }

  let query_device_state ~(device_id : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint8 buf device_id;
    buf

  let device_bell ~(device_id : int) ~(feedback_id : int)
      ~(feedback_class : int) ~(percent : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint8 buf device_id;
    encode_uint8 buf feedback_id;
    encode_uint8 buf feedback_class;
    encode_int8 buf percent;
    buf

  type set_device_valuators_reply = {
    xi_reply_type : int;
    status : Xproto.grab_status_enum;
  }

  let set_device_valuators ~(device_id : int) ~(first_valuator : int)
      ~(valuators : int32 list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint8 buf device_id;
    encode_uint8 buf first_valuator;
    encode_uint8 buf (List.length valuators);
    encode_list encode_int32 buf valuators;
    buf

  type device_control_enum =
    [ `Resolution | `Abs_calib | `Core | `Enable | `Abs_area ]

  let device_control_enum_of_int : int -> device_control_enum option = function
    | 1 -> Some `Resolution
    | 2 -> Some `Abs_calib
    | 3 -> Some `Core
    | 4 -> Some `Enable
    | 5 -> Some `Abs_area
    | _ -> None

  let int_of_device_control_enum : device_control_enum -> int = function
    | `Resolution -> 1
    | `Abs_calib -> 2
    | `Core -> 3
    | `Enable -> 4
    | `Abs_area -> 5

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

  let encode_device_resolution_state buf
      { control_id; len; resolution_values; resolution_min; resolution_max } =
    encode_to_int encode_uint16 int_of_device_control_enum buf control_id;
    encode_uint16 buf len;
    encode_int32 buf (Int32.of_int (List.length resolution_values));
    encode_list encode_int32 buf resolution_values;
    encode_list encode_int32 buf resolution_min;
    encode_list encode_int32 buf resolution_max;
    ignore buf

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

  let encode_device_abs_calib_state buf
      {
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
      } =
    encode_to_int encode_uint16 int_of_device_control_enum buf control_id;
    encode_uint16 buf len;
    encode_int32 buf min_x;
    encode_int32 buf max_x;
    encode_int32 buf min_y;
    encode_int32 buf max_y;
    encode_int32 buf flip_x;
    encode_int32 buf flip_y;
    encode_int32 buf rotation;
    encode_int32 buf button_threshold;
    ignore buf

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

  let encode_device_abs_area_state buf
      { control_id; len; offset_x; offset_y; width; height; screen; following }
      =
    encode_to_int encode_uint16 int_of_device_control_enum buf control_id;
    encode_uint16 buf len;
    encode_int32 buf offset_x;
    encode_int32 buf offset_y;
    encode_int32 buf width;
    encode_int32 buf height;
    encode_int32 buf screen;
    encode_int32 buf following;
    ignore buf

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

  let encode_device_core_state buf { control_id; len; status; iscore } =
    encode_to_int encode_uint16 int_of_device_control_enum buf control_id;
    encode_uint16 buf len;
    encode_uint8 buf status;
    encode_uint8 buf iscore;
    ignore buf

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

  let encode_device_enable_state buf { control_id; len; enable } =
    encode_to_int encode_uint16 int_of_device_control_enum buf control_id;
    encode_uint16 buf len;
    encode_uint8 buf enable;
    ignore buf

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

  let decode_device_control_variant buf ~at enum :
      (device_control_variant * int) option =
    let decode_resolution buf ~at : (device_control_variant * int) option =
      let orig = at in
      let* num_valuators, at = decode_int32 buf ~at in
      let num_valuators = Int32.to_int num_valuators in
      let num_valuators = num_valuators in
      let* resolution_values, at =
        decode_list decode_int32 num_valuators buf ~at
      in
      let* resolution_min, at =
        decode_list decode_int32 num_valuators buf ~at
      in
      let* resolution_max, at =
        decode_list decode_int32 num_valuators buf ~at
      in
      ignore orig;
      Some (Resolution { resolution_values; resolution_min; resolution_max }, at)
    in
    let decode_abs_calib buf ~at : (device_control_variant * int) option =
      let orig = at in
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
        ( Abs_calib
            {
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
    in
    let decode_core buf ~at : (device_control_variant * int) option =
      let orig = at in
      let* status, at = decode_uint8 buf ~at in
      let* iscore, at = decode_uint8 buf ~at in
      let at = at + 2 in
      ignore orig;
      Some (Core { status; iscore }, at)
    in
    let decode_enable buf ~at : (device_control_variant * int) option =
      let orig = at in
      let* enable, at = decode_uint8 buf ~at in
      let at = at + 3 in
      ignore orig;
      Some (Enable { enable }, at)
    in
    let decode_abs_area buf ~at : (device_control_variant * int) option =
      let orig = at in
      let* offset_x, at = decode_int32 buf ~at in
      let* offset_y, at = decode_int32 buf ~at in
      let* width, at = decode_int32 buf ~at in
      let* height, at = decode_int32 buf ~at in
      let* screen, at = decode_int32 buf ~at in
      let* following, at = decode_int32 buf ~at in
      ignore orig;
      Some
        (Abs_area { offset_x; offset_y; width; height; screen; following }, at)
    in
    match enum with
    | 1 -> decode_resolution buf ~at
    | 2 -> decode_abs_calib buf ~at
    | 3 -> decode_core buf ~at
    | 4 -> decode_enable buf ~at
    | 5 -> decode_abs_area buf ~at
    | invalid ->
        failwith
          (Printf.sprintf "Invalid enum tag %d for device_control " invalid)

  type device_state = { len : int; data : device_control_variant }

  let decode_device_state buf ~at : (device_state * int) option =
    let orig = at in
    let* data, at = decode_uint16 buf ~at in
    let* len, at = decode_uint16 buf ~at in
    let* data, at = decode_device_control_variant buf ~at data in
    ignore orig;
    Some ({ len; data }, at)

  let encode_device_state buf { len; data } =
    encode_uint16 buf len;
    ignore buf

  type get_device_control_reply = {
    xi_reply_type : int;
    status : (Xproto.grab_status_enum, int) alt;
    control : device_state;
  }

  let get_device_control ~(control_id : device_control_enum) ~(device_id : int)
      () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_uint16 int_of_device_control_enum buf control_id;
    encode_uint8 buf device_id;
    buf

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

  let encode_device_resolution_ctl buf
      { control_id; len; first_valuator; resolution_values } =
    encode_to_int encode_uint16 int_of_device_control_enum buf control_id;
    encode_uint16 buf len;
    encode_uint8 buf first_valuator;
    encode_uint8 buf (List.length resolution_values);
    encode_list encode_int32 buf resolution_values;
    ignore buf

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

  let encode_device_abs_calib_ctl buf
      {
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
      } =
    encode_to_int encode_uint16 int_of_device_control_enum buf control_id;
    encode_uint16 buf len;
    encode_int32 buf min_x;
    encode_int32 buf max_x;
    encode_int32 buf min_y;
    encode_int32 buf max_y;
    encode_int32 buf flip_x;
    encode_int32 buf flip_y;
    encode_int32 buf rotation;
    encode_int32 buf button_threshold;
    ignore buf

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

  let encode_device_abs_area_ctrl buf
      { control_id; len; offset_x; offset_y; width; height; screen; following }
      =
    encode_to_int encode_uint16 int_of_device_control_enum buf control_id;
    encode_uint16 buf len;
    encode_int32 buf offset_x;
    encode_int32 buf offset_y;
    encode_int32 buf width;
    encode_int32 buf height;
    encode_int32 buf screen;
    encode_int32 buf following;
    ignore buf

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

  let encode_device_core_ctrl buf { control_id; len; status } =
    encode_to_int encode_uint16 int_of_device_control_enum buf control_id;
    encode_uint16 buf len;
    encode_uint8 buf status;
    ignore buf

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

  let encode_device_enable_ctrl buf { control_id; len; enable } =
    encode_to_int encode_uint16 int_of_device_control_enum buf control_id;
    encode_uint16 buf len;
    encode_uint8 buf enable;
    ignore buf

  type device_ctl = { len : int; data : device_control_variant }

  let decode_device_ctl buf ~at : (device_ctl * int) option =
    let orig = at in
    let* data, at = decode_uint16 buf ~at in
    let* len, at = decode_uint16 buf ~at in
    let* data, at = decode_device_control_variant buf ~at data in
    ignore orig;
    Some ({ len; data }, at)

  let encode_device_ctl buf { len; data } =
    encode_uint16 buf len;
    ignore buf

  type change_device_control_reply = {
    xi_reply_type : int;
    status : (Xproto.grab_status_enum, int) alt;
  }

  let change_device_control ~(control_id : device_control_enum)
      ~(device_id : int) ~(control : device_ctl) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_to_int encode_uint16 int_of_device_control_enum buf control_id;
    encode_uint8 buf device_id;
    encode_device_ctl buf control;
    buf

  type list_device_properties_reply = {
    xi_reply_type : int;
    atoms : Xproto.atom list;
  }

  let list_device_properties ~(device_id : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint8 buf device_id;
    buf

  type property_format_enum = [ `D8_bits | `D16_bits | `D32_bits ]

  let property_format_enum_of_int : int -> property_format_enum option =
    function
    | 8 -> Some `D8_bits
    | 16 -> Some `D16_bits
    | 32 -> Some `D32_bits
    | _ -> None

  let int_of_property_format_enum : property_format_enum -> int = function
    | `D8_bits -> 8
    | `D16_bits -> 16
    | `D32_bits -> 32

  type property_format_variant =
    | D8_bits of { data8 : int list }
    | D16_bits of { data16 : int list }
    | D32_bits of { data32 : int32 list }

  let decode_property_format_variant num_items buf ~at enum :
      (property_format_variant * int) option =
    let decode_8_bits buf ~at : (property_format_variant * int) option =
      let orig = at in
      let* data8, at = decode_list decode_uint8 num_items buf ~at in
      let at = at + ((at - orig) mod 4) in
      ignore orig;
      Some (D8_bits { data8 }, at)
    in
    let decode_16_bits buf ~at : (property_format_variant * int) option =
      let orig = at in
      let* data16, at = decode_list decode_uint16 num_items buf ~at in
      let at = at + ((at - orig) mod 4) in
      ignore orig;
      Some (D16_bits { data16 }, at)
    in
    let decode_32_bits buf ~at : (property_format_variant * int) option =
      let orig = at in
      let* data32, at = decode_list decode_int32 num_items buf ~at in
      ignore orig;
      Some (D32_bits { data32 }, at)
    in
    match enum with
    | 8 -> decode_8_bits buf ~at
    | 16 -> decode_16_bits buf ~at
    | 32 -> decode_32_bits buf ~at
    | invalid ->
        failwith
          (Printf.sprintf "Invalid enum tag %d for property_format " invalid)

  let change_device_property ~(property : Xproto.atom) ~(type_ : Xproto.atom)
      ~(device_id : int) ~(mode : Xproto.prop_mode_enum) ~(num_items : int32)
      ~(items : property_format_variant) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_atom buf property;
    Xproto.encode_atom buf type_;
    encode_uint8 buf device_id;
    encode_to_int encode_uint8 Xproto.int_of_prop_mode_enum buf mode;
    encode_int32 buf num_items;
    buf

  let delete_device_property ~(property : Xproto.atom) ~(device_id : int) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_atom buf property;
    encode_uint8 buf device_id;
    buf

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
      Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_atom buf property;
    Xproto.encode_atom buf type_;
    encode_int32 buf offset;
    encode_int32 buf len;
    encode_uint8 buf device_id;
    encode_bool buf delete;
    buf

  type device_enum = [ `All | `All_master ]

  let device_enum_of_int : int -> device_enum option = function
    | 0 -> Some `All
    | 1 -> Some `All_master
    | _ -> None

  let int_of_device_enum : device_enum -> int = function
    | `All -> 0
    | `All_master -> 1

  type group_info = { base : int; latched : int; locked : int; effective : int }

  let decode_group_info buf ~at : (group_info * int) option =
    let orig = at in
    let* base, at = decode_uint8 buf ~at in
    let* latched, at = decode_uint8 buf ~at in
    let* locked, at = decode_uint8 buf ~at in
    let* effective, at = decode_uint8 buf ~at in
    ignore orig;
    Some ({ base; latched; locked; effective }, at)

  let encode_group_info buf { base; latched; locked; effective } =
    encode_uint8 buf base;
    encode_uint8 buf latched;
    encode_uint8 buf locked;
    encode_uint8 buf effective;
    ignore buf

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

  let encode_modifier_info buf { base; latched; locked; effective } =
    encode_int32 buf base;
    encode_int32 buf latched;
    encode_int32 buf locked;
    encode_int32 buf effective;
    ignore buf

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
      ~(deviceid : (device_enum, device_id) alt) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    encode_alt encode_device_id int_of_device_enum encode_device_id buf deviceid;
    buf

  let xi_warp_pointer ~(src_win : Xproto.window) ~(dst_win : Xproto.window)
      ~(src_x : fp1616) ~(src_y : fp1616) ~(src_width : int) ~(src_height : int)
      ~(dst_x : fp1616) ~(dst_y : fp1616)
      ~(deviceid : (device_enum, device_id) alt) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf src_win;
    Xproto.encode_window buf dst_win;
    encode_fp1616 buf src_x;
    encode_fp1616 buf src_y;
    encode_uint16 buf src_width;
    encode_uint16 buf src_height;
    encode_fp1616 buf dst_x;
    encode_fp1616 buf dst_y;
    encode_alt encode_device_id int_of_device_enum encode_device_id buf deviceid;
    buf

  let xi_change_cursor ~(window : Xproto.window) ~(cursor : Xproto.cursor)
      ~(deviceid : (device_enum, device_id) alt) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    Xproto.encode_cursor buf cursor;
    encode_alt encode_device_id int_of_device_enum encode_device_id buf deviceid;
    buf

  type hierarchy_change_type_enum =
    [ `Add_master | `Remove_master | `Attach_slave | `Detach_slave ]

  let hierarchy_change_type_enum_of_int :
      int -> hierarchy_change_type_enum option = function
    | 1 -> Some `Add_master
    | 2 -> Some `Remove_master
    | 3 -> Some `Attach_slave
    | 4 -> Some `Detach_slave
    | _ -> None

  let int_of_hierarchy_change_type_enum : hierarchy_change_type_enum -> int =
    function
    | `Add_master -> 1
    | `Remove_master -> 2
    | `Attach_slave -> 3
    | `Detach_slave -> 4

  type change_mode_enum = [ `Attach | `Float ]

  let change_mode_enum_of_int : int -> change_mode_enum option = function
    | 1 -> Some `Attach
    | 2 -> Some `Float
    | _ -> None

  let int_of_change_mode_enum : change_mode_enum -> int = function
    | `Attach -> 1
    | `Float -> 2

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

  let encode_add_master buf { type_; len; send_core; enable; name } =
    encode_to_int encode_uint16 int_of_hierarchy_change_type_enum buf type_;
    encode_uint16 buf len;
    encode_uint16 buf (List.length name);
    encode_bool buf send_core;
    encode_bool buf enable;
    encode_list encode_char buf name;
    ignore buf

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
    let deviceid =
      match device_enum_of_int deviceid with
      | Some e -> E e
      | None -> T deviceid
    in
    let* return_mode, at =
      decode_enum decode_uint8 (fun x -> x) change_mode_enum_of_int buf ~at
    in
    let at = at + 1 in
    let* return_pointer, at = decode_device_id buf ~at in
    let return_pointer =
      match device_enum_of_int return_pointer with
      | Some e -> E e
      | None -> T return_pointer
    in
    let* return_keyboard, at = decode_device_id buf ~at in
    let return_keyboard =
      match device_enum_of_int return_keyboard with
      | Some e -> E e
      | None -> T return_keyboard
    in
    ignore orig;
    Some
      ( { type_; len; deviceid; return_mode; return_pointer; return_keyboard },
        at )

  let encode_remove_master buf
      { type_; len; deviceid; return_mode; return_pointer; return_keyboard } =
    encode_to_int encode_uint16 int_of_hierarchy_change_type_enum buf type_;
    encode_uint16 buf len;
    encode_alt encode_device_id int_of_device_enum encode_device_id buf deviceid;
    encode_to_int encode_uint8 int_of_change_mode_enum buf return_mode;
    encode_alt encode_device_id int_of_device_enum encode_device_id buf
      return_pointer;
    encode_alt encode_device_id int_of_device_enum encode_device_id buf
      return_keyboard;
    ignore buf

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
    let deviceid =
      match device_enum_of_int deviceid with
      | Some e -> E e
      | None -> T deviceid
    in
    let* master, at = decode_device_id buf ~at in
    let master =
      match device_enum_of_int master with Some e -> E e | None -> T master
    in
    ignore orig;
    Some ({ type_; len; deviceid; master }, at)

  let encode_attach_slave buf { type_; len; deviceid; master } =
    encode_to_int encode_uint16 int_of_hierarchy_change_type_enum buf type_;
    encode_uint16 buf len;
    encode_alt encode_device_id int_of_device_enum encode_device_id buf deviceid;
    encode_alt encode_device_id int_of_device_enum encode_device_id buf master;
    ignore buf

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
    let deviceid =
      match device_enum_of_int deviceid with
      | Some e -> E e
      | None -> T deviceid
    in
    let at = at + 2 in
    ignore orig;
    Some ({ type_; len; deviceid }, at)

  let encode_detach_slave buf { type_; len; deviceid } =
    encode_to_int encode_uint16 int_of_hierarchy_change_type_enum buf type_;
    encode_uint16 buf len;
    encode_alt encode_device_id int_of_device_enum encode_device_id buf deviceid;
    ignore buf

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

  let decode_hierarchy_change_type_variant buf ~at enum :
      (hierarchy_change_type_variant * int) option =
    let decode_add_master buf ~at : (hierarchy_change_type_variant * int) option
        =
      let orig = at in
      let* name_len, at = decode_uint16 buf ~at in
      let name_len = name_len in
      let* send_core, at = decode_bool buf ~at in
      let* enable, at = decode_bool buf ~at in
      let* name, at = decode_list decode_char name_len buf ~at in
      let at = at + ((at - orig) mod 4) in
      ignore orig;
      Some (Add_master { send_core; enable; name }, at)
    in
    let decode_remove_master buf ~at :
        (hierarchy_change_type_variant * int) option =
      let orig = at in
      let* deviceid, at = decode_device_id buf ~at in
      let deviceid =
        match device_enum_of_int deviceid with
        | Some e -> E e
        | None -> T deviceid
      in
      let* return_mode, at =
        decode_enum decode_uint8 (fun x -> x) change_mode_enum_of_int buf ~at
      in
      let at = at + 1 in
      let* return_pointer, at = decode_device_id buf ~at in
      let return_pointer =
        match device_enum_of_int return_pointer with
        | Some e -> E e
        | None -> T return_pointer
      in
      let* return_keyboard, at = decode_device_id buf ~at in
      let return_keyboard =
        match device_enum_of_int return_keyboard with
        | Some e -> E e
        | None -> T return_keyboard
      in
      ignore orig;
      Some
        ( Remove_master
            { deviceid; return_mode; return_pointer; return_keyboard },
          at )
    in
    let decode_attach_slave buf ~at :
        (hierarchy_change_type_variant * int) option =
      let orig = at in
      let* deviceid, at = decode_device_id buf ~at in
      let deviceid =
        match device_enum_of_int deviceid with
        | Some e -> E e
        | None -> T deviceid
      in
      let* master, at = decode_device_id buf ~at in
      let master =
        match device_enum_of_int master with Some e -> E e | None -> T master
      in
      ignore orig;
      Some (Attach_slave { deviceid; master }, at)
    in
    let decode_detach_slave buf ~at :
        (hierarchy_change_type_variant * int) option =
      let orig = at in
      let* deviceid, at = decode_device_id buf ~at in
      let deviceid =
        match device_enum_of_int deviceid with
        | Some e -> E e
        | None -> T deviceid
      in
      let at = at + 2 in
      ignore orig;
      Some (Detach_slave { deviceid }, at)
    in
    match enum with
    | 1 -> decode_add_master buf ~at
    | 2 -> decode_remove_master buf ~at
    | 3 -> decode_attach_slave buf ~at
    | 4 -> decode_detach_slave buf ~at
    | invalid ->
        failwith
          (Printf.sprintf "Invalid enum tag %d for hierarchy_change_type "
             invalid)

  type hierarchy_change = { len : int; data : hierarchy_change_type_variant }

  let decode_hierarchy_change buf ~at : (hierarchy_change * int) option =
    let orig = at in
    let* data, at = decode_uint16 buf ~at in
    let* len, at = decode_uint16 buf ~at in
    let* data, at = decode_hierarchy_change_type_variant buf ~at data in
    ignore orig;
    Some ({ len; data }, at)

  let encode_hierarchy_change buf { len; data } =
    encode_uint16 buf len;
    ignore buf

  let xi_change_hierarchy ~(changes : hierarchy_change list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint8 buf (List.length changes);
    encode_list encode_hierarchy_change buf changes;
    buf

  let xi_set_client_pointer ~(window : Xproto.window)
      ~(deviceid : (device_enum, device_id) alt) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    encode_alt encode_device_id int_of_device_enum encode_device_id buf deviceid;
    buf

  type xi_get_client_pointer_reply = {
    set : bool;
    deviceid : (device_enum, device_id) alt;
  }

  let xi_get_client_pointer ~(window : Xproto.window) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    buf

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

  let xi_event_mask_mask_flags =
    [
      (`Device_changed, 1);
      (`Key_press, 2);
      (`Key_release, 3);
      (`Button_press, 4);
      (`Button_release, 5);
      (`Motion, 6);
      (`Enter, 7);
      (`Leave, 8);
      (`Focus_in, 9);
      (`Focus_out, 10);
      (`Hierarchy, 11);
      (`Property, 12);
      (`Raw_key_press, 13);
      (`Raw_key_release, 14);
      (`Raw_button_press, 15);
      (`Raw_button_release, 16);
      (`Raw_motion, 17);
      (`Touch_begin, 18);
      (`Touch_update, 19);
      (`Touch_end, 20);
      (`Touch_ownership, 21);
      (`Raw_touch_begin, 22);
      (`Raw_touch_update, 23);
      (`Raw_touch_end, 24);
      (`Barrier_hit, 25);
      (`Barrier_leave, 26);
    ]

  let decode_xi_event_mask_mask i =
    List.filter_map
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      xi_event_mask_mask_flags

  let int_of_xi_event_mask_mask flags =
    List.fold_left
      (fun acc flag ->
        let code =
          match flag with
          | `Device_changed -> 1
          | `Key_press -> 2
          | `Key_release -> 3
          | `Button_press -> 4
          | `Button_release -> 5
          | `Motion -> 6
          | `Enter -> 7
          | `Leave -> 8
          | `Focus_in -> 9
          | `Focus_out -> 10
          | `Hierarchy -> 11
          | `Property -> 12
          | `Raw_key_press -> 13
          | `Raw_key_release -> 14
          | `Raw_button_press -> 15
          | `Raw_button_release -> 16
          | `Raw_motion -> 17
          | `Touch_begin -> 18
          | `Touch_update -> 19
          | `Touch_end -> 20
          | `Touch_ownership -> 21
          | `Raw_touch_begin -> 22
          | `Raw_touch_update -> 23
          | `Raw_touch_end -> 24
          | `Barrier_hit -> 25
          | `Barrier_leave -> 26
        in
        acc lor (1 lsl code))
      0 flags

  type event_mask = {
    deviceid : (device_enum, device_id) alt;
    mask : xi_event_mask_mask list;
  }

  let decode_event_mask buf ~at : (event_mask * int) option =
    let orig = at in
    let* deviceid, at = decode_device_id buf ~at in
    let deviceid =
      match device_enum_of_int deviceid with
      | Some e -> E e
      | None -> T deviceid
    in
    let* mask_len, at = decode_uint16 buf ~at in
    let mask_len = mask_len in
    let* mask, at = decode_list decode_int32 mask_len buf ~at in
    let mask =
      List.map (fun mask -> decode_xi_event_mask_mask (Int32.to_int mask)) mask
    in
    ignore orig;
    Some ({ deviceid; mask }, at)

  let encode_event_mask buf { deviceid; mask } =
    encode_alt encode_device_id int_of_device_enum encode_device_id buf deviceid;
    encode_uint16 buf (List.length mask);
    encode_list
      (encode_to_int encode_int32 (fun x ->
           Int32.of_int (int_of_xi_event_mask_mask x)))
      buf mask;
    ignore buf

  let xi_select_events ~(window : Xproto.window) ~(masks : event_mask list) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    encode_uint16 buf (List.length masks);
    encode_list encode_event_mask buf masks;
    buf

  type xi_query_version_reply = { major_version : int; minor_version : int }

  let xi_query_version ~(major_version : int) ~(minor_version : int) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint16 buf major_version;
    encode_uint16 buf minor_version;
    buf

  type device_class_type_enum = [ `Key | `Button | `Valuator | `Scroll | `Touch ]

  let device_class_type_enum_of_int : int -> device_class_type_enum option =
    function
    | 0 -> Some `Key
    | 1 -> Some `Button
    | 2 -> Some `Valuator
    | 3 -> Some `Scroll
    | 8 -> Some `Touch
    | _ -> None

  let int_of_device_class_type_enum : device_class_type_enum -> int = function
    | `Key -> 0
    | `Button -> 1
    | `Valuator -> 2
    | `Scroll -> 3
    | `Touch -> 8

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

  let int_of_device_type_enum : device_type_enum -> int = function
    | `Master_pointer -> 1
    | `Master_keyboard -> 2
    | `Slave_pointer -> 3
    | `Slave_keyboard -> 4
    | `Floating_slave -> 5

  type scroll_flags_mask = [ `No_emulation | `Preferred ] list

  let scroll_flags_mask_flags = [ (`No_emulation, 0); (`Preferred, 1) ]

  let decode_scroll_flags_mask i =
    List.filter_map
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      scroll_flags_mask_flags

  let int_of_scroll_flags_mask flags =
    List.fold_left
      (fun acc flag ->
        let code = match flag with `No_emulation -> 0 | `Preferred -> 1 in
        acc lor (1 lsl code))
      0 flags

  type scroll_type_enum = [ `Vertical | `Horizontal ]

  let scroll_type_enum_of_int : int -> scroll_type_enum option = function
    | 1 -> Some `Vertical
    | 2 -> Some `Horizontal
    | _ -> None

  let int_of_scroll_type_enum : scroll_type_enum -> int = function
    | `Vertical -> 1
    | `Horizontal -> 2

  type touch_mode_enum = [ `Direct | `Dependent ]

  let touch_mode_enum_of_int : int -> touch_mode_enum option = function
    | 1 -> Some `Direct
    | 2 -> Some `Dependent
    | _ -> None

  let int_of_touch_mode_enum : touch_mode_enum -> int = function
    | `Direct -> 1
    | `Dependent -> 2

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

  let encode_button_class buf { type_; len; sourceid; state; labels } =
    encode_to_int encode_uint16 int_of_device_class_type_enum buf type_;
    encode_uint16 buf len;
    encode_device_id buf sourceid;
    encode_uint16 buf (List.length labels);
    encode_list Xproto.encode_atom buf labels;
    ignore buf

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

  let encode_key_class buf { type_; len; sourceid; keys } =
    encode_to_int encode_uint16 int_of_device_class_type_enum buf type_;
    encode_uint16 buf len;
    encode_device_id buf sourceid;
    encode_uint16 buf (List.length keys);
    encode_list encode_int32 buf keys;
    ignore buf

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
    let flags = decode_scroll_flags_mask (Int32.to_int flags) in
    let* increment, at = decode_fp3232 buf ~at in
    ignore orig;
    Some ({ type_; len; sourceid; number; scroll_type; flags; increment }, at)

  let encode_scroll_class buf
      { type_; len; sourceid; number; scroll_type; flags; increment } =
    encode_to_int encode_uint16 int_of_device_class_type_enum buf type_;
    encode_uint16 buf len;
    encode_device_id buf sourceid;
    encode_uint16 buf number;
    encode_to_int encode_uint16 int_of_scroll_type_enum buf scroll_type;
    encode_to_int encode_int32
      (fun x -> Int32.of_int (int_of_scroll_flags_mask x))
      buf flags;
    encode_fp3232 buf increment;
    ignore buf

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

  let encode_touch_class buf { type_; len; sourceid; mode; num_touches } =
    encode_to_int encode_uint16 int_of_device_class_type_enum buf type_;
    encode_uint16 buf len;
    encode_device_id buf sourceid;
    encode_to_int encode_uint8 int_of_touch_mode_enum buf mode;
    encode_uint8 buf num_touches;
    ignore buf

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

  let encode_valuator_class buf
      { type_; len; sourceid; number; label; min; max; value; resolution; mode }
      =
    encode_to_int encode_uint16 int_of_device_class_type_enum buf type_;
    encode_uint16 buf len;
    encode_device_id buf sourceid;
    encode_uint16 buf number;
    Xproto.encode_atom buf label;
    encode_fp3232 buf min;
    encode_fp3232 buf max;
    encode_fp3232 buf value;
    encode_int32 buf resolution;
    encode_to_int encode_uint8 int_of_valuator_mode_enum buf mode;
    ignore buf

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

  let decode_device_class_type_variant buf ~at enum :
      (device_class_type_variant * int) option =
    let decode_key buf ~at : (device_class_type_variant * int) option =
      let orig = at in
      let* num_keys, at = decode_uint16 buf ~at in
      let num_keys = num_keys in
      let* keys, at = decode_list decode_int32 num_keys buf ~at in
      ignore orig;
      Some (Key { keys }, at)
    in
    let decode_button buf ~at : (device_class_type_variant * int) option =
      let orig = at in
      let* num_buttons, at = decode_uint16 buf ~at in
      let num_buttons = num_buttons in
      let* state, at =
        decode_list decode_int32 ((num_buttons + 31) / 32) buf ~at
      in
      let* labels, at = decode_list Xproto.decode_atom num_buttons buf ~at in
      ignore orig;
      Some (Button { state; labels }, at)
    in
    let decode_valuator buf ~at : (device_class_type_variant * int) option =
      let orig = at in
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
      Some (Valuator { number; label; min; max; value; resolution; mode }, at)
    in
    let decode_scroll buf ~at : (device_class_type_variant * int) option =
      let orig = at in
      let* number, at = decode_uint16 buf ~at in
      let* scroll_type, at =
        decode_enum decode_uint16 (fun x -> x) scroll_type_enum_of_int buf ~at
      in
      let at = at + 2 in
      let* flags, at = decode_int32 buf ~at in
      let flags = decode_scroll_flags_mask (Int32.to_int flags) in
      let* increment, at = decode_fp3232 buf ~at in
      ignore orig;
      Some (Scroll { number; scroll_type; flags; increment }, at)
    in
    let decode_touch buf ~at : (device_class_type_variant * int) option =
      let orig = at in
      let* mode, at =
        decode_enum decode_uint8 (fun x -> x) touch_mode_enum_of_int buf ~at
      in
      let* num_touches, at = decode_uint8 buf ~at in
      ignore orig;
      Some (Touch { mode; num_touches }, at)
    in
    match enum with
    | 0 -> decode_key buf ~at
    | 1 -> decode_button buf ~at
    | 2 -> decode_valuator buf ~at
    | 3 -> decode_scroll buf ~at
    | 8 -> decode_touch buf ~at
    | invalid ->
        failwith
          (Printf.sprintf "Invalid enum tag %d for device_class_type " invalid)

  type device_class = {
    len : int;
    sourceid : device_id;
    data : device_class_type_variant;
  }

  let decode_device_class buf ~at : (device_class * int) option =
    let orig = at in
    let* data, at = decode_uint16 buf ~at in
    let* len, at = decode_uint16 buf ~at in
    let* sourceid, at = decode_device_id buf ~at in
    let* data, at = decode_device_class_type_variant buf ~at data in
    ignore orig;
    Some ({ len; sourceid; data }, at)

  let encode_device_class buf { len; sourceid; data } =
    encode_uint16 buf len;
    encode_device_id buf sourceid;
    ignore buf

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
    let deviceid =
      match device_enum_of_int deviceid with
      | Some e -> E e
      | None -> T deviceid
    in
    let* type_, at =
      decode_enum decode_uint16 (fun x -> x) device_type_enum_of_int buf ~at
    in
    let* attachment, at = decode_device_id buf ~at in
    let attachment =
      match device_enum_of_int attachment with
      | Some e -> E e
      | None -> T attachment
    in
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

  let encode_xi_device_info buf
      { deviceid; type_; attachment; enabled; name; classes } =
    encode_alt encode_device_id int_of_device_enum encode_device_id buf deviceid;
    encode_to_int encode_uint16 int_of_device_type_enum buf type_;
    encode_alt encode_device_id int_of_device_enum encode_device_id buf
      attachment;
    encode_uint16 buf (List.length classes);
    encode_uint16 buf (List.length name);
    encode_bool buf enabled;
    encode_list encode_char buf name;
    encode_list encode_device_class buf classes;
    ignore buf

  type xi_query_device_reply = { infos : xi_device_info list }

  let xi_query_device ~(deviceid : (device_enum, device_id) alt) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_alt encode_device_id int_of_device_enum encode_device_id buf deviceid;
    buf

  let xi_set_focus ~(window : Xproto.window)
      ~(time : (Xproto.time_enum, Xproto.timestamp) alt)
      ~(deviceid : (device_enum, device_id) alt) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    encode_alt Xproto.encode_timestamp
      (fun x -> Int32.of_int (Xproto.int_of_time_enum x))
      Xproto.encode_timestamp buf time;
    encode_alt encode_device_id int_of_device_enum encode_device_id buf deviceid;
    buf

  type xi_get_focus_reply = { focus : Xproto.window }

  let xi_get_focus ~(deviceid : (device_enum, device_id) alt) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_alt encode_device_id int_of_device_enum encode_device_id buf deviceid;
    buf

  type grab_owner_enum = [ `No_owner | `Owner ]

  let grab_owner_enum_of_int : int -> grab_owner_enum option = function
    | 0 -> Some `No_owner
    | 1 -> Some `Owner
    | _ -> None

  let int_of_grab_owner_enum : grab_owner_enum -> int = function
    | `No_owner -> 0
    | `Owner -> 1

  type xi_grab_device_reply = { status : Xproto.grab_status_enum }

  let xi_grab_device ~(window : Xproto.window)
      ~(time : (Xproto.time_enum, Xproto.timestamp) alt)
      ~(cursor : Xproto.cursor) ~(deviceid : (device_enum, device_id) alt)
      ~(mode : Xproto.grab_mode_enum)
      ~(paired_device_mode : Xproto.grab_mode_enum)
      ~(owner_events : grab_owner_enum) ~(mask : int32 list) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    encode_alt Xproto.encode_timestamp
      (fun x -> Int32.of_int (Xproto.int_of_time_enum x))
      Xproto.encode_timestamp buf time;
    Xproto.encode_cursor buf cursor;
    encode_alt encode_device_id int_of_device_enum encode_device_id buf deviceid;
    encode_to_int encode_uint8 Xproto.int_of_grab_mode_enum buf mode;
    encode_to_int encode_uint8 Xproto.int_of_grab_mode_enum buf
      paired_device_mode;
    encode_to_int encode_bool
      (fun x -> (fun x -> x = 0) (int_of_grab_owner_enum x))
      buf owner_events;
    encode_uint16 buf (List.length mask);
    encode_list encode_int32 buf mask;
    buf

  let xi_ungrab_device ~(time : (Xproto.time_enum, Xproto.timestamp) alt)
      ~(deviceid : (device_enum, device_id) alt) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_alt Xproto.encode_timestamp
      (fun x -> Int32.of_int (Xproto.int_of_time_enum x))
      Xproto.encode_timestamp buf time;
    encode_alt encode_device_id int_of_device_enum encode_device_id buf deviceid;
    buf

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

  let int_of_event_mode_enum : event_mode_enum -> int = function
    | `Async_device -> 0
    | `Sync_device -> 1
    | `Replay_device -> 2
    | `Async_paired_device -> 3
    | `Async_pair -> 4
    | `Sync_pair -> 5
    | `Accept_touch -> 6
    | `Reject_touch -> 7

  let xi_allow_events ~(time : (Xproto.time_enum, Xproto.timestamp) alt)
      ~(deviceid : (device_enum, device_id) alt) ~(event_mode : event_mode_enum)
      ~(touchid : int32) ~(grab_window : Xproto.window) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_alt Xproto.encode_timestamp
      (fun x -> Int32.of_int (Xproto.int_of_time_enum x))
      Xproto.encode_timestamp buf time;
    encode_alt encode_device_id int_of_device_enum encode_device_id buf deviceid;
    encode_to_int encode_uint8 int_of_event_mode_enum buf event_mode;
    encode_int32 buf touchid;
    Xproto.encode_window buf grab_window;
    buf

  type grab_mode22_enum = [ `Sync | `Async | `Touch ]

  let grab_mode22_enum_of_int : int -> grab_mode22_enum option = function
    | 0 -> Some `Sync
    | 1 -> Some `Async
    | 2 -> Some `Touch
    | _ -> None

  let int_of_grab_mode22_enum : grab_mode22_enum -> int = function
    | `Sync -> 0
    | `Async -> 1
    | `Touch -> 2

  type grab_type_enum =
    [ `Button | `Keycode | `Enter | `Focus_in | `Touch_begin ]

  let grab_type_enum_of_int : int -> grab_type_enum option = function
    | 0 -> Some `Button
    | 1 -> Some `Keycode
    | 2 -> Some `Enter
    | 3 -> Some `Focus_in
    | 4 -> Some `Touch_begin
    | _ -> None

  let int_of_grab_type_enum : grab_type_enum -> int = function
    | `Button -> 0
    | `Keycode -> 1
    | `Enter -> 2
    | `Focus_in -> 3
    | `Touch_begin -> 4

  type modifier_mask_mask = [ `Any ] list

  let modifier_mask_mask_flags = [ (`Any, 31) ]

  let decode_modifier_mask_mask i =
    List.filter_map
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      modifier_mask_mask_flags

  let int_of_modifier_mask_mask flags =
    List.fold_left
      (fun acc flag ->
        let code = match flag with `Any -> 31 in
        acc lor (1 lsl code))
      0 flags

  type grab_modifier_info = {
    modifiers : (modifier_mask_mask, int32) alt;
    status : Xproto.grab_status_enum;
  }

  let decode_grab_modifier_info buf ~at : (grab_modifier_info * int) option =
    let orig = at in
    let* modifiers, at = decode_int32 buf ~at in
    let modifiers =
      match decode_modifier_mask_mask (Int32.to_int modifiers) with
      | [] -> T modifiers
      | es -> E es
    in
    let* status, at =
      decode_enum decode_uint8
        (fun x -> x)
        Xproto.grab_status_enum_of_int buf ~at
    in
    let at = at + 3 in
    ignore orig;
    Some ({ modifiers; status }, at)

  let encode_grab_modifier_info buf { modifiers; status } =
    encode_alt encode_int32
      (fun x -> Int32.of_int (int_of_modifier_mask_mask x))
      encode_int32 buf modifiers;
    encode_to_int encode_uint8 Xproto.int_of_grab_status_enum buf status;
    ignore buf

  type xi_passive_grab_device_reply = { modifiers : grab_modifier_info list }

  let xi_passive_grab_device ~(time : (Xproto.time_enum, Xproto.timestamp) alt)
      ~(grab_window : Xproto.window) ~(cursor : Xproto.cursor) ~(detail : int32)
      ~(deviceid : (device_enum, device_id) alt) ~(grab_type : grab_type_enum)
      ~(grab_mode : grab_mode22_enum)
      ~(paired_device_mode : Xproto.grab_mode_enum)
      ~(owner_events : grab_owner_enum) ~(mask : int32 list)
      ~(modifiers : int32 list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_alt Xproto.encode_timestamp
      (fun x -> Int32.of_int (Xproto.int_of_time_enum x))
      Xproto.encode_timestamp buf time;
    Xproto.encode_window buf grab_window;
    Xproto.encode_cursor buf cursor;
    encode_int32 buf detail;
    encode_alt encode_device_id int_of_device_enum encode_device_id buf deviceid;
    encode_uint16 buf (List.length modifiers);
    encode_uint16 buf (List.length mask);
    encode_to_int encode_uint8 int_of_grab_type_enum buf grab_type;
    encode_to_int encode_uint8 int_of_grab_mode22_enum buf grab_mode;
    encode_to_int encode_uint8 Xproto.int_of_grab_mode_enum buf
      paired_device_mode;
    encode_to_int encode_bool
      (fun x -> (fun x -> x = 0) (int_of_grab_owner_enum x))
      buf owner_events;
    encode_list encode_int32 buf mask;
    encode_list encode_int32 buf modifiers;
    buf

  let xi_passive_ungrab_device ~(grab_window : Xproto.window) ~(detail : int32)
      ~(deviceid : (device_enum, device_id) alt) ~(grab_type : grab_type_enum)
      ~(modifiers : int32 list) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf grab_window;
    encode_int32 buf detail;
    encode_alt encode_device_id int_of_device_enum encode_device_id buf deviceid;
    encode_uint16 buf (List.length modifiers);
    encode_to_int encode_uint8 int_of_grab_type_enum buf grab_type;
    encode_list encode_int32 buf modifiers;
    buf

  type xi_list_properties_reply = { properties : Xproto.atom list }

  let xi_list_properties ~(deviceid : (device_enum, device_id) alt) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_alt encode_device_id int_of_device_enum encode_device_id buf deviceid;
    buf

  let xi_change_property ~(deviceid : (device_enum, device_id) alt)
      ~(mode : Xproto.prop_mode_enum) ~(property : Xproto.atom)
      ~(type_ : Xproto.atom) ~(num_items : int32)
      ~(items : property_format_variant) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_alt encode_device_id int_of_device_enum encode_device_id buf deviceid;
    encode_to_int encode_uint8 Xproto.int_of_prop_mode_enum buf mode;
    Xproto.encode_atom buf property;
    Xproto.encode_atom buf type_;
    encode_int32 buf num_items;
    buf

  let xi_delete_property ~(deviceid : (device_enum, device_id) alt)
      ~(property : Xproto.atom) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_alt encode_device_id int_of_device_enum encode_device_id buf deviceid;
    Xproto.encode_atom buf property;
    buf

  type xi_get_property_reply = {
    type_ : Xproto.atom;
    bytes_after : int32;
    num_items : int32;
    items : property_format_variant;
  }

  let xi_get_property ~(deviceid : (device_enum, device_id) alt)
      ~(delete : bool) ~(property : Xproto.atom) ~(type_ : Xproto.atom)
      ~(offset : int32) ~(len : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_alt encode_device_id int_of_device_enum encode_device_id buf deviceid;
    encode_bool buf delete;
    Xproto.encode_atom buf property;
    Xproto.encode_atom buf type_;
    encode_int32 buf offset;
    encode_int32 buf len;
    buf

  type xi_get_selected_events_reply = { masks : event_mask list }

  let xi_get_selected_events ~(window : Xproto.window) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    buf

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

  let encode_barrier_release_pointer_info buf { deviceid; barrier; eventid } =
    encode_device_id buf deviceid;
    Xfixes.encode_barrier buf barrier;
    encode_int32 buf eventid;
    ignore buf

  let xi_barrier_release_pointer ~(barriers : barrier_release_pointer_info list)
      () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf (Int32.of_int (List.length barriers));
    encode_list encode_barrier_release_pointer_info buf barriers;
    buf

  type device_valuator_event = {
    device_id : int;
    device_state : int;
    num_valuators : int;
    first_valuator : int;
    valuators : int32 list;
  }

  type more_events_mask_mask = [ `More_events ] list

  let more_events_mask_mask_flags = [ (`More_events, 7) ]

  let decode_more_events_mask_mask i =
    List.filter_map
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      more_events_mask_mask_flags

  let int_of_more_events_mask_mask flags =
    List.fold_left
      (fun acc flag ->
        let code = match flag with `More_events -> 7 in
        acc lor (1 lsl code))
      0 flags

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

  let classes_reported_mask_mask_flags =
    [
      (`Out_of_proximity, 7);
      (`Device_mode_absolute, 6);
      (`Reporting_valuators, 2);
      (`Reporting_buttons, 1);
      (`Reporting_keys, 0);
    ]

  let decode_classes_reported_mask_mask i =
    List.filter_map
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      classes_reported_mask_mask_flags

  let int_of_classes_reported_mask_mask flags =
    List.fold_left
      (fun acc flag ->
        let code =
          match flag with
          | `Out_of_proximity -> 7
          | `Device_mode_absolute -> 6
          | `Reporting_valuators -> 2
          | `Reporting_buttons -> 1
          | `Reporting_keys -> 0
        in
        acc lor (1 lsl code))
      0 flags

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

  let int_of_change_device_enum : change_device_enum -> int = function
    | `New_pointer -> 0
    | `New_keyboard -> 1

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

  let int_of_device_change_enum : device_change_enum -> int = function
    | `Added -> 0
    | `Removed -> 1
    | `Enabled -> 2
    | `Disabled -> 3
    | `Unrecoverable -> 4
    | `Control_changed -> 5

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

  let int_of_change_reason_enum : change_reason_enum -> int = function
    | `Slave_switch -> 1
    | `Device_change -> 2

  type device_changed_event = {
    deviceid : (device_enum, device_id) alt;
    time : (Xproto.time_enum, Xproto.timestamp) alt;
    sourceid : (device_enum, device_id) alt;
    reason : change_reason_enum;
    classes : device_class list;
  }

  type key_event_flags_mask = [ `Key_repeat ] list

  let key_event_flags_mask_flags = [ (`Key_repeat, 16) ]

  let decode_key_event_flags_mask i =
    List.filter_map
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      key_event_flags_mask_flags

  let int_of_key_event_flags_mask flags =
    List.fold_left
      (fun acc flag ->
        let code = match flag with `Key_repeat -> 16 in
        acc lor (1 lsl code))
      0 flags

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

  let pointer_event_flags_mask_flags = [ (`Pointer_emulated, 16) ]

  let decode_pointer_event_flags_mask i =
    List.filter_map
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      pointer_event_flags_mask_flags

  let int_of_pointer_event_flags_mask flags =
    List.fold_left
      (fun acc flag ->
        let code = match flag with `Pointer_emulated -> 16 in
        acc lor (1 lsl code))
      0 flags

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

  let int_of_notify_mode_enum : notify_mode_enum -> int = function
    | `Normal -> 0
    | `Grab -> 1
    | `Ungrab -> 2
    | `While_grabbed -> 3
    | `Passive_grab -> 4
    | `Passive_ungrab -> 5

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

  let int_of_notify_detail_enum : notify_detail_enum -> int = function
    | `Ancestor -> 0
    | `Virtual -> 1
    | `Inferior -> 2
    | `Nonlinear -> 3
    | `Nonlinear_virtual -> 4
    | `Pointer -> 5
    | `Pointer_root -> 6
    | `None -> 7

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

  let hierarchy_mask_mask_flags =
    [
      (`Master_added, 0);
      (`Master_removed, 1);
      (`Slave_added, 2);
      (`Slave_removed, 3);
      (`Slave_attached, 4);
      (`Slave_detached, 5);
      (`Device_enabled, 6);
      (`Device_disabled, 7);
    ]

  let decode_hierarchy_mask_mask i =
    List.filter_map
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      hierarchy_mask_mask_flags

  let int_of_hierarchy_mask_mask flags =
    List.fold_left
      (fun acc flag ->
        let code =
          match flag with
          | `Master_added -> 0
          | `Master_removed -> 1
          | `Slave_added -> 2
          | `Slave_removed -> 3
          | `Slave_attached -> 4
          | `Slave_detached -> 5
          | `Device_enabled -> 6
          | `Device_disabled -> 7
        in
        acc lor (1 lsl code))
      0 flags

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
    let deviceid =
      match device_enum_of_int deviceid with
      | Some e -> E e
      | None -> T deviceid
    in
    let* attachment, at = decode_device_id buf ~at in
    let attachment =
      match device_enum_of_int attachment with
      | Some e -> E e
      | None -> T attachment
    in
    let* type_, at =
      decode_enum decode_uint8 (fun x -> x) device_type_enum_of_int buf ~at
    in
    let* enabled, at = decode_bool buf ~at in
    let at = at + 2 in
    let* flags, at = decode_int32 buf ~at in
    let flags = decode_hierarchy_mask_mask (Int32.to_int flags) in
    ignore orig;
    Some ({ deviceid; attachment; type_; enabled; flags }, at)

  let encode_hierarchy_info buf { deviceid; attachment; type_; enabled; flags }
      =
    encode_alt encode_device_id int_of_device_enum encode_device_id buf deviceid;
    encode_alt encode_device_id int_of_device_enum encode_device_id buf
      attachment;
    encode_to_int encode_uint8 int_of_device_type_enum buf type_;
    encode_bool buf enabled;
    encode_to_int encode_int32
      (fun x -> Int32.of_int (int_of_hierarchy_mask_mask x))
      buf flags;
    ignore buf

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

  let int_of_property_flag_enum : property_flag_enum -> int = function
    | `Deleted -> 0
    | `Created -> 1
    | `Modified -> 2

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

  let touch_event_flags_mask_flags =
    [ (`Touch_pending_end, 16); (`Touch_emulating_pointer, 17) ]

  let decode_touch_event_flags_mask i =
    List.filter_map
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      touch_event_flags_mask_flags

  let int_of_touch_event_flags_mask flags =
    List.fold_left
      (fun acc flag ->
        let code =
          match flag with
          | `Touch_pending_end -> 16
          | `Touch_emulating_pointer -> 17
        in
        acc lor (1 lsl code))
      0 flags

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

  let int_of_touch_ownership_flags_enum : touch_ownership_flags_enum -> int =
    function
    | `None -> 0

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

  let barrier_flags_mask_flags =
    [ (`Pointer_released, 0); (`Device_is_grabbed, 1) ]

  let decode_barrier_flags_mask i =
    List.filter_map
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      barrier_flags_mask_flags

  let int_of_barrier_flags_mask flags =
    List.fold_left
      (fun acc flag ->
        let code =
          match flag with `Pointer_released -> 0 | `Device_is_grabbed -> 1
        in
        acc lor (1 lsl code))
      0 flags

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
      ~(classes : event_class list) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf destination;
    encode_uint8 buf device_id;
    encode_bool buf propagate;
    encode_uint16 buf (List.length classes);
    encode_uint8 buf (List.length events);
    encode_list encode_event_for_send buf events;
    encode_list encode_event_class buf classes;
    buf

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

  let encode_string8 = encode_char

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

  let encode_printer buf { name; description } =
    encode_int32 buf (Int32.of_int (List.length name));
    encode_list encode_string8 buf name;
    encode_int32 buf (Int32.of_int (List.length description));
    encode_list encode_string8 buf description;
    ignore buf

  type pcontext = xid

  let decode_pcontext = decode_xid

  let encode_pcontext = encode_xid

  type get_doc_enum = [ `Finished | `Second_consumer ]

  let get_doc_enum_of_int : int -> get_doc_enum option = function
    | 0 -> Some `Finished
    | 1 -> Some `Second_consumer
    | _ -> None

  let int_of_get_doc_enum : get_doc_enum -> int = function
    | `Finished -> 0
    | `Second_consumer -> 1

  type ev_mask_mask =
    ([ `Print_mask | `Attribute_mask ], [ `No_event_mask ]) mask

  let ev_mask_mask_flags = [ (`Print_mask, 0); (`Attribute_mask, 1) ]

  let ev_mask_mask_values = [ (`No_event_mask, 0) ]

  let decode_ev_mask_mask i =
    match List.find_opt (fun (value, v) -> v = i) ev_mask_mask_values with
    | Some (value, _) -> V value
    | None ->
        F
          (List.filter_map
             (fun (flag, f) ->
               if f land (1 lsl i) <> 0 then Some flag else None)
             ev_mask_mask_flags)

  let int_of_ev_mask_mask = function
    | F flags ->
        List.fold_left
          (fun acc flag ->
            let code =
              match flag with `Print_mask -> 0 | `Attribute_mask -> 1
            in
            acc lor (1 lsl code))
          0 flags
    | V `No_event_mask -> 0

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

  let int_of_detail_enum : detail_enum -> int = function
    | `Start_job_notify -> 1
    | `End_job_notify -> 2
    | `Start_doc_notify -> 3
    | `End_doc_notify -> 4
    | `Start_page_notify -> 5
    | `End_page_notify -> 6

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

  let int_of_attr_enum : attr_enum -> int = function
    | `Job_attr -> 1
    | `Doc_attr -> 2
    | `Page_attr -> 3
    | `Printer_attr -> 4
    | `Server_attr -> 5
    | `Medium_attr -> 6
    | `Spooler_attr -> 7

  type print_query_version_reply = { major_version : int; minor_version : int }

  let print_query_version () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  type print_get_printer_list_reply = { printers : printer list }

  let print_get_printer_list ~(printer_name : string8 list)
      ~(locale : string8 list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf (Int32.of_int (List.length printer_name));
    encode_int32 buf (Int32.of_int (List.length locale));
    encode_list encode_string8 buf printer_name;
    encode_list encode_string8 buf locale;
    buf

  let print_rehash_printer_list () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  let create_context ~(context_id : int32) ~(printer_name : string8 list)
      ~(locale : string8 list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf context_id;
    encode_int32 buf (Int32.of_int (List.length printer_name));
    encode_int32 buf (Int32.of_int (List.length locale));
    encode_list encode_string8 buf printer_name;
    encode_list encode_string8 buf locale;
    buf

  let print_set_context ~(context : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf context;
    buf

  type print_get_context_reply = { context : int32 }

  let print_get_context () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  let print_destroy_context ~(context : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf context;
    buf

  type print_get_screen_of_context_reply = { root : Xproto.window }

  let print_get_screen_of_context () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  let print_start_job ~(output_mode : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint8 buf output_mode;
    buf

  let print_end_job ~(cancel : bool) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_bool buf cancel;
    buf

  let print_start_doc ~(driver_mode : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint8 buf driver_mode;
    buf

  let print_end_doc ~(cancel : bool) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_bool buf cancel;
    buf

  let print_put_document_data ~(drawable : Xproto.drawable) ~(data : char list)
      ~(doc_format : string8 list) ~(options : string8 list) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_drawable buf drawable;
    encode_int32 buf (Int32.of_int (List.length data));
    encode_uint16 buf (List.length doc_format);
    encode_uint16 buf (List.length options);
    encode_list encode_char buf data;
    encode_list encode_string8 buf doc_format;
    encode_list encode_string8 buf options;
    buf

  type print_get_document_data_reply = {
    status_code : int32;
    finished_flag : int32;
    data : char list;
  }

  let print_get_document_data ~(context : pcontext) ~(max_bytes : int32) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_pcontext buf context;
    encode_int32 buf max_bytes;
    buf

  let print_start_page ~(window : Xproto.window) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    buf

  let print_end_page ~(cancel : bool) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_bool buf cancel;
    buf

  let print_select_input ~(context : pcontext) ~(event_mask : int32) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_pcontext buf context;
    encode_int32 buf event_mask;
    buf

  type print_input_selected_reply = {
    event_mask : int32;
    all_events_mask : int32;
  }

  let print_input_selected ~(context : pcontext) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_pcontext buf context;
    buf

  type print_get_attributes_reply = { attributes : string8 list }

  let print_get_attributes ~(context : pcontext) ~(pool : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_pcontext buf context;
    encode_uint8 buf pool;
    buf

  type print_get_one_attributes_reply = { value : string8 list }

  let print_get_one_attributes ~(context : pcontext) ~(pool : int)
      ~(name : string8 list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_pcontext buf context;
    encode_int32 buf (Int32.of_int (List.length name));
    encode_uint8 buf pool;
    encode_list encode_string8 buf name;
    buf

  let print_set_attributes ~(context : pcontext) ~(string_len : int32)
      ~(pool : int) ~(rule : int) ~(attributes : string8 list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_pcontext buf context;
    encode_int32 buf string_len;
    encode_uint8 buf pool;
    encode_uint8 buf rule;
    buf

  type print_get_page_dimensions_reply = {
    width : int;
    height : int;
    offset_x : int;
    offset_y : int;
    reproducible_width : int;
    reproducible_height : int;
  }

  let print_get_page_dimensions ~(context : pcontext) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_pcontext buf context;
    buf

  type print_query_screens_reply = { roots : Xproto.window list }

  let print_query_screens () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  type print_set_image_resolution_reply = {
    status : bool;
    previous_resolutions : int;
  }

  let print_set_image_resolution ~(context : pcontext) ~(image_resolution : int)
      () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_pcontext buf context;
    encode_uint16 buf image_resolution;
    buf

  type print_get_image_resolution_reply = { image_resolution : int }

  let print_get_image_resolution ~(context : pcontext) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_pcontext buf context;
    buf

  type notify_event = { detail : int; context : pcontext; cancel : bool }

  type attribut_notify_event = { detail : int; context : pcontext }

  type bad_context_error = unit

  type bad_sequence_error = unit
end
[@@warning "-27"]

module Xselinux = struct
  type query_version_reply = { server_major : int; server_minor : int }

  let query_version ~(client_major : int) ~(client_minor : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint8 buf client_major;
    encode_uint8 buf client_minor;
    buf

  let set_device_create_context ~(context : char list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf (Int32.of_int (List.length context));
    encode_list encode_char buf context;
    buf

  type get_device_create_context_reply = { context : char list }

  let get_device_create_context () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  let set_device_context ~(device : int32) ~(context : char list) () : Buffer.t
      =
    let buf = Buffer.create 16 in
    encode_int32 buf device;
    encode_int32 buf (Int32.of_int (List.length context));
    encode_list encode_char buf context;
    buf

  type get_device_context_reply = { context : char list }

  let get_device_context ~(device : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf device;
    buf

  let set_window_create_context ~(context : char list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf (Int32.of_int (List.length context));
    encode_list encode_char buf context;
    buf

  type get_window_create_context_reply = { context : char list }

  let get_window_create_context () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  type get_window_context_reply = { context : char list }

  let get_window_context ~(window : Xproto.window) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    buf

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

  let encode_list_item buf { name; object_context; data_context } =
    Xproto.encode_atom buf name;
    encode_int32 buf (Int32.of_int (List.length object_context));
    encode_int32 buf (Int32.of_int (List.length data_context));
    encode_list encode_char buf object_context;
    encode_list encode_char buf data_context;
    ignore buf

  let set_property_create_context ~(context : char list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf (Int32.of_int (List.length context));
    encode_list encode_char buf context;
    buf

  type get_property_create_context_reply = { context : char list }

  let get_property_create_context () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  let set_property_use_context ~(context : char list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf (Int32.of_int (List.length context));
    encode_list encode_char buf context;
    buf

  type get_property_use_context_reply = { context : char list }

  let get_property_use_context () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  type get_property_context_reply = { context : char list }

  let get_property_context ~(window : Xproto.window) ~(property : Xproto.atom)
      () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    Xproto.encode_atom buf property;
    buf

  type get_property_data_context_reply = { context : char list }

  let get_property_data_context ~(window : Xproto.window)
      ~(property : Xproto.atom) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    Xproto.encode_atom buf property;
    buf

  type list_properties_reply = { properties : list_item list }

  let list_properties ~(window : Xproto.window) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    buf

  let set_selection_create_context ~(context : char list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf (Int32.of_int (List.length context));
    encode_list encode_char buf context;
    buf

  type get_selection_create_context_reply = { context : char list }

  let get_selection_create_context () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  let set_selection_use_context ~(context : char list) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf (Int32.of_int (List.length context));
    encode_list encode_char buf context;
    buf

  type get_selection_use_context_reply = { context : char list }

  let get_selection_use_context () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  type get_selection_context_reply = { context : char list }

  let get_selection_context ~(selection : Xproto.atom) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_atom buf selection;
    buf

  type get_selection_data_context_reply = { context : char list }

  let get_selection_data_context ~(selection : Xproto.atom) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_atom buf selection;
    buf

  type list_selections_reply = { selections : list_item list }

  let list_selections () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  type get_client_context_reply = { context : char list }

  let get_client_context ~(resource : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_int32 buf resource;
    buf
end
[@@warning "-27"]

module Xtest = struct
  type get_version_reply = { major_version : int; minor_version : int }

  let get_version ~(major_version : int) ~(minor_version : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_uint8 buf major_version;
    encode_uint16 buf minor_version;
    buf

  type cursor_enum = [ `None | `Current ]

  let cursor_enum_of_int : int -> cursor_enum option = function
    | 0 -> Some `None
    | 1 -> Some `Current
    | _ -> None

  let int_of_cursor_enum : cursor_enum -> int = function
    | `None -> 0
    | `Current -> 1

  type compare_cursor_reply = { same : bool }

  let compare_cursor ~(window : Xproto.window) ~(cursor : Xproto.cursor) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    Xproto.encode_cursor buf cursor;
    buf

  let fake_input ~(type_ : char) ~(detail : char) ~(time : int32)
      ~(root : Xproto.window) ~(root_x : int) ~(root_y : int) ~(deviceid : int)
      () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_char buf type_;
    encode_char buf detail;
    encode_int32 buf time;
    Xproto.encode_window buf root;
    encode_int16 buf root_x;
    encode_int16 buf root_y;
    encode_uint8 buf deviceid;
    buf

  let grab_control ~(impervious : bool) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_bool buf impervious;
    buf
end
[@@warning "-27"]

module Xv = struct
  type port = xid

  let decode_port = decode_xid

  let encode_port = encode_xid

  type encoding = xid

  let decode_encoding = decode_xid

  let encode_encoding = encode_xid

  type type_mask =
    [ `Input_mask | `Output_mask | `Video_mask | `Still_mask | `Image_mask ]
    list

  let type_mask_flags =
    [
      (`Input_mask, 0);
      (`Output_mask, 1);
      (`Video_mask, 2);
      (`Still_mask, 3);
      (`Image_mask, 4);
    ]

  let decode_type_mask i =
    List.filter_map
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      type_mask_flags

  let int_of_type_mask flags =
    List.fold_left
      (fun acc flag ->
        let code =
          match flag with
          | `Input_mask -> 0
          | `Output_mask -> 1
          | `Video_mask -> 2
          | `Still_mask -> 3
          | `Image_mask -> 4
        in
        acc lor (1 lsl code))
      0 flags

  type image_format_info_type_enum = [ `Rgb | `Yuv ]

  let image_format_info_type_enum_of_int :
      int -> image_format_info_type_enum option = function
    | 0 -> Some `Rgb
    | 1 -> Some `Yuv
    | _ -> None

  let int_of_image_format_info_type_enum : image_format_info_type_enum -> int =
    function
    | `Rgb -> 0
    | `Yuv -> 1

  type image_format_info_format_enum = [ `Packed | `Planar ]

  let image_format_info_format_enum_of_int :
      int -> image_format_info_format_enum option = function
    | 0 -> Some `Packed
    | 1 -> Some `Planar
    | _ -> None

  let int_of_image_format_info_format_enum :
      image_format_info_format_enum -> int = function
    | `Packed -> 0
    | `Planar -> 1

  type attribute_flag_mask = [ `Gettable | `Settable ] list

  let attribute_flag_mask_flags = [ (`Gettable, 0); (`Settable, 1) ]

  let decode_attribute_flag_mask i =
    List.filter_map
      (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      attribute_flag_mask_flags

  let int_of_attribute_flag_mask flags =
    List.fold_left
      (fun acc flag ->
        let code = match flag with `Gettable -> 0 | `Settable -> 1 in
        acc lor (1 lsl code))
      0 flags

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

  let int_of_video_notify_reason_enum : video_notify_reason_enum -> int =
    function
    | `Started -> 0
    | `Stopped -> 1
    | `Busy -> 2
    | `Preempted -> 3
    | `Hard_error -> 4

  type scanline_order_enum = [ `Top_to_bottom | `Bottom_to_top ]

  let scanline_order_enum_of_int : int -> scanline_order_enum option = function
    | 0 -> Some `Top_to_bottom
    | 1 -> Some `Bottom_to_top
    | _ -> None

  let int_of_scanline_order_enum : scanline_order_enum -> int = function
    | `Top_to_bottom -> 0
    | `Bottom_to_top -> 1

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

  let int_of_grab_port_status_enum : grab_port_status_enum -> int = function
    | `Success -> 0
    | `Bad_extension -> 1
    | `Already_grabbed -> 2
    | `Invalid_time -> 3
    | `Bad_reply -> 4
    | `Bad_alloc -> 5

  type rational = { numerator : int32; denominator : int32 }

  let decode_rational buf ~at : (rational * int) option =
    let orig = at in
    let* numerator, at = decode_int32 buf ~at in
    let* denominator, at = decode_int32 buf ~at in
    ignore orig;
    Some ({ numerator; denominator }, at)

  let encode_rational buf { numerator; denominator } =
    encode_int32 buf numerator;
    encode_int32 buf denominator;
    ignore buf

  type format = { visual : Xproto.visualid; depth : int }

  let decode_format buf ~at : (format * int) option =
    let orig = at in
    let* visual, at = Xproto.decode_visualid buf ~at in
    let* depth, at = decode_uint8 buf ~at in
    let at = at + 3 in
    ignore orig;
    Some ({ visual; depth }, at)

  let encode_format buf { visual; depth } =
    Xproto.encode_visualid buf visual;
    encode_uint8 buf depth;
    ignore buf

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
    let type_ = decode_type_mask type_ in
    let at = at + 1 in
    let* name, at = decode_list decode_char name_size buf ~at in
    let at = at + ((at - orig) mod 4) in
    let* formats, at = decode_list decode_format num_formats buf ~at in
    ignore orig;
    Some ({ base_id; num_ports; type_; name; formats }, at)

  let encode_adaptor_info buf { base_id; num_ports; type_; name; formats } =
    encode_port buf base_id;
    encode_uint16 buf (List.length name);
    encode_uint16 buf num_ports;
    encode_uint16 buf (List.length formats);
    encode_to_int encode_uint8 int_of_type_mask buf type_;
    encode_list encode_char buf name;
    encode_list encode_format buf formats;
    ignore buf

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

  let encode_encoding_info buf { encoding; width; height; rate; name } =
    encode_encoding buf encoding;
    encode_uint16 buf (List.length name);
    encode_uint16 buf width;
    encode_uint16 buf height;
    encode_rational buf rate;
    encode_list encode_char buf name;
    ignore buf

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

  let encode_image buf { id; width; height; pitches; offsets; data } =
    encode_int32 buf id;
    encode_uint16 buf width;
    encode_uint16 buf height;
    encode_int32 buf (Int32.of_int (List.length data));
    encode_int32 buf (Int32.of_int (List.length pitches));
    encode_list encode_int32 buf pitches;
    encode_list encode_int32 buf offsets;
    encode_list encode_uint8 buf data;
    ignore buf

  type attribute_info = {
    flags : attribute_flag_mask;
    min : int32;
    max : int32;
    name : char list;
  }

  let decode_attribute_info buf ~at : (attribute_info * int) option =
    let orig = at in
    let* flags, at = decode_int32 buf ~at in
    let flags = decode_attribute_flag_mask (Int32.to_int flags) in
    let* min, at = decode_int32 buf ~at in
    let* max, at = decode_int32 buf ~at in
    let* size, at = decode_int32 buf ~at in
    let size = Int32.to_int size in
    let size = size in
    let* name, at = decode_list decode_char size buf ~at in
    let at = at + ((at - orig) mod 4) in
    ignore orig;
    Some ({ flags; min; max; name }, at)

  let encode_attribute_info buf { flags; min; max; name } =
    encode_to_int encode_int32
      (fun x -> Int32.of_int (int_of_attribute_flag_mask x))
      buf flags;
    encode_int32 buf min;
    encode_int32 buf max;
    encode_int32 buf (Int32.of_int (List.length name));
    encode_list encode_char buf name;
    ignore buf

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

  let encode_image_format_info buf
      {
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
      } =
    encode_int32 buf id;
    encode_to_int encode_uint8 int_of_image_format_info_type_enum buf type_;
    encode_to_int encode_uint8 Xproto.int_of_image_order_enum buf byte_order;
    encode_uint8 buf bpp;
    encode_uint8 buf num_planes;
    encode_uint8 buf depth;
    encode_int32 buf red_mask;
    encode_int32 buf green_mask;
    encode_int32 buf blue_mask;
    encode_to_int encode_uint8 int_of_image_format_info_format_enum buf format;
    encode_int32 buf y_sample_bits;
    encode_int32 buf u_sample_bits;
    encode_int32 buf v_sample_bits;
    encode_int32 buf vhorz_y_period;
    encode_int32 buf vhorz_u_period;
    encode_int32 buf vhorz_v_period;
    encode_int32 buf vvert_y_period;
    encode_int32 buf vvert_u_period;
    encode_int32 buf vvert_v_period;
    encode_to_int encode_uint8 int_of_scanline_order_enum buf vscanline_order;
    ignore buf

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

  let query_extension () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  type query_adaptors_reply = { info : adaptor_info list }

  let query_adaptors ~(window : Xproto.window) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_window buf window;
    buf

  type query_encodings_reply = { info : encoding_info list }

  let query_encodings ~(port : port) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_port buf port;
    buf

  type grab_port_reply = { result : grab_port_status_enum }

  let grab_port ~(port : port)
      ~(time : (Xproto.time_enum, Xproto.timestamp) alt) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_port buf port;
    encode_alt Xproto.encode_timestamp
      (fun x -> Int32.of_int (Xproto.int_of_time_enum x))
      Xproto.encode_timestamp buf time;
    buf

  let ungrab_port ~(port : port)
      ~(time : (Xproto.time_enum, Xproto.timestamp) alt) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_port buf port;
    encode_alt Xproto.encode_timestamp
      (fun x -> Int32.of_int (Xproto.int_of_time_enum x))
      Xproto.encode_timestamp buf time;
    buf

  let put_video ~(port : port) ~(drawable : Xproto.drawable)
      ~(gc : Xproto.gcontext) ~(vid_x : int) ~(vid_y : int) ~(vid_w : int)
      ~(vid_h : int) ~(drw_x : int) ~(drw_y : int) ~(drw_w : int) ~(drw_h : int)
      () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_port buf port;
    Xproto.encode_drawable buf drawable;
    Xproto.encode_gcontext buf gc;
    encode_int16 buf vid_x;
    encode_int16 buf vid_y;
    encode_uint16 buf vid_w;
    encode_uint16 buf vid_h;
    encode_int16 buf drw_x;
    encode_int16 buf drw_y;
    encode_uint16 buf drw_w;
    encode_uint16 buf drw_h;
    buf

  let put_still ~(port : port) ~(drawable : Xproto.drawable)
      ~(gc : Xproto.gcontext) ~(vid_x : int) ~(vid_y : int) ~(vid_w : int)
      ~(vid_h : int) ~(drw_x : int) ~(drw_y : int) ~(drw_w : int) ~(drw_h : int)
      () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_port buf port;
    Xproto.encode_drawable buf drawable;
    Xproto.encode_gcontext buf gc;
    encode_int16 buf vid_x;
    encode_int16 buf vid_y;
    encode_uint16 buf vid_w;
    encode_uint16 buf vid_h;
    encode_int16 buf drw_x;
    encode_int16 buf drw_y;
    encode_uint16 buf drw_w;
    encode_uint16 buf drw_h;
    buf

  let get_video ~(port : port) ~(drawable : Xproto.drawable)
      ~(gc : Xproto.gcontext) ~(vid_x : int) ~(vid_y : int) ~(vid_w : int)
      ~(vid_h : int) ~(drw_x : int) ~(drw_y : int) ~(drw_w : int) ~(drw_h : int)
      () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_port buf port;
    Xproto.encode_drawable buf drawable;
    Xproto.encode_gcontext buf gc;
    encode_int16 buf vid_x;
    encode_int16 buf vid_y;
    encode_uint16 buf vid_w;
    encode_uint16 buf vid_h;
    encode_int16 buf drw_x;
    encode_int16 buf drw_y;
    encode_uint16 buf drw_w;
    encode_uint16 buf drw_h;
    buf

  let get_still ~(port : port) ~(drawable : Xproto.drawable)
      ~(gc : Xproto.gcontext) ~(vid_x : int) ~(vid_y : int) ~(vid_w : int)
      ~(vid_h : int) ~(drw_x : int) ~(drw_y : int) ~(drw_w : int) ~(drw_h : int)
      () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_port buf port;
    Xproto.encode_drawable buf drawable;
    Xproto.encode_gcontext buf gc;
    encode_int16 buf vid_x;
    encode_int16 buf vid_y;
    encode_uint16 buf vid_w;
    encode_uint16 buf vid_h;
    encode_int16 buf drw_x;
    encode_int16 buf drw_y;
    encode_uint16 buf drw_w;
    encode_uint16 buf drw_h;
    buf

  let stop_video ~(port : port) ~(drawable : Xproto.drawable) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_port buf port;
    Xproto.encode_drawable buf drawable;
    buf

  let select_video_notify ~(drawable : Xproto.drawable) ~(onoff : bool) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    Xproto.encode_drawable buf drawable;
    encode_bool buf onoff;
    buf

  let select_port_notify ~(port : port) ~(onoff : bool) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_port buf port;
    encode_bool buf onoff;
    buf

  type query_best_size_reply = { actual_width : int; actual_height : int }

  let query_best_size ~(port : port) ~(vid_w : int) ~(vid_h : int)
      ~(drw_w : int) ~(drw_h : int) ~(motion : bool) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_port buf port;
    encode_uint16 buf vid_w;
    encode_uint16 buf vid_h;
    encode_uint16 buf drw_w;
    encode_uint16 buf drw_h;
    encode_bool buf motion;
    buf

  let set_port_attribute ~(port : port) ~(attribute : Xproto.atom)
      ~(value : int32) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_port buf port;
    Xproto.encode_atom buf attribute;
    encode_int32 buf value;
    buf

  type get_port_attribute_reply = { value : int32 }

  let get_port_attribute ~(port : port) ~(attribute : Xproto.atom) () : Buffer.t
      =
    let buf = Buffer.create 16 in
    encode_port buf port;
    Xproto.encode_atom buf attribute;
    buf

  type query_port_attributes_reply = {
    text_size : int32;
    attributes : attribute_info list;
  }

  let query_port_attributes ~(port : port) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_port buf port;
    buf

  type list_image_formats_reply = { format : image_format_info list }

  let list_image_formats ~(port : port) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_port buf port;
    buf

  type query_image_attributes_reply = {
    data_size : int32;
    width : int;
    height : int;
    pitches : int32 list;
    offsets : int32 list;
  }

  let query_image_attributes ~(port : port) ~(id : int32) ~(width : int)
      ~(height : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_port buf port;
    encode_int32 buf id;
    encode_uint16 buf width;
    encode_uint16 buf height;
    buf

  let put_image ~(port : port) ~(drawable : Xproto.drawable)
      ~(gc : Xproto.gcontext) ~(id : int32) ~(src_x : int) ~(src_y : int)
      ~(src_w : int) ~(src_h : int) ~(drw_x : int) ~(drw_y : int) ~(drw_w : int)
      ~(drw_h : int) ~(width : int) ~(height : int) ~(data : int list) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_port buf port;
    Xproto.encode_drawable buf drawable;
    Xproto.encode_gcontext buf gc;
    encode_int32 buf id;
    encode_int16 buf src_x;
    encode_int16 buf src_y;
    encode_uint16 buf src_w;
    encode_uint16 buf src_h;
    encode_int16 buf drw_x;
    encode_int16 buf drw_y;
    encode_uint16 buf drw_w;
    encode_uint16 buf drw_h;
    encode_uint16 buf width;
    encode_uint16 buf height;
    buf

  let shm_put_image ~(port : port) ~(drawable : Xproto.drawable)
      ~(gc : Xproto.gcontext) ~(shmseg : Shm.seg) ~(id : int32)
      ~(offset : int32) ~(src_x : int) ~(src_y : int) ~(src_w : int)
      ~(src_h : int) ~(drw_x : int) ~(drw_y : int) ~(drw_w : int) ~(drw_h : int)
      ~(width : int) ~(height : int) ~(send_event : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_port buf port;
    Xproto.encode_drawable buf drawable;
    Xproto.encode_gcontext buf gc;
    Shm.encode_seg buf shmseg;
    encode_int32 buf id;
    encode_int32 buf offset;
    encode_int16 buf src_x;
    encode_int16 buf src_y;
    encode_uint16 buf src_w;
    encode_uint16 buf src_h;
    encode_int16 buf drw_x;
    encode_int16 buf drw_y;
    encode_uint16 buf drw_w;
    encode_uint16 buf drw_h;
    encode_uint16 buf width;
    encode_uint16 buf height;
    encode_uint8 buf send_event;
    buf
end
[@@warning "-27"]

module Xvmc = struct
  type context = xid

  let decode_context = decode_xid

  let encode_context = encode_xid

  type surface = xid

  let decode_surface = decode_xid

  let encode_surface = encode_xid

  type subpicture = xid

  let decode_subpicture = decode_xid

  let encode_subpicture = encode_xid

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

  let encode_surface_info buf
      {
        id;
        chroma_format;
        pad0;
        max_width;
        max_height;
        subpicture_max_width;
        subpicture_max_height;
        mc_type;
        flags;
      } =
    encode_surface buf id;
    encode_uint16 buf chroma_format;
    encode_uint16 buf pad0;
    encode_uint16 buf max_width;
    encode_uint16 buf max_height;
    encode_uint16 buf subpicture_max_width;
    encode_uint16 buf subpicture_max_height;
    encode_int32 buf mc_type;
    encode_int32 buf flags;
    ignore buf

  type query_version_reply = { major : int32; minor : int32 }

  let query_version () : Buffer.t =
    let buf = Buffer.create 16 in
    buf

  type list_surface_types_reply = { surfaces : surface_info list }

  let list_surface_types ~(port_id : Xv.port) () : Buffer.t =
    let buf = Buffer.create 16 in
    Xv.encode_port buf port_id;
    buf

  type create_context_reply = {
    width_actual : int;
    height_actual : int;
    flags_return : int32;
    priv_data : int32 list;
  }

  let create_context ~(context_id : context) ~(port_id : Xv.port)
      ~(surface_id : surface) ~(width : int) ~(height : int) ~(flags : int32) ()
      : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context buf context_id;
    Xv.encode_port buf port_id;
    encode_surface buf surface_id;
    encode_uint16 buf width;
    encode_uint16 buf height;
    encode_int32 buf flags;
    buf

  let destroy_context ~(context_id : context) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_context buf context_id;
    buf

  type create_surface_reply = { priv_data : int32 list }

  let create_surface ~(surface_id : surface) ~(context_id : context) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    encode_surface buf surface_id;
    encode_context buf context_id;
    buf

  let destroy_surface ~(surface_id : surface) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_surface buf surface_id;
    buf

  type create_subpicture_reply = {
    width_actual : int;
    height_actual : int;
    num_palette_entries : int;
    entry_bytes : int;
    component_order : int list;
    priv_data : int32 list;
  }

  let create_subpicture ~(subpicture_id : subpicture) ~(context : context)
      ~(xvimage_id : int32) ~(width : int) ~(height : int) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_subpicture buf subpicture_id;
    encode_context buf context;
    encode_int32 buf xvimage_id;
    encode_uint16 buf width;
    encode_uint16 buf height;
    buf

  let destroy_subpicture ~(subpicture_id : subpicture) () : Buffer.t =
    let buf = Buffer.create 16 in
    encode_subpicture buf subpicture_id;
    buf

  type list_subpicture_types_reply = { types : Xv.image_format_info list }

  let list_subpicture_types ~(port_id : Xv.port) ~(surface_id : surface) () :
      Buffer.t =
    let buf = Buffer.create 16 in
    Xv.encode_port buf port_id;
    encode_surface buf surface_id;
    buf
end
[@@warning "-27"]