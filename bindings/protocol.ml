let pad n = (if n = 0 then 0 else ((n - 1) lsr 2) + 1) * 4

let little_endian = 0x6C

let protocol_version = (11, 0)

let make_handshake xauth_name xauth_data =
  let xauth_name_len = String.length xauth_name in
  let xauth_data_len = String.length xauth_data in
  let len = 12 + pad xauth_name_len + pad xauth_data_len in
  let buf = Bytes.create len in
  Bytes.set_uint8 buf 0 little_endian;
  (* 1 unused byte *)
  Bytes.set_uint16_le buf 2 (fst protocol_version);
  Bytes.set_uint16_le buf 4 (snd protocol_version);
  Bytes.set_uint16_le buf 6 xauth_name_len;
  Bytes.set_uint16_le buf 8 xauth_data_len;
  (* 2 unused bytes *)
  if xauth_name <> "" then Bytes.blit_string xauth_name 0 buf 12 xauth_name_len;
  if xauth_data <> "" then
    Bytes.blit_string xauth_data 0 buf (12 + pad xauth_name_len) xauth_data_len;
  (buf, len)

type xid = int32 [@@deriving show]

let size_of_xid _ = 4

type backing_stores =
  | Backing_stores_never
  | Backing_stores_when_mapped
  | Backing_stores_always
[@@deriving show]

let backing_stores_of_int = function
  | 0 ->
      Backing_stores_never
  | 1 ->
      Backing_stores_when_mapped
  | 2 ->
      Backing_stores_always
  | n ->
      invalid_arg (string_of_int n)

let int_of_backing_stores = function
  | Backing_stores_never ->
      0
  | Backing_stores_when_mapped ->
      1
  | Backing_stores_always ->
      2

type window_class = Copy_from_parent | Input_output | Input_only

let int_of_window_class = function
  | Copy_from_parent ->
      0
  | Input_output ->
      1
  | Input_only ->
      2

type back_pixmap = Back_pixmap_none | Parent_relative

let int_of_back_pixmap = function Back_pixmap_none -> 0 | Parent_relative -> 1

type gravity =
  | Bit_forget
  | Win_unmap
  | North_west
  | North
  | North_east
  | West
  | Center
  | East
  | South_west
  | South
  | South_east
  | Static

let int_of_gravity = function
  | Bit_forget ->
      0
  | Win_unmap ->
      1
  | North_west ->
      2
  | North ->
      3
  | North_east ->
      4
  | West ->
      5
  | Center ->
      6
  | East ->
      7
  | South_west ->
      8
  | South ->
      9
  | South_east ->
      10
  | Static ->
      11

type pixmap = xid

type ('a, 'b) alt = Value of 'a | Enum of 'b

type ('flags, 'v) mask = Flags of 'flags list | Val of 'v

type pixmap_enum = Pixmap_none

let int_of_pixmap_enum Pixmap_none = 0

type event_mask_flag =
  | Key_press
  | Key_release
  | Button_press
  | Button_release
  | Enter_window
  | Leave_window
  | Pointer_motion
  | Pointer_motion_hint
  | Button_1_motion
  | Button_2_motion
  | Button_3_motion
  | Button_4_motion
  | Button_5_motion
  | Button_motion
  | Keymap_state
  | Exposure
  | Visibility_change
  | Structure_notify
  | Resize_redirect
  | Substructure_notify
  | Substructure_redirect
  | Focus_change
  | Property_change
  | Color_map_change
  | Owner_grab_button

let int_of_event_mask_flag = function
  | Key_press ->
      0
  | Key_release ->
      1
  | Button_press ->
      2
  | Button_release ->
      3
  | Enter_window ->
      4
  | Leave_window ->
      5
  | Pointer_motion ->
      6
  | Pointer_motion_hint ->
      7
  | Button_1_motion ->
      8
  | Button_2_motion ->
      9
  | Button_3_motion ->
      10
  | Button_4_motion ->
      11
  | Button_5_motion ->
      12
  | Button_motion ->
      13
  | Keymap_state ->
      14
  | Exposure ->
      15
  | Visibility_change ->
      16
  | Structure_notify ->
      17
  | Resize_redirect ->
      18
  | Substructure_notify ->
      19
  | Substructure_redirect ->
      20
  | Focus_change ->
      21
  | Property_change ->
      22
  | Color_map_change ->
      23
  | Owner_grab_button ->
      24

type event_mask_enum = No_event

let int_of_event_mask_enum No_event = 0

type event_mask = (event_mask_flag, event_mask_enum) mask

type colormap = xid

type colormap_enum = Colormap_none

let int_of_colormap_enum Colormap_none = 0

type cursor = xid

type cursor_enum = Cursor_none

let int_of_cursor_enum Cursor_none = 0

type cw =
  | Back_pixmap of (pixmap, back_pixmap) alt
  | Back_pixel of int32
  | Border_pixmap of (pixmap, pixmap_enum) alt
  | Border_pixel of int32
  | Bit_gravity of gravity
  | Win_gravity of gravity
  | Backing_store of backing_stores
  | Backing_planes of int32
  | Backing_pixel of int32
  | Override_redirect of bool
  | Save_under of bool
  | Event_mask of event_mask
  | Dont_propagate of event_mask
  | Colormap of (colormap, colormap_enum) alt
  | Cursor of (cursor, cursor_enum) alt

let int_of_cw = function
  | Back_pixmap _ ->
      0
  | Back_pixel _ ->
      1
  | Border_pixmap _ ->
      2
  | Border_pixel _ ->
      3
  | Bit_gravity _ ->
      4
  | Win_gravity _ ->
      5
  | Backing_store _ ->
      6
  | Backing_planes _ ->
      7
  | Backing_pixel _ ->
      8
  | Override_redirect _ ->
      9
  | Save_under _ ->
      10
  | Event_mask _ ->
      11
  | Dont_propagate _ ->
      12
  | Colormap _ ->
      13
  | Cursor _ ->
      14

let size_of_cw x = size_of_xid x

let flags_to_int32 to_int ls =
  let fold mask v = Int32.logand mask (Int32.of_int (to_int v)) in
  List.fold_left fold 0l ls

let write_alt_int32 buf ~at to_int32 = function
  | Value v ->
      Bytes.set_int32_le buf at v
  | Enum e ->
      Bytes.set_int32_le buf at (to_int32 e)

let write_bool32 buf ~at b = Bytes.set_int32_le buf at (if b then 0l else 1l)

let write_mask buf ~at ~int32_of_val ~int_of_flag = function
  | Flags f ->
      Bytes.set_int32_le buf at (flags_to_int32 int_of_flag f)
  | Val v ->
      Bytes.set_int32_le buf at (int32_of_val v)

let write_cw buf ~at = function
  | Back_pixmap pm ->
      write_alt_int32 buf ~at (fun x -> Int32.of_int (int_of_back_pixmap x)) pm
  | Back_pixel bp ->
      Bytes.set_int32_le buf at bp
  | Border_pixmap bp ->
      write_alt_int32 buf ~at (fun x -> Int32.of_int (int_of_pixmap_enum x)) bp
  | Border_pixel bp ->
      Bytes.set_int32_le buf at bp
  | Bit_gravity g ->
      Bytes.set_int32_le buf at (Int32.of_int (int_of_gravity g))
  | Win_gravity g ->
      Bytes.set_int32_le buf at (Int32.of_int (int_of_gravity g))
  | Backing_store bs ->
      Bytes.set_int32_le buf at (Int32.of_int (int_of_backing_stores bs))
  | Backing_planes bp ->
      Bytes.set_int32_le buf at bp
  | Backing_pixel bp ->
      Bytes.set_int32_le buf at bp
  | Override_redirect o ->
      write_bool32 buf ~at o
  | Save_under s ->
      write_bool32 buf ~at s
  | Event_mask em ->
      write_mask buf ~at
        ~int32_of_val:(fun x -> Int32.of_int (int_of_event_mask_enum x))
        ~int_of_flag:int_of_event_mask_flag em
  | Dont_propagate em ->
      write_mask buf ~at
        ~int32_of_val:(fun x -> Int32.of_int (int_of_event_mask_enum x))
        ~int_of_flag:int_of_event_mask_flag em
  | Colormap cm ->
      write_alt_int32 buf ~at
        (fun x -> Int32.of_int (int_of_colormap_enum x))
        cm
  | Cursor c ->
      write_alt_int32 buf ~at (fun x -> Int32.of_int (int_of_cursor_enum x)) c

let write_cw buf ~at cws =
  let cws =
    List.sort_uniq
      (fun v1 v2 ->
        let p1 = int_of_cw v1 in
        let p2 = int_of_cw v2 in
        if p1 = p2 then 0 else if p1 > p2 then 1 else -1)
      cws
  in
  List.fold_left (fun at cw -> write_cw buf ~at cw; at + 4) at cws

(*
let create_window_request
  ~depth
  ~wid
  ~parent
  ~x ~y
  ~width ~height
  ~border_width
  ~class_
  ~visual
  ~values
=
  let len = 36 in
  let buf = Bytes.create len in
  Bytes.set_int8 buf 0 1;
  Bytes.set_uint16_le buf 2 (len / 4);
  Bytes.set_int32_le buf 4 xid;
  Bytes.set_int32_le buf 8 parent;
  Bytes.set_uint16_le buf 12 x;
  Bytes.set_uint16_le buf 14 y;
  Bytes.set_uint16_le buf 16 width;
  Bytes.set_uint16_le buf 18 height;
  Bytes.set_uint16_le buf 20 border_width;
  Bytes.set_uint16_le buf 22 (int_of_window_class class_);
  Bytes.set_int32_le buf 24 visual;
  Bytes.set_int32_le buf 28 (flags_to_int32 int_of_cw values);
  *)

let read_handshake_refused ?(at = 0) buf =
  let reason_length = Bytes.get_int8 buf (at + 1) in
  Bytes.sub_string buf (at + 8) reason_length

type 'a range = { min : 'a; max : 'a } [@@deriving show]

type endianness = Little_endian | Big_endian [@@deriving show]

let endianness_of_int = function
  | 0 ->
      Little_endian
  | 1 ->
      Big_endian
  | n ->
      invalid_arg (string_of_int n)

type bitmap_format_bit_order = Least_significant | Most_significant
[@@deriving show]

let bitmap_format_bit_order_of_int = function
  | 0 ->
      Least_significant
  | 1 ->
      Most_significant
  | n ->
      invalid_arg (string_of_int n)

type pixmap_format =
  { pixmap_depth : int; bits_per_pixel : int; scanline_pad : int }
[@@deriving show]

let read_pixmap_format ~at buf =
  let pixmap_depth = Bytes.get_uint8 buf at in
  let bits_per_pixel = Bytes.get_uint8 buf (at + 1) in
  let scanline_pad = Bytes.get_uint8 buf (at + 2) in
  ({ pixmap_depth; bits_per_pixel; scanline_pad }, at + 8)

let rec read_list ~at ~count read_f buf acc =
  if count = 0 then (List.rev acc, at)
  else
    let v, at = read_f ~at buf in
    let count = count - 1 in
    read_list ~at ~count read_f buf (v :: acc)

let read_list ~at ~count read_f buf = read_list ~at ~count read_f buf []

let bool_of_int = function
  | 0 ->
      false
  | 1 ->
      true
  | n ->
      invalid_arg (string_of_int n)

type visual_class =
  | Visual_static_gray
  | Visual_gray_scale
  | Visual_static_color
  | Visual_pseudo_color
  | Visual_true_color
  | Visual_direct_color
[@@deriving show]

let visual_class_of_int = function
  | 0 ->
      Visual_static_gray
  | 1 ->
      Visual_gray_scale
  | 2 ->
      Visual_static_color
  | 3 ->
      Visual_pseudo_color
  | 4 ->
      Visual_true_color
  | 5 ->
      Visual_direct_color
  | n ->
      invalid_arg (string_of_int n)

type visual_type =
  { visual_id : xid
  ; visual_class : visual_class
  ; bits_per_rgb_value : int
  ; colormap_entries : int
  ; red_mask : int32
  ; green_mask : int32
  ; blue_mask : int32
  }
[@@deriving show]

let read_visual_type ~at buf =
  let visual_id = Bytes.get_int32_le buf at in
  let visual_class = Bytes.get_uint8 buf (at + 4) |> visual_class_of_int in
  let bits_per_rgb_value = Bytes.get_uint8 buf (at + 5) in
  let colormap_entries = Bytes.get_uint16_le buf (at + 6) in
  let red_mask = Bytes.get_int32_le buf (at + 8) in
  let green_mask = Bytes.get_int32_le buf (at + 12) in
  let blue_mask = Bytes.get_int32_le buf (at + 16) in
  ( { visual_id
    ; visual_class
    ; bits_per_rgb_value
    ; colormap_entries
    ; red_mask
    ; green_mask
    ; blue_mask
    }
  , at + 24 )

type depth = { depth : int; visual_types : visual_type list } [@@deriving show]

let read_depth ~at buf =
  let depth = Bytes.get_uint8 buf at in
  let visuals_count = Bytes.get_uint16_le buf (at + 2) in
  let at = at + 8 in
  let visual_types, at =
    read_list ~at read_visual_type ~count:visuals_count buf
  in
  ({ depth; visual_types }, at)

type screen =
  { root : xid
  ; default_colormap : xid
  ; white_pixel : int32
  ; black_pixel : int32
  ; current_input_masks : int32
  ; width_in_pixels : int
  ; height_in_pixels : int
  ; width_in_millimeters : int
  ; height_in_millimeters : int
  ; installed_maps : int range
  ; root_visual : xid
  ; backing_stores : backing_stores
  ; save_unders : bool
  ; root_depth : int
  ; depths : depth list
  }
[@@deriving show]

let read_screen ~at buf =
  let root = Bytes.get_int32_le buf at in
  let default_colormap = Bytes.get_int32_le buf (at + 4) in
  let white_pixel = Bytes.get_int32_le buf (at + 8) in
  let black_pixel = Bytes.get_int32_le buf (at + 12) in
  let current_input_masks = Bytes.get_int32_le buf (at + 16) in
  let width_in_pixels = Bytes.get_uint16_le buf (at + 20) in
  let height_in_pixels = Bytes.get_uint16_le buf (at + 22) in
  let width_in_millimeters = Bytes.get_uint16_le buf (at + 24) in
  let height_in_millimeters = Bytes.get_uint16_le buf (at + 26) in
  let min_installed_maps = Bytes.get_uint16_le buf (at + 28) in
  let max_installed_maps = Bytes.get_uint16_le buf (at + 30) in
  let root_visual = Bytes.get_int32_le buf (at + 32) in
  let backing_stores = Bytes.get_int8 buf (at + 36) |> backing_stores_of_int in
  let save_unders = Bytes.get_int8 buf (at + 37) |> bool_of_int in
  let root_depth = Bytes.get_int8 buf (at + 38) in
  let depths_count = Bytes.get_int8 buf (at + 39) in
  let at = at + 40 in
  let depths, at = read_list ~at read_depth ~count:depths_count buf in
  ( { root
    ; default_colormap
    ; white_pixel
    ; black_pixel
    ; current_input_masks
    ; width_in_pixels
    ; height_in_pixels
    ; width_in_millimeters
    ; height_in_millimeters
    ; installed_maps = { min = min_installed_maps; max = max_installed_maps }
    ; root_visual
    ; backing_stores
    ; save_unders
    ; root_depth
    ; depths
    }
  , at )

type handshake_resp =
  { protocol_version : int * int
  ; release_number : int32
  ; resource_id_base : int32
  ; resource_id_mask : int32
  ; motion_buffer_size : int32
  ; maximum_request_length : int
  ; image_byte_order : endianness
  ; bitmap_format_bit_order : bitmap_format_bit_order
  ; bitmap_format_scanline_unit : int
  ; bitmap_format_scanline_pad : int
  ; keycode_range : int range
  ; vendor : string
  ; pixmap_formats : pixmap_format list
  ; screens : screen list
  }
[@@deriving show]

let read_handshake_accepted ?(at = 0) buf =
  let protocol_major_version = Bytes.get_uint16_le buf (at + 2) in
  let protocol_minor_version = Bytes.get_uint16_le buf (at + 4) in
  let release_number = Bytes.get_int32_le buf (at + 8) in
  let resource_id_base = Bytes.get_int32_le buf (at + 12) in
  let resource_id_mask = Bytes.get_int32_le buf (at + 16) in
  let motion_buffer_size = Bytes.get_int32_le buf (at + 20) in
  let vendor_length = Bytes.get_uint16_le buf (at + 24) in
  let maximum_request_length = Bytes.get_uint16_le buf (at + 26) in
  let screen_count = Bytes.get_uint8 buf (at + 28) in
  let pixmap_format_count = Bytes.get_uint8 buf (at + 29) in
  let image_byte_order = Bytes.get_uint8 buf (at + 30) |> endianness_of_int in
  let bitmap_format_bit_order =
    Bytes.get_uint8 buf (at + 31) |> bitmap_format_bit_order_of_int
  in
  let bitmap_format_scanline_unit = Bytes.get_uint8 buf (at + 32) in
  let bitmap_format_scanline_pad = Bytes.get_uint8 buf (at + 33) in
  let min_keycode = Bytes.get_uint8 buf (at + 34) in
  let max_keycode = Bytes.get_uint8 buf (at + 35) in
  let vendor = Bytes.sub_string buf (at + 40) vendor_length in
  let at = at + 40 + pad vendor_length in
  let formats, at =
    read_list ~at read_pixmap_format ~count:pixmap_format_count buf
  in
  let screens, at = read_list ~at read_screen ~count:screen_count buf in
  ( { protocol_version = (protocol_major_version, protocol_minor_version)
    ; release_number
    ; resource_id_base
    ; resource_id_mask
    ; motion_buffer_size
    ; maximum_request_length
    ; image_byte_order
    ; bitmap_format_bit_order
    ; bitmap_format_scanline_unit
    ; bitmap_format_scanline_pad
    ; keycode_range = { min = min_keycode; max = max_keycode }
    ; vendor
    ; pixmap_formats = formats
    ; screens
    }
  , at )

let read_handshake buf =
  match Bytes.get buf 0 with
  | '\000' ->
      let reason = read_handshake_refused buf in
      Printf.kprintf failwith "Connection refused: %s" reason
  | '\002' ->
      failwith "Authentication required"
  | '\001' ->
      read_handshake_accepted buf
  | code ->
      Printf.kprintf invalid_arg "invalid response: %#x" (Char.code code)

let read_handshake_response sock =
  let buf = Bytes.create 8 in
  let%lwt _ = Lwt_unix.read sock buf 0 8 in
  let additional_data_length = Bytes.get_uint16_le buf 6 in
  let whole_buf = Bytes.create (8 + (additional_data_length * 4)) in
  Bytes.blit buf 0 whole_buf 0 8;
  let%lwt _ = Lwt_unix.read sock whole_buf 8 (additional_data_length * 4) in
  Lwt.return whole_buf

let read_response sock =
  let buf = Bytes.create 32 in
  let%lwt _ = Lwt_unix.read sock buf 0 32 in
  let additional_data_length = Bytes.get_int32_le buf 4 |> Int32.to_int in
  if additional_data_length < 1 then Lwt.return buf
  else
    let whole_buf = Bytes.create (32 + (additional_data_length * 4)) in
    Bytes.blit buf 0 whole_buf 0 8;
    let%lwt _ = Lwt_unix.read sock whole_buf 32 (additional_data_length * 4) in
    Lwt.return whole_buf
