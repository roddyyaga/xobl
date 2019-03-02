open Xobl_bindings.X11_base

let get_byte_inverse_of_put_int8 =
  QCheck.(Test.make (int_range 0 0xFF) (fun n ->
    let buf = Buffer.create 2 in
    Put.int8 buf n;
    Get.byte (Buffer.contents buf) 0 = n
  ))

let get_int8_inverse_of_put_int8 =
  QCheck.(Test.make (int_range (-0x80) 0x7F) (fun n ->
    let buf = Buffer.create 2 in
    Put.int8 buf n;
    Get.int8 (Buffer.contents buf) 0 = n
  ))

let get_uint16_inverse_of_put_int16 =
  QCheck.(Test.make (int_range 0 0xFFFF) (fun n ->
    let buf = Buffer.create 2 in
    Put.int16 buf n;
    Get.uint16 (Buffer.contents buf) 0 = n
  ))

let get_int16_inverse_of_put_int16 =
  QCheck.(Test.make (int_range (-0x8000) 0x7FFF) (fun n ->
    let buf = Buffer.create 2 in
    Put.int16 buf n;
    Get.int16 (Buffer.contents buf) 0 = n
  ))

let get_int32_inverse_of_put_int32 =
  QCheck.(Test.make int32 (fun n ->
    let buf = Buffer.create 4 in
    Put.int32 buf n;
    Get.int32 (Buffer.contents buf) 0 = n
  ))

let _ =
  QCheck_base_runner.run_tests_main
    [ get_byte_inverse_of_put_int8
    ; get_int8_inverse_of_put_int8
    ; get_uint16_inverse_of_put_int16
    ; get_int16_inverse_of_put_int16
    ]
