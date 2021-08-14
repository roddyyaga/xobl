#!/bin/bash
ocamlformat -i generated/xcb.ml
sed -i 's/decode_device_time_coord buf/decode_device_time_coord num_axes buf/g' generated/xcb.ml
sed -i 's/decode_property_format_variant buf/decode_property_format_variant num_items buf/g' generated/xcb.ml
