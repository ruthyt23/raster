open Core

(* You need to change the implementation of this function so that it replaces
   the "blue" pixels of the foreground image with pixels from the
   corresponding position in the background image instead of just ignoring
   the background image and returning the foreground image. *)
let transform ~foreground ~background =
  Image.mapi foreground ~f:(fun ~x ~y (r, g, b) ->
    if b > r + g then Image.get background ~x ~y else r, g, b)
;;

let%expect_test "transform" =
  let oz = Image.load_ppm ~filename:"../images/oz_bluescreen.ppm" in
  let ref_oz_meadow =
    Image.load_ppm ~filename:"../images/reference-oz_bluescreen_vfx.ppm"
  in
  let oz_meadow = transform ~foreground:oz ~background:ref_oz_meadow in
  let pixels_wrong =
    Image.foldi oz_meadow ~init:0 ~f:(fun ~x ~y acc _ ->
      if Pixel.equal
           (Image.get oz_meadow ~x ~y)
           (Image.get ref_oz_meadow ~x ~y)
      then acc
      else acc + 1)
  in
  (match pixels_wrong with
   | 0 -> ()
   | _ -> printf "Test failed: # of pixels that differ = %d" pixels_wrong);
  [%expect ""]
;;

let command =
  Command.basic
    ~summary:
      "Replace the 'blue' pixels of an image with those from another image"
    [%map_open.Command
      let foreground_file =
        flag
          "foreground"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the foreground PPM image file"
      and background_file =
        flag
          "background"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the background PPM image file"
      in
      fun () ->
        let foreground = Image.load_ppm ~filename:foreground_file in
        let background = Image.load_ppm ~filename:background_file in
        let image' = transform ~foreground ~background in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn foreground_file ~suffix:".ppm"
             ^ "_vfx.ppm")]
;;
