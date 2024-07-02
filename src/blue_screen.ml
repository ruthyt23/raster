open Core

(* You need to change the implementation of this function so that it replaces
   the "blue" pixels of the foreground image with pixels from the
   corresponding position in the background image instead of just ignoring
   the background image and returning the foreground image. *)
(*let transform ~foreground ~background = Image.mapi foreground ~f:(fun ~x ~y
  (r, g, b) -> if b > r + g then Image.get background ~x ~y else r, g, b)
  ;; *)

let boundary_check ~x ~y ~height ~width =
  let og_neighbors =
    [ x - 1, y - 1
    ; x - 1, y
    ; x - 1, y + 1
    ; x, y - 1
    ; x, y
    ; x, y + 1
    ; x + 1, y - 1
    ; x + 1, y
    ; x + 1, y + 1
    ]
  in
  List.filter og_neighbors ~f:(fun (x, y) ->
    x >= 0 && x < width && y >= 0 && y < height)
;;

let blueness_check ~r ~g ~b = b >= r + g

let coor_to_rgb image ~x ~y =
  ( Pixel.red (Image.get image ~x ~y)
  , Pixel.green (Image.get image ~x ~y)
  , Pixel.blue (Image.get image ~x ~y) )
;;

let improved_transform ~foreground ~background =
  Image.mapi foreground ~f:(fun ~x ~y (r, g, b) ->
    let neighbors =
      boundary_check
        ~x
        ~y
        ~height:(Image.height foreground)
        ~width:(Image.width foreground)
    in
    if blueness_check ~r ~g ~b
       || List.length
            (List.filter neighbors ~f:(fun (x, y) ->
               let r, g, b = coor_to_rgb foreground ~x ~y in
               blueness_check ~r ~g ~b))
          > Float.to_int (Int.to_float (List.length neighbors) *. 0.5)
    then Image.get background ~x ~y
    else r, g, b)
;;

(* let more_improved_transform ~foreground ~background = Image.mapi
   foreground ~f:(fun ~x ~y (r, g, b) -> let avg_r, avg_g, avg_b =
   Blur.transform_helper foreground ~x ~y ~radius:3 in if b < r + g || avg_b
   < avg_r + avg_g then r, g, b else if blueness_check ~r ~g ~b ||
   blueness_check ~r:avg_r ~g:avg_g ~b:avg_b then Image.get background ~x ~y
   else r, g, b) ;; *)

let more_improved_transform ~foreground ~background =
  Image.mapi foreground ~f:(fun ~x ~y (r, g, b) ->
    let avg_r, avg_g, avg_b =
      Blur.transform_helper foreground ~x ~y ~radius:5
    in
    if blueness_check ~r ~g ~b
    then Image.get background ~x ~y
    else if avg_b < avg_r + avg_g
    then r, g, b
    else Image.get background ~x ~y)
;;

let%expect_test "transform" =
  let oz = Image.load_ppm ~filename:"../images/oz_bluescreen.ppm" in
  let ref_oz_meadow =
    Image.load_ppm ~filename:"../images/reference-oz_bluescreen_vfx.ppm"
  in
  let oz_meadow =
    improved_transform ~foreground:oz ~background:ref_oz_meadow
  in
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
        let image' = more_improved_transform ~foreground ~background in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn foreground_file ~suffix:".ppm"
             ^ "_vfx.ppm")]
;;
