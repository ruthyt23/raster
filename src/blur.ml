open Core

(* You need to modify this function to blur the input image based on the
   provided radius instead of ignoring it. *)

let transform_helper image ~x ~y ~radius =
  let width = Image.width image in
  let height = Image.height image in
  let x_start = max 0 (x - radius) in
  let x_end = min (width - 1) (x + radius) in
  (*should just be width according to slice's doc..?*)
  let y_start = max 0 (y - radius) in
  let y_end = min (height - 1) (y + radius) in
  let avg = Image.slice image ~x_start ~x_end ~y_start ~y_end in
  Image.mean_pixel avg
;;

let transform image ~radius =
  Image.mapi image ~f:(fun ~x ~y _ -> transform_helper image ~x ~y ~radius)
;;

let%expect_test "transform" =
  let beach = Image.load_ppm ~filename:"../images/beach_portrait.ppm" in
  let ref_beach_blur =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_blur.ppm"
  in
  let beach_blur = transform beach ~radius:3 in
  let pixels_wrong =
    Image.foldi beach_blur ~init:0 ~f:(fun ~x ~y acc _ ->
      if Pixel.equal
           (Image.get beach_blur ~x ~y)
           (Image.get ref_beach_blur ~x ~y)
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
    ~summary:"Blur an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and radius =
        flag
          "radius"
          (required Command.Param.int)
          ~doc:"N the radius to use when blurring (higher = more blurred)"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~radius in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_blur.ppm")]
;;
