open Core

(* You need to change the implementation of this function so that it does
   something to the image instead of just leaving it untouched. *)
let transform image =
  Image.map image ~f:(fun (r, g, b) ->
    let avg = (r + g + b) / 3 in
    avg, avg, avg)
;;

let%expect_test "transform" =
  let beach = Image.load_ppm ~filename:"../images/beach_portrait.ppm" in
  let gray_beach = transform beach in
  let ref_gray_beach =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_gray.ppm"
  in
  let pixels_wrong =
    Image.foldi gray_beach ~init:0 ~f:(fun ~x ~y acc _ ->
      if Pixel.equal
           (Image.get gray_beach ~x ~y)
           (Image.get ref_gray_beach ~x ~y)
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
    ~summary:"Convert an image to grayscale"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_gray.ppm")]
;;
