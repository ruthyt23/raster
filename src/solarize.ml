open Core

let solarize image ~threshold =
  Image.map image ~f:(fun (r, g, b) ->
    let max = Image.max_val image in
    let max_float = Int.to_float max in
    let new_r =
      if Float.( >= ) (Int.to_float r /. max_float) threshold
      then max - r
      else r
    in
    let new_g =
      if Float.( >= ) (Int.to_float g /. max_float) threshold
      then max - g
      else g
    in
    let new_b =
      if Float.( >= ) (Int.to_float b /. max_float) threshold
      then max - b
      else b
    in
    new_r, new_g, new_b)
;;

let%expect_test "solarize" =
  let meadow = Image.load_ppm ~filename:"../images/meadow.ppm" in
  let ref_solarized_meadow =
    Image.load_ppm ~filename:"../images/reference-meadow_solarize.ppm"
  in
  let solarized_meadow = solarize meadow ~threshold:0.40 in
  let pixels_wrong =
    Image.foldi solarized_meadow ~init:0 ~f:(fun ~x ~y acc _ ->
      if Pixel.equal
           (Image.get solarized_meadow ~x ~y)
           (Image.get ref_solarized_meadow ~x ~y)
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
    ~summary:"Pseudo-solarize a photo"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and threshold =
        flag
          "threshold"
          (required Command.Param.float)
          ~doc:"the limit to use when determining inversion"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> solarize ~threshold in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm"
             ^ "_solarized.ppm")]
;;
