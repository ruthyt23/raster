open Core

let boundary_check img ~x ~y =
  let height = Image.height img in
  let width = Image.width img in
  x >= 0 && x < width && y >= 0 && y < height
;;

let sobel_x img ~x ~y =
  let kernel = [ -1; 0; 1; -2; 0; 2; -1; 0; 1 ] in
  let neighbors =
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
  let mapped_kernel =
    List.map2_exn neighbors kernel ~f:(fun (x, y) num ->
      if boundary_check img ~x ~y
      then Pixel.red (Image.get img ~x ~y) * num
      else 0)
  in
  List.fold mapped_kernel ~init:0 ~f:(fun acc num -> acc + num)
  |> Int.to_float
;;

let sobel_y img ~x ~y =
  let kernel = [ -1; -2; -1; 0; 0; 0; 1; 2; 1 ] in
  let neighbors =
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
  let mapped_kernel =
    List.map2_exn neighbors kernel ~f:(fun (x, y) num ->
      if boundary_check img ~x ~y
      then Pixel.red (Image.get img ~x ~y) * num
      else 0)
  in
  List.fold mapped_kernel ~init:0 ~f:(fun acc num -> acc + num)
  |> Int.to_float
;;

let sobel_convolution img ~x ~y ~threshold =
  let max = Image.max_val img in
  let max_float = Int.to_float max in
  let gradient =
    sqrt ((sobel_x img ~x ~y ** 2.0) +. (sobel_y img ~x ~y ** 2.0))
  in
  if Float.( >= ) (gradient /. max_float) threshold
  then max, max, max
  else Pixel.zero
;;

let transform img ~threshold =
  Image.mapi img ~f:(fun ~x ~y _ -> sobel_convolution img ~x ~y ~threshold)
;;

let command =
  Command.basic
    ~summary:"Detect edges in a photo"
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
        let image =
          Image.load_ppm ~filename
          |> Blur.transform ~radius:2
          |> transform ~threshold
        in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_edge.ppm")]
;;
