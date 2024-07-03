open Core

(*let transform_helper image ~moves ~region_starts ~height ~width = image;*)

let transform image ~moves ~height ~width =
  let x_size = Image.width image / width in
  let y_size = Image.height image / height in
  let x_coors = List.init x_size ~f:(fun x -> x * width) in
  let y_coors = List.init y_size ~f:(fun y -> y * height) in
  let region_starts =
    List.concat_map x_coors ~f:(fun x_coor ->
      List.map y_coors ~f:(fun y_coor -> x_coor, y_coor))
  in
  (*transform_helper image ~moves ~region_starts ~height ~width;*)
  image
;;

let command =
  Command.basic
    ~summary:"Mosaic operation on a photo"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and moves =
        flag
          "moves"
          (required Command.Param.int)
          ~doc:"the number of times to repeat mosaic operation"
      and height =
        flag
          "height"
          (required Command.Param.int)
          ~doc:"the height for each region"
      and width =
        flag
          "width"
          (required Command.Param.int)
          ~doc:"the width for each region"
      in
      fun () ->
        let image =
          Image.load_ppm ~filename |> transform ~moves ~height ~width
        in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_mosaic.ppm")]
;;
