open Core

let error_multiply ~error ~float =
  let r, g, b = error in
  ( Float.to_int (Int.to_float r *. float)
  , Float.to_int (Int.to_float g *. float)
  , Float.to_int (Int.to_float b *. float) )
;;

let pixel_error_adjustment image ~x ~y ~height ~width ~(error : Pixel.t) =
  if not (x + 1 > width - 1)
  then
    Image.set
      image
      ~x:(x + 1)
      ~y
      (Pixel.( + )
         (Image.get image ~x:(x + 1) ~y)
         (error_multiply ~error ~float:(7.0 /. 16.0)));
  if not (y + 1 > height - 1)
  then
    Image.set
      image
      ~x
      ~y:(y + 1)
      (Pixel.( + )
         (Image.get image ~x ~y:(y + 1))
         (error_multiply ~error ~float:(5.0 /. 16.0)));
  if not (x + 1 > width - 1 || y + 1 > height - 1)
  then
    Image.set
      image
      ~x:(x + 1)
      ~y:(y + 1)
      (Pixel.( + )
         (Image.get image ~x:(x + 1) ~y:(y + 1))
         (error_multiply ~error ~float:(1.0 /. 16.0)));
  if not (x - 1 < 0 || y + 1 > height - 1)
  then
    Image.set
      image
      ~x:(x - 1)
      ~y:(y + 1)
      (Pixel.( + )
         (Image.get image ~x:(x - 1) ~y:(y + 1))
         (error_multiply ~error ~float:(3.0 /. 16.0)));
  1
;;

let dither_color ~color ~intervals =
  let new_val = ref 0 in
  List.iter intervals ~f:(fun x ->
    if abs (x - color) < abs (!new_val - color) then new_val.contents <- x);
  !new_val
;;

(* This should look familiar by now! *)
let transform image ~n =
  let height = Image.height image in
  let width = Image.width image in
  let max = Int.to_float (Image.max_val image) in
  let diff = Float.to_int (max /. Int.to_float (n - 1)) in
  let temp = List.init n ~f:(fun x -> x * diff) in
  let intervals = List.append temp [ Image.max_val image ] in
  let _ =
    Image.foldi ~init:0 image ~f:(fun ~x ~y _ _ ->
      let old_r, old_g, old_b =
        ( Pixel.red (Image.get image ~x ~y)
        , Pixel.green (Image.get image ~x ~y)
        , Pixel.blue (Image.get image ~x ~y) )
      in
      let new_r, new_g, new_b =
        ( dither_color ~color:old_r ~intervals
        , dither_color ~color:old_g ~intervals
        , dither_color ~color:old_b ~intervals )
      in
      Image.set image ~x ~y (new_r, new_g, new_b);
      pixel_error_adjustment
        image
        ~x
        ~y
        ~height
        ~width
        ~error:(old_r - new_r, old_g - new_g, old_b - new_b))
  in
  image
;;

let%expect_test "transform" =
  let beach = Image.load_ppm ~filename:"../images/beach_portrait.ppm" in
  let color_dither_beach = transform beach ~n:2 in
  let ref_color_dither_beach =
    Image.load_ppm
      ~filename:"../images/reference-beach_portrait_dither_color.ppm"
  in
  let pixels_wrong =
    Image.foldi color_dither_beach ~init:0 ~f:(fun ~x ~y acc _ ->
      if Pixel.equal
           (Image.get color_dither_beach ~x ~y)
           (Image.get ref_color_dither_beach ~x ~y)
      then acc
      else acc + 1)
  in
  (match pixels_wrong with
   | 0 -> ()
   | _ -> printf "Test failed: # of pixels that\n   differ = %d" pixels_wrong);
  [%expect ""]
;;

let command =
  Command.basic
    ~summary:"Apply color dithering to a photo"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and n =
        flag
          "n"
          (required Command.Param.int)
          ~doc:"the number of colors per channel"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform ~n in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm"
             ^ "_dither_color.ppm")]
;;
