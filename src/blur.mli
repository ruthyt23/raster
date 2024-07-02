open Core

val transform : Image.t -> radius:int -> Image.t
val transform_helper : Image.t -> x:int -> y:int -> radius:int -> Pixel.t
val command : Command.t
