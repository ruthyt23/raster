open! Core

let command =
  Command.group
    ~summary:"A tool to perform various image manipulations"
    [ "grayscale", Grayscale.command
    ; "bluescreen", Blue_screen.command
    ; "blur", Blur.command
    ; "dither", Dither.command
    ; "solarize", Solarize.command
    ; "color-dither", Color_dither.command
    ; "edge-detect", Edge_detect.command
    ; "mosaic", Mosaic.command
    ]
;;
