library(hexSticker)
sticker("man/figures/logo-raw.png", package = "pdindicatoR",
        p_size = 22, p_color = "#48A529",
        p_x = 1.02, p_y = 1.42,
        s_x = 1, s_y = 0.8, s_width = 0.9, s_height = 0.9,
        h_fill = "black", h_color = "#48A529",
        filename = "man/figures/hexa-sticker.png")

usethis::use_logo("man/figures/hexa-sticker.png")
