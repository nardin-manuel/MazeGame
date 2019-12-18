(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Menu.fs: menu
* (C) 2019 Manuel Nardin @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Menu

open System
open Gfx
open Engine


type menuState = {
    selector : sprite
}

type menu(width, height) =    
    member this.draw() =
        let menuImg = new image(width, height)
        menuImg.draw_text("Option 1",13,10,Color.Red)
        menuImg.draw_text("Option 2",13,14, Color.Red)
        menuImg.draw_text("Option 3",13,18, Color.Red)
        menuImg.draw_text("Option 3",13,22, Color.Red)
        menuImg

let main()=
    let w = 31
    let h = 31
    let menuEngine = new engine (w,h)
    menuEngine.show_fps <- false
    menuEngine.create_and_register_sprite(menu(w,h).draw(),0,0,1) |>ignore
    let menuSelectorSpr = menuEngine.create_and_register_sprite(image.rectangle(1, 1, pixel.create(char('*'), Color.Cyan)),10,10,1)

    let startGame()=
        Maze.main()


    let menuUpdate (key : ConsoleKeyInfo) (screen : wronly_raster) (st : menuState) =
        let nextMove=
            match key.KeyChar with
            |'w' -> -4.
            |'s'-> 4.
            |_ -> 0.

        match key.KeyChar with
            |' ' -> match (int(st.selector.y)-10)/4 with
                    |0 -> startGame()
                    |1 -> ()
                    |2 -> ()
                    |3 -> ()
                    |_ -> ()

            |_ -> ()


        let nx, ny = st.selector.dryMove(0., nextMove) 

        if ny >= 10. && ny <= 22. then
            st.selector.move_by(0.,nextMove)

        
        st, key.KeyChar = 'q'

    let menuState = {
        selector = menuSelectorSpr}

    //mazeEngine.loop_on_key mazeUpdate st0
    menuEngine.loop_on_key menuUpdate menuState




    
