(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Menu.fs: menu
* (C) 2019 Manuel Nardin @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Menu

open System
open System.Collections.Generic

open Engine
open Gfx
open MazeController

type MenuState = {
    selector : sprite
    //mutable player : Player option
}

type Menu(width, height) =    
    member this.Draw() =
        let menuImg = new image(width, height)
        menuImg.draw_text("New Game",13,10,Color.Red)
        menuImg.draw_text("Resume Game",13,14, Color.Red)
        menuImg.draw_text("Solve Maze",13,18, Color.Red)
        menuImg.draw_text("Boh",13,22, Color.Red)
        menuImg

let main()=
    let w = 101
    let h = 51
    let menuEngine = new engine (w,h)
    menuEngine.show_fps <- false
    menuEngine.create_and_register_sprite(Menu(w,h).Draw(),0,0,1) |> ignore
    let menuSelectorSpr = menuEngine.create_and_register_sprite(image.rectangle(1, 1, pixel.create(char('*'), Color.Cyan)),10,10,1)
    let mazeController = MazeControl(w,h)

    let newGame()=
        mazeController.NewGame()

    let resume(player: Player)=
        mazeController.Resume(player)

    let solve(player: Player)=
        mazeController.Solve(player)

    let players  = new List<Player>()

        
    let menuState = {
        selector = menuSelectorSpr
        }

    let selectPlayerUpdate (key : ConsoleKeyInfo) (screen : wronly_raster) (st : MenuState) =
        let nextMove = match key.KeyChar with
                       |'w' -> -4.
                       |'s'-> 4.
                       |_ -> 0.

        let nx, ny = st.selector.dryMove(0., nextMove) 
                       
        if ny >= 10. && ny < float((players.Count)*4+10) then
            st.selector.move_by(0.,nextMove)
                               
        st, key.KeyChar = ' '

    let selectPlayer() =
        let subMenuEngine = new engine(w,h)
        subMenuEngine.show_fps <- false
        let mutable pos = 10            
        let textImg = new image(w,h)
        for player in players do
            textImg.draw_text(player.name, 13, pos, Color.Red)
            pos  <- pos + 4
        let menuSelectorSpr = subMenuEngine.create_and_register_sprite(image.rectangle(1, 1, pixel.create(char('*'), Color.Cyan)),10,10,1)
        subMenuEngine.create_and_register_sprite(textImg,0,0,0) |> ignore

        let selectPlayerState = {
            selector = menuSelectorSpr
            }

        subMenuEngine.loop_on_key selectPlayerUpdate selectPlayerState
        (int(selectPlayerState.selector.y)-10)/4


    let menuUpdate (key : ConsoleKeyInfo) (screen : wronly_raster) (st : MenuState) =
        let nextMove = match key.KeyChar with
                       |'w' -> -4.
                       |'s'-> 4.
                       |_ -> 0.

        match key.KeyChar with
        |' ' -> match (int(st.selector.y)-10)/4 with
                |0 -> players.Add(newGame())
                |1 -> resume(players.Item(selectPlayer()))
                |2 -> solve(players.Item(players.Count-1))
                |3 -> ()
                |_ -> ()

        |_ -> ()

        
        let nx, ny = st.selector.dryMove(0., nextMove) 

        if ny >= 10. && ny <= 22. then
            st.selector.move_by(0.,nextMove)
        
        st, key.KeyChar = 'q'

    //mazeEngine.loop_on_key mazeUpdate st0
    menuEngine.loop_on_key menuUpdate menuState




    
