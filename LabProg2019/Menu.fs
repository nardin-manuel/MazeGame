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
open PlayersDatabase

type MenuState = {
    selector : sprite
}
type RequestInput = {
    strSpr : sprite
    mutable strBuffer: String
}

type Menu(width: int, heigh: int, itemList: list<String>) =
    let actionList = List<unit->unit>()

    let menuUpdate(key: ConsoleKeyInfo) (screen:wronly_raster) (st) =
        let nextMove = match key.KeyChar with
                       |'w' -> -4.
                       |'s'-> 4.
                       |_ -> 0.

        let itemNumber = (int(st.selector.y)-2)/4
        if itemNumber < actionList.Count then
            match key.KeyChar with
            |' ' -> actionList.[itemNumber]()                               
            |_ -> ()


            
        let nx, ny = st.selector.dryMove(0., nextMove) 

        
        if ny >= 2. && ny <= float(((actionList.Count)*4)+2) then
            st.selector.move_by(0.,nextMove)
    
        st, key.KeyChar = 'q'

    let requestInputUpdate(key: ConsoleKeyInfo) (screen: wronly_raster) (st:RequestInput) =  
        let isValidKey(key) =
             (key  >= '\048' && key <= '\057')||(key >= '\065' && key <= '\090') ||(key >= '\097' && key <= '\122')

        match key.KeyChar with
        |'\008' -> st.strSpr.clear
                   if st.strBuffer.Length>0 then
                     st.strBuffer <- st.strBuffer.Remove(st.strBuffer.Length-1)
                   //st.strSpr.draw_text("Type your name: ", 13,14, Color.Red)
    
        |key when isValidKey(key) -> st.strBuffer <- st.strBuffer + string key
        |_ -> () 
            
        st.strSpr.draw_text(st.strBuffer,13,15,Color.Red)
        st, key.KeyChar = ' '


    member val menuEngine = engine(width,heigh)
    member val v_offset = 4 with get, set
    member val h_offset = 4
    member val textColor = Color.Red

    
    member this.drawMenu() = 
        let menuImg = image(width,heigh)
        let mutable y = this.v_offset
        for item in itemList do
            menuImg.draw_text(item, this.h_offset, y, this.textColor)
            y <- y + this.v_offset

        menuImg


    member this.requestInput(toPrint: String) =         
        let menu = Menu(width, heigh, [toPrint])
        let menuImg = menu.drawMenu()
        let menuSpr = menu.menuEngine.create_and_register_sprite(menuImg,0,0,0)


        let requestState = {
            strSpr = menuSpr
            strBuffer = ""
        }

        menu.menuEngine.loop_on_key requestInputUpdate requestState
        requestState.strBuffer
        

    member this.addAction(action: unit->unit) = 
        actionList.Add(action) 
        
    
    member this.run(menuUpdate, menuState) =
        this.menuEngine.show_fps<- false
        this.menuEngine.loop_on_key menuUpdate menuState

    member this.run() = 
        this.menuEngine.show_fps <- false
        this.menuEngine.create_and_register_sprite(this.drawMenu(),0,0,0) |> ignore
        let menuSelectorSpr = this.menuEngine.create_and_register_sprite(image.rectangle(1, 1, pixel.create(char('*'), Color.Cyan)),2,4,1)

        let menuState = {
               selector = menuSelectorSpr
               }
        this.menuEngine.loop_on_key menuUpdate menuState
                              
let main()=
    let w = 30
    let h = 30
    let playersDb = PlayersDatabase()
    let mazeController = MazeControl(w,h)
    let menu = Menu(w,h, ["New Game";"Resume Game";"Solve Maze"])
    
    let addPlayer = fun _ -> playersDb.addPlayer(mazeController.NewGame(menu.requestInput("Player's name")))
    
    let resumeGame = fun _ -> let submenu = Menu(w,h, playersDb.toString())
                              for player in playersDb.getPlayersList() do
                                submenu.addAction(fun _ -> mazeController.Resume(player))
                              submenu.run()
                              

    let solveMaze = fun _ -> let submenu = Menu(w,h, playersDb.toString())
                             for player in playersDb.getPlayersList() do
                                submenu.addAction(fun _ -> mazeController.Solve(player))
                             submenu.run()
                             

    menu.addAction(addPlayer)
    menu.addAction(resumeGame)
    menu.addAction(solveMaze)
        
    menu.run()








    
