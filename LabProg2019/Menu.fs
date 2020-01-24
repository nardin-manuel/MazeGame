(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Menu.fs: menu
* (C) 2019 Manuel Nardin @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Menu

open System
open System.Collections.Generic
open System.Linq
open Engine
open Gfx
open Controller
open PlayersDatabase

type MenuState = {
    selector : sprite option
}
type RequestInput = {
    strSpr : sprite
    mutable strBuffer: String
}

type Menu(width: int, heigh: int, itemList: list<String>) =
    let actionList = List<unit->bool>()

    let menuUpdate(key: ConsoleKeyInfo) (screen:wronly_raster) (st) =
        let nextMove = match key.KeyChar with
                       |'w' -> -4.
                       |'s'-> 4.
                       |_ -> 0.
        
        match st.selector with
        |Some selector -> let itemNumber = (int(selector.y)-2)/4
                          let exitCode = match key.KeyChar with            
                                         |' ' -> if itemNumber < actionList.Count then
                                                    actionList.[itemNumber]()
                                                 else
                                                    false
                                         |_ -> false    


            
                          let nx, ny = selector.dryMove(0., nextMove) 

        
                          if ny >= 2. && ny <= float(((actionList.Count)*4)+2) then
                              selector.move_by(0.,nextMove)

                          st, (key.KeyChar = 'q' || exitCode)
        
        |None -> st, key.KeyChar = 'q'
        

    let requestInputUpdate(key: ConsoleKeyInfo) (screen: wronly_raster) (st:RequestInput) =  
        let isValidKey(key) =
             (key  >= '\048' && key <= '\057')||(key >= '\065' && key <= '\090') ||(key >= '\097' && key <= '\122')

        match key.KeyChar with
        |'\008' -> st.strSpr.clear
                   if st.strBuffer.Length > 0 then
                     st.strBuffer <- st.strBuffer.Remove(st.strBuffer.Length-1)
                   st.strSpr.draw_text("Player's name ", 4,4, Color.Red)
    
        |key when isValidKey(key) -> st.strBuffer <- st.strBuffer + string key
        |_ -> () 
            
        st.strSpr.draw_text(st.strBuffer,4,5,Color.Red)
        st, key.KeyChar = ' '


    member val menuEngine = engine(width,heigh)
    member val v_offset = 4 with get, set
    member val h_offset = 4
    member val textColor = Color.Red
    member val viewSelector = true with get, set

    
    member private this.drawMenu() = 
        let menuImg = image(width,heigh)
        let mutable y = this.v_offset
        for item in itemList do
            menuImg.draw_text(item, this.h_offset, y, this.textColor)
            y <- y + this.v_offset

        menuImg

    member private this.drawText(text: String) =
        let menuImg = image(width,heigh)
        menuImg.draw_text(text, this.h_offset, 5, this.textColor)

    member this.requestInput(toPrint: String) =         
        let menu = Menu(width, heigh, [toPrint])
        let menuImg = menu.drawMenu()
        let menuSpr = menu.menuEngine.create_and_register_sprite(menuImg,0,0,0)


        let requestState = {
            strSpr = menuSpr
            strBuffer = ""
        }

        menu.run(requestInputUpdate, requestState)
        //menu.menuEngine.loop_on_key requestInputUpdate requestState
        requestState.strBuffer
        


    member this.addAction(action: unit->bool) = 
        actionList.Add(action)
    
    member private this.run(menuUpdate, menuState) =
        this.menuEngine.show_fps<- false
        this.menuEngine.loop_on_key menuUpdate menuState

    member this.run() = 
        this.menuEngine.show_fps <- false
        this.menuEngine.create_and_register_sprite(this.drawMenu(),0,0,0) |> ignore
        let menuState= if this.viewSelector then
                            let menuSelectorSpr = this.menuEngine.create_and_register_sprite(image.rectangle(1, 1, pixel.create(char('*'), Color.Cyan)),2,4,1)
                            {
                            selector = Some(menuSelectorSpr)
                            }
                        else
                            {
                            selector = None
                            }  
        this.menuEngine.loop_on_key menuUpdate menuState
                              
let main()=
    let w = 31
    let h = 30
    let playersDb = PlayersDatabase()
    let mazeController = Controller(w,h)
    let menu = Menu(w,h, ["New Game";"Resume Game";"Solve Maze"; "Player Chart"])
    
    let addPlayer = fun () -> let playerName = menu.requestInput("Player's name")                            
                              if not <| playersDb.playerExist(playerName) then
                                 playersDb.addPlayer(mazeController.NewGame(playerName))
                              else 
                                 Log.msg("Player name not available")
                              false //do NOT close the current menu if the previous has been closed
    
    let resumeGame = fun () -> let playerList = playersDb.hasNotFinished()
                               let submenu = Menu(w,h, [for player in playerList do yield player.name])    
                               for player in playerList do
                                 submenu.addAction(fun () -> mazeController.Resume(player) 
                                                             true) //close the current menu if the previous has been closed
                               submenu.run()
                               false                          
                              
    let solveMaze = fun () -> let submenu = Menu(w,h, playersDb.toList())
                              for player in playersDb.getPlayersList() do
                                  submenu.addAction(fun () -> mazeController.Solve(player)
                                                              true)
                              submenu.run()
                              false

    let playerChart = fun () -> let playerList = playersDb.hasFinished()
                                playerList.Sort(fun (p1: Player) (p2: Player) -> p1.score.CompareTo(p2.score))
                                let submenu = Menu(w,h, [for player in playerList do yield player.name + ": " + player.score.ToString()])
                                submenu.viewSelector <- false
                                submenu.run()
                                false

                             


    menu.addAction(addPlayer)
    menu.addAction(resumeGame)
    menu.addAction(solveMaze)
    menu.addAction(playerChart)        
    menu.run()








    
