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
    players : List<Player>
}
type RequestInput = {
    strSpr : sprite
    mutable strBuffer: String
}
//type Menu(width, height) =    

//    let players  = new List<Player>()
//    let menuEngine = new engine (width,height)
//    let mazeController = MazeControl(width,height)

//    let selectPlayerUpdate (key : ConsoleKeyInfo) (screen : wronly_raster) (st : MenuState) =
//        let nextMove = match key.KeyChar with
//                       |'w' -> -4.
//                       |'s'-> 4.
//                       |_ -> 0.

//        let nx, ny = st.selector.dryMove(0., nextMove) 
                   
//        if ny >= 10. && ny < float((st.players.Count)*4+10) then
//            st.selector.move_by(0.,nextMove)
                           
//        st, key.KeyChar = ' '

//    let requestInputUpdate(key: ConsoleKeyInfo)(screen: wronly_raster)(st:RequestInput) =  
//        let isValidKey(key) =
//             (key  >= '\048' && key <= '\057')||(key >= '\065' && key <= '\090') ||(key >= '\097' && key <= '\122')

//        match key.KeyChar with
//        |'\008' -> st.strSpr.clear
//                   if st.strBuffer.Length>0 then
//                     st.strBuffer <- st.strBuffer.Remove(st.strBuffer.Length-1)
//                   st.strSpr.draw_text("Type your name: ", 13,14, Color.Red)
    
//        |key when isValidKey(key) -> st.strBuffer <- st.strBuffer + string key
//        |_ -> () 
    
//        st.strSpr.draw_text(st.strBuffer,13,15,Color.Red)
//        st, key.KeyChar = ' '

//    let selectPlayer() =
//        let subMenuEngine = new engine(width,height)
//        subMenuEngine.show_fps <- false
//        let mutable pos = 10            
//        let textImg = new image(width,height)
//        for player in players do
//            textImg.draw_text(player.name, 13, pos, Color.Red)
//            pos  <- pos + 4
//        let menuSelectorSpr = subMenuEngine.create_and_register_sprite(image.rectangle(1, 1, pixel.create(char('*'), Color.Cyan)),10,10,1)
//        subMenuEngine.create_and_register_sprite(textImg,0,0,0) |> ignore

//        let selectPlayerState = {
//            selector = menuSelectorSpr
//            players = players
//            }

//        subMenuEngine.loop_on_key selectPlayerUpdate selectPlayerState
//        (int(selectPlayerState.selector.y)-10)/4

//    let RequestInput() =
//        let printTextImg = new image(width, height)
//        printTextImg.draw_text("Type your name: ", 13,14, Color.Red)
//        let printTextSpr = menuEngine.create_and_register_sprite(printTextImg,0,0,0)

//        let requestInput = {
//              strSpr = printTextSpr
//              strBuffer = ""
//        }

//        menuEngine.loop_on_key requestInputUpdate requestInput
//        menuEngine.remove_all_sprite()
//        requestInput.strBuffer

//    member private this.menuUpdate (key : ConsoleKeyInfo) (screen : wronly_raster) (st : MenuState) =
//        let nextMove = match key.KeyChar with
//                       |'w' -> -4.
//                       |'s'-> 4.
//                       |_ -> 0.

//        match key.KeyChar with
//        |' ' -> match (int(st.selector.y)-10)/4 with
//                |0 -> st.players.Add(this.newGame())
//                |1 -> this.resume(st.players.Item(selectPlayer()))
//                |2 -> this.solve(st.players.Item(st.players.Count-1))
//                |3 -> ()
//                |_ -> ()

//        |_ -> ()

    
//        let nx, ny = st.selector.dryMove(0., nextMove) 

//        if ny >= 10. && ny <= 22. then
//            st.selector.move_by(0.,nextMove)
    
//        st, key.KeyChar = 'q'
    
//    member this.Draw() =
//        let menuImg = new image(width, height)
//        menuImg.draw_text("New Game",13,10,Color.Red)
//        menuImg.draw_text("Resume Game",13,14, Color.Red)
//        menuImg.draw_text("Solve Maze",13,18, Color.Red)
//        menuImg.draw_text("Boh",13,22, Color.Red)
//        menuImg
        
//    member this.newGame() = 
//        mazeController.NewGame(RequestInput())

//    member this.resume(player:Player) = 
//        mazeController.Resume(player)

//    member this.solve(player:Player) =
//        mazeController.Solve(player)
        
//    member this.run() =
//        menuEngine.show_fps <- false
//        menuEngine.create_and_register_sprite(Menu(width,height).Draw(),0,0,1) |> ignore
//        let menuSelectorSpr = menuEngine.create_and_register_sprite(image.rectangle(1, 1, pixel.create(char('*'), Color.Cyan)),10,10,1)
        
//        let menuState = {
//                selector = menuSelectorSpr
//                players = players
//                }

//        menuEngine.loop_on_key this.menuUpdate menuState


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
        //menuEngine.loop_on_key requestInputUpdate requestState
        //menuEngine.remove_all_sprite()
        menu.menuEngine.loop_on_key requestInputUpdate requestState
        requestState.strBuffer
        

    member this.addAction(action: unit->unit) = 
        actionList.Add(action) 
        
    
    member this.run(menuUpdate, menuState) =
        this.menuEngine.show_fps<- false
        this.menuEngine.loop_on_key menuUpdate menuState
       // menuEngine.remove_all_sprite()

    member this.run() = 
        this.menuEngine.show_fps <- false
        this.menuEngine.create_and_register_sprite(this.drawMenu(),0,0,0) |> ignore
        let menuSelectorSpr = this.menuEngine.create_and_register_sprite(image.rectangle(1, 1, pixel.create(char('*'), Color.Cyan)),2,4,1)

        let menuState = {
               selector = menuSelectorSpr
               players = new List<Player>()
               }

        this.menuEngine.loop_on_key menuUpdate menuState
        //menuEngine.remove_all_sprite()

type PlayersDatabase() =
    let playersList = List<Player>()

    member this.addPlayer(player: Player) =
        playersList.Add(player)

    member this.removePlayer(player: Player) = 
        playersList.Remove(player)

    member this.getPlayerFromName(playerName: String) =
        playersList.Find(fun player -> player.name = playerName)

    member this.getPlayersList() = 
        playersList

    member this.toString() =
        [
        for player in playersList do
            yield player.name
        ]


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

    //let resumeGame = fun _ -> mazeController.Resume(playersDb.ge)
    menu.addAction(addPlayer)
    menu.addAction(resumeGame)
    menu.addAction(solveMaze)



    menu.run()

    //let menuEngine = new engine (w,h)
    //menuEngine.show_fps <- false
    //menuEngine.create_and_register_sprite(Menu(w,h).Draw(),0,0,1) |> ignore
    //let menuSelectorSpr = menuEngine.create_and_register_sprite(image.rectangle(1, 1, pixel.create(char('*'), Color.Cyan)),10,10,1)
    //let mazeController = MazeControl(w,h)
           
    //let RequestInput() =
    //    let printTextImg = new image(w, h)
    //    printTextImg.draw_text("Type your name: ", 13,14, Color.Red)
    //    let printTextSpr = menuEngine.create_and_register_sprite(printTextImg,0,0,0)

    //    let requestInput = {
    //          strSpr = printTextSpr
    //          strBuffer = ""
    //    }

    //    menuEngine.loop_on_key requestInputUpdate requestInput
    //    menuEngine.remove_all_sprite()
    //    requestInput.strBuffer

    //let newGame()=
    //    mazeController.NewGame(RequestInput())

    //let resume(player: Player)=
    //    mazeController.Resume(player)

    //let solve(player: Player)=
    //    mazeController.Solve(player)
        
    //let menuState = {
    //    selector = menuSelectorSpr
    //    players = new List<Player>()
    //    }

    //let selectPlayer() =
    //    let subMenuEngine = new engine(w,h)
    //    subMenuEngine.show_fps <- false
    //    let mutable pos = 10            
    //    let textImg = new image(w,h)
    //    for player in menuState.players do
    //        textImg.draw_text(player.name, 13, pos, Color.Red)
    //        pos  <- pos + 4
    //    let menuSelectorSpr = subMenuEngine.create_and_register_sprite(image.rectangle(1, 1, pixel.create(char('*'), Color.Cyan)),10,10,1)
    //    subMenuEngine.create_and_register_sprite(textImg,0,0,0) |> ignore

    //    let selectPlayerState = {
    //        selector = menuSelectorSpr
    //        players = menuState.players
    //        }

    //    subMenuEngine.loop_on_key selectPlayerUpdate selectPlayerState
    //    (int(selectPlayerState.selector.y)-10)/4







    
