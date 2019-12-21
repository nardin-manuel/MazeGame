module LabProg2019.MazeController

open System
open Gfx
open Engine
open Maze
open External



type MazeState = {
    maze: maze
    mazeSpr: sprite
    playerSpr: sprite
    }

type SolutionState = {
    solution : sprite
}

type Player = {
    name : String
    score : int
    mazeState : MazeState
    }

type RequestInput = {
    strSpr : sprite
    mutable strBuffer: String
}
    
type MazeControl(w, h) =
    let mazeUpdate (key : ConsoleKeyInfo) (screen : wronly_raster) (st) =
        let dx, dy as nextMove=
            match key.KeyChar with
            |'w' -> 0.,-1.
            |'a'-> -1.,0.
            |'s'-> 0.,1.
            |'d'-> 1.,0.
            |_ -> 0.,0.   

        if not (st.playerSpr.checkCollissionWith(nextMove, st.mazeSpr, CharInfo.wall)) then
            st.playerSpr.move_by(dx, dy)
            Log.msg "Player position: x:%f,y:%f" st.playerSpr.x st.playerSpr.y

        st, key.KeyChar = 'q'
    
    //let solutionUpdate(key : ConsoleKeyInfo Option)(screen: wronly_raster)(st) =
    //    st, key.Value.KeyChar = 'q'

    let solutionUpdate(key: ConsoleKeyInfo)(screen: wronly_raster)(st) = 
        st, key.KeyChar ='q'

    let requestInputUpdate(key: ConsoleKeyInfo)(screen: wronly_raster)(st:RequestInput) =          

       st.strBuffer <- st.strBuffer + string key.KeyChar
       st.strSpr.draw_text(st.strBuffer,13,15,Color.Red)
       st, key.KeyChar = ' '



    let mazeEngine = new engine(w,h)
    

    let RequestInput() =
        let printTextImg = new image(w, h)
        printTextImg.draw_text("Type your name: ", 13,14, Color.Red)
        let printTextSpr = mazeEngine.create_and_register_sprite(printTextImg,0,0,0)
       // printTextSpr.draw_text("test", 13,14, Color.Red)

        let requestInput = {
              strSpr = printTextSpr
              strBuffer = ""
        }

        mazeEngine.loop_on_key requestInputUpdate requestInput
        mazeEngine.remove_all_sprite()
        requestInput.strBuffer

    member this.NewGame() =
        mazeEngine.show_fps <- false
        let playerName = RequestInput()
        
        let maze = maze(w,h)
        let entry, exit = maze.createMaze()
        let mazeImg = maze.drawMaze()
        Log.msg "Create maze IMG"
        let mazeSpr = mazeEngine.create_and_register_sprite(mazeImg,0,0,0)
        Log.msg "Create maze Spr"    
        let playerSpr = mazeEngine.create_and_register_sprite(image.rectangle(1,1, CharInfo.player),1,1,2)
        Log.msg "Create maze player"

        let mazeState = {
            maze = maze
            mazeSpr = mazeSpr
            playerSpr = playerSpr
            }               
        
        let player = {
            name = playerName
            score = 0
            mazeState = mazeState
            }


        Log.msg "Create player: %s" player.name

        mazeEngine.loop_on_key mazeUpdate mazeState
        mazeEngine.remove_all_sprite()
        player

    member this.Solve(player: Player) =        
        let solutionImg= player.mazeState.maze.drawSolution((1,1),(31,31))
        mazeEngine.create_and_register_sprite(player.mazeState.mazeSpr, 0,0,0) |> ignore
        let solutionSpr = mazeEngine.create_and_register_sprite(solutionImg,0,0,1)
        let solutionState = {
            solution = solutionSpr            
            }

        mazeEngine.loop_on_key solutionUpdate solutionState
        mazeEngine.remove_all_sprite()

        

    member this.Resume(player : Player) =
        let mazeSpr = mazeEngine.create_and_register_sprite(player.mazeState.mazeSpr, 0,0,0)
        let playerSpr = mazeEngine.create_and_register_sprite(image.rectangle(1,1, CharInfo.player), int(player.mazeState.playerSpr.x), int(player.mazeState.playerSpr.y), 1)
        let mazeState = {
            maze = player.mazeState.maze
            mazeSpr = mazeSpr
            playerSpr = playerSpr
        }
        mazeEngine.loop_on_key mazeUpdate mazeState
        player.mazeState.playerSpr.x <- playerSpr.x
        player.mazeState.playerSpr.y <- playerSpr.y
        
        mazeEngine.remove_all_sprite()
        //player




    
