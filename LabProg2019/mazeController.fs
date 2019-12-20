module LabProg2019.MazeController

open System
open Gfx
open Engine
open Maze
open External
open System.Collections


type mazeState = {
    maze: maze
    mazeSpr: sprite
    playerSpr: sprite
    }

type Player = {
    name : String
    score : int
    mazeState : mazeState
    }
    
type mazeControl(w, h) =
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

    let mazeEngine = new engine(w,h)
    member val players = new ArrayList()
   // member private this.mazes = new ArrayList()

    member this.newGame() =
        mazeEngine.show_fps <- false
        
        let maze = maze(w,h)
        let entry, exit = maze.createMaze()
        let mazeImg = maze.drawMaze()
        Log.msg "Create maze IMG"
        let mazeSpr = mazeEngine.create_and_register_sprite(mazeImg,0,0,0)
        Log.msg "Create maze Spr"    
        let playerSpr = mazeEngine.create_and_register_sprite(image.rectangle(1,1, CharInfo.player),1,1,2)
        Log.msg "Create maze player"
       // let playerName = System.Console.ReadLine()
        let mazeState = {
            maze = maze
            mazeSpr = mazeSpr
            playerSpr = playerSpr
            }   
            
        let player = {
            name = "Gianni"
            score = 0
            mazeState = mazeState
            }

        this.players.Add(player) |> ignore   
        
        

        mazeEngine.loop_on_key mazeUpdate mazeState
        player

    member this.solve(player: Player) =
        
        let solutionImg= player.mazeState.maze.drawSolution((1,1),(31,31))
        let solutionSpr = mazeEngine.create_and_register_sprite(solutionImg,0,0,1)
        let solutionState = {
            solution = solutionSpr            
            }

        mazeEngine.loop_on_key solutionUpdate solutionState
        //player

    member this.resume(player : Player ) =          
        mazeEngine.loop_on_key mazeUpdate player.mazeState
        //player

    
